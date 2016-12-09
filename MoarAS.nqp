# Copyright 2016 cygx <cygx@cpan.org>
# Distributed under the Boost Software License, Version 1.0

# TODO: remove last uses of $*parser that break encapsulation

my $assembler := nqp::getcomp('MAST');

my %types := nqp::hash(
    'int', int,
    'num', num,
    'str', str,
    'obj', NQPMu
);

my %suffix := nqp::hash(
    'int', 'i',
    'str', 's',
);

my %extsuffix := nqp::hash(
    'int', 'i64',
    'str', 's',
);

my %flagmap := nqp::hash(
    'obj', 1,
    'int', 2,
    'num', 4,
    'str', 8,
    ':obj', 1 +| 32,
    ':int', 2 +| 32,
    ':num', 4 +| 32,
    ':str', 8 +| 32,
    #'flat', 64,
    #'flatnamed', 128,
);

sub bailout($msg = '?') {
    nqp::closefh($*fh) if nqp::defined($*fh);
    nqp::die("panic: $msg\n[{$*parser.n}] {$*parser.line}");
}

sub trim(str $str) {
    my int $pos  := nqp::chars($str) - 1;
    my int $left := nqp::findnotcclass(
        nqp::const::CCLASS_WHITESPACE, $str, 0, $pos + 1);

    $pos := $pos - 1 while nqp::isge_i($pos, $left)
           && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $str, $pos);

    nqp::islt_i($pos, $left)
        ?? ''
        !! nqp::substr($str, $left, $pos + 1 - $left);
}

grammar MoarAS::Grammar {
    token name { <[\w]-[\d]> \w* }
    token type { int | num | str | obj }

    proto token ident {*}
    token ident:sym<name> { <name> }
    token ident:sym<str>  { "'" (<-[']>*) "'" }

    proto token varexpr {*}
    token varexpr:sym<local> { '%' (\d+) }
    token varexpr:sym<alias> { '$' <name> }

    proto token constexpr {*}
    token constexpr:sym<str>  { '"' (<-["]>*) '"' }
    token constexpr:sym<cstr> { ':' <name> }
    token constexpr:sym<num>  { \d+ '.' \d+ ['e' <[+-]>? \d+ ]? }
    token constexpr:sym<int>  { <[+-]> \d+ }
    token constexpr:sym<uint> { \d+ }

    proto token refexpr {*}
    token refexpr:sym<lexical> { '*' <ident> }
    token refexpr:sym<farlex>  { '*' (\d+) '!' (\d+) }
    token refexpr:sym<rawlex>  { '*' (\d+) }
    token refexpr:sym<label>   { '@' <name> }
    token refexpr:sym<code>    { '&' <ident> }

    proto token expr {*}
    token expr:<var>   { <varexpr> }
    token expr:<const> { <constexpr> }
    token expr:<ref>   { <refexpr> }

    token opname { (\w+) <?{ nqp::existskey(%MAST::Ops::codes, ~$/[0]) }> }

    proto token statement {*}
    token statement:sym<hll>      {:s '.hll' <ident>}
    token statement:sym<set_main> {:s '.set' 'main'}
    token statement:sym<set_load> {:s '.set' 'load'}
    token statement:sym<frame2>   {:s '.frame' <ident> <ident>}
    token statement:sym<frame1>   {:s '.frame' <ident>}
    token statement:sym<frame0>   {:s '.frame'}
    token statement:sym<locals>   {:s '.locals' <type>+%<.ws>}
    token statement:sym<local2>   {:s '.local' <type> <constexpr>}
    token statement:sym<local1>   {:s '.local' <type>}
    token statement:sym<aliases>  {:s '.aliases' <name>+%<.ws>}
    token statement:sym<alias>    {:s '.alias' <name> (\d+)}
    token statement:sym<var3>     {:s '.var' <type> <name> <constexpr>}
    token statement:sym<var2>     {:s '.var' <type> <name>}
    token statement:sym<lex3>     {:s '.lex' <type> <ident> <varexpr>}
    token statement:sym<lex2>     {:s '.lex' <type> <ident>}
    token statement:sym<label>    {:s '.label' <name>}
    token statement:sym<param>    {:s '.param' <type> <name> <constexpr>}
    token statement:sym<flags>    {:s '.flags' (':'? <type>)+%<.ws>}
    token statement:sym<result>   {:s '.result' <varexpr>}
    token statement:sym<call_>    {:s '.call' <varexpr> <expr>+%<.ws>}
    token statement:sym<call1>    {:s '.call' <varexpr>}
    token statement:sym<op_>      {:s <opname> <expr>+%<.ws>}
    token statement:sym<op0>      {:s <opname>}

    token TOP { '#' || <statement>? $ || {bailout()} }
}

class MoarAS::Parser {
    has $!line;
    has $!n;
    has $!cu;
    has $!cuuid;
    has %!frames;
    has $!frame;
    has @!flags;
    has $!result;

    method new() {
        my $parser := nqp::create(MoarAS::Parser);
        $parser.BUILD;
        $parser;
    }

    method BUILD() {
        $!n := 0;
        $!cu := MAST::CompUnit.new;
        $!cuuid := 0;
        %!frames := {};
        @!flags := [];
        $!result := MAST::Node;
    }

    method line() { $!line }
    method n() { $!n }
    method cu() { $!cu }
    method frame() { $!frame // bailout('no frame') }

    method call_flags() { @!flags }
    method call_result() { $!result }
    method call_reset() {
        @!flags := [];
        $!result := MAST::Node;
    }

    method next-line($line) {
        if nqp::defined($line) {
            $!line := trim($line);
            $!n := $!n + 1;
            1;
        }
        else { 0 }
    }

    method get_frame($uid) {
        nqp::existskey(%!frames, $uid)
            ?? %!frames{$uid}
            !! (%!frames{$uid} := MoarAS::Frame.new($uid));
    }

    method name($/) {}
    method type($/) {}

    method ident:sym<name>($/) { make ~$<name> }
    method ident:sym<str>($/) { make ~$/[0] }

    method varexpr:sym<local>($/) { make MAST::Local.new(index => +~$/[0]) }
    method varexpr:sym<alias>($/) { make self.frame.dealias(~$<name>) }

    method constexpr:sym<str>($/) { make MAST::SVal.new(value => ~$/[0]) }
    method constexpr:sym<cstr>($/) { make MAST::SVal.new(value => ~$<name>) }
    method constexpr:sym<num>($/) { make MAST::NVal.new(value => +~$/) }
    method constexpr:sym<int>($/) { make MAST::IVal.new(value => +~$/) }
    method constexpr:sym<uint>($/) { make MAST::IVal.new(value => +~$/, signed => 0) }

    method refexpr:sym<lexical>($/) { make self.frame.lexical($<ident>.made) }
    method refexpr:sym<farlex>($/) { make self.frame.rawlex(+~$/[0], +~$/[1]) }
    method refexpr:sym<rawlex>($/) { make self.frame.rawlex(+~$/[0]) }
    method refexpr:sym<label>($/) { make self.frame.label(~$<name>) }
    method refexpr:sym<code>($/) { make self.get_frame($<ident>.made).node }

    method expr:<var>($/) { make $<varexpr>.made }
    method expr:<const>($/) { make $<constexpr>.made }
    method expr:<ref>($/) { make $<refexpr>.made }

    method opname($/) {}

    method statement:sym<hll>($/) { $!cu.hll($<ident>.made) }
    method statement:sym<set_main>($/) { $!cu.main_frame(self.frame.node) }
    method statement:sym<set_load>($/) { $!cu.load_frame(self.frame.node) }
    method statement:sym<frame2>($/) {
        my $uid := $<ident>[0].made;
        my $name := $<ident>[1].made;
        bailout("frame $uid already exists")
            if nqp::existskey(%!frames, $uid);
        $!frame := self.get_frame($uid).init($name);
    }
    method statement:sym<frame1>($/) {
        my $uid := $<ident>.made;
        $!frame := self.get_frame($uid);
        $!frame.init("frame_$uid") unless $!frame.initialized;
    }
    method statement:sym<frame0>($/) {
        $!frame := MoarAS::Frame.new($!cuuid++).init;
    }
    method statement:sym<locals>($/) { self.frame.add_local(~$_) for $<type> }
    method statement:sym<local2>($/) {
        my $type := ~$<type>;
        my $local := self.frame.add_local($type);
        my $init := $<constexpr>.made;
        my $suffix := %extsuffix{$type};
        my $op := "const_$suffix";
        $!frame.add_op($op, $local, $init);
    }
    method statement:sym<local1>($/) { self.frame.add_local(~$<type>) }
    method statement:sym<aliases>($/) {
        my $i := 0;
        self.frame.add_alias(~$_, $i++) for $<name>;
    }
    method statement:sym<alias>($/) { self.frame.add_alias(~$<name>, +~$/[0]) }
    method statement:sym<var3>($/) {
        my $type := ~$<type>;
        my $name := ~$<name>;
        my $local := self.frame.add_local($type);
        my $init := $<constexpr>.made;
        my $suffix := %extsuffix{$type};
        my $op := "const_$suffix";
        $!frame.add_alias($name, $local.index);
        $!frame.add_op($op, $local, $init);
    }
    method statement:sym<var2>($/) {
        my $type := ~$<type>;
        my $name := ~$<name>;
        my $local := self.frame.add_local($type);
        $!frame.add_alias($name, $local.index);
    }
    method statement:sym<lex3>($/) {
        my $type := ~$<type>;
        my $name := $<ident>.made;
        my $lexical := self.frame.add_lexical($type, $name);
        my $init := $<varexpr>.made;
        $!frame.add_op('bindlex', $lexical, $init);
    }
    method statement:sym<lex2>($/) { self.frame.add_lexical(~$<type>, $<ident>.made) }
    method statement:sym<label>($/) { self.frame.add_label(~$<name>) }
    method statement:sym<param>($/) {
        my $type := ~$<type>;
        my $name := ~$<name>;
        my $index := $<constexpr>.made;
        my $local := self.frame.add_local($type);
        $!frame.add_alias($name, $local.index);
        $!frame.add_op("param_rp_{%suffix{$type}}", $local, $index);
    }
    method statement:sym<flags>($/) {
        for $/[0] {
            my $flag := ~$_;
            bailout('illegal flag')
                unless nqp::existskey(%flagmap, $flag);
            nqp::push(@!flags, %flagmap{$flag});
        }
    }
    method statement:sym<result>($/) { $!result := $<varexpr>.made }
    method statement:sym<call_>($/) {
        my @args;
        nqp::push(@args, $_.made) for $<expr>;
        self.frame.add_call($<varexpr>.made, |@args);
    }
    method statement:sym<call1>($/) { self.frame.add_call($<varexpr>.made) }
    method statement:sym<op_>($/) {
        my @args;
        nqp::push(@args, $_.made) for $<expr>;
        self.frame.add_op(~$<opname>, |@args);
    }
    method statement:sym<op0>($/) { self.frame.add_op(~$<opname>) }

    method TOP($/) {}
}

class MoarAS::Frame {
    has $!node;
    has $!uid;
    has %!aliases;
    has %!lexicals;
    has %!labels;

    method new($uid) {
        my $frame := nqp::create(MoarAS::Frame);
        $frame.BUILD($uid);
        $frame;
    }

    method BUILD($uid) {
        $!uid := $uid;
        %!aliases := nqp::hash();
        %!lexicals := nqp::hash();
        %!labels := nqp::hash();
    }

    method initialized() {
        nqp::defined($!node);
    }

    method init($name = '<anon>') {
        bailout('frame already initialized')
            if self.initialized;

        $!node := MAST::Frame.new(:$name, cuuid => $!uid);
        $*parser.cu.add_frame($!node);
        self;
    }

    method node() {
        self.init unless self.initialized;
        $!node;
    }

    method label($name) {
        %!labels{$name} // (%!labels{$name} := MAST::Label.new);
    }

    method lexical($name) {
        %!lexicals{$name} // bailout("lexical '$name' not declared");
    }

    method rawlex($index, $frames_out = 0) {
        MAST::Lexical.new(:$index, :$frames_out);
    }

    method dealias($name) {
        MAST::Local.new(index => %!aliases{$name}
            // bailout("alias '$name' not declared"));
    }

    method add_label($name) {
        my $label := self.label($name);
        nqp::push($!node.instructions, $label);
        $label;
    }

    method add_local($type) {
        bailout('illegal type')
            unless nqp::existskey(%types, $type);

        my $index := $!node.add_local(%types{$type});
        MAST::Local.new(:$index);
    }

    method add_alias($name, $id) {
        %!aliases{$name} := $id;
    }

    method add_lexical($type, $name) {
        bailout("illegal type $type")
            unless nqp::existskey(%types, $type);

        bailout("lexical '$name' already exists")
            if nqp::existskey(%!lexicals, $name);

        my $index := $!node.add_lexical(%types{$type}, $name);
        my $lexical := MAST::Lexical.new(:$index);
        %!lexicals{$name} := $lexical;

        $lexical;
    }

    method add_op($op, *@args) {
        my $opnode := MAST::Op.new(:$op, |@args);
        nqp::push($!node.instructions, $opnode);
        $opnode;
    }

    method add_call($target, *@args) {
        my @flags := $*parser.call_flags;
        my $result := $*parser.call_result;

        my $call := MAST::Call.new(:$target, :@flags, :$result, |@args);
        nqp::push($!node.instructions, $call);

        $*parser.call_reset;
        $call;
    }
}

class MoarAS::Compiler {
    method new() {
        nqp::die('MAST::Compiler cannot be instantiated');
    }

    method parse(&lines) {
        my $*parser := MoarAS::Parser.new;
        MoarAS::Grammar.parse($*parser.line, actions => $*parser)
            while $*parser.next-line(lines());

        $*parser.cu;
    }

    method parse_code($code) {
        my int $i := 0;
        my @lines := nqp::split("\n", $code);
        self.parse({ $i < @lines ?? @lines[$i++] !! NQPMu });
    }

    method parse_fh($*fh) {
        my $cu := self.parse({
            my $line := nqp::readlinefh($*fh);
            nqp::chars($line) ?? $line !! NQPMu;
        });
        nqp::closefh($*fh);
        $cu;
    }

    method parse_file($file) { self.parse_fh(nqp::open($file, 'r')) }

    method load($cu) { $assembler.assemble_and_load($cu) }
    method load_code($code) { self.load(self.parse_code($code)) }
    method load_fh($fh) { self.load(self.parse_fh($fh)) }
    method load_file($file) { self.load(self.parse_file($file)); }

    method compile($cu, $dest) { $assembler.assemble_to_file($cu, $dest) }
    method compile_code($code, $dest) { self.compile(self.parse_code($code), $dest) }
    method compile_fh($fh, $dest) { self.compile(self.parse_fh($fh), $dest) }
    method compile_file($file, $dest) { self.compile(self.parse_file($file), $dest) }

    method eval($cu, *@args) { nqp::compunitmainline(self.load($cu))(|@args) }
    method eval_code($fh, *@args) { self.eval(self.parse_code($fh), |@args) }
    method eval_fh($fh, *@args) { self.eval(self.parse_fh($fh), |@args) }
    method eval_file($file, *@args) { self.eval(self.parse_file($file), |@args) }
}

nqp::bindcurhllsym('MoarAS::Compiler', MoarAS::Compiler);
