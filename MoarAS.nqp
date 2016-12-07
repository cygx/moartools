# Copyright 2016 cygx <cygx@cpan.org>
# Distributed under the Boost Software License, Version 1.0

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
    nqp::die("panic: $msg\n[$*n] $*line");
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

sub frame($uid) {
    nqp::existskey(%*frames, $uid)
        ?? %*frames{$uid}
        !! (%*frames{$uid} := MoarAS::Frame.new($uid));
}

sub theframe() {
    bailout('no frame') unless nqp::defined($*frame);
    $*frame;
}

grammar MoarAS::Grammar {
    token name { <[\w]-[\d]> \w* }

    token type { int | num | str | obj }

    proto token varexpr {*}
          token varexpr:sym<local> { '%' (\d+) }
          token varexpr:sym<alias> { '$' <name> }

    proto token constexpr {*}
          token constexpr:sym<str>  { "'" (<-[']>*) "'" }
          token constexpr:sym<cstr> { ':' <name> }
          token constexpr:sym<num>  { \d+ '.' \d+ ['e' <[+-]>? \d+ ]? }
          token constexpr:sym<int>  { <[+-]> \d+ }
          token constexpr:sym<uint> { \d+ }

    proto token refexpr {*}
          token refexpr:sym<lexical> { '*' <name> }
          token refexpr:sym<farlex>  { '*' (\d+) '!' (\d+) }
          token refexpr:sym<rawlex>  { '*' (\d+) }
          token refexpr:sym<label>   { '@' <name> }
          token refexpr:sym<code>    { '&' (\S+) }

    proto token expr {*}
          token expr:<var>   { <varexpr> }
          token expr:<const> { <constexpr> }
          token expr:<ref>   { <refexpr> }

    token opname { (\w+) <?{ nqp::existskey(%MAST::Ops::codes, ~$/[0]) }> }

    proto token statement {*}
          token statement:sym<hll>      {:s '.hll' <name>}
          token statement:sym<set_main> {:s '.set' 'main'}
          token statement:sym<set_load> {:s '.set' 'load'}
          token statement:sym<frame2>   {:s '.frame' (\S+) <name>}
          token statement:sym<frame1>   {:s '.frame' (\S+)}
          token statement:sym<frame0>   {:s '.frame'}
          token statement:sym<locals>   {:s '.locals' <type>+%<.ws>}
          token statement:sym<local3>   {:s '.local' <type> <name> <constexpr>}
          token statement:sym<local2>   {:s '.local' <type> <name>}
          token statement:sym<lexical3> {:s '.lexical' <type> <name> <varexpr>}
          token statement:sym<lexical2> {:s '.lexical' <type> <name>}
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

class MoarAS::Actions {
    method name($/) { make ~$/ }
    method type($/) { make ~$/ }
    method varexpr:sym<local>($/) { make MAST::Local.new(index => +~$/[0]) }
    method varexpr:sym<alias>($/) { make theframe().dealias($<name>.made) }
    method constexpr:sym<str>($/) { make MAST::SVal.new(value => ~$/[0]) }
    method constexpr:sym<cstr>($/) { make MAST::SVal.new(value => $<name>.made) }
    method constexpr:sym<uint>($/) { make MAST::IVal.new(value => +~$/, signed => 0) }
    method constexpr:sym<int>($/) { make MAST::IVal.new(value => +~$/) }
    method constexpr:sym<num>($/) { make MAST::NVal.new(value => +~$/) }
    method refexpr:sym<lexical>($/) { make theframe().lexical($<name>.made) }
    method refexpr:sym<rawlex>($/) { make theframe().rawlex(+~$/[0]) }
    method refexpr:sym<farlex>($/) { make theframe().rawlex(+~$/[0], +~$/[1]) }
    method refexpr:sym<label>($/) { make theframe().label($<name>.made) }
    method refexpr:sym<code>($/) { make frame(~$/[0]).node }
    method expr:<var>($/) { make $<varexpr>.made }
    method expr:<const>($/) { make $<constexpr>.made }
    method expr:<ref>($/) { make $<refexpr>.made }
    method opname($/) { make ~$/ }
    method statement:sym<hll>($/) { $*cu.hll($<name>.made) }
    method statement:sym<set_main>($/) { $*cu.main_frame(theframe().node) }
    method statement:sym<set_load>($/) { $*cu.load_frame(theframe().node) }
    method statement:sym<frame0>($/) { $*frame := MoarAS::Frame.new.init }
    method statement:sym<frame1>($/) {
        my $uid := ~$/[0];
        $*frame := frame($uid);
        $*frame.init("frame_$uid") unless $*frame.initialized:
    }
    method statement:sym<frame2>($/) {
        my $uid := ~$/[0];
        bailout("frame $uid already exists")
            if nqp::existskey(%*frames, $uid);

        $*frame := frame($uid).init($<name>.made);
    }
    method statement:sym<locals>($/) {
        theframe().add_local($_.made) for $<type>;
    }
    method statement:sym<local2>($/) {
        my $local := theframe().add_local($<type>.made);
        theframe().add_alias($<name>.made, $local.index);
    }
    method statement:sym<local3>($/) {
        my $type := $<type>.made;
        bailout('invalid type')
            unless nqp::existskey(%extsuffix, $type);

        my $local := theframe().add_local($type);
        theframe().add_alias($<name>.made, $local.index);
        my $init := $<constexpr>.made;
        my $suffix := %extsuffix{$type};
        my $op := "const_$suffix";
        theframe().add_op($op, $local, $init);
    }
    method statement:sym<lexical2>($/) {
        theframe().add_lexical($<type>.made, $<name>.made);
    }
    method statement:sym<lexical3>($/) {
        my $type := $<type>.made;
        bailout('invalid type')
            unless nqp::existskey(%extsuffix, $type);

        my $lexical := theframe().add_lexical($type, $<name>.made);
        my $init := $<varexpr>.made;
        theframe().add_op('bindlex', $lexical, $init);

    }
    method statement:sym<label>($/) { theframe().add_label($<name>.made) }
    method statement:sym<param>($/) {
        my $type := $<type>.made;
        my $name := $<name>.made;
        my $index := $<constexpr>.made;
        bailout('invalid type')
            unless nqp::existskey(%suffix, $type);

        my $local := theframe().add_local($type);
        theframe().add_alias($name, $local.index);
        theframe().add_op("param_rp_{%suffix{$type}}", $local, $index);
    }
    method statement:sym<flags>($/) {
        for $/[0] {
            my $flag := ~$_;
            bailout('illegal flag')
                unless nqp::existskey(%flagmap, $flag);

            nqp::push(@*flags, %flagmap{$flag});
        }
    }
    method statement:sym<result>($/) { $*result := $<varexpr>.made }
    method statement:sym<call1>($/) { theframe().add_call($<varexpr>.made) }
    method statement:sym<call_>($/) {
        my @args;
        nqp::push(@args, $_.made) for $<expr>;
        theframe().add_call($<varexpr>.made, |@args);
    }
    method statement:sym<op0>($/) { theframe().add_op($<opname>.made) }
    method statement:sym<op_>($/) {
        my @args;
        nqp::push(@args, $_.made) for $<expr>;
        theframe().add_op($<opname>.made, |@args);
    }
}

class MoarAS::Frame {
    has $!node;
    has $!uid;
    has %!aliases;
    has @!locals;
    has %!lexicals;
    has %!labels;

    method new($uid = $*cuuid++) {
        my $frame := nqp::create(MoarAS::Frame);
        $frame.BUILD($uid);
        $frame;
    }

    method BUILD($uid) {
        $!uid := $uid;
        @!locals := nqp::list();
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
        $*cu.add_frame($!node);
        self;
    }

    method node() {
        self.init unless self.initialized;
        $!node;
    }

    method label($name) {
        nqp::existskey(%!labels, $name)
            ?? %!labels{$name}
            !! (%!labels{$name} := MAST::Label.new);
    }

    method lexical($name) {
        bailout("lexical '$name' not declared")
            unless nqp::existskey(%!lexicals, $name);

        %!lexicals{$name};
    }

    method rawlex($index, $frames_out = 0) {
        MAST::Lexical.new(:$index, :$frames_out);
    }

    method dealias($name) {
        bailout("alias '$name' not declared")
            unless nqp::existskey(%!aliases, $name);

        %!aliases{$name};
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
        my $local := MAST::Local.new(:$index);
        nqp::push(@!locals, $local);
        $local;
    }

    method add_alias($name, $id) {
        bailout("local %{$id} doesn't exist")
            if $id < 0 || $id >= +@!locals;

        bailout("alias '$name' already exists")
            if nqp::existskey(%!aliases, $name);

        %!aliases{$name} := @!locals[$id];
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
        my $call := MAST::Call.new(:$target, :@*flags, :$*result, |@args);
        nqp::push($!node.instructions, $call);

        @*flags := [];
        $*result := MAST::Node;

        $call;
    }
}

class MoarAS::Compiler {
    method parse(&lines) {
        my $*n := 0;
        my $*line;

        my $*cu := MAST::CompUnit.new;
        my $*cuuid := 0;
        my %*frames;
        my $*frame;

        my @*flags;
        my $*result := MAST::Node;

        while nqp::defined($*line := lines()) {
            $*n := $*n + 1;
            $*line := trim($*line);
            MoarAS::Grammar.parse($*line, actions => MoarAS::Actions);
        }

        $*cu;
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
