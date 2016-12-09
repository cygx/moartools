# Copyright 2016 cygx <cygx@cpan.org>
# Distributed under the Boost Software License, Version 1.0

use v6;

my &asm = &put;

class Op { ... }

sub op(*@signature, :$suffix, :$dummy) {
    Op.new(:@signature, :$suffix, :$dummy);
}

my \OPS = {
    bindlex         => op(<* O>),
    boothash        => op(<O>),
    bootintarray    => op(<O>),
    chars           => op(<I S>),
    chr             => op(<S I>),
    close           => op(<O>, :suffix<fh>),
    create          => op(<O O>),
    ctx             => op(<O>),
    dec             => op(<I>, :suffix<i>),
    exit            => op(<I>),
    forceouterctx   => op(<O O>),
    getcode         => op(<O &>),
    getcurhllsym    => op(<O S>),
    getlexrel       => op(<O O S>),
    getstdin        => op(<O>),
    getstdout       => op(<O>),
    newtype         => op(<O O S>),
    open            => op(<O S S>, :suffix<fh>),
    print           => op(<S>),
    readline        => op(<S O>, :suffix<fh>),
    reprname        => op(<S O>),
    say             => op(<S>),
}

my \MULTIOPS = {
    atkey => %(
        op(<I O S>, :suffix<i>).pair,
        op(<N O S>, :suffix<n>).pair,
        op(<S O S>, :suffix<s>).pair,
        op(<O O S>, :suffix<o>).pair,
    ),
    atpos => %(
        op(<I O I>, :suffix<i>).pair,
        op(<N O I>, :suffix<n>).pair,
        op(<S O I>, :suffix<s>).pair,
        op(<O O I>, :suffix<o>).pair,
    ),
    bindcurhllsym => %(
        op(<O S O>).pair,
        op(<S O>, :dummy(+0 => 1)).pair,
    ),
    bindkey => %(
        op(<O S I>, :suffix<i>).pair,
        op(<O S N>, :suffix<n>).pair,
        op(<O S S>, :suffix<s>).pair,
        op(<O S O>, :suffix<o>).pair,
    ),
    composetype => %(
        op(<O O O>).pair,
        op(<O O>, :dummy(+0 => 0)).pair,
    ),
    getlex => %(
        op(<I s>, :suffix<ni>).pair,
        op(<N s>, :suffix<nn>).pair,
        op(<S s>, :suffix<ns>).pair,
        op(<O s>, :suffix<no>).pair,
    ),
    loadbytecode => %(
        op(<S S>).pair,
        op(<S>, :dummy(+0 => 0)).pair,
    ),
    read => %(
        op(<O O I>, :suffix<fhb>).pair,
        op(<S O I>, :suffix<fhs>).pair,
    ),
    return => %(
        op(<I>, :suffix<i>).pair,
        op(<N>, :suffix<n>).pair,
        op(<S>, :suffix<s>).pair,
        op(<O>, :suffix<o>).pair,
        op().pair,
    ),
    set => %(
        op(<I I>).pair,
        op(<N N>).pair,
        op(<S S>).pair,
        op(<O O>).pair,
    ),
    unbox => %(
        op(<I O>, :suffix<i>).pair,
        op(<N O>, :suffix<n>).pair,
        op(<S O>, :suffix<s>).pair,
    ),
}

my @types = <int num str obj>;
my @ops = OPS.keys;
my @multiops = MULTIOPS.keys;

my $lines;
my $line;
my $n;

my %unit;
my @scopes = $(%unit);
my %lexpads;
my %lexchain;
my $blocks;
my $doblock;
my $dovar;
my $frame;
my $block;
my $vars;
my %temps;
my %temptops;

class MoarTLException is Exception {
    has $.msg;
    has $.n;
    has $.line;
    method new($msg) { self.bless(:$msg, :$n, :$line) }
    method message { "{$.type}: $!msg\n[$!n] $!line" }
    method type { !!! }
}

class X::MoarTL::Panic is MoarTLException { method type { 'panic' } }
class X::MoarTL::Syntax is MoarTLException { method type { 'syntax error' } }
class X::MoarTL::Lexical is MoarTLException { method type { 'lexical error' } }

multi bailout($msg = '?') is hidden-from-backtrace {
    die X::MoarTL::Panic.new($msg);
}

multi bailout($msg = '?', :$syn!) is hidden-from-backtrace {
    die X::MoarTL::Syntax.new($msg);
}

multi bailout($msg = '?', :$lex!) is hidden-from-backtrace {
    die X::MoarTL::Lexical.new($msg);
}

class Block { ... }
class Coderef { ... }
class Lexref { ... }
class Var { ... }
class Tmp { ... }
class Lex { ... }
class IVal { ... }
class NVal { ... }
class SVal { ... }
class Noop { ... }
class Cast { ... }
class Delex { ... }
class IBox { ... }
class CodeBox { ... }

sub doblock {
    $doblock // bailout('not inside a do block');
}

sub tmp($type, $init = Noop) {
    my $id = %temptops{$type}++;
    my $tmp = %temps{$type}[$id] //= Tmp.new(:$type, :$id).declare;
    $init.init($tmp);
    $tmp;
}

sub var($type, $name, $init = Noop) {
    my $var = Var.new(:$name, :$type, id => $vars++).declare;
    $init.init($var);
    $var;
}

sub next-line {
    my $next := $lines.pull-one;
    return $next if $next =:= IterationEnd;
    ++$n;
    $line = $next.trim;
}

sub frame($name) { %unit{$name} }

sub lookup($name) {
    for @scopes {
        return .{$name}
            if .{$name}:exists;
    }

    Nil;
}

sub lexlookup($name) {
    my $current = $frame;
    my $i = 0;
    loop {
        if defined my $pad = %lexpads{$current} {
            my $lex = $pad{$name};
            return $lex.ref($i) if defined $lex;
        }

        ++$i;
        $current = %lexchain{$current} // last;
    }
}

sub find_multi($op, $argsig) {
    my %multi := MULTIOPS{$op};
    %multi{$argsig} // do {
        my @candis = %multi.keys.grep: {
            .lc eq $argsig.lc and [&&] .comb Zle $argsig.comb;
        }

        if @candis == 0 { bailout 'wrong operands' }
        elsif @candis > 1 { bailout 'ambiguous operands' }

        %multi{@candis[0]};
    }
}

sub parse-frame { ... }
sub parse-block($name) { ... }
sub parse-lexpad($name) { ... }

sub parse(Iterable $src --> Nil) {
    asm ".hll tiny";

    $n = 0;
    $lines := $src.iterator;

    while ($_ := next-line) !=:= IterationEnd { /^[
        | [\#|$]
        | (:s fn (\w+)${
            my $name = ~$0;
            %unit{$name} //= Coderef.new(:$name);
            asm ".frame $name";
        })
        | (:s lex (\w+) '{'${ parse-lexpad ~$0 })
        | (:s lex (\w+) ':' (\w+)${
            my $child := ~$0;
            my $parent := ~$1;
            bailout "lexical environment for '$child' already exists"
                if %lexchain{$child}:exists;

            bailout "unknown lexical environment '$parent'"
                unless %lexpads{$parent}:exists;

            %lexchain{$child} = $parent;
        })
        | (:s ld (\w+)'()' '{'${ parse-frame ~$0, :load })
        | (:s fn (\w+)'()' '{'${ parse-frame ~$0 })
        || {bailout}
    ]/ }

    asm '# ok';
}

sub iv(Int() $i) { IVal.new(:$i) }
sub nv(Num() $n) { NVal.new(:$n) }
sub sv(Str() $s) { SVal.new(:$s) }
sub cast($expr, $type) { Cast.new(:$expr, :$type) }

sub objectify($expr) {
    given $expr.type {
        when 'int' { IBox.new(:$expr) }
        default { bailout "cannot objectify '$_'" }
    }
}

sub sig($_) {
    when 'int' { 'i' }
    when 'num' { 'n' }
    when 'str' { 's' }
    when 'obj' { 'o' }
    default { bailout "no sig for type $_" }
}

sub extsig($_) {
    when 'int' { 'i64' }
    when 'num' { 'n64' }
    when 'str' { 's' }
    default { bailout "no extended sig for type $_" }
}

sub argsig(*@args) { @args>>.sig.join }
sub multisig(*@args) { @args>>.multisig.join }

my token subexpression {
    | ((\w+) <?{ lookup(~$0) ~~ Var|Coderef }> { push @*made, lookup(~$0) })
    | ((\w+) <?{ lexlookup(~$0) ~~ Lexref }> { push @*made, lexlookup(~$0) })
    | ((\d+ '.' \d+ ['e' <[+-]>? \d+ ]?) { push @*made, nv(~$0) })
    | ((\d+) { push @*made, iv(~$0) })
    | ('"' (<-["]>*) '"' { push @*made, sv(~$0) })
}

my token expression {
    | ((str) '(' <&subexpression> ')' { push @*made, cast(@*made.pop, ~$0) })
    | ((obj) '(' <&subexpression> ')' { push @*made, objectify(@*made.pop) })
    | <&subexpression>
    || { bailout 'failed to parse arguments' }
}

sub is-label($name) {
    lookup($name) ~~ Block || bailout :lex, "unknown label '$name'";
}

sub parse-lexpad($name) {
    asm ".frame $name";

    %unit{$name} //= Coderef.new(:$name);
    $frame = $name;
    %temps = ();

    my %pad := %lexpads{$name} //= {};
    my %scope;

    while ($_ := next-line) !=:= IterationEnd {
        %temptops = ();
        /^[
        | [\#|$]
        | (:s use (\w+)${
            my $name = ~$0;
            bailout "local '$name' already exists"
                if %scope{$name}:exists;

            my $var = var('obj', $name);
            %scope{$name} = $var;

            my $tmpname = tmp('str', sv($name));
            my $path = tmp('str', sv("lib/{$name}.moarvm"));

            asm "    loadbytecode {$path.eval} {$path.eval}";
            asm "    getcurhllsym {$var.eval} {$tmpname.eval}";
        })
        | (:s import (@types) (\w+) from (\w+)${
            my ($type, $name, $mod) = ~<<$/;
            bailout "'{$mod}' not declared"
                unless %scope{$mod}:exists;

            bailout "'{$name}' already declared", :lex
                if %pad{$name}:exists;

            %pad{$name} = Lex.new(:$name, :$type, index => +%pad);
            my $modvar = %scope{$mod};

            asm "    .lex $type $name";

            my $tmp = tmp($type);
            my $str = tmp('str', sv($name));

            asm "    getlexrel {$tmp.eval} {$modvar.eval} {$str.eval}";
            asm "    bindlex *{$name} {$tmp.eval}";
        })
        | (:s (@types) (\w+)${
            my $type = ~$0;
            my $name = ~$1;
            bailout "'$name' already declared", :lex
                if %pad{$name}:exists;

            %pad{$name} = Lex.new(:$name, :$type, index => +%pad);
            asm "    .lex $type $name";
        })
        | ('}' ${ return })
        || {bailout}
        ]/
    }

    bailout 'unclosed block';
}

sub parse-frame($name, :$load, :$main) {
    asm ".frame $name";
    asm '.set load' if $load;
    asm '.set main' if $main;

    %unit{$name} //= Coderef.new(:$name);
    $blocks = 0;
    $frame = $name;
    $vars = 0;
    %temps = ();
    %temptops = ();

    parse-block $name;
}

sub parse-block($blockname, :$do) {
    my %scope;
    temp $block = Block.new(name => $blockname, id => $blocks++);
    temp $doblock = $block if $do;
    temp $dovar = $do if $do;
    %scope{$blockname} = $block;
    @scopes.unshift(%scope);
    LEAVE @scopes.shift;

    asm ".label {$block.bra}";

    my @*made;
    while ($_ := next-line) !=:= IterationEnd {
        %temptops = ();
        /^[
        | [\#|$]
        | (:s '.'(\w+) '{'${ parse-block ~$0 })
        | (:s do '{'${ parse-block 'do', :do })
        | (:s done <?{ $dovar ~~ Var }><expression>${
            asm find_multi('set', argsig($dovar, @*made[0]))
                .eval('set', $dovar, @*made[0]);
            asm "    goto \@{doblock.ket}";
            @*made = ();
        })
        | ('}' ${
            asm ".label {$block.ket}";
            return;
        })
        | (:s (@types) (\w+)${
            my ($type, $name) = ~<<$/;
            %scope{$name} = var($type, $name);
        })
        | (:s (@types) (\w+) '=' do '{'${
            my ($type, $name) = ~<<$/;
            my $var = var($type, $name);
            %scope{$name} = $var;
            parse-block 'do', do => $var;
        })
        | (:s (int) (\w+) '=' (\d+)${
            my ($type, $name, $value) = $/>>.Str;
            my $var = var($type, $name, iv($value));
            %scope{$name} = $var;
        })
        | (:s next (\w+)$ {is-label ~$0}{
            asm "    goto \@{lookup(~$0).bra}";
        })
        | (:s next (\w+) {is-label ~$0} if <expression>${
            asm "    if_i {@*made[0].eval} \@{lookup(~$0).bra}";
            @*made = ();
        })
        | (:s break (\w+) {is-label ~$0} unless <expression>${
            asm "    unless_i {@*made[0].eval} \@{lookup(~$0).ket}";
            @*made = ();
        })
        | (:s done <?{ $dovar !~~ Var }>unless <expression>${
            asm "    unless_i {@*made[0].eval} \@{doblock.ket}";
            @*made = ();
        })
        | (:s redo${
            asm "    goto \@{doblock.bra}";
        })
        | (:s (@ops) <expression>**{OPS{$0}.arity}%[<.ws>?','<.ws>?]${
            asm OPS{~$0}.eval(~$0, @*made);
            @*made = ();
        })
        | (:s (@types) (\w+) '=' (@multiops)[' '|$]{
            bailout 'cannot initialize multi-ops for now';
        })
        | (:s (@types) (\w+) '=' (@ops)<?{OPS{$2}.arity-1 == 0}>${
            my ($type, $name, $op) = ~<<$/;
            my $var = var($type, $name);
            %scope{$name} = $var;
            @*made = $var;
            asm OPS{$op}.eval($op, @*made);
            @*made = ();
        })
        | (:s (@types) (\w+) '=' (@ops)
                <expression>**{OPS{$2}.arity-1}%[<.ws>?','<.ws>?]${
            my ($type, $name, $op) = ~<<$/[^3];
            my $var = var($type, $name);
            %scope{$name} = $var;
            unshift @*made, $var;
            asm OPS{$op}.eval($op, @*made);
            @*made = ();
        })
        | (:s (@types) (\w+) '=' <expression>${
            my $type = ~$0;
            my $name = ~$1;
            my $arg = @*made.pop;
            bailout 'type mismatch' unless $arg.type eq $type;
            my $var = var($type, $name, $arg);
            %scope{$name} = $var;
        })
        | (:s (\w+) '=' <?{ lookup(~$0) ~~ Var }>(@ops)
                <expression>**{OPS{$1}.arity-1}%[<.ws>?','<.ws>?]${
            my ($varname, $op) = ~<<$/;
            @*made.unshift(lookup($varname));
            asm OPS{$op}.eval($op, @*made);
            @*made = ();
        })
        | (:s (\w+) '=' <?{ lookup(~$0) ~~ Var }>(@multiops)[' '|$]{
            bailout 'cannot assign multi-ops for now';
        })
        | (:s (\w+) ':=' <expression>${
            bailout "TODO $?LINE"
        })
        | (:s (@multiops) <expression>*%[<.ws>?','<.ws>?]${
            asm find_multi(~$0, multisig(@*made)).eval(~$0, @*made);
            @*made = ();
        })
        | (:s lex (\w+) '=' <expression>${
            my $arg = @*made[0];
            my $name = ~$0;
            my $type = $arg.type;

            my %pad := %lexpads{$frame} //= {};
            bailout "lexical '$name' already declared"
                if %pad{$name}:exists;

            %pad{$name} = Lex.new(:$name, :$type, index => +%pad);
            asm "    .lex {$type} {$name}";
            asm "    bindlex *{$name} {$arg.promote.eval}";
            @*made = ();
        })
        | (:s (\w+)'()'<?{ frame(~$0) ~~ Coderef }>${
            my $tmp = tmp('obj');
            asm "    getcode {$tmp.eval} \&{$0}";
            asm "    .call {$tmp.eval}";
            @*made = ();
        })
        || {bailout}
        ]/
    }

    bailout 'unclosed block';
}

class Op {
    has @.signature;
    has $.suffix;
    has $.dummy;
    method suffix { $!suffix ?? "_{$!suffix}" !! '' }
    method arity { +@!signature }
    method pair { $.parsig => self }
    method parsig { @!signature.join }
    method eval($op, *@args) {
        # FIXME: promote first, copy dummy second
        my @sigs = @!signature;
        if $!dummy {
            @args.splice($!dummy.key, 0, @args[$!dummy.value]);
            @sigs.splice($!dummy.key, 0, @sigs[$!dummy.value]);
        }
        "    {$op}{$.suffix}" ~ join '', @args.kv.map: -> $i, $arg {
            ' ' ~ do given @sigs[$i] {
                when any <i s o & *> { $arg.eval }
                when any <I S O> { $arg.promote.eval }
                default { die "panic: unexpected parameter sig '$_'" }
            }
        }
    }
}

class Block {
    has $.name;
    has $.id;
    has %.temps;
    method type { 'block' }
    method bra { "bra{$!id}_{$!name}" }
    method ket { "ket{$!id}_{$!name}" }
}

class Coderef {
    has $.name;
    method type { 'coderef' }
    method sig { '&' }
    method multisig { 'O' }
    method eval { "\&{$!name}" }
    method promote { tmp 'obj', CodeBox.new(ref => self) }
}

class Lexref {
    has $.lex;
    method type { 'lexref' }
    method sig { '*' }
    method multisig { uc sig $!lex.type }
    method eval { "*{$!lex.name}" }
    method promote { tmp $!lex.type, Delex.new(ref => self) }
}

class Outerref is Lexref {
    has $.outer;
    method eval { "*{$.lex.index}!{$!outer}" }
}

role Local {
    has $.type;
    has $.id;
    has $!declared;
    method longname { ... }
    method sig { uc sig $!type }
    method multisig { self.sig }
    method promote { self }
    method eval { "\${$.longname}" }
    method declare {
        asm "    .var {$.type} {$.longname}";
        self;
    }
}

class Var does Local {
    has $.name;
    method longname { "v{$.id}_{$!name}" }
}

class Tmp does Local {
    method longname { "{sig $.type}{$.id}" }
}

class Lex {
    has $.type;
    has $.name;
    has $.index;
    method ref($outer = 0) {
        $outer
            ?? Outerref.new(lex => self, :$outer)
            !! Lexref.new(lex => self);
    }
}

role Value {
    method type { ... }
    method sig { sig self.type }
    method multisig { self.sig }
    method eval { ... }
    method promote { tmp($.type, self) }
    method init($target) {
        asm "    const_{extsig $.type} {$target.eval} {$.eval}";
    }
}

class IVal does Value {
    has $.i;
    method type { 'int' }
    method eval { "$!i" }
}

class NVal does Value {
    has $.n;
    method type { 'num' }
    method eval { "$!n" }
}

class SVal does Value {
    has $.s;
    method type { 'str' }
    method eval { "\"{$.enc}\"" }
    sub enc($_) {
        when ^0x100 { .fmt("\\%02X") }
        when ^0x10000 { .fmt("\\u%04X") }
        when ^0x110000 { .fmt("\\U%06X") }
    }
    method enc { $!s.subst(:g, /\'|\v/, { .Str.NFC.map(&enc).join }) }
}

class Noop {
    method init($target) {}
}

class Cast {
    has $.expr;
    has $.type;
    method sig { uc sig $!type }
    method multisig { self.sig }
    method promote { tmp($!type, self) }
    method init($target) {
        asm "    coerce_{sig $!expr.type}{sig $!type} "
            ~ "{$target.eval} {$!expr.eval}";
    }
}

class Delex {
    has $.ref;
    method type { $!ref.lex.type }
    method sig { uc sig $.type }
    method multisig { self.sig }
    method init($target) { asm "    getlex {$target.eval} {$!ref.eval}" }
}

class CodeBox {
    has $.ref;
    method type { 'obj' }
    method sig { 'O' }
    method multisig { 'O' }
    method init($target) { asm "    getcode {$target.eval} {$!ref.eval}" }
}

class IBox {
    has $.expr;
    method type { 'obj' }
    method sig { 'O' }
    method multisig { 'O' }
    method promote { tmp('obj', self) }
    method init($target) {
        my $eval = $target.eval;
        asm "    bootint {$eval}";
        asm "    box_i {$eval} {$!expr.promote.eval} {$eval}";
    }
}

sub as {
    once {
        use nqp;
        nqp::loadbytecode('MoarAS.moarvm');
        nqp::gethllsym('nqp', 'MoarAS::Compiler');
    }
}

sub dest($_) {
    .subst(/'.tiny'?$/, '.moarvm');
}

sub capture-asm(&block) {
    my @asm;
    temp &asm = { @asm.push("$_\n") }
    block;
    @asm.join;
}

proto MAIN(|) is export(:MAIN) {
    CATCH {
        note "$_\n" ~ .backtrace.grep(none *.is-hidden, *.is-setting)[^2].join;
        exit 1;
    }

    @*ARGS.shift;
    {*}
}

multi MAIN(Str $src?, Bool :parse(:$p)!) {
    temp &asm = -> $ {}
    parse lines;
}

multi MAIN(Str $src?, Bool :dump(:$d)!) {
    parse lines;
}

multi MAIN(Str $src!, Bool :compile(:$c)!) {
    as.compile_code(capture-asm({ parse lines }), dest($src));
}

multi MAIN(Str $src?, Str :compile-to($dest)!) {
    as.compile_code(capture-asm({ parse lines }), $dest);
}

multi MAIN(Str $src?, Bool :run(:$r)!) {
    as.eval_code(capture-asm({ parse lines }));
}
