# Copyright 2016 cygx <cygx@cpan.org>
# Distributed under the Boost Software License, Version 1.0

use v6;

class Op { ... }

sub op(*@signature, :$suffix, :$dummy) {
    Op.new(:@signature, :$suffix, :$dummy);
}

my \OPS = {
    bindcurhllsym   => op(<O S O>),
    boothash        => op(<O>),
    bootintarray    => op(<O>),
    chars           => op(<I S>),
    chr             => op(<S I>),
    close           => op(<O>, :suffix<fh>),
    create          => op(<O O>),
    composetype     => op(<O O O>),
    ctx             => op(<O>),
    dec             => op(<I>, :suffix<i>),
    exit            => op(<I>),
    forceouterctx   => op(<O O>),
    getcode         => op(<O &>),
    getcurhllsym    => op(<O S>),
    getlexrel       => op(<O O S>),
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
    bindkey => %(
        op(<O S I>, :suffix<i>).pair,
        op(<O S N>, :suffix<n>).pair,
        op(<O S S>, :suffix<s>).pair,
        op(<O S O>, :suffix<o>).pair,
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

my @scopes;
my $blocks;

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
class Var { ... }
class Tmp { ... }
class IVal { ... }
class SVal { ... }
class Noop { ... }
class Const { ... }
class Cast { ... }
class IBox { ... }

sub next-line {
    my $next := $lines.pull-one;
    return $next if $next =:= IterationEnd;
    ++$n;
    $line = $next.trim;
}

sub lookup($name) {
    for @scopes {
        return .{$name}
            if .{$name}:exists;
    }

    Nil;
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

sub block($name) { ... }

sub parse($src --> Nil) {
    put ".hll tiny";

    my %*scope;
    @scopes = $(%*scope);

    $n = 0;
    $lines := $src.IO.lines(:close).iterator();

    while ($_ := next-line) !=:= IterationEnd { /^[
        | [\#|$]
        | (:s fn (\w+)${
            my $name = ~$0;
            %*scope{$name} //= Coderef.new(:$name);
            put ".frame $name";
        })
        | (:s ld (\w+)'()' '{'${
            my $name = ~$0;
            %*scope{$name} //= Coderef.new(:$name);
            put ".frame $name";
            put ".set load";
            $blocks = 0;
            block $name;
        })
        | (:s fn (\w+)'()' '{'${
            my $name = ~$0;
            %*scope{$name} //= Coderef.new(:$name);
            put ".frame $name";
            $blocks = 0;
            block "__$name";
        })
        || {bailout}
    ]/ }

    put '# ok';
}

sub iv(Int() $i) { IVal.new(:$i) }
sub sv(Str() $s) { SVal.new(:$s) }
sub const($value) { Const.new(:$value) }
sub cast($expr, $type) { Cast.new(:$expr, :$type) }

sub box($expr) {
    given $expr.type {
        when 'int' { IBox.new(:$expr) }
        default { bailout 'cannot box that' }
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
    when 'str' { 's' }
    default { bailout "no extended sig for type $_" }
}

sub argsig(*@args) {
    @args>>.sig.join;
}

my token subexpression {
    | ((\w+) <?{ lookup(~$0) ~~ Var|Coderef }> { push @*made, lookup(~$0) })
    | ((\d+) { push @*made, iv(~$0) })
    | ("'" (<-[']>*) "'" { push @*made, sv(~$0) })
}

my token expression {
    | ((str) '(' <&subexpression> ')' { push @*made, cast(@*made.pop, ~$0) })
    | ((obj) '(' <&subexpression> ')' { push @*made, box(@*made.pop) })
    | <&subexpression>
    || { bailout 'failed to parse arguments' }
}

sub is-label($name) {
    lookup($name) ~~ Block || bailout :lex, "unknown label '$name'";
}

sub block($blockname) {
    my %*scope;
    my $*block = Block.new(name => $blockname, id => $blocks++);
    %*scope{$blockname} = $*block;
    @scopes.unshift(%*scope);
    LEAVE @scopes.shift;

    put ".label {$*block.bra}";

    my @*made;
    while ($_ := next-line) !=:= IterationEnd { /^[
        | [\#|$]
        | (:s '.'(\w+) '{'${ block ~$0 })
        | (:s do '{'${ block "do{$*block.temps<do>++}" })
        | (:s done <expression>${
            bailout 'done outside do block' unless $*dovar;
            put find_multi('set', argsig($*dovar, @*made[0]))
                .eval('set', $*dovar, @*made[0]);
            @*made = ();
        })
        | ('}' ${
            put ".label {$*block.ket}";
            return;
        })
        | (:s (@types) (\w+)${
            my ($type, $name) = ~<<$/;
            my $init = Noop;
            my $var = Var.new(:$name, :$type, :$*block, :$init);
            (%*scope{$name} = $var).declare;
        })
        | (:s (@types) (\w+) '=' do '{'${
            my ($type, $name) = ~<<$/;
            my $init = Noop;
            my $var = Var.new(:$name, :$type, :$*block, :$init);
            (%*scope{$name} = $var).declare;
            my $*dovar = $var;
            block "do{$*block.temps<do>++}"
        })
        | (:s (int) (\w+) '=' (\d+)${
            my ($type, $name, $value) = $/>>.Str;
            my $init = const(iv($value));
            my $var = Var.new(:$name, :$type, :$*block, :$init);
            (%*scope{$name} = $var).declare;
        })
        | (:s next (\w+)$ {is-label ~$0}{
            put "    goto \@{lookup(~$0).bra}";
        })
        | (:s next (\w+) {is-label ~$0} if <expression>${
            put "    if_i {@*made[0].eval} \@{lookup(~$0).bra}";
            @*made = ();
        })
        | (:s break (\w+) {is-label ~$0} unless <expression>${
            put "    unless_i {@*made[0].eval} \@{lookup(~$0).ket}";
            @*made = ();
        })
        | (:s (@ops) <expression>**{OPS{$0}.arity}%[<.ws>?','<.ws>?]${
            put OPS{~$0}.eval(~$0, @*made);
            @*made = ();
        })
        | (:s (@types) (\w+) '=' (@multiops)[' '|$]{
            bailout 'cannot initialize multi-ops for now';
        })
        | (:s (@types) (\w+) '=' (@ops)<?{OPS{$2}.arity-1 == 0}>${
            my ($type, $name, $op) = ~<<$/;
            my $init = Noop;
            my $var = Var.new(:$name, :$type, :$*block, :$init);
            (%*scope{$name} = $var).declare;
            @*made = $var;
            put OPS{$op}.eval($op, @*made);
            @*made = ();
        })
        | (:s (@types) (\w+) '=' (@ops)
                <expression>**{OPS{$2}.arity-1}%[<.ws>?','<.ws>?]${
            my ($type, $name, $op) = ~<<$/[^3];
            my $init = Noop;
            my $var = Var.new(:$name, :$type, :$*block, :$init);
            (%*scope{$name} = $var).declare;
            unshift @*made, $var;
            put OPS{$op}.eval($op, @*made);
            @*made = ();
        })
        | (:s (@types) (\w+) '=' <expression>${
            my $type = ~$0;
            my $name = ~$1;
            my $arg = @*made.pop;
            bailout 'type mismatch' unless $arg.type eq $type;
            my $init = $arg.init;
            my $var = Var.new(:$name, :$type, :$*block, :$init);
            (%*scope{$name} = $var).declare;
        })
        | (:s (\w+) '=' <?{ lookup(~$0) ~~ Var }>(@ops)
                <expression>**{OPS{$1}.arity-1}%[<.ws>?','<.ws>?]${
            my ($varname, $op) = ~<<$/;
            @*made.unshift(lookup($varname));
            put OPS{$op}.eval($op, @*made);
            @*made = ();
        })
        | (:s (\w+) '=' <?{ lookup(~$0) ~~ Var }>(@multiops)[' '|$]{
            bailout 'cannot assign multi-ops for now';
        })
        | (:s (\w+) ':=' <expression>${
            bailout "TODO $?LINE"
        })
        | (:s (@multiops) <expression>*%[<.ws>?','<.ws>?]${
            put find_multi(~$0, argsig(@*made)).eval(~$0, @*made);
            @*made = ();
        })
        | (:s lex (\w+) '=' <expression>${
            my $arg = @*made[0];
            put "    .lexical {$arg.type} {$0}";
            put "    bindlex *{$0} {$arg.promote.eval}";
            @*made = ();
        })
        || {bailout}
    ]/ }

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
                when any <i s o &> { $arg.eval }
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
    method eval { "\&{$!name}" }
}

role Alias {
    has $.type;
    has $.block;
    has $.init;
    has $!declared;
    method longname { ... }
    method sig { uc sig $!type }
    method declare {
        $!declared = True;
        put "    .local {$.type} {$.longname}";
        .put with $!init.eval("\${$.longname}");
    }
    method eval {
        self.declare unless $!declared;
        "\${$.longname}";
    }
}

class Var does Alias {
    has $.name;
    method longname { "var{$!block.id}_{$!name}" }
    method promote { self }
}

class Tmp does Alias {
    has $.id;
    submethod TWEAK { $!id = $!block.temps{$!type}++ }
    method longname { "tmp{$!block.id}_{sig $.type}{$!id}" }
    method promote { self }
}

role Value {
    method type { ... }
    method sig { sig self.type }
    method eval { ... }
    method promote { Tmp.new(:$.type, :$*block, init => const(self)) }
    method init { const(self) }
}

class IVal does Value {
    has $.i;
    method type { 'int' }
    method eval { "$!i" }
}

class SVal does Value {
    has $.s;
    method type { 'str' }
    method eval { "'{$.enc}'" }
    sub enc($_) {
        when ^0x100 { .fmt("\\%02X") }
        when ^0x10000 { .fmt("\\u%04X") }
        when ^0x110000 { .fmt("\\U%06X") }
    }
    method enc { $!s.subst(:g, /\'|\v/, { .Str.NFC.map(&enc).join }) }
}

class Noop {
    method eval($target) {}
}

class Const {
    has $.value;
    method eval($target) {
        "    const_{extsig $!value.type} {$target} {$!value.eval}";
    }
}

class Cast {
    has $.expr;
    has $.type;
    method sig { uc sig $!type }
    method promote { Tmp.new(:$!type, :$*block, init => self) }
    method init { self }
    method eval($target) {
        "    coerce_{sig $!expr.type}{sig $!type} {$target} {$!expr.eval}";
    }
}

class IBox {
    has $.expr;
    method type { 'obj' }
    method sig { 'O' }
    method promote { Tmp.new(:type<obj>, :$*block, init => self) }
    method init { self }
    method eval($target) {
        "    bootint {$target}\n" ~
        "    box_i {$target} {$!expr.promote.eval} {$target}";
    }
}

sub capture-out(&block) {
    my @out;
    temp $*OUT = class { method print($_) { @out.push($_) } }
    block;
    @out.join;
}

sub ignore-out(&block) {
    temp $*OUT = class { method print($) {} }
    block;
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

sub trace {
    %*ENV<MOAR_TRACE>.defined &&  %*ENV<MOAR_TRACE> eq '1';
}

proto MAIN(|c) is export(:MAIN) {
    CATCH {
        note trace() ?? $_ !! ~$_;
        exit 1;
    }
    {*}
}

multi MAIN(Str $src, Bool :$parse!) {
    ignore-out { parse $src }
}

multi MAIN(Str $src, Str $dest = dest($src), Bool :$compile!) { 
    as.compile_code(capture-out({ parse $src }), $dest);
}

multi MAIN(Str $src, Bool :$dump!) {
    print capture-out({ parse $src });
}

multi MAIN(Str $src, *@args, Bool :$run!) {
    as.eval_code(capture-out({ parse $src }), |@args);
}

multi MAIN(Bool :$parse-stdin!) { MAIN '-', :parse }
multi MAIN(Str $dest, Bool :$compile-stdin!) { MAIN '-', $dest, :compile }
multi MAIN(Bool :$dump-stdin!) { MAIN '-', :dump }
multi MAIN(*@args, Bool :$run-stdin!) { MAIN '-', @args, :run }
