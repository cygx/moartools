#!/usr/bin/env perl6
use v6;
use Test;

plan 2;

my $tmpfile = 't/tl01.tmp';
my $perl6 = %*ENV<PERL6> || ~$*EXECUTABLE;
my $nqp = %*ENV<NQP> || $*EXECUTABLE.flip.subst('6lrep', 'pqn').flip;

my $asm = qqx{"$perl6" moartl t/hello.tiny};
like $asm, /^^ ".done\n" $/, 'compiled';

unlink $tmpfile;
spurt $tmpfile, $asm;

my $out = qqx{"$nqp" moaras --run $tmpfile};
is $out, "hello world\n", 'executed';
