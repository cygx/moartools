#!/usr/bin/env perl6
use v6;
use Test;

plan 2;

my $tmpfile = 't/tl02.tmp';
my $perl6 = %*ENV<PERL6> || ~$*EXECUTABLE;
my $nqp = %*ENV<NQP> || $*EXECUTABLE.flip.subst('6lrep', 'pqn').flip;

my $asm = qqx{"$perl6" moartl t/loop.tiny};
like $asm, /^^ ".done\n" $/, 'compiled';

unlink $tmpfile;
spurt $tmpfile, $asm;

my $out = qqx{"$nqp" moaras --run $tmpfile};
is $out, '54321', 'executed';
