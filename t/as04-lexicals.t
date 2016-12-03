#!/usr/bin/env perl6
use v6;
use Test;

plan my \N = 1;
my $nqp = $*EXECUTABLE.flip.subst('6lrep', 'pqn').flip;
for ^N {
    my $n = N.fmt('%02i');
    is qqx{"$nqp" moaras --run t/lexicals$n.asm}, 'ok', "lexicals $n";
}
