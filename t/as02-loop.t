#!/usr/bin/env perl6
use v6;
use Test;

plan 1;
my $nqp = $*EXECUTABLE.flip.subst('6lrep', 'pqn').flip;
is qqx{"$nqp" moaras --run t/loop.asm}, '54321', 'loop';
