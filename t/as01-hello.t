#!/usr/bin/env perl6
use v6;
use Test;

plan 1;
my $nqp = %*ENV<NQP> || $*EXECUTABLE.flip.subst('6lrep', 'pqn').flip;
is qqx{"$nqp" moaras --run t/hello.asm}, "hello world\n", 'hello world';
