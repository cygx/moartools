#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

plan 2;

my $tmpfile = 't/tl01.tmp';

my $asm = ~run-perl6 <moartl0 t/hello.tiny>;
like $asm, /^^ ".done\n" $/, 'compiled';

unlink $tmpfile;
spurt $tmpfile, $asm;

my $out = ~run-nqp <<moaras --run $tmpfile>>;
is $out, "hello world\n", 'executed';
