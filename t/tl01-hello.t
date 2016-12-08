#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

plan 2;

my $tmpfile = 't/tl01.tmp';

my $asm = ~run6 <moartl0 -d t/hello.tiny>;
like $asm, /^^ "# ok\n" $/, 'compiled';

unlink $tmpfile;
spurt $tmpfile, $asm;

my $out = ~run6 <moaras -r>, $tmpfile;
is $out, "hello world\n", 'executed';
