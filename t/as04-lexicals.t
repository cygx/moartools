#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

plan my \N = 1;

for ^N {
    my $n = N.fmt('%02i');
    is ~run6(<moaras --run>, "t/lexicals$n.asm"), 'ok', "lexicals $n";
}
