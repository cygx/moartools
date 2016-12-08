#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

plan my \N = 3;

for 1..N {
    my $n = .fmt('%02i');
    is ~run6(<moaras -r>, "t/lexicals$n.asm"), 'ok', "lexicals $n";
}
