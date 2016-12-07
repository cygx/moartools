#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

my @scripts =
    't/loop.asm' => '54321',
    't/alias.asm' => 'ok';

plan +@scripts;
is ~run6(<moaras --run>, .key), .value, .key
    for @scripts;
