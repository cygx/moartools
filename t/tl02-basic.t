#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

my @scripts =
    't/loop.tiny' => '54321',
    't/do.tiny' => 'ok',
    't/box.tiny' => '42';

plan +@scripts;
is ~run6(<moartl0 --run>, .key), .value, .key
    for @scripts;
