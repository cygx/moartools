#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

plan 1;
is ~run-perl6(<moartl --run t/loop.tiny>), '54321', 'ran loop';
