#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

plan 1;
is ~run6(<moaras -r t/hello.asm>), "hello world\n", 'hello world';
