#!/usr/bin/env perl6
use v6;
use Test;

use lib '.';
use t::run;

plan 1;
is ~run-moar(<moardis.moarvm>), 'ok', 'can run moardis';
