#!/usr/bin/env perl6
use v6;
use Test;

plan 1;

my $perl6 = %*ENV<PERL6> || ~$*EXECUTABLE;
is qqx{"$perl6" moartl0 -r t/echo.tiny <t/42.txt}, '42', 'echoed 42';
