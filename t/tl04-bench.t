#!/usr/bin/env perl6
use v6;
use Test;

use nqp;
use lib '.';
use t::run;

plan 3;

my \N = 400_000;

my $parse_lines_p6 := do {
    use t::bench;
    &parse_lines;
}

my $parse_lines_tiny := try {
    unlink 't/bench.moarvm';
    run6 <moartl0 -c t/bench.tiny>, :no-err;
    nqp::loadbytecode('t/bench.moarvm');
    nqp::hllize(nqp::gethllsym('tiny', 'parse_lines'));
}

ok $parse_lines_tiny, 'compiled t/bench.tiny';

unless $parse_lines_tiny {
    skip 1;
    exit;
}

print '  generating input...';
my $input := do {
    my $lines := nqp::list();
    nqp::push($lines, "$_\t42\n") for ^N;
    nqp::join('', $lines);
}
print "done.\n";

sub bench($name, &code) {
    use nqp;
    my $start = now;
    my $words := code($input);
    my $end = now;
    put "  {+$words} words parsed in {($end - $start).round(0.01)}s ($name)";
    +$words;
}

is $_, N, 'ran p6 benchmark' given bench 'p6', $parse_lines_p6;
is $_, N, 'ran tiny benchmark' given try bench 'tiny', $parse_lines_tiny;
