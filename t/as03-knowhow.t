#!/usr/bin/env perl6
use v6;
use Test;
use nqp;

use lib '.';
use t::run;

plan 5;

unlink 't/knowhow.moarvm';
is ~run6(<moaras --compile t/knowhow.asm>), '', 'assemble';

nqp::loadbytecode('t/knowhow.moarvm');
my $create := nqp::hllize(nqp::gethllsym('asm', 'create_knowhow_type'));
if $create ~~ Callable {
    ok 1, 'found frame';
    my $type := $create('Dummy');
    is $type.^name, 'Dummy', 'name';
    is $type.REPR, 'KnowHOWREPR', 'repr';
    is $type.HOW.^name, 'KnowHOW', 'how';
}
else { skip-rest 'failed to find frame' }
