#!/usr/bin/env perl6
use v6;
use Test;
use nqp;

plan 3;

unlink 't/knowhow.moarvm';
qqx{$*EXECUTABLE moaras -x t/knowhow.asm};

nqp::loadbytecode('t/knowhow.moarvm');
my &create := nqp::hllize(nqp::gethllsym('asm', 'create_knowhow_type'));
my $type := create('Dummy');

is $type.^name, 'Dummy', 'name';
is $type.REPR, 'KnowHOWREPR', 'repr';
is $type.HOW.^name, 'KnowHOW', 'how';
