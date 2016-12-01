#!/usr/bin/env perl6
use v6;
use Test;

plan 1;
is qqx{$*EXECUTABLE moaras --run t/hello.asm}, "hello world\n", 'hello world';
