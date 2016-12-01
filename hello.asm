# hello world example code

.hll asm
.frame main

.frame hello
.local str
.alias hello %0
const_s @hello 'hello world'
say @hello

.frame main
.local int obj
getcode %1 &hello
.call %1
exit %0
