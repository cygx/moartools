# hello world example code

.hll asm
.frame main

.frame hello
.local str
const_s %0 'hello world'
say %0
return

.frame main
.local int obj
getcode %1 &hello
.call %1
return_i %0
