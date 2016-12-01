# hello world example code

.hll asm

.frame main
.local int obj
return_i %0

.frame hello
.local str
const_s %0 'hello world'
print %0
return
