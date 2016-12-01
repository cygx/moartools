.hll asm

.frame main
.local int
return_i %0

.frame hello
.local str
const_s %0 'hello'
print %0
return
