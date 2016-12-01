.hll asm
.frame main

.frame hello
.const[str] hello 'hello world'
say @hello

.frame main
.local int obj
getcode %1 &hello
.call %1
exit %0
