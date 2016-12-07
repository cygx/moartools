.hll asm

.frame main
    .locals int obj
    getcode %1 &hello
    .call %1
    exit %0

.frame hello
    .var str hello "hello world"
    say $hello
