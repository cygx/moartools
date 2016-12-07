.hll asm

.frame main
    .locals int str
    .var str OK "ok"
    .lex str ok $OK
    getlex %1 *0
    print %1
    exit %0
