.hll asm

.frame main
    .locals int str
    .var str OK "ok"
    .lex str ok $OK
    getlex %1 *ok
    print %1
    exit %0
