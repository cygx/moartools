.hll asm

.frame main
    .locals int str
    .local str OK 'ok'
    .lexical str ok $OK
    getlex %1 *0
    print %1
    exit %0
