.hll asm
.frame main

.frame inner
    .locals str
    getlex %0 *0!1
    print %0

.frame main
    .locals int

    .local str OK 'ok'
    .lexical str ok $OK

    .local obj coderef
    .local obj context
    getcode $coderef &inner
    ctx $context
    forceouterctx $coderef $context

    .local obj inner
    getcode $inner &inner
    .call $inner

    exit %0
