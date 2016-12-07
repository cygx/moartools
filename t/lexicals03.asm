.hll asm
.frame main

.frame inner
    .local str
    getlex %0 *0!1
    print %0

.frame main
    .local int

    .var str OK "ok"
    .lex str ok $OK

    .var obj coderef
    .var obj context
    getcode $coderef &inner
    ctx $context
    forceouterctx $coderef $context

    .var obj inner
    getcode $inner &inner
    .call $inner

    exit %0
