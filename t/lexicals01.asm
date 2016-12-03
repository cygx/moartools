.hll asm

.frame main
    .local int
    .lexical int i
    .const[str] ok 'ok'
    .alias rv %0
    getlex @rv $i
    print @ok
    exit @rv
