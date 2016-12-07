.hll asm

.frame main
    .locals int str
    .alias ok 1
    const_s $ok 'ok'
    print $ok
    exit %0
