.hll asm

.frame main
    .locals int str
    .aliases _ ok
    .alias zero 0
    const_s $ok "ok"
    print $ok
    exit $zero
