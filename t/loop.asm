.hll asm

.frame main
    .local int i 5
    .local str s
.label loop
    coerce_is $s $i
    print $s
    dec_i $i
    if_i $i @loop
    exit $i
