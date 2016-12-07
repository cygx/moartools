.hll asm

.frame main
    .var int i 5
    .var str s
.label loop
    coerce_is $s $i
    print $s
    dec_i $i
    if_i $i @loop
    exit $i
