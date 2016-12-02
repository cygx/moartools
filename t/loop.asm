.hll asm

.frame main
    .const[int] i 5
    .local[str] s
.loop:
    coerce_is @s @i
    print @s
    dec_i @i
    if_i @i @.loop
    exit @i
