.hll asm

.frame main
    .locals int
    exit %0

.frame create_knowhow_type
    checkarity 1 1
    .param str name 0
    .local str repr 'KnowHOWREPR'
    .local obj rv
    .local obj how
    .local obj method
    knowhow $how
    findmeth $method $how :new_type
    .flags obj :str :str
    .result $rv
    .call $method $how :name $name :repr $repr
    return_o $rv

.frame load
    .set load
    .local str fn :create_knowhow_type
    .locals obj
    getcode %1 &create_knowhow_type
    bindcurhllsym %1 $fn %1
