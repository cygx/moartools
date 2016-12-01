.hll asm
.frame main
.local int
exit %0

.frame create_knowhow_type
.local obj obj obj str str
checkarity 1 1
param_rp_s %3 0
const_s %4 'KnowHOWREPR'
knowhow %1
findmeth %2 %1 'new_type'
.flags obj :str :str
.result %0
.call %2 %1 'name' %3 'repr' %4
return_o %0

.frame load
.set load
.local str obj
const_s %0 'create_knowhow_type'
getcode %1 &create_knowhow_type
bindcurhllsym %1 %0 %1
