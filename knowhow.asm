.hll asm
.frame main
.local int
exit %0

.frame create_knowhow_type
.local[obj] rv how method
.local[str] name repr
checkarity 1 1
param_rp_s @name 0
const_s @repr 'KnowHOWREPR'
knowhow @how
findmeth @method @how 'new_type'
.flags obj :str :str
.result @rv
.call @method @how name: @name repr: @repr
return_o @rv

.frame load
.set load
.local str obj
const_s %0 'create_knowhow_type'
getcode %1 &create_knowhow_type
bindcurhllsym %1 %0 %1
