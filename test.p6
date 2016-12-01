use nqp;

nqp::loadbytecode('knowhow.moarvm');
my &create := nqp::hllize(nqp::gethllsym('asm', 'create_knowhow_type'));
my $type := create('Dummy');
say $type.^name;
say $type.REPR;
say $type.HOW.^name;
