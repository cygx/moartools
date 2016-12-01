my $assembler := nqp::getcomp('MAST');
my %types := nqp::hash(
    'int', int,
    'str', str
);

class asm {
    method ast() {
        MAST::CompUnit.new();
    }

    method hll($ast, $name) {
        $ast.hll($name);
    }

    method iv($value, :$size = 64) {
        MAST::IVal.new(:$value, :$size, signed => 1);
    }

    method uv($value, :$size = 64) {
        MAST::IVal.new(:$value, :$size, signed => 0);
    }

    method nv($value, :$size = 64) {
        MAST::NVal.new(:$value, :$size);
    }

    method sv($value) {
        MAST::SVal.new(:$value);
    }

    method local($index) {
        MAST::Local.new(:$index);
    }

    method add_local($frame, $typename) {
        nqp::die("unknown type: $typename")
            unless nqp::existskey(%types, $typename);

        my $type := %types{$typename};
        $frame.add_local($type);
    }

    method add_frame($ast, $name = '<anon>', $cuuid?) {
        my $frame := MAST::Frame.new(:$name);
        $ast.add_frame($frame);
        $frame;
    }

    method push_op($frame, $op, *@args) {
        nqp::die("unknown op: $op")
            unless nqp::existskey(%MAST::Ops::codes, $op);

        nqp::push($frame.instructions(), MAST::Op.new(:$op, |@args));
    }

    method compile($ast) {
        $assembler.assemble_and_load($ast);
    }

    method dump($ast, $file) {
        $assembler.assemble_to_file($ast, $file);
    }

    method mainline($cu) {
        nqp::compunitmainline($cu);
    }
}
