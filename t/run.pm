use v6;
use nqp;

sub spawn($cmd, @args, $cwd, $env) {
    my $out := nqp::syncpipe();
    my $rv := nqp::spawn(
        CLONE-LIST-DECONTAINERIZED($cmd, |@args),
        $cwd // ~$*CWD,
        $env ?? CLONE-HASH-DECONTAINERIZED($env) !! nqp::hash(),
        nqp::getstdin(), $out, nqp::getstderr(), 0
        +| nqp::const::PIPE_INHERIT_IN
        +| nqp::const::PIPE_CAPTURE_OUT
        +| nqp::const::PIPE_INHERIT_ERR);

    fail IntStr.new($rv, "process creation failed with code $rv") if $rv;
    nqp::readallfh($out);
}

sub run6(*@args, :$cwd, :$env) is export {
    my $perl6 = %*ENV<PERL6> || ~$*EXECUTABLE;
    spawn($perl6, @args, $cwd, $env);
}

sub run-nqp(*@args, :$cwd, :$env) is export {
    my $nqp = %*ENV<NQP> || $*EXECUTABLE.flip.subst('6lrep', 'pqn').flip;
    spawn($nqp, @args, $cwd, $env);
}
