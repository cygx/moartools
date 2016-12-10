use v6;
use nqp;

sub spawn($cmd, @args, $cwd, $env, $no-err) {
    my $out := nqp::syncpipe();
    my $rv := nqp::spawn(
        CLONE-LIST-DECONTAINERIZED($cmd, |@args),
        $cwd // ~$*CWD,
        $env ?? CLONE-HASH-DECONTAINERIZED($env) !! nqp::hash(),
        nqp::getstdin(), $out, nqp::getstderr(), 0
        +| nqp::const::PIPE_INHERIT_IN
        +| nqp::const::PIPE_CAPTURE_OUT
        +| ($no-err ?? nqp::const::PIPE_IGNORE_ERR
                    !! nqp::const::PIPE_INHERIT_ERR));

    fail IntStr.new($rv, "process creation failed with code $rv") if $rv;
    nqp::readallfh($out);
}

sub run6(*@args, :$cwd, :$env, :$no-err) is export {
    my $exe = %*ENV<PERL6> || ~$*EXECUTABLE;
    spawn($exe, @args, $cwd, $env, $no-err);
}

sub run-nqp(*@args, :$cwd, :$env, :$no-err) is export {
    my $exe = %*ENV<NQP> || $*EXECUTABLE.flip.subst('6lrep', 'pqn').flip;
    spawn($exe, @args, $cwd, $env, $no-err);
}

sub run-moar(*@args, :$cwd, :$env, :$no-err) is export {
    my $exe = %*ENV<MOAR> || ~$*EXECUTABLE.parent.child('moar');
    spawn($exe, @args, $cwd, $env, $no-err);
}
