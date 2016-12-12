use v6;
use nqp;

sub parse_lines($_ --> Mu) is export {
    my str $str = $_;
    my int $pos = 0;
    my int $len = nqp::chars($str);
    my int $tab;
    my int $nl;
    my $words := nqp::hash();
    nqp::while(
        nqp::islt_i($pos, $len),
        nqp::stmts(
            ($tab = nqp::index($str, "\t", $pos)),
            ($nl = nqp::index($str, "\n", $tab)),
            nqp::bindkey($words,
                nqp::substr($str, $pos, $tab - $pos),
                nqp::substr($str, $tab + 1, $nl - $tab - 1)),
            ($pos = nqp::add_i($nl, 1))
        )
    );
    $words;
}
