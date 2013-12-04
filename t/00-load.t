#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 12;

BEGIN {
    use_ok('Carp');
    use_ok('CPAN::LinksToDocs::No404s');
    use_ok('POE');
    use_ok('POE::Filter::Line');
    use_ok('POE::Filter::Reference');
    use_ok('POE::Wheel::Run');
	use_ok( 'POE::Component::CPAN::LinksToDocs::No404s' );
}

diag( "Testing POE::Component::CPAN::LinksToDocs::No404s $POE::Component::CPAN::LinksToDocs::No404s::VERSION, Perl $], $^X" );
use POE;
my $poco = POE::Component::CPAN::LinksToDocs::No404s->spawn(
    obj_args => { tags => {foos => 'bars'} },
    debug => 1,
);

isa_ok($poco, 'POE::Component::CPAN::LinksToDocs::No404s');
can_ok($poco, qw(spawn shutdown link_for session_id _start _sig_child
                    _link_for _shutdown _child_closed _child_error
                    _child_stderr _child_stdout _wheel _process_request));

my $VAR1 = [
          'http://perldoc.perl.org/functions/map.html',
          'http://perldoc.perl.org/functions/grep.html',
          'http://search.cpan.org/perldoc?perlrequick',
          'http://search.cpan.org/perldoc?perlretut',
          'http://search.cpan.org/perldoc?perlre',
          'http://search.cpan.org/perldoc?perlreref',
          'http://search.cpan.org/perldoc?perlboot',
          'http://search.cpan.org/perldoc?perltoot',
          'http://search.cpan.org/perldoc?perltooc',
          'http://search.cpan.org/perldoc?perlbot',
          'bars',
        ];

POE::Session->create(
    package_states => [
        main => [ qw(_start results) ],
    ],
);
$poe_kernel->run;
sub _start {
    $poco->link_for({tags=>'map,grep,RE,OOP,foos',event=>'results',_user=>'foos'}),
}
sub results {
    my $in = $_[ARG0];
    is_deeply(
        $in->{response},
        $VAR1,
        'checks for links'
    );
    is($in->{tags},'map,grep,RE,OOP,foos', '{tags}');
    is($in->{_user}, 'foos', 'user defined args');
    $poco->shutdown;
}
