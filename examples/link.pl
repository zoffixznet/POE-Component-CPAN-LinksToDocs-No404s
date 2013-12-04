#!/usr/bin/env perl

use strict;
use warnings;

die "Usage: perl link.pl <tags_to_lookup>\n"
    unless @ARGV;

my $Tags = shift;

use lib '../lib';

use POE qw(Component::CPAN::LinksToDocs::No404s);

my $poco = POE::Component::CPAN::LinksToDocs::No404s->spawn;

POE::Session->create(
    package_states => [
        main => [ qw(_start results) ],
    ],
);

$poe_kernel->run;

sub _start {
    $poco->link_for({ tags => $Tags, event => 'results' });
}

sub results {
    my $in_ref = $_[ARG0];
    my %out;
    @out{ split q|,|, $in_ref->{tags} } = @{ $_[ARG0]->{response} };

    keys %out;
    while ( my ( $key, $value ) = each %out ) {
        print "$key => $value\n";
    }

    $poco->shutdown;
}