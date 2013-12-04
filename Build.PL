use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'POE::Component::CPAN::LinksToDocs::No404s',
    license             => 'perl',
    dist_author         => 'Zoffix Znet <zoffix@cpan.org>',
    dist_version_from   => 'lib/POE/Component/CPAN/LinksToDocs/No404s.pm',
    build_requires => {
        'Test::More' => 0,
    },
    requires => {
        'Carp'                      => 0,
        'CPAN::LinksToDocs::No404s' => 0.002,
        'POE'                       => 0,
        'POE::Filter::Line'         => 0,
        'POE::Filter::Reference'    => 0,
        'POE::Wheel::Run'           => 0,
        perl                        => '5.006',
    },
    configure_requires => { 'Module::Build' => 0 },
    add_to_cleanup      => [ 'POE-Component-CPAN-LinksToDocs-No404s-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
