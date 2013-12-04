package POE::Component::CPAN::LinksToDocs::No404s;

use warnings;
use strict;

our $VERSION = '0.002';


use Carp;
use CPAN::LinksToDocs::No404s;
use POE (qw( Filter::Reference  Filter::Line  Wheel::Run ));

sub spawn {
    my $package = shift;
    croak "$package requires an even number of arguments"
        if @_ & 1;

    my %params = @_;
    
    $params{ lc $_ } = delete $params{ $_ } for keys %params;

    delete $params{options}
        unless ref $params{options} eq 'HASH';

    my $self = bless \%params, $package;

    $self->{session_id} = POE::Session->create(
        object_states => [
            $self => {
                link_for => '_link_for',
                shutdown => '_shutdown',
            },
            $self => [
                qw(
                    _child_error
                    _child_closed
                    _child_stdout
                    _child_stderr
                    _sig_child
                    _start
                )
            ]
        ],
        ( defined $params{options} ? ( options => $params{options} ) : () ),
    )->ID();

    return $self;
}


sub _start {
    my ( $kernel, $self ) = @_[ KERNEL, OBJECT ];
    $self->{session_id} = $_[SESSION]->ID();

    if ( $self->{alias} ) {
        $kernel->alias_set( $self->{alias} );
    }
    else {
        $kernel->refcount_increment( $self->{session_id} => __PACKAGE__ );
    }

    $self->{wheel} = POE::Wheel::Run->new(
        Program    => sub{ _wheel( $self->{obj_args} ); },
        ErrorEvent => '_child_error',
        CloseEvent => '_child_close',
        StdoutEvent => '_child_stdout',
        StderrEvent => '_child_stderr',
        StdioFilter => POE::Filter::Reference->new,
        StderrFilter => POE::Filter::Line->new,
        ( $^O eq 'MSWin32' ? ( CloseOnCall => 0 ) : ( CloseOnCall => 1 ) )
    );

    $kernel->yield('shutdown')
        unless $self->{wheel};

    $kernel->sig_child( $self->{wheel}->PID(), '_sig_child' );

    undef;
}

sub _sig_child {
    $poe_kernel->sig_handled;
}

sub session_id {
    return $_[0]->{session_id};
}

sub link_for {
    my $self = shift;
    $poe_kernel->post( $self->{session_id} => 'link_for' => @_ );
}

sub _link_for {
    my ( $kernel, $self ) = @_[ KERNEL, OBJECT ];
    my $sender = $_[SENDER]->ID;
    
    return
        if $self->{shutdown};
        
    my $args;
    if ( ref $_[ARG0] eq 'HASH' ) {
        $args = { %{ $_[ARG0] } };
    }
    else {
        carp "First parameter must be a hashref, trying to adjust...";
        $args = { @_[ARG0 .. $#_] };
    }
    
    $args->{ lc $_ } = delete $args->{ $_ }
        for grep { !/^_/ } keys %$args;

    unless ( $args->{event} ) {
        carp "Missing 'event' parameter to link_for()";
        return;
    }

    unless ( defined $args->{tags} ) {
        carp "Missing 'tags' parameter to link_for()";
        return;
    }

    if ( $args->{session} ) {
        if ( my $ref = $kernel->alias_resolve( $args->{session} ) ) {
            $args->{sender} = $ref->ID;
        }
        else {
            carp "Could not resolve 'session' parameter to a valid"
                    . " POE session";
            return;
        }
    }
    else {
        $args->{sender} = $sender;
    }
    
    $kernel->refcount_increment( $args->{sender} => __PACKAGE__ );
    $self->{wheel}->put( $args );
    
    undef;
}

sub shutdown {
    my $self = shift;
    $poe_kernel->call( $self->{session_id} => 'shutdown' => @_ );
}

sub _shutdown {
    my ( $kernel, $self ) = @_[ KERNEL, OBJECT ];
    $kernel->alarm_remove_all;
    $kernel->alias_remove( $_ ) for $kernel->alias_list;
    $kernel->refcount_decrement( $self->{session_id} => __PACKAGE__ )
        unless $self->{alias};

    $self->{shutdown} = 1;
    
    $self->{wheel}->shutdown_stdin
        if $self->{wheel};
}

sub _child_closed {
    my ( $kernel, $self ) = @_[ KERNEL, OBJECT ];
    
    carp "_child_closed called (@_[ARG0..$#_])\n"
        if $self->{debug};

    delete $self->{wheel};
    $kernel->yield('shutdown')
        unless $self->{shutdown};

    undef;
}

sub _child_error {
    my ( $kernel, $self ) = @_[ KERNEL, OBJECT ];
    carp "_child_error called (@_[ARG0..$#_])\n"
        if $self->{debug};

    delete $self->{wheel};
    $kernel->yield('shutdown')
        unless $self->{shutdown};

    undef;
}

sub _child_stderr {
    my ( $kernel, $self ) = @_[ KERNEL, OBJECT ];
    carp "_child_stderr: $_[ARG0]\n"
        if $self->{debug};

    undef;
}

sub _child_stdout {
    my ( $kernel, $self, $input ) = @_[ KERNEL, OBJECT, ARG0 ];
    
    my $session = delete $input->{sender};
    my $event   = delete $input->{event};

    $kernel->post( $session, $event, $input );
    $kernel->refcount_decrement( $session => __PACKAGE__ );
    
    undef;
}

sub _wheel {
    my $obj_args = shift;

    if ( $^O eq 'MSWin32' ) {
        binmode STDIN;
        binmode STDOUT;
    }
    
    my $raw;
    my $size = 4096;
    my $filter = POE::Filter::Reference->new;

    my $obj = CPAN::LinksToDocs::No404s->new( %{ $obj_args || {} } );

    while ( sysread STDIN, $raw, $size ) {
        my $requests = $filter->get( [ $raw ] );
        foreach my $req_ref ( @$requests ) {

            _process_request( $obj, $req_ref ); # changes $req_ref

            my $response = $filter->put( [ $req_ref ] );
            print STDOUT @$response;
        }
    }
}

sub _process_request {
    my ( $obj, $req_ref ) = @_;

    $req_ref->{response} = $obj->link_for( $req_ref->{tags} );

    undef;
}

1;
__END__

=encoding utf8

=head1 NAME

POE::Component::CPAN::LinksToDocs::No404s - non-blocking wrapper around CPAN::LinksToDocs::No404s

=head1 SYNOPSIS

=head1 SYNOPSIS

    use strict;
    use warnings;

    use POE qw(Component::CPAN::LinksToDocs::No404s);

    my $poco = POE::Component::CPAN::LinksToDocs::No404s->spawn;

    POE::Session->create(
        package_states => [ main => [qw(_start response )] ],
    );

    $poe_kernel->run;

    sub _start {
        $poco->link_for( {
                tags => 'map,RE,Carp,Acme::Something,OtherModule',
                event => 'response',
            }
        );
    }

    sub response {
        print "$_\n" for @{ $_[ARG0]->{results} };

        $poco->shutdown;
    }

Using event based interface is also possible of course.

=head2 DESCRIPTION

The module is a non-blocking wrapper around L<CPAN::LinksToDocs::No404s>
which provides means to get documentation to L<http://search.cpan.org>
documentation by giving predefined "tags" or module names. To find out
more about "tags" see TAGS section in L<CPAN::LinksToDocs::No404s> module's
documentation.

=head1 CONSTRUCTOR

=head2 spawn

    my $poco = POE::Component::CPAN::LinksToDocs::No404s->spawn;

    POE::Component::CPAN::LinksToDocs::No404s->spawn(
        alias => 'docs',
        obj_args => {
            tags => {
                foos => 'http://bars.com/',
            },
            timeout => 20,
        },
        options => {
            debug => 1,
            trace => 1,
            # POE::Session arguments for the component
        },
        debug => 1, # output some debug info
    );

The C<spawn> method returns a
POE::Component::CPAN::LinksToDocs::No404s object. It takes a few arguments,
I<all of which are optional>. The possible arguments are as follows:

=head3 alias

    POE::Component::CPAN::LinksToDocs::No404s->spawn(
        alias => 'docs'
    );

B<Optional>. Specifies a POE Kernel alias for the component.

=head3 obj_args

    my $poco = POE::ComponentCPAN::LinksToDocs::No404s->spawn(
        obj_args => {
            tags => {
                foos => 'http://bars.com/',
            },
            timeout => 20,
        }
    );

B<Optional>. The C<obj_args> argument takes a hashref as a value. The
hashref must contain L<CPAN::LinksToDocs::No404s> object's constructor
arguments. See L<CPAN::LinksToDocs::No404s>'s constructor (C<new()> method)
documentation for more information.

=head3 options

    my $poco = POE::Component::CPAN::LinksToDocs::No404s->spawn(
        options => {
            trace => 1,
            default => 1,
        },
    );

B<Optional>.
A hashref of POE Session options to pass to the component's session.

=head3 debug

    my $poco = POE::Component::CPAN::LinksToDocs::No404s->spawn(
        debug => 1
    );

When set to a true value turns on output of debug messages. B<Defaults to:>
C<0>.

=head1 METHODS

=head2 link_for

    $poco->link_for( {
            event => 'event_for_output',
            tags  => 'map,grep,Acme::Something,RE,OOP,SomeModule',
            _blah => 'pooh!',
            session => 'other',
        }
    );

Takes a hashref as an argument, does not return a sensible return value.
See C<link_for> event's description for more information.

=head2 session_id

    my $poco_id = $poco->session_id;

Takes no arguments. Returns component's session ID.

=head2 shutdown

    $poco->shutdown;

Takes no arguments. Shuts down the component.

=head1 ACCEPTED EVENTS

=head2 link_for

    $poe_kernel->post( docs => link_for => {
            event => 'event_for_output',
            tags  => 'map,grep,Acme::Something,OOP,SomeModule',
            _blah => 'pooh!',
            session => 'other',
        }
    );

Instructs the component to make out the links (and check the ones that
need checking) for "tags" given in the
C<tags> argument. Takes a hashref as an
argument, the possible keys/value of that hashref are as follows:

=head3 event

    { event => 'results_event', }

B<Mandatory>. Specifies the name of the event to emit when results are
ready. See OUTPUT section for more information.

=head3 tags

    { tags => 'map,grep,OOP,Acme::Something,SomeModule' }

B<Mandatory>. Takes a scalar containing comma separated one or more "tags".
See C<TAGS> section in L<CPAN::LinksToDocs::No404s> module's documentation
for more information about possible "tags".

=head3 session

    { session => 'other' }

    { session => $other_session_reference }

    { session => $other_session_ID }

B<Optional>. Takes either an alias, reference or an ID of an alternative
session to send output to.

=head3 user defined

    {
        _user    => 'random',
        _another => 'more',
    }

B<Optional>. Any keys starting with C<_> (underscore) will not affect the
component and will be passed back in the result intact.

=head2 shutdown

    $poe_kernel->post( docs => 'shutdown' );

Takes no arguments. Tells the component to shut itself down.

=head1 OUTPUT

    $VAR1 = {
        'response' => [
            'http://perldoc.perl.org/functions/map.html',
            'http://perldoc.perl.org/functions/grep.html',
            'http://search.cpan.org/perldoc?Acme::BabyEater',
            'Not found'
        ],
        'tags' => 'map,grep,Acme::BabyEater,Zoffer',
        '_user' => 'foos',
    };

The event handler set up to handle the event which you've specified in
the C<event> argument to C<link_for()> method/event will recieve input
in the C<$_[ARG0]> in a form of a hashref. The possible keys/value of
that hashref are as follows:

=head2 response

    {
        'response' => [
            'http://perldoc.perl.org/functions/map.html',
            'http://perldoc.perl.org/functions/grep.html',
            'http://search.cpan.org/perldoc?Acme::BabyEater',
            'Not found'
        ],
    }

The C<response> key will contain a (possibly empty) arrayref of links
to documentation. This is the same arrayref that you would get from
L<CPAN::LinksToDocs::No404s>'s C<link_for()> method. See documentation
for L<CPAN::LinksToDocs::No404s> module for more information.

=head2 tags

    { 'tags' => 'map,grep,Acme::BabyEater,Zoffer', }

The C<tags> key will contain whatever you've passed in C<tags> argument to
C<link_for()> method/event.

=head2 user defined

    { '_blah' => 'foos' }

Any arguments beginning with C<_> (underscore) passed into the C<EXAMPLE()>
event/method will be present intact in the result.

=head1 SEE ALSO

L<POE>, L<CPAN::LinksToDocs::No404s>

=head1 AUTHOR

Zoffix Znet, C<< <zoffix at cpan.org> >>
(L<http://zoffix.com>, L<http://haslayout.net>)

=head1 BUGS

Please report any bugs or feature requests to C<bug-poe-component-cpan-linkstodocs-no404s at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=POE-Component-CPAN-LinksToDocs-No404s>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc POE::Component::CPAN::LinksToDocs::No404s

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=POE-Component-CPAN-LinksToDocs-No404s>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/POE-Component-CPAN-LinksToDocs-No404s>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/POE-Component-CPAN-LinksToDocs-No404s>

=item * Search CPAN

L<http://search.cpan.org/dist/POE-Component-CPAN-LinksToDocs-No404s>

=back

=head1 COPYRIGHT & LICENSE

Copyright 2008 Zoffix Znet, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
