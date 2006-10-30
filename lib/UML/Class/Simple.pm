package UML::Class::Simple;

use 5.006001;
use strict;
use warnings;

use Class::Inspector;
use IPC::Run3;
use Template;
use Carp qw(carp);
use List::MoreUtils 'any';

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(classes_from_runtime classes_from_files);

our $VERSION = '0.01';

my $tt = Template->new;
my $dot_template;

sub classes_from_runtime ($$) {
    my ($module, $pattern) = @_;
    $pattern = '' if !defined $pattern;
    if ($module) {
        eval "use $module;";
        if ($@) { carp $@; return (); }
    }
    grep { /$pattern/ } map { s/^::|::$//g; $_ } _runtime_packages();
}

sub _runtime_packages {
    no strict 'refs';
    my $pkg_name = shift || '::';
    my $cache = shift || {};
    return if $cache->{$pkg_name};
    $cache->{$pkg_name} = 1;
    for my $entry (keys %$pkg_name) {
        next if $entry !~ /\:\:$/ or $entry eq 'main::';
        my $subpkg_name = $pkg_name.$entry;
        #warn $subpkg_name;
        _runtime_packages($subpkg_name, $cache);
        $cache->{$subpkg_name} = 1;
    }
    wantarray ? keys %$cache : $cache;
}

sub classes_from_files ($@) {
    require PPI;
    my ($list, $pattern) = @_;
    my @classes;
    for my $file (@$list) {
        my $doc = PPI::Document->new( $file );
        if (!$doc) {
            carp "warning: Can't parse $file: ", PPI->errstr;
            next;
        }
        my $res = $doc->find('PPI::Statement::Package');
        next if !$res;
        push @classes, map { $_->namespace } @$res;
    }
    wantarray ? @classes : \@classes;
}

sub new ($$) {
    my $class = ref $_[0] ? ref shift : shift;
    my $rclasses = shift;
    bless {
        class_names => $rclasses,
        node_color  => '#f1e1f4',
    }, $class;
}

sub size ($$$) {
    my $self = shift;
    if (@_) {
        my ($width, $height) = @_;
        if (!$width || !$height || ($width . $height) !~ /^[\.\d]+$/) {
            carp "invalid width and height";
            return undef;
        } else {
            $self->{width}  = $width;
            $self->{height} = $height;
            return 1;
        }
    } else {
        return ($self->{width}, $self->{height});
    }
}

sub node_color {
    my $self = shift;
    if (@_) {
        $self->{node_color} = shift;
    } else {
        $self->{node_color};
    }
}

sub public_only ($$) {
    my $self = shift;
    if (@_) {
        $self->{public_only} = shift;
        $self->_build_dom(1);
    } else {
        $self->{public_only};
    }
}

sub as_png ($@) {
    my $self = shift;
    $self->_as_image('png', @_);
}

sub as_gif ($@) {
    my $self = shift;
    $self->_as_image('gif', @_);
}

sub _as_image {
    my ($self, $type, $fname) = @_;
    my $dot = $self->as_dot;
    my @cmd = ('dot', '-T', $type);
    if ($fname) {
        push @cmd, '-o', $fname;
    }
    my ($img_data, $stderr);
    my $success = run3 \@cmd, \$dot, \$img_data, \$stderr;
    if ($stderr) {
        carp $stderr;
    }
    if (!$fname) {
        return $img_data;
    }
}

sub as_dom ($) {
    my $self = shift;
    $self->_build_dom;
    { classes => $self->{classes} };
}

sub set_dom ($$) {
    my $self = shift;
    $self->{classes} = shift->{classes};
    1;
}

sub _build_dom {
    my ($self, $force) = @_;
    # avoid unnecessary evaluation:
    return if $self->{classes} && !$force || !$self->{class_names};
    my @pkg = @{ $self->{class_names} };
    my @classes;
    $self->{classes} = \@classes;
    my $public_only = $self->{public_only};
    my %visited; # used to eliminate potential repetitions
    for my $pkg (@pkg) {
        $pkg =~ s/::::/::/g;
        if ($visited{$pkg}) { next; }
        $visited{$pkg} = 1;

        if (!Class::Inspector->loaded($pkg)) {
            my $pmfile = Class::Inspector->filename($pkg);
            my $done = 1;
            if ($pmfile) {
                eval { require $pmfile };
                if ($@) {
                    #warn $@;
                    $done = 0 ;
                }
            } else { $done = 0 }
            if (!$done) {
                #carp "warning: $pkg not loaded";
                next;
            }
        }
        push @classes, {
            name => $pkg, methods => [],
            properties => [], subclasses => [],
        };
        my $func = Class::Inspector->functions($pkg);
        if ($func and @$func) {
            if ($public_only) {
                @$func = grep { /^[^_]/ } @$func;
            }
            $classes[-1]->{methods} = $func;
        }
        my $subclasses = Class::Inspector->subclasses($pkg);
        if ($subclasses) {
            no strict 'refs';
            my @child = grep {
                #warn "!!!! ", join ' ', @{"${_}::ISA"};
                any { $_ eq $pkg } @{"${_}::ISA"};
            } @$subclasses;
            if (@child) {
                $classes[-1]->{subclasses} = \@child;
            }
        }
    }
}

sub as_dot ($@) {
    my ($self, $fname) = @_;
    $self->_build_dom;
    if ($fname) {
        $tt->process(\$dot_template, $self, $fname)
            || carp $tt->error();
    } else {
        my $dot;
        $tt->process(\$dot_template, $self, \$dot)
            || carp $tt->error();
        $dot;
    }
}

sub set_dot ($$) {
    my $self = shift;
    $self->{dot} = shift;
}

$dot_template = <<'_EOC_';
digraph uml_class_diagram {
  [%- IF width && height %]
    size="[% width %],[% height %]";
  [%- END %]
    node [shape=record, fillcolor="[% node_color %]", style="filled"];
    edge [color=red, dir=none];

[%- name2id = {} %]
[%- id = 1 %]
[%- FOREACH class = classes %]
    [%- name = class.name %]
    [%- name2id.$name = id %]
    class_[% id %] [shape=plaintext, style="", label=<
<table BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
  <tr><td port="title" bgcolor="#f1e1f4">[% name %]</td></tr>
  <tr>
    <td>
    <table border="0" cellborder="0" cellspacing="0" cellpadding="1">
      <tr>
    <td><font color="red">
    [%- FOREACH property = class.properties %]
      [%- property.match("^_") ? "-" : "+" %]<br align="left"/>

    [%- END %]</font></td>
    <td port="properties" bgcolor="white" align="left">
    [%- FOREACH property = class.properties %]
      [%- property %]<br align="left"/>

    [%- END %]</td>
      </tr>
    </table>
    </td>
  </tr>
  <tr>
    <td port="methods" >
    <table border="0" cellborder="0" cellspacing="0" cellpadding="0">
      <tr>
    <td><font color="red">
    [%- FOREACH method = class.methods %]
      [%- method.match("^_") ? "-" : "+" %]<br align="left"/>

    [%- END %]</font></td>
    <td bgcolor="white" align="left">
    [%- FOREACH method = class.methods %]
      [%- method %]<br align="left"/>

    [%- END %]</td>
      </tr>
    </table>
    </td>
  </tr>
</table>>];
  [%- id = id + 1 %]
[% END %]
[%- class_id = id %]

[%- first = 1 %]
[%- id = 0 %]
[%- FOREACH class = classes %]
  [%- id = id + 1 %]
  [%- super = class.name %]
  [%- NEXT IF !class.subclasses.size -%]

  [%- IF first -%]
     node [shape="triangle", fillcolor=yellow, height=0.3, width=0.3];
     [%- first = 0 %]
  [%- END -%]

     angle_[% id %] [label=""];


  [%- super_id = name2id.$super %]
     class_[% super_id %]:methods -> angle_[% id %]
  [%- FOREACH child = class.subclasses %]
    [%- child_id = name2id.$child %]
    [%- IF !child_id %]
     class_[% class_id %] [shape=record, label="[% child %]" fillcolor="#f1e1f4", style="filled"];
     angle_[% id %] -> class_[% class_id %]
        [%- class_id = class_id + 1 %]
      [%- ELSE %]
     angle_[% id %] -> class_[% child_id %]:title
    [%- END %]
  [%- END %]
[%- END %]

}
_EOC_

1;
__END__

=head1 NAME

UML::Class::Simple - Render simple UML class diagrams, often by loading the code.

=head1 VERSION

This document describes C<UML::Class::Simple> 0.01 released by Oct 30, 2006.

=head1 SYNOPSIS

    use UML::Class::Simple;

    # produce a class diagram for fglock's Pugs::Compiler::Perl6
    # which has already installed to your perl:

    @classes = classes_from_runtime("PPI", qr/^PPI::/);
    $painter = UML::Class::Simple->new(\@classes);
    $painter->as_png('ppi.png');

    # produce a class diagram for your CPAN module on the disk

    @classes = classes_from_files(['lib/Foo.pm', 'lib/Foo/Bar.pm']);
    $painter = UML::Class::Simple->new(\@classes);
    
    # we can explicitly specify the image size
    $painter->size(5, 3.6); # in inches

    # ...and change the default title background color:
    $painter->node_color('#ffffff'); # defaults to '#f1e1f4'
    
    # only show public methods and properties
    $painter->public_only(1);

    $painter->as_png('my_module.png');

=head1 DESCRIPTION

C<UML::Class::Simple> is a Perl CPAN module that generates UML class
diagrams (PNG format, GIF format, or dot source) automatically
from Perl 5 source or Perl 5 runtime.

Perl developers can use this module to obtain pretty class diagrams
for arbitrary existing Perl class libraries (including modern perl OO
modules based on Moose.pm), by only a single command. Companies can
also use the resulting pictures to visualize the project hierarchy and
embed them into their documentation.

The users no longer need to drag a mouse on the screen so as to draw
figures themselves or provide any specs other than the source code of
their own libraries that they want to depict. This module does all the
jobs for them! :)

You know, I was really impressed by the outputs of L<UML::Sequence>, so I 
decided to find something to (automatically) get pretty class diagrams
too. The images from L<Autodia>'s Graphviz backend didn't quite fit my needs
when I was making some slides for my representations, so I started another 
competing project after getting some guide from C<#perl> on C<irc.perl.org>.
;-)

I think most of the time you just want to use the command-line utility L<umlclass.pl>
offered by this module (just like me). See the documentation of L<umlclass.pl> for
details.

=head1 SAMPLE OUTPUTS

=over

=item PPI

=begin html

&nbsp; &nbsp; <img src="samples/ppi_small.png" />

=end html

(See samples/ppi_small.png in the distribution.)

=item Moose

=begin html

&nbsp; &nbsp; <img src="samples/moose_small.png" />

=end html

(See samples/moose_small.png in the distribution.)

=item FAST

=begin html

&nbsp; &nbsp; <img src="samples/fast.png" />

=end html

(See samples/fast.png in the distribution.)

=back

=head1 SUBROUTINES

=over

=item classes_from_runtime($module_to_load, $regex)

Returns a list of class (or package) names by inspecting the perl runtime environment.
$module_to_load is the I<main> module name to load while $regex is
a perl regex used to filter out interesting package names.

The $regex argument can be omitted.

=item classes_from_files(\@pmfiles, $regex)

Returns a list of class (or package) names by scanning through the perl source files
given in the first argument. $regex is used to filter out interesting package names.

The $regex argument can be omitted.

=back

These subroutines are not imported by default.

=head1 METHODS

=over

=item C<< $obj->new( [@class_names] ) >>

Creates a new UML::Class::Simple instance with the specified class name list.
This list can either be constructed manually or by the utility functions
C<classes_from_runtime> and C<classes_from_files>.

=item C<< $obj->as_png($filename?) >>

Generates PNG image file when C<$filename> is given. It returns
binary data when C<$filename> is not given.

=item C<< $obj->as_gif($filename?) >>

Similar to C<<as_png>>, bug generate a GIF-format image.

=item C<< $obj->as_dom() >>

Returns the internal DOM tree used to generate dot and png. The tree's structure
looks like this:

  {
    'classes' => [
                   {
                     'subclasses' => [],
                     'methods' => [],
                     'name' => 'PPI::Structure::List',
                     'properties' => []
                   },
                   {
                     'subclasses' => [
                                       'PPI::Structure::Block',
                                       'PPI::Structure::Condition',
                                       'PPI::Structure::Constructor',
                                       'PPI::Structure::ForLoop',
                                       'PPI::Structure::List',
                                       'PPI::Structure::Subscript',
                                       'PPI::Structure::Unknown'
                                     ],
                     'methods' => [
                                    '_INSTANCE',
                                    '_set_finish',
                                    'braces',
                                    'content',
                                    'elements',
                                    'finish',
                                    'first_element',
                                    'insert_after',
                                    'insert_before',
                                    'last_element',
                                    'new',
                                    'refaddr',
                                    'start',
                                    'tokens'
                                  ],
                     'name' => 'PPI::Structure',
                     'properties' => []
                   },
                   ...
                ]
  }

You can adjust the data structure and feed it back to C<$obj> via
the C<set_dom> method.

=item C<< $obj->set_dom($dom) >>

Set the internal DOM structure to C<$obj>. This will be used to
generate the dot source and thus the PNG image.

=item C<< $obj->as_dot() >>

Returns the Graphviz dot source code generated by C<$obj>.

=item C<< $obj->set_dot($dot) >>

Sets the dot source code used by C<$obj>.

=back

=head1 PROPERTIES

=over

=item C<< $obj->size($width, $height) >>

=item C<< ($width, $height) = $obj->size >>

Sets/gets the size of the output images, in inches.

=item C<< $obj->public_only($bool) >>

=item C<< $bool = $obj->public_only >>

when the C<public_only> property is set to true, only public methods or properties
are shown. It defaults to false.

=item C<< $obj->node_color($color) >>

=item C<< $color = $obj->node_color >>

Sets/gets the background color for the class nodes. It defaults to C<'#f1e1f4'>.

=back

=head1 INSTALLATION

Please download and intall the latest Graphviz binary from its home:

L<http://www.graphviz.org/>

UML::Class::Simple requires the HTML label feature which is only
available on versions of Graphviz that are newer than mid-November 2003.
In particular, it is not part of release 1.10.

Add Graphviz's F<bin/> path to your PATH environment. This module needs its
F<dot> utility.

Grab this module from the CPAN mirror near you and run the following commands:

    perl Makefile.PL
    make
    make test
    make install

For windows users, use C<nmake> instead of C<make>.

Note that it's recommended to use the C<cpan> utility to install CPAN modules.

=head1 LIMITATIONS

=over

=item *

It's pretty hard to distinguish perl methods from properties (actually they're both
subs in perl). If you have any good ideas, please drop me a line.

=item *

Only the inheritance relationships are shown in the images. I believe other subtle 
relations can only mess up the Graphviz layouter. Hence the "::Simple" suffix in
this module name.

=item *

Unlike L<Autodia>, at this moment only Graphviz backend is provided.

=item *

There's no way to recognize C<real> perl classes automatically. After all, Perl 5's 
classes are implemented by packages. I think Perl 6 will make my life much easier.

=item *

To prevent potential naming confusion. I'm using Perl's C<::> namespace separator
in the class diagrams instead of dot (C<.>) in the standard UML syntax. One can argue
that following UML standards are more important since people in the same team may
use different programming languages. But sorry, it's not the case for me. ;-)

=back

=head1 TODO

=over

=item *

Add more unit tests.

=item *

Add support for more image formats, such as as_ps, as_jpg, and etc.

=item *

Provide a DOM interface for the class hierarchy so that the user can control what's
in there and what not.

=item *

Provide backends other than Graphviz.

=item *

Plot class relationships other than inheritance on the user's request.

=back

Please send me your wish list via emails or preferably the CPAN RT site. I'll add
them here if I'm also interested in your crazy ideas. ;-)

=head1 BUGS

There must be some serious bugs lurking somewhere. so if you found one, please report
it to L<http://rt.cpan.org>.

=head1 ACKNOWLEDGEMENT

I must thank Adam Kennedy (Alias) for writing the excellent PPI module and
L<Class::Inspector>. pm2png uses the former to extract package names from user's .pm files
while rt2png uses the latter to retrieve the function list of a specific package.

=head1 AUTHOR

Agent Zhang E<lt>agentzh@gmail.comE<gt>

=head1 COPYRIGHT

Copyright 2006 by Agent Zhang. All rights reserved.

This library is free software; you can redistribute it and/or modify it under
the same terms as perl itself.

=head1 SEE ALSO

L<umlclass.pl>, L<Autodia>, L<UML::Sequence>, L<PPI>, L<Class::Inspector>.
