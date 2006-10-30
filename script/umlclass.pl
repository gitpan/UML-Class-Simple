#!/usr/bin/env perl

use strict;
use warnings;

use File::Slurp;
use YAML::Syck;
use Getopt::Std;
use File::Spec;
use UML::Class::Simple;

my $ext_regex = qr/(?:\.pl|\.pm)$/i;

my %opts;
getopts('hM:o:p:Prs:', \%opts) or help(1);

help(0) if $opts{h};

my ($width, $height);
if ($opts{s}) {
    if ($opts{s} !~ /(?x) ([\d\.]+) x ([\d\.]+) /) {
        die "error: -s option only takes argument like 3.2x5 and 7x3\n";
    }
    ($width, $height) = ($1, $2);
}

my $recursive   = $opts{r};
my $public_only = $opts{P};
#my $root_class  = $opts{R};

my @infiles = sort map { -d $_ ? all_in($_) : $_ } map glob, @ARGV;

my @plfiles = grep { !/(?:\.dot|\.yml)$/i } @infiles;

my $painter;

if (!@plfiles) {
    if (@infiles) {
        my $file = pop @infiles;
        if ($file =~ /\.dot$/i) {
            $painter = UML::Class::Simple->new;
            $painter->set_dot(read_file($file));
        }
        elsif ($file =~ /\.yml$/i) {
            $painter = UML::Class::Simple->new;
            my $dom = LoadFile($file);
            $painter->set_dom($dom);
        }
    }
} else {
    if (@plfiles != @infiles) {
        die "error: perl source files are not allowed when ",
            ".yml or .dot files are also given.\n";
    }
}

if (!$painter) {
    my @classes;
    @classes = classes_from_runtime($opts{M}, $opts{p}) if !@plfiles;
    push @classes, classes_from_files(\@plfiles, $opts{p}) if @plfiles;
    if (@classes) {
        print join("\n", sort @classes), "\n\n";
        $painter = UML::Class::Simple->new(\@classes);
    } else {
        die "error: no class found.\n";
    }
}

$painter->public_only($public_only) if $public_only;
$painter->size($width, $height) if $width and $height;
#$painter->root_at($root_class) if $root_class;

my $outfile = $opts{o} || 'a.png';
my $ext = 'png';
if ($outfile =~ /\.(\w+)$/) { $ext = lc($1); }

if ($ext eq 'png') {
    $painter->as_png($outfile);
}
elsif ($ext eq 'gif') {
    $painter->as_gif($outfile);
}
elsif ($ext eq 'dot') {
    $painter->as_dot($outfile);
}
elsif ($ext eq 'yml') {
    my $dom = $painter->as_dom;
    DumpFile($outfile, $dom);
}
else {
    die "error: unknown output file format: $ext\n";
}

print "$outfile generated.\n";

sub help {
    my $code = shift;
    warn <<'_EOC_';
Usage: $0 [-M module] [-o outfile] [-p regex] [infile... indir...]
    infile...    Perl source files, .pm, .pl, .yml, or .dot file, or
                 .yml file containing the class info DOM. They're
                 optional.
    indir...     Directory containing perl source files. They're
                 optional too.
Options: 
    -h           Print this help.
    -M module    Preload the specified module to runtime.
    -o outfile   Specify the output file name. it can be one of the
                 following types: .png, .dot, and .yml. Defaults
                 to a.png.
    -P           Show public methods only.
    -p regex     Specify the perl regex as the pattern used to
                 filter out classes to be drawn.
    -r           Process subdirectories of indir recursively.
    -R class     Specify the root class from which only its
                 descendents are shown.
    -s <w>x<h>   Specify the width and height (in inches) for the
                 output images. For instance, 3.2x6.3 and 4x8.

Report bugs or wishlist to Agent Zhang <agentzh@gmail.com>.
_EOC_
    exit($code);
}

# Stolen directly from 'prove'
sub all_in {
    my $start = shift;
    my @hits = ();

    local *DH;
    if ( opendir( DH, $start ) ) {
        my @files = sort readdir DH;
        closedir DH;
        for my $file ( @files ) {
            next if $file eq File::Spec->updir || $file eq File::Spec->curdir;
            next if $file eq ".svn";
            next if $file eq "CVS";

            my $currfile = File::Spec->catfile( $start, $file );
            if ( -d $currfile ) {
                push( @hits, all_in( $currfile ) ) if $recursive;
            } else {
                push( @hits, $currfile ) if $currfile =~ $ext_regex;
            }
        }
    } else {
        warn "$start: $!\n";
    }

    return @hits;
}

__END__

=head1 NAME

umlclass.pl - utility to generate UML class diagrams from source or runtime

=head1 SYNOPSIS

=head1 DESCRIPTION

This is a simple commandline frontend for the L<UML::Class::Simple> module.

I'll illustrate the usage of this tool via some real-world examples.

=head2 Draw Stevan's Moose

  $ umlclass.pl -M Moose -o samples/moose_small.png -p "^(Class::MOP|Moose::)" -s 4x8

This command will generate a simple class diagram in PNG format for the Moose module
with classes having names matching the regex C<"^(Class::MOP|Moose::)">. The image's
width is 4 inches while its height is 8 inches.

We need the -M option here since C<umlclass.pl> needs to preload L<Moose> into the
memory so as to inspect it at runtime.

The graphical output is given below:

=begin html

&nbsp; &nbsp; <img src="samples/moose_small.png" alt="samples/moose_small.png"/>

=end html

(See also L<http://perlcabal.org/agent/images/ppi_small.png>.)

Yes, the image above looks very fuzzy since the whole stuff is huge. If you strip
the -s option, then the resulting image will enlarge automatically:

  $ umlclass.pl -M Moose -o samples/moose_big.png -p "^(Class::MOP|Moose::)"

Since the image obtained is really really large, I won't show it here.

Before trying out these commands, please make sure that you have L<Moose>
already installed. (It's also on CPAN, btw.)

=head2 Draw Alias's PPI

  $ umlclass.pl -M PPI -o samples/ppi_small.png -p "^PPI::" -s 10x10

=begin html

&nbsp; &nbsp; <img src="samples/ppi_small.png" alt="samples/ppi_small.png"/>

=end html

(See also L<http://perlcabal.org/agent/images/ppi_small.png>.)

Or the full-size version:

  $ umlclass -M PPI -o samples/ppi_big.png -p "^PPI::"

(See L<http://perlcabal.org/agent/images/ppi_big.png>.)

=head2 Draw FAST.pm from UML::Class::Simple's Test Suite

  $ umlclass.pl -M FAST -o samples/fast.png -s 5x10 -r t/FAST/lib

This is an example of drawing classes contained in Perl source files.

L<PPI> is a prerequisite of this module. So if you have this module installed
then you should also have L<PPI>. :)

=head2 Draw Modules of Your Own

Suppose that you're a CPAN author too and want to produce a class diagram for I<all>
the classes contained in your lib/ directory. The following command can do all the
job for you:

    $ umlclass.pl -o mylib.png -r lib

or just plot the packages in the specified .pm files:

    $ umlclass.pl -o a.png lib/foo.pm lib/bar/baz.pm

or even specify a pattern (in perl regex) to filter out the packages you want to plot:

    $ umlclass.pl -o a.png -p "^Foo::" lib/foo.pm

Quite handy, isn't it?

=head1 OPTIONS

=over

=item -h

Show the help message.

=item -M module

Preload modules which may contain the classes you want to depict. For example,

    $ umlclass.pl -M PPI -o ppi.png -p "^PPI::"

=item -o outfile

Specify the output file name. Note that the file extension will be honoured.
If you specify "C<-o foo.png>", a PNG image named F<foo.png> will be generated,
and if you specify "C<-o foo.dot>", the dot source file named F<foo.dot> will
be obtained. Likewise, "C<-o foo.yml>" will lead to a YAML file holding the whole
internal DOM data.

A typical usage is as follows:

    $ umlclass.pl -o foo.yml lib/Foo.pm

    # ...edit the foo.yml so as to adjust the class info
    # feed the updated foo.dot back
    $ umlclass.pl -o foo.dot foo.yml

    # ...edit the foo.dot so as to adjust the graphviz dot source
    # now feed the updated foo.dot back
    $ umlclass.pl -o foo.png foo.dot

You see, F<umlclass.pl> allows you to control the behaviors at serval different
levels. I really like this freedom, since tools can't always do exactly what I want.

=item -p regex

Specify the pattern (perl regex) used to filter out the class names to be drawn.

=item -P

Show public methods only.

=item -r

Recursively process subdirectories of input directories.

=item -s <w>x<h>

Specify the width and height of the resulting image. For example:

    -s 3.6x7

where the unit is inches instead of pixels.

=back

=head1 TODO

=over

=item *

Add the C<-R class> option which specifies the root class from which 
only its descendents will be shown.

=back

=head1 AUTHOR

Agent Zhang E<lt>agentzh@gmail.comE<gt>

=head1 COPYRIGHT

Copyright 2006 by Agent Zhang. All rights reserved.

This library is free software; you can redistribute it and/or modify it under
the same terms as perl itself.

=head1 SEE ALSO

L<UML::Class::Simple>.
