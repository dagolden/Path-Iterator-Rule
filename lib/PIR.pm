use 5.008001;
use strict;
use warnings;

package PIR;
# ABSTRACT: Short alias for Path::Iterator::Rule
our $VERSION = '1.013';

# Dependencies
use Path::Iterator::Rule;
our @ISA = qw/Path::Iterator::Rule/;

1;

=for Pod::Coverage method_names_here

=head1 SYNOPSIS

  use PIR;

  my $rule = PIR->new;          # match anything
  $rule->file->size(">10k");    # add/chain rules

  # iterator interface
  my $next = $rule->iter( @dirs );
  while ( defined( my $file = $next->() ) ) {
    ...
  }

  # list interface
  for my $file ( $rule->all( @dirs ) ) {
    ...
  }

=head1 DESCRIPTION

This is an empty subclass of L<Path::Iterator::Rule>.  It saves you from having to type
the full name repeatedly, which is particularly handy for one-liners:

    $ perl -MPIR -wE 'say for PIR->new->skip_dirs(".")->perl_module->all(@INC)'

=cut

# vim: ts=4 sts=4 sw=4 et:
