use 5.008001;
use strict;
use warnings;

package Path::Iterator::Rule;
# ABSTRACT: Iterative, recursive file finder
our $VERSION = '1.015';

# Register warnings category
use warnings::register;

use if $] ge '5.010000', 're', 'regexp_pattern';

# Dependencies
use Carp           ();
use File::Basename ();
use File::Spec     ();
use List::Util     ();
use Number::Compare 0.02;
use Scalar::Util ();
use Text::Glob   ();
use Try::Tiny;

#--------------------------------------------------------------------------#
# constructors and meta methods
#--------------------------------------------------------------------------#

sub new {
    my $class = shift;
    $class = ref $class if ref $class;
    return bless { rules => [] }, $class;
}

sub clone {
    my $self = shift;
    return bless _my_clone( {%$self} ), ref $self;
}

# avoid XS/buggy dependencies for a simple recursive clone; we clone
# fully instead of just 'rules' in case we get subclassed and they
# add attributes
sub _my_clone {
    my $d = shift;
    if ( ref $d eq 'HASH' ) {
        return {
            map { ; my $v = $d->{$_}; $_ => ( ref($v) ? _my_clone($v) : $v ) }
              keys %$d
        };
    }
    elsif ( ref $d eq 'ARRAY' ) {
        return [ map { ref($_) ? _my_clone($_) : $_ } @$d ];
    }
    else {
        return $d;
    }
}

sub add_helper {
    my ( $class, $name, $coderef, $skip_negation ) = @_;
    $class = ref $class if ref $class;
    if ( !$class->can($name) ) {
        no strict 'refs'; ## no critic
        *$name = sub {
            my $self = shift;
            my $rule = $coderef->(@_);
            $self->and($rule);
        };
        if ( !$skip_negation ) {
            *{"not_$name"} = sub {
                my $self = shift;
                my $rule = $coderef->(@_);
                $self->not($rule);
            };
        }
    }
    else {
        Carp::croak("Can't add rule '$name' because it conflicts with an existing method");
    }
}

#--------------------------------------------------------------------------#
# Implementation-specific method; these may be overridden by subclasses
# to test/return results of file wrappers like Path::Class or IO::All
# or to provide custom error handler, visitors or other features
#--------------------------------------------------------------------------#

sub _objectify {
    my ( $self, $path ) = @_;
    return "$path";
}

## We inline this below, but a subclass equivalent would be this:
##sub _children {
##    my $self = shift;
##    my $path = "" . shift; # stringify objects
##    opendir( my $dh, $path );
##    return map { [ $_, "$path/$_" ] } grep { $_ ne "." && $_ ne ".." } readdir $dh;
##}

# The _stringify option controls whether the string form of an object is cached
# for iteration control.  This is generally a good idea to avoid extra overhead,
# but subclasses can override this if necessary

sub _defaults {
    return (
        _stringify      => 1,
        follow_symlinks => 1,
        depthfirst      => 0,
        sorted          => 1,
        loop_safe       => ( $^O eq 'MSWin32' ? 0 : 1 ),       # No inode #'s on Windows
        error_handler   => sub { die sprintf( "%s: %s", @_ ) },
        visitor         => undef,
    );
}

sub _fast_defaults {
    return (
        _stringify      => 1,
        follow_symlinks => 1,
        depthfirst      => -1,
        sorted          => 0,
        loop_safe       => 0,
        error_handler   => undef,
        visitor         => undef,
    );
}

#--------------------------------------------------------------------------#
# iteration methods
#--------------------------------------------------------------------------#

sub iter {
    my $self = shift;
    $self->_iter( { $self->_defaults }, @_ );
}

sub iter_fast {
    my $self = shift;
    $self->_iter( { $self->_fast_defaults }, @_ );
}

sub _iter {
    my $self     = shift;
    my $defaults = shift;
    my $args =
        ref( $_[0] )  && !Scalar::Util::blessed( $_[0] )  ? shift
      : ref( $_[-1] ) && !Scalar::Util::blessed( $_[-1] ) ? pop
      :                                                     {};
    my %opts = ( %$defaults, %$args );

    # unroll these for efficiency
    my $opt_stringify       = $opts{_stringify};
    my $opt_depthfirst      = $opts{depthfirst};
    my $opt_follow_symlinks = $opts{follow_symlinks};
    my $opt_sorted          = $opts{sorted};
    my $opt_loop_safe       = $opts{loop_safe};
    my $opt_error_handler   = $opts{error_handler};
    my $opt_relative        = $opts{relative};
    my $opt_visitor         = $opts{visitor};
    my $has_rules           = @{ $self->{rules} };
    my $stash               = {};

    my $opt_report_symlinks =
      defined( $opts{report_symlinks} )
      ? $opts{report_symlinks}
      : $opts{follow_symlinks};

    # if not subclassed, we want to inline
    my $can_children = $self->can("_children");

    # queue structure: flat list of (unnested) tuples of 4 (object,
    # basename, depth, origin).  If object is a coderef, it's a deferred
    # directory list.  If the object is an arrayref, then that's a special
    # case signal that it was already of interest and can finally be
    # returned for postorder searches
    my @queue =
      map {
        my $i = $self->_objectify($_);
        ( $i, File::Basename::basename("$_"), 0, $i )
      } @_ ? @_ : '.';

    return sub {
        LOOP: {
            my ( $item, $base, $depth, $origin ) = splice( @queue, 0, 4 );
            return unless $item;
            if ( ref $item eq 'CODE' ) {
                # replace placeholder with children
                unshift @queue, $item->();
                redo LOOP;
            }
            return $item->[0] if ref $item eq 'ARRAY'; # deferred for postorder
            my $string_item = $opt_stringify ? "$item" : $item;

            # by default, we're interested in everything and prune nothing
            my ( $interest, $prune ) = ( 1, 0 );

            if ( -l $string_item ) {
                $prune = 1 if !$opt_follow_symlinks;
                redo LOOP if !$opt_report_symlinks;
            }

            if ($has_rules) {
                local $_ = $item;
                $stash->{_depth} = $depth;
                if ($opt_error_handler) {
                    $interest = try { $self->test( $item, $base, $stash ) }
                    catch { $opt_error_handler->( $item, $_ ) };
                }
                else {
                    $interest = $self->test( $item, $base, $stash );
                }
                # New way to signal prune is returning a reference to a scalar.
                # Value of the scalar indicates if it should be returned by the
                # iterator or not
                if ( ref $interest eq 'SCALAR' ) {
                    $prune    = 1;
                    $interest = $$interest;
                }
            }

            # if we have a visitor, we call it like a custom rule
            if ( $opt_visitor && $interest ) {
                local $_ = $item;
                $stash->{_depth} = $depth;
                $opt_visitor->( $item, $base, $stash );
            }

            # if it's a directory, maybe add children to the queue
            if (   ( -d $string_item )
                && ( !$prune )
                && ( !$opt_loop_safe || $self->_is_unique( $string_item, $stash ) ) )
            {
                if ( !-r $string_item ) {
                    warnings::warnif("Directory '$string_item' is not readable. Skipping it");
                }
                else {
                    my $depth_p1 = $depth + 1;
                    my $next;
                    if ($can_children) {
                        $next = sub {
                            my @paths = $can_children->( $self, $item );
                            if ($opt_sorted) {
                                @paths = sort { "$a->[0]" cmp "$b->[0]" } @paths;
                            }
                            map { ( $_->[1], $_->[0], $depth_p1, $origin ) } @paths;
                        };
                    }
                    else {
                        $next = sub {
                            my $dh;
                            # Windows can return true for -r but still fail opendir.
                            if ( ! opendir( $dh, $string_item ) ) {
                                warnings::warnif("Directory '$string_item' is not readable. Skipping it");
                                return;
                            }
                            if ($opt_sorted) {
                                map { ( "$string_item/$_", $_, $depth_p1, $origin ) }
                                  sort { $a cmp $b } grep { $_ ne "." && $_ ne ".." } readdir $dh;
                            }
                            else {
                                map { ( "$string_item/$_", $_, $depth_p1, $origin ) }
                                  grep { $_ ne "." && $_ ne ".." } readdir $dh;
                            }
                        };
                    }

                    if ($opt_depthfirst) {
                        # either preorder (parents before kids) or
                        # postorder (parents after kids); for postorder,
                        # requeue current item as a reference to signal it
                        # can be returned without being retested
                        unshift @queue,
                          [
                            (
                                  $opt_relative
                                ? $self->_objectify( File::Spec->abs2rel( $string_item, $origin ) )
                                : $item
                            )
                          ],
                          undef, undef, undef
                          if $interest && $opt_depthfirst > 0;
                        unshift @queue, $next, undef, undef, undef;
                        redo LOOP if $opt_depthfirst > 0;
                    }
                    else {
                        # breadth-first: add placeholder for children at the end
                        push @queue, $next, undef, undef, undef;
                    }
                }
            } # end of "is directory maybe with children"
            return (
                  $opt_relative
                ? $self->_objectify( File::Spec->abs2rel( $string_item, $origin ) )
                : $item
            ) if $interest;
            redo LOOP;
        }
    };
}

sub all {
    my $self = shift;
    return $self->_all( $self->iter(@_) );
}

sub all_fast {
    my $self = shift;
    return $self->_all( $self->iter_fast(@_) );
}

sub _all {
    my $self = shift;
    my $iter = shift;
    if (wantarray) {
        my @results;
        while ( defined( my $item = $iter->() ) ) {
            push @results, $item;
        }
        return @results;
    }
    elsif ( defined wantarray ) {
        my $count = 0;
        $count++ while defined $iter->();
        return $count;
    }
    else {
        1 while defined $iter->();
    }
}

#--------------------------------------------------------------------------#
# logic methods
#--------------------------------------------------------------------------#

sub and {
    my $self = shift;
    push @{ $self->{rules} }, $self->_rulify(@_);
    return $self;
}

sub or {
    my $self    = shift;
    my @rules   = $self->_rulify(@_);
    my $coderef = sub {
        my ( $result, $prune );
        for my $rule (@rules) {
            $result = $rule->(@_);
            # once any rule says to prune, we remember that
            $prune ||= ref($result) eq 'SCALAR';
            # extract whether constraint was met
            $result = $$result if ref($result) eq 'SCALAR';
            # shortcut if met, propagating prune state
            return ( $prune ? \1 : 1 ) if $result;
        }
        return ( $prune ? \$result : $result )
          ; # may or may not be met, but propagate prune state
    };
    return $self->and($coderef);
}

sub not {
    my $self    = shift;
    my $obj     = $self->new->and(@_);
    my $coderef = sub {
        my $result = $obj->test(@_);
        return ref($result) ? \!$$result : !$result; # invert, but preserve prune
    };
    return $self->and($coderef);
}

sub skip {
    my $self    = shift;
    my @rules   = @_;
    my $obj     = $self->new->or(@rules);
    my $coderef = sub {
        my $result = $obj->test(@_);
        my ( $prune, $interest );
        if ( ref($result) eq 'SCALAR' ) {
            # test told us to prune, so make that sticky
            # and also skip it
            $prune    = 1;
            $interest = 0;
        }
        else {
            # prune if test was true
            $prune = $result;
            # negate test result
            $interest = !$result;
        }
        return $prune ? \$interest : $interest;
    };
    return $self->and($coderef);
}

sub test {
    my ( $self, $item, $base, $stash ) = @_;
    my ( $result, $prune );
    for my $rule ( @{ $self->{rules} } ) {
        $result = $rule->( $item, $base, $stash ) || 0;
        if ( !ref($result) && $result eq '0 but true' ) {
            Carp::croak("0 but true no longer supported by custom rules");
        }
        # once any rule says to prune, we remember that
        $prune ||= ref($result) eq 'SCALAR';
        # extract whether constraint was met
        $result = $$result if ref($result) eq 'SCALAR';
        # shortcut if not met, propagating prune state
        return ( $prune ? \0 : 0 ) if !$result;
    }
    return ( $prune ? \1 : 1 ); # all constraints met, but propagate prune state
}

#--------------------------------------------------------------------------#
# private methods
#--------------------------------------------------------------------------#

sub _rulify {
    my ( $self, @args ) = @_;
    my @rules;
    for my $arg (@args) {
        my $rule;
        if ( Scalar::Util::blessed($arg) && $arg->isa("Path::Iterator::Rule") ) {
            $rule = sub { $arg->test(@_) };
        }
        elsif ( ref($arg) eq 'CODE' ) {
            $rule = $arg;
        }
        else {
            Carp::croak("Rules must be coderef or Path::Iterator::Rule");
        }
        push @rules, $rule;
    }
    return @rules;
}

sub _is_unique {
    my ( $self, $string_item, $stash ) = @_;
    my $unique_id;
    my @st = eval { stat $string_item };
    @st = eval { lstat $string_item } unless @st;
    if (@st) {
        $unique_id = join( ",", $st[0], $st[1] );
    }
    else {
        my $type = -d $string_item ? 'directory' : 'file';
        warnings::warnif("Could not stat $type '$string_item'");
        $unique_id = $string_item;
    }
    return !$stash->{_seen}{$unique_id}++;
}

#--------------------------------------------------------------------------#
# built-in helpers
#--------------------------------------------------------------------------#

sub _regexify {
    my ( $re, $add ) = @_;
    $add ||= '';
    my $new = ref($re) eq 'Regexp' ? $re : Text::Glob::glob_to_regex($re);
    return $new unless $add;
    my ( $pattern, $flags ) = _split_re($new);
    my $new_flags = $add ? _reflag( $flags, $add ) : "";
    return qr/$new_flags$pattern/;
}

sub _split_re {
    my $value = shift;
    if ( $] ge 5.010 ) {
        return re::regexp_pattern($value);
    }
    else {
        $value =~ s/^\(\?\^?//;
        $value =~ s/\)$//;
        my ( $opt, $re ) = split( /:/, $value, 2 );
        $opt =~ s/\-\w+$//;
        return ( $re, $opt );
    }
}

sub _reflag {
    my ( $orig, $add ) = @_;
    $orig ||= "";

    if ( $] >= 5.014 ) {
        return "(?^$orig$add)";
    }
    else {
        my ( $pos, $neg ) = split /-/, $orig;
        $pos ||= "";
        $neg ||= "";
        $neg =~ s/i//;
        $neg = "-$neg" if length $neg;
        return "(?$add$pos$neg)";
    }
}

# "simple" helpers take no arguments
my %simple_helpers = (
    directory => sub { -d $_ },           # see also -d => dir below
    dangling => sub { -l $_ && !stat $_ },
);

while ( my ( $k, $v ) = each %simple_helpers ) {
    __PACKAGE__->add_helper( $k, sub { return $v } );
}

sub _generate_name_matcher {
    my (@patterns) = @_;
    if ( @patterns > 1 ) {
        return sub {
            my $name = "$_[1]";
            return ( List::Util::first { $name =~ $_ } @patterns ) ? 1 : 0;
          }
    }
    else {
        my $pattern = $patterns[0];
        return sub {
            my $name = "$_[1]";
            return $name =~ $pattern ? 1 : 0;
          }
    }
}

# "complex" helpers take arguments
my %complex_helpers = (
    name => sub {
        Carp::croak("No patterns provided to 'name'") unless @_;
        _generate_name_matcher( map { _regexify($_) } @_ );
    },
    iname => sub {
        Carp::croak("No patterns provided to 'iname'") unless @_;
        _generate_name_matcher( map { _regexify( $_, "i" ) } @_ );
    },
    min_depth => sub {
        Carp::croak("No depth argument given to 'min_depth'") unless @_;
        my $min_depth = 0+ shift; # if this warns, do here and not on every file
        return sub {
            my ( $f, $b, $stash ) = @_;
            return $stash->{_depth} >= $min_depth;
          }
    },
    max_depth => sub {
        Carp::croak("No depth argument given to 'max_depth'") unless @_;
        my $max_depth = 0+ shift; # if this warns, do here and not on every file
        return sub {
            my ( $f, $b, $stash ) = @_;
            return 1  if $stash->{_depth} < $max_depth;
            return \1 if $stash->{_depth} == $max_depth;
            return \0;
          }
    },
    shebang => sub {
        Carp::croak("No patterns provided to 'shebang'") unless @_;
        my @patterns = map { _regexify($_) } @_;
        return sub {
            my $f = shift;
            return unless !-d $f;
            open my $fh, "<", $f;
            my $shebang = <$fh>;
            return unless defined $shebang;
            return ( List::Util::first { $shebang =~ $_ } @patterns ) ? 1 : 0;
        };
    },
    contents_match => sub {
        my @regexp = @_;
        my $filter = ':encoding(UTF-8)';
        $filter = shift @regexp unless ref $regexp[0];
        return sub {
            my $f = shift;
            return unless !-d $f;
            my $contents = do {
                local $/ = undef;
                open my $fh, "<$filter", $f;
                <$fh>;
            };
            for my $re (@regexp) {
                return 1 if $contents =~ $re;
            }
            return 0;
        };
    },
    line_match => sub {
        my @regexp = @_;
        my $filter = ':encoding(UTF-8)';
        $filter = shift @regexp unless ref $regexp[0];
        return sub {
            my $f = shift;
            return unless !-d $f;
            open my $fh, "<$filter", $f;
            while ( my $line = <$fh> ) {
                for my $re (@regexp) {
                    return 1 if $line =~ $re;
                }
            }
            return 0;
        };
    },
);

while ( my ( $k, $v ) = each %complex_helpers ) {
    __PACKAGE__->add_helper( $k, $v );
}

# skip_dirs
__PACKAGE__->add_helper(
    skip_dirs => sub {
        Carp::croak("No patterns provided to 'skip_dirs'") unless @_;
        my $name_check = Path::Iterator::Rule->new->name(@_);
        return sub {
            return \0 if -d $_[0] && $name_check->test(@_);
            return 1; # otherwise, like a null rule
          }
      } => 1 # don't create not_skip_dirs
);

__PACKAGE__->add_helper(
    skip_subdirs => sub {
        Carp::croak("No patterns provided to 'skip_subdirs'") unless @_;
        my $name_check = Path::Iterator::Rule->new->name(@_);
        return sub {
            my ( $f, $b, $stash ) = @_;
            return \0 if -d $f && $stash->{_depth} && $name_check->test(@_);
            return 1; # otherwise, like a null rule
          }
      } => 1 # don't create not_skip_dirs
);

# X_tests adapted from File::Find::Rule
#<<< do not perltidy this
my %X_tests = (
    -r  =>  readable           =>  -R  =>  r_readable      =>
    -w  =>  writeable          =>  -W  =>  r_writeable     =>
    -w  =>  writable           =>  -W  =>  r_writable      =>
    -x  =>  executable         =>  -X  =>  r_executable    =>
    -o  =>  owned              =>  -O  =>  r_owned         =>

    -e  =>  exists             =>  -f  =>  file            =>
    -z  =>  empty              =>  -d  =>  dir             =>
    -s  =>  nonempty           =>  -l  =>  symlink         =>
                               =>  -p  =>  fifo            =>
    -u  =>  setuid             =>  -S  =>  socket          =>
    -g  =>  setgid             =>  -b  =>  block           =>
    -k  =>  sticky             =>  -c  =>  character       =>
                               =>  -t  =>  tty             =>
    -T  =>  ascii              =>
    -B  =>  binary             =>
);
#>>>

while ( my ( $op, $name ) = each %X_tests ) {
    my $coderef = eval "sub { $op \$_ }"; ## no critic
    __PACKAGE__->add_helper( $name, sub { return $coderef } );
}

my %time_tests = ( -A => accessed => -M => modified => -C => changed => );

while ( my ( $op, $name ) = each %time_tests ) {
    my $filetest = eval "sub { $op \$_ }"; ## no critic
    my $coderef  = sub {
        Carp::croak("The '$name' test requires a single argument") unless @_ == 1;
        my $comparator = Number::Compare->new(shift);
        return sub { return $comparator->( $filetest->() ) };
    };
    __PACKAGE__->add_helper( $name, $coderef );
}

# stat tests adapted from File::Find::Rule
my @stat_tests = qw(
  dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks
);

for my $i ( 0 .. $#stat_tests ) {
    my $name    = $stat_tests[$i];
    my $coderef = sub {
        Carp::croak("The '$name' test requires a single argument") unless @_ == 1;
        my $comparator = Number::Compare->new(shift);
        return sub { return $comparator->( ( stat($_) )[$i] ) };
    };
    __PACKAGE__->add_helper( $name, $coderef );
}

# VCS rules adapted from File::Find::Rule::VCS
my %vcs_rules = (
    skip_cvs => sub {
        return Path::Iterator::Rule->new->skip_dirs('CVS')->not_name(qr/\.\#$/);
    },
    skip_rcs => sub {
        return Path::Iterator::Rule->new->skip_dirs('RCS')->not_name(qr/,v$/);
    },
    skip_git => sub {
        return Path::Iterator::Rule->new->skip_dirs('.git');
    },
    skip_svn => sub {
        return Path::Iterator::Rule->new->skip_dirs(
            ( $^O eq 'MSWin32' ) ? ( '.svn', '_svn' ) : ('.svn') );
    },
    skip_bzr => sub {
        return Path::Iterator::Rule->new->skip_dirs('.bzr');
    },
    skip_hg => sub {
        return Path::Iterator::Rule->new->skip_dirs('.hg');
    },
    skip_darcs => sub {
        return Path::Iterator::Rule->new->skip_dirs('_darcs');
    },
    skip_vcs => sub {
        return Path::Iterator::Rule->new->skip_dirs(qw/.git .bzr .hg _darcs CVS RCS/)
          ->skip_svn->not_name( qr/\.\#$/, qr/,v$/ );
    },
);

while ( my ( $name, $coderef ) = each %vcs_rules ) {
    __PACKAGE__->add_helper( $name, $coderef, 1 ); # don't create not_*
}

# perl rules adapted from File::Find::Rule::Perl
my %perl_rules = (
    perl_module    => sub { return Path::Iterator::Rule->new->file->name('*.pm') },
    perl_pod       => sub { return Path::Iterator::Rule->new->file->name('*.pod') },
    perl_test      => sub { return Path::Iterator::Rule->new->file->name('*.t') },
    perl_installer => sub {
        return Path::Iterator::Rule->new->file->name( 'Makefile.PL', 'Build.PL' );
    },
    perl_script => sub {
        return Path::Iterator::Rule->new->file->or(
            Path::Iterator::Rule->new->name('*.pl'),
            Path::Iterator::Rule->new->shebang(qr/#!.*\bperl\b/),
        );
    },
    perl_file => sub {
        return Path::Iterator::Rule->new->or(
            Path::Iterator::Rule->new->perl_module, Path::Iterator::Rule->new->perl_pod,
            Path::Iterator::Rule->new->perl_test,   Path::Iterator::Rule->new->perl_installer,
            Path::Iterator::Rule->new->perl_script,
        );
    },
);

while ( my ( $name, $coderef ) = each %perl_rules ) {
    __PACKAGE__->add_helper( $name, $coderef );
}

1;

=head1 SYNOPSIS

  use Path::Iterator::Rule;

  my $rule = Path::Iterator::Rule->new; # match anything
  $rule->file->size(">10k");         # add/chain rules

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

This module iterates over files and directories to identify ones matching a
user-defined set of rules.  The API is based heavily on L<File::Find::Rule>,
but with more explicit distinction between matching rules and options that
influence how directories are searched.  A C<Path::Iterator::Rule> object is a
collection of rules (match criteria) with methods to add additional criteria.
Options that control directory traversal are given as arguments to the method
that generates an iterator.

Here is a summary of features for comparison to other file finding modules:

=for :list
* provides many "helper" methods for specifying rules
* offers (lazy) iterator and flattened list interfaces
* custom rules implemented with callbacks
* breadth-first (default) or pre- or post-order depth-first searching
* follows symlinks (by default, but can be disabled)
* directories visited only once (no infinite loop; can be disabled)
* doesn't chdir during operation
* provides an API for extensions

As a convenience, the L<PIR> module is an empty subclass of this one
that is less arduous to type for one-liners.

B<Note>: paths are constructed with unix-style forward-slashes for
efficiency rather than using L<File::Spec>.  If proper path separators are
needed, call L<canonpath|File::Spec/canonpath> on the search results.

=cut

# vim: ts=4 sts=4 sw=4 et:
