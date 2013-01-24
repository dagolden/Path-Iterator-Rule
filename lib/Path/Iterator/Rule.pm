use 5.010; # re::regexp_pattern
use strict;
use warnings;

package Path::Iterator::Rule;
# ABSTRACT: Iterative, recursive file finder
# VERSION

# Register warnings category
use warnings::register;

# Dependencies
use re 'regexp_pattern';
use Carp;
use Data::Clone qw/data_clone/;
use File::Basename qw/basename/;
use List::Util qw/first/;
use Number::Compare 0.02;
use Scalar::Util qw/blessed/;
use Text::Glob qw/glob_to_regex/;
use Try::Tiny;

use namespace::clean;

#--------------------------------------------------------------------------#
# constructors and meta methods
#--------------------------------------------------------------------------#

sub new {
    my $class = shift;
    return bless { rules => [] }, ref $class || $class;
}

sub clone {
    my $self = shift;
    return data_clone($self);
}

sub add_helper {
    my ( $class, $name, $coderef, $skip_negation ) = @_;
    $class = ref $class || $class;
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
        Carp::carp("Can't add rule '$name' because it conflicts with an existing method");
    }
}

#--------------------------------------------------------------------------#
# Implementation-specific method; these may be overridden by subclasses
# to test/return results of file wrappers like Path::Class or IO::All
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

#--------------------------------------------------------------------------#
# iteration methods
#--------------------------------------------------------------------------#

sub iter {
    my $self     = shift;
    my %defaults = (
        follow_symlinks => 1,
        depthfirst      => 0,
        sorted          => 1,
        loop_safe       => ( $^O eq 'MSWin32' ? 0 : 1 ),       # No inode #'s on Windows
        error_handler   => sub { die sprintf( "%s: %s", @_ ) },
    );
    $self->_iter( \%defaults, @_ );
}

sub iter_fast {
    my $self     = shift;
    my %defaults = (
        follow_symlinks => 1,
        depthfirst      => -1,
        sorted          => 0,
        loop_safe       => 0,
        error_handler   => undef,
    );
    $self->_iter( \%defaults, @_ );
}

sub _iter {
    my $self     = shift;
    my $defaults = shift;
    my $args =
        ref( $_[0] )  && !blessed( $_[0] )  ? shift
      : ref( $_[-1] ) && !blessed( $_[-1] ) ? pop
      :                                       {};
    my %opts = ( %$defaults, %$args );

    # unroll these for efficiency
    my $opt_depthfirst      = $opts{depthfirst};
    my $opt_follow_symlinks = $opts{follow_symlinks};
    my $opt_sorted          = $opts{sorted};
    my $opt_loop_safe       = $opts{loop_safe};
    my $opt_error_handler   = $opts{error_handler};
    my $has_rules           = @{ $self->{rules} };
    my $stash               = {};

    # if not subclassed, we want to inline
    my $can_children = $self->can("_children");

    # queue structure: flat list in tuples of 3: (object, basename, depth)
    # if object is arrayref, then that's a special case signal that it
    # was already of interest and can finally be returned for postorder searches
    my @queue =
      map { ( $self->_objectify($_), basename("$_"), 0 ) } @_ ? @_ : '.';

    return sub {
        LOOP: {
            my ( $item, $base, $depth ) = splice( @queue, 0, 3 );
            return unless $item;
            return $item->[0] if ref $item eq 'ARRAY'; # deferred for postorder
            my $string_item = "$item";
            if ( !$opt_follow_symlinks ) {
                redo LOOP if -l $string_item;
            }

            # by default, we're interested in everything and prune nothing
            my ( $interest, $prune ) = ( 1, 0 );
            if ($has_rules) {
                local $_ = $item;
                $stash->{_depth} = $depth;
                if ($opt_error_handler) {
                    $interest =
                      try { $self->test( $item, $base, $stash ) }
                    catch { $opt_error_handler->( $item, $_ ) };
                }
                else {
                    $interest = $self->test( $item, $base, $stash );
                }
                $prune = $interest && !( 0 + $interest ); # capture "0 but true"
                $interest += 0;                           # then ignore "but true"
            }

            # if it's a directory, maybe add children to the queue
            if (   -d $string_item
                && !$prune
                && ( !$opt_loop_safe || $self->_is_unique( $string_item, $stash ) ) )
            {
                if ( !-r $string_item ) {
                    warnings::warnif("Directory '$string_item' is not readable. Skipping it");
                }
                else {
                    my @next;
                    my $depth_p1 = $depth + 1;
                    if ( $can_children ) {
                        my @paths = $can_children->($self, $item);
                        if ($opt_sorted) {
                            @paths = sort { "$a->[0]" cmp "$b->[0]" } @paths;
                        }
                        @next = map { ( $_->[1], $_->[0], $depth_p1 ) } @paths;
                    }
                    else {
                        opendir( my $dh, $string_item );
                        if ($opt_sorted) {
                            @next= map { ("$string_item/$_", $_, $depth_p1) } sort { $a cmp $b } grep { $_ ne "." && $_ ne ".." } readdir $dh;
                        }
                        else {
                            @next= map { ("$string_item/$_", $_, $depth_p1) } grep { $_ ne "." && $_ ne ".." } readdir $dh;
                        }
                    }


                    if ($opt_depthfirst) {
                        # for postorder, requeue as reference to signal it can be returned
                        # without being retested
                        push @next, [$item], $base, $depth
                          if $interest && $opt_depthfirst > 0;
                        unshift @queue, @next;
                        redo LOOP if $opt_depthfirst > 0;
                    }
                    else {
                        push @queue, @next;
                    }
                }
            }
            return $item
              if $interest;
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
    my @results;
    while ( my $item = $iter->() ) {
        push @results, $item;
    }
    return @results;
}

#--------------------------------------------------------------------------#
# logic methods
#--------------------------------------------------------------------------#

sub and {
    my $self = shift;
    push @{ $self->{rules} }, $self->_rulify( "and", @_ );
    return $self;
}

sub or {
    my $self    = shift;
    my @rules   = $self->_rulify( "or", @_ );
    my $coderef = sub {
        my $result;
        for my $rule (@rules) {
            $result = $rule->(@_) || 0;
            return $result if $result; # want to shortcut on "0 but true"
        }
        return $result;
    };
    return $self->and($coderef);
}

sub not {
    my $self    = shift;
    my @rules   = $self->_rulify( "not", @_ );
    my $obj     = $self->new->and(@rules);
    my $coderef = sub {
        my $result = $obj->test(@_);
        # XXX what to do about "0 but true"? Ignore it?
        return $result ? "0" : "1";
    };
    return $self->and($coderef);
}

sub skip {
    my $self    = shift;
    my @rules   = $self->_rulify( "not", @_ );
    my $obj     = $self->new->or(@rules);
    my $coderef = sub {
        my $result = $obj->test(@_);
        return $result ? "0 but true" : "1";
    };
    return $self->and($coderef);
}

sub test {
    my ( $self, $item, $base, $stash ) = @_;
    my $result;
    for my $rule ( @{ $self->{rules} } ) {
        $result = $rule->( $item, $base, $stash ) || 0;
        return $result if !( 0 + $result ); # want to shortcut on "0 but true"
    }
    return $result // 1;                    # not defined means we had no rules
}

#--------------------------------------------------------------------------#
# private methods
#--------------------------------------------------------------------------#

sub _rulify {
    my ( $self, $method, @args ) = @_;
    my @rules;
    for my $arg (@args) {
        my $rule;
        if ( blessed($arg) && $arg->isa("Path::Iterator::Rule") ) {
            $rule = sub { $arg->test(@_) };
        }
        elsif ( ref($arg) eq 'CODE' ) {
            $rule = $arg;
        }
        else {
            Carp::croak("Argument to ->and() must be coderef or Path::Iterator::Rule");
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
    my $new = ref($re) eq 'Regexp' ? $re : glob_to_regex($re);
    my ( $pattern, $flags ) = regexp_pattern($new);
    my $new_flags = $add ? _reflag( $flags, $add ) : "";
    return qr/$new_flags$pattern/;
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
    directory => sub { -d "$_" },             # see also -d => dir below
    dangling => sub { -l "$_" && !stat "$_" },
);

while ( my ( $k, $v ) = each %simple_helpers ) {
    __PACKAGE__->add_helper( $k, sub { return $v } );
}

sub _generate_name_matcher {
    my (@patterns) = @_;
    if ( @patterns > 1 ) {
        return sub {
            my $name = "$_[1]";
            return ( first { $name =~ $_ } @patterns ) ? 1 : 0;
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
        my $min_depth = 0 + shift; # if this warns, do here and not on every file
        return sub {
            my ( $f, $b, $stash ) = @_;
            return $stash->{_depth} >= $min_depth;
          }
    },
    max_depth => sub {
        Carp::croak("No depth argument given to 'max_depth'") unless @_;
        my $max_depth = 0 + shift; # if this warns, do here and not on every file
        return sub {
            my ( $f, $b, $stash ) = @_;
            return $stash->{_depth} <= $max_depth ? 1 : "0 but true"; # prune
          }
    },
    shebang => sub {
        Carp::croak("No patterns provided to 'shebang'") unless @_;
        my @patterns = map { _regexify($_) } @_;
        return sub {
            my $f = shift;
            return unless !-d "$f";
            open my $fh, "<", "$f";
            my $shebang = <$fh>;
            return unless defined $shebang;
            return ( first { $shebang =~ $_ } @patterns ) ? 1 : 0;
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
            return "0 but true" if -d "$_[0]" && $name_check->test(@_);
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
    my $coderef = eval "sub { $op qq{\$_} }"; ## no critic
    __PACKAGE__->add_helper( $name, sub { return $coderef } );
}

my %time_tests = ( -A => accessed => -M => modified => -C => changed => );

while ( my ( $op, $name ) = each %time_tests ) {
    my $filetest = eval "sub { $op qq{\$_} }"; ## no critic
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
        return sub { return $comparator->( ( stat("$_") )[$i] ) };
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
    skip_vcs => sub {
        return Path::Iterator::Rule->new->skip_dirs(qw/.git .bzr .hg CVS RCS/)
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
  while ( my $file = $next->() ) {
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

=head1 USAGE

=head2 Constructors

=head3 C<new>

  my $rule = Path::Iterator::Rule->new;

Creates a new rule object that matches any file or directory.  It takes
no arguments. For convenience, it may also be called on an object, in which
case it still returns a new object that matches any file or directory.

=head3 C<clone>

  my $common      = Path::Iterator::Rule->new->file->not_empty;
  my $big_files   = $common->clone->size(">1M");
  my $small_files = $common->clone->size("<10K");

Creates a copy of a rule object.  Useful for customizing different
rule objects against a common base.

=head2 Matching and iteration

=head3 C<iter>

  my $next = $rule->iter( @dirs, \%options);
  while ( my $file = $next->() ) {
    ...
  }

Creates a subroutine reference iterator that returns a single result
when dereferenced.  This iterator is "lazy" -- results are not
pre-computed.

It takes as arguments a list of directories to search and an optional hash
reference of control options.  If no search directories are provided, the
current directory is used (C<".">).  Valid options include:

=for :list
* C<depthfirst> -- Controls order of results.  Valid values are "1" (post-order, depth-first search), "0" (breadth-first search) or "-1" (pre-order, depth-first search). Default is 0.
* C<error_handler> -- Catches errors during execution of rule tests. Default handler dies with the filename and error. If set to undef, error handling is disabled.
* C<follow_symlinks> -- Follow directory symlinks when true. Default is 1.
* C<loop_safe> -- Prevents visiting the same directory more than once when true.  Default is 1.
* C<sorted> -- Whether entries in a directory are sorted before processing. Default is 1.

Filesystem loops might exist from either hard or soft links.  The C<loop_safe>
option prevents infinite loops, but adds some overhead by making C<stat> calls.
Because directories are visited only once when C<loop_safe> is true, matches
could come from a symlinked directory before the real directory depending on
the search order.  To get only the real files, turn off C<follow_symlinks>.
Turning C<loop_safe> off and leaving C<follow_symlinks> on avoids C<stat> calls
and will be fastest, but with the risk of an infinite loop and repeated files.
The default is slow, but safe.

The C<error_handler> parameter must be a subroutine reference.  It will be
called when a rule test throws an exception.  The first argument will be
the file name being inspected and the second argument will be
the exception.

The paths inspected and returned will be relative to the search directories
provided.  If these are absolute, then the paths returned will have absolute
paths.  If these are relative, then the paths returned will have relative
paths.

=head3 C<iter_fast>

This works just like C<iter>, except that it optimizes for speed over
safety. Don't do this unless you're sure you need it and accept
the consequences.  See L</PERFORMANCE> for details.

=head3 C<all>

  my @matches = $rule->all( @dir, \%options );

Returns a list of paths that match the rule.  It takes the same arguments and
has the same behaviors as the C<iter> method.  The C<all> method uses C<iter>
internally to fetch all results.

=head3 C<all_fast>

This works just like C<all>, except that it optimizes for speed over
safety. Don't do this unless you're sure you need it and accept
the consequences.  See L</PERFORMANCE> for details.

=head3 C<test>

  if ( $rule->test( $path ) ) { ... }

Test a file path against a rule.  Used internally, but provided should
someone want to create their own, custom iteration algorithm.

=head2 Logic operations

C<Path::Iterator::Rule> provides three logic operations for adding rules to the
object.  Rules may be either a subroutine reference with specific semantics
(described below in L</EXTENDING>) or another C<Path::Iterator::Rule> object.

=head3 C<and>

  $rule->and( sub { -r -w -x $_ } ); # stacked filetest example
  $rule->and( @more_rules );

Adds one or more constraints to the current rule. E.g. "old rule AND
new1 AND new2 AND ...".  Returns the object to allow method chaining.

=head3 C<or>

  $rule->or(
    $rule->new->name("foo*"),
    $rule->new->name("bar*"),
    sub { -r -w -x $_ },
  );

Takes one or more alternatives and adds them as a constraint to the current
rule. E.g. "old rule AND ( new1 OR new2 OR ... )".  Returns the object to allow
method chaining.

=head3 C<not>

  $rule->not( sub { -r -w -x $_ } );

Takes one or more alternatives and adds them as a negative constraint to the
current rule. E.g. "old rule AND NOT ( new1 AND new2 AND ...)".  Returns the
object to allow method chaining.

=head3 C<skip>

  $rule->skip(
    $rule->new->dir->not_writeable,
    $rule->new->dir->name("foo"),
  );

Takes one or more alternatives and will prune a directory if any of the
criteria match.  For files, it is equivalent to
C<< $rule->not($rule->or(@rules)) >>.  Returns the object to allow method
chaining.

This method should be called as early as possible in the rule chain.
See L</skip_dirs> below for further explanation and an example.

=head1 RULE METHODS

Rule methods are helpers that add constraints.  Internally, they generate a
closure to accomplish the desired logic and add it to the rule object with the
C<and> method.  Rule methods return the object to allow for method chaining.

=head2 File name rules

=head3 C<name>

  $rule->name( "foo.txt" );
  $rule->name( qr/foo/, "bar.*");

The C<name> method takes one or more patterns and creates a rule that is true
if any of the patterns match the B<basename> of the file or directory path.
Patterns may be regular expressions or glob expressions (or literal names).

=head3 C<iname>

  $rule->iname( "foo.txt" );
  $rule->iname( qr/foo/, "bar.*");

The C<iname> method is just like the C<name> method, but matches
case-insensitively.

=head3 C<skip_dirs>

  $rule->skip_dirs( @patterns );

The C<skip_dirs> method skips directories that match one or more patterns.
Patterns may be regular expressions or globs (just like C<name>).  Directories
that match will not be returned from the iterator and will be excluded from
further search.

B<Note:> this rule should be specified early so that it has a chance to
operate before a logical shortcut.  E.g.

  $rule->skip_dirs(".git")->file; # OK
  $rule->file->skip_dirs(".git"); # Won't work

In the latter case, when a ".git" directory is seen, the C<file> rule
shortcuts the rule before the C<skip_dirs> rule has a chance to act.

=head2 File test rules

Most of the C<-X> style filetest are available as boolean rules.  The table
below maps the filetest to its corresponding method name.

   Test | Method               Test |  Method
  ------|-------------        ------|----------------
    -r  |  readable             -R  |  r_readable
    -w  |  writeable            -W  |  r_writeable
    -w  |  writable             -W  |  r_writable
    -x  |  executable           -X  |  r_executable
    -o  |  owned                -O  |  r_owned
        |                           |
    -e  |  exists               -f  |  file
    -z  |  empty                -d  |  directory, dir
    -s  |  nonempty             -l  |  symlink
        |                       -p  |  fifo
    -u  |  setuid               -S  |  socket
    -g  |  setgid               -b  |  block
    -k  |  sticky               -c  |  character
        |                       -t  |  tty
    -T  |  ascii
    -B  |  binary

For example:

  $rule->file->nonempty; # -f -s $file

The -X operators for timestamps take a single argument in a form that
L<Number::Compare> can interpret.

   Test | Method
  ------|-------------
    -A  |  accessed
    -M  |  modified
    -C  |  changed

For example:

  $rule->modified(">1"); # -M $file > 1

=head2 Stat test rules

All of the C<stat> elements have a method that takes a single argument in
a form understood by L<Number::Compare>.

  stat()  |  Method
 --------------------
       0  |  dev
       1  |  ino
       2  |  mode
       3  |  nlink
       4  |  uid
       5  |  gid
       6  |  rdev
       7  |  size
       8  |  atime
       9  |  mtime
      10  |  ctime
      11  |  blksize
      12  |  blocks

For example:

  $rule->size(">10K")

=head2 Depth rules

  $rule->min_depth(3);
  $rule->max_depth(5);

The C<min_depth> and C<max_depth> rule methods take a single argument
and limit the paths returned to a minimum or maximum depth (respectively)
from the starting search directory.

=head2 Perl file rules

  # All perl rules
  $rule->perl_file;

  # Individual perl file rules
  $rule->perl_module;     # .pm files
  $rule->perl_pod;        # .pod files 
  $rule->perl_test;       # .t files
  $rule->perl_installer;  # Makefile.PL or Build.PL
  $rule->perl_script;     # .pl or 'perl' in the shebang

These rule methods match file names (or a shebang line) that are typical
of Perl distribution files.

=head2 Version control file rules

  # Skip all known VCS files
  $rule->skip_vcs;

  # Skip individual VCS files
  $rule->skip_cvs;
  $rule->skip_rcs;
  $rule->skip_svn;
  $rule->skip_git;
  $rule->skip_bzr;
  $rule->skip_hg;

Skips files and/or prunes directories related to a version control system.
Just like C<skip_dirs>, these rules should be specified early to get the
correct behavior.

=head2 Other rules

=head3 C<dangling>

  $rule->symlink->dangling;
  $rule->not_dangling;

The C<dangling> rule method matches dangling symlinks.  Use it or its inverse
to control how dangling symlinks should be treated.

=head3 C<shebang>

  $rule->shebang(qr/#!.*\bperl\b/);

The C<shebang> rule takes a list of regular expressions or glob patterns and
checks them against the first line of a file.

=head2 Negated rules

Most rule methods have a negated form preceded by "not_".

  $rule->not_name("foo.*")

Because this happens automatically, it includes somewhat silly ones like
C<not_nonempty> (which is thus a less efficient way of saying C<empty>).

Rules that skip directories or version control files do not have a negated
version.

=head1 EXTENDING

=head2 Custom rule subroutines

Rules are implemented as (usually anonymous) subroutine callbacks that return
a value indicating whether or not the rule matches.  These callbacks are called
with three arguments.  The first argument is a path, which is
also locally aliased as the C<$_> global variable for convenience in simple
tests.

  $rule->and( sub { -r -w -x $_ } ); # tests $_

The second argument is the basename of the path, which is useful for certain
types of name checks:

  $rule->and( sub { $_[1] =~ /foo|bar/ } ); "foo" or "bar" in basename;

The third argument is a hash reference that can be used to maintain state.
Keys beginning with an underscore are B<reserved> for C<Path::Iterator::Rule>
to provide additional data about the search in progress.
For example, the C<_depth> key is used to support minimum and maximum
depth checks.

The custom rule subroutine must return one of three values:

=for :list
* A true value -- indicates the constraint is satisfied
* A false value -- indicates the constraint is not satisfied
* "0 but true" -- a special return value that signals that a directory should not be searched recursively

The C<0 but true> value will shortcut logic (it is treated as "true" for an
"or" rule and "false" for an "and" rule).  For a directory, it ensures that the
directory will not be returned from the iterator and that its children will not
be evaluated either.  It has no effect on files -- it is equivalent to
returning a false value.

For example, this is equivalent to the "max_depth" rule method with
a depth of 3:

  $rule->and(
    sub {
      my ($path, $basename, $stash) = @_;
      return $stash->{_depth} <= 3 ? 1 : "0 but true";
    }
  );

Files of depth 4 will not be returned by the iterator; directories of depth
4 will not be returned and will not be searched.

Generally, if you want to do directory pruning, you are encouraged to use the
L</skip> method instead of writing your own logic using C<0 but true>.

=head2 Extension modules and custom rule methods

One of the strengths of L<File::Find::Rule> is the many CPAN modules
that extend it.  C<Path::Iterator::Rule> provides the C<add_helper> method
to provide a similar mechanism for extensions.

The C<add_helper> class method takes three arguments, a C<name> for the rule
method, a closure-generating callback, and a flag for not generating a negated
form of the rule.  Unless the flag is true, an inverted "not_*" method is
generated automatically.  Extension classes should call this as a class method
to install new rule methods.  For example, this adds a "foo" method that checks
if the filename is "foo":

  package Path::Iterator::Rule::Foo;

  use Path::Iterator::Rule;

  Path::Iterator::Rule->add_helper(
    foo => sub {
      my @args = @_; # do this to customize closure with arguments
      return sub {
        my ($item, $basename) = shift;
        return if -d "$item";
        return $basename =~ /^foo$/;
      }
    }
  );

  1;

This allows the following rule methods:

  $rule->foo;
  $fule->not_foo;

The C<add_helper> method will warn and ignore a helper with the same name as
an existing method.

=head2 Subclassing

Instead of processing and returning strings, this module may be subclassed
to operate on objects that represent files.  Such objects B<must> stringify
to a file path.

The following private implementation methods must be overridden:

=for :list
* _objectify -- given a path, return an object
* _children -- given a directory, return an (unsorted) list of [ basename, full path ] entries within it, excluding "." and ".."

Note that C<_children> should return a I<list> of I<tuples>, where the tuples
are array references containing basename and full path.

See L<Path::Class::Rule> source for an example.

=head1 LEXICAL WARNINGS

If you run with lexical warnings enabled, C<Path::Iterator::Rule> will issue
warnings in certain circumstances (such as an unreadable directory that must be
skipped).  To disable these categories, put the following statement at the
correct scope:

  no warnings 'Path::Iterator::Rule';

=head1 PERFORMANCE

By default, C<Path::Iterator::Rule> iterator options are "slow but safe".  They
ensure uniqueness, return files in sorted order, and throw nice error messages
if something goes wrong.

If you want speed over safety, set these options:

    %options = (
        loop_safe => 0,
        sorted => 0,
        depthfirst => -1,
        error_handler => undef
    );

Alternatively, use the C<iter_fast> and C<fast_all> methods instead, which set
these options for you.

    $iter = $rule->iter( @dirs, \%options );

    $iter = $rule->iter_fast( @dirs ); # same thing

Depending on the file structure being searched, C<< depthfirst => -1 >> may or
may not be a good choice. If you have lots of nested directories and all the
files at the bottom, a depth first search might do less work or use less
memory, particularly if the search will be halted early (e.g. finding the first
N matches.)

Rules will shortcut on failure, so be sure to put rules likely to fail
early in a rule chain.

Consider:

    $r1 = Path::Iterator::Rule->new->name(qr/foo/)->file;
    $r2 = Path::Iterator::Rule->new->file->name(qr/foo/);

If there are lots of files, but only a few containing "foo", then
C<$r1> above will be faster.

Rules are implemented as code references, so long chains have
some overhead.  Consider testing with a custom coderef that
combines several tests into one.

Consider:

    $r3 = Path::Iterator::Rule->new->and( sub { -x -w -r $_ } );
    $r4 = Path::Iterator::Rule->new->executable->writeable->readable;

Rule C<$r3> above will be much faster, not only because it stacks
the file tests, but because it requires only a single code reference.

=head1 CAVEATS

Some features are still unimplemented:

=for :list
* Untainting options
* Some L<File::Find::Rule> helpers (e.g. C<grep>)
* Extension class loading via C<import()>

Filetest operators and stat rules are subject to the usual portability
considerations.  See L<perlport> for details.

=head1 SEE ALSO

There are many other file finding modules out there.  They all have various
features/deficiencies, depending on your preferences and needs.  Here is an
(incomplete) list of alternatives, with some comparison commentary.

L<Path::Class::Rule> and L<IO::All::Rule> are subclasses of
C<Path::Iterator::Rule> and operate on L<Path::Class> and L<IO::All> objects,
respectively.  Because of this, they are substantially slower on
large directory trees than just using this module directly.

L<File::Find> is part of the Perl core.  It requires the user to write a
callback function to process each node of the search.  Callbacks must use
global variables to determine the current node.  It only supports depth-first
search (both pre- and post-order). It supports pre- and post-processing
callbacks; the former is required for sorting files to process in a directory.
L<File::Find::Closures> can be used to help create a callback for
L<File::Find>.

L<File::Find::Rule> is an object-oriented wrapper around L<File::Find>.  It
provides a number of helper functions and there are many more
C<File::Find::Rule::*> modules on CPAN with additional helpers.  It provides
an iterator interface, but precomputes all the results.

L<File::Next> provides iterators for file, directories or "everything".  It
takes two callbacks, one to match files and one to decide which directories to
descend.  It does not allow control over breadth/depth order, though it does
provide means to sort files for processing within a directory. Like
L<File::Find>, it requires callbacks to use global variables.

L<Path::Class::Iterator> walks a directory structure with an iterator.  It is
implemented as L<Path::Class> subclasses, which adds a degree of extra
complexity. It takes a single callback to define "interesting" paths to return.
The callback gets a L<Path::Class::Iterator::File> or
L<Path::Class::Iterator::Dir> object for evaluation.

L<File::Find::Object> and companion L<File::Find::Object::Rule> are like
File::Find and File::Find::Rule, but without File::Find inside.  They use an
iterator that does not precompute results. They can return
L<File::Find::Object::Result> objects, which give a subset of the utility
of Path::Class objects.  L<File::Find::Object::Rule> appears to be a literal
translation of L<File::Find::Rule>, including oddities like making C<-M> into a
boolean.

L<File::chdir::WalkDir> recursively descends a tree, calling a callback on each
file.  No iterator.  Supports exclusion patterns.  Depth-first post-order by
default, but offers pre-order option. Does not process symlinks.

L<File::Find::Iterator> is based on iterator patterns in Higher Order Perl.  It
allows a filtering callback. Symlinks are followed automatically without
infinite loop protection. No control over order. It offers a "state file"
option for resuming interrupted work.

L<File::Find::Declare> has declarative helper rules, no iterator, is
Moose-based and offers no control over ordering or following symlinks.

L<File::Find::Node> has no iterator, does matching via callback and offers
no control over ordering.

L<File::Set> builds up a set of files to operate on from a list of directories
to include or exclude, with control over recursion.  A callback is applied to
each file (or directory) in the set.  There is no iterator.  There is no
control over ordering.  Symlinks are not followed.  It has several extra
features for checksumming the set and creating tarballs with F</bin/tar>.

=head1 THANKS

Thank you to Ricardo Signes (rjbs) for inspiring me to write yet another file
finder module, for writing file finder optimization benchmarks, and tirelessly
running my code over and over to see if it got faster.

=for :list
* See L<the speed of Perl file finders|http://rjbs.manxome.org/rubric/entry/1981>

=cut

# vim: ts=4 sts=4 sw=4 et:
