use 5.006;
use strict;
use warnings;
use Test::More 0.92;
use File::Temp;
use File::pushd qw/pushd/;

use lib 't/lib';
use PCNTest;

use Path::Iterator::Rule;

#--------------------------------------------------------------------------#

{
  my $td = make_tree(qw(
    atroot.txt
    empty/
    data/file1.txt
  ));

  my ($iter, @files);
  my $rule = Path::Iterator::Rule->new->file;

  $iter = $rule->iter($td);

  @files = ();
  while ( my $f = $iter->() ) {
    push @files, $f;
  }

  is( scalar @files, 2, "Iterator: two files") or diag explain \@files;
  is( ref $files[0], '', "Iterator: returns string, not object" );

  @files = ();
  @files = $rule->all($td);

  is( scalar @files, 2, "All: two files") or diag explain \@files;

  $rule = Path::Iterator::Rule->new->dir;
  @files = ();
  @files = map { "$_" } $rule->all($td);

  is( scalar @files, 3, "All: 3 directories") or diag explain \@files;

  my $wd = pushd($td);

  @files = ();
  @files = map { "$_" } $rule->all();
  is( scalar @files, 3, "All w/ cwd: 3 directories") or diag explain \@files;

  $rule->skip_dirs(qw/data/);
  @files = ();
  @files = map { "$_" } $rule->all();
  is( scalar @files, 2, "All w/ prune: 2 directories") or diag explain \@files;

  $rule = Path::Iterator::Rule->new->skip_dirs(qr/./)->file;
  @files = ();
  @files = map { "$_" } $rule->all();
  is( scalar @files, 0, "All w/ prune everything: nothing") or diag explain \@files;

  $rule = Path::Iterator::Rule->new->skip_subdirs(qr/./)->file;
  @files = ();
  @files = map { "$_" } $rule->all();
  is( scalar @files, 1, "All w/ prune subdirs: 1 file") or diag explain \@files;
}

done_testing;
# COPYRIGHT
