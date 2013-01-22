use 5.006;
use strict;
use warnings;
use autodie;
use Test::More 0.92;
use Path::Class;
use File::Temp;
use Test::Deep qw/cmp_deeply/;
use File::pushd qw/pushd/;

use lib 't/lib';
use PCNTest;

use Path::Iterator::Rule;

#--------------------------------------------------------------------------#

my @tree = qw(
  aaaa.txt
  bbbb.txt
  cccc/dddd.txt
  cccc/eeee/ffff.txt
  gggg.txt
);

my $td = make_tree(@tree);

{
  my @files;
  my $rule = Path::Iterator::Rule->new->file->not_name("gggg.txt");
  my $expected = [ qw(
    aaaa.txt
    bbbb.txt
    cccc/dddd.txt
    cccc/eeee/ffff.txt
  )];
  @files = map { unixify($_, $td) } $rule->all($td);
  cmp_deeply( \@files, $expected, "not() test")
    or diag explain { got => \@files, expected => $expected };
}

{
  my @files;
  my $rule = Path::Iterator::Rule->new->file;
  $rule->or(
    $rule->new->name("gggg.txt"),
    $rule->new->name("bbbb.txt"),
  );
  my $expected = [qw/bbbb.txt gggg.txt/];

  @files = map { unixify($_, $td) } $rule->all($td);
  cmp_deeply( \@files, $expected, "or() test")
    or diag explain { got => \@files, expected => $expected };
}

{
  my @files;
  my $rule = Path::Iterator::Rule->new;
  $rule->skip(
    $rule->new->name("gggg.txt"),
    $rule->new->name("cccc"),
  );
  $rule->file;
  my $expected = [qw(
    aaaa.txt
    bbbb.txt
  )];
  @files = map { unixify($_, $td) } $rule->all($td);
  cmp_deeply( \@files, $expected, "skip() test")
    or diag explain { got => \@files, expected => $expected };
}

done_testing;
# COPYRIGHT
