use 5.006;
use strict;
use warnings;
use Test::More 0.92;
use Path::Class;
use File::Temp;
use Test::Deep qw/cmp_deeply/;
use File::pushd qw/pushd/;

use lib 't/lib';
use PCNTest;

use Path::Iterator::Rule;

#--------------------------------------------------------------------------#

{
  my @tree = qw(
    aaaa.txt
    gggg.txt
    cccc.txt
    dddd.txt
    bbbb.txt
    eeee.txt
  );

  my $td = make_tree(@tree);

  opendir( my $dh, "$td" );
  my @expected = ( grep { $_ ne "." && $_ ne ".." } readdir $dh );
  closedir $dh;

  my ($iter, @files);
  my $rule = Path::Iterator::Rule->new->file;

  @files = map  { unixify($_, $td) } $rule->all($td);
  cmp_deeply( \@files, [ sort @expected ], "all() gives sorted order")
    or diag explain \@files;

  @files = map  { unixify($_, $td) } $rule->all_fast($td);
  cmp_deeply( \@files, \@expected, "all_fast() gives disk order")
    or diag explain \@files;

}

done_testing;
# COPYRIGHT
