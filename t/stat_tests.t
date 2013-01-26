use 5.006;
use strict;
use warnings;
use Test::More 0.92;
use Test::Filename;
use Path::Class;
use File::Temp;
use File::pushd qw/pushd/;

use lib 't/lib';
use PCNTest;

use Path::Iterator::Rule;

sub copy {
  my ($src, $dst) = @_;
  open my $fh, ">", $dst;
  print {$fh} do { local (@ARGV, $/) = $src; <> };
}

#--------------------------------------------------------------------------#

{
  my ($rule, @files);

  my $td = make_tree(qw(
    data/file1.txt
  ));

  my $changes = file($td, 'data', 'Changes');

  copy( file('Changes'), $changes );
  
  $rule = Path::Iterator::Rule->new->file;

  @files = ();
  @files = $rule->all($td);
  is( scalar @files, 2, "Any file") or diag explain \@files;

  $rule = Path::Iterator::Rule->new->file->size(">0k");
  @files = ();
  @files = $rule->all($td);
  filename_is( $files[0], $changes, "size > 0") or diag explain \@files;

}

done_testing;
# COPYRIGHT
