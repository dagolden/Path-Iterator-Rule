use 5.006;
use strict;
use warnings;
use Test::More 0.92;
use Test::Filename 0.03;
use Path::Tiny;
use File::Temp;
use File::pushd qw/pushd/;

use lib 't/lib';
use PCNTest;

use Path::Iterator::Rule;

#--------------------------------------------------------------------------#

{
  my ($rule, @files);

  my $td = make_tree(qw(file1.txt));
  path($td, 'file2.txt')->spew(map "$_\n", qw( foo bar baz ));
  path($td, 'file3.txt')->spew(            qw( foo bar baz ));

  is_deeply(
    [ sort Path::Iterator::Rule->new->file->line_match(qr{foo.*baz}s)->all($td) ],
    [ sort map "$_", path($td, 'file3.txt') ],
  );
  
  is_deeply(
    [ sort Path::Iterator::Rule->new->file->not_line_match("iso-8859-1", qr{foo.*baz}s)->all($td) ],
    [ sort map "$_", path($td, 'file1.txt'), path($td, 'file2.txt') ],
  );
  
  is_deeply(
    [ Path::Iterator::Rule->new->file->contents_match(qr{foo.*baz}s)->all($td) ],
    [ sort map "$_", path($td, 'file2.txt'), path($td, 'file3.txt') ],
  );
  
  is_deeply(
    [ Path::Iterator::Rule->new->file->not_contents_match(qr{foo.*baz}s)->all($td) ],
    [ sort map "$_", path($td, 'file1.txt') ],
  );
}

done_testing;
# COPYRIGHT
