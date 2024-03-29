use strict;
use warnings;

use 5.008001;

use ExtUtils::MakeMaker 6.17;

my %WriteMakefileArgs = (
  "ABSTRACT" => "Iterative, recursive file finder",
  "AUTHOR" => "David Golden <dagolden\@cpan.org>",
  "CONFIGURE_REQUIRES" => {
    "ExtUtils::MakeMaker" => "6.17"
  },
  "DISTNAME" => "Path-Iterator-Rule",
  "LICENSE" => "apache",
  "MIN_PERL_VERSION" => "5.008001",
  "NAME" => "Path::Iterator::Rule",
  "PREREQ_PM" => {
    "Carp" => 0,
    "File::Basename" => 0,
    "File::Spec" => 0,
    "List::Util" => 0,
    "Number::Compare" => "0.02",
    "Scalar::Util" => 0,
    "Text::Glob" => 0,
    "Try::Tiny" => 0,
    "if" => 0,
    "strict" => 0,
    "warnings" => 0,
    "warnings::register" => 0
  },
  "TEST_REQUIRES" => {
    "Exporter" => 0,
    "ExtUtils::MakeMaker" => 0,
    "File::Spec" => 0,
    "File::Temp" => 0,
    "File::pushd" => 0,
    "Path::Tiny" => 0,
    "Test::Deep" => 0,
    "Test::Filename" => "0.03",
    "Test::More" => "0.92",
    "lib" => 0,
    "parent" => 0
  },
  "VERSION" => "1.016",
  "test" => {
    "TESTS" => "t/*.t"
  }
);


my %FallbackPrereqs = (
  "Carp" => 0,
  "Exporter" => 0,
  "ExtUtils::MakeMaker" => 0,
  "File::Basename" => 0,
  "File::Spec" => 0,
  "File::Temp" => 0,
  "File::pushd" => 0,
  "List::Util" => 0,
  "Number::Compare" => "0.02",
  "Path::Tiny" => 0,
  "Scalar::Util" => 0,
  "Test::Deep" => 0,
  "Test::Filename" => "0.03",
  "Test::More" => "0.92",
  "Text::Glob" => 0,
  "Try::Tiny" => 0,
  "if" => 0,
  "lib" => 0,
  "parent" => 0,
  "strict" => 0,
  "warnings" => 0,
  "warnings::register" => 0
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.63_03) } ) {
  delete $WriteMakefileArgs{TEST_REQUIRES};
  delete $WriteMakefileArgs{BUILD_REQUIRES};
  $WriteMakefileArgs{PREREQ_PM} = \%FallbackPrereqs;
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);
