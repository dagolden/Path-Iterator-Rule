requires "Carp" => "0";
requires "File::Basename" => "0";
requires "File::Spec" => "0";
requires "List::Util" => "0";
requires "Number::Compare" => "0.02";
requires "Scalar::Util" => "0";
requires "Text::Glob" => "0";
requires "Try::Tiny" => "0";
requires "perl" => "5.010";
requires "re" => "0";
requires "strict" => "0";
requires "warnings" => "0";
requires "warnings::register" => "0";

on 'test' => sub {
  requires "Exporter" => "0";
  requires "ExtUtils::MakeMaker" => "0";
  requires "File::Spec::Functions" => "0";
  requires "File::Temp" => "0";
  requires "File::pushd" => "0";
  requires "Path::Tiny" => "0";
  requires "Test::Deep" => "0";
  requires "Test::Filename" => "0.03";
  requires "Test::More" => "0.92";
  requires "lib" => "0";
  requires "version" => "0";
};

on 'test' => sub {
  recommends "CPAN::Meta" => "0";
  recommends "CPAN::Meta::Requirements" => "2.120900";
};

on 'configure' => sub {
  requires "ExtUtils::MakeMaker" => "6.17";
};

on 'develop' => sub {
  requires "Dist::Zilla" => "5.012";
  requires "Dist::Zilla::Plugin::AppendExternalData" => "0";
  requires "Dist::Zilla::PluginBundle::DAGOLDEN" => "0.053";
  requires "File::Spec" => "0";
  requires "File::Temp" => "0";
  requires "IO::Handle" => "0";
  requires "IPC::Open3" => "0";
  requires "Test::CPAN::Meta" => "0";
  requires "Test::More" => "0";
  requires "Test::Pod" => "1.41";
};
