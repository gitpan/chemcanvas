use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Tk::ChemCanvas',
    AUTHOR              => 'Dean Brockhausen <dean.brockhausen@gmail.com>',
    VERSION_FROM        => 'lib/Tk/ChemCanvas.pm',
    ABSTRACT_FROM       => 'lib/Tk/ChemCanvas.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Tk-ChemCanvas-*' },
);
