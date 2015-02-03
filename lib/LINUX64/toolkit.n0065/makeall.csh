#! /bin/csh
\echo This script builds the SPICE delivery
\echo for the toolkit package of the toolkit.
\echo
\echo The script must be executed from the
\echo toolkit directory.
\echo
cd src
\echo
\echo Creating spicelib
\echo
cd spicelib
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating support
\echo
cd support
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating brief
\echo
cd brief
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating chronos
\echo
cd chronos
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating ckbrief
\echo
cd ckbrief
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating commnt
\echo
cd commnt
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating cookbook
\echo
cd cookbook
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating frmdiff
\echo
cd frmdiff
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating inspekt
\echo
cd inspekt
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating mkspk
\echo
cd mkspk
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating msopck
\echo
cd msopck
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating spacit
\echo
cd spacit
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating spkdiff
\echo
cd spkdiff
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating spkmerge
\echo
cd spkmerge
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating tobin
\echo
cd tobin
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating toxfr
\echo
cd toxfr
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
\echo
\echo Creating version
\echo
cd version
chmod u+x mkprodct.csh; ./mkprodct.csh
cd ..
cd ..
\echo Toolkit Build Complete
