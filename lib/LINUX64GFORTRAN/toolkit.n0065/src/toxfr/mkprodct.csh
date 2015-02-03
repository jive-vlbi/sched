#! /bin/csh
#
#   This script is a more or less generic library/executable
#   builder for SPICE products.  It assumes that it is executed
#   from one of the "product" directories in a tree that looks like
#   the one displayed below:
#
#                      package
#                         |
#                         |
#       +------+------+------+------+------+
#       |      |      |      |      |      | 
#     data    doc    etc    exe    lib    src
#                                          |
#                                          |
#                         +----------+----------+------- ... ------+
#                         |          |          |                  |
#                     product_1  product_2  product_3    ...   product_n
#
#   Here's the basic strategy:
#
#     1)  Compile all of the .f files in the current directory
#
#     2)  If there are no .pgm files in the current directory this
#         is assumed to be a library source directory.  The name
#         of the library is the same as the name of the product.
#         The library is placed in the "lib" directory in the tree
#         above.  The script is then done.
# 
#         If there are .pgm files and there were some .f
#         files compiled the objects are gathered together in the
#         current directory into a library called locallib.a
#
#     3)  If any *.pgm files exist in the current directory, compile 
#         them and link their objects with locallib.a (if it exists), 
#         ../../spicelib.a and ../../csupport.a. The output 
#         executables have an empty extension.  The executables are
#         placed in the "exe" directory in the tree above.
#         
#   The environment variables F_COMPILEOPTIONS and C_COMPILEOPTIONS 
#   containing Fortran and C compile options are optionally set. If they
#   are set prior to executing this script,  those options are used. If
#   they are not set externally, they are set within this script as #    
#   local variables.
#
#   References:
#   ===========
#
#   "Unix Power Tools", page 11.02 
#      Use the "\" character to unalias a command temporarily.
#        
#   "A Practical Guide to the Unix System"
#
#   "The Unix C Shell Field Guide"
#
#   Change History:
#   ===============
#
#   Version 4.1.0  Aug 20, 2002 Boris Semenov
#
#      Updated to use only gfortran. Removed escape character from \setenv
#      \echo as needed. Modified to return non-zero status if failed.
#
#   Version 4.0.1  Jan 29, 2002 Boris Semenov
#
#      Moved "\rm locallib.a" out of the *.pgm loop to allow 
#      correct compilation of multiple .pgm's.     
#
#   Version 4.0.0  Nov 01, 1999 Bill Taber
#
#      Modified the script so that the FORTRAN compiler is selected
#      from choices gfortran and fort77.  gfortran is given preference.
#
#   Version 3.0.0  Apr 05, 1998  Nat Bachman
#
#      Modified to handle both Fortran and C code, in order to support
#      the PC-LINUX environment.
#
#   Version 2.0.0  Jan 27, 1998  Nat Bachman
#
#      Modified to handle C code.  HP Version.
#
#   Version 1.0.0  Dec  8, 1995  Bill Taber
#

#
#   Use gfortran compiler.
#

which gfortran >& /dev/null

if ( $status == 0 ) then
   setenv F_COMPILER gfortran
   echo " "
   echo "      Using the gfortran compiler."
else
   echo " "
   echo "      The FORTRAN compiler gfortran is not in the current path:"
   echo " "
   echo $PATH
   echo " "
   echo "      As a result, I am unable to build the toolkit."
   echo " "
   exit 1;
endif

set C_COMPILER  =  "gcc"

#
#  What compile options do we want to use? If they were 
#  set somewhere else, use those values.  The same goes
#  for link options.
#
if ( $?F_COMPILEOPTIONS ) then
   echo " "
   echo "      Using Fortran compile options: "
   echo "      $F_COMPILEOPTIONS"
else
   set F_COMPILEOPTIONS = "-m64 -c"
   echo " "
   echo "      Setting default Fortran compile options:"
   echo "      $F_COMPILEOPTIONS"
endif

if ( $?C_COMPILEOPTIONS ) then
   echo " "
   echo "      Using C compile options: "
   echo "      $C_COMPILEOPTIONS"
else
   set C_COMPILEOPTIONS = "-m64 -c"
   echo " "
   echo "      Setting default C compile options:"
   echo "      $C_COMPILEOPTIONS"
endif

if ( $?TKLINKOPTIONS ) then
   echo " "
   echo "      Using link options: "
   echo "      $TKLINKOPTIONS"
else
   set TKLINKOPTIONS = "-static -m64  "
   echo " "
   echo "      Setting default link options:"
   echo "      $TKLINKOPTIONS"
endif

echo " "

#
# Keep track of the number of times something goes wrong.
#
@ FAILURES = 0

#
#   Determine a provisional LIBRARY name.
#
foreach item ( `pwd` )
   set LIBRARY = "../../lib/"$item:t
end

#
#  Are there any *.f files that need to be compiled?
#

\ls *.f >& /dev/null

if ( $status == 0 ) then

   foreach SRCFILE ( *.f )
      echo "      Compiling: "   $SRCFILE
      $F_COMPILER $F_COMPILEOPTIONS $SRCFILE
      if ( ( $status != 0 ) || ( ! -e $SRCFILE:r.o ) ) then
         @ FAILURES++
      endif   
   end

endif

#
#  Are there any *.c files that need to be compiled?
#

\ls *.c >& /dev/null

if ( $status == 0 ) then

   foreach SRCFILE ( *.c )
      echo "      Compiling: "   $SRCFILE
      $C_COMPILER $C_COMPILEOPTIONS $SRCFILE
      if ( ( $status != 0 ) || ( ! -e $SRCFILE:r.o ) ) then
         @ FAILURES++
      endif   
   end

endif


echo " "

#
#  If object files exist, we need to create an object library.
#

\ls *.pgm >& /dev/null

if ( $status == 0 ) then
   set LIBRARY = "locallib"
endif

\ls *.o >& /dev/null

if ( $status == 0 ) then

   echo "      Inserting objects in the library $LIBRARY ..."
   ar  crv $LIBRARY.a *.o
   ranlib  $LIBRARY.a
   \rm                *.o    
   echo " "

endif

#
#  If there are any main programs in the directory, compile
#  them. If they have their own locallib.a link with it in addition
#  to the default libraries.
#

\ls *.pgm >& /dev/null

if ( $status == 0 ) then

   echo " "

   foreach MAIN ( *.pgm )
      set TARGET  = $MAIN:r.f
      set MAINOBJ = $MAIN:r.o
      set EXECUT = ../../exe/$MAIN:r
      \cp $MAIN  $TARGET
   
      echo "      Compiling and linking: " $MAIN
      
      if ( -e locallib.a ) then

         $F_COMPILER    $F_COMPILEOPTIONS $TARGET
         $F_COMPILER -o $EXECUT           $MAINOBJ             \
                                          locallib.a           \
                                          ../../lib/support.a  \
                                          ../../lib/spicelib.a \
                                          $TKLINKOPTIONS    

         if ( ( $status != 0 ) || ( ! -e $EXECUT ) ) then
            @ FAILURES++
         endif

         \rm $TARGET
         \rm $MAINOBJ

      else

         echo "Compiling and linking: " $MAIN     
         $F_COMPILER    $F_COMPILEOPTIONS $TARGET
         $F_COMPILER -o $EXECUT           $MAINOBJ             \
                                          ../../lib/support.a  \
                                          ../../lib/spicelib.a \
                                          $TKLINKOPTIONS
 
         if ( ( $status != 0 ) || ( ! -e $EXECUT ) ) then
            @ FAILURES++
         endif

         \rm $TARGET
         \rm $MAINOBJ

      endif

   end

endif

#
#  Cleanup.
#

echo " "

if ( -e locallib.a ) then
   \rm locallib.a
endif

\ls *.o >& /dev/null

if ( $status == 0 ) then
   \rm *.o
endif

exit $FAILURES

