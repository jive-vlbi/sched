C$Procedure ZZPLATFM ( Private --- Get platform attributes )
 
      SUBROUTINE ZZPLATFM ( KEY, VALUE )
 
C$ Abstract
C
C     Return platform ID and various attributes of the intended
C     environment
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         KEY
      CHARACTER*(*)         VALUE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     KEY        I   String indicating what information to return.
C     VALUE      O   String containing the requested information.
C
C$ Detailed_Input
C
C     KEY         is a string value that indicates which platform
C                 specific information is desired.  Acceptable inputs
C                 are:
C
C                    'SYSTEM'      -  System Identification String
C                    'O/S'         -  Operating System or Environment
C                    'COMPILER'    -  NAIF Supported Compiler
C                    'FILE_FORMAT' -  Native Binary File Format
C                    'TEXT_FORMAT' -  Native Text File Format
C                    'READS_BFF'   -  List of supported binary file
C                                     formats.
C
C                 Note: The comparison is case-insensitive, and the
C                       supplied value must fit into a string buffer
C                       KYSIZE characters in length.
C
C$ Detailed_Output
C
C     VALUE       is the string that holds the information requested
C                 by the input string KEY. VALUE must be able to
C                 contain the maximum number of characters returned
C                 by this routine, WDSIZE, or truncation will occur.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the KEY is invalid, then VALUE is set to the value
C        stored in the character string parameter DEFRPY defined
C        below.
C
C     2) If VALUE is not large enough to contain the requested
C        KEY's value, then truncation will occur.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves to identify the platform and compiler
C     used in creating SPICELIB.  It is provided so that routines
C     and programs can make run-time decisions based upon the
C     ambient Fortran environment.
C
C     Operating Systems:
C
C        This routine is now aware of the operating systems for which
C        the code is intended for compilation.  In some cases this may
C        be more than one operating system, particularly in the case
C        of the PC.
C
C     Binary File Format:
C
C        This routine now adds the capability to return at run time
C        the binary file architecture that is native to the system.
C
C     Text File Format:
C
C        This routine now has the capability to return at run time
C        the mechanism (or line terminator) used to delimit lines
C        in text files.  In most cases it will return common labels
C        for the special characters FORTRAN considers line break
C        indicators.
C
C     Binary File Formats Read:
C
C        This returns a space delimited list of all the binary file
C        formats this environment can read for DAF/DAS based files.
C
C$ Examples
C
C     This routine could be used so that a single routine
C     could be written that translates the meaning of IOSTAT values
C     that depend upon the machine and compiler.  At run time
C     the routine could look up the appropriate message to associate
C     with an IOSTAT value.
C
C$ Restrictions
C
C     1) VALUE must be large enough to contain the requested
C        information, otherwise truncation will occur.
C
C     2) The string passed in via the KEY input must be capable
C        of being properly copied into the KEYCPY buffer internal
C        to this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 10-MAR-2014 (BVS)
C
C        Updated for:
C
C           PC-CYGWIN-64BIT-GCC_C
C           PC-CYGWIN-64BIT-GFORTRAN
C           PC-CYGWIN-GFORTRAN
C           PC-LINUX-64BIT-IFORT
C           SUN-SOLARIS-64BIT-INTEL
C
C        environments.
C
C-    SPICELIB Version 2.9.0, 16-MAR-2010 (EDW)
C
C        Updated for:
C
C        -  MAC-OSX-64BIT-INTEL_C
C        -  PC-64BIT-MS_C
C        -  SUN-SOLARIS-64BIT-NATIVE_C
C           MAC-OSX-64BIT-GFORTRAN
C           MAC-OSX-64BIT-IFORT
C           PC-LINUX-64BIT-GFORTRAN
C           PC-WINDOWS-64BIT-IFORT
C           SUN-SOLARIS-INTEL-64BIT-CC_C
C           SUN-SOLARIS-INTEL-CC_C
C           SUN-SOLARIS-INTEL
C
C        environments.
C
C-    SPICELIB Version 2.8.0, 12-JAN-2009 (EDW)
C
C        Added MAC-OSX-GFORTRAN PC-LINUX-GFORTRAN environments.
C
C-    SPICELIB Version 2.7.0, 19-FEB-2008 (BVS)
C
C        Added PC-LINUX-IFORT environment.
C
C-    SPICELIB Version 2.6.0, 15-NOV-2006 (NJB)
C
C        Added PC-WINDOWS-IFORT, MAC-OSX-IFORT, and MAC-OSX-INTEL_C
C        environments.
C
C-    SPICELIB Version 2.5.0, 21-FEB-2006 (NJB)
C
C        Added PC-LINUX-64BIT-GCC_C environment.
C
C        Corrected error in in-line comments:  changed keyword
C        from FILE_ARCH to FILE_FORMAT.
C
C-    SPICELIB Version 2.4.0, 14-MAR-2005 (BVS)
C
C        Added SUN-SOLARIS-64BIT-GCC_C environment.
C
C-    SPICELIB Version 2.3.0, 31-DEC-2004 (BVS)
C
C        Added PC CYGWIN environments. Changed OS for PC-LAHEY,
C        PC-DIGITAL, and PC-MS_C to 'MICROSOFT WINDOWS'.
C
C-    SPICELIB Version 2.2.0, 07-JUL-2002 (EDW)
C
C        Added Mac OS X Unix environment.
C
C-    SPICELIB Version 2.1.0, 06-FEB-2002 (FST)
C
C        Updated the 'TEXT_FORMAT' key value for the PC-LINUX_C
C        environment.  Previous versions incorrectly indicated
C        'CR-LF' as line terminators.
C
C-    SPICELIB Version 2.0.0, 05-JUN-2001 (FST)
C
C        Added TEXT_FORMAT and READS_BFF key/value pairs.
C        Modified the header slightly to improve word choice;
C        specifically binary file format replaces file
C        architecture.
C
C        Updated the compiler entry for the PC-LINUX
C        environment to refer to g77 as opposed to f2c.
C
C        Updated the compiler entry for the MACPPC environment.
C        This environment is now officially tied to Absoft
C        Fortran.
C
C        Updated the compiler entry for the PC-LAHEY environment.
C        The compiler for this environment is LF95, the latest
C        offering from Lahey.
C
C-    SPICELIB Version 1.0.0, 22-FEB-1999 (FST)
C
C-&
 
 
C$ Index_Entries
C
C     fetch platform dependent information
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
 
C
C     Local Parameters
C
 
C
C     Array index parameters for each of the key/value pairs.
C
C     SYSTEM Index.
C
      INTEGER               IDXSYS
      PARAMETER           ( IDXSYS = 1 )
 
C
C     O/S Index.
C
      INTEGER               IDXOS
      PARAMETER           ( IDXOS  = IDXSYS + 1 )
 
C
C     Compiler Index.
C
      INTEGER               IDXCMP
      PARAMETER           ( IDXCMP = IDXOS  + 1 )
 
C
C     Binary File Format Index.
C
      INTEGER               IDXFMT
      PARAMETER           ( IDXFMT = IDXCMP + 1 )
 
C
C     Text File Format Index
C
      INTEGER               IDXTFM
      PARAMETER           ( IDXTFM = IDXFMT + 1 )
 
C
C     Reads Binary File Format Index.
C
      INTEGER               IDXRBF
      PARAMETER           ( IDXRBF = IDXTFM + 1 )
 
C
C     Size of the buffer in which KEY is placed.
C
      INTEGER               KYSIZE
      PARAMETER           ( KYSIZE = 64 )
 
C
C     Maximum Size of local string returned in VALUE
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
C
C     Number of Platform Dependent values stored here.
C
      INTEGER               NATTR
      PARAMETER           ( NATTR  =  6 )
 
C
C     Default Reply in the event of an invalid KEY.
C
      CHARACTER*(WDSIZE)     DEFRPY
      PARAMETER            ( DEFRPY = '<UNAVAILABLE>' )
 
C
C     Local Variables
C
 
      CHARACTER*(KYSIZE)    KEYCPY
      CHARACTER*(KYSIZE)    KEYVAL ( NATTR )
      CHARACTER*(WDSIZE)    ATTCPY ( 0 : NATTR )
 
      INTEGER               INDEX
 
      LOGICAL               FIRST
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  ATTCPY
      SAVE                  KEYVAL
 
C
C     Data Statements
C
      DATA   FIRST        /.TRUE./
 
C
C     Make the initial assignments to the saved character array.
C
      IF ( FIRST ) THEN
C
C        Store the keys in the KEYVAL array.
C
         KEYVAL (IDXSYS) = 'SYSTEM'
         KEYVAL (IDXOS)  = 'O/S'
         KEYVAL (IDXCMP) = 'COMPILER'
         KEYVAL (IDXFMT) = 'FILE_FORMAT'
         KEYVAL (IDXTFM) = 'TEXT_FORMAT'
         KEYVAL (IDXRBF) = 'READS_BFF'
 
C
C        Set the default reply to be the zero'th component of ATTCPY.
C        This obviates IF-THEN-ELSE branching all together.
C
         ATTCPY (0) = DEFRPY
 
C
C        Platform/Environment specific assignments follow.
C
 
 
 
 
 
         ATTCPY (IDXSYS) = 'PC'
 
 
 
 
         ATTCPY (IDXOS) = 'LINUX'
 
 
 
         ATTCPY (IDXCMP) = 'GFORTRAN/64BIT'
 
 
 
         ATTCPY (IDXFMT) = 'LTL-IEEE'
 
 
 
         ATTCPY (IDXTFM) = 'LF'
 
 
 
         ATTCPY (IDXRBF) = 'BIG-IEEE LTL-IEEE'
 
C
C        Don't execute these assignments again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Determine which KEY was passed in; do this by converting KEY
C     to the known member of the equivalence class of possible
C     values.
C
      CALL UCASE ( KEY,    KEYCPY )
      CALL LJUST ( KEYCPY, KEYCPY )
 
C
C     Find out which key we were given.  In the event that one of the
C     KEYVALs (or some equivalent string) was not passed in, ISRCHC
C     returns a value of zero.
C
      INDEX = ISRCHC ( KEYCPY, NATTR, KEYVAL )
      VALUE = ATTCPY ( INDEX )
 
      RETURN
      END
