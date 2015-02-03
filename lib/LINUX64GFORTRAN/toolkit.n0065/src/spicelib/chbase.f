C$Procedure CHBASE ( Character set base )
 
      INTEGER FUNCTION CHBASE ()
 
      IMPLICIT NONE
 
C$ Abstract
C
C     Return the base value used to encode unsigned integer values
C     in character strings.
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
C     CONSTANTS
C
C$ Declarations
 
C     None.
 
C$ Brief_I/O
C
C     The function returns the base value used to encode unsigned
C     integer values in character strings.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     CHBASE is the base used by ENCHAR and DECHAR to encode and decode
C     non-negative integers to and from character strings. Its value is
C     determined by the size of the character set available for a given
C     machine and compiler. Strictly speaking, CHBASE is one more than
C     the biggest positive integer which can be handled by both the
C     CHAR and ICHAR intrinsic functions (which are used by ENCHAR and
C     DECHAR). That is, CHBASE is the first positive integer for which
C     the logical expression
C
C           ( ICHAR ( CHAR ( CHBASE ) ) .EQ. CHBASE )
C
C     is false.
C
C     Note that CHBASE can be (and probably is) different from the
C     number of characters in the character set used by the processor.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The function always returns a constant value, set by the user
C     prior to compilation.
C
C     CHBASE should always be at least 128 (the size of the ASCII
C     character set), and will usually be 256 for machines which use
C     eight bits to represent a single character. The following list
C     contains the values of CHBASE for a range of environments.
C
C     Environment: VAX/VMS, VAX FORTRAN
C     Value:       256
C
C     Environment: Sun, Sun FORTRAN
C     Value:       256
C
C     Environment: PC, MS FORTRAN
C     Value:       256
C
C     Environment: Macintosh, Language Systems FORTRAN
C     Value:       256
C
C     Environment: PC, Lahey F77 EM/32 Version 4.0
C     Value:       256
C
C     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
C     Value:       256
C
C     Environment: Silicon Graphics IRIX OS, SGI FORTRAN 77
C     Value:       256
C
C     Environment: DEC Alpha 3000/4000, OSF/1, DEC FORTRAN-77
C     Value:       256
C
C     Environment: NeXT/Mach OS, Absoft Fortran
C     Value:       256
C
C     Environment: PC/Linux, Fort77
C     Value:       128
C
C
C     For other machines, the value can be determined by running
C     the following simple program:
C
C            INTEGER          CHBASE
C            DATA             CHBASE    / 0 /
C
C            DO WHILE ( .TRUE. )
C
C               IF ( ICHAR (CHAR ( CHBASE ) ) .EQ. CHBASE ) THEN
C                  CHBASE = CHBASE + 1
C               ELSE
C                  WRITE (6,*) 'CHBASE for this machine is : ', CHBASE
C                  STOP
C               END IF
C
C            END DO
C            END
C
C$ Examples
C
C     See ENCHAR, DECHAR.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1. "Programming in VAX FORTRAN", Digital Equipment Corporation,
C        September 1984, Section 8.3, page 8-6.
C
C     2. "Microsoft FORTRAN Reference", Microsoft Corporation,
C        1989, Section 5.1.1, page 241.
C
C     3. "Language Systems FORTRAN Reference Manual", Language Systems
C        Corporation, version 1.2.1, page 3-20.
C
C     4. "Lahey F77L EM/32 FORTRAN Language Reference Manual", page
C        222, Note 20.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     M.J. Spencer    (JPL)
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.26.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 2.21.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.11.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 2.9.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 2.8.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 2.4.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 2.3.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 2.1.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 2.1.0, 05-DEC-2001 (FST)
C
C        Updated the value for PC-LINUX environment.
C
C-    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 2.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
C
C       The environment lines were expanded so that the supported
C       environments are now explicitely given.  New
C       environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
C
C       The environment lines were expanded so that the supported
C       environments are now explicitely given.  Previously,
C       environments such as SUN-SUNOS and SUN-SOLARIS were implied
C       by the environment label SUN.
C
C-    SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C        Added reference to the PC-LINUX environment.
C
C-    SPICELIB Version 1.5.0, 03-NOV-1993 (HAN)
C
C        Module was updated to include the character base
C        value for the Silicon Graphics, DEC Alpha-OSF/1, and
C        NeXT platforms.
C
C-    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN)
C
C        Module was updated to include the character base
C        value for the Hewlett Packard UX 9000/750 environment,
C        and the value for the Sun was changed from 128 to 256.
C        Both changes are the result of running the program in
C        the Particulars section of the header on both machines.
C
C-    SPICELIB Version 1.3.1, 10-MAR-1999 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
C
C        Module was updated to include the character base
C        value for the Lahey FORTRAN EM/32 environment (PC).
C
C-    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS)
C
C        Module was updated to include the character base
C        value for the Macintosh.
C
C-    SPICELIB Version 1.1.0, 09-MAR-1990 (HAN)
C
C        Module was updated to include the character base
C        value for the Sun. Sources for the values contained
C        in this module are now specified in the Literature_References
C        section.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     base for encoding integers in character_string
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 05-DEC-2001 (FST)
C
C        It was discovered that linux distributions shipping
C        versions of g77 derived off of gcc versions 2.96-3.00
C        suffer from in implementation change in ICHAR that
C        requires CHBASE to change to 128.  Since restricting
C        CHBASE to 128 has little impact on other linux
C        environments utilizing other versions of g77 or fort77,
C        we elected to make the change to all environments
C        rather than complicate this issue by forking a new one.
C
C-    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN)
C
C        Module was updated to include the character base
C        value for the Hewlett Packard UX 9000/750 environment,
C        and the value for the Sun was changed from 128 to 256.
C        Both changes are the result of running the program in
C        the Particulars section of the header on both machines.
C
C        The previous Sun value was computed on the Sun3 and was
C        not updated when we moved to the Sun4. Everything passed
C        the suite of test programs that would have indicated a bug.
C
C        The code was also reformatted so that a utility program can
C        create the file for each environment.
C
C-    Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
C
C        Contents of the Exceptions section was changed
C        to "error free" to reflect the decision that the
C        module will never participate in error handling.
C
C        Missing parentheses added to CHBASE declaration.
C
C-&
 
C
C     We have provided values for several popular machines. Remove
C     the comment character in front of the value for your machine,
C     or provide your own value. Numbers are provided in a variety
C     of formats: decimal, hex, and binary. These last two formats
C     are not portable; but then, neither are the values.
C
C
C     PC Linux, g77 or fort77
C
      CHBASE = 128
 
      RETURN
      END
