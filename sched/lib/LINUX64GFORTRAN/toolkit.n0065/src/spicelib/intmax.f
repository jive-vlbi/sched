C$Procedure            INTMAX ( Largest integer number )
 
      INTEGER FUNCTION INTMAX ()
 
C$ Abstract
C
C     Return the value of the largest (positive) number representable
C     in an integer variable.
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
C
C     None.
C
C$ Brief_I/O
C
C     The function returns the value of the largest (positive) number
C     that can be represented in an integer variable.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     The function returns the value of the largest (positive) number
C     that can be represented in an integer variable.
C
C     This varies from machine to machine. Values for several popular
C     machines are shown below.
C
C     Environment: VAX/VMS, VAX FORTRAN
C     Value:       2147483647
C
C     Environment: Sun, Sun FORTRAN
C     Value:       INMAX()
C
C     Environment: PC, MS (Microsoft) FORTRAN
C     Value:       2147483647
C
C     Environment: Macintosh, Language Systems FORTRAN
C     Value:       2147483647
C
C     Environment: PC/Linux, Fort77
C     Value:       2147483647
C
C     Environment: PC, Lahey F77 EM/32 Version 4.0
C     Value:       2147483647
C
C     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
C     Value:       2147483647
C
C     Environment: Silicon Graphics, IRIX OS, SGI FORTRAN 77
C     Value:       2147483647
C
C     Environment: DEC Alpha, OSF/1, DEC FORTRAN-77
C     Value:       2147483647
C
C     Environment: NeXT, Mach OS, Absoft Fortran 77
C     Value:       2147483647
C
C     References for the values above are listed in the
C     Literature_References section of the header.
C
C     PC/Linux Fort77 Note:
C
C      Value was validated by experiment.  The value is identical to
C      that for the PC/MS Fortran and HP-UX 9000/750, FORTRAN/9000
C      platforms.
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
C$ Examples
C
C     The following code fragment illustrates the use of INTMAX.
C
C        C
C        C     If the integer component is out of range, avoid overflow
C        C     by making it as large as possible.
C        C
C              IF ( DVALUE .GT. DBLE ( INTMAX() ) ) THEN
C                 IVALUE = INTMAX()
C
C              ELSE IF ( DVALUE .LT. DBLE ( INTMIN() ) THEN
C                 IVALUE = INTMIN()
C
C              ELSE
C                 IVALUE = INT ( DVALUE )
C              END IF
C
C              FRACT = MOD ( DVALUE, 1.D0 )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1. "Programming in VAX FORTRAN", Digital Equipment Corporation,
C        September 1984, Appendix C, FORTRAN Data Representation,
C        page C-2.
C
C     2. "Microsoft FORTRAN Reference", Microsoft Corporation
C        1989, Section 1.3.1, page 10.
C
C     3. "Sun FORTRAN Programmer's Guide, Sun Microsystems,
C        Revision A of 6 May 1988, Appendix F, Manual Pages for FORTRAN,
C        page 306 (RANGE).
C
C     5. "Language Systems FORTRAN Reference Manual", Language Systems
C        Corporation, version 1.2.1, page 3-2.
C
C     6. "Lahey F77L EM/32 Programmers Reference Manual", version 4.0,
C        page 95.
C
C     7. "FORTRAN/9000 Reference HP 9000 Series 700 Computers",
C        First Edition, June 1991, Hewlett Packard Company, page 4-4.
C
C     8.  "SGI Fortran 77 Programmer's Guide", Document number
C         007-0711-030, page 2-2.
C
C     9.  "Language Reference Manual", Absoft Fortran V3.2, 1993,
C         page 3-14, section 3.6.1.5. (for the NeXT)
C
C     10. "Unix/VMS Compatibility Libraries", Absoft Fortran V3.2, 1993;
C         Chapter 3, Support Libraries, page 3-14, inmax. (for the NeXT)
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     M.J. Spencer    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.16.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 2.15.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 2.14.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 2.13.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 2.10.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.9.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 2.8.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 2.7.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 2.6.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 2.5.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 2.3.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 2.1.0, 30-JUN-2002 (EDW)
C
C        Added environments for Apple OS X G77 (MAC-OSX-G77),
C        and Absoft F77 (MAC-OSX-F77)
C
C-     SPICELIB Version 2.0.3, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-     SPICELIB Version 2.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-     SPICELIB Version 2.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-     SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C         Added references to the PC-LINUX environment.
C
C-     SPICELIB Version 1.5.0, 3-NOV-1993 (HAN)
C
C         Module was updated to include the function value
C         for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms.
C
C-     SPICELIB Version 1.4.0, 9-OCT-1992 (HAN)
C
C         Module was updated to include the value of INTMAX for the
C         Hewlett Packard UX 9000/750 environment.
C
C-     SPICELIB Version 1.3.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
C
C         Module was updated to include the value of INTMAX
C         for the Lahey F77L EM/32 environment (PC).
C
C-     SPICELIB Version 1.2.0, 7-DEC-1990 (MJS)
C
C         Module was updated to include the value of INTMAX for
C         the Macintosh.
C
C-     SPICELIB Version 1.1.0, 12-MAR-1990 (HAN)
C
C         Module was changed to include the function value for
C         the Sun. References were added to specify the sources
C         of the function values on different machines.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     largest integer number
C
C-&
 
 
C$ Revisions
C
C-     SPICELIB Version 2.0.0, 05-APR-1998 (NJB)
C
C         Added references to the PC-LINUX environment.
C
C-     SPICELIB Version 1.5.0, 3-NOV-1993 (HAN)
C
C         Module was updated to include the function value
C         for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms.
C
C-     SPICELIB Version 1.4.0, 9-OCT-1992 (HAN)
C
C         Module was updated to include the value of INTMAX for the
C         Hewlett Packard UX 9000/750 environment.
C
C         The code was also reformatted so that a utility program can
C         create the source file for a specific environment given a
C         master source file.
C
C-     SPICELIB Version 1.3.0, 13-NOV-1991 (MJS)
C
C         Module was updated to include the value of INTMAX
C         for the Lahey F77L EM/32 environment (PC).
C
C-     SPICELIB Version 1.2.0, 7-DEC-1990 (MJS)
C
C         Module was updated to include the value of INTMAX for
C         the Macintosh. Literature_References section was
C         updated.
C
C-     SPICELIB Version 1.1.0, 12-MAR-1990 (HAN)
C
C         Code was changed to include the function value
C         for the Sun. Documentation in the Particulars
C         section was changed to include the value, and the
C         example was updated and corrected.
C
C         All sources for the values contained in this module
C         are now specified in the Literature_References section.
C
C-     Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB)
C
C         Contents of the Exceptions section was changed
C         to "error free" to reflect the decision that the
C         module will never participate in error handling.
C
C         Parentheses added to INTMAX declaration.
C-&
 
 
 
C
C     Numbers are provided in a variety of formats: decimal, hex,
C     and binary. These last two formats are not portable; but then,
C     neither are the values.
C
 
C
C     VAX/VMS,          VAX FORTRAN
C     IBM PC,           Microsoft FORTRAN
C     PC,               Lahey F77L EM/32 FORTRAN
C     Macintosh,        Language Systems FORTRAN
C     HP-UX 9000/750,   FORTRAN/9000 Series 700 computers
C     Silicon Graphics, Silicon Graphics Fortran
C     DEC Alpha-OSF/1,  DEC Fortran
C     NeXT Workstation, Absoft Fortran
C     PC/Linux,         Fort77
C     Apple OS X G77,   Absoft OS X F77
C
      INTMAX = 2147483647
 
      RETURN
      END
