C$Procedure ZZDDHIVF ( Private --- DDH Identify VAX DAF File Format )
 
      SUBROUTINE ZZDDHIVF ( NSUM, BFF, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Identify VAX DAF file format.
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      CHARACTER*(*)         NSUM
      INTEGER               BFF
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NSUM       I   String storing the 8 bytes of the FDREC NSUM DP.
C     BFF        O   Integer code indicating the binary file format.
C     FOUND      O   Logical indicating that BFF was determined.
C
C$ Detailed_Input
C
C     NSUM       is a string whose first 8 bytes contain NSUM (the third
C                double precision number) from the first descriptor
C                record of a non-empty DAF suspected to be in one of
C                the VAX binary file formats.
C
C$ Detailed_Output
C
C     BFF        is an integer that signals whether NSUM indicates the
C                DAF is VAX-DFLT or VAX-GFLT.  Possible values are:
C
C                   VAXGFL
C                   VAXDFL
C
C                as defined in the include file 'zzddhman.inc'.  See it
C                for details.
C
C     FOUND      is a logical that indicates whether the ZZDDHVFF check
C                was successful.  If TRUE, BFF contains the code for
C                VAX-DFLT or VAX-GFLT binary file format.  If FALSE,
C                then BFF is untouched.
C
C$ Parameters
C
C     See the include file 'zzddhman.inc'.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     This routine examines a series of bytes from a potential pre-N0050
C     DAF to determine its architecture, but does not access the file
C     itself.
C
C$ Particulars
C
C     This routine examines the bit patterns stored in NSUM to determine
C     which of the two VAX binary file formats are used to store the
C     double precision values in the DAF file.
C
C$ Examples
C
C     See ZZDDHPPF for sample usage.
C
C$ Restrictions
C
C     1) The first 8 bytes of NSUM must contain the third double
C        precision value from the first descriptor record in a DAF
C        file not in BIG-IEEE binary file format.
C
C     2) The DAF file from which NSUM is extracted must be correct
C        or this routine may produce incorrect results.
C
C     3) Assumes CHARACTER*(1) is byte sized.
C
C     4) Assumes that ICHAR(CHAR(CHARACTER)) yields an integer with
C        the same bit pattern as the source character.
C
C$ Literature_References
C
C     1) Binary File Formats and Code Relying on Function Not Addressed
C        by the ANSI '77 Fortran Standard.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 1.0.0, 04-OCT-2001 (FST)
C
C
C-&
 
C
C     Local Variables
C
      CHARACTER*(1)         CARG
 
      INTEGER               LEADER
      INTEGER               TRAILR
 
C
C     Statement Functions
C
      INTEGER               ZZICHR
 
C
C     Statement Function Definitions
C
C     This function controls the conversion of characters to integers.
C     Some versions of the g77 implement ICHAR with a signed integer.
C     This function computes the value of ICHAR that this code requires
C     on any version of g77 for x86 Linux.
C
      ZZICHR(CARG) = ICHAR(CARG) - MAX( -1, MIN(0,ICHAR(CARG)) )*256
 
C
C     Before diving right into the code that examines the bit patterns
C     stored in NSUM, review exactly what checks require completion and
C     why they function.
C
C     When this module is invoked, we already know that the DAF from
C     which NSUM was extracted is little endian, and that it is not
C     a LTL-IEEE file.  This leaves us with one of 3 sources for
C     NSUM:
C
C       (a) A VAX D-Floating file
C       (b) A VAX G-Floating file
C       (c) A damaged file
C
C     In the case of (c) the algorithm outlined below is not guarenteed
C     to produce correct results.  If the case is either (a) or (b),
C     then the routine will correctly determine the source binary file
C     format.  Here's why:
C
C        NSUM is the third double precision number from the first
C        descriptor record of a non-empty DAF file.  This number is
C        an integral valued DP bounded between 1 and 125 inclusive.
C
C        An examination of a binary file created with the following
C        code fragment:
C
C           INCLUDE              'zzddhman.inc'
C
C           DOUBLE PRECISION      DPDATA ( 125 )
C           INTEGER               I
C           INTEGER               LUN
C             .
C             .
C             .
C           CALL GETLUN( LUN )
C
C           DO I = 1, 125
C              DPDATA (I) = DBLE (I)
C           END DO
C
C           OPEN ( UNIT   = LUN,
C          .       FILE   = FNAME,
C          .       STATUS = 'NEW',
C          .       ACCESS = 'DIRECT',
C          .       RECL   = RECL      )
C
C           WRITE ( UNIT = LUN, REC = 1 ) ( DPDATA(I), I = 1, 125 )
C
C           END
C
C        This source file was compiled on a VMS VAX system both with
C        G-Floating and D-Floating options, and executed to produce
C        the binary file of interest.  The bit patterns for each of
C        the 125 entries were compared using the UNIX command 'od'.
C
C        This comparison yielded the fact that these two sets of 125
C        bit patterns did not intersect, and all that remained was to
C        uncover an efficient means of identifying which set a
C        particular member belonged to.
C
C        The following was observed:
C
C           With the exception of the first entry representing the
C           number 1.0D0 in the D-Floating case, all entries
C           appeared as: (hexadecimal byte dump from 'od' output)
C
C              0041 0000 0000 0000
C              4041 0000 0000 0000
C              8041 0000 0000 0000
C                       .
C                       .
C                       .
C              f643 0000 0000 0000
C              f843 0000 0000 0000
C              fa43 0000 0000 0000
C
C           While the G-Floating case:
C
C              1040 0000 0000 0000
C              2040 0000 0000 0000
C              2840 0000 0000 0000
C                       .
C                       .
C                       .
C              7e40 00c0 0000 0000
C              7f40 0000 0000 0000
C              7f40 0040 0000 0000
C
C           The important thing to note is that the fourth entry in
C           G-Floating bit patterns is always '0', and in the
C           D-Floating case (with the exception of the first entry)
C           is always non-zero.  The first entry in the D-Floating
C           table is:
C
C              8040 0000 0000 0000
C
C           It also happens to be the case that the leading value
C           of all G-Floating cases are numbers less than 8.
C           Constructing a series of tests around these observations
C           will produce correct results.  When the input file meets
C           the restrictions non-empty and correct.
C
C     So now all that remains is to lay out the specifics of the test.
C     First extract the leading 4 bits from NSUM(1:1) and the trailing
C     four bits from NSUM(2:2).  Then enter this IF/ELSE IF block:
C
C        If the value of the leading 4 bits from NSUM(1:1) is 8 and
C        the trailing 4 bits from NSUM(2:2) are 0, then the file is
C        of the D-Floating binary format.
C
C        Else if the value of the trailing 4 bits of NSUM(2:2) is
C        non-zero, then the file is also of the D-Floating binary
C        format.
C
C        Else if the value of the leading 4 bits of NSUM(1:1) is
C        strictly less than 8 and the trailing bits of NSUM(2:2)
C        are 0, then the file is of the G-Floating binary format.
C
C        Else the file is not of VAX type.
C
C     This routine could be reimplemented to examine all 8 bytes of
C     each double precision number and compare it to two tables of
C     values.  In the interest of simplicity the preceding option
C     was selected.
C
C
C
C     Convert the first and second characters in NSUM to integers.
C
      LEADER = ZZICHR( NSUM(1:1) )
      TRAILR = ZZICHR( NSUM(2:2) )
 
C
C     Shift the trailing 4 bits off LEADER.
C
      LEADER = LEADER / 16
 
C
C     Subtract the leading bits off TRAILR.
C
      TRAILR = TRAILR - ( 16 * (TRAILR / 16) )
 
C
C     Now determine what file we are looking at.
C
      IF ( ( LEADER .EQ. 8 ) .AND. ( TRAILR .EQ. 0 ) ) THEN
 
         FOUND = .TRUE.
         BFF   = VAXDFL
 
      ELSE IF ( TRAILR .NE. 0 ) THEN
 
         FOUND = .TRUE.
         BFF   = VAXDFL
 
      ELSE IF ( ( LEADER .LT. 8 ) .AND. ( TRAILR .EQ. 0 ) ) THEN
 
         FOUND = .TRUE.
         BFF   = VAXGFL
 
      ELSE
 
         FOUND = .FALSE.
 
      END IF
 
      RETURN
      END
