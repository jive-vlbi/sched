C$Procedure      FNDLUN ( Find a free logical unit )
 
      SUBROUTINE FNDLUN ( UNIT )
 
C$ Abstract
C
C     Return the number of a free logical unit, if one is available.
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
C     FILES
C
C$ Declarations
 
      INTEGER          UNIT
 
      INTEGER          MINLUN
      PARAMETER      ( MINLUN =  1 )
 
      INTEGER          MAXLUN
      PARAMETER      ( MAXLUN = 99 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       O   The number of a free logical unit.
C     MINLUN     P   Minimum logical unit number.
C     MAXLUN     P   Maximum logical unit number.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     UNIT        is the number of a free logical unit (also called
C                 an "external unit").  A "free" logical unit is one
C                 that is not reserved and is not currently connected to
C                 and open file. If no free units are available, the
C                 value of UNIT is 0.
C
C$ Parameters
C
C     MINLUN      is the minimum logical unit number. The Fortran
C                 standard states that unit numbers must be zero or
C                 positive.  However, the value 0 is reserved as a
C                 status code for this routine, so MINLUN must be
C                 assigned a value greater than 0.
C
C     MAXLUN      is the maximum logical unit number allowed by the
C                 VAX Fortran compiler. This may differ for other
C                 machines.
C
C     Listed below are the values for several machines:
C
C     Environment: VAX/VMS, VAX FORTRAN
C     MINLUN:      1
C     MAXLUN:      99
C
C     Environment: Sun, Sun FORTRAN
C     MINLUN:      1
C     MAXLUN:      63
C
C     Environment: PC, MS FORTRAN *
C     MINLUN:      1
C     MAXLUN:      99
C
C     Environment: PC/Linux, Fort77
C     MINLUN:      1
C     MAXLUN:      99
C
C     Environment: Macintosh, Language Systems FORTRAN
C     MINLUN:      1
C     MAXLUN:      99
C
C     Environment: PC, Lahey F77 EM/32 Version 4.0
C     MINLUN:      1
C     MAXLUN:      99
C
C     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers
C     MINLUN:      1
C     MAXLUN:      61
C
C     Environment: Silicon Graphics, SGI f77
C     MINLUN:      1
C     MAXLUN:      63
C
C     Environment: DEC Alpha OSF/1, DEC FORTRAN
C     MINLUN:      1
C     MAXLUN:      99
C
C     Environment: NeXT, Absoft Fortran
C     MINLUN:      1
C     MAXLUN:      99
C
C     * 32767 is the actual value a logical unit may be assigned to
C       on the IBM PC, however, using this value increases the memory
C       requirements of a program calling this routine by 128K.
C
C$ Exceptions
C
C         Error free.
C
C
C     1)  If no logical units are available, UNIT is set equal
C         to 0.
C
C     2)  This routine performs a Fortran INQUIRE operation.  If
C         the INQUIRE fails, UNIT is set equal to the negative
C         of the INQUIRE iostat ( UNIT will thus have a negative
C         value).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     FNDLUN returns the number of the first (unreserved) unit not
C     currently connected to a file.  It thus frees the user from
C     having to maintain an accounting of which units are open, which
C     are closed, and which are available.
C
C     This routine is related to the routines GETLUN, RESLUN, and
C     FRELUN.  Together, these routines support coordinated usage of
C     Fortran logical units.  FNDLUN (Find a free logical unit) and
C     GETLUN (Get a free logical unit) both have the function of
C     returning a logical unit number that is not reserved or already
C     in use.  The principal difference between the functionality of
C     these routines is that GETLUN both returns a status code and
C     signals an error if a free unit is not found, while FNDLUN
C     merely returns a status code.
C
C     RESLUN is used to reserve logical unit numbers, so that they will
C     not be returned by GETLUN or FNDLUN; FRELUN frees logical units
C     previously reserved via calls to RESLUN.
C
C     On the VAX, SUN, PC, and HP logical units 5-7 are reserved by
C     default.  On the Macintosh logical units 5,6 and 9 are reserved
C     by default.  Other units may be reserved by calling RESLUN. Once
C     reserved, units (except ones reserved by default) may be
C     unreserved by calling FRELUN.
C
C     To reserve logical unit numbers for special use, refer to
C     RESLUN. To make reserved units available to FNDLUN and GETLUN,
C     refer to FRELUN.
C
C     A unit returned by FNDLUN does NOT automatically become a
C     reserved unit.  If the user wishes to reserve a unit found by
C     FNDLUN, the call to FNDLUN must be followed by a call to RESLUN.
C
C     Note that although 0 is a valid logical unit number on some
C     systems, a value of 0 returned by FNDLUN indicates that no free
C     logical unit was available, rather than that logical unit 0 is
C     available.  Similarly, negative values returned by FNDLUN are
C     status codes, not logical unit numbers.
C
C$ Examples
C
C     The following code fragment illustrates the use of FNDLUN.
C
C        CALL FNDLUN ( UNIT )
C
C        IF ( UNIT .LT. 0 ) THEN
C           RETURN
C        END IF
C
C$ Restrictions
C
C     This routine never returns logical unit numbers that are less
C     than or equal to 0.
C
C$ Literature_References
C
C     1.  "Programming in VAX FORTRAN", Digital Equipment Corporation,
C         September 1984, Section 11.1.1, page 11-2.
C
C     2.  "Microsoft FORTRAN Reference", Microsoft Corporation
C         1989, Section 3.2.2, page 61.
C
C     3.  "Sun FORTRAN Programmer's Guide", Sun Microsystems,
C         Revision A of 6 May 1988, Section 7.2, page 73.
C
C     4.  "Language Systems FORTRAN Reference Manual", Version 2.1,
C         page 193.
C
C     5.  "Lahey F77L EM/32 Programmers Reference Manual", version 4.0,
C         page 94.
C
C     6.  "FORTRAN/9000 Reference HP 9000 Series 700 Computers",
C         First Edition, June 1991, Hewlett Packard Company, pages 6-2
C         and 6-4.
C
C     7.  Silicon Graphics "Fortran 77 Programmer's Guide",
C         Document number 007-0711-030, page 1-20.
C
C     8.  "Language Reference Manual", Absoft Fortran V3.2, 1993,
C         page 7-4, section 7.3.1 (for the NeXT).
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     M.J. Spencer    (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.26.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 6.25.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 6.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 6.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 6.21.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 6.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 6.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 6.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 6.17.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 6.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 6.14.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 6.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 6.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 6.11.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 6.10.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 6.9.0, 16-MAR-2009 (BVS)
C
C        Changed MAXLUN from 99 to 61 for HP and HP_C environments. The
C        value 61 was determined by trial-n-error while preparing a
C        special HP toolkit delivery for GSFC in July 2008.
C
C-    SPICELIB Version 6.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 6.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 6.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 6.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 6.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 6.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 6.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 6.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 6.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 6.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 6.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 6.0.0, 05-APR-1998 (NJB)
C
C        References to the PC-LINUX environment were added.
C
C-     SPICELIB Version 5.0.0, 9-NOV-1993 (HAN)
C
C         Module was updated to include the logical unit values
C         for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms.
C
C-     SPICELIB Version 4.0.0, 6-OCT-1992 (HAN)
C
C         Module was updated to include the logical unit values for
C         the Hewlett Packard UX 9000/750 environment.
C
C-     SPICELIB Version 3.0.0, 20-MAR-1992 (MJS)
C
C         IOSTAT check now placed directly after the INQUIRE
C         statement.
C
C-     SPICELIB Version 2.2.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 2.2.0, 13-NOV-1991 (MJS)
C
C         Module was updated to include the value of MAXLUN
C         for the Lahey F77L EM/32 environment (PC).
C
C-     SPICELIB Version 2.1.0, 15-MAY-1991 (MJS)
C
C         Module was updated to allow portability to the Macintosh
C         environment.
C
C-     SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
C
C         The array RESNUM now contains the default reserved
C         logical units. All the elements of the array RESVD
C         were initialized.  The value of MAXLUN for the IBM PC
C         was changed from 32767 to 99.  Some header comments
C         were clarified.
C
C-     SPICELIB Version 1.0.1, 20-MAR-1990 (HAN)
C
C         Parameters section was updated to include the values
C         of MINLUN and MAXLUN for several machines. Sources of
C         these values are listed in the Literature References
C         section.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     find a free logical unit
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 6.0.0, 05-APR-1998 (NJB)
C
C        References to the PC-LINUX environment were added.
C
C-     SPICELIB Version 5.0.0, 9-NOV-1993 (HAN)
C
C         Module was updated to include the logical unit values
C         for the Silicon Graphics, DEC Alpha-OSF/1, and
C         NeXT platforms.
C
C         The values used for the DEC Alpha worked in all of the
C         porting tests, but NAIF has no documentation for this
C         platform.
C
C-     SPICELIB Version 1.4.0, 6-OCT-1992 (HAN)
C
C         Module was updated to include the logical unit values for
C         the Hewlett Packard UX 9000/750 environment.
C
C         The code was also reformatted so that a utility program can
C         create the source file for a specific environment given a
C         master source file.
C
C-     SPICELIB Version 3.0.0, 20-MAR-1992 (MJS)
C
C         IOSTAT check now placed directly after the INQUIRE
C         statement. Previously, IOSTAT could have been checked
C         without first being assigned a value.
C
C-     SPICELIB Version 2.1.0, 15-MAY-1991 (MJS)
C
C         Module was updated to allow portability to the Macintosh
C         environment. Literature References section was updated.
C         Some header comments were clarified.
C
C-     SPICELIB Version 2.0.0, 26-MAR-1991 (MJS) (NJB)
C
C         The default reserved logical units are now declared in the
C         array RESNUM. All the elements of the array RESVD were
C         initialized. These two changes allow FNDLUN to be ported
C         to other platforms more easily. The value of MAXLUN for the
C         IBM PC was decreased from 32767 to 99.
C
C         Some cosmetic changes to variable declarations were made.
C         Also, some header comments were added to make the header's
C         discussion clearer.
C
C-     Beta Version 1.1.0, 09-MAR-1989  (HAN)
C
C         Declaration of the variable RETURN was removed from the code.
C         The variable was declared, but not used.
C
C-&
 
 
C
C     Parameters
C
      INTEGER               NRESV
      PARAMETER           ( NRESV = 3 )
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               LAST
      INTEGER               RESNUM  ( NRESV )
 
      LOGICAL               FIRST
      LOGICAL               OPENED
      LOGICAL               RESVD   ( MINLUN : MAXLUN )
 
 
C
C     Save everything between calls.
C
      SAVE
 
C
C     Initial values
C
      DATA             LAST      / MINLUN /
      DATA             FIRST     / .TRUE. /
 
 
C
C     VAX, SUN, PC, HP, SGI, DEC Alpha-OSF/1, and PC/Lunix
C     reserved units.
C
      DATA             RESNUM    / 5, 6, 7 /
 
 
C
C     Initialize RESVD if it hasn't already been done.
C
      IF ( FIRST ) THEN
 
         DO I = MINLUN, MAXLUN
            RESVD ( I ) = .FALSE.
         END DO
 
         DO I = 1, NRESV
            RESVD ( RESNUM(I) ) = .TRUE.
         END DO
 
         FIRST = .FALSE.
 
      END IF
 
C
C     Begin with the unit following the last one returned.
C     Cycle through the available units. Skip reserved units,
C     INQUIRE about others.
C
      DO I = LAST+1, MAXLUN
 
         IF ( RESVD(I) ) THEN
            OPENED = .TRUE.
         ELSE
            INQUIRE ( UNIT=I, OPENED=OPENED, IOSTAT=IOSTAT )
 
            IF ( IOSTAT .GT. 0 ) THEN
               UNIT = -IOSTAT
               RETURN
            END IF
 
         END IF
 
         IF ( .NOT. OPENED ) THEN
            UNIT = I
            LAST = UNIT
            RETURN
         END IF
 
      END DO
 
C
C     If we've come this far, we need to search the first part of
C     the list again, up to the last unit returned. Once again,
C     skip reserved units, INQUIRE about others.
C
      DO I = MINLUN, LAST
 
         IF ( RESVD(I) ) THEN
            OPENED = .TRUE.
         ELSE
            INQUIRE ( UNIT=I, OPENED=OPENED, IOSTAT=IOSTAT )
 
            IF ( IOSTAT .GT. 0 ) THEN
               UNIT = -IOSTAT
               RETURN
            END IF
 
         END IF
 
         IF ( .NOT. OPENED ) THEN
            UNIT = I
            LAST = UNIT
            RETURN
         END IF
 
      END DO
 
C
C     If we've come this far, there are no free units to be had.
C     C'est la vie. Assign 0 to the unit number.
C
 
      UNIT = 0
 
      RETURN
 
 
 
 
C$Procedure RESLUN ( Reserve a logical unit )
 
      ENTRY RESLUN ( UNIT )
 
C$ Abstract
C
C     Reserve a logical unit number. Reserved units are never returned
C     by FNDLUN or GETLUN.
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
C     FILES
C
C$ Declarations
C
C     INTEGER               UNIT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Number of the logical unit to be reserved.
C
C$ Detailed_Input
C
C     UNIT      is the number of the logical unit to be reserved.
C               Once reserved, the unit number will not be returned
C               by the routines FNDLUN or GETLUN, even if it is not
C               connected to a file.
C
C               On the VAX, SUN, PC, and HP logical units 5-7 are
C               reserved by default.  On the Macintosh logical units
C               5,6 and 9 are reserved by default.
C
C$ Detailed_Output
C
C     None.
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
C     FNDLUN maintains an array of logical flags, one for each positive
C     unit number offered by the system. RESLUN sets the value of the
C     flag for UNIT to TRUE.
C
C     Once reserved, units (except units reserved by default) may be
C     unreserved by calling FRELUN.
C
C$ Examples
C
C     The following code fragment illustrates the use of RESLUN.
C
C           C
C           C     Units 17-23 are used by non-NAIF file readers.
C           C     Reserve these, so that they will not be returned
C           C     by FNDLUN or GETLUN.
C           C
C                 DO I = 17, 23
C                    CALL RESLUN ( I )
C                 END DO
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     See the module FNDLUN.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     C.A. Curzon     (JPL)
C     H.A. Neilan     (JPL)
C     M.J  Spencer    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 6.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 6.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-     SPICELIB Version 2.0.0, 16-MAR-1992 (MJS)
C
C         RESVD is now initialized on entry to this routine if
C         it hasn't been done previously.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (IMU)
C
C-&
 
C$ Index_Entries
C
C     reserve a logical unit
C
C-&
 
 
C$ Revisions
C
C-     SPICELIB Version 2.0.0, 16-MAR-1992 (MJS)
C
C         RESVD is now initialized on entry to this routine if
C         it hasn't been done previously. Prior to this fix, any actions
C         taken by RESLUN or FRELUN before FNDLUN was called would have
C         been discarded. FIRST is now checked on entry to all entry
C         points.
C
C-     Beta Version 1.1.0, 27-FEB-1989 (HAN) (NJB)
C
C         This routine is now an entry point of FNDLUN rather than
C         GETLUN.  The code of this entry point itself has not changed
C         however. References to the routine FNDLUN were added to the
C         header.  The restrictions section was updated to read "none."
C         This module was declared "error free", which means
C         that it will never participate in error handling.
C
C-&
 
 
C
C     Initialize RESVD if it hasn't already been done.
C
      IF ( FIRST ) THEN
 
         DO I = MINLUN, MAXLUN
            RESVD ( I ) = .FALSE.
         END DO
 
         DO I = 1, NRESV
            RESVD ( RESNUM(I) ) = .TRUE.
         END DO
 
         FIRST = .FALSE.
 
      END IF
 
C
C     If UNIT is in the proper range, set the corresponding flag
C     to TRUE.
C
      IF ( UNIT .GE. MINLUN .AND. UNIT .LE. MAXLUN ) THEN
         RESVD(UNIT) = .TRUE.
      END IF
 
      RETURN
 
 
 
 
C$Procedure FRELUN ( Free a reserved logical unit )
 
      ENTRY FRELUN ( UNIT )
 
C$ Abstract
C
C     Unreserve a logical unit number reserved by RESLUN.
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
C     FILES
C
C$ Declarations
C
C     INTEGER               UNIT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Number of the logical unit to be unreserved.
C
C$ Detailed_Input
C
C     UNIT      is the number of the logical unit to be unreserved.
C               Once unreserved, the unit number may be returned by
C               the routines GETLUN or FNDLUN whenever not connected to
C               a file.
C
C               On the VAX, SUN, PC, and HP logical units 5-7 are
C               reserved by default.  On the Macintosh logical units
C               5,6 and 9 are reserved by default. These may not be
C               unreserved.
C
C$ Detailed_Output
C
C     None.
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
C     FNDLUN maintains an array of logical flags, one for each unit
C     offered by the system. FRELUN sets the value of the flag for
C     UNIT to FALSE.
C
C$ Examples
C
C     The following code fragment illustrates the use of FRELUN.
C
C           C
C           C     Free the units used by the non-NAIF file readers,
C           C     so that they may be returned by FNDLUN or GETLUN.
C           C
C                 DO I = 17, 23
C                    CALL FRELUN ( I )
C                 END DO
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     See the module FNDLUN.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     C.A. Curzon     (JPL)
C     H.A. Neilan     (JPL)
C     M.J. Spencer    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.0.4, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 6.0.3, 24-SEP-1999 (NJB)
C
C        CSPICE environments were added.  Some typos were corrected.
C
C-    SPICELIB Version 6.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 6.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-     SPICELIB Version 2.0.0, 16-MAR-1992 (MJS)
C
C         RESVD is now initialized on entry to this routine if
C         it hasn't been done previously.
C
C-     SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.1.0, 12-MAR-1991 (MJS)
C
C         The array RESNUM now contains the default reserved
C         logical units. All the elements of the array RESVD
C         were initialized.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (IMU)
C
C-&
 
C$ Index_Entries
C
C     free a reserved logical unit
C
C-&
 
 
C$ Revisions
C
C-     SPICELIB Version 2.0.0, 16-MAR-1992 (MJS)
C
C         RESVD is now initialized on entry to this routine if
C         it hasn't been done previously. Prior to this fix, any actions
C         taken by RESLUN or FRELUN before FNDLUN was called would have
C         been discarded. FIRST is now checked on entry to all entry
C         points.
C
C-     Beta Version 1.1.0, 27-FEB-1989 (HAN) (NJB)
C
C         This routine is now an entry point of FNDLUN rather than
C         GETLUN.  The code of this entry point itself has not changed
C         however. References to the routine FNDLUN were added to the
C         header.  The restrictions section was updated to read "none."
C         This module was declared "error free", which means
C         that it will never participate in error handling.
C
C-&
 
 
C
C     Initialize RESVD if it hasn't already been done.
C
      IF ( FIRST ) THEN
 
         DO I = MINLUN, MAXLUN
            RESVD ( I ) = .FALSE.
         END DO
 
         DO I = 1, NRESV
            RESVD ( RESNUM(I) ) = .TRUE.
         END DO
 
         FIRST = .FALSE.
 
      END IF
 
C
C     If UNIT is in the proper range and it has not been reserved by
C     default, set the corresponding flag to FALSE.
C
      IF ( UNIT .GE. MINLUN .AND. UNIT .LE. MAXLUN ) THEN
 
         DO I = 1, NRESV
            IF ( UNIT .EQ. RESNUM(I) ) THEN
               RETURN
            END IF
         END DO
 
         RESVD ( UNIT ) = .FALSE.
 
      END IF
 
      RETURN
 
      END
