C$Procedure ZZCPUTIM ( CPU Time )
 
      SUBROUTINE ZZCPUTIM ( TVEC )
 
C$ Abstract
C
C     Fetch the current CPU date and time and store the result
C     as a double precision 6-vector.
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
C     TIME
C     UTILITY
C
C$ Declarations
 
      DOUBLE PRECISION      TVEC ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TVEC       O   contains year, month, day, hours, minutes, seconds
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     TVEC       is a 6-vector containing the current system time.
C                The various components have the following meaning
C
C                   TVEC(1)  --- current calendar year
C                   TVEC(2)  --- current month
C                   TVEC(3)  --- current day of month
C                   TVEC(4)  --- current hour. Hours have a range from
C                                0 to 23.  0 corresponds to system
C                                midnight.
C                   TVEC(5)  --- current minutes
C                   TVEC(6)  --- current seconds and fraction of a
C                                second (provided the system clock
C                                has sufficiently fine granularity
C                                to provide greater precision).
C
C                The first 5 components will be double precision
C                integers.  (They truncate without change.)
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the components of the current date and
C     time as determined by the system clock.
C
C
C$ Examples
C
C     Example 1.
C
C     The following routine illustrates how you might use ZZCPUTIM
C     to generate a "time stamp" that might be used to tag data
C     you plan to write to a file.
C
C           SUBROUTINE TSTAMP ( STAMP )
C
C           CHARACTER*(15)        STAMP
C
C           DOUBLE PRECISION      TVEC ( 6 )
C
C     C
C     C     First fetch the current system time.
C     C
C           CALL ZZCPUTIM ( TVEC )
C
C     C
C     C     now form a time stamp of the form YYYYYMMDDhhmmss
C     C
C           CALL DPFMT ( TVEC(1), '0YYYY', STAMP(1:5)   )
C           CALL DPFMT ( TVEC(2), '0M',    STAMP(6:7)   )
C           CALL DPFMT ( TVEC(3), '0D',    STAMP(8:9)   )
C           CALL DPFMT ( TVEC(4), '0h',    STAMP(10:11) )
C           CALL DPFMT ( TVEC(5), '0m',    STAMP(12:13) )
C           CALL DPFMT ( TVEC(6), '0s',    STAMP(14:15) )
C
C           RETURN
C
C     Example 2.
C
C     The following code illustrates how you might use this routine
C     to perform a crude estimate of the running time of the
C     SPICELIB routine VDOT. (This assumes that the program runs
C     during a single calendar day and that there is no competition
C     between users for system resources.)
C
C           DOUBLE PRECISION      VDOT
C
C           DOUBLE PRECISION      AVE
C           DOUBLE PRECISION      SINCE0( 2    )
C           DOUBLE PRECISION      TVEC  ( 6, 3 )
C           DOUBLE PRECISION      V1    ( 3    )
C           DOUBLE PRECISION      V2    ( 3    )
C           DOUBLE PRECISION      X
C
C           INTEGER               I
C           INTEGER               TRIALS
C           PARAMETER           ( TRIALS = 100000 )
C
C     C
C     C     Give the vectors some values (these seem as good as
C     C     anything else that comes to mind).
C     C
C           V1(1) = 1.0D0
C           V1(2) = 2.0D0
C           V1(3) = 3.0D0
C
C           V2(1) = 10.0D0
C           V2(2) = 20.0D0
C           V3(3) = 30.0D0
C
C     C
C     C     Perform the loop twice, once with one call to VDOT, the
C     C     second with two calls to VDOT.
C     C     The first will require
C     C
C     C        LOOP_OVERHEAD + TRIALS*TIME_FOR_VDOT
C     C
C     C     The second will require
C     C
C     C        LOOP_OVERHEAD + 2*TRIALS*TIME_FOR_VDOT
C     C
C     C     The difference of the two, will give us
C     C
C     C        TRIALS*TIME_FOR_VDOT
C     C
C
C     C
C     C     get the current system time.
C     C
C           CALL ZZCPUTIM ( TVEC(1,1) )
C
C           DO I = 1, TRIALS
C              X = VDOT( V1, V2 )
C           END DO
C
C     C
C     C     Get the time after the first pass.
C     C
C           CALL ZZCPUTIM ( TVEC(1,2)
C
C           DO I = 1, TRIALS
C              X = VDOT( V1, V2 )
C              X = VDOT( V1, V2 )
C           END DO
C
C      C
C      C    Get the time after the second pass.
C      C
C           CALL ZZCPUTIM ( TVEC(1,3)
C
C
C      C
C      C    Now compute seconds past midnight at each clock reading.
C      C
C           DO I = 1, 3
C
C              SINCE0(I) = TVEC(4,I) * 3600.0D0
C          .             + TVEC(5,I) *   60.0D0
C          .             + TVEC(6,I)
C           END DO
C
C      C
C      C    The time for the first  pass is SINCE0(2) - SINCE0(1)
C      C    The time for the second pass is SINCE0(3) - SINCE0(2)
C      C
C      C    The difference between these divided by the number of
C      C    trials is the average running time.
C      C
C           AVE = (SINCE0(3) - 2*SINCE0(2) - SINCE0(1)) / DBLE(TRIALS)
C
C           WRITE (*,*) 'The average running time for VDOT is ', AVE
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1) "Absoft FORTRAN 77 Compatibility Libraries", 1992,
C        Chapter 2, Chapter 3, pages 3-14 and 3-16.
C
C     2) "Language Systems FORTRAN 3.0 Reference Manual", October 1991
C        Chapter 12, pages 214 and 218.
C
C     3) Other UNIX boxes, we used the commands "man idate" and
C        "man itime".
C
C     4) "Programming VAX FORTRAN", "Fortran Language Summary",
C        Appendix D, September 1984, D-39 and D-41.
C
C     5) For the SGI, we use a small C language program to obtain the
C        date and time. This avoids ambiguities with the two digit year
C        returned by idate and avoids a name collision with ltime, the
C        SPICELIB light time subroutine. The C language program is in
C        the file loctim.c and is given the symbol loctim_ as its name.
C
C     6) "LF 95 Language Reference", Lahey Computer Systems Inc.
C
C     7) "The Intel Fortran Reference, Document Number: 253261-003"
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     K.R. Gehringer (JPL)
C     H.A. Neilan    (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    Beta Version 7.13.0, 02-APR-2014 (BVS)
C
C        Changed PC-CYGWIN-GFORTRAN and PC-CYGWIN-64BIT-GFORTRAN
C        to be like other GFORTRAN environments.
C
C-    Beta Version 7.12.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    Beta Version 7.11.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    Beta Version 7.10.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    Beta Version 7.9.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    Beta Version 7.8.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    Beta Version 7.7.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    Beta Version 7.6.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    Beta Version 7.5.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    Beta Version 7.4.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    Beta Version 7.3.0, 03-DEC-2008 (EDW)
C
C        Updated for PC-LINUX-GFORTRAN MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version  7.2.1, 07-SEP-2007 (EDW)
C
C        Renamed CPUTIM to ZZCPUTIM; moved routine into
C        SPICELIB from SUPPORT.
C
C-    SPICELIB Version  7.2.0, 26-NOV-2006 (EDW)
C
C        Added support for ifort compilers, OS X and Windows.
C
C-    SPICELIB Version  7.1.0, 10-NOV-2005 (BVS)
C
C        Fixed duplicate arguments in RMAIND calls.
C
C-    SPICELIB Version  7.0.5, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version  7.0.4, 27-DEC-2001 (EDW) (FST)
C
C        MACPPC environment updated to reflect the use of Absoft
C        compiler. It now uses the GetTime call to return
C        a DateTimeRec record (structure). The CPU time returns as
C        components of the DateTimeRec record.
C
C        PC-LAHEY environment updated to reflect the use of the new
C        LF95 compiler.
C
C-    SPICELIB Version  7.0.3, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version  7.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version  7.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version  7.0.0, 10-JUN-1998 (WLT)
C
C        Both the MAC and VAX versions included code to deal
C        with 2-digit years.  This code was replaced with a
C        call to TEXPYR so that two-digit years are handled
C        uniformly.
C
C        Also when the year 2068 rolls around, future NAIF
C        programmers can simply modify TEXPYR to change the default
C        century and code will continue to work.
C
C-    SPICELIB Version  6.0.0, 05-APR-1998 (NJB)
C
C        Added support for the PC-Linux environment.  The extant
C        SGI code sufficed.
C
C        Added SAVE statement for Mac/PPC environment.  Removed
C        non-printing characters.  Shortened one long line.
C
C-    SPICELIB Version  5.0.0, 16-FEB-1996 (KRG)
C
C        Generally just cleaned things up and rearranged the code for
C        each environment so that it was contiguous, to make it easier
C        to add new environments if necessary.
C
C        Removed VMS library dependencies from the NeXT code.
C
C        Updated and simplified the SGI code. THis now uses a small C
C        language program to obtain the system time and date.
C
C-    SPICELIB Version  4.0.0, 28-SEP-1995 (HAN)
C
C       Added EXTERNAL declarations required for the DEC-OSF1
C       platform.
C
C-    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 3.0.0, 16-MAY-1995 (HAN)
C
C       Added the NeXT environment to the master source file. The
C       Absoft compiler requires system functions to be in lower case
C       and they must have a postpended underscore.
C
C       Added the Silicon Graphics and Macintosh environments to the
C       master source file. The DEC Alpha running OSF/1 is the same
C       as the Sun and HP.
C
C-    SPICELIB Version 2.0.0, 7-APR-1994 (HAN)
C
C       Created a master source file and added information about the
C       HP-UX/HP Fortran environment.
C
C-    SPICELIB Version 1.0.0, 1-MAY-1991 (NJB) (HAN) (MJS) (WLT)
C
C-&
 
C$ Index_Entries
C
C     get system date and time
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
 
C
C     Local variables
C
      INTEGER               DTIME (8)
      CHARACTER*(12)        RCLOCK(3)
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZCPUTIM' )
      END IF
 
      CALL DATE_AND_TIME( RCLOCK(1), RCLOCK(2), RCLOCK(3), DTIME )
C
C     Let's pack all this information into our double precision TVEC
C     array.
C
      TVEC ( 1 ) = DBLE( DTIME(1) )
      TVEC ( 2 ) = DBLE( DTIME(2) )
      TVEC ( 3 ) = DBLE( DTIME(3) )
      TVEC ( 4 ) = DBLE( DTIME(5) )
      TVEC ( 5 ) = DBLE( DTIME(6) )
      TVEC ( 6 ) = DBLE( DTIME(7) ) + DBLE( DTIME(8) )/1000.D0
 
C
C     That's it.
C
      CALL CHKOUT ( 'ZZCPUTIM' )
      RETURN
 
      END
