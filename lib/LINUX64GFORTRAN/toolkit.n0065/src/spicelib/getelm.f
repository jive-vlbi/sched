C$Procedure GETELM ( Get the components from two-line elements)
 
      SUBROUTINE GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
 
C$ Abstract
C
C    Given a the "lines" of a two-line element set, parse the
C    lines and return the elements in units suitable for use
C    in SPICE software.
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
C     PARSING
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               FRSTYR
      CHARACTER*(*)         LINES ( 2 )
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      ELEMS ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRSTYR     I   year of earliest representable two-line elements
C     LINES      I   a pair of "lines" containing two-line elements
C     EPOCH      O   The epoch of the elements in seconds past J2000
C     ELEMS      O   The elements converted to SPICE units.
C
C$ Detailed_Input
C
C     FRSTYR    is the first year possible for two line elements.
C               Since two line elements allow only two digits for
C               the year, some conventions must be followed concerning
C               which century the two digits refer to .  FRSTYR
C               is the year of the earliest representable elements.
C               The two-digit year is mapped to the year in
C               the interval from FRSTYR to FRSTYR + 99 that
C               has the same last two digits as the two digit
C               year in the element set.  For example if FRSTYR
C               is set to 1960  then the two digit years are mapped
C               as shown in the table below:
C
C               Two-line         Maps to
C               element year
C                  00            2000
C                  01            2001
C                  02            2002
C                   .              .
C                   .              .
C                   .              .
C                  58            2058
C                  59            2059
C                 --------------------
C                  60            1960
C                  61            1961
C                  62            1962
C                   .              .
C                   .              .
C                   .              .
C                  99            1999
C
C                Note that if Space Command should decide to represent
C                years in 21st century as 100 + the last two digits
C                of the year (for example: 2015 is represented as 115)
C                instead of simply dropping the first two digits of
C                the year, this routine will correctly map the year
C                as long as you set FRSTYR to some value between 1900
C                and 1999.
C
C     LINES      is a pair of lines of text that comprise a Space
C                command ``two-line element'' set.  These text lines
C                should be the same as they are presented in the
C                two-line element files available from Space Command
C                (formerly NORAD). Below is an example of a two-line
C                set for TOPEX.
C
C  TOPEX
C  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
C  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
C
C
C$ Detailed_Output
C
C     EPOCH      is the epoch of the two line elements supplied via
C                the input array LINES.  Epoch is returned in TDB
C                seconds past J2000.
C
C     ELEMS      is an array containing the elements from the two line
C                set supplied via the array LINES.  The elements are
C                in units suitable for use by the SPICE routine
C                EV2LIN.
C
C                Also note that the elements XNDD6O and BSTAR
C                incorporate the exponential factor present in the
C                input two line elements in LINES.  (See particulars
C                below.
C
C                    ELEMS (  1 ) = XNDT2O in radians/minute**2
C                    ELEMS (  2 ) = XNDD6O in radians/minute**3
C                    ELEMS (  3 ) = BSTAR
C                    ELEMS (  4 ) = XINCL  in radians
C                    ELEMS (  5 ) = XNODEO in radians
C                    ELEMS (  6 ) = EO
C                    ELEMS (  7 ) = OMEGAO in radians
C                    ELEMS (  8 ) = XMO    in radians
C                    ELEMS (  9 ) = XNO    in radians/minute
C                    ELEMS ( 10 ) = EPOCH of the elements in seconds
C                                   past ephemeris epoch J2000.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     You must have loaded a SPICE leapseconds kernel into the
C     kernel pool prior to caling this routine.
C
C$ Exceptions
C
C     1) If an error occurs while trying to parse the two-line element
C        set, the error 'SPICE(BADTLE)' signals.
C
C$ Particulars
C
C     This routine passes a Space Command Two-line element set
C     to the parsing routine ZZGETELM.  Input elements have the
C     form:
C
C  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
C  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
C  ^
C  123456789012345678901234567890123456789012345678901234567890123456789
C           1         2         3         4         5         6
C
C$ Examples
C
C     Suppose you have a set of two-line elements and an array
C     containing the related geophysical constants necessary
C     to evaluate a state.  The example below shows how you
C     can use this routine together with the routine EV2LIN to
C     propagate a state to an epoch of interest.
C
C
C        The parameters below will make it easier to make assignments
C        to the array GEOPHS required by EV2LIN.
C
C        J2  --- location of J2
C        J3  --- location of J3
C        J4  --- location if J4
C        KE  --- location of KE = sqrt(GM) in eart-radii**1.5/MIN
C        QO  --- location of upper bound of atmospheric model in KM
C        SO  --- location of lower bound of atmospheric model in KM
C        ER  --- location of earth equatorial radius in KM.
C        AE  --- location of distance units/earth radius
C
C        PARAMETER           ( J2 = 1 )
C        PARAMETER           ( J3 = 2 )
C        PARAMETER           ( J4 = 3 )
C        PARAMETER           ( KE = 4 )
C        PARAMETER           ( QO = 5 )
C        PARAMETER           ( SO = 6 )
C        PARAMETER           ( ER = 7 )
C        PARAMETER           ( AE = 8 )
C
C
C        We set the lower bound for the years to be the beginning
C        of the space age.
C
C        FRSTYR = 1957
C
C        Read in the next two lines from the text file that contains
C        the two-line elements.  We assume that file has been opened
C        properly and that we have set the ``file pointer'' to the
C        correct location for reading the next set of elements.
C
C        READ  (UNIT,FMT='(A)' ) LINE(1)
C        READ  (UNIT,FMT='(A)' ) LINE(2)
C
C        CALL GETELM ( FRSTYR, LINE, EPOCH, ELEMS )
C
C        Set up the geophysical quantities.  At last check these
C        were the values used by Space Command.
C
C        GEOPHS( J2 ) =    1.082616D-3
C        GEOPHS( J3 ) =   -2.53881D-6
C        GEOPHS( J4 ) =   -1.65597D-6
C        GEOPHS( KE ) =    7.43669161D-2
C        GEOPHS( QO ) =  120.0D0
C        GEOPHS( SO ) =   78.0D0
C        GEOPHS( ER ) = 6378.135D0
C        GEOPHS( AE ) =    1.0D0
C
C        Now propagate the state using EV2LIN to the epoch of
C        interest.
C
C        CALL EV2LIN ( ET, GEOPHS, ELEMS, STATE )
C
C
C$ Restrictions
C
C    Please refer to the header of ZZGETELM.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 3.0.0, 30-MAR-2004 (EDW)
C
C        Routine now passes inputs to ZZGETELM then reponds to
C        any error condition.
C
C-    SPICELIB Version 2.0.0, 03-MAR-2000 (WLT)
C
C        The routine was modified to check that all of the terms
C        in the two-line element set are parsed correctly.
C
C-    SPICELIB Version 1.0.0, 26-JUN-1997 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Parse two-line elements
C
C-&

C
C     Spicelib functions
C
      LOGICAL               RETURN

C
C     Local.
C
      LOGICAL               OK
      CHARACTER*(256)       ERROR

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'GETELM' )

C
C     Pass the input to the parse routine...
C      
      CALL ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, OK, ERROR )

C
C     ...check for an error parsing the TLE pair. Signal an
C     error if OK equals .FALSE.
C
      IF ( .NOT. OK ) THEN

         CALL SETMSG ( 'Error in TLE set. #' )
         CALL ERRCH  ( '#', ERROR  )
         CALL SIGERR ( 'SPICE(BADTLE)' )
         CALL CHKOUT ( 'GETELM' )
         RETURN

      END IF
 
      CALL CHKOUT ( 'GETELM' )
      RETURN
      END
