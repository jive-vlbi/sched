C$Procedure ZZGETELM ( Get the components from two-line elements)

      SUBROUTINE ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, OK, ERROR )

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
      LOGICAL               OK
      CHARACTER*(*)         ERROR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRSTYR     I   year of earliest representable two-line elements
C     LINES      I   a pair of "lines" containing two-line elements
C     EPOCH      O   The epoch of the elements in seconds past J2000
C     ELEMS      O   The elements converted to SPICE units.
C     OK         O   Boolean indicating error state.
C     ERROR      O   String describing error.
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
C     OK         a boolean flag indicating whether an error occured
C                while processing the TLE. Processing errors include 
C                incorrect format for TLEs, angular values beyond
C                allowed range.
C
C     ERROR      a string containing a description of any TLE
C                processing error.
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
C     This routine does not signal errors, rather it returns an
C     error flag and description to the calling program. Errors 
C     reported:
C
C     1) TLE line has incorrect format.
C
C     2) A TLE '2' line has a different vehicle tag than the
C         corresponding '1' line.
C
C     3) TLE data length more than 69 characters, or less than 68 
C        characters.
C
C     4) An NPARS* routine cannot parse a string to a numeric value.
C
C     5) The value of an angular measure, NODE0, OMEGA, MO, or INCL
C        fails to lie within the expected numerical bounds.
C
C$ Particulars
C
C     This routine parses a Space Command Two-line element set and
C     returns the orbital elements properly scaled and in units
C     suitable for use by other SPICE software.  Input elements
C     have the form:
C
C  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
C  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
C  ^
C  123456789012345678901234567890123456789012345678901234567890123456789
C           1         2         3         4         5         6
C
C     The ``raw'' elements in the first  and second lines are marked
C     below.  Note that in several instances exponents and decimal
C     points are implied.  Also note that
C     input units are degrees, degrees/day**n and revolutions/day.
C
C
C                      DAY OF YEAR             NDD60    BSTAR
C                      vvvvvvvvvvvv            vvvvvv   vvvvvv
C  ---------------------------------------------------------------------
C  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
C  ---------------------------------------------------------------------
C                    ^^             ^^^^^^^^^^       ^^       ^^
C                    YEAR             NDT20          IEXP     IBEXP
C
C
C
C     The ``raw'' elements in the second line are marked below
C                   NODE0            OMEGA             N0
C                   vvvvvvvv         vvvvvvvv          vvvvvvvvvvv
C  ---------------------------------------------------------------------
C  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
C  ---------------------------------------------------------------------
C          ^^^^^^^^          ^^^^^^^          ^^^^^^^^
C          Inclination       Eccentricity     M0
C
C     This routine extracts these values ``inserts'' the implied
C     decimal points and exponents and then converts the inputs
C     to units of radians, radians/minute, radians/minute**2, and
C     radians/minute**3
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
C        CALL ZZGETELM ( FRSTYR, LINE, EPOCH, ELEMS, OK, ERROR )
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
C    The format of the two-line elements suffer from a "millenium"
C    problem---only two digits are used for the year of the elements.
C    It is not clear how Space Command will deal with this problem
C    as the year 2000 comes and goes.  We hope that by adjusting
C    the input FRSTYR you should be able to use this
C    routine well into the 21st century.  However, since we can't
C    predict how others will resolve the millenium problem we
C    can't be sure that our approach will be addequate to deal with
C    the problem.
C
C    The approach taken to mapping the two-digit year to the
C    full year is given by the code below. Here, YR is the
C    integer obtained by parsing the two-digit year from the first
C    line of the elements.
C
C        BEGYR = (FRSTYR/100)*100
C        YEAR  = BEGYR + YR
C
C        IF ( YEAR .LT. FRSTYR ) THEN
C           YEAR = YEAR + 100
C        END IF
C
C     This mapping will be changed if future two-line element
C     representations make this method of computing the full year
C     inaccurate.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C
C-    SPICELIB Version 1.0.0, 26-APR-2004 (EDW)
C
C        Modified routine GETELM to confirm
C        acceptable range for angular measures and exponents. 
C        The routine does not signal errors,it returns an error 
C        flag and error string to the calling program.
C
C        Routine named ZZGETELM from GETELM.
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
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      LOGICAL               RETURN
      INTEGER               LASTNB

C
C     An enumeration of the various components of the
C     elements array---ELEMS
C
C        KNDT20
C        KNDD60
C        KBSTAR
C        KINCL
C        KNODE0
C        KECC
C        KOMEGA
C        KMO
C        KNO
C

      INTEGER               START
      PARAMETER           ( START = 0 )

      INTEGER               KNDT20
      PARAMETER           ( KNDT20 = START  + 1 )

      INTEGER               KNDD60
      PARAMETER           ( KNDD60 = KNDT20 + 1 )

      INTEGER               KBSTAR
      PARAMETER           ( KBSTAR = KNDD60 + 1 )

      INTEGER               KINCL
      PARAMETER           ( KINCL  = KBSTAR + 1 )

      INTEGER               KNODE0
      PARAMETER           ( KNODE0 = KINCL  + 1 )

      INTEGER               KECC
      PARAMETER           ( KECC   = KNODE0 + 1 )

      INTEGER               KOMEGA
      PARAMETER           ( KOMEGA = KECC   + 1 )

      INTEGER               KMO
      PARAMETER           ( KMO    = KOMEGA + 1 )

      INTEGER               KNO
      PARAMETER           ( KNO    = KMO    + 1 )

      INTEGER               KEPOCH
      PARAMETER           ( KEPOCH = KNO    + 1 )

      INTEGER               NELEMS
      PARAMETER           ( NELEMS = KEPOCH )

C
C     Character string lengths
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 160 )

C
C     Maximum exponent (base 10)
C
      INTEGER               MAXP
      PARAMETER           ( MAXP =     37     )

      INTEGER               MINP
      PARAMETER           ( MINP =  -MAXP     )

C
C     Double precision constants.
C
      DOUBLE PRECISION      ONE
      PARAMETER           ( ONE  =      1.0D0 )

      DOUBLE PRECISION      TEN
      PARAMETER           ( TEN  =     10.0D0 )

C
C     Minutes/day
C
      DOUBLE PRECISION      MNPDAY
      PARAMETER           ( MNPDAY = 1440.0D0 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ERRPRS ( 13 )
      CHARACTER*(LNSIZE)    TERM  ( 13 )

      CHARACTER*(WDSIZE)    CYEAR
      CHARACTER*(WDSIZE)    CDAY
      CHARACTER*(WDSIZE)    CNDT20
      CHARACTER*(WDSIZE)    CNDD60
      CHARACTER*(WDSIZE)    CIEXP
      CHARACTER*(WDSIZE)    CBSTAR
      CHARACTER*(WDSIZE)    CIBEXP
      CHARACTER*(WDSIZE)    CINCL
      CHARACTER*(WDSIZE)    CNODE0
      CHARACTER*(WDSIZE)    CECC
      CHARACTER*(WDSIZE)    COMEGA
      CHARACTER*(WDSIZE)    CMO
      CHARACTER*(WDSIZE)    CNO

      DOUBLE PRECISION      BSTAR
      DOUBLE PRECISION      DAY
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      INCL
      DOUBLE PRECISION      MO
      DOUBLE PRECISION      NDD60
      DOUBLE PRECISION      NDT20
      DOUBLE PRECISION      NO
      DOUBLE PRECISION      NODE0
      DOUBLE PRECISION      OMEGA
      DOUBLE PRECISION      D2R
      DOUBLE PRECISION      PI2
      DOUBLE PRECISION      POWER  ( MINP:MAXP )
      DOUBLE PRECISION      TVEC   ( 8 )



      INTEGER               BEXP
      INTEGER               BEGYR
      INTEGER               I
      INTEGER               NEXP
      INTEGER               PTR
      INTEGER               YEAR
      INTEGER               YR
      INTEGER               K

      LOGICAL               FIRST

      SAVE

      DATA                  FIRST / .TRUE. /

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGETELM' )

C
C     Initialize the error indicators and the elements to zero.
C
      OK               = .TRUE.
      ERROR            = ' '
      ELEMS ( KNDT20 ) = 0.D0
      ELEMS ( KNDD60 ) = 0.D0
      ELEMS ( KBSTAR ) = 0.D0
      ELEMS ( KINCL  ) = 0.D0
      ELEMS ( KNODE0 ) = 0.D0
      ELEMS ( KECC   ) = 0.D0
      ELEMS ( KOMEGA ) = 0.D0
      ELEMS ( KMO    ) = 0.D0
      ELEMS ( KNO    ) = 0.D0
      ELEMS ( KEPOCH ) = 0.D0
      EPOCH            = 0.D0


C
C     First entry initialization.
C
      IF ( FIRST ) THEN

C
C        Define two constants. This initialization proves the most
C        useful when processing thousands of TLE sets.
C
         D2R = RPD()
         PI2 = TWOPI()

         FIRST    = .FALSE.

         POWER(0) =  ONE

         DO I = 1, MAXP
            POWER(I)  = TEN * POWER(I-1)
            POWER(-I) = ONE / POWER(I  )
         END DO

         TERM( 1) = '"YEAR" (characters 19 to 20 of the first line '
     .   //         'of a two-line element set)'
     
         TERM( 2) = '"DAY" (characters 21 to 32 of the first line o'
     .   //         'f a two-line element set)'
     
         TERM( 3) = '"NDT20" (characters 34 to 43 of the first line'
     .   //         ' of a two-line element set)'
     
         TERM( 4) = '"NDD60" (characters 45 to 45 of the first line'
     .   //         ' of a two-line element set)'
     
         TERM( 5) = '"IEXP" (characters 51 to 52 of the first line '
     .   //         'of a two-line element set)'
     
         TERM( 6) = '"BSTAR" (characters 54 to 54 of the first line'
     .   //         ' of a two-line element set)'
     
         TERM( 7) = '"IBEXP" (characters 60 to 61 of the first line'
     .   //         ' of a two-line element set)'
     
         TERM( 8) = '"INCL" (characters 9 to 16 of the second line o'
     .   //         'f a two-line element set)'
     
         TERM( 9) = '"NODE0" (characters 18 to 25 of the second line'
     .   //         ' of a two-line element set)'
     
         TERM(10) = '"ECC" (characters 27 to 33 of the second line o'
     .   //         'f a two-line element set)'
     
         TERM(11) = '"OMEGA" (characters 35 to 42 of the second line'
     .   //         ' of a two-line element set)'
     
         TERM(12) = '"MO" (characters 44 to 51 of the second line of'
     .   //         ' a two-line element set)'
     
         TERM(13) = '"NO" (characters 53 to 63 of the second line of'
     .   //         ' a two-line element set)'

      END IF

C
C     Ensure the vehicle IDs match in each line.
C
      IF ( LINES(1)(2:7) .NE. LINES(2)(2:7) ) THEN

C
C        Vehicle IDs do not match. Flag an error.
C
         ERROR = 'Line 1 of the TLE pair tagged with vehicle ID #1,'
     .      //   '  line 2 of TLE pair tagged with vehicle ID #2'
         CALL REPMC ( ERROR, '#1', LINES(1)(2:7), ERROR )
         CALL REPMC ( ERROR, '#2', LINES(2)(2:7), ERROR )

         OK = .FALSE.

         CALL CHKOUT ( 'ZZGETELM' )
         RETURN
      END IF

C
C    Check line format and length.
C
      DO K = 1, 2
    
         IF ( LASTNB( LINES(K) ) .NE. 68 .AND.
     .        LASTNB( LINES(K) ) .NE. 69 ) THEN

C
C          The TLE data line was not 68 or 69 characters long (ignoring
C          trailing whitespace). Flag an error.
C
            ERROR = 'Line #1 of the TLE has incorrect data length.'
     .         //   ' Expected length 68 or 69 elements, '
     .         //   'actual length: #2. TLE line value: #3'
            CALL REPMI ( ERROR, '#1', K                 , ERROR )
            CALL REPMI ( ERROR, '#2', LASTNB( LINES(K) ), ERROR )
            CALL REPMC ( ERROR, '#3', LINES(K)          , ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

      END DO

C
C     This isn't particularly pretty, but it is straight
C     forward.  According to the documentation on the two line
C     element sets, (as well as what's indicated by the program
C     driver that is documented in SPACETRACK REPORT NO.3
C     we can simply pick out the various components of the
C     elements from the input lines 1 and 2.
C
C     For the record we include the DECODE statement in DRIVER
C     for fetching the data out of lines one and two (after a bit
C     of pretty printing).  Note that some of these formats have
C     and implied decimal point.  In particular f6.5 and f7.7  in
C     all other cases the decimal points seem to be given explicitely.
C
C     decode (abuf,702) epoch,     xndt20,                       ...
C                       xndd60,    iexp,        bstar,    ibexp, ...
C                       xincl,     xnodeo,                       ...
C                       eo,        omegao,     xmo,       xno
C     format(      18x, d14.8, 1x, f10.8,
C                   1x, f6.5,      i2,     1x, f6.5,      i2,    /,
C                   8x, f8.4,  1x, f8.4,
C                   1x, f7.7,  1x, f8.4,   1x, f8.4   1x, f11.8 )
C
C     Note that in the two-line element sets, the epoch is read
C     as a single number.  However the documentation that describes
C     this data (as well as the code in THETAG) show that it's a lot
C     easier to capture the year and day of year separately.
C
      CYEAR  = LINES(1)(19:20)
      CDAY   = LINES(1)(21:32)
      CNDT20 = LINES(1)(34:43)
      CNDD60 = LINES(1)(45:45) // '.' // LINES(1)(46:50)
      CIEXP  = LINES(1)(51:52)
      CBSTAR = LINES(1)(54:54) // '.' // LINES(1)(55:59)
      CIBEXP = LINES(1)(60:61)

      CINCL  = LINES(2)( 9:16)
      CNODE0 = LINES(2)(18:25)
      CECC   = '0.'            // LINES(2)(27:33)
      COMEGA = LINES(2)(35:42)
      CMO    = LINES(2)(44:51)
      CNO    = LINES(2)(53:63)


C
C     Parse the numerical values from the data string.
C
      CALL NPARSI ( CYEAR,  YR,     ERRPRS(1),  PTR )
      CALL NPARSD ( CDAY,   DAY,    ERRPRS(2),  PTR )
      CALL NPARSD ( CNDT20, NDT20,  ERRPRS(3),  PTR )
      CALL NPARSD ( CNDD60, NDD60,  ERRPRS(4),  PTR )
      CALL NPARSI ( CIEXP,  NEXP,   ERRPRS(5),  PTR )
      CALL NPARSD ( CBSTAR, BSTAR,  ERRPRS(6),  PTR )
      CALL NPARSI ( CIBEXP, BEXP,   ERRPRS(7),  PTR )
      CALL NPARSD ( CINCL,  INCL,   ERRPRS(8),  PTR )
      CALL NPARSD ( CNODE0, NODE0,  ERRPRS(9),  PTR )
      CALL NPARSD ( CECC,   ECC,    ERRPRS(10), PTR )
      CALL NPARSD ( COMEGA, OMEGA,  ERRPRS(11), PTR )
      CALL NPARSD ( CMO,    MO,     ERRPRS(12), PTR )
      CALL NPARSD ( CNO,    NO,     ERRPRS(13), PTR )

C
C     Check for parse errors.
C
      DO I = 1, 13

         IF ( ERRPRS(I) .NE. ' ' ) THEN

C
C           Something could not parse. Set the error message then 
C           return.
C
            ERROR = 'An error occurred while trying to '
     .      //      'parse the term #. The diagnostic was:  # '
            CALL REPMC ( ERROR, '#', TERM(I)  , ERROR )
            CALL REPMC ( ERROR, '#', ERRPRS(I), ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

      END DO


C
C     Check for reasonable exponets; a single digit. These should
C     probably be LE 0.
C
      IF ( ABS(NEXP) .GT. 9 ) THEN

            ERROR = 'NEXP (exponent) not a single digit. '
     .              //'Actual value #1'
            CALL REPMI ( ERROR, '#1', NEXP, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN

         END IF

      IF ( ABS(BEXP) .GT. 9 ) THEN

            ERROR = 'BEXP (exponent) not a single digit. '
     .              //'Actual value #1'
            CALL REPMI ( ERROR, '#1', BEXP, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN

      END IF

C
C     Confirm correct bounds on angular values.
C
C     NODE0 - right ascension of the ascending node, [0,360)
C
      IF ( NODE0 .LT. 0.D0 .OR. NODE0 .GE. 360.D0 ) THEN

            ERROR = 'NODE0 (RA acend node) expected bounds [0,360). '
     .              //'Actual value #1'
            CALL REPMD ( ERROR, '#1', NODE0, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN

         END IF

C
C     OMEAGA - argument of the periapsis, [0,360)
C
      IF ( OMEGA .LT. 0.D0 .OR. OMEGA .GE. 360.D0 ) THEN

            ERROR = 'OMEGA (arg periap) expected bounds [0,360). '
     .              //'Actual value #1'
            CALL REPMD ( ERROR, '#1', OMEGA, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

C
C     MO - mean anomoly, [0,360)
C
      IF ( MO .LT. 0.D0 .OR. MO .GE. 360.D0 ) THEN

            ERROR = 'MO (mean anomoly) expected bounds [0,360). '
     .              //'Actual value #1'
            CALL REPMD ( ERROR, '#1', MO, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

C
C     INCL - inclination, [0,180]
C
      IF ( INCL .LT. 0.D0 .OR. INCL .GT. 180.D0 ) THEN

            ERROR = 'INCL (inclination) expected bounds [0,180). '
     .              //'Actual value #1'
            CALL REPMD ( ERROR, '#1', INCL, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF


C
C     NO - mean motion (0,20) (Earth orbiter).
C
      IF ( NO .GT. 20.D0 .OR. NO .LT. 0.D0 ) THEN

            ERROR = 'NO (mean motion) expected bounds (0,20). '
     .              //'Actual value #1'
            CALL REPMD ( ERROR, '#1', NO, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF




C
C     Finish up the computation of NDD60 and BSTAR
C
      NDD60 = NDD60 * POWER(NEXP)
      BSTAR = BSTAR * POWER(BEXP)

C
C     Convert everything from degrees to radians ...
C
      NODE0 = NODE0  * D2R
      OMEGA = OMEGA  * D2R
      MO    = MO     * D2R
      INCL  = INCL   * D2R

C
C     ... and from revolutions/day**n to radians/minutes**n
C
      NO    =  NO    * PI2/MNPDAY
      NDT20 =  NDT20 * PI2/MNPDAY/MNPDAY
      NDD60 =  NDD60 * PI2/MNPDAY/MNPDAY/MNPDAY

C
C     Finally, we need to convert the input epoch to
C     seconds past 2000. First let's adjust the year.
C     Add to YR the largest multiple of 100 that is
C     less than or equal to FRSTYR
C
      BEGYR = (FRSTYR/100)*100
      YEAR  = BEGYR + YR

      IF ( YEAR .LT. FRSTYR ) THEN
         YEAR = YEAR + 100
      END IF

C
C     Compute the epoch of the year and date.
C
      TVEC(1) = DBLE(YEAR)
      TVEC(2) = DAY

      CALL TTRANS ( 'YD.D', 'TDB', TVEC )

      EPOCH = TVEC(1)

C
C     That's it.  Load ELEMS with the elements and ship them
C     back to the calling routine.
C
      ELEMS ( KNDT20 ) = NDT20
      ELEMS ( KNDD60 ) = NDD60
      ELEMS ( KBSTAR ) = BSTAR
      ELEMS ( KINCL  ) = INCL
      ELEMS ( KNODE0 ) = NODE0
      ELEMS ( KECC   ) = ECC
      ELEMS ( KOMEGA ) = OMEGA
      ELEMS ( KMO    ) = MO
      ELEMS ( KNO    ) = NO
      ELEMS ( KEPOCH ) = EPOCH

      CALL CHKOUT ( 'ZZGETELM' )
      RETURN
      END

