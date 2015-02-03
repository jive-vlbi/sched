C$Procedure DPSPCE ( Propagate a two line element set for deep space )

      SUBROUTINE DPSPCE ( TIME, GEOPHS, ELEMS, STATE )

C$ Abstract
C
C     This routine propagates NORAD two-line element data for
C     earth orbiting deep space vehicles (a vehicle with an
C     orbital period more than 225 minutes).
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
C     EPHEMERIS
C     TWO LINE ELEMENTS
C     DEEP SPACE PROPAGATOR
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      TIME
      DOUBLE PRECISION      GEOPHS ( 8  )
      DOUBLE PRECISION      ELEMS  ( 10 )
      DOUBLE PRECISION      STATE  ( 6  )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TIME       I   Time for state evaluation in seconds past ephemeris
C                    epoch J2000.
C     GEOPHS     I   The array of geophysical constants
C     ELEMS      I   Array of orbit elements
C     STATE      O   State vector at TIME
C
C$ Detailed_Input
C
C     TIME        is the epoch in seconds past ephemeris epoch J2000
C                 to produced a state from the input elements.
C
C     GEOPHS      is a collection of 8 geophysical constants needed
C                 for computing a state.  The order of these
C                 constants must be:
C
C                 GEOPHS(1) = J2 gravitational harmonic for earth
C                 GEOPHS(2) = J3 gravitational harmonic for earth
C                 GEOPHS(3) = J4 gravitational harmonic for earth
C
C                 These first three constants are dimensionless.
C
C                 GEOPHS(4) = KE: Square root of the GM for earth where
C                             GM is expressed in earth radii cubed per
C                             minutes squared.
C
C                 GEOPHS(5) = QO: Low altitude bound for atmospheric
C                             model in km.
C
C                 GEOPHS(6) = SO: High altitude bound for atmospheric
C                             model in km.
C
C
C                 GEOPHS(7) = RE: Equatorial radius of the earth in km.
C
C
C                 GEOPHS(8) = AE: Distance units/earth radius
C                             (normally 1)
C
C                 Below are currently recommended values for these
C                 items:
C
C                   J2 =    1.082616D-3
C                   J3 =   -2.53881D-6
C                   J4 =   -1.65597D-6
C
C                 The next item is the square root of GM for the
C                 earth given in units of earth-radii**1.5/Minute
C
C                   KE =    7.43669161D-2
C
C                 The next two items define the top and
C                 bottom of the atmospheric drag model
C                 used by the type 10 ephemeris type.
C                 Don't adjust these unless you understand
C                 the full implications of such changes.
C
C                   QO =  120.0D0
C                   SO =   78.0D0
C
C                 The ER value is the equatorial radius in km
C                 of the earth as used by NORAD.
C
C                   ER = 6378.135D0
C
C                 The value of AE is the number of
C                 distance units per earth radii used by
C                 the NORAD state propagation software.
C                 The value is 1 unless you've got
C                 a very good understanding of the NORAD
C                 routine SGP4 and the affect of changing
C                 this value..
C
C                   AE =    1.0D0
C
C     ELEMS       is an array containing two-line element data
C                 as prescribed below. The elements XNDD6O and BSTAR
C                 must have been scaled by the proper exponent stored
C                 in the two line elements set.  Moreover, the
C                 various items must be converted to the units shown
C                 here.
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
C$ Detailed_Output
C
C     STATE       A 6 vector containing the X, Y, Z, Vx, Vy, Vz
C                 coordinates in the inertial frame (double
C                 precision).
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
C     This subroutine is an extensive rewrite of the SDP4
C     routine as described in the Spacetrack 3 report.  All common
C     blocks were removed and all variables are explicitly defined.
C
C     The removal of common blocks causes the set of routines to
C     execute slower than the original version of SDP4.  However the
C     stability improves especially as concerns memory and
C     expanded internal documentation.
C
C     Trivial or redundant variables have been eliminated.
C
C       R         removed, occurrence replaced with RK
C       E6A       renamed TOL
C       THETA4    removed, relevant equation recast in Horner's form
C                 i.e. something like x^4 + x^2 -> x^2 ( x^2 + 1 )
C       U         renamed UANG, U is now a euclidean 3 vector.
C       Ux,Uy,Uz  removed, replaced with 3-vector U
C       Vx,Vy,Vz  removed, replaced with 3-vector V
C       OMEGAQ    removed, usage replaced with OMEGAO
C       OMGDT     removed, same variable as OMGDOT, so all occurrences
C                 replaced with OMGDOT
C       SSL,SSG   replaced with the 5-vector SSX
C       SSH,SSE
C       SSI
C
C     Three functions present in the original Spacetrack report, ACTAN,
C     FMOD2P and THETAG, have been either replaced with an intrinsic
C     FORTRAN function (ACTAN -> DATAN2, FMOD2P -> DMOD) or recoded
C     using SPICELIB calls (THETAG).
C
C     The code at the end of this subroutine which calculates
C     orientation vectors, was replaced with a set of calls to
C     SPICELIB vector routines.
C
C     A direct comparison of output from the original Spacetrack 3 code
C     and these NAIF routines for the same elements and time parameters
C     will produce unacceptably different results.
C
C$ Examples
C
C
C   C---  Load the geophysical constants kernel and the leapsecond
C         kernel
C         CALL FURNSH( '/Users/ewright/lib/geophysical.ker' )
C         CALL FURNSH( '/kernels/gen/lsk/naif0008.tls' )
C
C
C   C---  Define a vehicle element array, TDRS 4 Geosynch
C         LINES( 1 ) = '1 19883U 89021B   97133.05943164 -.00000277  '
C        .//           '00000-0  10000-3 0  3315'
C         LINES( 2 ) = '2 19883   0.5548  86.7278 0001786 312.2904 '
C        .//           '172.2391  1.00269108202415'
C
C
C   C---  Identify the earliest first year for the elements
C         FRSTYR = 1988
C
C
C   C---  Parse the elements to something SPICE can use
C         CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS )
C
C
C   C---  Final time past epoch, 1400 mins (in seconds)
C         TF     = 1440.D0 * 60.D0
C
C   C---  Step size for elements output 360 mins (in seconds)
C         DELT   = 360.D0  * 60.D0
C
C   C---  Start time keyed off epoch
C         TIME   = EPOCH - 2.D0 * DELT
C
C         DO WHILE ( DABS(TIME - EPOCH) .LE. DABS(TF) )
C
C            CALL DPSPCE ( TIME, GEOPHS, ELEMS, STATE )
C
C            WRITE(*, FMT ='(7F17.8)' ) (TIME-EPOCH)/60.D0,
C        .                              (STATE(I),I=1,6)
C
C            TIME = TIME + DELT
C
C         END DO
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models 
C     for Propagation of NORAD Element Sets". United States Department
C     of Defense Spacetrack Report (3).
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 23-JAN-2013 (EDW)
C
C        Corrected initialization block error. The ZZDPINIT call
C        causes a side-effect required for each DPSPCE call.
C        The ZZDPINIT call now occurs outside the initialization
C        block. Note from designer, side-effects are bad.
C
C        Added proper citation for Hoots paper.
C
C-    SPICELIB Version 1.2.2, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references
C        to FURNSH.
C
C-    SPICELIB Version 1.2.1, DEC-27-2000 (EDW)
C
C       Corrected error in header documentation. Horner's Rule
C       not Butcher's.
C
C-    SPICELIB Version 1.2.0, MAR-24-1999 (EDW)
C
C       Documentation expanded to include modifications made
C       to private routines.  Some english errors corrected.
C
C       Alphabetized variable declaration lists.
C
C       Temporary variable TEMP removed.  OMGDOT argument added to
C       ZZDPSEC call.
C
C-    SPICELIB Version 1.1.0, OCT-05-1998 (WLT)
C
C        Forced initialization section until we can figure out
C        why it doesn't work on SUNs.
C
C-    SPICELIB Version 1.0.1, MAR-11-1998 (EDW)
C
C       Corrected error in header describing GEOPHS array.
C
C-    SPICELIB Version 1.0.0, NOV-11-1998 (EDW)
C
C-&

C$ Index_Entries
C
C     NORAD two line elements deep space evaluator
C
C-&

C
C     Local variables
C

C
C     Define parameters for convergence tolerance and the value for 2/3,
C     0 and 1.
C
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL     = 1.D-6     )

      DOUBLE PRECISION      TOTHRD
      PARAMETER           ( TOTHRD  = 2.D0/3.D0 )

      DOUBLE PRECISION      ZERO
      PARAMETER           ( ZERO    =  0.0D0    )

      DOUBLE PRECISION      ONE
      PARAMETER           ( ONE     =  1.0D0    )

      INTEGER               NUMGEO
      PARAMETER           ( NUMGEO  =    8      )

      INTEGER               NUMELM
      PARAMETER           ( NUMELM  =   10      )


C
C     The geophysical Quantities
C
      DOUBLE PRECISION      A3OVK2
      DOUBLE PRECISION      AE
      DOUBLE PRECISION      CK2
      DOUBLE PRECISION      CK4
      DOUBLE PRECISION      QOMS2T
      DOUBLE PRECISION      S
      DOUBLE PRECISION      XJ2
      DOUBLE PRECISION      XJ3
      DOUBLE PRECISION      XJ4
      DOUBLE PRECISION      XKE
      DOUBLE PRECISION      XKMPER
      DOUBLE PRECISION      LSTPHS ( NUMGEO )


C
C     Elements
C
      DOUBLE PRECISION      BSTAR
      DOUBLE PRECISION      EO
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      OMEGAO
      DOUBLE PRECISION      QO
      DOUBLE PRECISION      SO
      DOUBLE PRECISION      XINCL
      DOUBLE PRECISION      XMO
      DOUBLE PRECISION      XNO
      DOUBLE PRECISION      XNODEO
      DOUBLE PRECISION      LSTELM ( NUMELM )


C
C     Other quantities
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      A1
      DOUBLE PRECISION      AO
      DOUBLE PRECISION      AODP
      DOUBLE PRECISION      AXN
      DOUBLE PRECISION      AYCOF
      DOUBLE PRECISION      AYN
      DOUBLE PRECISION      AYNL
      DOUBLE PRECISION      BETAL
      DOUBLE PRECISION      BETAO
      DOUBLE PRECISION      BETAO2
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C4
      DOUBLE PRECISION      CAPU
      DOUBLE PRECISION      COEF
      DOUBLE PRECISION      COEF1
      DOUBLE PRECISION      COS2U
      DOUBLE PRECISION      COSEPW
      DOUBLE PRECISION      COSIO
      DOUBLE PRECISION      COSU
      DOUBLE PRECISION      COSUK
      DOUBLE PRECISION      DEL1
      DOUBLE PRECISION      DELO
      DOUBLE PRECISION      E
      DOUBLE PRECISION      ECOSE
      DOUBLE PRECISION      EETA
      DOUBLE PRECISION      ELSQ
      DOUBLE PRECISION      EM
      DOUBLE PRECISION      EPW
      DOUBLE PRECISION      ESINE
      DOUBLE PRECISION      ETA
      DOUBLE PRECISION      ETASQ
      DOUBLE PRECISION      OMGADF
      DOUBLE PRECISION      OMGDOT
      DOUBLE PRECISION      PERIGE
      DOUBLE PRECISION      PINVSQ
      DOUBLE PRECISION      PIO2
      DOUBLE PRECISION      PIX2
      DOUBLE PRECISION      PL
      DOUBLE PRECISION      PSISQ
      DOUBLE PRECISION      QOMS24
      DOUBLE PRECISION      RDOT
      DOUBLE PRECISION      RDOTK
      DOUBLE PRECISION      RFDOT
      DOUBLE PRECISION      RFDOTK
      DOUBLE PRECISION      RK
      DOUBLE PRECISION      S4
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SIN2U
      DOUBLE PRECISION      SINEPW
      DOUBLE PRECISION      SINIO
      DOUBLE PRECISION      SINU
      DOUBLE PRECISION      SINUK
      DOUBLE PRECISION      T2COF
      DOUBLE PRECISION      TEMP
      DOUBLE PRECISION      TEMP1
      DOUBLE PRECISION      TEMP2
      DOUBLE PRECISION      TEMP3
      DOUBLE PRECISION      TEMP4
      DOUBLE PRECISION      TEMP5
      DOUBLE PRECISION      TEMP6
      DOUBLE PRECISION      TEMPA
      DOUBLE PRECISION      TEMPE
      DOUBLE PRECISION      TEMPL
      DOUBLE PRECISION      THETA2
      DOUBLE PRECISION      TSI
      DOUBLE PRECISION      TSINCE
      DOUBLE PRECISION      TSQ
      DOUBLE PRECISION      UANG
      DOUBLE PRECISION      UK
      DOUBLE PRECISION      X1M5TH
      DOUBLE PRECISION      X1MTH2
      DOUBLE PRECISION      X3THM1
      DOUBLE PRECISION      X7THM1
      DOUBLE PRECISION      XHDOT1
      DOUBLE PRECISION      XINC
      DOUBLE PRECISION      XINCK
      DOUBLE PRECISION      XL
      DOUBLE PRECISION      XLCOF
      DOUBLE PRECISION      XLL
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XMAM
      DOUBLE PRECISION      XMDF
      DOUBLE PRECISION      XMDOT
      DOUBLE PRECISION      XN
      DOUBLE PRECISION      XNODCF
      DOUBLE PRECISION      XNODDF
      DOUBLE PRECISION      XNODE
      DOUBLE PRECISION      XNODEK
      DOUBLE PRECISION      XNODOT
      DOUBLE PRECISION      XNODP
      DOUBLE PRECISION      M     ( 3 )
      DOUBLE PRECISION      N     ( 3 )
      DOUBLE PRECISION      U     ( 3 )
      DOUBLE PRECISION      V     ( 3 )

      INTEGER               I

      LOGICAL               CONT
      LOGICAL               DOINIT
      LOGICAL               FIRST



C
C     SPICELIB routines
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI

      LOGICAL               RETURN


C
C     Save everything.
C
      SAVE


C
C     Set initialization flags
C
      DATA                  DOINIT / .TRUE. /
      DATA                  FIRST  / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DPSPCE' )
      END IF



C
C     If this is the very first time into this routine, set these
C     values.
C
      IF ( FIRST ) THEN

         PIX2   = TWOPI()
         PIO2   = HALFPI()
         FIRST  = .FALSE.

      END IF


C
C     If initialization flag is FALSE, then this is not the first
C     call to this routine.  Check the stuff.
C
      IF ( .NOT. DOINIT ) THEN

C
C        Check whether the current and last constants and elements
C        match.  If not, we need to reinitialize everything
C        since the propagation is dependent on the value of these
C        arrays.
C
         DO I = 1, NUMGEO

           IF( LSTPHS( I ) .NE. GEOPHS( I ) ) THEN
              DOINIT = .TRUE.
           END IF

         END DO


         DO I = 1, NUMELM

           IF( LSTELM( I ) .NE. ELEMS( I ) ) THEN
              DOINIT = .TRUE.
           END IF

         END DO


      END IF


C
C     Initialization block.  Always called on the initial entry and
C     anytime the geophysical or elements array changes.
C
      IF ( DOINIT ) THEN

         DOINIT = .FALSE.

C
C        Retrieve the geophysical constants from the GEOPHS array
C
         XJ2    = GEOPHS( 1 )
         XJ3    = GEOPHS( 2 )
         XJ4    = GEOPHS( 3 )
         XKE    = GEOPHS( 4 )
         QO     = GEOPHS( 5 )
         SO     = GEOPHS( 6 )
         XKMPER = GEOPHS( 7 )
         AE     = GEOPHS( 8 )

C
C        Save the geophysical constants for later comparison
C
         DO I = 1, NUMGEO
            LSTPHS( I ) = GEOPHS( I )
         END DO


C
C        Unpack the elements array.
C
         BSTAR  = ELEMS( 3  )
         XINCL  = ELEMS( 4  )
         XNODEO = ELEMS( 5  )
         EO     = ELEMS( 6  )
         OMEGAO = ELEMS( 7  )
         XMO    = ELEMS( 8  )
         XNO    = ELEMS( 9  )
         EPOCH  = ELEMS( 10 )

C
C        Save the elements for later comparison
C
         DO I = 1, NUMELM
            LSTELM( I ) = ELEMS( I )
         END DO


C
C        Set common variables, the init flag and calculate the
C        WGS-72 physical and geopotential constants
C
C        CK2 =  0.5   * J2 * AE^2
C        CK4 = -0.375 * J4 * AE^4
C
C        These are values calculated only once and then saved for
C        future access.
C
         CK2    = 0.5D0   * XJ2 * AE**2
         CK4    =-0.375D0 * XJ4 * AE**4
         QOMS2T = ( (QO - SO)*AE/XKMPER )**4
         S      = AE * ( ONE + SO/XKMPER )


C
C        Recover original mean motion (XNODP) and semimajor axis (AODP)
C        from input elements
C
         A1     = ( XKE / XNO )**TOTHRD
         COSIO  = DCOS( XINCL )
         THETA2 = COSIO**2
         X3THM1 = 3.D0 * THETA2  -  ONE
         BETAO2 = ONE - EO**2
         BETAO  = DSQRT( BETAO2 )

         DEL1   = 1.5D0*CK2*X3THM1/( (A1**2) * BETAO * BETAO2 )
         AO     = A1 * ( ONE - DEL1 * ( 0.5D0 * TOTHRD  +
     .            DEL1*( ONE + 134.D0/81.D0 * DEL1 ) ) )

         DELO   = 1.5D0 * CK2 * X3THM1/( (AO**2)*BETAO*BETAO2 )
         XNODP  = XNO/( ONE + DELO )
         AODP   =  AO/( ONE - DELO )

C
C        For perigee below 156 km, the values of S and QOMS2T are
C        altered
C
         S4     = S
         QOMS24 = QOMS2T
         PERIGE = ( AODP * ( ONE - EO ) - AE ) * XKMPER


         IF ( PERIGE .LT. 156.D0 ) THEN

            S4 = PERIGE - 78.D0

            IF ( PERIGE .GT. 98.D0) THEN

               QOMS24 = ( (120.D0 - S4) * AE/XKMPER )**4
               S4     = S4/XKMPER + AE

            ELSE
               S4     = 20.D0

            END IF

         END IF


         PINVSQ = ONE/(AODP**2 * BETAO2**2)
         TSI    = ONE/(AODP - S4)
         ETA    = AODP*EO*TSI
         ETASQ  = ETA**2
         EETA   = EO*ETA
         PSISQ  = DABS(ONE-ETASQ)
         COEF   = QOMS24 * TSI**4
         COEF1  = COEF / PSISQ**3.5D0

         C2     = COEF1 * XNODP *
     .           ( AODP *
     .             ( ONE + 1.5D0 * ETASQ +
     .                              EETA * ( 4.D0 + ETASQ) ) +
     .             0.75D0 * CK2 * TSI/PSISQ * X3THM1 *
     .                (8.D0 + 3.D0 * ETASQ * (8.D0+ETASQ) ) )

         C1     = BSTAR * C2
         SINIO  = DSIN( XINCL )
         A3OVK2 =-XJ3 / CK2 * AE**3
         X1MTH2 = ONE - THETA2

         C4     = 2.D0 * XNODP * COEF1 * AODP * BETAO2 *
     .           ( ETA * ( 2.D0 + .5D0 * ETASQ ) +
     .             EO * ( 0.5D0 + 2.D0 * ETASQ ) -
     .             2.D0 * CK2 * TSI/( AODP * PSISQ ) *
     .             (-3.D0 * X3THM1 *
     .             ( ONE - 2.D0 * EETA +
     .                           ETASQ *( 1.5D0 - 0.5D0*EETA) ) +
     .             0.75D0 * X1MTH2 * ( 2.D0 * ETASQ - EETA *
     .             ( ONE + ETASQ ) ) * DCOS( 2.D0 * OMEGAO) ) )

         TEMP1  = 3.D0   * CK2 * PINVSQ * XNODP
         TEMP2  = TEMP1  * CK2 * PINVSQ
         TEMP3  = 1.25D0 * CK4 * PINVSQ * PINVSQ * XNODP

         XMDOT  = XNODP + .5D0 * TEMP1 * BETAO * X3THM1 +
     .            0.0625D0 * TEMP2 * BETAO *
     .            ( 13.D0 + THETA2 * ( 137.D0 * THETA2 - 78.D0 ) )

         X1M5TH = ONE - 5.D0 * THETA2

         OMGDOT =-0.5D0 * TEMP1 * X1M5TH + 0.0625D0 *
     .            TEMP2 * (7.D0 + THETA2*( 395.D0 * THETA2 - 114.D0))
     .          + TEMP3 * (3.D0 + THETA2*(  49.D0 * THETA2 - 36.D0 ))

         XHDOT1 =-TEMP1 * COSIO

         XNODOT = XHDOT1 + ( 0.5D0*TEMP2*( 4.D0 - 19.D0 * THETA2 )
     .                   + 2.D0 * TEMP3
     .                   * ( 3.D0 - 7.D0 * THETA2 ) )*COSIO

         XNODCF = 3.5D0 * BETAO2 * XHDOT1 * C1
         T2COF  = 1.5D0 * C1
         XLCOF  = 0.125D0*A3OVK2*SINIO*(3.D0+5.D0*COSIO)/(ONE+COSIO)
         AYCOF  = 0.25D0 * A3OVK2*SINIO
         X7THM1 = 7.D0 * THETA2 - ONE

      END IF


      CALL ZZDPINIT( AODP  , XMDOT, OMGDOT, XNODOT,
     .               XNODP , ELEMS )


C
C     Get the time since the EPOCH in minutes.
C
      TSINCE = ( TIME - EPOCH )/60.D0

C
C     Update for secular gravity and atmospheric drag
C
      XMDF   = XMO    +  XMDOT  * TSINCE
      OMGADF = OMEGAO +  OMGDOT * TSINCE
      XNODDF = XNODEO +  XNODOT * TSINCE
      TSQ    =           TSINCE * TSINCE
      XNODE  = XNODDF +  XNODCF * TSQ
      TEMPA  = ONE    -  C1     * TSINCE
      TEMPE  =           BSTAR  * C4 * TSINCE
      TEMPL  =           T2COF  * TSQ
      XN     = XNODP


C
C     Calculate the secular terms.
C
      CALL ZZDPSEC( XMDF , OMGADF, XNODE, EM, XINC, XN, TSINCE,
     .              ELEMS, OMGDOT )

      A    = ( (XKE/XN)**TOTHRD ) * TEMPA**2
      E    = EM - TEMPE
      XMAM = XMDF + XNODP * TEMPL


C
C     Calculate the periodic terms.
C
      CALL ZZDPPER( TSINCE, E, XINC, OMGADF, XNODE, XMAM )

      XL   = XMAM + OMGADF + XNODE
      XN   = XKE/( A**1.5D0 )


C
C      Long period periodics
C
      AXN  = E * DCOS( OMGADF )
      TEMP = ONE/( A * (ONE - E**2) )
      XLL  = TEMP * XLCOF * AXN
      AYNL = TEMP * AYCOF
      XLT  = XL + XLL
      AYN  = E * DSIN( OMGADF ) + AYNL


C
C     Solve Kepler's equation
C
C           U = EPW - AXN * SIN(EPW)  +  AYN * COS(EPW)
C
C     Where
C
C        AYN  = E * SIN(OMEGA)  +   AYNL
C        AXN  = E * COS(OMEGA)
C
C     And
C
C        AYNL =  -0.50D0 * SINIO * AE * J3 / (J2 * A * (1.0D0  -  E^2))
C


C
C     Get the mod division of CAPU with 2 Pi
C
      CAPU = DMOD( (XLT - XNODE), PIX2 )

      IF ( CAPU .LT. ZERO ) THEN
         CAPU = CAPU + PIX2
      END IF


C
C     Set initial states for the Kepler solution
C

      EPW   = CAPU
      CONT  =.TRUE.

      DO WHILE ( CONT )

         TEMP2  = EPW
         SINEPW = DSIN( TEMP2 )
         COSEPW = DCOS( TEMP2 )
         TEMP3  = AXN * SINEPW
         TEMP4  = AYN * COSEPW
         TEMP5  = AXN * COSEPW
         TEMP6  = AYN * SINEPW
         EPW    = (CAPU-TEMP4+TEMP3-TEMP2)/(ONE-TEMP5-TEMP6)+TEMP2

C
C        Test for convergence against the defined tolerance
C

         IF ( DABS( EPW - TEMP2 ) .LE. TOL ) THEN
            CONT = .FALSE.
         END IF

      END DO


C
C     Short period preliminary quantities
C

      ECOSE = TEMP5 + TEMP6
      ESINE = TEMP3 - TEMP4
      ELSQ  = AXN * AXN  +  AYN * AYN
      TEMP  = ONE - ELSQ
      PL    = A * TEMP
      RK    = A * ( ONE - ECOSE )
      TEMP1 = ONE/RK
      RDOT  = XKE * DSQRT(A) * ESINE * TEMP1
      RFDOT = XKE * DSQRT(PL)* TEMP1
      TEMP2 = A * TEMP1
      BETAL = DSQRT( TEMP )
      TEMP3 = ONE/( ONE + BETAL )

      COSU  = TEMP2 * ( COSEPW - AXN + AYN * ESINE * TEMP3 )
      SINU  = TEMP2 * ( SINEPW - AYN - AXN * ESINE * TEMP3 )

C
C     Compute the angle from the x-axis of the point ( COSU, SINU )
C
      IF (      SINU .NE. ZERO
     .     .OR. COSU .NE. ZERO ) THEN

         UANG = DATAN2 (SINU,COSU)

         IF ( UANG .LT. ZERO ) THEN
            UANG = UANG + PIX2
         END IF

      ELSE
         UANG = ZERO

      END IF

      SIN2U = 2.D0 * SINU * COSU
      COS2U = 2.D0 * COSU * COSU  -  ONE
      TEMP1 = CK2   * ( ONE/PL )
      TEMP2 = TEMP1 * ( ONE/PL )


C
C     Update for short periodics
C
      RK     = RK*( ONE - 1.5D0 * TEMP2 * BETAL * X3THM1 ) +
     .         0.5D0 * TEMP1 * X1MTH2 * COS2U
      UK     = UANG  - 0.25D0 * TEMP2 * X7THM1 * SIN2U
      XNODEK = XNODE + 1.5D0  * TEMP2 * COSIO  * SIN2U
      XINCK  = XINC  + 1.5D0  * TEMP2 * COSIO  * SINIO  * COS2U
      RDOTK  = RDOT  -          XN    * TEMP1  * X1MTH2 * SIN2U
      RFDOTK = RFDOT + XN * TEMP1*( X1MTH2*COS2U + 1.5D0*X3THM1 )


C
C     Orientation vectors are calculated by
C
C     U = M sin(uk) + N cos(uk)
C     V = M cos(uk) - N sin(uk)
C
C     Where M and N are euclidean 3 vectors
C
C     M = (-sin(xnodek)cos(xinck), cos(xnodek)cos(xinck), sin(xinck) )
C     N = (           cos(xnodek), sin(xnodek)          , 0          )
C

      SINUK  = DSIN(UK)
      COSUK  = DCOS(UK)

C
C     Use LATREC to generate M and N.  M is a latitude to rectangle
C     conversion of a unit vector where PI/2 + XNODEK is the longitude
C

      CALL LATREC( ONE, PIO2 + XNODEK, XINCK, M )
      CALL LATREC( ONE,        XNODEK,  ZERO, N )

C
C     Sum the components to obtain U and V
C
      CALL VLCOM ( SINUK, M, COSUK, N, U )
      CALL VLCOM ( COSUK, M,-SINUK, N, V )


C
C     Determine the position and velocity then pack the STATE vector
C     with value scaled to KM and KPS.
C
C     R = RK    U +        0 V
C     V = RKDOT U + RK RFDOT V
C

      SCALE = XKMPER/AE

      CALL VLCOM( RK*SCALE, U, ZERO, V, STATE(1) )


C
C     Now scale to KPS for the velocity component
C

      SCALE = SCALE / 60.D0

      CALL VLCOM( RDOTK*SCALE, U, RFDOTK*SCALE, V, STATE(4) )


C
C     All done now....
C

      CALL CHKOUT ( 'DPSPCE' )

      RETURN
      END
