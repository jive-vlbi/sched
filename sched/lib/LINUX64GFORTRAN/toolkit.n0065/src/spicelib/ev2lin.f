C$Procedure      EV2LIN ( Evaluate "two-line" element data)
 
      SUBROUTINE EV2LIN  ( ET, GEOPHS, ELEMS, STATE )
 
C$ Abstract
C
C     This routine evaluates NORAD two-line element data for
C     near-earth orbiting spacecraft (that is spacecraft with
C     orbital periods less than 225 minutes).
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
C      None.
C
C$ Keywords
C
C       EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      GEOPHS ( * )
      DOUBLE PRECISION      ELEMS  ( * )
      DOUBLE PRECISION      STATE  ( * )
 
      INTEGER               NMODL
      PARAMETER           ( NMODL = 6 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C     ET          I   Epoch in seconds past ephemeris epoch J2000.
C     GEOPHS      I   Geophysical constants
C     ELEMS       I   Two-line element data
C     STATE       O   Evaluated state
C     NMODL       P   Parameter controlling number of buffered elements.
C
C$ Detailed_Input
C
C     ET          is the poch in seconds past ephemeris epoch J2000
C                 at which a state should be produced from the
C                 input elements.
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
C                 The next two items give the top and
C                 bottom of the atmospheric drag model
C                 used by the type 10 ephemeris type.
C                 Don't adjust these unless you understand
C                 the full implications of such changes.
C
C                   QO =  120.0D0
C                   SO =   78.0D0
C
C                 The following is the equatorial radius
C                 of the earth as used by NORAD in km.
C
C                   ER = 6378.135D0
C
C                 The value of AE is the number of
C                 distance units per earth radii used by
C                 the NORAD state propagation software.
C                 The value should be 1 unless you've got
C                 a very good understanding of the NORAD
C                 routine SGP4 and the affect of changing
C                 this value..
C
C                   AE =    1.0D0
C
C     ELEMS       is an array containg two-line element data
C                 as prescribed below. The elements XNDD6O and BSTAR
C                 must already be scaled by the proper exponent stored
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
C     STATE       is the state produced by evaluating the input elements
C                 at the input epoch ET. Units are km and km/sec.
C
C$ Parameters
C
C      NMODL      is a parameter that controls how many element sets
C                 can be buffered internally.  Since there are a lot
C                 of computations that are independent of time these
C                 are buffered and only computed if an unbuffered
C                 model is supplied.  This value should always
C                 be at least 2.  Increasing it a great deal is not
C                 advised since the time needed to search the
C                 buffered elements for a match increases linearly
C                 with the NMODL.  Imperically, 6 seems to be a good
C                 break even value for NMODL.
C
C$ Exceptions
C
C     Error free.
C
C     1)  No checks are made on the reasonableness of the inputs.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine evaluates NORAD two-line element sets for
C     near-earth orbitting satellites.  Near earth is defined to
C     be a satellite with an orbital period of less than 225
C     minutes.  This code is an adaptation of the NORAD routine
C     SGP4 to elliminate common blocks, allow buffering of models
C     and intermediate parameters and double precision calculations.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 02-JAN-2008 (EDW)
C
C        Corrected error in the calculation of the C4 term
C        identified by Curtis Haase.
C
C        Minor edit to the COEF1 declaration strictly
C        identifying the constant as a double.
C
C        From:
C
C           COEF1  = COEF  / PSISQ**3.5
C
C        To:
C
C           COEF1  = COEF  / PSISQ**3.5D0
C
C-    SPICELIB Version 1.0.2, 08-JUL-2004 (EDW)
C
C        Corrected error in the calculation of the C2 term.
C        Reordered C1, C2 calculations to avoid division 
C        by BSTAR.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1998 (EDW)
C
C        Corrected error in header describing the GEOPHS array.
C
C-    SPICELIB Version 1.0.0, 14-JAN-1994 (WLT)
C
C
C-&
 
 
 
C$ Index_Entries
C
C     Evaluate NORAD two-line element data.
C
C-&
 
C
C     Spicelib functions
C
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      TWOPI
 
C
C     Local Parameters
C
C
C     The following parameters give the location of the various
C     geophysical parameters needed for the two line element
C     sets.
C
C     KJ2  --- location of J2
C     KJ3  --- location of J3
C     KJ4  --- location if J4
C     KKE  --- location of KE = sqrt(GM) in eart-radii**1.5/MIN
C     KQO  --- upper bound of atmospheric model in KM
C     KSO  --- lower bound of atmospheric model in KM
C     KER  --- earth equatorial radius in KM.
C     KAE  --- distance units/earth radius
C
C
      INTEGER               KJ2
      INTEGER               KJ3
      INTEGER               KJ4
      INTEGER               KKE
      INTEGER               KQO
      INTEGER               KSO
      INTEGER               KER
      INTEGER               KAE
 
      PARAMETER           ( KJ2 = 1 )
      PARAMETER           ( KJ3 = 2 )
      PARAMETER           ( KJ4 = 3 )
      PARAMETER           ( KKE = 4 )
      PARAMETER           ( KQO = 5 )
      PARAMETER           ( KSO = 6 )
      PARAMETER           ( KER = 7 )
      PARAMETER           ( KAE = 8 )
 
      INTEGER               NGEOCN
      PARAMETER           ( NGEOCN = KAE )
C
C     An enumeration of the various components of the
C     elements array---ELEMS
C
C     KNDT20
C     KNDD60
C     KBSTAR
C     KINCL
C     KNODE0
C     KECC
C     KOMEGA
C     KMO
C     KNO
C
 
      INTEGER               KNDT20
      INTEGER               KNDD60
      INTEGER               KBSTAR
      INTEGER               KINCL
      INTEGER               KNODE0
      INTEGER               KECC
      INTEGER               KOMEGA
      INTEGER               KMO
      INTEGER               KNO
      INTEGER               KEPOCH
      INTEGER               NELEMS
 
      PARAMETER           ( KNDT20 =      1 )
      PARAMETER           ( KNDD60 =      2 )
      PARAMETER           ( KBSTAR =      3 )
      PARAMETER           ( KINCL  =      4 )
      PARAMETER           ( KNODE0 =      5 )
      PARAMETER           ( KECC   =      6 )
      PARAMETER           ( KOMEGA =      7 )
      PARAMETER           ( KMO    =      8 )
      PARAMETER           ( KNO    =      9 )
      PARAMETER           ( KEPOCH =     10 )
      PARAMETER           ( NELEMS = KEPOCH )
 
C
C     The parameters NEXT and PREV are used in our linked list
C     LIST(NEXT,I) points to the list item the occurs after
C     list item I.  LIST ( PREV, I ) points to the list item
C     that preceeds list item I.
C     NEXT
C     PREV
C
 
      INTEGER               NEXT
      PARAMETER           ( NEXT   = 1 )
 
      INTEGER               PREV
      PARAMETER           ( PREV   = 2 )
 
      INTEGER               NIL
      PARAMETER           ( NIL    = 0 )
 
C
C     There are a number of preliminary quantities that are needed
C     to compute the state.  Those that are not time dependent and
C     depend only upon the elements are stored in a buffer so that
C     if an element set matches a saved set, these preliminary
C     quantities  will not be recomputed.  PRSIZE is the parameter
C     used to declare the needed room.
C
      INTEGER               PRSIZE
      PARAMETER           ( PRSIZE = 29 )
C
C     When we perform bisection in the solution of Kepler's equation
C     we don't want to bisect too many times.
C
      INTEGER               MXLOOP
      PARAMETER           ( MXLOOP = 20 )
 
C
C     Numerical Constants
C
      DOUBLE PRECISION      HALF
      DOUBLE PRECISION      ONE
      DOUBLE PRECISION      F3OV8
      DOUBLE PRECISION      F3OV2
      DOUBLE PRECISION      TOTHRD
      DOUBLE PRECISION      ONETHD
 
      PARAMETER           ( HALF    =    0.5D0         )
      PARAMETER           ( ONE     =    1.0D0         )
      PARAMETER           ( F3OV8   =    3.0D0/8.0D0   )
      PARAMETER           ( F3OV2   =    1.50D0        )
      PARAMETER           ( TOTHRD  =    2.0D0/3.0D0   )
      PARAMETER           ( ONETHD  =    1.0D0/3.0D0   )
 
C
C     Local variables
C
C     Geophysical Quantities
C
      DOUBLE PRECISION      J2
      DOUBLE PRECISION      J3
      DOUBLE PRECISION      J4
      DOUBLE PRECISION      KE
      DOUBLE PRECISION      QO
      DOUBLE PRECISION      SO
      DOUBLE PRECISION      ER
      DOUBLE PRECISION      AE
      DOUBLE PRECISION      AE2
      DOUBLE PRECISION      AE3
      DOUBLE PRECISION      AE4
      DOUBLE PRECISION      CK2
      DOUBLE PRECISION      A3OVK2
      DOUBLE PRECISION      CK4
      DOUBLE PRECISION      QOMSO
      DOUBLE PRECISION      Q1
      DOUBLE PRECISION      Q2
      DOUBLE PRECISION      QOMS2T
      DOUBLE PRECISION      S
 
 
C
C     Elements
C
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      XNDT2O
      DOUBLE PRECISION      XNDD6O
      DOUBLE PRECISION      BSTAR
      DOUBLE PRECISION      XINCL
      DOUBLE PRECISION      XNODEO
      DOUBLE PRECISION      EO
      DOUBLE PRECISION      OMEGAO
      DOUBLE PRECISION      XMO
      DOUBLE PRECISION      XNO
 
C
C     Intermediate quantities. The time independent quantities
C     are calculated only as new elements come into the routine.
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
      DOUBLE PRECISION      BETAO3
      DOUBLE PRECISION      BETAO4
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C1SQ
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C3
      DOUBLE PRECISION      C4
      DOUBLE PRECISION      C5
      DOUBLE PRECISION      CAPU
      DOUBLE PRECISION      COEF
      DOUBLE PRECISION      COEF1
      DOUBLE PRECISION      COS2U
      DOUBLE PRECISION      COSEPW
      DOUBLE PRECISION      COSIK
      DOUBLE PRECISION      COSIO
      DOUBLE PRECISION      COSNOK
      DOUBLE PRECISION      COSU
      DOUBLE PRECISION      COSUK
      DOUBLE PRECISION      D2
      DOUBLE PRECISION      D3
      DOUBLE PRECISION      D4
      DOUBLE PRECISION      DEL1
      DOUBLE PRECISION      DELM
      DOUBLE PRECISION      DELMO
      DOUBLE PRECISION      DELO
      DOUBLE PRECISION      DELOMG
      DOUBLE PRECISION      E
      DOUBLE PRECISION      ECOSE
      DOUBLE PRECISION      EETA
      DOUBLE PRECISION      ELEMNT ( NELEMS, NMODL )
      DOUBLE PRECISION      ELSQ
      DOUBLE PRECISION      EPSILN
      DOUBLE PRECISION      EPW
      DOUBLE PRECISION      EPWNXT
      DOUBLE PRECISION      ESINE
      DOUBLE PRECISION      EST
      DOUBLE PRECISION      ETA
      DOUBLE PRECISION      ETASQ
      DOUBLE PRECISION      F
      DOUBLE PRECISION      FL
      DOUBLE PRECISION      FMOD2P
      DOUBLE PRECISION      FPRIME
      DOUBLE PRECISION      FU
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      LSTGEO ( NGEOCN )
      DOUBLE PRECISION      M
      DOUBLE PRECISION      MOV1M
      DOUBLE PRECISION      OMEGA
      DOUBLE PRECISION      OMGADF
      DOUBLE PRECISION      OMGCOF
      DOUBLE PRECISION      OMGDOT
      DOUBLE PRECISION      PERIGE
      DOUBLE PRECISION      PINVSQ
      DOUBLE PRECISION      PIX2
      DOUBLE PRECISION      PL
      DOUBLE PRECISION      PRELIM ( PRSIZE, NMODL )
      DOUBLE PRECISION      PSISQ
      DOUBLE PRECISION      QOMS24
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RDOT
      DOUBLE PRECISION      RDOTK
      DOUBLE PRECISION      RFDOT
      DOUBLE PRECISION      RFDOTK
      DOUBLE PRECISION      RK
      DOUBLE PRECISION      S4
      DOUBLE PRECISION      SIN2U
      DOUBLE PRECISION      SINEPW
      DOUBLE PRECISION      SINIK
      DOUBLE PRECISION      SINIO
      DOUBLE PRECISION      SINMO
      DOUBLE PRECISION      SINNOK
      DOUBLE PRECISION      SINU
      DOUBLE PRECISION      SINUK
      DOUBLE PRECISION      T2COF
      DOUBLE PRECISION      T3COF
      DOUBLE PRECISION      T4COF
      DOUBLE PRECISION      T5COF
      DOUBLE PRECISION      TCUBE
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
      DOUBLE PRECISION      TFOUR
      DOUBLE PRECISION      THETA2
      DOUBLE PRECISION      THETA4
      DOUBLE PRECISION      TOKM
      DOUBLE PRECISION      TOKMPS
      DOUBLE PRECISION      TSI
      DOUBLE PRECISION      TSINCE
      DOUBLE PRECISION      TSQ
      DOUBLE PRECISION      U
      DOUBLE PRECISION      UK
      DOUBLE PRECISION      UPPER
      DOUBLE PRECISION      UX
      DOUBLE PRECISION      UY
      DOUBLE PRECISION      UZ
      DOUBLE PRECISION      VX
      DOUBLE PRECISION      VY
      DOUBLE PRECISION      VZ
      DOUBLE PRECISION      X1M5TH
      DOUBLE PRECISION      X1MTH2
      DOUBLE PRECISION      X3THM1
      DOUBLE PRECISION      X7THM1
      DOUBLE PRECISION      XHDOT1
      DOUBLE PRECISION      XINCK
      DOUBLE PRECISION      XL
      DOUBLE PRECISION      XLCOF
      DOUBLE PRECISION      XLL
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XMCOF
      DOUBLE PRECISION      XMDF
      DOUBLE PRECISION      XMDOT
      DOUBLE PRECISION      XMP
      DOUBLE PRECISION      XMX
      DOUBLE PRECISION      XMY
      DOUBLE PRECISION      XN
      DOUBLE PRECISION      XNODCF
      DOUBLE PRECISION      XNODDF
      DOUBLE PRECISION      XNODE
      DOUBLE PRECISION      XNODEK
      DOUBLE PRECISION      XNODOT
      DOUBLE PRECISION      XNODP
 
      INTEGER               BEFORE
      INTEGER               COUNT
      INTEGER               AFTER
      INTEGER               HEAD
      INTEGER               I
      INTEGER               J
      INTEGER               LAST
      INTEGER               LIST ( 2, NMODL )
      INTEGER               N
 
      LOGICAL               DOINIT
      LOGICAL               NEWGEO
      LOGICAL               RECOG
      LOGICAL               UNREC
      SAVE
 
      DATA                  DOINIT / .TRUE. /
 
 
C
C     Rather than always making function calls we store the
C     values of the PI dependent constants the first time
C     through the routine.
C
      IF ( DOINIT ) THEN
 
         DOINIT = .FALSE.
         PIX2   = TWOPI()
 
         DO I = 1, NGEOCN
            LSTGEO(I) = 0.0D0
         END DO
 
         DO I = 1, NMODL
            DO J = 1, NELEMS
               ELEMNT ( J, I ) = 0.0D0
            END DO
         END DO
 
C
C        Set up our doubly linked list of most recently used
C        models.  Here's how things are supposed to be arranged:
C
C        LIST(NEXT,I)   points to the ephemeris model that was used
C                       most recently after ephemeris model I.
C        LIST(PREV,I)   points to the latest ephemeris model used
C                       that was used more recently than I.
C
C        HEAD           points to the most recently used ephemris
C                       model.
C
C
         HEAD            = 1
 
         LIST(PREV,HEAD) = NIL
         LIST(NEXT,1   ) = 2
 
 
         DO I = 2,NMODL-1
            LIST(NEXT,I) = I+1
            LIST(PREV,I) = I-1
         END DO
 
         LIST(NEXT,NMODL) = NIL
         LIST(PREV,NMODL) = NMODL - 1
 
      END IF
C
C     We update the geophysical parameters only if there
C     has been a change from the last time they were
C     supplied.
C
      IF (       ( LSTGEO( KAE ) .NE. GEOPHS( KAE ) )
     .     .OR.  ( LSTGEO( KER ) .NE. GEOPHS( KER ) )
     .     .OR.  ( LSTGEO( KJ2 ) .NE. GEOPHS( KJ2 ) )
     .     .OR.  ( LSTGEO( KJ3 ) .NE. GEOPHS( KJ3 ) )
     .     .OR.  ( LSTGEO( KJ4 ) .NE. GEOPHS( KJ4 ) )
     .     .OR.  ( LSTGEO( KKE ) .NE. GEOPHS( KKE ) )
     .     .OR.  ( LSTGEO( KQO ) .NE. GEOPHS( KQO ) )
     .     .OR.  ( LSTGEO( KSO ) .NE. GEOPHS( KSO ) ) ) THEN
 
         DO I = 1, NGEOCN
            LSTGEO(I) = GEOPHS(I)
         END DO
 
         J2     = GEOPHS( KJ2 )
         J3     = GEOPHS( KJ3 )
         J4     = GEOPHS( KJ4 )
         KE     = GEOPHS( KKE )
         QO     = GEOPHS( KQO )
         SO     = GEOPHS( KSO )
         ER     = GEOPHS( KER )
         AE     = GEOPHS( KAE )
 
         AE2    =  AE    * AE
         AE3    =  AE    * AE2
         AE4    =  AE    * AE3
         CK2    =  HALF  * J2 * AE2
         A3OVK2 = -2.0D0 * J3 * AE  / J2
         CK4    = -F3OV8 * J4 * AE4
         QOMSO  = QO-SO
         Q1     = QOMSO  * AE / ER
         Q2     = Q1     * Q1
         QOMS2T = Q2     * Q2
         S      = AE     * (ONE+SO/ER)
C
C        When we've finished up we will need to convert everything
C        back to KM and KM/SEC  the two variables below give the
C        factors we shall need to do this.
C
         TOKM   = ER/AE
         TOKMPS = TOKM   / 60.0D0
 
         NEWGEO = .TRUE.
 
      ELSE
 
         NEWGEO = .FALSE.
 
      END IF
 
 
C
C     Fetch all of the pieces of this model.
C
      EPOCH  = ELEMS ( KEPOCH )
      XNDT2O = ELEMS ( KNDT20 )
      XNDD6O = ELEMS ( KNDD60 )
      BSTAR  = ELEMS ( KBSTAR )
      XINCL  = ELEMS ( KINCL  )
      XNODEO = ELEMS ( KNODE0 )
      EO     = ELEMS ( KECC   )
      OMEGAO = ELEMS ( KOMEGA )
      XMO    = ELEMS ( KMO    )
      XNO    = ELEMS ( KNO    )
 
C
C     See if this model is already buffered, start at the first
C     model in the list (the most recently used model).
C
 
      UNREC = .TRUE.
      N     =  HEAD
 
      DO WHILE (  N .NE. NIL .AND. UNREC )
C
C        The actual order of the elements is such that we can
C        usually tell that a stored model is different from
C        the one under consideration by looking at the
C        end of the list first.  Hence we start with I = NELEMS
C        and decrement I until we have looked at everything
C        or found a mismatch.
C
         RECOG = .TRUE.
         I     =  NELEMS
 
         DO WHILE  ( RECOG .AND. I           .GT. 0      )
            RECOG  = RECOG .AND. ELEMNT(I,N) .EQ. ELEMS(I)
            I      = I - 1
         END DO
 
         UNREC = .NOT. RECOG
 
         IF ( UNREC ) THEN
            LAST = N
            N    = LIST(NEXT,N)
         END IF
 
      END DO
 
      IF ( N .EQ. NIL ) THEN
         N = LAST
      END IF
C
C     Either N points to a recognized item or it points to the
C     tail of the list where the least recently used items is
C     located.  In either case N must be made the head of the
C     list.  (If it is already the head of the list we don't
C     have to bother with anything.)
C
      IF ( N .NE. HEAD ) THEN
 
C
C        Find the items that come before and after N and
C        link them together.
C
         BEFORE                = LIST ( PREV, N )
         AFTER                 = LIST ( NEXT, N )
 
         LIST ( NEXT, BEFORE ) = AFTER
 
         IF ( AFTER .NE. NIL ) THEN
            LIST ( PREV, AFTER  ) = BEFORE
         END IF
C
C        Now the guy that will come after N is the current
C        head of the list.  N will have no predecessor.
C
         LIST ( NEXT, N      ) = HEAD
         LIST ( PREV, N      ) = NIL
C
C        The predecessor the current head of the list becomes N
C
         LIST ( PREV, HEAD   ) = N
C
C        and finally, N becomes the head of the list.
C
         HEAD = N
 
      END IF
 
 
      IF ( RECOG .AND. .NOT. NEWGEO ) THEN
C
C        We can just look up the intermediate values from
C        computations performed on a previous call to this
C        routine.
C
         AODP   = PRELIM (  1, N )
         AYCOF  = PRELIM (  2, N )
         C1     = PRELIM (  3, N )
         C4     = PRELIM (  4, N )
         C5     = PRELIM (  5, N )
         COSIO  = PRELIM (  6, N )
         D2     = PRELIM (  7, N )
         D3     = PRELIM (  8, N )
         D4     = PRELIM (  9, N )
         DELMO  = PRELIM ( 10, N )
         ETA    = PRELIM ( 11, N )
         OMGCOF = PRELIM ( 12, N )
         OMGDOT = PRELIM ( 13, N )
         PERIGE = PRELIM ( 14, N )
         SINIO  = PRELIM ( 15, N )
         SINMO  = PRELIM ( 16, N )
         T2COF  = PRELIM ( 17, N )
         T3COF  = PRELIM ( 18, N )
         T4COF  = PRELIM ( 19, N )
         T5COF  = PRELIM ( 20, N )
         X1MTH2 = PRELIM ( 21, N )
         X3THM1 = PRELIM ( 22, N )
         X7THM1 = PRELIM ( 23, N )
         XLCOF  = PRELIM ( 24, N )
         XMCOF  = PRELIM ( 25, N )
         XMDOT  = PRELIM ( 26, N )
         XNODCF = PRELIM ( 27, N )
         XNODOT = PRELIM ( 28, N )
         XNODP  = PRELIM ( 29, N )
 
      ELSE
 
C
C        Compute all of the intermediate items needed.
C        First, the inclination dependent constants.
C
         COSIO  = DCOS ( XINCL )
         SINIO  = DSIN ( XINCL )
         THETA2 = COSIO  * COSIO
 
         THETA4 = THETA2 * THETA2
         X3THM1 = 3.0D0  * THETA2  -  ONE
         X7THM1 = 7.0D0  * THETA2  -  ONE
 
         X1MTH2 = ONE    - THETA2
         X1M5TH = ONE    - 5.0D0 * THETA2
C
C        Eccentricity dependent constants
C
         BETAO  = DSQRT(ONE - EO*EO)
         BETAO2 = ONE       - EO*EO
         BETAO3 = BETAO  * BETAO2
         BETAO4 = BETAO2 * BETAO2
C
C        Semi-major axis and ascending node related constants.
C
         A1     = (KE/XNO)**TOTHRD
         DEL1   = 1.5D0 * CK2 * X3THM1 / ( A1*A1*BETAO3 )
 
         AO     = A1    * ( ONE
     .                     - DEL1*( ONETHD
     .                           + DEL1*( ONE + DEL1*134.0D0/81.0D0)))
 
         DELO   = 1.5D0 * CK2 * X3THM1 / (AO*AO*BETAO3)
 
         XNODP  = XNO / (ONE + DELO)
         AODP   = AO  / (ONE - DELO)
 
         S4     = S
         QOMS24 = QOMS2T
         PERIGE = ER * ( AODP*(ONE-EO) - AE )
 
C
C        For perigee below 156 km, the values of S and QOMS2T are
C        altered.
C
         IF ( PERIGE .LT. 156.0D0 ) THEN
            S4 = PERIGE - 78.0D0
 
            IF ( PERIGE .LE. 98.0D0 ) THEN
               S4 = 20.0D0
            END IF
 
            QOMS24 = ( (120.0D0-S4)*AE / ER )**4
            S4     = AE + ( S4/ER )
         END IF
C
C        The next block is simply a pretty print of the code in
C        sgp4 from label number 10 through the label 90.
C
         PINVSQ = ONE  / (AODP * AODP * BETAO4)
         TSI    = ONE  / (AODP - S4           )
 
         ETA    = AODP   *  EO  * TSI
         ETASQ  = ETA    *  ETA
         EETA   = EO     *  ETA
         COEF   = QOMS24 *  TSI**4
 
         PSISQ  = DABS(ONE - ETASQ)
 
         COEF1  = COEF  / PSISQ**3.5D0
         C2     = COEF1
     .            * XNODP
     .            * (   AODP   * (  ONE
     .                           + F3OV2*ETASQ
     .                           + EETA *( 4.0D0 +       ETASQ) )
     .               + 0.75D0 * CK2
     .                        * (TSI/PSISQ)
     .                        * X3THM1
     .                        * (  8.0D0
     .                           + ETASQ*(24.0D0 + 3.0D0*ETASQ) )   )
 
         C1     =  C2 * BSTAR
 
         C3     =  COEF * TSI * A3OVK2 * XNODP *AE * SINIO / EO
 
         C4    = 2.0D0 * XNODP * COEF1 * AODP
     .         * BETAO2
     .         * (  ETA   * ( 2.0D0 + 0.50D0*ETASQ  )
     .            + EO    * ( HALF  + 2.0D0*ETASQ  )
     .            - 2.0D0 * ( CK2*TSI/(AODP*PSISQ) )
     .                     * ( -3.00D0 * X3THM1
     .                                 * ( ONE
     .                                   - EETA * 2.0D0
     .                                   + ETASQ*(F3OV2 - HALF*EETA))
     .                        + 0.75D0 * DCOS ( 2.0D0 * OMEGAO )
     .                                 * X1MTH2
     .                                 * (  ETASQ * 2.0D0
     .                                    - EETA  * (ONE+ETASQ))))
 
         C5    = 2.0D0*COEF1*AODP*BETAO2
     .                  *(ONE + 2.75D0*(ETASQ+EETA) + EETA*ETASQ )
 
         TEMP1  = 3.0D0  * CK2 * PINVSQ * XNODP
         TEMP2  = TEMP1  * CK2 * PINVSQ
         TEMP3  = 1.25D0 * CK4 * PINVSQ * PINVSQ * XNODP
 
         XMDOT  =  XNODP
     .            +  HALF     * TEMP1 * BETAO * X3THM1
     .            +  0.0625D0 * TEMP2 * BETAO * (  13.0D0
     .                                       -  78.0D0*THETA2
     .                                       + 137.0D0*THETA4 )
 
         OMGDOT = -HALF      * TEMP1 * X1M5TH
     .            + 0.0625D0 * TEMP2 * (   7.0D0
     .                                 - 114.0D0*THETA2
     .                                 + 395.0D0*THETA4 )
     .            +            TEMP3 * (   3.0D0
     .                                 -  36.0D0*THETA2
     .                                 +  49.0D0*THETA4 )
 
         XHDOT1 = -TEMP1*COSIO
 
         XNODOT = XHDOT1
     .            + COSIO * (  HALF  * TEMP2*(4.0D0 - 19.0D0*THETA2)
     .                     + 2.0D0 * TEMP3*(3.0D0 -  7.0D0*THETA2) )
 
         OMGCOF =  BSTAR   * C3     * DCOS(OMEGAO)
         XMCOF  = -BSTAR   * TOTHRD * COEF  * AE / EETA
         XNODCF =  3.5D0   * BETAO2 * XHDOT1* C1
         T2COF  =  F3OV2   * C1
         AYCOF  =  0.250D0 * A3OVK2 * SINIO
         XLCOF  =  HALF    * AYCOF  * (3.0D0 + 5.0D0*COSIO)
     .                              / (ONE   +       COSIO)
 
         DELMO  =  (ONE    + ETA*COS(XMO) )**3
         SINMO  =  SIN(XMO)
C
C        For perigee less than 220 kilometers, the ISIMP flag is set
C        and the equations are truncated to linear variation in SQRT
C        A and quadratic variation in mean anomaly.  Also, the C3
C        term, the Delta OMEGA term, and the Delta M term are
C        dropped.  (Note: Normally we would just use
C
         IF ( PERIGE .GE. 220.0D0 ) THEN
 
            C1SQ  = C1*C1
            D2    = 4.0D0 * TSI   * C1SQ   * AODP
            TEMP  = D2    * TSI   * C1     * ONETHD
            D3    = TEMP  * (S4   + 17.0D0 * AODP )
            D4    = TEMP  * TSI   * C1     * AODP
     .                    * HALF  *(221.0D0* AODP + 31.0D0*S4)
 
            T3COF = D2    + 2.0D0 * C1SQ
 
            T4COF = 0.25D0 * ( 3.0D0*D3 + C1*(  12.0D0*D2
     .                                        + 10.0D0*C1SQ) )
 
            T5COF = 0.20D0 * (3.0D0*D4 + 12.0D0*C1*D3
     .                                 +  6.0D0*D2*D2
     .                                 + 15.0D0*C1SQ*(  2.0D0*D2
     .                                                + C1SQ) )
         END IF
C
C        Now store the intermediate computations so that if we
C        should hit this model again we can just look up the needed
C        results from the above computations.
C
         PRELIM (  1, N ) = AODP
         PRELIM (  2, N ) = AYCOF
         PRELIM (  3, N ) = C1
         PRELIM (  4, N ) = C4
         PRELIM (  5, N ) = C5
         PRELIM (  6, N ) = COSIO
         PRELIM (  7, N ) = D2
         PRELIM (  8, N ) = D3
         PRELIM (  9, N ) = D4
         PRELIM ( 10, N ) = DELMO
         PRELIM ( 11, N ) = ETA
         PRELIM ( 12, N ) = OMGCOF
         PRELIM ( 13, N ) = OMGDOT
         PRELIM ( 14, N ) = PERIGE
         PRELIM ( 15, N ) = SINIO
         PRELIM ( 16, N ) = SINMO
         PRELIM ( 17, N ) = T2COF
         PRELIM ( 18, N ) = T3COF
         PRELIM ( 19, N ) = T4COF
         PRELIM ( 20, N ) = T5COF
         PRELIM ( 21, N ) = X1MTH2
         PRELIM ( 22, N ) = X3THM1
         PRELIM ( 23, N ) = X7THM1
         PRELIM ( 24, N ) = XLCOF
         PRELIM ( 25, N ) = XMCOF
         PRELIM ( 26, N ) = XMDOT
         PRELIM ( 27, N ) = XNODCF
         PRELIM ( 28, N ) = XNODOT
         PRELIM ( 29, N ) = XNODP
C
C        Finally, move these elements in the storage area
C        for checking the next time through.
C
         DO I = 1, NELEMS
            ELEMNT(I,N) = ELEMS(I)
         END DO
 
      END IF
C
C     Now that all of the introductions are out of the way
C     we can get down to business.
C
C     Compute the time since the epoch for this model.
C
      TSINCE = ET - EPOCH
C
C     and convert it to minutes
C
      TSINCE = TSINCE / 60.0D0
 
 
      XMDF   = XMO      +   XMDOT  * TSINCE
      OMGADF = OMEGAO   +   OMGDOT * TSINCE
      XNODDF = XNODEO   +   XNODOT * TSINCE
      OMEGA  = OMGADF
      XMP    = XMDF
 
      TSQ    = TSINCE  *  TSINCE
      XNODE  = XNODDF  +  XNODCF * TSQ
      TEMPA  = 1.0D0   -  C1     * TSINCE
      TEMPE  = BSTAR   *  C4     * TSINCE
      TEMPL  = T2COF   *  TSQ
 
 
      IF(PERIGE .GT. 220.0D0 ) THEN
 
         TCUBE  = TSQ    * TSINCE
         TFOUR  = TCUBE  * TSINCE
 
         DELOMG = OMGCOF * TSINCE
         DELM   = XMCOF* ( (1.0D0 + ETA*COS(XMDF))**3  -  DELMO )
         TEMP   = DELOMG + DELM
         XMP    = XMDF   + TEMP
         OMEGA  = OMGADF - TEMP
 
         TEMPA  = TEMPA - D2*TSQ
     .                  - D3*TCUBE
     .                  - D4*TFOUR
 
         TEMPE  = TEMPE  +  BSTAR * C5 * (SIN(XMP) - SINMO)
         TEMPL  = TEMPL  +  TCUBE * T3COF
     .                   +  TFOUR *(T4COF + TSINCE*T5COF)
      END IF
 
      A    =  AODP * TEMPA**2
      XL   =  XMP  + OMEGA  +  XNODE  +  XNODP*TEMPL
 
      E    =  EO   - TEMPE
 
C
C     The parameter BETA used to be needed, but it's only use
C     was in the computation of TEMP below where it got squared
C     so we'll remove it from the list of things to compute.
C
C     BETA =  DSQRT( 1.0D0 - E*E )
C
 
 
 
      XN   =  KE/A**1.5D0
 
C
C     Long period periodics
C
      TEMP = 1.0D0/(A*(1.0D0 - E*E) )
      AYNL = TEMP * AYCOF
      AYN  = E    * SIN(OMEGA)   +    AYNL
      AXN  = E    * COS(OMEGA)
 
      XLL  = TEMP * XLCOF * AXN
      XLT  = XL   + XLL
 
C
C     Solve keplers equation.
C
C     We are going to solve for the roots of this equation by
C     using a mixture of Newton's method and the prescription for
C     root finding outlined in the SPICE routine UNITIM.
C
C     We are going to solve the equation
C
C           U = EPW - AXN * SIN(EPW)  +  AYN * COS(EPW)
C
C     Where
C
C        AYN  = E    * SIN(OMEGA)   +    AYNL
C        AXN  = E    * COS(OMEGA)
C
C     And
C
C        AYNL =  -0.50D0  * SINIO * AE * J3   / (J2*A*(1.0D0 - E*E))
C
C     Since this is a low earth orbiter (period less than 225 minutes)
C     The maximum value E can take (without having the orbiter
C     plowing fields) is approximately 0.47 and AYNL will not be
C     more than about .01.  ( Typically E will be much smaller
C     on the order of about .1 )  Thus we can initially
C     view the problem of solving the equation for EPW as a
C     function of the form
C
C     U = EPW + F ( EPW )                                   (1)
C
C     Where F( EPW ) = -AXN*SIN(EPW) + AYN*COS(EPW)
C
C     Note that | F'(EPW) | < M =  DSQRT( AXN**2 + AYN**2 ) < 0.48
C
C     From the above discussion it is evident that F is a contraction
C     mapping.  So that we can employ the same techniques as were
C     used in the routine UNITIM to get our first approximations of
C     the root.  Once we have some good first approximations, we
C     will speed up the root finding by using Newton's method for
C     finding a zero of a function.  The function we will work on
C     is
C
C        f  (x) = x - U - AXN*SIN(x) + AYN*COS(x)         (2)
C
C     By applying Newton's method we will go from linear to
C     quadratic convergence.
C
C     We will keep track of our error bounds along the way so
C     that we will know how many iterations to perform in each
C     phase of the root extraction.
C
C     few steps using bisection.
C
C
C     For the benefit of those interested
C     here's the basics of what we'll do.
C
C        Whichever EPW satisfies equation (1) will be
C        unique. The uniqueness of the solution is ensured because the
C        expression on the right-hand side of the equation is
C        monotone increasing in EPW.
C
C        Let's suppose that EPW is the solution, then the following
C        is true.
C
C           EPW = U - F(EPW)
C
C        but we can also replace the EPW on the right hand side of the
C        equation by U - F(EPW).  Thus
C
C           EPW = U - F( U - F(EPW))
C
C               = U - F( U - F( U - F(EPW)))
C
C               = U - F( U - F( U - F( U - F(EPW))))
C
C               = U - F( U - F( U - F( U - F( U - F(EPW)))))
C               .
C               .
C               .
C               = U - F( U - F( U - F( U - F( U - F(U - ... )))
C
C        and so on, for as long as we have patience to perform the
C        substitutions.
C
C        The point of doing this recursive substitution is that we
C        hope to move EPW to an insignificant part of the computation.
C        This would seem to have a reasonable chance of success since
C        F is a bounded and has a small derivative.
C
C        Following this idea, we will attempt to solve for EPW using
C        the recursive method outlined below.
C
C     We will make our first guess at EPW, call it EPW_0.
C
C        EPW_0 = U
C
C     Our next guess, EPW_1, is given by:
C
C        EPW_1 = U - F(EPW_0)
C
C     And so on:
C
C        EPW_2 = U - F(EPW_1)        [ = U - F(U - F(U))          ]
C        EPW_3 = U - F(EPW_2)        [ = U - F(U - F(U - F(U)))   ]
C           .
C           .
C           .
C        EPW_n = U - F(EPW_(n-1))    [ = U - F(U - F(U - F(U...)))]
C
C        The questions to ask at this point are:
C
C           1) Do the EPW_i's converge?
C           2) If they converge, do they converge to EPW?
C           3) If they converge to EPW, how fast do they get there?
C
C        1) The sequence of approximations converges.
C
C           | EPW_n - EPW_(n-1) | =  [ U - F( EPW_(n-1) ) ]
C                                 -  [ U - F( EPW_(n-2) ) ]
C
C                                 =  [ F( EPW_(n-2) ) - F( EPW_(n-1)) ]
C
C     The function F has an important property. The absolute
C     value of its derivative is always less than M.
C     This means that for any pair of real numbers s,t
C
C        | F(t) - F(s) |  < M*| t - s |.
C
C     From this observation, we can see that
C
C        | EPW_n - EPW_(n-1) | < M*| EPW_(n-1) - EPW_(n-2) |
C
C     With this fact available, we could (with a bit more work)
C     conclude that the sequence of EPW_i's converges and that
C     it converges at a rate that is at least as fast as the
C     sequence M, M**2, M**3.  In fact the difference
C        |EPW - EPW_N| < M/(1-M) * | EPW_N - EPW_(N-1) |
C
C                       < M/(1-M) * M**N | EPW_1 - EPW_0 |
C
C     2) If we let EPW be the limit of the EPW_i's then it follows
C        that
C
C               EPW = U - F(EPW).
C
C
C     or that
C
C               U = EPW + F(EPW).
C
C     We will use this technique to get an approximation that
C     is within a tolerance of EPW and then switch to
C     a Newton's method. (We'll compute the tolerance using
C     the value of M given above).
C
C
C     For the Newton's method portion of the problem, recall
C     from Taylor's formula that:
C
C        f(x) = f(x_0) + f'(x_0)(x-x_0) +  f''(c)/2 (x-x_0)**2
C
C     for some c between x and x_0
C
C     If x happens to be a zero of f then we can rearrange the
C     terms above to get
C
C                       f(x_0)       f''(c)
C           x = x_0 -   -------  +  -------- ( x - x_0)**2
C                       f'(x_0)      f'(x_0)
C
C     Thus the error in the Newton approximation
C
C
C                       f(x_0)
C           x = x_0  -  -------
C                       f'(x_0)
C
C     is
C
C                     f''(c)
C                    -------- ( x - x_0)**2
C                     f'(x_0)
C
C     Thus if we can bound f'' and pick a good first
C     choice for x_0 (using the first method outlined
C     above we can get quadratic convergence.)
C
C     In our case we have
C
C        f  (x) = x - U - AXN*SIN(x) + AYN*COS(x)
C        f' (x) = 1     - AXN*COS(x) - AYN*SIN(x)
C        f''(x) =         AXN*SIN(x) - AYN*COS(x)
C
C     So that:
C
C        f' (x) >  1 - M
C
C        f''(x) <  M
C
C     Thus the error in the Newton's approximation is
C     at most
C
C        M/(1-M) * ( x - x_0 )**2
C
C     Thus as long as our original estimate (determined using
C     the contraction method) gets within a reasonable tolerance
C     of x, we can use Newton's method to acheive faster
C     convergence.
C
 
      M      = DSQRT ( AXN*AXN + AYN*AYN )
      MOV1M  = DABS  ( M / ( 1.0D0 - M ) )
 
 
      FMOD2P = DMOD(XLT-XNODE,PIX2)
 
      IF ( FMOD2P .LT. 0 ) THEN
         FMOD2P = FMOD2P + PIX2
      END IF
 
      CAPU  = FMOD2P
      EPW   = CAPU
      EST   = 1.0D0
 
      DO WHILE ( EST .GT. 0.125D0 )
         EPWNXT = CAPU - AXN*SIN(EPW) + AYN*COS(EPW)
         EST    = MOV1M * DABS ( EPWNXT - EPW )
         EPW    = EPWNXT
      END DO
C
C     We need to be able to add something to EPW and not
C     get EPW (but not too much).
C
      EPSILN = EST
 
      IF ( EPSILN + EPW .NE. EPW ) THEN
 
C
C        Now we switch over to Newton's method.  Note that
C        since our error estimate is less than 1/8, six iterations
C        of Newton's method should get us to within 1/2**96 of
C        the correct answer (If there were no round off to contend
C        with).
C
         DO I = 1, 5
 
            SINEPW = SIN(EPW)
            COSEPW = COS(EPW)
 
            F      =  EPW - CAPU - AXN * SINEPW + AYN * COSEPW
            FPRIME =  1.0D0      - AXN * COSEPW -  AYN * SINEPW
 
            EPWNXT = EPW -  F/FPRIME
C
C           Our new error estimate comes from the discussion
C           of convergence of Newton's method.
C
            EPW    = EPWNXT
 
            IF ( EPW + EST .NE. EPW ) THEN
               EPSILN = EST
               EST    = MOV1M * EST * EST
            END IF
 
         END DO
 
      END IF
C
C     Finally, we use bisection to avoid the problems of
C     round-off that may be present in Newton's method.  Since
C     we've gotten quite close to the answer (theoretically
C     anyway)  we won't have to perform many bisection passes.
C
C     First we must bracket the root.  Note that we will
C     increase EPSILN so that we don't spend much time
C     determining the bracketing interval.  Also if the first
C     addition of EPSILN to EPW doesn't modify it, were set up
C     to just quit.  This happens only if F is sufficiently
C     close to zero that it can't alter EPW by adding it to
C     or subtracting it from EPW.
C
 
      SINEPW = SIN(EPW)
      COSEPW = COS(EPW)
 
      F      =  EPW - CAPU - AXN * SINEPW + AYN * COSEPW
      EPSILN =  MAX ( DABS(F), EPSILN )
 
      IF ( F .EQ. 0 ) THEN
 
         LOWER = EPW
         UPPER = EPW
 
      ELSE IF ( F .GT. 0 ) THEN
 
         FU    = F
         UPPER = EPW
         LOWER = EPW - EPSILN
         EPW   = LOWER
 
         DO WHILE ( F .GT. 0 .AND. LOWER .NE. UPPER )
            EPW    = EPW - EPSILN
            F      = EPW - CAPU - AXN * SIN(EPW) + AYN * COS(EPW)
            EPSILN = EPSILN * 2.0D0
         END DO
 
         LOWER = EPW
         FL    = F
 
         IF ( F .EQ. 0 ) THEN
            UPPER = LOWER
         END IF
 
 
      ELSE IF ( F .LT. 0 ) THEN
 
         FL    = F
         LOWER = EPW
         UPPER = EPW + EPSILN
         EPW   = UPPER
 
         DO WHILE ( F .LT. 0 .AND. LOWER .NE. UPPER )
            EPW    = EPW + EPSILN
            F      = EPW - CAPU - AXN * SIN(EPW) + AYN * COS(EPW)
            EPSILN = EPSILN * 2.0D0
         END DO
 
         UPPER = EPW
         FU    = F
 
         IF ( F .EQ. 0 ) THEN
            LOWER = EPW
         END IF
 
      END IF
 
C
C     Finally, bisect until we can do no more.
C
      COUNT = 0
 
      DO WHILE ( UPPER .GT. LOWER .AND. COUNT .LT. MXLOOP )
 
         COUNT = COUNT + 1
         EPW   = BRCKTD( 0.5D0 * ( UPPER + LOWER ), LOWER, UPPER )
C
C        EPW eventually will not be different from one of the
C        two bracketing values.  If this is the time, we need
C        to decide on a value for EPW.  That's done below.
C
         IF   (      ( EPW .EQ. UPPER )
     .          .OR. ( EPW .EQ. LOWER ) ) THEN
 
            IF ( -FL .LT. FU ) THEN
               EPW   = LOWER
               UPPER = LOWER
            ELSE
               EPW   = UPPER
               LOWER = UPPER
            END IF
 
         ELSE
 
            F   = EPW - CAPU - AXN * SIN(EPW) + AYN * COS(EPW)
 
            IF ( F .GT. 0 ) THEN
               UPPER = EPW
               FU    = F
            ELSE IF ( F .LT. 0 ) THEN
               LOWER = EPW
               FL    = F
            ELSE
               LOWER = EPW
               UPPER = EPW
            END IF
 
         END IF
 
      END DO
 
 
C
C     Short period preliminary quantities
C
 
      SINEPW = SIN(EPW)
      COSEPW = COS(EPW)
 
      TEMP3  = AXN * SINEPW
      TEMP4  = AYN * COSEPW
 
      TEMP5  = AXN * COSEPW
      TEMP6  = AYN * SINEPW
 
      ECOSE = TEMP5   + TEMP6
      ESINE = TEMP3   - TEMP4
      ELSQ  = AXN*AXN + AYN*AYN
      TEMP  = 1.0D0   - ELSQ
      PL    = A       * TEMP
      R     = A       * (1.0D0 - ECOSE)
 
      TEMP1 = 1.0D0   /  R
      RDOT  = KE      * TEMP1 * SQRT(A)  * ESINE
      RFDOT = KE      * TEMP1 * SQRT(PL)
      TEMP2 = A       * TEMP1
      BETAL = DSQRT   ( TEMP )
      TEMP3 = 1.0D0 / (1.0D0 + BETAL)
 
      COSU  = TEMP2 * (COSEPW  -  AXN  +  AYN * ESINE * TEMP3)
      SINU  = TEMP2 * (SINEPW  -  AYN  -  AXN * ESINE * TEMP3)
C
C     Compute the angle from the x-axis of the point ( COSU, SINU )
C
      IF (      SINU .NE. 0.0D0
     .     .OR. COSU .NE. 0.0D0 ) THEN
 
         U     = DATAN2 (SINU,COSU)
 
         IF ( U .LT. 0 ) THEN
            U = U + PIX2
         END IF
 
      ELSE
 
         U = 0.0D0
 
      END IF
 
 
      SIN2U = 2.0D0 * SINU * COSU
      COS2U = 2.0D0 * COSU * COSU  -  1.0D0
      TEMP  = 1.0D0 / PL
      TEMP1 = CK2   * TEMP
      TEMP2 = TEMP1 * TEMP
 
C
C     Update for short periodics
C
      RK     = R * ( 1.0D0   -  1.5D0 * TEMP2 * BETAL * X3THM1)
     .       +     0.50D0 * TEMP1 * X1MTH2 * COS2U
      UK     = U - 0.25D0 * TEMP2 * X7THM1 * SIN2U
 
      XNODEK = XNODE + 1.50D0 * TEMP2 * COSIO  * SIN2U
      XINCK  = XINCL + 1.50D0 * TEMP2 * COSIO  * COS2U * SINIO
      RDOTK  = RDOT  - XN     * TEMP1 * X1MTH2 * SIN2U
      RFDOTK = RFDOT + XN     * TEMP1 *(X1MTH2 * COS2U + 1.5D0*X3THM1)
C
C     Orientation vectors
C
      SINUK  = SIN(UK)
      COSUK  = COS(UK)
 
      SINIK  = SIN(XINCK)
      COSIK  = COS(XINCK)
 
      SINNOK = SIN(XNODEK)
      COSNOK = COS(XNODEK)
 
      XMX    = -SINNOK * COSIK
      XMY    =  COSNOK * COSIK
 
      UX     =  XMX    * SINUK   +   COSNOK * COSUK
      UY     =  XMY    * SINUK   +   SINNOK * COSUK
      UZ     =  SINIK  * SINUK
      VX     =  XMX    * COSUK   -   COSNOK * SINUK
      VY     =  XMY    * COSUK   -   SINNOK * SINUK
      VZ     =  SINIK  * COSUK
C
C     Position and velocity
C
      STATE(1) = TOKM    *   RK    * UX
      STATE(2) = TOKM    *   RK    * UY
      STATE(3) = TOKM    *   RK    * UZ
      STATE(4) = TOKMPS  * ( RDOTK * UX     +   RFDOTK * VX )
      STATE(5) = TOKMPS  * ( RDOTK * UY     +   RFDOTK * VY )
      STATE(6) = TOKMPS  * ( RDOTK * UZ     +   RFDOTK * VZ )
 
      RETURN
      END
