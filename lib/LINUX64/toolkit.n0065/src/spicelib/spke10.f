C$Procedure SPKE10 ( Evaluate SPK record, type 10 )
 
      SUBROUTINE SPKE10 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single SPK data record from a segment of type 10
C     (NORAD two-line element sets.).
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
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Target epoch.
C     RECORD     I   Data record.
C     STATE      O   State (position and velocity).
C
C$ Detailed_Input
C
C     ET          is a target epoch, specified as ephemeris seconds past
C                 J2000, at which a state vector is to be computed.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of some
C                 body, relative to some center, in some inertial
C                 reference frame.
C
C                 The structure of RECORD is:
C
C                     RECORD(1)
C                        .            Geophysical Constants such as
C                        .            GM, J2, J3, J4, etc.
C                        .
C                     RECORD(NGEOCN)
C
C                     RECORD(NGEOCN + 1)
C                        .
C                        .            elements and epoch for the body
C                        .            at epoch 1.
C                        .
C                     RECORD(NGEOCN + NELEMN )
C
C                     RECORD(NGEOCN + NELEMN + 1)
C                        .
C                        .            elements and epoch for the body
C                        .            at epoch 2.
C                        .
C                     RECORD(NGEOCN + 2*NELEMN )
C
C                 Epoch 1 and epoch 2 are the times in the segment that
C                 bracket ET.  If ET is less than the first time in the
C                 segment then both epochs 1 and 2 are equal to the
C                 first time.  And if ET is greater than the last time
C                 then, epochs 1 and 2 are set equal to this last time.
C
C$ Detailed_Output
C
C     STATE       is the state produced by evaluating RECORD at ET.
C                 Units are km and km/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If there is a problem evaluating the two-line elements,
C     the error will be diagnosed by EV2LIN.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine interpolates a state from the two reference sets
C     of two-line element sets contained in RECORD.
C
C     It is assumed that this routine is used in conjunction with
C     the routine SPKR10 as shown here:
C
C        CALL SPKR10 ( HANDLE, DESCR, ET, RECORD         )
C        CALL SPKE10 (                ET, RECORD, STATE  )
C
C     Where it is known in advance that the HANDLE, DESCR pair points
C     to a type 10 data segment.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRnn
C     routines might be used to examine raw segment data before
C     evaluating it with the SPKEnn routines.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 10 ) THEN
C
C              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE10 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 01-JAN-2011 (EDW)
C
C        Correction of state transformation calculation. Algorithm
C        now computes state transformation as from TEME to J2000.
C        The previous version of this routine calculated TETE to
C        J2000.
C
C-    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in MTXV and VADD calls.
C
C-    SPICELIB Version 1.0.0 18-JUL-1997 (WLT)
C
C-&
 
C$ Index_Entries
C
C     evaluate type_10 spk segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
 
 
C
C     Local Parameters
C
      INTEGER               START
      PARAMETER           ( START  = 0 )
 
C
C
C     The following parameters give the location of the various
C     geophysical parameters needed for the two line element
C     sets.  We need these only so that we can count how many there
C     are (NGEOCN).
C
C     KJ2  --- location of J2
C     KJ3  --- location of J3
C     KJ4  --- location if J4
C     KKE  --- location of KE = sqrt(GM) in earth-radii**1.5/MIN
C     KQO  --- upper bound of atmospheric model in KM
C     KSO  --- lower bound of atmospheric model in KM
C     KER  --- earth equatorial radius in KM.
C     KAE  --- distance units/earth radius
C
      INTEGER               KJ2
      PARAMETER           ( KJ2    = START  + 1 )
 
      INTEGER               KJ3
      PARAMETER           ( KJ3    = KJ2    + 1 )
 
      INTEGER               KJ4
      PARAMETER           ( KJ4    = KJ3    + 1 )
 
      INTEGER               KKE
      PARAMETER           ( KKE    = KJ4    + 1 )
 
      INTEGER               KQO
      PARAMETER           ( KQO    = KKE    + 1 )
 
      INTEGER               KSO
      PARAMETER           ( KSO    = KQO    + 1 )
 
      INTEGER               KER
      PARAMETER           ( KER    = KSO    + 1 )
 
      INTEGER               KAE
      PARAMETER           ( KAE    = KER    + 1 )
 
      INTEGER               NGEOCN
      PARAMETER           ( NGEOCN = KAE )

C
C     An enumeration of the various components of the
C     a two-line element set.  These are needed so that we
C     can locate the epochs in the two sets and so that
C     we can count the number of elements in a two-line
C     element set.
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
C     KEPOCH
C
 
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
C     The nutation in obliquity and longitude as well as their rates
C     follow the elements.  So we've got four angles/angle rates
C     following the elements
C
      INTEGER               NANGS
      PARAMETER           ( NANGS = 4 )
 
C
C     The locations of the epochs and the starts of the element
C     sets are given below.
C
      INTEGER               TIME1
      PARAMETER           ( TIME1  = NGEOCN + KEPOCH )
 
      INTEGER               TIME2
      PARAMETER           ( TIME2  = NELEMS + NANGS + TIME1  )
 
      INTEGER               MODEL1
      PARAMETER           ( MODEL1 = NGEOCN + 1      )
 
      INTEGER               ANGS1
      PARAMETER           ( ANGS1 =  MODEL1 + NELEMS )
 
      INTEGER               MODEL2
      PARAMETER           ( MODEL2 = ANGS1 + NANGS )
 
      INTEGER               MEANMO
      PARAMETER           ( MEANMO = NGEOCN + KNO )
 

C
C     Local variables
C
      DOUBLE PRECISION      INVPRC   ( 6, 6 )
      DOUBLE PRECISION      PRECM    ( 6, 6 )
      DOUBLE PRECISION      S1       ( 6 )
      DOUBLE PRECISION      S2       ( 6 )
      DOUBLE PRECISION      TMPSTA   ( 6 )
      DOUBLE PRECISION      VCOMP    ( 3 )
      DOUBLE PRECISION      ARG
      DOUBLE PRECISION      DARGDT
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      DWDT
      DOUBLE PRECISION      MNRATE
      DOUBLE PRECISION      MY2PI
      DOUBLE PRECISION      MYPI
      DOUBLE PRECISION      N0
      DOUBLE PRECISION      NUMER
      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      DOUBLE PRECISION      W
 
      LOGICAL               FIRST
      LOGICAL               LOWORB
      SAVE
 
      DATA                  FIRST /.TRUE./
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKE10' )
      END IF
 
      IF ( FIRST ) THEN
         FIRST  = .FALSE.
         MYPI   = PI()
         MY2PI  = TWOPI()
      END IF
 
C
C     Fetch the mean motion from the first set of two-line elements
C     stored in the record.
C
      N0     = RECORD(MEANMO)
      MNRATE = MY2PI / 225.0D0
      LOWORB = N0 .GE. MNRATE
 
C
C     Fetch the two epochs stored in the record.
C
      T1 = RECORD(TIME1)
      T2 = RECORD(TIME2)
 
C
C     Evaluate the two states. Call them s_1(t) and s_2(t).
C     Let the position and velocity components be: p_1, v_1, p_2, v_2.
C
C     The final position is a weighted average.
C
C     Let
C
C        W(t) =  0.5 + 0.5*COS( PI*(t-t1)/(t2-t1) )
C
C     then
C
C        p  = W(t)*p_1(t) + (1 - W(t))*p_2(t)
C        v  = W(t)*v_1(t) + (1 - W(t))*v_2(t) + W'(t)*(p_1(t) - p_2(t))
C
C     If t1 = t2, the state is just s(t1).
C
C
C     Note: there are a number of weighting schemes we could have
C     used.  This one has the nice property that
C
C     The graph of W is symmetric about the point
C
C
C        ( (t1+t2)/2,  W( (t1+t2)/2 ) )
C
C     The range of W is from 1 to 0. The derivative of W is
C     symmetric and zero at both t1 and t2.
C

      IF ( T1 .NE. T2 ) THEN
  
         IF ( LOWORB ) THEN
            CALL EV2LIN ( ET, RECORD(1), RECORD(MODEL1), S1 )
            CALL EV2LIN ( ET, RECORD(1), RECORD(MODEL2), S2 )
         ELSE
            CALL DPSPCE ( ET, RECORD(1), RECORD(MODEL1), S1 )
            CALL DPSPCE ( ET, RECORD(1), RECORD(MODEL2), S2 )
         END IF

C
C        Compute the weighting function that we'll need later
C        when we combine states 1 and 2.
C
         NUMER  = ET - T1
         DENOM  = T2 - T1
         ARG    = NUMER*MYPI/DENOM
         DARGDT =       MYPI/DENOM
 
         W      =  0.5D0 + 0.5D0 * COS( ARG )
         DWDT   =        - 0.5D0 * SIN( ARG ) * DARGDT

C
C        Now compute the weighted average of the two states.
C
         CALL VLCOMG ( 6,  W,    S1,       1.D0-W, S2,     STATE     )
         CALL VLCOM  (     DWDT, S1,       -DWDT,  S2,     VCOMP     )
         CALL VADD   (           STATE(4),         VCOMP,  TMPSTA(4) )
         CALL VEQU   (           TMPSTA(4),                STATE(4)  )
          
      ELSE

         IF ( LOWORB ) THEN
            CALL EV2LIN ( ET, RECORD(1), RECORD(MODEL1), STATE )
         ELSE
            CALL DPSPCE ( ET, RECORD(1), RECORD(MODEL1), STATE )
         END IF
 
      END IF

C
C     Finally, convert the TEME state to J2000.  First get
C     the rotation from J2000 to TEME...
C
      CALL ZZTEME( ET, PRECM )

C
C     ...now convert STATE to J2000. Invert the state transformation
C     operator (important to correctly do this).
C
      CALL INVSTM ( PRECM, INVPRC )

C
C     Map STATE to the corresponding expression in J2000.
C
      CALL MXVG   ( INVPRC, STATE, 6, 6, TMPSTA )
      CALL MOVED  ( TMPSTA,  6,      STATE     )
 
      CALL CHKOUT ( 'SPKE10' )
      RETURN

      END
