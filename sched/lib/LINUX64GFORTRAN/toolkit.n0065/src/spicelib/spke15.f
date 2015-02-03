C$Procedure      SPKE15 ( Evaluate a type 15 SPK data record)
 
      SUBROUTINE SPKE15 ( ET, RECIN, STATE )
 
C$ Abstract
C
C     Evaluates a single SPK data record from a segment of type 15
C    (Precessing Conic Propagation).
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
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECIN  ( * )
      DOUBLE PRECISION      STATE  ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Target epoch.
C     RECIN      I   Data record.
C     STATE      O   State (position and velocity).
C
C$ Detailed_Input
C
C     ET          is a target epoch, specified as ephemeris seconds past
C                 J2000, at which a state vector is to be computed.
C
C     RECIN       is a data record which, when evaluated at epoch ET,
C                 will give the state (position and velocity) of some
C                 body, relative to some center, in some inertial
C                 reference frame.
C
C                 The structure of RECIN is:
C
C                 RECIN(1)             epoch of periapsis
C                                      in ephemeris seconds past J2000.
C                 RECIN(2)-RECIN(4)    unit trajectory pole vector
C                 RECIN(5)-RECIN(7)    unit periapsis vector
C                 RECIN(8)             semi-latus rectum---p in the
C                                      equation:
C
C                                      r = p/(1 + ECC*COS(Nu))
C
C                 RECIN(9)             eccentricity
C                 RECIN(10)            J2 processing flag describing
C                                      what J2 corrections are to be
C                                      applied when the orbit is
C                                      propagated.
C
C                                      All J2 corrections are applied
C                                      if this flag has a value that
C                                      is not 1,2 or 3.
C
C                                      If the value of the flag is 3
C                                      no corrections are done.
C
C                                      If the value of the flag is 1
C                                      no corrections are computed for
C                                      the precession of the line
C                                      of apsides.  However, regression
C                                      of the line of nodes is
C                                      performed.
C
C                                      If the value of the flag is 2
C                                      no corrections are done for
C                                      the regression of the line of
C                                      nodes. However, precession of the
C                                      line of apsides is performed.
C
C                                      Note that J2 effects are computed
C                                      only if the orbit is elliptic and
C                                      does not intersect the central
C                                      body.
C
C                 RECIN(11)-RECIN(13)  unit central body pole vector
C                 RECIN(14)            central body GM
C                 RECIN(15)            central body J2
C                 RECIN(16)            central body radius
C
C                 Units are radians, km, seconds
C
C$ Detailed_Output
C
C     STATE       is the state produced by evaluating RECIN at ET.
C                 Units are km and km/sec.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     1) If the eccentricity is less than zero, the error
C        'SPICE(BADECCENTRICITY)' will be signalled.
C
C     2) If the semi-latus rectum is non-positive, the error
C        'SPICE(BADLATUSRECTUM)' is signalled.
C
C     3) If the pole vector, trajectory pole vector or periapsis vector
C        has zero length, the error 'SPICE(BADVECTOR)' is signalled.
C
C     4) If the trajectory pole vector and the periapsis vector are
C        not orthogonal, the error 'SPICE(BADINITSTATE)' is
C        signalled.  The test for orthogonality is very crude.  The
C        routine simply checks that the absolute value of the dot
C        product of the unit vectors parallel to the trajectory pole
C        and periapse vectors is less than 0.00001.  This check is
C        intended to catch blunders, not to enforce orthogonality to
C        double precision tolerance.
C
C     5) If the mass of the central body is non-positive, the error
C       'SPICE(NONPOSITIVEMASS)' is signalled.
C
C     6) If the radius of the central body is negative, the error
C       'SPICE(BADRADIUS)' is signalled.
C
C$ Particulars
C
C     This algorithm applies J2 corrections for precessing the
C     node and argument of periapse for an object orbiting an
C     oblate spheroid.
C
C     Note the effects of J2 are incorporated only for elliptic
C     orbits that do not intersect the central body.
C
C     While the derivation of the effect of the various harmonics
C     of gravitational field are beyond the scope of this header
C     the effect of the J2 term of the gravity model are as follows
C
C
C        The line of node precesses. Over one orbit average rate of
C        precession,  DNode/dNu,  is given by
C
C                                3 J2
C              dNode/dNu =  -  -----------------  DCOS( inc )
C                                2 (P/RPL)**2
C
C        (Since this is always less than zero for oblate spheroids, this
C           should be called regression of nodes.)
C
C        The line of apsides precesses. The average rate of precession
C        DPeri/dNu is given by
C                                   3 J2
C              dPeri/dNu =     ----------------- ( 5*DCOS ( inc ) - 1 )
C                                2 (P/RPL)**2
C
C        Details of these formulae are given in the Battin's book (see
C        literature references below).
C
C
C     It is assumed that this routine is used in conjunction with
C     the routine SPKR15 as shown here:
C
C        CALL SPKR15 ( HANDLE, DESCR, ET, RECIN         )
C        CALL SPKE15 (                ET, RECIN, STATE  )
C
C     where it is known in advance that the HANDLE, DESCR pair points
C     to a type 15 data segment.
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
C           IF ( TYPE .EQ. 15 ) THEN
C
C              CALL SPKR15 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE15 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
C                  .
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      S.   Schlaifer  (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C     [1] `Fundamentals of Celestial Mechanics', Second Edition 1989
C         by J.M.A. Danby;  Willman-Bell, Inc., P.O. Box 35025
C         Richmond Virginia;  pp 345-347.
C
C     [2] `Astronautical Guidance', by Richard H. Battin. 1964
C          McGraw-Hill Book Company, San Francisco.  pp 199
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHAT, VROTV, and VSCL calls.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
C
C-&
 
C$ Index_Entries
C
C     evaluate type_15 spk segment
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHAT, VROTV, and VSCL calls.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an 
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.0, 15-NOV-1994 (WLT) (SS)
C
C-&
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      DPR
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP
      LOGICAL               RETURN
      LOGICAL               VZERO
 
C
C     Local Variables
C
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      COSINC
      DOUBLE PRECISION      DMDT
      DOUBLE PRECISION      DNODE
      DOUBLE PRECISION      DOT
      DOUBLE PRECISION      DPERI
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      K2PI
      DOUBLE PRECISION      MANOM
      DOUBLE PRECISION      NEAR
      DOUBLE PRECISION      OJ2
      DOUBLE PRECISION      ONEME2
      DOUBLE PRECISION      P
      DOUBLE PRECISION      PA     ( 3 )
      DOUBLE PRECISION      PV     ( 3 )
      DOUBLE PRECISION      RPL
      DOUBLE PRECISION      SPEED
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      TA
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TMPSTA ( 6 )
      DOUBLE PRECISION      TP     ( 3 )
      DOUBLE PRECISION      Z
 
      INTEGER               J2FLG
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SPKE15')
 
C
C     Fetch the various entities from the input record, first the epoch.
C
      EPOCH  = RECIN(1)
 
C
C     The trajectory pole vector.
C
      CALL VEQU ( RECIN(2), TP )
 
C
C     The periapsis vector.
C
      CALL VEQU ( RECIN(5), PA )
 
C
C     Semi-latus rectum ( P in the P/(1 + ECC*COS(Nu)  ),
C     and eccentricity.
C
      P      = RECIN(8)
      ECC    = RECIN(9)
 
C
C     J2 processing flag.
C
      J2FLG  = INT( RECIN(10) )
 
C
C     Central body pole vector.
C
      CALL VEQU ( RECIN(11), PV )
 
C
C     The central mass, J2 and radius of the central body.
C
      GM     = RECIN(14)
      OJ2    = RECIN(15)
      RPL    = RECIN(16)
 
C
C     Check all the inputs here for obvious failures.  Yes, perhaps
C     this is overkill.  However, there is a lot more computation
C     going on in this routine so that the small amount of overhead
C     here should not be significant.
C
 
 
      IF ( P .LE. 0 ) THEN
 
         CALL SETMSG ( 'The semi-latus rectum supplied to the '
     .   //            'SPK type 15 evaluator was non-positive.  This '
     .   //            'value must be positive. The value supplied was '
     .   //            '#.'                    )
         CALL ERRDP  ( '#', P                  )
         CALL SIGERR ( 'SPICE(BADLATUSRECTUM)' )
         CALL CHKOUT ( 'SPKE15'                )
         RETURN
 
      ELSE IF ( ECC .LT. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The eccentricity supplied for a type 15 '
     .   //            'segment is negative.  It must be non-negative. '
     .   //            'The value supplied '
     .   //            'to the type 15 evaluator was #. ' )
         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'SPKE15'                  )
         RETURN
 
      ELSE IF ( GM  .LE. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The mass supplied for the central body '
     .   //            'of a type 15 segment was non-positive. '
     .   //            'Masses must be positive.  The value '
     .   //            'supplied was #. '        )
         CALL ERRDP  ( '#', GM                   )
         CALL SIGERR ( 'SPICE(NONPOSITIVEMASS)'  )
         CALL CHKOUT ( 'SPKE15'                  )
         RETURN
 
      ELSE IF ( VZERO(TP) ) THEN
 
         CALL SETMSG ( 'The trajectory pole vector supplied to '
     .   //            'SPKE15 had length zero. The most likely '
     .   //            'cause of this problem is a corrupted SPK '
     .   //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE15'             )
         RETURN
 
      ELSE IF ( VZERO(PA) ) THEN
 
         CALL SETMSG ( 'The periapse vector supplied to SPKE15 '
     .   //            'had length zero. The most likely cause '
     .   //            'of this problem is a corrupted SPK '
     .   //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE15'             )
         RETURN
 
      ELSE IF ( VZERO(PV) ) THEN
 
         CALL SETMSG ( 'The central pole vector supplied to '
     .   //            'SPKE15 had length zero. The most likely '
     .   //            'cause of this problem is a corrupted SPK '
     .   //            '(ephemeris) file. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'SPKE15'             )
         RETURN
 
 
      ELSE IF ( RPL .LT. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The central body radius was negative. '
     .   //            'It must be zero or positive.  The value '
     .   //            'supplied was #. '  )
         CALL ERRDP  ( '#', RPL            )
         CALL SIGERR ( 'SPICE(BADRADIUS)'  )
         CALL CHKOUT ( 'SPKE15'            )
         RETURN
 
      END IF
 
 
C
C     Convert TP, PV and PA to unit vectors.
C     (It won't hurt to polish them up a bit here if they are already
C      unit vectors.)
C
      CALL VHATIP ( PA )
      CALL VHATIP ( TP )
      CALL VHATIP ( PV )

C
C     One final check.  Make sure the pole and periapse vectors are
C     orthogonal. (We will use a very crude check but this should
C     rule out any obvious errors.)
C
      DOT = VDOT ( PA, TP )
 
      IF ( ABS(DOT) .GT. 1.0D-5 ) THEN
 
         ANGLE = VSEP ( PA, TP ) * DPR()
 
         CALL SETMSG ( 'The periapsis and trajectory pole '
     .   //            'vectors are not orthogonal. The angle'
     .   //            'between them is # degrees. '         )
         CALL ERRDP  ( '#',     ANGLE        )
         CALL SIGERR ( 'SPICE(BADINITSTATE)' )
         CALL CHKOUT ( 'SPKE15'              )
         RETURN
 
      END IF
 
C
C     Compute the distance and speed at periapse.
C
      NEAR  = P           / ( 1.0D0 + ECC)
      SPEED = DSQRT(GM/P) * ( 1.0D0 + ECC )
 
C
C     Next get the position at periapse ...
C
      CALL VSCL( NEAR, PA, STATE0 )
 
C
C     ... and the velocity at periapsis.
C
      CALL VCRSS  ( TP,    PA, STATE0(4) )
      CALL VSCLIP ( SPEED,     STATE0(4) )
 
C
C     Determine the elapsed time from periapse to the requested
C     epoch and propagate the state at periapsis to the epoch of
C     interest.
C
C     Note that we are making use of the following fact.
C
C        If R is a rotation, then the states obtained by
C        the following blocks of code are mathematically the
C        same. (In reality they may differ slightly due to
C        roundoff.)
C
C        Code block 1.
C
C           CALL MXV   ( R,  STATE0,     STATE0    )
C           CALL MXV   ( R,  STATE0(4),  STATE0(4) )
C           CALL PROP2B( GM, STATE0, DT, STATE     )
C
C        Code block 2.
C
C           CALL PROP2B( GM, STATE0, DT, STATE    )
C           CALL MXV   ( R,  STATE,      STATE    )
C           CALL MXV   ( R,  STATE(4),   STATE(4) )
C
C
C     This allows us to first compute the propagation of our initial
C     state and then if needed perform the precession of the line
C     of nodes and apsides by simply precessing the resulting state.
C
      DT = ET - EPOCH
      CALL PROP2B ( GM, STATE0, DT, STATE )
 
C
C     If called for, handle precession needed due to the J2 term.  Note
C     that the motion of the lines of nodes and apsides is formulated
C     in terms of the true anomaly.  This means we need the accumulated
C     true anomaly in order to properly transform the state.
C
      IF (       ( J2FLG .NE. 3     )
     .     .AND. ( OJ2   .NE. 0.0D0 )
     .     .AND. ( ECC   .LT. 1.0D0 )
     .     .AND. ( NEAR  .GT. RPL   )  ) THEN
  
C
C        First compute the change in mean anomaly since periapsis.
C
         ONEME2 = 1.0D0 - ECC**2
         DMDT   = ( ONEME2 / P ) * DSQRT( GM * ONEME2 / P )
         MANOM  = DMDT*DT
C
C        Next compute the angle THETA such that THETA is between
C        -pi and pi and such than MANOM = THETA + K*2*pi for
C        some integer K.
C
         THETA  = MOD( MANOM, TWOPI() )
 
         IF ( ABS(THETA).GT. PI() ) THEN
            THETA = THETA - SIGN(TWOPI(),THETA)
         END IF
 
         K2PI   = MANOM - THETA
C
C        We can get the accumulated true anomaly from the propagated
C        state theta and the accumulated mean anomaly prior to this
C        orbit.
C
         TA     = VSEP ( PA, STATE )
         TA     = SIGN ( TA, THETA )
         TA     = TA  +  K2PI
 
C
C        Determine how far the line of nodes and periapsis have moved.
C
         COSINC = VDOT(PV,TP)
 
         Z      = TA * 1.5D0 * OJ2 *(RPL/P)**2
         DNODE  = -Z * COSINC
         DPERI  =  Z * ( 2.5D0*COSINC**2 - 0.5D0 )
 
C
C        Precess the periapsis by rotating the state vector about the
C        trajectory pole
C
         IF ( J2FLG .NE. 1 ) THEN

            CALL VROTV ( STATE(1), TP, DPERI, TMPSTA(1) )
            CALL VROTV ( STATE(4), TP, DPERI, TMPSTA(4) )
            CALL MOVED ( TMPSTA,   6,         STATE     )

         END IF
 
C
C        Regress the line of nodes by rotating the state
C        about the pole of the central body.
C
         IF ( J2FLG .NE. 2 ) THEN

            CALL VROTV ( STATE,    PV, DNODE, TMPSTA(1) )
            CALL VROTV ( STATE(4), PV, DNODE, TMPSTA(4) )
            CALL MOVED ( TMPSTA,   6,         STATE     )

         END IF
 
C
C        We could perform the rotations above in the other order,
C        but we would also have to rotate the pole before precessing
C        the line of apsides.
C
 
      END IF
 
C
C     That's all folks.  Check out and return.
C
      CALL CHKOUT('SPKE15')
      RETURN
      END
 
 
 
