C$Procedure      CONICS ( Determine state from conic elements )
 
      SUBROUTINE CONICS ( ELTS, ET, STATE )
 
C$ Abstract
C
C     Determine the state (position, velocity) of an orbiting body
C     from a set of elliptic, hyperbolic, or parabolic orbital
C     elements.
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
C     CONIC
C     EPHEMERIS
C
C$ Declarations
 
      DOUBLE PRECISION      ELTS    ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      STATE   ( 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ELTS       I   Conic elements.
C     ET         I   Input time.
C     STATE      O   State of orbiting body at ET.
C
C$ Detailed_Input
C
C     ELTS       are conic elements describing the orbit of a body
C                around a primary. The elements are, in order:
C
C                      RP      Perifocal distance.
C                      ECC     Eccentricity.
C                      INC     Inclination.
C                      LNODE   Longitude of the ascending node.
C                      ARGP    Argument of periapse.
C                      M0      Mean anomaly at epoch.
C                      T0      Epoch.
C                      MU      Gravitational parameter.
C
C                Units are km, rad, rad/sec, km**3/sec**2.  The epoch
C                is given in ephemeris seconds past J2000. The same
C                elements are used to describe all three types
C                (elliptic, hyperbolic, and parabolic) of conic orbit.
C
C     ET         is the time at which the state of the orbiting body
C                is to be determined, in ephemeris seconds J2000.
C
C$ Detailed_Output
C
C     STATE      is the state (position and velocity) of the body at
C                time ET. Components are x, y, z, dx/dt, dy/dt, dz/dt.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the eccentricity supplied is less than 0, the error
C        'SPICE(BADECCENTRICITY)' is signalled.
C
C     2) If a non-positive periapse distance is supplied, the error
C       'SPICE(BADPERIAPSEVALUE)' is signalled.
C
C     3) If a non-positive value for the attracting mass is supplied,
C        the error 'SPICE(BADGM)',  is signalled.
C
C     4) Errors such as an out of bounds value for ET are diagnosed
C        by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     Let VINIT contain the initial state of a spacecraft relative to
C     the center of a planet at epoch ET, and let GM be the gravitation
C     parameter of the planet. The call
C
C        CALL OSCELT ( VINIT, ET, GM, ELTS )
C
C     produces a set of osculating elements describing the nominal
C     orbit that the spacecraft would follow in the absence of all
C     other bodies in the solar system and non-gravitational forces
C     on the spacecraft.
C
C     Now let STATE contain the state of the same spacecraft at some
C     other epoch, LATER. The difference between this state and the
C     state predicted by the nominal orbit at the same epoch can be
C     computed as follows.
C
C        CALL CONICS ( ELTS, LATER, NOMINAL )
C        CALL VSUBG  ( NOMINAL, STATE, 6, DIFF )
C
C        WRITE (*,*) 'Perturbation in x, dx/dt = ', DIFF(1), DIFF(4)
C        WRITE (*,*) '                y, dy/dt = ', DIFF(2), DIFF(5)
C        WRITE (*,*) '                z, dz/dt = ', DIFF(3), DIFF(6)
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] Roger Bate, Fundamentals of Astrodynamics, Dover, 1971.
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.0, 26-MAR-1998 (WLT)
C
C        There was a coding error in the computation of the mean
C        anomaly in the parabolic case.  This problem has been
C        corrected.
C
C-    SPICELIB Version 3.0.1, 15-OCT-1996 (WLT)
C
C        Corrected a typo in the description of the units associated
C        with the input elements.
C
C-    SPICELIB Version 3.0.0, 12-NOV-1992 (WLT)
C
C        The routine was re-written to make use of NAIF's universal
C        variables formulation for state propagation (PROP2B).  As
C        a result, several problems were simultaneously corrected.
C
C        A major bug was fixed that caused improper state evaluations
C        for ET's that precede the epoch of the elements in the
C        elliptic case.
C
C        A danger of non-convergence in the solution of Kepler's
C        equation has been eliminated.
C
C        In addition to this reformulation of CONICS checks were
C        installed that ensure the elements supplied are physically
C        meaningful.  Eccentricity must be non-negative. The
C        distance at periapse and central mass must be positive.  If
C        not errors are signalled.
C
C-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
C
C        An error in the hyperbolic state generation was corrected.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     state from conic elements
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 3.0.1, 15-OCT-1996 (WLT)
C
C        Corrected a typo in the description of the units associated
C        with the input elements.
C
C-    SPICELIB Version 3.0.0, 12-NOV-1992 (WLT)
C
C        The routine was re-written to make use of NAIF's universal
C        variables formulation for state propagation (PROP2B).  As
C        a result, several problems were simultaneously corrected.
C
C        A major bug was fixed that caused improper state evaluations
C        for ET's that precede the epoch of the elements in the
C        elliptic case.
C
C        A danger of non-convergence in the solution of Kepler's
C        equation has been eliminated.
C
C        In addition to this reformulation of CONICS checks were
C        installed that ensure the elements supplied are physically
C        meaningful.  Eccentricity must be non-negative. The
C        distance at periapse and central mass must be positive.  If
C        not errors are signalled.
C
C        These changes were prompted by the discovery that the old
C        formulation had a severe bug for elliptic orbits and epochs
C        prior to the epoch of the input elements, and by the discovery
C        that the time of flight routines had problems with convergence.
C
C-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
C
C        The original version of the routine had a bug in that
C        it attempted to restrict the hyperbolic anomaly to
C        the interval 0 to 2*PI.  This has been fixed.
C
C-    Beta Version 1.0.1, 27-JAN-1989 (IMU)
C
C        Examples section completed.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      TWOPI
 
C
C     Local variables
C
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      INC
      DOUBLE PRECISION      LNODE
      DOUBLE PRECISION      ARGP
      DOUBLE PRECISION      M0
      DOUBLE PRECISION      T0
      DOUBLE PRECISION      MU
      DOUBLE PRECISION      N
      DOUBLE PRECISION      PERIOD
 
      DOUBLE PRECISION      AINVRS
      DOUBLE PRECISION      DT
 
      DOUBLE PRECISION      COSI
      DOUBLE PRECISION      SINI
      DOUBLE PRECISION      COSN
      DOUBLE PRECISION      SINN
      DOUBLE PRECISION      COSW
      DOUBLE PRECISION      SINW
 
      DOUBLE PRECISION      SNCI
      DOUBLE PRECISION      CNCI
 
      DOUBLE PRECISION      BASISP     ( 3 )
      DOUBLE PRECISION      BASISQ     ( 3 )
      DOUBLE PRECISION      PSTATE     ( 6 )
      DOUBLE PRECISION      V
 
 
 
C
C      The only real work required by this routine is the construction
C      of a preliminary state vector from the input elements.  Once this
C      is in hand, we can simply let the routine PROP2B do the real
C      work, free from the instabilities inherent in the classical
C      elements formulation of two-body motion.
C
C      To do this we shall construct a basis of vectors that lie in the
C      plane of the orbit.  The first vector P shall point towards the
C      position of the orbiting body at periapse.  The second
C      vector Q shall point along the velocity vector of the body at
C      periapse.
C
C      The only other consideration is determining an epoch, TP, of
C      this state and the delta time ET - TP.
C
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CONICS' )
      END IF
 
 
C
C     Unpack the element vector.
C
      RP     = ELTS(1)
      ECC    = ELTS(2)
      INC    = ELTS(3)
      LNODE  = ELTS(4)
      ARGP   = ELTS(5)
      M0     = ELTS(6)
      T0     = ELTS(7)
      MU     = ELTS(8)
 
 
C
C     Handle all of the exceptions first.
C
 
      IF ( ECC .LT. 0 ) THEN
         CALL SETMSG ( 'The eccentricity supplied was negative. Only '//
     .                 'positive values are meaningful.  The value '  //
     .                 'was #'                  )
         CALL ERRDP  ( '#', ECC                 )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)' )
         CALL CHKOUT ( 'CONICS'                 )
         RETURN
      END IF
 
      IF ( RP .LE. 0 ) THEN
         CALL SETMSG ( 'The value of periapse range supplied was '    //
     .                 'non-positive.  Only positive values are '     //
     .                 'allowed.  The value supplied was #. '         )
         CALL ERRDP  ( '#', RP                                        )
         CALL SIGERR ( 'SPICE(BADPERIAPSEVALUE)'                      )
         CALL CHKOUT ( 'CONICS'                                       )
         RETURN
      END IF
 
 
      IF ( MU .LE. 0 ) THEN
         CALL SETMSG ( 'The value of GM supplied was '                //
     .                 'non-positive.  Only positive values are '     //
     .                 'allowed.  The value supplied was #. '         )
         CALL ERRDP  ( '#', MU                                        )
         CALL SIGERR ( 'SPICE(BADGM)'                                 )
         CALL CHKOUT ( 'CONICS' )
         RETURN
      END IF
 
C
C     First construct the orthonormal basis vectors that span the orbit
C     plane.
C
      COSI      =  DCOS ( INC   )
      SINI      =  DSIN ( INC   )
      COSN      =  DCOS ( LNODE )
      SINN      =  DSIN ( LNODE )
      COSW      =  DCOS ( ARGP  )
      SINW      =  DSIN ( ARGP  )
 
      SNCI      =  SINN * COSI
      CNCI      =  COSN * COSI
 
      BASISP(1) =  COSN * COSW  -  SNCI * SINW
      BASISP(2) =  SINN * COSW  +  CNCI * SINW
      BASISP(3) =  SINI * SINW
 
      BASISQ(1) = -COSN * SINW  -  SNCI * COSW
      BASISQ(2) = -SINN * SINW  +  CNCI * COSW
      BASISQ(3) =  SINI * COSW
 
C
C     Next construct the state at periapse.
C
C     The position at periapse is just BASISP scaled by the distance
C     at periapse.
C
C     The velocity must be constructed so that we can get an orbit
C     of this shape.  Recall that the magnitude of the specific angular
C     momentum vector is given by DSQRT ( MU*RP*(1+ECC) )
C     The velocity will be given by V * BASISQ.  But we must have the
C     magnitude of the cross product of position and velocity be
C     equal to DSQRT ( MU*RP*(1+ECC) ). So we must have
C
C        RP*V = DSQRT( MU*RP*(1+ECC) )
C
C     so that:
C
      V = DSQRT( MU*(1.0D0 + ECC) / RP )
 
      CALL VSCL( RP, BASISP, PSTATE(1) )
      CALL VSCL( V,  BASISQ, PSTATE(4) )
 
C
C     Finally compute DT the elapsed time since the epoch of periapse.
C     Ellipses first, since they are the most common.
C
      IF ( ECC .LT. 1.D0 ) THEN
 
C
C        Recall that:
C
C        N ( mean motion ) is given by DSQRT( MU / A**3 ).
C        But since, A = RP / ( 1 - ECC ) ...
C
         AINVRS = (1.0D0 - ECC)   / RP
         N      = SQRT(MU*AINVRS) * AINVRS
         PERIOD = TWOPI()         / N
 
C
C        In general the mean anomaly is given by
C
C           M  = (T - TP) * N
C
C        Where TP is the time of periapse passage.  M0 is the mean
C        anomaly at time T0 so that
C        Thus
C
C           M0 = ( T0 - TP ) * N
C
C        So TP = T0-M0/N hence the time since periapse at time ET
C        is given by ET - T0 + M0/N.  Finally, since elliptic orbits are
C        periodic, we can mod this value by the period of the orbit.
C
         DT = MOD ( ET - T0 + M0/N, PERIOD )
 
C
C     Hyperbolas next.
C
      ELSE IF ( ECC .GT. 1 ) THEN
 
 
C
C        Again, recall that:
C
C        N ( mean motion ) is given by DSQRT( MU / |A**3| ).
C        But since, |A| = RP / ( ECC - 1 ) ...
C
         AINVRS = (ECC - 1.0D0)    / RP
         N      = DSQRT(MU*AINVRS) * AINVRS
         DT     = ET - T0          + M0/N
 
C
C     Finally, parabolas.
C
      ELSE
 
         N  = DSQRT( MU / (2.0D0*RP) ) / RP
         DT = ET - T0                + M0/N
 
      END IF
 
C
C     Now let PROP2B do the work of propagating the state.
C
      CALL PROP2B ( MU, PSTATE, DT, STATE )
 
      CALL CHKOUT ( 'CONICS' )
      RETURN
      END
