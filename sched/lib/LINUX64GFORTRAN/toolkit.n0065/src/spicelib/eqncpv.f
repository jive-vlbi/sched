C$Procedure      EQNCPV (Equinoctial Elements to position and velocity)
 
      SUBROUTINE EQNCPV ( ET, EPOCH, EQEL, RAPOL, DECPOL, STATE )
 
C$ Abstract
C
C     Compute the state (position and velocity of an object whose
C     trajectory is described via equinoctial elements relative to some
C     fixed plane (usually the equatorial plane of some planet).
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
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      EQEL ( 9 )
      DOUBLE PRECISION      RAPOL
      DOUBLE PRECISION      DECPOL
      DOUBLE PRECISION      STATE ( 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch in seconds past J2000 to find state
C     EPOCH      I   Epoch of elements in seconds past J2000
C     EQEL       I   Array of equinoctial elements
C     RAPOL      I   Right Ascension of the pole of the reference plane
C     DECPOL     I   Declination of the pole of the reference plane
C     STATE      O   State of the object described by EQEL.
C
C$ Detailed_Input
C
C     ET         is the epoch (ephemeris time) at which the state
C                of the target body is to be computed. ET is measured
C                in seconds past the J2000 epoch.
C
C     EPOCH      is the epoch of the equinoctial elements in seconds
C                past the J2000 epoch.
C
C     EQEL       is an array of 9 double precision numbers that
C                are the equinoctial elements for some orbit expressed
C                relative to the equatorial frame of the central body.
C                (The z-axis of the equatorial frame is the direction
C                of the pole of the central body relative to some
C                inertial frame.  The x-axis is given by the cross
C                product of the Z-axis of the inertial frame
C                with the direction of the pole of the central body.
C                The Y-axis completes a right handed frame.
C                (If the z-axis of the equatorial frame is aligned
C                with the z-axis of the inertial frame, then the
C                x-axis of the equatorial frame will be located at
C                90 degrees + RAPOL in the inertial frame.)
C
C                The specific arrangement of the elements is spelled
C                out below.  The following terms are used in the
C                discussion of elements of EQEL
C
C                    INC  --- inclination of the orbit
C                    ARGP --- argument of periapse
C                    NODE --- longitude of the ascending node
C                    E    --- eccentricity of the orbit
C
C                EQEL(1) is the semi-major axis (A) of the orbit in km.
C
C                EQEL(2) is the value of H at the specified epoch.
C                        ( E*SIN(ARGP+NODE) ).
C
C                EQEL(3) is the value of K at the specified epoch
C                        ( E*COS(ARGP+NODE) ).
C
C                EQEL(4) is the mean longitude (MEAN0+ARGP+NODE)at
C                        the epoch of the elements measured in radians.
C
C                EQEL(5) is the value of P (TAN(INC/2)*SIN(NODE))at
C                        the specified epoch.
C
C                EQEL(6) is the value of Q (TAN(INC/2)*COS(NODE))at
C                        the specified epoch.
C
C                EQEL(7) is the rate of the longitude of periapse
C                        (dARGP/dt + dNODE/dt ) at the epoch of
C                        the elements.  This rate is assumed to hold
C                        for all time. The rate is measured in
C                        radians per second.
C
C                EQEL(8) is the derivative of the mean longitude
C                        ( dM/dt + dARGP/dt + dNODE/dt ).  This
C                        rate is assumed to be constant and is
C                        measured in radians/second.
C
C                EQEL(9) is the rate of the longitude of the ascending
C                        node ( dNODE/dt).  This rate is measured
C                        in radians per second.
C
C     RAPOL      Right Ascension of the pole of the reference plane
C                with respect to some inertial frame (measured in
C                radians).
C
C     DECPOL     Declination of the pole of the reference plane
C                with respect to some inertial frame (measured in
C                radians).
C
C$ Detailed_Output
C
C     STATE      State of the object described by EQEL relative to the
C                inertial frame used to define RAPOL and DECPOL. Units
C                are in km and km/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the eccentricity corresponding to the input elements is
C        greater than 0.9, the error SPICE(ECCOUTOFRANGE) is signalled.
C
C     2) If the semi-major axis of the elements is non-positive, the
C        error SPICE(BADSEMIAXIS) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine evaluates the input equinoctial elements for
C     the specified epoch and return the corresponding state.
C
C     This routine was adapted from a routine provided by
C     Bob Jacobson of the Planetary Dynamics Group of
C     the Navigation and Flight Mechanics Section at JPL.
C
C$ Examples
C
C     Suppose you have classical elements and rates of
C     change of the ascending node and argument of periapse
C     for some satellite of the earth.
C
C     By transforming the classical elements
C     this routine can be used to compute the state of the
C     object at an arbitrary epoch.  The code below illustrates
C     how you might do this.
C
C     The table below illustrates the meanings of the various
C     variables used in the discussion below.
C
C           Variable     Meaning
C           --------     ----------------------------------
C           A            Semi-major axis in km
C           ECC          Eccentricity of orbit
C           INC          Inclination of orbit
C           NODE         Longitude of the ascending node at epoch
C           OMEGA        Argument of periapse at epoch
C           M            Mean anomaly at epoch
C           DMDT         Mean anomaly rate in radians/second
C           DNODE        Rate of change of longitude of ascending node
C                        in radians/second
C           DARGP        Rate of change of argument of periapse in
C                        radians/second
C           EPOCH        is the epoch of the elements in seconds past
C                        the J2000 epoch.
C
C
C        EQEL(1) = A
C        EQEL(2) = ECC * DSIN ( OMEGA + NODE )
C        EQEL(3) = ECC * DCOS ( OMEGA + NODE )
C
C        EQEL(4) = M + OMEGA + NODE
C
C        EQEL(5) = TAN(INC/2.0D0) * DSIN(NODE)
C        EQEL(6) = TAN(INC/2.0D0) * DCOS(NODE)
C
C        EQEL(7) = DARGP
C        EQEL(8) = DARGP + DMDT + DNODE
C        EQEL(9) = DNODE
C
C
C        We shall compute the state of the satellite in the
C        pole and equator reference system.
C
C        RAPOL   = -HALFPI()
C        DECPOL  =  HALFPI()
C
C
C        Now compute the state at the desired epoch ET.
C
C        CALL EQNCPV ( ET, EPOCH, EQEL, RAPOL, DECPOL, STATE )
C
C$ Restrictions
C
C     The equinoctial elements used by this routine are taken
C     from  "Tangent" formulation of equinoctial elements
C
C        p = Tan(inclination/2) * Sin(R.A. of ascending node)
C        q = Tan(inclination/2) * Cos(R.A. of ascending node)
C
C     Other formulations use Sine instead of Tangent.  We shall
C     call these the "Sine" formulations.
C
C        p = Sin(inclination/2) * Sin(R.A. of ascending node)
C        q = Sin(inclination/2) * Cos(R.A. of ascending node)
C
C     If you have equinoctial elements from this alternative
C     formulation you should replace p and q  by the
C     expressions below.
C
C       P = P / DSQRT ( 1.0D0 - P*P - Q*Q )
C       Q = Q / DSQRT ( 1.0D0 - P*P - Q*Q )
C
C     This will convert the Sine formulation to the Tangent formulation.
C
C$ Literature_References
C
C     JPL Engineering Memorandum 314-513 "Optical Navigation Program
C     Mathematical Models" by William M. Owen, Jr. and Robin M Vaughan
C     August 9, 1991
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     R.A. Jacobson   (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
C
C-    SPICELIB Version 1.0.1, 31-JAN-2008 (BVS)
C
C        Removed non-standard header section heading 
C        'Declarations_of_external_functions'.
C
C-    SPICELIB Version 1.0.0, 8-JAN-1997 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Compute a state from equinoctial elements
C
C-&
 
C
C     SPICELIB Functions.
C
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      KEPLEQ
      LOGICAL               RETURN
 
      EXTERNAL              TWOPI
      EXTERNAL              KEPLEQ
      EXTERNAL              RETURN
 
C
C     LOCAL VARIABLES
C
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      CAN
      DOUBLE PRECISION      CF
      DOUBLE PRECISION      CN
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      DI
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      NODEDT
      DOUBLE PRECISION      NODE
      DOUBLE PRECISION      DLPDT
      DOUBLE PRECISION      DLP
      DOUBLE PRECISION      MLDT
 
      DOUBLE PRECISION      TRANS(3,3)
      DOUBLE PRECISION      SA
      DOUBLE PRECISION      CA
      DOUBLE PRECISION      SD
      DOUBLE PRECISION      CD
 
      DOUBLE PRECISION      DX
      DOUBLE PRECISION      DX1
      DOUBLE PRECISION      DY
      DOUBLE PRECISION      DY1
      DOUBLE PRECISION      P
 
 
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      PRATE
      DOUBLE PRECISION      RB
      DOUBLE PRECISION      SAN
      DOUBLE PRECISION      SF
      DOUBLE PRECISION      SN
      DOUBLE PRECISION      TEMP ( 3  )
 
      DOUBLE PRECISION      X1,Y1
      DOUBLE PRECISION      H
      DOUBLE PRECISION      K
 
      DOUBLE PRECISION      L
      DOUBLE PRECISION      ML
      DOUBLE PRECISION      NFAC
      DOUBLE PRECISION      EECAN
      DOUBLE PRECISION      XHOLD(6)
      DOUBLE PRECISION      VF   (3)
      DOUBLE PRECISION      VG   (3)
 
 
      LOGICAL               FIRST
 
 
C
C     Constants computed on first pass
C
      DOUBLE PRECISION      PI2
 
      SAVE                  PI2
      SAVE                  FIRST
 
      DATA                  FIRST / .TRUE. /
 
C
C     Standard SPICE exception handling code.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'EQNCPV' )
 
C
C     The first time through this routine we fetch the various
C     constants we need for this routine.
C
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         PI2   = TWOPI()
      END IF
 
C
C     Take care of the various errors that can arise with the
C     input elements.
C
      IF ( EQEL(1) .LE. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The semi-major axis supplied to EQNCPV '
     .   //            'was non-positive. The value is required '
     .   //            'to be positive by this routine. The '
     .   //            'value supplied was #. ' )
 
         CALL ERRDP  ( '#', EQEL(1) )
         CALL SIGERR ( 'SPICE(BADSEMIAXIS)'  )
         CALL CHKOUT ( 'EQNCPV' )
         RETURN
 
      END IF
 
      ECC = DSQRT ( EQEL(2)*EQEL(2) + EQEL(3)*EQEL(3) )
 
      IF ( ECC .GT. 0.9D0 ) THEN
 
         CALL SETMSG ( 'The routine EQNCPV can reliably evaluate '
     .   //            'states from equinoctial elements if the '
     .   //            'eccentricity of the orbit associated '
     .   //            'with the elements is less than 0.9.  The '
     .   //            'eccentricity associated with the '
     .   //            'elements supplies is #.  The values of H '
     .   //            'and K are: # and # respectively. ' )
 
         CALL ERRDP  ( '#', ECC     )
         CALL ERRDP  ( '#', EQEL(2) )
         CALL ERRDP  ( '#', EQEL(3) )
         CALL SIGERR ( 'SPICE(ECCOUTOFRANGE)'  )
         CALL CHKOUT ( 'EQNCPV' )
         RETURN
 
      END IF
 
 
 
 
C
C     Form the transformation from planetary equator to the inertial
C     reference frame.
C
      SA          =  DSIN(RAPOL)
      CA          =  DCOS(RAPOL)
 
      SD          =  DSIN(DECPOL)
      CD          =  DCOS(DECPOL)
 
      TRANS(1,1)  =  -SA
      TRANS(1,2)  =  -CA*SD
      TRANS(1,3)  =   CA*CD
 
      TRANS(2,1)  =   CA
      TRANS(2,2)  =  -SA*SD
      TRANS(2,3)  =   SA*CD
 
      TRANS(3,1)  =   0.0D0
      TRANS(3,2)  =   CD
      TRANS(3,3)  =   SD
C
C     Compute the offset of the input epoch (ET) from the
C     epoch of the elements.
C
      DT     =  ET - EPOCH
 
C
C     Obtain the elements, rates, and other parameters. First get
C     the semi-major axis.
C
      A     = EQEL(1)
 
C
C     Recall that H and K at the epoch of the elements are in
C     EQEL(2) and EQEL(3) respectively.
C
C        H_0 = E*Sin(ARGP_0 + NODE_0 )
C        K_0 = E*Cos(ARGP_0 + NODE_0 )
C
C     The values of H and K at the epoch of interest is
C
C        H_dt = E*Sin(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt )
C        K_dt = E*Cos(ARGP_0 + NODE_0 + dt*d(ARGP+NODE)/dt )
C
C     But using the identities Sin(A+B) = Sin(A)Cos(B) + Sin(B)Cos(A)
C                              Cos(A+B) = Cos(A)Cos(B) - Sin(A)Sin(B)
C
C     We can re-write the expression for H_dt and K_dt as
C
C        H_dt = E*Sin(ARGP_0 + NODE_0 )Cos(dt*d(ARGP+NODE)/dt )
C             + E*Cos(ARGP_0 + NODE_0 )Sin(dt*d(ARGP+NODE)/dt )
C
C
C             = H_0 * Cos(dt*d(ARGP+NODE)/dt )
C             + K_0 * Sin(dt*d(ARGP+NODE)/dt )
C     and
C
C        K_dt = E*Cos(ARGP_0 + NODE_0)Cos(dt*d(ARGP+NODE)/dt)
C             - E*Sin(ARGP_0 + NODE_0)Sin(dt*d(ARGP+NODE)/dt)
C
C             = K_0 * Cos(dt*d(ARGP+NODE)/dt)
C             - H_0 * Sin(dt*d(ARGP+NODE)/dt)
C
C     Thus we can easily compute H and K at the current epoch.
C     Recall that the derivative of the longitude of periapse is
C     in entry 7 of EQEL.
C
      DLPDT = EQEL(7)
      DLP   = DT*DLPDT
 
      CAN   = DCOS( DLP )
      SAN   = DSIN( DLP )
 
      H     = EQEL(2)*CAN + EQEL(3)*SAN
      K     = EQEL(3)*CAN - EQEL(2)*SAN
 
C
C     The mean longitude at epoch is in the 4th element of EQEL.
C
      L     = EQEL(4)
 
C
C     The values for P and Q at epoch are stored in entries 5 and 6
C     of the array EQEL.  Recall that
C
C        P_0 = TAN(INC/2)*SIN(NODE_0)
C        Q_0 = TAN(INC/2)*COS(NODE_0)
C
C     We need P and Q offset from the initial epoch by DT.
C
C        P   = TAN(INC/2)*SIN(NODE_0 + dt*dNODE/dt)
C        Q   = TAN(INC/2)*COS(NODE_0 + dt*dNODE/dt)
C
C     Applying the same identities as we did before we have
C
C        P    = P_0 * Cos( dt*dNODE/dt ) + Q_0 * Sin( dt*dNODE/dt )
C        Q    = Q_0 * Cos( dt*dNODE/dt ) - P_0 * Sin( dt*dNODE/dt )
C
      NODEDT = EQEL(9)
      NODE   = DT*NODEDT
 
      CN     = DCOS( NODE )
      SN     = DSIN( NODE )
 
      P      = EQEL(5)*CN + EQEL(6)*SN
      Q      = EQEL(6)*CN - EQEL(5)*SN
 
      MLDT  = EQEL(8)
C
C     We compute the rate of change of the argument of periapse
C     by taking the difference between the rate of the longitude
C     of periapse and the rate of the node.
C
      PRATE = DLPDT - NODEDT
 
C
C     Form Broucke's beta parameter
C
      B     = DSQRT( 1.D0 - H*H - K*K )
      B     = 1.D0/( 1.D0 + B )
 
C
C     Construct the coordinate axes
C
      DI     = 1.0D0/(1.D0 + P*P + Q*Q)
 
      VF(1)  = (1.D0 - P*P + Q*Q) * DI
      VF(2)  =  2.D0 * P   * Q    * DI
      VF(3)  = -2.D0 * P          * DI
 
      VG(1)  =  2.D0 * P * Q      * DI
      VG(2)  = (1.D0 + P*P - Q*Q) * DI
      VG(3)  =  2.D0 * Q          * DI
 
C
C     Compute the mean longitude
C
      ML  =  L + DMOD( MLDT*DT, PI2 )
 
C
C     Obtain the eccentric longitude from Kepler's equation
C
      EECAN  =  KEPLEQ( ML, H, K )
 
C
C     Trigonometric functions of the eccentric longitude
C
      SF = DSIN(EECAN)
      CF = DCOS(EECAN)
 
C
C     Position in the orbit plane
C
      X1 = A*( (1.D0-(B*H**2))*CF + (H*K*B*SF - K) )
      Y1 = A*( (1.D0-(B*K**2))*SF + (H*K*B*CF - H) )
 
C
C     Radial distance and functions of the radial distance
C
      RB = H*SF + K*CF
      R  = A*(1.0D0-RB)
      RA = (MLDT*A*A)/R
C
C
C     Velocity in the orbit plane
C
      DX1 = RA * ( -SF + H*B*RB )
      DY1 = RA * (  CF - K*B*RB )
 
C
C     Correction factor for periapsis rate
C
      NFAC = 1.0D0 - ( DLPDT/MLDT )
 
C
C     Include precession in velocity
C
      DX  = NFAC*DX1 - PRATE*Y1
      DY  = NFAC*DY1 + PRATE*X1
 
C
C     Form the planetary mean equator position vector
C
      CALL VLCOM ( X1, VF, Y1, VG, XHOLD )
 
C
C     Form the planetary mean equator velocity vector
C
      TEMP (1) = -NODEDT*XHOLD(2)
      TEMP (2) =  NODEDT*XHOLD(1)
      TEMP (3) =  0.0D0
 
      CALL VLCOM3 ( 1.0D0, TEMP, DX, VF, DY, VG, XHOLD(4) )
 
C
C     Transform to an inertial state vector
C
      CALL MXV ( TRANS, XHOLD(1), STATE(1) )
      CALL MXV ( TRANS, XHOLD(4), STATE(4) )
 
      CALL CHKOUT ( 'EQNCPV' )
      RETURN
      END
