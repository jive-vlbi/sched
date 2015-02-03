C$Procedure ZZGFCPRX ( GF, coordinate derivative proxy )

      SUBROUTINE ZZGFCPRX ( STATE, CORSYS, RE, F, SENSE, CDSIGN )
 
C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Return the signs of a Cartesian velocity vector's coordinates
C     when the velocity is transformed to a given coordinate system.
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
C     COORDINATE
C     GEOMETRY
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
      INCLUDE 'zzgf.inc'

      DOUBLE PRECISION      STATE  ( 6 )
      CHARACTER*(*)         CORSYS
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      INTEGER               SENSE
      INTEGER               CDSIGN ( 3 )

 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STATE      I   A 6-dimensional Cartesian state vector.
C     CORSYS     I   A coordinate system name parameter.
C     RE         I   Ellipsoid equatorial radius.
C     F          I   Ellipsoid flattening coefficient.
C     SENSE      I   Reference body longitude sense.
C     CDSIGN     O   Velocity sign vector.
C
C$ Detailed_Input
C
C     STATE          is any Cartesian state vector. The order of the
C                    components matches those used by the SPK system.
C
C     CORSYS         is a character string parameter identifying a
C                    coordinate system. The recognized values of CORSYS
C                    are declared in the INCLUDE file
C
C                       zzgf.inc
C
C     RE             Equatorial radius of a reference spheroid.  This
C                    spheroid is a volume of revolution:  its
C                    horizontal cross sections are circular.  The shape
C                    of the spheroid is defined by an equatorial radius
C                    RE and a polar radius RP.
C
C     F              Flattening coefficient = (RE-RP) / RE,  where RP
C                    is the polar radius of the spheroid.
C
C     SENSE          is an integer indicating the sense of longitude
C                    for planetographic coordinate systems. A value of
C                    +1 indicates positive East; a value of -1 indicates
C                    positive West.
C
C$ Detailed_Output
C
C     CDSIGN         is an array of three integers indicating signs of
C                    the derivatives with respect to time of each
C                    coordinate, where the coordinates are determined
C                    by the input state and coordinate system. The
C                    elements of CDSIGN are -1, 0, or 1: these indicate
C                    negative, zero, or positive derivatives,
C                    respectively. The relationship between elements of
C                    CDSIGN and coordinates is given by the coordinate
C                    orders used in the RECxxx coordinate conversion
C                    routines. Those orders are shown in the table
C                    below.
C                    
C                    System          Coordinates
C                    ----------      -----------
C                    Rectangular     X, Y, Z
C                    Latitudinal     Radius, Longitude, Latitude
C                    Spherical       Radius, Colatitude, Longitude
C                    RA/Dec          Range, Right Ascension, Declination
C                    Cylindrical     Radius, Longitude, Z
C                    Geodetic        Longitude, Latitude, Altitude
C                    Planetographic  Longitude, Latitude, Altitude
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input argument SENSE has a value other than -1 or 1,
C        and the coordinate system is planetographic, the error
C        SPICE(VALUEOUTOFRANGE) is signaled. For other coordinate
C        systems, this argument is ignored.
C
C     2) If the input coordinate system specifier is not recognized,
C        the error SPICE(NOTSUPPORTED) is signaled.
C
C     3) If the coordinate system is geodetic or planetographic,
C        invalid ellipsoid shape parameters will be diagnosed by
C        routines in the call tree of this routine. For other
C        coordinate systems, these arguments are ignored.
C
C$ Files
C
C     None.
C
C$ Particulars
C 
C     In order to conduct searches involving constraints on the
C     coordinates of a position vector, the GF system requires the
C     signs of the derivatives with respect to time of the coordinates
C     referenced in the constraints. The most direct way to obtain
C     these signs is to convert the Cartesian velocity to the
C     coordinate system of interest using the SPICE Jacobian matrix
C     routines; however, that technique has the drawback of being
C     unusable at or near singularities of the mapping from rectangular
C     coordinates to any non-rectangular coordinate system.
C
C     This routine avoids problems with singularities by determining
C     signs of coordinate derivatives without computing the
C     (problematic) derivatives themselves. Instead this routine uses
C     proxy functions that have the same signs, for a given set of
C     inputs, as the coordinate derivatives of interest, where those
C     derivatives are defined. In addition, this routine returns
C     derivative signs for any position, including those where Jacobian
C     matrices are undefined. This allows the GF system to handle cases
C     where the time derivative of one coordinate is defined but
C     unavailable from the Jacobian matrix routines because another
C     coordinate is undefined or not differentiable at the same
C     position.
C 
C     Below, we discuss the proxy functions used by this routine.
C
C
C     Non-singular case
C     =================
C    
C     For positions off the Z-axis, all of the rectangular-to-alternate
C     coordinate transformation Jacobian matrices are defined in
C     principle. These matrices may not be computable in practice
C     because the derivative with respect to time of longitude can
C     overflow.
C
C     Our solution is to transform the input Cartesian velocity to a
C     "modified radial, tangential, normal" (MRTN) reference
C     frame: the basis vectors of this frame point "up", "East," and
C     "North." For geodetic and planetographic coordinate systems, the
C     "up" direction points along the outward normal of the reference
C     ellipsoid defined by the input parameters RE and F; in other
C     words, "up" is the direction of increasing altitude. For
C     cylindrical coordinates, "up" is the radial direction and "North"
C     is the +Z direction.
C
C     For the other latitudinal systems, the "up" direction points in
C     the direction of increasing radius; the up direction is parallel
C     to the position component of the input state.
C
C     The basis vectors of the MRTN frame lose precision for positions
C     very close to the Z-axis, but there are no problems with division
C     by zero or arithmetic overflow.
C     
C     The MRTN frame velocity indicates the signs of the coordinate 
C     derivatives as follows:
C
C        - Longitude: the sign of the rate of change of positive East
C          longitude is equal to the sign of the East component of
C          the MRTN velocity. 
C
C          For planetographic coordinate systems, the sign is adjusted
C          as needed to account for the sense of positive longitude.
C          The caller passes in a "longitude sense" indicator, allowing
C          the GF system to determine this sense once per search at
C          search initialization time.
C          
C        - Latitude: the sign of the rate of change of planetocentric
C          latitude is equal to the sign of the North component of
C          the MRTN velocity. 
C
C        - Co-latitude:  the sign of the rate of change of
C          planetocentric latitude is equal to the negative of the sign
C          of the North component of the MRTN velocity.
C
C        - Radius or altitude: the sign of the rate of change of
C          these coordinates is equal to sign of the up component of
C          the MRTN velocity. 
C
C
C     Singular cases
C     ==============
C
C     When the position lies on the Z-axis, some or all of the 
C     derivatives of the coordinates with respect to Cartesian
C     coordinates may not exist. This routine assigns all such 
C     derivatives a sign of zero. Other derivatives, such as
C     those of radius or altitude, may exist.
C
C     Below we summarize the treatment of the singular cases.
C     We assume the input velocity is non-zero, and we omit
C     the case of rectangular coordinates.
C
C        Coordinate Derivative                Sign
C        ---------------------                ----
C        Longitude (all systems)              0
C        Right ascension                      0
C        Latitude  (all systems)              0              
C        Declination                          0
C        Co-latitude                          0
C
C        Non-cylindrical radius, altitude  {  0 if position is at 
C                                               origin
C
C                                             1 if dot product of 
C                                               velocity and position 
C                                               is positive
C
C                                            -1 if dot product of 
C                                               velocity and position
C                                               is negative            }
C
C        Cylindrical radius                   0
C
C        Z                                 {  1 if velocity Z-component
C                                               is positive
C
C                                             0 if velocity Z-component
C                                               is zero
C
C                                            -1 if velocity Z-component
C                                               is negative            }
C        
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
C     [1] ANSI Fortran 77 Standard, p. 15-23.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C   
C$ Version
C
C-    SPICELIB Version 1.1.0, 15-APR-2014 (NJB)
C
C        Added FAILED() check to avoid numeric problems.
C
C-    SPICELIB Version 1.0.0, 15-APR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     coordinate derivative proxy
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
      
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C
      INTEGER               UPIDX
      PARAMETER           ( UPIDX  = 1 )

      INTEGER               ESTIDX
      PARAMETER           ( ESTIDX = 2 )

      INTEGER               NORIDX
      PARAMETER           ( NORIDX = 3 )


C
C     Local variables
C
C     Internally, we're going to use the more
C     descriptive names EAST for the "tangential"
C     direction and NORTH for the "normal" direction.
C
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      NORMAL ( 3 ) 
      DOUBLE PRECISION      RTNVEL ( 3 )
      DOUBLE PRECISION      VEL    ( 3 )
      DOUBLE PRECISION      XMAT   ( 3, 3 )

      INTEGER               DPSIGN
      INTEGER               I
      INTEGER               RTNSGN ( 3 )


C
C     Standard SPICE error handling.
C     
      IF ( RETURN() ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'ZZGFCPRX' )

C
C     For planetographic coordinates, check the longitude sense.
C
      IF ( CORSYS .EQ. PGRSYS ) THEN

         IF (  ( SENSE .NE. 1 ) .AND. ( SENSE .NE. -1 )  ) THEN

            CALL SETMSG ( 'Longitude sense # should be 1 or -1.'  )
            CALL ERRINT ( '#', SENSE                              )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                )
            CALL CHKOUT ( 'ZZGFCPRX'                              )
            RETURN

         END IF

      END IF

C
C     If we have a zero velocity vector, just indicate that each
C     velocity coordinate isn't changing and return now. If the
C     velocity vector is non-zero, convert it to a unit vector; this
C     guarantees that overflow can't occur.
     
      IF (  VZERO( STATE(4) )  ) THEN
C
C        The velocity is zero. Indicate that the coordinates are
C        not changing and return. Returning now simplifies the
C        logic of the rest of the routine, since the case of
C        zero-velocity can be ignored.
C
         CALL CLEARI ( 3, CDSIGN )
 
         CALL CHKOUT ( 'ZZGFCPRX' )
         RETURN

      ELSE
         CALL VHAT ( STATE(4), VEL )
      END IF

C
C     The rectangular case is trivial; handle it now.
C     
      IF ( CORSYS .EQ. RECSYS ) THEN
C
C        The output system is rectangular. Just indicate the
C        signs of the input velocity.
C        
         DO I = 1, 3

            IF ( VEL(I) .EQ. 0.D0 ) THEN

               CDSIGN(I) = 0

            ELSE
C
C              Use the Fortran sign transfer intrinsic function
C              to set CDSIGN(I) to 1 or -1, depending
C              on whether the corresponding velocity component
C              is positive or negative. See reference [1] for a 
C              discussion of this Fortran intrinsic function.
C
               CDSIGN(I) = NINT(  SIGN( 1.D0, VEL(I) )  )

            END IF

         END DO
C
C        All done. 
C
         CALL CHKOUT ( 'ZZGFCPRX' )
         RETURN

      END IF

C
C     There's quite a bit of common logic for the "on Z-axis" case;
C     take care of it here.
C
      IF (       ( STATE(1) .EQ. 0.D0 ) 
     .     .AND. ( STATE(2) .EQ. 0.D0 ) ) THEN
C
C        The position lies on the Z-axis.
C
C        For all of the coordinate systems having a longitude
C        coordinate (this includes right ascension), the derivative of
C        longitude with respect to time is undefined; we set the sign
C        of the derivative to zero.
C
C        For all of the coordinate systems having a latitude coordinate
C        (this includes declination), if the position is not at the
C        origin, the derivative of latitude with respect to time is
C        undefined unless the input velocity is zero. At the origin,
C        the derivative of latitude with respect to time doesn't exist.
C        In both cases, we set the sign of the velocity components
C        to zero.
C
C        For the coordinate systems that have a radius or range
C        coordinate, where distance is measured from the origin, when
C        the input position is not at the origin, distance is
C        increasing, constant, or decreasing depending on whether the
C        dot product of velocity and the position's Z-coordinate is
C        positive, zero, or negative, respectively. This dot product
C        test is valid for the derivative of altitude as well (we
C        assert this without proof for the case of positions inside
C        prolate spheroids).
C
C        If the position is at the origin, then since range and 
C        altitude are not differentiable, their signs are set to
C        zero.
C
C        Cylindrical coordinates are a special case which we treat
C        separately.
C
         IF ( STATE(3) .NE. 0.D0 ) THEN
C
C           The position is on the Z-axis but not at the origin.
C
C           Compute the dot product used for the range/altitude 
C           derivative.
C 
            DP = VDOT ( STATE, VEL )
         
            IF ( DP .EQ. 0.D0 ) THEN

               DPSIGN = 0
            ELSE
               DPSIGN = NINT(  SIGN( 1.D0, DP )  )
            END IF

         ELSE
C
C           The position is at the origin. We know the velocity
C           is non-zero, and any movement increases radius or
C           altitude. However, neither radius nor altitude are
C           differentiable here, so we indicate no sign.
C
            DPSIGN = 0

         END IF

C
C        Set the coordinate derivative signs for all but the 
C        rectangular system, which was handled already, and
C        the cylindrical system.
C
C
C        Recall the coordinate systems and their coordinate orders:
C
C        System          Coordinates
C        ----------      -----------
C        Rectangular     X, Y, Z
C        Latitudinal     Radius, Longitude, Latitude
C        Spherical       Radius, Colatitude, Longitude
C        RA/Dec          Range, Right Ascension, Declination
C        Cylindrical     Radius, Longitude, Z
C        Geodetic        Longitude, Latitude, Altitude
C        Planetographic  Longitude, Latitude, Altitude
C
C
         IF ( CORSYS .EQ. LATSYS ) THEN
C
C           The radial derivative sign was computed; the
C           other derivative signs are set to zero.
C
            CDSIGN(1) = DPSIGN
            CDSIGN(2) = 0
            CDSIGN(3) = 0

         ELSE IF ( CORSYS .EQ. SPHSYS ) THEN
C
C           The radial derivative sign was computed; the
C           longitude derivative signs is set to zero.
C
            CDSIGN(1) = DPSIGN
            CDSIGN(3) = 0

C
C           Co-latitude is a special case. Co-latitude is
C           not differentiable with respect to Cartesian
C           position for positions on the Z-axis, since
C           co-latitude is a v-shaped function of distance
C           from the Z-axis. We simply set the sign 
C           of the co-latitude derivative to zero in this
C           case.
C
            CDSIGN(2) = 0

         ELSE IF ( CORSYS .EQ. RADSYS ) THEN
C
C           RA/Dec derivatives are assigned in the same manner
C           as latitudinal ones.
C
            CDSIGN(1) = DPSIGN
            CDSIGN(2) = 0
            CDSIGN(3) = 0

         ELSE IF ( CORSYS .EQ. GEOSYS ) THEN
C
C           Altitude plays the role of radius for this
C           system.
C
            CDSIGN(1) = 0
            CDSIGN(2) = 0
            CDSIGN(3) = DPSIGN

         ELSE IF ( CORSYS .EQ. PGRSYS ) THEN
C
C           Altitude plays the role of radius for this
C           system.
C
            CDSIGN(1) = 0
            CDSIGN(2) = 0
            CDSIGN(3) = DPSIGN

         ELSE IF ( CORSYS .EQ. CYLSYS ) THEN

            CDSIGN(1) = 0
            CDSIGN(2) = 0
C
C           For cylindrical coordinates, the derivative of Z with
C           respect to time is already present in VEL.
C
            IF ( VEL(3) .EQ. 0.D0 ) THEN
               CDSIGN(3) = 0
            ELSE
               CDSIGN(3) = NINT(  SIGN( 1.D0, VEL(3) )  )
            END IF


         ELSE
C
C           If we end up here, we have an invalid coordinate system.
C
            CALL SETMSG ( 'Coordinate system # is not supported. ' 
     .      //            'Verify that the coordinate system '
     .      //            'specifier matches a value from zzgf.inc.' )
            CALL ERRCH  ( '#', CORSYS                                )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
            CALL CHKOUT ( 'ZZGFCPRX'                                 )
            RETURN

         END IF

C
C        We've handled the on-Z-axis cases. Return now.
C
         CALL CHKOUT ( 'ZZGFCPRX' )
         RETURN

      END IF

C
C     This is the normal case: the position is not on the Z-axis.
C
C     The type of MRTN frame we use depends on the coordinate system.
C     Planetodetic and planetographic coordinate systems are a special
C     case.
C     
      IF ( ( CORSYS .EQ. GEOSYS ) .OR. ( CORSYS .EQ. PGRSYS )  ) THEN
C
C        Instead of defining the MRTN frame using the input
C        position vector, we define it using an outward normal vector 
C        on the reference ellipsoid at the geodetic latitude
C        and longitude of the input position.
C
         CALL RECGEO ( STATE, RE,  F,   LON, LAT, ALT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFCPRX' )
            RETURN
         END IF

         CALL LATREC ( 1.D0,  LON, LAT, NORMAL )


      ELSE IF ( CORSYS .EQ. CYLSYS ) THEN
C
C        The normal vector is aligned with the local radial
C        direction; this vector is parallel to the X-Y plane.
C
         CALL VPACK  ( STATE(1), STATE(2), 0.D0, NORMAL )
         CALL VHATIP ( NORMAL )

      ELSE 
C
C        The position vector provides the normal direction.
C
         CALL VHAT ( STATE, NORMAL )

      END IF


C     Obtain the matrix required to transform the velocity to the MRTN
C     frame; transform the velocity.
C
      CALL ZZRTNMAT ( NORMAL, XMAT )
      CALL MXV      ( XMAT,   VEL, RTNVEL )
 
C
C     We can think of the basis vectors of the MRTN frame as local "up",
C     "East," "North" directions. Compute the signs of the up, East,
C     and North velocity components.
C
      DO I = 1, 3

         IF ( RTNVEL(I) .EQ. 0.D0 ) THEN
         
            RTNSGN(I) = 0
         ELSE
            RTNSGN(I) = NINT(  SIGN( 1.D0, RTNVEL(I) )  )
         END IF

      END DO

C
C     Set the signs of the coordinate derivatives from the MRTN
C     derivative signs.
C
C
C        Recall the coordinate systems and their coordinate orders:
C
C        System          Coordinates
C        ----------      -----------
C        Rectangular     X, Y, Z
C        Latitudinal     Radius, Longitude, Latitude
C        Spherical       Radius, Colatitude, Longitude
C        RA/Dec          Range, Right Ascension, Declination
C        Cylindrical     Radius, Longitude, Z
C        Geodetic        Longitude, Latitude, Altitude
C        Planetographic  Longitude, Latitude, Altitude
C
C
      IF ( CORSYS .EQ. LATSYS ) THEN

         CDSIGN(1) = RTNSGN(UPIDX)
         CDSIGN(2) = RTNSGN(ESTIDX)
         CDSIGN(3) = RTNSGN(NORIDX)

      ELSE IF ( CORSYS .EQ. SPHSYS ) THEN
C
C        For spherical coordinate systems, the sign of the 
C        derivative of co-latitude is the negative of the
C        sign of the North derivative.
C
         CDSIGN(1) =   RTNSGN(UPIDX)
         CDSIGN(2) = - RTNSGN(NORIDX)
         CDSIGN(3) =   RTNSGN(ESTIDX)

      ELSE IF ( CORSYS .EQ. RADSYS ) THEN

         CDSIGN(1) = RTNSGN(UPIDX)
         CDSIGN(2) = RTNSGN(ESTIDX)
         CDSIGN(3) = RTNSGN(NORIDX)

      ELSE IF ( CORSYS .EQ. GEOSYS ) THEN

         CDSIGN(1) = RTNSGN(ESTIDX)
         CDSIGN(2) = RTNSGN(NORIDX)
         CDSIGN(3) = RTNSGN(UPIDX)

      ELSE IF ( CORSYS .EQ. PGRSYS ) THEN
C
C        For planetographic coordinates, altitude and latitude 
C        behave identically to their geodetic counterparts. We 
C        need to adjust the sign of the longitude derivative
C        according to whether longitude is positive East or West.
C
         CDSIGN(1) = RTNSGN(ESTIDX) * SENSE
         CDSIGN(2) = RTNSGN(NORIDX)
         CDSIGN(3) = RTNSGN(UPIDX)

      ELSE IF ( CORSYS .EQ. CYLSYS ) THEN

         CDSIGN(1) = RTNSGN(UPIDX)
         CDSIGN(2) = RTNSGN(ESTIDX)
         CDSIGN(3) = RTNSGN(NORIDX)

      ELSE
C
C        If we end up here, we have an invalid coordinate system.
C
         CALL SETMSG ( 'Coordinate system # is not supported. ' 
     .   //            'Verify that the coordinate system '
     .   //            'specifier matches a value from zzgf.inc.' )
         CALL ERRCH  ( '#', CORSYS                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZGFCPRX'                                 )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZGFCPRX' )
      RETURN
      END 

