C$Procedure      XFMSTA ( Transform state between coordinate systems)
 
      SUBROUTINE XFMSTA ( ISTATE, ICOSYS, OCOSYS, BODY, OSTATE )
 
C$ Abstract
C
C     Transform a state between coordinate systems.
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
C     CONVERSION
C     COORDINATE
C     EPHEMERIS
c     STATE
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'
 
      DOUBLE PRECISION      ISTATE(6)
      CHARACTER*(*)         ICOSYS
      CHARACTER*(*)         OCOSYS
      CHARACTER*(*)         BODY
      DOUBLE PRECISION      OSTATE(6)
      
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     ISTATE     I   Input state.
C     ICOSYS     I   Current (input) coordinate system.
C     OCOSYS     I   Desired (output) coordinate system.
C     BODY       I   Name or NAIF ID of body with which
C                    coordinates are associated (if applicable).
C     OSTATE     O   Converted output state.
C
C$ Detailed_Input
C
C     ISTATE     is a state vector in the input (ICOSYS) coordinate
C                system representing position and velocity.
C
C                All angular measurements must be in radians.
C
C                Note: body radii values taken from the kernel
C                pool are used when converting to or from geodetic or
C                planetographic coordinates. It is the user's
C                responsibility to verify the distance inputs are in
C                the same units as the radii in the kernel pool,
C                typically kilometers.
C
C     ICOSYS     is the name of the coordinate system that the input
C                state vector (ISTATE) is currently in. 
C
C                ICOSYS may be any of the following:
C
C                    'RECTANGULAR' 
C                    'CYLINDRICAL' 
C                    'LATITUDINAL'
C                    'SPHERICAL'   
C                    'GEODETIC'   
C                    'PLANETOGRAPHIC'   
C
C                Leading spaces, trailing spaces, and letter case
C                are ignored. For example, ' cyLindRical  ' would be
C                accepted.
C
C     OCOSYS     is the name of the coordinate system that the state
C                should be converted to.
C
C                Please see the description of ICOSYS for details.
C
C     BODY       is the name or NAIF ID of the body associated with the
C                planetographic or geodetic coordinate system.
C
C                If neither of the coordinate system choices are
C                geodetic or planetographic, BODY may be an empty
C                string (' ').
C
C                Examples of accepted body names or IDs are:
C                         'Earth'
C                         '399'
C
C                Leading spaces, trailing spaces, and letter case are
C                ignored.
C
C$ Detailed_Output
C
C     OSTATE     is the state vector that has been converted to the
C                output coordinate system (OCOSYS).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either the input or output coordinate system is not
C         recognized, the error SPICE(COORDSYSNOTREC) is signaled.
C
C     2)  If the input body name cannot be converted to a NAIF ID
C         (applies to geodetic and planetographic coordinate
C         systems), the error 'SPICE(IDCODENOTFOUND)' is signaled.
C
C     3)  If the input state ISTATE is not valid, meaning the position
C         but not the velocity is along the z-axis, the error
C         'SPICE(INVALIDSTATE)' is signaled.
C
C         Note: If both the input position and velocity are along
C         the z-axis and the output coordinate system is not
C         rectangular, the velocity can still be calculated even 
C         though the Jacobian is undefined. This case will not 
C         signal an error. An example of the input position and
C         velocity along the z-axis is below.
C
C                       Term    Value
C                       -----   ------
C                         x       0    
C                         y       0 
C                         z       z 
C                       dx/dt     0 
C                       dy/dt     0 
C                       dz/dt   dz_dt
C
C     4)  If either the input or output coordinate system is 
C         geodetic or planetographic and at least one of the body's
C         radii is less than or equal to zero, the error 
C         SPICE(INVALIDRADIUS) will be signaled. 
C
C     5)  If either the input or output coordinate system is
C         geodetic or planetographic and the difference of the
C         equatorial and polar radii divided by the equatorial radius
C         would produce numeric overflow, the error
C         'SPICE(INVALIDRADIUS)' will be signaled. 
C
C     6)  If the product of the Jacobian and velocity components
C         may lead to numeric overflow, the error
C         'SPICE(NUMERICOVERFLOW)' is signaled.  
C
C$ Files
C
C     SPK, PCK, CK, and FK kernels may be required. 
C
C     If the input or output coordinate systems are either geodetic or
C     planetographic, a PCK providing the radii of the body
C     name BODY must be loaded via FURNSH.
C
C     Kernel data are normally loaded once per program run, NOT every
C     time this routine is called.
C
C$ Particulars
C
C     Input Order
C     -------------------------------------------
C
C     The input and output states will be structured by the 
C     following descriptions.
C
C     For rectangular coordinates, the state vector is the following
C     in which X, Y, and Z are the rectangular position components and
C     DX, DY, and DZ are the time derivatives of each position
C     component.
C
C             ISTATE = ( X, Y, Z, DX, DY, DZ )
C
C     For cylindrical coordinates, the state vector is the following
C     in which R is the radius, LONG is the longitudes, Z is the
C     height, and DR, DLONG, and DZ are the time derivatives of each
C     position component.
C
C             ISTATE = ( R, LONG, Z, DR, DLONG, DZ )
C
C     For latitudinal coordinates, the state vector is the following
C     in which R is the radius, LONG is the longitude, LAT is the 
C     latitude, and DR, DLONG, and DLAT are the time derivatives of
C     each position component.
C
C             ISTATE = ( R, LONG, LAT, DR, DLONG, DLAT )
C
C     For spherical coordinates, the state vector is the following in
C     which R is the radius, COLAT is the colatitude, LONG is the 
C     longitude, and DR, DCOLAT, and DLONG are the time derivatives of
C     each position component.
C
C             ISTATE = ( R, COLAT, LONG, DR, DCOLAT, DLONG )
C
C     For geodetic coordinates, the state vector is the following in
C     which LONG is the longitude, LAT is the latitude, ALT is the
C     altitude, and DLONG, DLAT, and DALT are the time derivatives of 
C     each position component.
C
C             ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT )
C
C     For planetographic coordinates, the state vector is the
C     following in which LONG is the longitude, LAT is the latitude,
C     ALT is the altitude, and DLONG, DLAT, and DALT are the time
C     derivatives of each position component. 
C
C             ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT )
C
C
C     Input Boundaries
C     -------------------------------------------
C
C     There are intervals the input angles must fall within if
C     the input coordinate system is not rectangular. These
C     intervals are provided below.
C
C        Input variable    Input meaning   Input interval [rad]
C        --------------    -------------   ------------------------
C            LONG           Longitude        0     <= LONG  <  2*pi
C            LAT            Latitude        -pi/2  <= LAT   <= pi/2
C            COLAT          Colatitude       0     <= COLAT <= pi
C
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1) Find the apparent state of Phoebe as seen by CASSINI in the
C        J2000 frame at 2004 Jun 11 19:32:00. Transform the state
C        from rectangular to latitudinal coordinates. For verification,
C        transform the state back from latitudinal to rectangular
C        coordinates. 
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File name: xfmsta_ex1.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C                  File name                     Contents
C                  ---------                     --------
C                  cpck05Mar2004.tpc             Planet orientation and
C                                                radii
C                  naif0009.tls                  Leapseconds
C                  020514_SE_SAT105.bsp          Satellite ephemeris for
C                                                Saturn
C                  030201AP_SK_SM546_T45.bsp     CASSINI ephemeris
C                  981005_PLTEPH-DE405S.bsp      Planetary ephemeris
C
C
C           \begindata
C
C           KERNELS_TO_LOAD = ( 'naif0009.tls'  ,
C                               '020514_SE_SAT105.bsp'  ,
C                               '030201AP_SK_SM546_T45.bsp'  ,
C                               '981005_PLTEPH-DE405S.bsp',
C                               'cpck05Mar2004.tpc'   )
C
C           End of meta-kernel
C
C        Example code begins here.
C
C           PROGRAM  EX1_XFMSTA
C           IMPLICIT NONE
C     C
C     C     Local parameters
C     C
C     C     METAKR is the meta-kernel's filename.
C     C
C           CHARACTER*(*)         METAKR
C           PARAMETER           ( METAKR = 'xfmsta_ex1.tm' )
C
C           CHARACTER*(*)         FORM
C           PARAMETER           ( FORM = '(F16.6, F16.6, F16.6)' )
C
C     C
C     C     Local variables
C     C
C     C     STAREC is the state of Phoebe with respect to CASSINI in 
C     C     rectangular coordinates. STALAT is the state rotated into
C     C     latitudinal coordinates. STREC2 is the state transformed
C     C     back into rectangular coordinates from latitudinal.
C     C
C           DOUBLE PRECISION      STAREC (6)
C           DOUBLE PRECISION      STALAT (6)
C           DOUBLE PRECISION      STREC2 (6)
C
C     C
C     C     ET is the ephemeris time (TDB) corresponding to the
C     C     observation.
C     C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      LT
C
C           INTEGER               I
C
C     C
C     C     The required kernels must be loaded.
C     C
C           CALL FURNSH ( METAKR )
C
C     C
C     C     Calculate the state at 2004 Jun 11 19:32:00 UTC.
C     C
C           CALL STR2ET ( '2004-JUN-11-19:32:00', ET )
C
C     C
C     C     Calculate the apparent state of Phoebe as seen by
C     C     CASSINI in the J2000 frame.
C     C
C           CALL SPKEZR ( 'PHOEBE',  ET, 'IAU_PHOEBE', 'LT+S',
C          .              'CASSINI', STAREC, LT )
C
C     C
C     C     Transform the state from rectangular to latitudinal.
C     C     Notice that since neither the input nor output
C     C     coordinate frames are 'geodetic' or 'planetographic',
C     C     the input for the body name is a blank string.
C     C
C           CALL XFMSTA ( STAREC, 'RECTANGULAR', 'LATITUDINAL', ' ',
C          .              STALAT )
C
C     C
C     C     Transform the state back to rectangular from latitudinal
C     C     for verification. This result should be very similar to
C     C     STAREC.
C     C
C           CALL XFMSTA ( STALAT, 'LATITUDINAL', 'RECTANGULAR',' ',
C          .              STREC2 )
C
C     C
C     C     Report the results.
C     C
C           WRITE (*,*)    ' '
C           WRITE (*,*)    'Phoebe as seen by CASSINI - rectangular'
C           WRITE (*,*)    '  Position [km]:'
C           WRITE (*,FORM) (STAREC(I), I = 1, 3)
C           WRITE (*,*)    '  Velocity [km/s]:'
C           WRITE (*,FORM) (STAREC(I), I = 4, 6)
C           WRITE (*,*)    ' '
C           WRITE (*,*)    'Phoebe as seen by CASSINI - latitudinal'
C           WRITE (*,*)    '  Position [km, rad, rad]:'
C           WRITE (*,FORM) (STALAT(I), I = 1, 3)
C           WRITE (*,*)    '  Velocity [km/s, rad/s, rad/s]:'
C           WRITE (*,FORM) (STALAT(I), I = 4, 6)
C           WRITE (*,*)    ' '
C           WRITE (*,*)    'Verification: '
C           WRITE (*,*)    'Phoebe as seen by CASSINI - rectangular'
C           WRITE (*,*)    '  Position [km]:'
C           WRITE (*,FORM) (STREC2(I), I = 1, 3)
C           WRITE (*,*)    '  Velocity [km/s]:'
C           WRITE (*,FORM) (STREC2(I), I = 4, 6)
C
C           END
C
C        When this program was executed using gfortran on a PC Linux
C        64 bit environment, the output was:
C
C             Phoebe as seen by CASSINI - rectangular
C               Position [km]:
C                -1982.639762     -934.530471     -166.562595
C               Velocity [km/s]:
C                    3.970832       -3.812496       -2.371663
C
C             Phoebe as seen by CASSINI - latitudinal
C               Position [km, rad, rad]:
C                 2198.169858       -2.701121       -0.075846
C               Velocity [km/s, rad/s, rad/s]:
C                   -1.780939        0.002346       -0.001144
C
C             Verification:
C             Phoebe as seen by CASSINI - rectangular
C               Position [km]:
C                -1982.639762     -934.530471     -166.562595
C               Velocity [km/s]:
C                    3.970832       -3.812496       -2.371663
C
C     2) Transform a given state from cylindrical to planetographic
C        coordinates with respect to Earth. 
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File name: xfmsta_ex2.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              cpck05Mar2004.tpc             Planet orientation and
C                                            radii
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'cpck05Mar2004.tpc' )
C
C           \begintext
C
C           End of meta-kernel
C
C
C        Example code begins here.
C
C           PROGRAM  EX2_XFMSTA
C           IMPLICIT NONE
C
C     C
C     C     Local parameters
C     C
C     C     METAKR is the meta-kernel's filename.
C     C
C           CHARACTER*(*)         METAKR
C           PARAMETER           ( METAKR = 'xfmsta_ex2.tm' )
C
C           CHARACTER*(*)         FORM
C           PARAMETER           ( FORM = '(F16.6, F16.6, F16.6)' )
C
C     C
C     C     Local variables
C     C
C     C     STACYL is the state in cylindrical coordinates.
C     C
C           DOUBLE PRECISION      STACYL (6)
C     C
C     C     STAPLN is the state transformed into planetographic
C     C     coordinates.
C     C
C           DOUBLE PRECISION      STAPLN (6)
C     C
C     C     STCYL2 is the state transformed back into
C     C     cylindrical coordinates from planetographic.
C     C
C           DOUBLE PRECISION      STCYL2 (6)
C
C           INTEGER               I
C
C           DATA STACYL / 1.0D0, 0.5D0, 0.5D0, 0.2D0, 0.1D0, -0.2D0 /
C     C
C     C     The required kernels must be loaded.
C     C
C           CALL FURNSH ( METAKR )
C
C     C
C     C     Transform the state from cylindrical to planetographic.
C     C     Note that since one of the coordinate systems is
C     C     planetographic, the body name must be input.
C     C
C           CALL XFMSTA ( STACYL, 'CYLINDRICAL', 'PLANETOGRAPHIC',
C          .              'EARTH', STAPLN )
C
C     C
C     C     Transform the state back to cylindrical from
C     C     planetographic for verification. The result should be very
C     C     close to STACYL.
C     C
C           CALL XFMSTA ( STAPLN, 'PLANETOGRAPHIC', 'CYLINDRICAL',
C          .              'EARTH', STCYL2 )
C
C     C
C     C     Report the results.
C     C
C           WRITE (*,*)    'Cylindrical state'
C           WRITE (*,*)    '  Position [km, rad, km]:'
C           WRITE (*,FORM) (STACYL(I), I = 1, 3)
C           WRITE (*,*)    '  Velocity [km/s, rad/s, km/s]:'
C           WRITE (*,FORM) (STACYL(I), I = 4, 6)
C           WRITE (*,*)    ' '
C           WRITE (*,*) 'Planetographic state'
C           WRITE (*,*)    '  Position [rad, rad, km]:'
C           WRITE (*,FORM) (STAPLN(I), I = 1, 3)
C           WRITE (*,*)    '  Velocity [rad/s, rad/s, km/s]:'
C           WRITE (*,FORM) (STAPLN(I), I = 4, 6)
C           WRITE (*,*)    ' '
C           WRITE (*,*)    'Verification:  Cylindrical state'
C           WRITE (*,*)    '  Position [km, rad, km]:'
C           WRITE (*,FORM) (STCYL2(I), I = 1, 3)
C           WRITE (*,*)    '  Velocity [km/s, rad/s, km/s]:'
C           WRITE (*,FORM) (STCYL2(I), I = 4, 6)
C
C           END
C
C        When this program was executed using gfortran on a PC Linux
C        64 bit environment, the output was:
C
C             Cylindrical state
C               Position [km, rad, km]:
C                    1.000000        0.500000        0.500000
C               Velocity [km/s, rad/s, km/s]:
C                    0.200000        0.100000       -0.200000
C
C             Planetographic state
C               Position [rad, rad, km]:
C                    0.500000        1.547727    -6356.238467
C               Velocity [rad/s, rad/s, km/s]:
C                    0.100000       -0.004721       -0.195333
C
C             Verification:  Cylindrical state
C               Position [km, rad, km]:
C                    1.000000        0.500000        0.500000
C               Velocity [km/s, rad/s, km/s]:
C                    0.200000        0.100000       -0.200000
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
C     S.C. Krening      (JPL)
C     B.V. Semenov      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0  22-APR-2014 (SCK)(BVS)
C
C-&
 
C$ Index_Entries
C
C     state transformation between coordinate systems
C     convert state
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      INTEGER               ISRCHC
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
C     Potentially large numbers produced by transforming the
C     velocity using the Jacobian must not exceed DPMAX()/MARGIN:
C
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D2 )

C
C     The size of each coordinate system name must not exceed 
C     CHSIZ characters.
C
      INTEGER               CHSIZ
      PARAMETER           ( CHSIZ  = 40 )

C
C     NCOSYS is the number of coordinate systems supported by
C     this routine.
C
      INTEGER               NCOSYS
      PARAMETER           ( NCOSYS = 6 )

C
C     The following integer parameters represent the coordinate
C     systems supported by this routine.
C
      INTEGER               RECTAN
      PARAMETER           ( RECTAN = 1 )

      INTEGER               CYL
      PARAMETER           ( CYL    = 2 )

      INTEGER               LATNL
      PARAMETER           ( LATNL  = 3 )

      INTEGER               SPHCL
      PARAMETER           ( SPHCL  = 4 )

      INTEGER               GEODET
      PARAMETER           ( GEODET = 5 )

      INTEGER               PLNTGR
      PARAMETER           ( PLNTGR = 6 )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL   = 36 )


C
C     Local variables
C
C     COSYS is the array of supported coordinate system names.
C     ISYSU and OSYSU are the input and output coordinate systems
C     from the user that are made insensitive to case or leading and 
C     trailing spaces.
C
      CHARACTER*(CHSIZ)     COSYS  (NCOSYS)
      CHARACTER*(CHSIZ)     ISYSU
      CHARACTER*(CHSIZ)     OSYSU

C
C     IPOS and IVEL are the input position and velocity translated
C     into rectangular.
C
      DOUBLE PRECISION      IPOS   ( 3 )
      DOUBLE PRECISION      IVEL   ( 3 )

C
C     For transformations including either geodetic or planetographic
C     coordinate systems, RADII is an array of the radii values 
C     associated with the input body. These values will be loaded
C     from the kernel pool.
C
      DOUBLE PRECISION      RADII  ( 3 )

C
C     JACOBI is the Jacobian matrix that converts the velocity 
C     coordinates between systems.
C
      DOUBLE PRECISION      JACOBI ( 3, 3 )

C
C     The flattening coefficient, F, is calculated when either
C     geodetic or planetographic coordinate systems are included
C     in the transformation.
C
      DOUBLE PRECISION      F

C
C     SQTMP and TOOBIG are used to check for possible numeric 
C     overflow situations.
C
      DOUBLE PRECISION      SQTMP
      DOUBLE PRECISION      TOOBIG

C
C     BODYID and DIM are only used when the input or output coordinate
C     systems are geodetic or planetographic. The BODYID is the NAID ID
C     associated with the input body name. DIM is used while retrieving
C     the radii from the kernel pool.
C
      INTEGER               BODYID
      INTEGER               DIM

C
C     ISYS and OSYS are the integer codes corresponding to the 
C     input and output coordinate systems. I and J are iterators.
C
      INTEGER               I
      INTEGER               ISYS
      INTEGER               J
      INTEGER               OSYS

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVBODY
      INTEGER               SVBDID
      LOGICAL               SVFND1

      LOGICAL               FIRST
C
C     Saved variables
C
      SAVE                  COSYS

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVBODY
      SAVE                  SVBDID
      SAVE                  SVFND1

      SAVE                  FIRST

C
C     Assign the names of the coordinate systems to a character 
C     array in which each coordinate system name is located at
C     the index of the integer ID of the coordinate system.
C
      DATA COSYS ( RECTAN ) / 'RECTANGULAR'    /
      DATA COSYS ( CYL    ) / 'CYLINDRICAL'    /
      DATA COSYS ( LATNL  ) / 'LATITUDINAL'    /
      DATA COSYS ( SPHCL  ) / 'SPHERICAL'      /
      DATA COSYS ( GEODET ) / 'GEODETIC'       /
      DATA COSYS ( PLNTGR ) / 'PLANETOGRAPHIC' /

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     There are three main sections of this routine:
C
C       1)  Error handling and initialization.
C       2)  Conversion of the input to rectangular coordinates.
C       3)  Conversion from rectangular to the output coordinates.
C
C     Error handling and initialization
C     ----------------------------------------------------------------
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'XFMSTA' )

C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counter.
C
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF

C
C     Remove initial and trailing spaces.
C     Convert the input coordinate systems to upper case.
C
      CALL LJUCRS ( 0, ICOSYS, ISYSU )
      CALL LJUCRS ( 0, OCOSYS, OSYSU )

C
C     Check to see if the input and output coordinate systems 
C     provided by the user are acceptable. Store the integer 
C     code of the input and output coordinate systems into
C     ISYS and OSYS.
C
      ISYS = ISRCHC ( ISYSU, NCOSYS, COSYS )
      OSYS = ISRCHC ( OSYSU, NCOSYS, COSYS )

C
C     If the coordinate systems are not acceptable, an error is 
C     signaled.
C
      IF ( ( ISYS .EQ. 0 )  .OR.  ( OSYS .EQ. 0 ) ) THEN

         IF    ( ( ISYS .EQ. 0 )  .AND. ( OSYS .EQ. 0 ) ) THEN

C
C           Both the input and the output coordinate systems were not
C           recognized.
C
            CALL SETMSG ( 'Input coordinate system # '
     .                //  'and output coordinate system # '
     .                //  'are not recognized.' )
            CALL ERRCH  ( '#', ICOSYS )
            CALL ERRCH  ( '#', OCOSYS )
            CALL SIGERR ( 'SPICE(COORDSYSNOTREC)' )
            CALL CHKOUT ( 'XFMSTA' )
            RETURN

         ELSE IF ( ISYS .EQ. 0 ) THEN

C
C           The input coordinate system was not recognized.
C
            CALL SETMSG ( 'Input coordinate system # '
     .               //   'was not recognized'  )
            CALL ERRCH  ( '#', ICOSYS )
            CALL SIGERR ( 'SPICE(COORDSYSNOTREC)' )
            CALL CHKOUT ( 'XFMSTA' )
            RETURN   

         ELSE 

C
C           The output coordinate system was not recognized.
C
            CALL SETMSG ( 'Output coordinate system # '
     .               //   'was not recognized'    )
            CALL ERRCH  ( '#', OCOSYS )
            CALL SIGERR ( 'SPICE(COORDSYSNOTREC)' )
            CALL CHKOUT ( 'XFMSTA' )
            RETURN    

         END IF

      END IF

C
C     If the input and output coordinate systems are equal, set the
C     output equal to the input since no conversion needs to take
C     place.
C
      IF ( ISYS .EQ. OSYS ) THEN

         CALL VEQUG  ( ISTATE, 6, OSTATE )
         CALL CHKOUT ( 'XFMSTA' )
         RETURN

      END IF

C
C     If converting to or from either geodetic or planetographic, the
C     NAIF ID must be found from the input body name BODY. If the
C     body name does not have a valid NAIF ID code, an error is
C     signaled. If the NAIF ID is valid, the radii of the body are
C     located and the flattening coefficient is calculated.
C
      IF ( ( OSYS .EQ. GEODET ) .OR. ( OSYS .EQ. PLNTGR )   .OR.
     .     ( ISYS .EQ. GEODET ) .OR. ( ISYS .EQ. PLNTGR ) ) THEN
C
C        Find the NAIF ID code
C
         CALL ZZBODS2C ( SVCTR1, SVBODY, SVBDID, SVFND1,
     .                   BODY, BODYID, FOUND    )
C
C        If the body's name was found, find the body's radii and
C        compute flattening coefficient. Otherwise, signal an error.
C
         IF ( FOUND ) THEN

            CALL BODVCD ( BODYID, 'RADII', 3, DIM, RADII )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'XFMSTA' )
               RETURN
            END IF

C
C           If either radius is less than or equal to zero, an error is 
C           signaled.
C
            IF  ( ( RADII(3) .LE. 0.D0 )   .OR. 
     .            ( RADII(1) .LE. 0.D0 ) ) THEN

               CALL SETMSG ( 'At least one radii is less '
     .                  //   'than or equal to zero. The equatorial '
     .                  //   'radius has a value of # and the polar '
     .                  //   'radius has has a value of #.'    )
               CALL ERRDP  ( '#', RADII(1) )
               CALL ERRDP  ( '#', RADII(3) )
               CALL SIGERR ( 'SPICE(INVALIDRADIUS)' )
               CALL CHKOUT ( 'XFMSTA'      )
               RETURN

            END IF
C
C           If the difference of the equatorial and polar radii
C           divided by the equatorial radius is greater than DPMAX,
C           a numeric overflow may occur, so an error is signaled.
C
            IF ( SQRT(ABS( RADII(1) - RADII(3) )) / 
     .           SQRT(ABS( RADII(1) )) .GE. SQRT(DPMAX()) ) THEN

               CALL SETMSG ( 'The equatorial radius for # '
     .                  //   'has a value of # and a polar radius of '
     .                  //   '#. The flattening coefficient cannot be '
     .                  //   'calculated due to numeric overflow.' )
               CALL ERRCH  ( '#', BODY     )
               CALL ERRDP  ( '#', RADII(1) )
               CALL ERRDP  ( '#', RADII(3) )
               CALL SIGERR ( 'SPICE(INVALIDRADIUS)' )
               CALL CHKOUT ( 'XFMSTA'      )
               RETURN

            END IF

            F = ( RADII(1) - RADII(3) ) / RADII(1)

         ELSE

            CALL SETMSG ( 'The input body name # does not '
     .               //   'have a valid NAIF ID code.' )
            CALL ERRCH  ( '#', BODY )
            CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'     )
            CALL CHKOUT ( 'XFMSTA'  )
            RETURN

         END IF

      END IF

C
C     Conversion of the input to rectangular coordinates
C     ----------------------------------------------------------------
C
C     First, the position and velocity coordinates will be converted
C     into rectangular coordinates. If the input system is not
C     rectangular, then the velocity coordinates must be translated
C     into rectangular using the Jacobian. If the input system is
C     rectangular, then the input state must simply be saved into IPOS
C     and IVEL.
C
C     TOOBIG is used for preventing numerical overflow. The square
C     roots of values are used to safely check if overflow will occur.
C
      TOOBIG = SQRT ( DPMAX() / MARGIN )

      IF  ( ISYS .NE. RECTAN ) THEN
C
C        To rectangular... 
C
         IF       ( ISYS .EQ. CYL )    THEN

C
C                  ... from cylindrical
C
            CALL CYLREC ( ISTATE(1), ISTATE(2), ISTATE(3), IPOS   )

            CALL DRDCYL ( ISTATE(1), ISTATE(2), ISTATE(3), JACOBI )


         ELSE IF  ( ISYS .EQ. LATNL )  THEN

C
C                  ... from latitudinal
C
            CALL LATREC ( ISTATE(1), ISTATE(2), ISTATE(3), IPOS   )

            CALL DRDLAT ( ISTATE(1), ISTATE(2), ISTATE(3), JACOBI )


         ELSE IF  ( ISYS .EQ. SPHCL )  THEN

C
C                  ... from spherical
C
            CALL SPHREC ( ISTATE(1), ISTATE(2), ISTATE(3), IPOS   )

            CALL DRDSPH ( ISTATE(1), ISTATE(2), ISTATE(3), JACOBI )


         ELSE IF  ( ISYS .EQ. GEODET ) THEN

C
C                  ... from geodetic
C
            CALL GEOREC ( ISTATE(1), ISTATE(2), ISTATE(3), RADII(1), 
     .                    F, IPOS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'XFMSTA' )
               RETURN
            END IF

            CALL DRDGEO ( ISTATE(1), ISTATE(2), ISTATE(3), RADII(1), 
     .                    F, JACOBI )


         ELSE IF  ( ISYS .EQ. PLNTGR ) THEN

C
C                  ... from planetographic
C
            CALL PGRREC ( BODY, ISTATE(1), ISTATE(2), ISTATE(3), 
     .                    RADII(1), F, IPOS   )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'XFMSTA' )
               RETURN
            END IF

            CALL DRDPGR ( BODY, ISTATE(1), ISTATE(2), ISTATE(3), 
     .                    RADII(1), F, JACOBI )

         ELSE 

            CALL SETMSG ( 'This error should never occur. '
     .               //   'This is an intermediate step in which a '
     .               //   'non-rectangular input state should be '
     .               //   'transferred to rectangular.  The input '
     .               //   'coordinate system is not recognized, yet '
     .               //   'was not caught by an earlier check.' )
            CALL SIGERR ( 'SPICE(BUG1)')
            CALL CHKOUT ( 'XFMSTA' )
            RETURN

         END IF 

C
C        Some DRD* routines are not error free. Be safe and check
C        FAILED to not use un-initialized JACOBI.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'XFMSTA' )
            RETURN
         END IF

C
C        If the multiplication of the Jacobian and velocity can cause
C        overflow, signal an error.
C
         DO  I = 1, 3 

            DO  J = 1, 3 

               SQTMP = SQRT ( ABS ( JACOBI(I,J) ) ) * 
     .                 SQRT ( ABS ( ISTATE(J+3) ) ) 

               IF ( SQTMP .GT. TOOBIG ) THEN

                  CALL SETMSG ( 'The product of the Jacobian '
     .                     //   'and velocity may cause numeric '
     .                     //   'overflow.' )
                  CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)' )
                  CALL CHKOUT ( 'XFMSTA'    )
                  RETURN

               END IF

            END DO

         END DO

C
C        Transform the velocity into rectangular coordinates.
C
         CALL MXV ( JACOBI, ISTATE(4), IVEL )


      ELSE IF  ( ISYS .EQ. RECTAN ) THEN

C
C        If the input coordinate system is rectangular, the input
C        position does not need to be translated into rectangular.
C
         CALL VEQU ( ISTATE(1), IPOS )
         CALL VEQU ( ISTATE(4), IVEL )

      ELSE 

         CALL SETMSG ( 'This error should never occur. '
     .            //   'This is an ELSE statement. If the input '
     .            //   'coordinate system is not rectangular, the '
     .            //   'IF should be executed. If the input '
     .            //   'coordinate system is rectangular, the ELSE IF '
     .            //   'should be executed.' )
         CALL SIGERR ( 'SPICE(BUG2)' )
         CALL CHKOUT ( 'XFMSTA'    )
         RETURN

      END IF

C
C     Conversion from rectangular into the output coordinates
C     ----------------------------------------------------------------
C
C     Convert to the output coordinate system. If the output 
C     coordinate system is not rectangular, four calculations must
C     be made:
C
C       1)  Verify the position and velocity are not along the z-axis.
C           If the position and velocity are along the z-axis, the
C           velocity can still be converted even though the 
C           Jacobian is not defined. If the position is along the
C           z-axis but the velocity is not, the velocity cannot be
C           converted to the output coordinate system.
C
C       2)  Calculate the Jacobian from rectangular to the output
C           coordinate system and verify the product of the Jacobian
C           and velocity will not cause numeric overflow.
C
C       3)  Transform the position to the output coordinate system.
C
C       4)  Transform the velocity to the output coordinates using
C           the Jacobian and the rectangular velocity IVEL.
C
      IF ( OSYS .NE. RECTAN ) THEN

C
C        From rectangular for the case when the input position is along
C        the z-axis ...
C
         IF ( ( ABS(IPOS(1)) + ABS(IPOS(2)) ) .EQ. 0.D0) THEN

            IF ( ( ABS(IVEL(1)) + ABS(IVEL(2)) ) .EQ. 0.D0 ) THEN
C
C              If the velocity is along the z-axis, then the velocity
C              can be computed in the output coordinate frame even
C              though the Jacobian is not defined.
C
               IF       ( OSYS .EQ. CYL )    THEN

C
C                  ... to cylindrical
C
                  CALL VPACK  ( 0.D0, 0.D0, IVEL(3), OSTATE(4) )

                  CALL RECCYL ( IPOS(1),  OSTATE(1), OSTATE(2), 
     .                          OSTATE(3) )


               ELSE IF  ( OSYS .EQ. LATNL )  THEN

C
C                  ... to latitudinal
C
                  CALL VPACK  ( IVEL(3), 0.D0, 0.D0, OSTATE(4) )

                  CALL RECLAT ( IPOS(1), OSTATE(1),  OSTATE(2),
     .                          OSTATE(3) )


               ELSE IF  ( OSYS .EQ. SPHCL )  THEN

C
C                  ... to spherical
C
                  CALL VPACK  ( IVEL(3), 0.D0, 0.D0, OSTATE(4) )

                  CALL RECSPH ( IPOS(1), OSTATE(1),  OSTATE(2),
     .                          OSTATE(3) )


               ELSE IF  ( OSYS .EQ. GEODET ) THEN

C
C                  ... to geodetic
C
                  CALL VPACK  ( 0.D0, 0.D0, IVEL(3),  OSTATE(4) )

                  CALL RECGEO ( IPOS(1), RADII(1), F, OSTATE(1),
     .                          OSTATE(2), OSTATE(3) )


               ELSE IF  ( OSYS .EQ. PLNTGR ) THEN

C
C                  ... to planetographic
C
                  CALL VPACK  ( 0.D0, 0.D0, IVEL(3), OSTATE(4) )

                  CALL RECPGR ( BODY, IPOS(1), RADII(1), F, OSTATE(1),
     .                          OSTATE(2), OSTATE(3) )

               ELSE 

                  CALL SETMSG ( 'This error should never '
     .                 //   'occur. This is an intermediate step in '
     .                 //   'which a position and velocity along '
     .                 //   'the z-axis are converted to a '
     .                 //   'non-rectangular coordinate system from '
     .                 //   'rectangular. The output coordinate '
     .                 //   'system is not recognized, yet was not '
     .                 //   'caught by an earlier check.' )
                  CALL SIGERR ( 'SPICE(BUG3)' )
                  CALL CHKOUT ( 'XFMSTA'    )
                  RETURN

               END IF
C
C              The output state has been calculated for the special
C              case of the position and velocity existing along the
C              z-axis.
C
               CALL CHKOUT ( 'XFMSTA' )
               RETURN
               
            ELSE
C
C              The Jacobian is undefined and the velocity cannot be
C              converted since it is not along the z-axis. 
C              Signal an error.
C
               CALL SETMSG ( 'Invalid input state: z axis.' )
               CALL SIGERR ( 'SPICE(INVALIDSTATE)' )
               CALL CHKOUT ( 'XFMSTA' )
               RETURN

            END IF

         END IF

C
C        From rectangular for cases when the input position is not along
C        the z-axis ...
C
         IF       ( OSYS .EQ. CYL )    THEN
C
C                  ... to cylindrical
C
            CALL DCYLDR ( IPOS(1), IPOS(2),   IPOS(3),   JACOBI    )
            
            CALL RECCYL ( IPOS(1), OSTATE(1), OSTATE(2), OSTATE(3) )

         ELSE IF  ( OSYS .EQ. LATNL )  THEN

C
C                  ... to latitudinal
C
            CALL DLATDR ( IPOS(1), IPOS(2),   IPOS(3),   JACOBI    )

            CALL RECLAT ( IPOS(1), OSTATE(1), OSTATE(2), OSTATE(3) )

         ELSE IF  ( OSYS .EQ. SPHCL )  THEN

C
C                  ... to spherical
C
            CALL DSPHDR ( IPOS(1), IPOS(2),   IPOS(3),   JACOBI    )

            CALL RECSPH ( IPOS(1), OSTATE(1), OSTATE(2), OSTATE(3) )

         ELSE IF  ( OSYS .EQ. GEODET ) THEN

C
C                  ... to geodetic
C
            CALL DGEODR ( IPOS(1), IPOS(2),  IPOS(3), RADII(1), F, 
     .                    JACOBI )

            CALL RECGEO ( IPOS(1), RADII(1), F, OSTATE(1), OSTATE(2), 
     .                    OSTATE(3) )

         ELSE IF  ( OSYS .EQ. PLNTGR ) THEN

C
C                  ... to planetographic
C
            CALL DPGRDR ( BODY, IPOS(1), IPOS(2), IPOS(3), RADII(1), 
     .                    F,    JACOBI )

            CALL RECPGR ( BODY, IPOS(1), RADII(1), F, OSTATE(1), 
     .                    OSTATE(2), OSTATE(3) )

         ELSE 

            CALL SETMSG ( 'This error should never occur. '
     .               //   'This is an intermediate step in which a '
     .               //   'state is converted to a non-rectangular '
     .               //   'coordinate system from rectangular. The '
     .               //   'output coordinate system is not recognized, '
     .               //   'yet was not caught by an earlier check.' )
            CALL SIGERR ( 'SPICE(BUG4)' )
            CALL CHKOUT ( 'XFMSTA'    )
            RETURN

         END IF

C
C        Many D*DR and REC* routines are not error free. Be safe and
C        check FAILED to not use un-initialized JACOBI.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'XFMSTA' )
            RETURN
         END IF

C
C        If the multiplication of the Jacobian and velocity can cause
C        overflow, signal an error.
C
         DO I = 1, 3 

            DO  J = 1, 3 

               SQTMP = SQRT ( ABS ( JACOBI(I,J) ) ) *
     .                 SQRT ( ABS ( IVEL(J)     ) )

               IF ( SQTMP .GT. TOOBIG ) THEN

                  CALL SETMSG ( 'The product of the Jacobian '
     .                     //   'and velocity may cause numeric '
     .                     //   'overflow.' )
                  CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)' )
                  CALL CHKOUT ( 'XFMSTA'    )
                  RETURN

               END IF

            END DO

         END DO

C
C        Calculate the velocity in the output coordinate system.
C
         CALL MXV ( JACOBI, IVEL, OSTATE(4) )


      ELSE IF ( OSYS .EQ. RECTAN ) THEN

C
C        If the output coordinate system is rectangular, the position
C        and velocity components of the output state are set equal to
C        the rectangular IPOS and IVEL, respectively, because the
C        components have already been converted to rectangular.
C
         CALL VEQU ( IPOS, OSTATE(1) )
         CALL VEQU ( IVEL, OSTATE(4) )

      ELSE 

         CALL SETMSG ( 'This error should never occur. '
     .            //   'This is an ELSE statement. If the output '
     .            //   'coordinate system is not rectangular, the '
     .            //   'IF should be executed. If the output '
     .            //   'coordinate system is rectangular, the ELSE IF '
     .            //   'should be executed.' )
         CALL SIGERR ( 'SPICE(BUG5)' )
         CALL CHKOUT ( 'XFMSTA'    )
         RETURN

      END IF

      CALL CHKOUT ( 'XFMSTA' )
      RETURN
      END


         
