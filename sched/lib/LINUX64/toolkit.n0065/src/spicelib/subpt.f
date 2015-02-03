C$Procedure      SUBPT ( Sub-observer point )
 
      SUBROUTINE SUBPT ( METHOD,  TARGET,  ET,         
     .                   ABCORR,  OBSRVR,  SPOINT,  ALT )
      
C$ Abstract
C
C     Deprecated: This routine has been superseded by the SPICELIB
C     routine SUBPNT. This routine is supported for purposes of
C     backward compatibility only.
C
C     Compute the rectangular coordinates of the sub-observer point on
C     a target body at a particular epoch, optionally corrected for
C     planetary (light time) and stellar aberration.  Return these
C     coordinates expressed in the body-fixed frame associated with the
C     target body.  Also, return the observer's altitude above the
C     target body.
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
C     FRAMES
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzctr.inc'
      
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      ALT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body.
C     ET         I   Epoch in ephemeris seconds past J2000 TDB.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observing body.
C     SPOINT     O   Sub-observer point on the target body.
C     ALT        O   Altitude of the observer above the target body.
C
C$ Detailed_Input
C
C     METHOD      is a short string specifying the computation method
C                 to be used.  The choices are:
C
C                    'Near point'       The sub-observer point is 
C                                       defined as the nearest point on
C                                       the target relative to the
C                                       observer.
C
C                    'Intercept'        The sub-observer point is
C                                       defined as the target surface
C                                       intercept of the line
C                                       containing the observer and the
C                                       target's center.
C                    
C                 In both cases, the intercept computation treats the
C                 surface of the target body as a triaxial ellipsoid.
C                 The ellipsoid's radii must be available in the kernel
C                 pool.
C
C                 Neither case nor white space are significant in 
C                 METHOD.  For example, the string ' NEARPOINT' is 
C                 valid.                 
C
C
C     TARGET      is the name of a target body.  Optionally, you may
C                 supply the integer ID code for the object as
C                 an integer string.  For example both 'MOON' and
C                 '301' are legitimate strings that indicate the 
C                 moon is the target body. This routine assumes
C                 that this body is modeled by a tri-axial ellipsoid,
C                 and that a PCK file containing its radii has been
C                 loaded into the kernel pool via FURNSH. 
C
C     ET          is the epoch in ephemeris seconds past J2000 at which
C                 the sub-observer point on the target body is to be
C                 computed.
C
C
C     ABCORR      indicates the aberration corrections to be applied
C                 when computing the observer-target state.  ABCORR
C                 may be any of the following.
C
C                    'NONE'     Apply no correction. Return the 
C                               geometric sub-observer point on the
C                               target body.
C
C                    'LT'       Correct for planetary (light time)
C                               aberration.  Both the state and rotation
C                               of the target body are corrected for 
C                               light time.
C
C                    'LT+S'     Correct for planetary (light time) and
C                               stellar aberrations. Both the state and
C                               rotation of the target body are 
C                               corrected for light time.
C
C
C                    'CN'       Converged Newtonian light time
C                               correction. In solving the light time
C                               equation, the 'CN' correction iterates
C                               until the solution converges (three
C                               iterations on all supported platforms).
C                               Whether the 'CN+S' solution is
C                               substantially more accurate than the
C                               'LT' solution depends on the geometry
C                               of the participating objects and on the
C                               accuracy of the input data. In all
C                               cases this routine will execute more
C                               slowly when a converged solution is
C                               computed. See the Particulars section
C                               of SPKEZR for a discussion of precision
C                               of light time corrections.
C
C                               Both the state and rotation of the
C                               target body are corrected for light
C                               time.
C
C                    'CN+S'     Converged Newtonian light time
C                               correction and stellar aberration
C                               correction.
C
C                               Both the state and rotation of the
C                               target body are corrected for light
C                               time.
C
C     OBSRVR      is the name of the observing body.  This is typically
C                 a spacecraft, the earth, or a surface point on the
C                 earth. Optionally, you  may supply the ID code of
C                 the object as an integer string. For example, both
C                 'EARTH' and '399' are legitimate strings to supply
C                 to indicate the observer is Earth.
C
C$ Detailed_Output
C
C     SPOINT      is the sub-observer point on the target body at ET
C                 expressed relative to the body-fixed frame of the
C                 target body.  
C
C                 The sub-observer point is defined either as the point
C                 on the target body that is closest to the observer,
C                 or the target surface intercept of the line from the
C                 observer to the target's center; the input argument
C                 METHOD selects the definition to be used.
C
C                 The body-fixed frame, which is time-dependent, is
C                 evaluated at ET if ABCORR is 'NONE'; otherwise the
C                 frame is evaluated at ET-LT, where LT is the one-way
C                 light time from target to observer.
C
C                 The state of the target body is corrected for 
C                 aberration as specified by ABCORR; the corrected 
C                 state is used in the geometric computation.  As 
C                 indicated above, the rotation of the target is 
C                 retarded by one-way light time if ABCORR specifies
C                 that light time correction is to be done.
C
C
C     ALT         is the "altitude" of the observer above the target
C                 body.  When METHOD specifies a "near point" 
C                 computation, ALT is truly altitude in the standard 
C                 geometric sense:  the length of a segment dropped from
C                 the observer to the target's surface, such that the
C                 segment is perpendicular to the surface at the 
C                 contact point SPOINT.
C
C                 When METHOD specifies an "intercept" computation, ALT
C                 is still the length of the segment from the observer
C                 to the surface point SPOINT, but this segment in
C                 general is not perpendicular to the surface.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If any of the listed errors occur, the output arguments are 
C     left unchanged.
C 
C
C     1) If the input argument METHOD is not recognized, the error
C        SPICE(DUBIOUSMETHOD) is signaled.
C
C     2) If either of the input body names TARGET or OBSRVR cannot be
C        mapped to NAIF integer codes, the error SPICE(IDCODENOTFOUND)
C        is signaled.
C
C     3) If OBSRVR and TARGET map to the same NAIF integer ID codes, the
C        error SPICE(BODIESNOTDISTINCT) is signaled.  
C
C     4) If frame definition data enabling the evaluation of the state 
C        of the target relative to the observer in target body-fixed
C        coordinates have not been loaded prior to calling SUBPT, the
C        error will be diagnosed and signaled by a routine in the call
C        tree of this routine.   
C
C     5) If the specified aberration correction is not recognized, the
C        error will be diagnosed and signaled by a routine in the call
C        tree of this routine.
C
C     6) If insufficient ephemeris data have been loaded prior to 
C        calling SUBPT, the error will be diagnosed and signaled by a 
C        routine in the call tree of this routine.  
C
C     7) If the triaxial radii of the target body have not been loaded
C        into the kernel pool prior to calling SUBPT, the error will be 
C        diagnosed and signaled by a routine in the call tree of this 
C        routine.   
C
C     8) The target must be an extended body:  if any of the radii of 
C        the target body are non-positive, the error will be diagnosed
C        and signaled by routines in the call tree of this routine.
C 
C     9) If PCK data supplying a rotation model for the target body 
C        have not been loaded prior to calling SUBPT, the error will be 
C        diagnosed and signaled by a routine in the call tree of this 
C        routine.   
C
C$ Files
C
C     Appropriate SPK, PCK, and frame kernels must be loaded
C     prior by the calling program before this routine is called.
C
C     The following data are required:
C
C        - SPK data:  ephemeris data for target and observer must be
C          loaded.  If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C          Typically ephemeris data are made available by loading one
C          or more SPK files via FURNSH.
C
C        - PCK data:  triaxial radii for the target body must be loaded
C          into the kernel pool.  Typically this is done by loading a 
C          text PCK file via FURNSH.
C
C        - Further PCK data:  rotation data for the target body must
C          be loaded.  These may be provided in a text or binary PCK
C          file.  Either type of file may be loaded via FURNSH.
C
C        - Frame data:  if a frame definition is required to convert
C          the observer and target states to the body-fixed frame of
C          the target, that definition must be available in the kernel
C          pool.  Typically the definition is supplied by loading a
C          frame kernel via FURNSH.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     SUBPT computes the sub-observer point on a target body.
C     (The sub-observer point is commonly called the sub-spacecraft
C     point when the observer is a spacecraft.)  SUBPT also
C     determines the altitude of the observer above the target body.
C
C     There are two different popular ways to define the sub-observer
C     point:  "nearest point on target to observer" or "target surface 
C     intercept of line containing observer and target."  These 
C     coincide when the target is spherical and generally are distinct
C     otherwise.
C
C     When comparing sub-point computations with results from sources
C     other than SPICE, it's essential to make sure the same geometric
C     definitions are used.  
C     
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms.  The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     In the following example program, the file
C
C        spk_m_031103-040201_030502.bsp
C
C     is a binary SPK file containing data for Mars Global Surveyor,
C     Mars, and the Sun for a time interval bracketing the date 
C
C         2004 JAN 1 12:00:00 UTC. 
C
C     pck00007.tpc is a planetary constants kernel file containing
C     radii and rotation model constants.  naif0007.tls is a
C     leapseconds kernel.
C
C     Find the sub-observer point of the Mars Global Surveyor (MGS)
C     spacecraft on Mars for a specified time.  Perform the computation
C     twice, using both the "intercept" and "near point" options.
C
C
C           IMPLICIT NONE
C
C           CHARACTER*25          METHOD ( 2 )
C
C           INTEGER               I
C
C           DOUBLE PRECISION      ALT
C           DOUBLE PRECISION      DPR
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      LAT      
C           DOUBLE PRECISION      LON      
C           DOUBLE PRECISION      RADIUS   
C           DOUBLE PRECISION      SPOINT ( 3 )
C
C           DATA                  METHOD / 'Intercept', 'Near point' /
C
C     C
C     C     Load kernel files.
C     C
C           CALL FURNSH ( 'naif0007.tls'                   )
C           CALL FURNSH ( 'pck00007.tpc'                   )
C           CALL FURNSH ( 'spk_m_031103-040201_030502.bsp' )
C
C     C
C     C     Convert the UTC request time to ET (seconds past  
C     C     J2000, TDB).
C     C     
C           CALL STR2ET ( '2004 JAN 1 12:00:00', ET )
C
C     C
C     C     Compute sub-spacecraft point using light time and stellar 
C     C     aberration corrections.  Use the "target surface intercept" 
C     C     definition of sub-spacecraft point on the first loop
C     C     iteration, and use the "near point" definition on the
C     C     second.
C     C
C           DO I = 1, 2
C
C              CALL SUBPT ( METHOD(I),  
C          .               'MARS',     ET,     'LT+S', 
C          .               'MGS',      SPOINT,  ALT    )
C
C     C
C     C        Convert rectangular coordinates to planetocentric
C     C        latitude and longitude.  Convert radians to degrees.
C     C
C              CALL RECLAT ( SPOINT, RADIUS, LON, LAT  )
C
C              LON = LON * DPR ()
C              LAT = LAT * DPR ()
C
C     C
C     C        Write the results.
C     C
C              WRITE (*,*) ' '
C              WRITE (*,*) 'Computation method: ', METHOD(I)
C              WRITE (*,*) ' '
C              WRITE (*,*) '  Radius                   (km)  = ', RADIUS
C              WRITE (*,*) '  Planetocentric Latitude  (deg) = ', LAT
C              WRITE (*,*) '  Planetocentric Longitude (deg) = ', LON
C              WRITE (*,*) '  Altitude                 (km)  = ', ALT
C              WRITE (*,*) ' '
C
C           END DO
C
C           END
C
C
C     When this program is executed, the output will be: 
C
C
C        Computation method: Intercept
C
C          Radius                   (km)  =   3387.97077
C          Planetocentric Latitude  (deg) =  -39.7022724
C          Planetocentric Longitude (deg) =  -159.226663
C          Altitude                 (km)  =   373.173506
C
C
C        Computation method: Near point
C
C          Radius                   (km)  =   3387.9845
C          Planetocentric Latitude  (deg) =  -39.6659329
C          Planetocentric Longitude (deg) =  -159.226663
C          Altitude                 (km)  =   373.166636
C
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
C     C.H. Acton     (JPL)
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL) 
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 04-JUL-2014 (NJB) (BVS)
C
C        Discussion of light time corrections was updated. Assertions
C        that converged light time corrections are unlikely to be
C        useful were removed.
C
C     Last update was 19-SEP-2013 (BVS)
C
C        Updated to save the input body names and ZZBODTRN state
C        counters and to do name-ID conversions only if the counters
C        have changed.
C
C-    SPICELIB Version 1.2.3, 18-MAY-2010 (BVS) 
C
C        Index line now states that this routine is deprecated.
C
C-    SPICELIB Version 1.2.2, 17-MAR-2009 (EDW) 
C
C        Typo correction in Required_Reading, changed 
C        FRAME to FRAMES.
C
C-    SPICELIB Version 1.2.1, 07-FEB-2008 (NJB) 
C
C        Abstract now states that this routine is deprecated.
C
C-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
C
C        Replaced call to BODVAR with call to BODVCD.
C
C-    SPICELIB Version 1.1.0, 21-JUL-2004 (EDW)
C
C        Changed BODN2C call to BODS2C giving the routine
C        the capability to accept string representations of
C        interger IDs for TARGET and OBSRVR.
C
C-    SPICELIB Version 1.0.1, 27-JUL-2003 (NJB) (CHA)
C
C        Various header corrections were made.  The example program
C        was upgraded to use real kernels, and the program's output is
C        shown.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1999 (NJB) (JEM)
C
C-&
 
C$ Index_Entries
C
C     DEPRECATED sub-observer point
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDIST
      
      LOGICAL               EQSTR
      LOGICAL               RETURN
      
C
C     Local parameters
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN =  80 )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )
     
C
C     Local variables
C
      CHARACTER*(FRNMLN)    FRNAME

      DOUBLE PRECISION      LT
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      TSTATE ( 6 )
 
      INTEGER               FRCODE
      INTEGER               NRADII
      INTEGER               OBSCDE
      INTEGER               TRGCDE

      LOGICAL               FOUND

C
C     Saved name/ID item declarations.
C
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTCDE
      LOGICAL               SVFND1

      INTEGER               SVCTR2 ( CTRSIZ )
      CHARACTER*(MAXL)      SVOBSR
      INTEGER               SVOBSC
      LOGICAL               SVFND2

      LOGICAL               FIRST
 
C
C     Saved variables
C
      SAVE                  ORIGIN

C
C     Saved name/ID items.
C
      SAVE                  SVCTR1
      SAVE                  SVTARG
      SAVE                  SVTCDE
      SAVE                  SVFND1

      SAVE                  SVCTR2
      SAVE                  SVOBSR
      SAVE                  SVOBSC
      SAVE                  SVFND2

      SAVE                  FIRST
      
C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.0D0 /

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SUBPT' )
      END IF
  
C
C     Initialization.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )

         FIRST = .FALSE.

      END IF

C
C     Obtain integer codes for the target and observer.
C
C     Target...
C 
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTCDE, SVFND1,
     .                TARGET, TRGCDE, FOUND    )

      IF ( .NOT. FOUND ) THEN
      
         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SUBPT'                                    )
         RETURN
      
      END IF

C
C     ...observer. 
C
      CALL ZZBODS2C ( SVCTR2, SVOBSR, SVOBSC, SVFND2,
     .                OBSRVR, OBSCDE, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'SUBPT'                                    )
         RETURN
      
      END IF
      
 
C
C     Check the input body codes.  If they are equal, signal
C     an error.
C
      IF ( OBSCDE .EQ. TRGCDE ) THEN
 
         CALL SETMSG ( 'In computing the sub-observer point, the '    //
     .                 'observing body and target body are the same. '//
     .                 'Both are #.'                                  )
         CALL ERRCH  ( '#',  OBSRVR                                   )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                     )
         CALL CHKOUT ( 'SUBPT'                                        )
         RETURN
 
      END IF
 
C
C     Get the radii of the target body from the kernel pool.
C
      CALL BODVCD ( TRGCDE, 'RADII', 3, NRADII, RADII )
  
C
C     Find the name of the body-fixed frame associated with the
C     target body.  We'll want the state of the target relative to
C     the observer in this body-fixed frame.
C
      CALL CIDFRM ( TRGCDE, FRCODE, FRNAME, FOUND )

      IF ( .NOT. FOUND ) THEN
      
         CALL SETMSG ( 'No body-fixed frame is associated with '   //
     .                 'target body #; a frame kernel must be '    //
     .                 'loaded to make this association.  Consult '//
     .                 'the FRAMES Required Reading for details.'   )
         CALL ERRCH  ( '#', TARGET                                  )
         CALL SIGERR ( 'SPICE(NOFRAME)'                             )
         CALL CHKOUT ( 'SUBPT'                                      )
         RETURN
 
      END IF
 
C
C     Determine the position of the observer in target
C     body-fixed coordinates.
C
C         -  Call SPKEZR to compute the position of the target
C            body as seen from the observing body and the light time
C            (LT) between them.  SPKEZR returns a state which is
C            the position and velocity, but we'll only use the position
C            which is the first three elements.  We request that the
C            coordinates of POS be returned relative to the body fixed
C            reference frame associated with the target body, using
C            aberration corrections specified by the input argument
C            ABCORR.
C
C         -  Call VMINUS to negate the direction of the vector (POS)
C            so it will be the position of the observer as seen from
C            the target body in target body fixed coordinates.
C
C            Note that this result is not the same as the result of
C            calling SPKEZR with the target and observer switched.  We
C            computed the vector FROM the observer TO the target in
C            order to get the proper light time and stellar aberration
C            corrections (if requested).  Now we need the inverse of
C            that corrected vector in order to compute the sub-point.
C
      CALL SPKEZ ( TRGCDE, ET, FRNAME, ABCORR, OBSCDE, TSTATE, LT )
 
C
C     Negate the target's state to obtain the position of the observer
C     relative to the target.
C
      CALL VMINUS ( TSTATE, POS )
 

C
C     Find the sub-point and "altitude" (distance from observer to 
C     sub-point) using the specified geometric definition.
C
      IF (  EQSTR( METHOD, 'Near point' )  ) THEN 

C
C        Locate the nearest point to the observer on the target.
C
         CALL NEARPT ( POS, RADII(1), RADII(2), RADII(3), SPOINT, ALT ) 
 
      ELSE IF (  EQSTR( METHOD, 'Intercept' )  ) THEN
      
         CALL SURFPT ( ORIGIN,  POS,   RADII(1),  RADII(2),  RADII(3), 
     .                 SPOINT,  FOUND                                 )

C
C        Since the line in question passes through the center of the
C        target, there will always be a surface intercept.  So we should
C        never have FOUND = .FALSE.
C
         IF ( .NOT. FOUND ) THEN
         
            CALL SETMSG ( 'Call to SURFPT returned FOUND=FALSE even ' //
     .                    'though vertex of ray is at target center. '//
     .                    'This indicates a bug. Please contact NAIF.')
            CALL SIGERR ( 'SPICE(BUG)'                                )
            CALL CHKOUT ( 'SUBPT'                                     )
            RETURN
            
         END IF
         
C
C        SURFPT doesn't compute altitude, so do it here.
C
         ALT  =  VDIST ( POS, SPOINT )
         
      ELSE
      
         CALL SETMSG ( 'The computation method # was not recognized. '//
     .                 'Allowed values are "Near point" and '         //
     .                 '"Intercept."'                                 )
         CALL ERRCH  ( '#',  METHOD                                   )
         CALL SIGERR ( 'SPICE(DUBIOUSMETHOD)'                         )
         CALL CHKOUT ( 'SUBPT'                                        )
         RETURN
         
      END IF


      CALL CHKOUT ( 'SUBPT' )
      RETURN
      END
