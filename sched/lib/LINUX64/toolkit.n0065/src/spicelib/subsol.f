C$Procedure SUBSOL ( Sub-solar point )
 
      SUBROUTINE SUBSOL ( METHOD, TARGET, ET, ABCORR, OBSRVR, SPOINT )
      
C$ Abstract
C
C     Deprecated: This routine has been superseded by the SPICELIB
C     routine SUBSLR. This routine is supported for purposes of
C     backward compatibility only.
C
C     Determine the coordinates of the sub-solar point on a target
C     body as seen by a specified observer at a specified epoch, 
C     optionally corrected for planetary (light time) and stellar
C     aberration.  
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
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body.
C     ET         I   Epoch in ephemeris seconds past J2000 TDB.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observing body.
C     SPOINT     O   Sub-solar point on the target body.
C
C$ Detailed_Input
C
C     METHOD      is a short string specifying the computation method
C                 to be used.  The choices are:
C
C                    'Near point'       The sub-solar point is defined
C                                       as the nearest point on the
C                                       target to the sun. 
C
C                    'Intercept'        The sub-observer point is
C                                       defined as the target surface
C                                       intercept of the line
C                                       containing the target's center
C                                       and the sun's center.
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
C     TARGET      is the name of the target body.  TARGET is
C                 case-insensitive, and leading and trailing blanks in
C                 TARGET are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object.  For example both 'MOON' and '301' are
C                 legitimate strings that indicate the moon is the
C                 target body.
C
C                 This routine assumes that the target body is modeled
C                 by a tri-axial ellipsoid, and that a PCK file
C                 containing its radii has been loaded into the kernel
C                 pool via FURNSH.
C
C
C     ET          is the epoch in ephemeris seconds past J2000 at which
C                 the sub-solar point on the target body is to be
C                 computed.
C
C
C     ABCORR      indicates the aberration corrections to be applied
C                 when computing the observer-target state.  ABCORR
C                 may be any of the following.
C
C                    'NONE'     Apply no correction. Return the
C                               geometric sub-solar point on the target
C                               body.
C
C                    'LT'       Correct for planetary (light time)
C                               aberration.  Both the state and rotation
C                               of the target body are corrected for one
C                               way light time from target to observer.
C
C                               The state of the sun relative to the 
C                               target is corrected for one way light
C                               from the sun to the target; this state
C                               is evaluated at the epoch obtained by
C                               retarding ET by the one way light time
C                               from target to observer.
C
C                    'LT+S'     Correct for planetary (light time) and
C                               stellar aberrations.  Light time 
C                               corrections are the same as in the 'LT'
C                               case above.  The target state is 
C                               additionally corrected for stellar
C                               aberration as seen by the observer, and
C                               the sun state is corrected for stellar
C                               aberration as seen from the target. 
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
C                               of light time corrections. Light time
C                               corrections are applied as in the 'LT'
C                               case.
C                      
C                    'CN+S'     Converged Newtonian light time
C                               corrections and stellar aberration
C                               correction. Light time and stellar
C                               aberration corrections are applied as
C                               in the 'LT+S' case.
C
C
C     OBSRVR      is the name of the observing body, typically a
C                 spacecraft, the earth, or a surface point on the
C                 earth.  OBSRVR is case-insensitive, and leading and
C                 trailing blanks in OBSRVR are not significant.
C                 Optionally, you may supply a string containing the
C                 integer ID code for the object.  For example both
C                 'EARTH' and '399' are legitimate strings that indicate
C                 the earth is the observer.
C
C$ Detailed_Output
C
C     SPOINT      is the sub-solar point on the target body at ET
C                 expressed relative to the body-fixed frame of the
C                 target body.
C
C                 The sub-solar point is defined either as the point on
C                 the target body that is closest to the sun, or the
C                 target surface intercept of the line containing the
C                 target's center and the sun's center; the input 
C                 argument METHOD selects the definition to be used.
C
C                 The body-fixed frame, which is time-dependent, is
C                 evaluated at ET if ABCORR is 'NONE'; otherwise the
C                 frame is evaluated at ET-LT, where LT is the one way
C                 light time from target to observer.
C
C                 The state of the target body is corrected for 
C                 aberration as specified by ABCORR; the corrected 
C                 state is used in the geometric computation.  As 
C                 indicated above, the rotation of the target is 
C                 retarded by one way light time if ABCORR specifies
C                 that light time correction is to be done.
C    
C                 The state of the sun as seen from the observing 
C                 body is also corrected for aberration as specified
C                 by ABCORR.  The corrections, when selected, are
C                 applied at the epoch ET-LT, where LT is the one way
C                 light time from target to observer.
C
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
C        coordinates have not been loaded prior to calling SUBSOL, the
C        error will be diagnosed and signaled by a routine in the call
C        tree of this routine.   
C
C     5) If the specified aberration correction is not recognized, the
C        error will be diagnosed and signaled by a routine in the call
C        tree of this routine.
C
C     6) If insufficient ephemeris data have been loaded prior to 
C        calling SUBSOL, the error will be diagnosed and signaled by a 
C        routine in the call tree of this routine.  
C
C     7) If the triaxial radii of the target body have not been loaded
C        into the kernel pool prior to calling SUBSOL, the error will be
C        diagnosed and signaled by a routine in the call tree of this 
C        routine.   
C
C     8) The target must be an extended body:  if any of the radii of 
C        the target body are non-positive, the error will be diagnosed
C        and signaled by routines in the call tree of this routine.
C 
C     9) If PCK data supplying a rotation model for the target body 
C        have not been loaded prior to calling SUBSOL, the error will be
C        diagnosed and signaled by a routine in the call tree of this 
C        routine.   
C
C$ Files
C
C     Appropriate SPK, PCK, and frame data must be available to 
C     the calling program before this routine is called.  Typically
C     the data are made available by loading kernels; however the
C     data may be supplied via subroutine interfaces if applicable.
C
C     The following data are required:
C
C        - SPK data:  ephemeris data for sun, target, and observer must 
C          be loaded.  If aberration corrections are used, the states of
C          sun, target, and observer relative to the solar system 
C          barycenter must be calculable from the available ephemeris
C          data. Ephemeris data are made available by loading
C          one or more SPK files via FURNSH.
C
C        - PCK data:  triaxial radii for the target body must be loaded
C          into the kernel pool.  Typically this is done by loading a 
C          text PCK file via FURNSH.
C
C        - Further PCK data:  a rotation model for the target body must
C          be loaded.  This may be provided in a text or binary PCK
C          file which is loaded via FURNSH.
C
C        - Frame data:  if a frame definition is required to convert
C          the sun, observer, and target states to the body-fixed frame
C          of the target, that definition must be available in the 
C          kernel pool.  Typically the definition is supplied by loading
C          a frame kernel via FURNSH.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     SUBSOL computes the sub-solar point on a target body, as seen by
C     a specified observer.
C
C     There are two different popular ways to define the sub-solar
C     point:  "nearest point on target to the sun" or "target surface
C     intercept of line containing target and sun."  These coincide
C     when the target is spherical and generally are distinct otherwise.
C
C     When comparing sub-point computations with results from sources
C     other than SPICE, it's essential to make sure the same geometric
C     definitions are used.  
C     
C$ Examples
C
C
C     In the following example program, the file MGS.BSP is a
C     hypothetical binary SPK ephemeris file containing data for the
C     Mars Global Surveyor orbiter.  The SPK file de405s.bsp contains
C     data for the planet barycenters as well as the Earth, Moon, and
C     Sun for the time period including the date 1997 Dec 31 12:000
C     UTC. MGS0000A.TPC is a planetary constants kernel file
C     containing radii and rotation model constants.  MGS00001.TLS is
C     a leapseconds file.  (File names shown here that are specific 
C     to MGS are not names of actual files.)
C
C           IMPLICIT NONE
C
C           CHARACTER*25          METHOD ( 2 )
C
C           INTEGER               I
C
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
C           CALL FURNSH ( 'MGS00001.TLS' )
C           CALL FURNSH ( 'MGS0000A.TPC' )
C           CALL FURNSH ( 'de405s.bsp'   )
C           CALL FURNSH ( 'MGS.BSP'      )
C
C     C
C     C     Convert the UTC request time to ET (seconds past
C     C     J2000, TDB).
C     C
C           CALL STR2ET ( '1997 Dec 31 12:00:00', ET )
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
C              CALL SUBSOL ( METHOD(I),
C          .                 'MARS',  ET,  'LT+S',  'MGS',  SPOINT )
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
C              WRITE (*,*) ' '
C
C           END DO
C
C           END
C
C$ Restrictions
C
C     The appropriate kernel data must have been loaded before this 
C     routine is called.  See the Files section above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 04-JUL-2014 (NJB)(BVS)
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
C        Call to BODVAR was replaced with call to BODVCD.
C
C-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB) 
C
C        Updated to support representations of integers in the input
C        arguments TARGET and OBSRVR.   Deleted references in header to
C        kernel-specific loaders. Made miscellaneous minor corrections
C        to header comments.
C
C-    SPICELIB Version 1.0.2, 12-DEC-2002 (NJB)
C
C        Corrected and updated code example in header.
C
C-    SPICELIB Version 1.0.1, 1-NOV-1999 (WLT)
C
C        Declared routine LTIME to be external.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1999 (NJB) (JEM)
C
C-&
 
C$ Index_Entries
C
C     DEPRECATED sub-solar point
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB) 
C
C        Updated to support representations of integers in the
C        input arguments TARGET and OBSRVR:  calls to BODN2C
C        were replaced by calls to BODS2C.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
      LOGICAL               RETURN
      EXTERNAL              LTIME 
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
 
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SUNLT
 
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
         CALL CHKIN ( 'SUBSOL' )
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
         CALL CHKOUT ( 'SUBSOL'                                   )
         RETURN
      
      END IF
      
      
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
         CALL CHKOUT ( 'SUBSOL'                                   )
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
         CALL CHKOUT ( 'SUBSOL'                                       )
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
         CALL CHKOUT ( 'SUBSOL'                                     )
         RETURN
 
      END IF
      
C
C     If we're using aberration corrections, we'll need the 
C     one way light time from the target to the observer.  Otherwise,
C     we set the time time to zero.
C
      IF (  EQSTR ( ABCORR, 'NONE' )  ) THEN
         
         LT      =  0.0D0
         ETTARG  =  ET
         
      ELSE
      
         CALL LTIME ( ET, OBSCDE, '<-', TRGCDE, ETTARG, LT )
         
      END IF
      
C
C     Determine the position of the sun in target body-fixed
C     coordinates.
C
C     Call SPKEZ to compute the position of the sun as seen from the
C     target body and the light time between them SUNLT.  This state is
C     evaluated at the target epoch ETTARG. We request that the
C     coordinates of the target-sun position vector POS be returned
C     relative to the body fixed reference frame associated with the
C     target body, using aberration corrections specified by the input
C     argument ABCORR.
C
      CALL SPKPOS ( 'SUN', ETTARG, FRNAME, ABCORR, TARGET, POS, SUNLT )
  
C
C     Find the sub-solar point using the specified geometric definition.
C
      IF (  EQSTR( METHOD, 'Near point' )  ) THEN 
C
C        Locate the nearest point to the sun on the target.
C
         CALL NEARPT ( POS, RADII(1), RADII(2), RADII(3), SPOINT, ALT )
 
 
      ELSE IF (  EQSTR( METHOD, 'Intercept' )  ) THEN
      
         CALL SURFPT ( ORIGIN,  POS,   RADII(1),  RADII(2),  RADII(3), 
     .                 SPOINT,  FOUND                                  )

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
            CALL CHKOUT ( 'SUBSOL'                                    )
            RETURN
            
         END IF
         
         
         
      ELSE
      
         CALL SETMSG ( 'The computation method # was not recognized. '//
     .                 'Allowed values are "Near point" and '         //
     .                 '"Intercept."'                                 )
         CALL ERRCH  ( '#',  METHOD                                   )
         CALL SIGERR ( 'SPICE(DUBIOUSMETHOD)'                         )
         CALL CHKOUT ( 'SUBSOL'                                       )
         RETURN
         
      END IF


      CALL CHKOUT ( 'SUBSOL' )
      RETURN
      END
