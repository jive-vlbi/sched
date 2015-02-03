C$Procedure      ZZGFSSOB ( GF, state of sub-observer point )
 
      SUBROUTINE ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF,    
     .                      ABCORR, OBSID, RADII, STATE  )
 
C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Return the state of a sub-observer point used to define
C     coordinates referenced in a GF search.
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
C     GF
C     SPK
C     TIME
C     NAIF_IDS
C     FRAMES
C
C$ Keywords
C
C     GEOMETRY
C     PRIVATE
C     SEARCH
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

      CHARACTER*(*)         METHOD
      INTEGER               TRGID
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      INTEGER               OBSID
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TRGID      I   Target ID code.
C     ET         I   Computation epoch.
C     FIXREF     I   Reference frame name.
C     ABCORR     I   Aberration correction.
C     OBSID      I   Observer ID code.
C     RADII      I   Target radii.
C     STATE      O   State used to define coordinates.
C
C$ Detailed_Input
C
C     METHOD      is a short string providing parameters defining
C                 the computation method to be used. Any value
C                 supported by SUBPNT may be used.
C
C
C     TRGID      is the NAIF ID code of the target object.
C
C                *This routine assumes that the target is modeled
C                as a tri-axial ellipsoid.*
C
C
C     ET         is the time, expressed as ephemeris seconds past J2000
C                TDB, at which the specified state is to be computed.
C
C
C     FIXREF     is the name of the reference frame relative to which
C                the state of interest is specified.
C
C                FIXREF must be centered on the target body.
C
C                Case, leading and trailing blanks are not significant
C                in the string FIXREF.
C
C     
C     ABCORR     indicates the aberration corrections to be applied to
C                the state of the target body to account for one-way
C                light time and stellar aberration. The orientation
C                of the target body will also be corrected for one-way
C                light time when light time corrections are requested.
C
C                Supported aberration correction options for
C                observation (case where radiation is received by
C                observer at ET) are:
C
C                   NONE           No correction.
C                   LT             Light time only.
C                   LT+S           Light time and stellar aberration.
C                   CN             Converged Newtonian (CN) light time.
C                   CN+S           CN light time and stellar aberration.
C
C                Supported aberration correction options for
C                transmission (case where radiation is emitted from
C                observer at ET) are:
C
C                   XLT            Light time only.
C                   XLT+S          Light time and stellar aberration.
C                   XCN            Converged Newtonian (CN) light time.
C                   XCN+S          CN light time and stellar aberration.
C
C                For detailed information, see the geometry finder
C                required reading, gf.req.  Also see the header of
C                SPKEZR, which contains a detailed discussion of
C                aberration corrections.
C
C                Case, leading and trailing blanks are not significant
C                in the string ABCORR.
C
C
C     OBSID      is the NAIF ID code of the observer. 
C
C
C     RADII      is an array containing three radii defining
C                a reference ellipsoid for the target body.
C
C$ Detailed_Output
C
C     STATE     is the state of the sub-observer point at ET.
C               The first three components of STATE contain the
C               sub-observer point itself; the last three 
C               components contain the derivative with respect to
C               time of the position. The state is expressed 
C               relative to the body-fixed frame designated by
C               FIXREF.
C
C               Units are km and km/s.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the aberration correction ABCORR is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     2)  If the frame FIXREF is not recognized by the frames
C         subsystem, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C     3)  FIXREF must be centered on the target body; if it isn't,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C     
C     4)  Any error that occurs while look up the state of the target
C         or observer will be diagnosed by routines in the call tree of
C         this routine.
C
C     5)  Any error that occurs while look up the orientation of
C         the target will be diagnosed by routines in the call tree of
C         this routine.
C
C     6)  If the input method is not recognized, the error 
C         SPICE(NOTSUPPORTED) will be signaled.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target and observer must be
C          loaded. If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C          Typically ephemeris data are made available by loading one
C          or more SPK files via FURNSH.
C
C        - PCK data: if the target body shape is modeled as an
C          ellipsoid, triaxial radii for the target body must be loaded
C          into the kernel pool. Typically this is done by loading a
C          text PCK file via FURNSH.
C
C        - Further PCK data: rotation data for the target body must be
C          loaded. These may be provided in a text or binary PCK file.
C
C        - Frame data: if a frame definition is required to convert the
C          observer and target states to the body-fixed frame of the
C          target, that definition must be available in the kernel
C          pool. Typically the definition is supplied by loading a
C          frame kernel via FURNSH.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This routine isolates the computation of the sub-observer state
C     (that is, the sub-observer point and its derivative with respect
C     to time).
C
C     This routine is used by the GF coordinate utility routines in
C     order to solve for time windows on which specified mathematical
C     conditions involving coordinates are satisfied. The role of
C     this routine is to provide Cartesian state vectors enabling
C     the GF coordinate utilities to determine the signs of the
C     derivatives with respect to time of coordinates of interest.
C     
C$ Examples
C
C     See ZZGFCOST.
C
C$ Restrictions
C
C     1)  This routine is restricted to use with ellipsoidal target
C         shape models.
C
C     2)  The computations performed by this routine are intended
C         to be compatible with those performed by the SPICE
C         routine SUBPNT. If that routine changes, this routine
C         may need to be updated.
C
C     3)  This routine presumes that error checking of inputs
C         has, where possible, already been performed by the 
C         GF coordinate utility initialization routine.
C
C     4)  The interface and functionality of this set of routines may
C         change without notice. These routines should be called only
C         by SPICELIB routines.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0 12-MAY-2009 (NJB)
C
C        Upgraded to support targets and observers having 
C        no names associated with their ID codes.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
C
C-&

C$ Index_Entries
C
C     sub-observer state
C
C-&       

C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDOT 

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 1.D0 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )
      
C
C     Local variables
C
      CHARACTER*(BDNMLN)    SVOBS
      CHARACTER*(BDNMLN)    SVTARG

      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      CORXFI ( 6, 6 )
      DOUBLE PRECISION      CORXFM ( 6, 6 )
      DOUBLE PRECISION      DALT   ( 2 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      FXOSTA ( 6 )
      DOUBLE PRECISION      FXPSTA ( 6 )
      DOUBLE PRECISION      FXPVEL ( 3 )
      DOUBLE PRECISION      FXTSTA ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSPNT ( 6 )
      DOUBLE PRECISION      OBSSTA ( 6, 2 )
      DOUBLE PRECISION      OBSTRG ( 6 )
      DOUBLE PRECISION      PNTSTA ( 6 )
      DOUBLE PRECISION      RAYSTA ( 6 )
      DOUBLE PRECISION      SA     ( 3 )
      DOUBLE PRECISION      SASTAT ( 6 )
      DOUBLE PRECISION      SAVEL  ( 3 )
      DOUBLE PRECISION      SCALE  
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      SSBTG0 ( 6 )
      DOUBLE PRECISION      SSBTRG ( 6 )
      DOUBLE PRECISION      STEMP  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      UPOS   ( 3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      INTEGER               CENTER
      INTEGER               CLSSID
      INTEGER               FRCLSS
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               PRVOBS
      INTEGER               PRVTRG

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               GEOM
      LOGICAL               NEAR
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

C
C     Saved variables
C
      SAVE                  FIRST
      SAVE                  SVOBS
      SAVE                  PRVOBS
      SAVE                  PRVTRG
      SAVE                  SVTARG

C
C     Initial values 
C
      DATA                  FIRST / .TRUE. /

      DATA                  PRVOBS /  0 /
      DATA                  PRVTRG /  0 /

      DATA                  SVOBS / ' ' /
      DATA                  SVTARG / ' ' /



C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFSSOB' ) 


      IF (  FIRST  .OR. ( TRGID .NE. PRVTRG )  ) THEN

         CALL BODC2S ( TRGID, SVTARG )

         PRVTRG = TRGID

      END IF


      IF (  FIRST  .OR. ( OBSID .NE. PRVOBS )  ) THEN

         CALL BODC2S ( OBSID, SVOBS )

         PRVOBS = OBSID

      END IF

      FIRST = .FALSE.

C
C     Parse the aberration correction specifier.
C
      CALL ZZPRSCOR ( ABCORR, ATTBLK )

      GEOM   = ATTBLK( GEOIDX )
      USELT  = ATTBLK( LTIDX  )
      USESTL = ATTBLK( STLIDX )
      XMIT   = ATTBLK( XMTIDX )

C
C     Decide whether the sub-observer point is computed using
C     the "near point" or "surface intercept" method. Only
C     ellipsoids may be used a shape models for this computation.
C
      IF (  EQSTR( METHOD, 'Near point: ellipsoid' )  ) THEN
         
         NEAR = .TRUE.

      ELSE IF (  EQSTR( METHOD, 'Intercept: ellipsoid' )  ) THEN

         NEAR = .FALSE.

      ELSE

         CALL SETMSG ( 'Sub-observer point computation method # '
     .   //            'is not supported by this routine.'       )
         CALL ERRCH  ( '#', METHOD                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
         CALL CHKOUT ( 'ZZGFSSOB'                                )
         RETURN

      END IF


      IF ( GEOM ) THEN
C
C        This is the geometric case.
C
C        We need to check the body-fixed reference frame here.
C
         CALL NAMFRM ( FIXREF, FRCODE )
         CALL FRINFO ( FRCODE, CENTER, FRCLSS, CLSSID, FND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSSOB' )
            RETURN
         END IF

         IF ( .NOT. FND ) THEN

            CALL SETMSG ( 'Input reference frame # was not '
     .      //            'recognized.'                     )
            CALL ERRCH  ( '#', FIXREF                       )
            CALL SIGERR ( 'SPICE(NOFRAME)'                  )
            CALL CHKOUT ( 'ZZGFSSOB'                        )
            RETURN
            
         END IF

         IF ( CENTER .NE. TRGID ) THEN

            CALL SETMSG ( 'Input reference frame # is centered '
     .      //            'on body # instead of body #.'        )
            CALL ERRCH  ( '#', FIXREF                           )
            CALL ERRINT ( '#', CENTER                           )
            CALL ERRINT ( '#', TRGID                            )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'                 )
            CALL CHKOUT ( 'ZZGFSSOB'                            )
            RETURN

         END IF

C
C        Get the state of the target with respect to the observer,
C        expressed relative to the target body-fixed frame. We don't
C        need to propagate states to the solar system barycenter in
C        this case.
C
         CALL SPKGEO ( TRGID, ET, FIXREF, OBSID, FXTSTA, LT )
           
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSSOB' )
            RETURN
         END IF

C
C        Compute the state of the observer with respect to the target
C        in the body-fixed frame.
C
         CALL VMINUG ( FXTSTA, 6, FXOSTA )

C
C        Now we can obtain the surface velocity of the sub-observer
C        point.
C
         IF ( NEAR ) THEN
C
C           The sub-observer point method is "near point."
C
            CALL DNEARP ( FXOSTA, RADII(1), RADII(2), RADII(3),
     .                    FXPSTA, DALT,     FOUND              )

            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'The sub-observer state could '
     .         //            'could not be computed because '
     .         //            'the velocity was not well '
     .         //            'defined. DNEARP returned "not found."' )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'                 )
               CALL CHKOUT ( 'ZZGFSSOB'                              )
               RETURN

            END IF

         ELSE
C
C           The sub-observer point method is "surface
C           intercept point." The ray direction is simply
C           the negative of the observer's position relative
C           to the target center.
C
            CALL VMINUG ( FXOSTA, 6, RAYSTA )

            CALL SURFPV ( FXOSTA,   RAYSTA, 
     .                    RADII(1), RADII(2), RADII(3), FXPSTA, FOUND )

C
C           Although in general it's not an error for SURFPV to
C           be unable to compute an intercept state, it *is*
C           an error in this case, since the ray points toward
C           the center of the target.
C
            IF ( .NOT. FOUND ) THEN

               CALL SETMSG ( 'The sub-observer state could '
     .         //            'could not be computed because '
     .         //            'the velocity was not well '
     .         //            'defined. SURFPV returned "not found."' )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'                 )
               CALL CHKOUT ( 'ZZGFSSOB'                              )
               RETURN

            END IF
 
         END IF


      ELSE IF ( USELT ) THEN
C
C        Light time and possibly stellar aberration corrections are 
C        applied.
C
C        Most our work consists of getting ready to call either of the
C        SPICELIB routines DNEARP or SURFPV. In order to make this
C        call, we'll need the velocity of the observer relative to the
C        target body's center in the target body-fixed frame. We must
C        evaluate the rotation state of the target at the correct
C        epoch, and account for the rate of change of light time, if
C        light time corrections are used. The algorithm we use depends
C        on the algorithm used in SUBPNT, since we're computing the
C        derivative with respect to time of the solution found by that
C        routine.
C
C        In this algorithm, we must take into account the fact that
C        SUBPNT performs light time and stellar aberration corrections
C        for the sub-observer point, not for the center of the target
C        body.
C
C        If light time and stellar aberration corrections are used,
C
C           - Find the aberration corrected sub-observer point and the
C             light time-corrected epoch TRGEPC associated with the
C             sub-observer point.
C
C           - Use TRGEPC to find the position of the target relative to
C             the solar system barycenter.
C
C           - Use TRGEPC to find the orientation of the target relative
C             to the J2000 reference frame.
C
C           - Find the light-time corrected position of the
C             sub-observer point; use this to compute the stellar
C             aberration offset that applies to the sub-observer point,
C             as well as the velocity of this offset.
C
C           - Find the corrected state of the target center as seen
C             from the observer, where the corrections are those
C             applicable to the sub-observer point.
C
C           - Negate the corrected target center state to obtain the
C             state of the observer relative to the target.
C
C           - Express the state of the observer relative to the target
C             in the target body fixed frame at TRGEPC.
C
C
C        Below, we'll use the convention that vectors expressed
C        relative to the body-fixed frame have names of the form
C
C           FX*
C
C        Note that SUBPNT will signal an error if FIXREF is not
C        actually centered on the target body.
C
         CALL SUBPNT ( METHOD, SVTARG, ET,     FIXREF,
     .                 ABCORR, SVOBS,  SPOINT, TRGEPC, SRFVEC )

C
C        Get J2000-relative states of observer and target with respect
C        to the solar system barycenter at their respective epochs of
C        participation.
C
         CALL SPKSSB ( OBSID, ET,     'J2000', SSBOBS )
         CALL SPKSSB ( TRGID, TRGEPC, 'J2000', SSBTG0 )

C
C        Get the uncorrected J2000 to body-fixed to state
C        transformation at TRGEPC.
C
         CALL SXFORM ( 'J2000', FIXREF, TRGEPC, XFORM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSSOB' )
            RETURN
         END IF

C
C        Initialize the state of the sub-observer point in the
C        body-fixed frame. At this point we don't know the
C        point's velocity; set it to zero.
C
         CALL MOVED  ( SPOINT, 3, FXPSTA    )
         CALL CLEARD (         3, FXPSTA(4) )

         IF ( USESTL ) THEN
C
C           We're going to need the acceleration of the observer
C           relative to the SSB. Compute this now.
C
            DO I = 1, 2
C
C              The epoch is ET -/+ TDELTA.
C               
               T =  ET  + (2*I-3)*TDELTA

               CALL SPKSSB ( OBSID,  T,  'J2000', OBSSTA(1,I) )

            END DO

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFSSOB' )
               RETURN
            END IF

C
C           Compute the observer's acceleration using a quadratic
C           approximation.
C
            CALL QDERIV ( 3, OBSSTA(4,1), OBSSTA(4,2), TDELTA, ACC )

         END IF

C
C        The rest of the algorithm is iterative. On the first
C        iteration, we don't have a good estimate of the velocity
C        of the sub-observer point relative to the body-fixed 
C        frame. Since we're using this velocity as an input
C        to the aberration velocity computations, we
C        expect that treating this velocity as zero on the first
C        pass yields a reasonable estimate. On the second pass,
C        we'll use the velocity derived on the first pass.
C
         CALL CLEARD ( 3, FXPVEL )

C
C        We'll also estimate the rate of change of light time
C        as zero on the first pass.
C         
         DLT = 0.D0
         
         DO I = 1, 2
C
C           Correct the target's velocity for the rate of
C           change of light time.
C
            IF ( XMIT ) THEN
               SCALE = 1.D0 + DLT
            ELSE
               SCALE = 1.D0 - DLT
            END IF

C
C           Scale the velocity portion of the target state to 
C           correct the velocity for the rate of change of light
C           time.
C
            CALL MOVED ( SSBTG0, 3,         SSBTRG    )
            CALL VSCL  ( SCALE,  SSBTG0(4), SSBTRG(4) )

C
C           Get the state of the target with respect to the observer.
C
            CALL VSUBG ( SSBTRG, SSBOBS, 6, OBSTRG )
               
C
C           Correct the J2000 to body-fixed state transformation matrix
C           for the rate of change of light time.
C
            CALL ZZCORSXF ( XMIT, DLT, XFORM, CORXFM )

C
C           Invert CORXFM to obtain the corrected 
C           body-fixed to J2000 state transformation.
C           
            CALL INVSTM ( CORXFM, CORXFI )

C
C           Convert the sub-observer point state to the J2000 frame.
C
            CALL MXVG ( CORXFI, FXPSTA, 6, 6, PNTSTA )

C
C           Find the J2000-relative state of the sub-observer
C           point with respect to the target.
C
            CALL VADDG ( OBSTRG, PNTSTA, 6, OBSPNT )

            IF ( USESTL ) THEN
C
C              Now compute the stellar aberration correction
C              applicable to OBSPNT. We need the velocity of
C              this correction as well.
C
               CALL ZZSTELAB ( XMIT, ACC, SSBOBS(4), OBSPNT, SA, SAVEL )

               CALL MOVED ( SA,    3, SASTAT    )
               CALL MOVED ( SAVEL, 3, SASTAT(4) )
C
C              Adding the stellar aberration state to the target center
C              state gives us the state of the target center with
C              respect to the observer, corrected for the aberrations
C              applicable to the sub-observer point.
          
               CALL VADDG ( OBSTRG, SASTAT, 6, STEMP )

            ELSE
               CALL MOVED ( OBSTRG, 6, STEMP )
            END IF

C
C           Convert STEMP to the body-fixed reference frame.
C
            CALL MXVG ( CORXFM, STEMP, 6, 6, FXTSTA )
            
C
C           At long last, compute the state of the observer
C           with respect to the target in the body-fixed frame.
C
            CALL VMINUG ( FXTSTA, 6, FXOSTA )

C
C           Now we can obtain the surface velocity of the 
C           sub-observer point. 
C
            IF ( NEAR ) THEN
C
C              The sub-observer point method is "near point."
C
               CALL DNEARP ( FXOSTA, RADII(1), RADII(2), RADII(3),
     .                       FXPSTA, DALT,     FOUND              )

               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'The sub-observer state could '
     .            //            'could not be computed because '
     .            //            'the velocity was not well '
     .            //            'defined.  DNEARP returned '
     .            //            '"not found."'                  )
                  CALL SIGERR ( 'SPICE(DEGENERATECASE)'         )
                  CALL CHKOUT ( 'ZZGFSSOB'                      )
                  RETURN

               END IF

            ELSE
C
C              The sub-observer point method is "surface intercept
C              point." The ray direction is simply the negative of the
C              observer's position relative to the target center.
C
               CALL VMINUG ( FXOSTA, 6, RAYSTA )

               CALL SURFPV ( FXOSTA,   RAYSTA,   RADII(1), 
     .                       RADII(2), RADII(3), FXPSTA,   FOUND )

C
C              Although in general it's not an error for SURFPV to be
C              unable to compute an intercept state, it *is* an error
C              in this case, since the ray points toward the center of
C              the target.
C
               IF ( .NOT. FOUND ) THEN

                  CALL SETMSG ( 'The sub-observer state could '
     .            //            'could not be computed because '
     .            //            'the velocity was not well '
     .            //            'defined. SURFPV returned '
     .            //            '"not found."'                   )
                  CALL SIGERR ( 'SPICE(DEGENERATECASE)'          )
                  CALL CHKOUT ( 'ZZGFSSOB'                       )
                  RETURN

               END IF

            END IF

C
C           At this point we can update the surface point
C           velocity and light time derivative estimates.
C
C           In order to compute the light time rate, we'll
C           need the J2000-relative velocity of the sub-observer
C           point with respect to the observer. First convert
C           the sub-observer state to the J2000 frame, then
C           add the result to the state of the target center
C           with respect to the observer.
C
            CALL MXVG  ( CORXFI, FXPSTA, 6, 6, PNTSTA )
            CALL VADDG ( OBSTRG, PNTSTA, 6,    OBSPNT )
            
C
C           Now that we have an improved estimate of the 
C           sub-observer state, we can estimate the rate of
C           change of light time as 
C
C              range rate
C              ----------
C                  c
C
C
C           If we're correcting for stellar aberration, *ideally* we
C           should remove that correction now, since the light time
C           rate is based on light time between the observer and the
C           light-time corrected sub-observer point. But the error made
C           by including stellar aberration is too small to make it
C           worthwhile to make this adjustment.
C
            CALL VHAT ( OBSPNT, UPOS )

            DLT = VDOT ( OBSPNT(4), UPOS ) / CLIGHT()
            
C
C           With FXPVEL and DLT updated, we'll repeat our
C           computations.
C
         END DO


      ELSE
C
C        We should never get here.
C
         CALL SETMSG ( 'Aberration correction # was not '
     .   //            'recognized.'                      )
         CALL ERRCH  ( '#',  ABCORR                       )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'              )
         CALL CHKOUT ( 'ZZGFSSOB'                         )
         RETURN

      END IF

C
C     Copy the computed state to the output argument STATE.
C
      CALL MOVED ( FXPSTA, 6, STATE )

      CALL CHKOUT  ( 'ZZGFSSOB' ) 
      RETURN
      END
