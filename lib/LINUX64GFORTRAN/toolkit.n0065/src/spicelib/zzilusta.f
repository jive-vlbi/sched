C$Procedure ZZILUSTA ( Illumination angle states )

      SUBROUTINE ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,   
     .                      FIXREF, ABCORR, OBSRVR, SPOINT, 
     .                      NORMAL, PHSSTA, INCSTA, EMISTA  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute illumination angles and their rates of change at a
C     surface point.
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
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PHSSTA ( 2 )
      DOUBLE PRECISION      INCSTA ( 2 )
      DOUBLE PRECISION      EMISTA ( 2 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body associated with surface point.
C     ILLUM      I   Name of illumination source.
C     ET         I   Observation epoch (TDB).
C     FIXREF     I   Body-fixed reference frame.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observer.
C     SPOINT     I   Surface point.
C     NORMAL     I   Outward normal vector at surface point.
C     PHSSTA     O   Phase angle state.
C     INCSTA     O   Solar incidence angle state.
C     EMISTA     O   Emission angle state.
C
C$ Detailed_Input
C                                 
C     METHOD         is a string specifying the computation method to
C                    be used by this routine. The only value currently
C                    allowed is 'ELLIPSOID'. This indicates that the
C                    target body shape is modeled as an ellipsoid.
C
C                    Case and leading and trailing blanks are not
C                    significant in METHOD.
C
C     TARGET         is the name of the target body associated with the
C                    surface point SPOINT. TARGET may be a body name or
C                    an ID code provided as as string.
C
C     ILLUM          is the name of the illumination source used to
C                    define the illumination angles computed by this
C                    routine. ILLUM may be a body name or an ID code
C                    provided as as string.
C
C     ET             is the observation time. ET is expressed as
C                    seconds past J2000 TDB.
C
C     FIXREF         is the name of the body-centered, body-fixed
C                    reference frame relative to which SPOINT is
C                    specified. The frame's center must coincide with
C                    TARGET.
C
C     ABCORR         indicates the aberration corrections to be
C                    applied. Only reception corrections are supported.
C                    See the header of ILUMIN for a discussion of
C                    aberration corrections used in illumination angle
C                    computations.
C
C     OBSRVR         is the name of the observing body. OBSRVR may be a
C                    body name or an ID code provided as as string.
C
C     SPOINT         is a 3-vector containing the cartesian coordinates
C                    of the surface point at which the illumination
C                    angle states are to be computed. SPOINT is
C                    expressed in the body-centered, body-fixed frame
C                    designated by FIXREF (see description above).
C
C                    Units are km.
C
C     NORMAL         is an outward normal vector to be used for
C                    emission angle and solar incidence angle
C                    calculations. NORMAL should be orthogonal to the
C                    plane tangent at SPOINT to the target body's
C                    surface.
C                       
C$ Detailed_Output
C
C     PHSSTA         is the phase angle and its rate of change with
C                    respect to TDB, evaluated at ET.
C
C     INCSTA         is the solar incidence angle and its rate of
C                    change with respect to TDB, evaluated at ET.
C
C     EMISTA         is the emission angle and its rate of change with
C                    respect to TDB, evaluated at ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the computation method is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C     2)  If ABCORR specifies a transmission aberration correction,
C         the error SPICE(INVALIDOPTION) is signaled.
C
C     3)  If an error occurs while looking up a state vector, the
C         error will be signaled by a routine in the call tree of
C         this routine.
C
C     4)  If the input normal vector is zero, the error 
C         SPICE(ZEROVECTOR) is signaled.
C
C$ Files
C
C     See GFILUM.
C
C$ Particulars
C
C     The term "state" used in the name of this routine refers to 
C     the combination of a function and its derivative with respect
C     to time.
C
C     This routine centralizes computation of illumination angles and
C     their rates of change. It also exposes the illumination angle
C     rates of change used by the GF system in order to allow these
C     rates to be tested using the TSPICE system.
C
C     See the SPICELIB routine ILUMIN for a description of the 
C     illumination angles computed by this routine.
C     
C$ Examples
C
C     See usage in ZZGFILDC.
C
C$ Restrictions
C
C     1) This routine is intended for use only by the GF subsystem.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 02-APR-2012 (NJB)
C
C       Previous version was dated 21-MAR-2012 
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C
      CHARACTER*(*)         IREF
      PARAMETER           ( IREF = 'J2000' )

C
C     Local variables
C      
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      ETSURF
      DOUBLE PRECISION      FXNSTA ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTSRC
      DOUBLE PRECISION      NRMSTA ( 6 )
      DOUBLE PRECISION      OBSSTA ( 6 )
      DOUBLE PRECISION      SRCSTA ( 6 )
      DOUBLE PRECISION      STARG  ( 6 )
      DOUBLE PRECISION      TMPXFM ( 6, 6 )
      DOUBLE PRECISION      UVEC   ( 3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               USELT
      LOGICAL               XMIT


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZILUSTA' )

C
C     For now, only ellipsoids are supported as target shapes.
C     
      IF (  .NOT.  EQSTR( METHOD, 'ELLIPSOID' )  ) THEN

         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( 'ZZILUSTA'                                      )
         RETURN

      END IF

C
C     Reject zero normal vectors.
C     
      IF ( VZERO(NORMAL) ) THEN
         
         CALL SETMSG ( 'The input normal vector must not be zero, '
     .   //            'but sadly, it was.'                       )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                        )
         CALL CHKOUT ( 'ZZILUSTA'                                 )
         RETURN

      END IF

C
C     Look up the state of the target with respect to the
C     observer. We'll represent the state in an inertial
C     reference frame.
C 
      CALL SPKCPT ( SPOINT,   TARGET, FIXREF, ET,    IREF,   
     .              'TARGET', ABCORR, OBSRVR, STARG, LT    )

C
C     Compute the epoch associated with the surface point.
C
      CALL ZZCOREPC ( ABCORR, ET, LT, ETSURF )

C
C     Now let the surface point be the observer, let the observation
C     epoch be ETSURF, and find the apparent state of the illumination
C     source as seen from the surface point.
C
      CALL SPKCPO ( ILLUM,  ETSURF, IREF,   'OBSERVER', ABCORR,
     .              SPOINT, TARGET, FIXREF, SRCSTA,     LTSRC  )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'ZZILUSTA' )
         RETURN
      END IF

C
C     We will need to transform the state of the normal vector to
C     the inertial frame. The epoch at which the transformation must be
C     evaluated is that associated with the surface point.

      CALL SXFORM ( FIXREF, IREF, ETSURF, XFORM )

C
C     Correct the body-fixed to inertial frame transformation for the
C     rate of change with respect to ET of observer-surface point light
C     time, if light time corrections are used.
C
C     Start out by parsing ABCORR.
C
      CALL ZZVALCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZILUSTA' )
         RETURN
      END IF

      USELT = ATTBLK( LTIDX  )
      XMIT  = ATTBLK( XMTIDX )

      IF ( XMIT ) THEN

         CALL SETMSG ( 'Aberration correction # is for transmission; '
     .   //            'only reception corrections are supported by '
     .   //            'this routine.'                                )
         CALL ERRCH  ( '#', ABCORR                                    )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                         )
         CALL CHKOUT ( 'ZZILUSTA' )
         RETURN
         
      END IF


      IF ( USELT ) THEN                 
C
C        Compute the rate of change with respect to ET of the
C        observer-surface point light time. This rate is the range rate
C        divided by the speed of light.
C
         CALL VHAT ( STARG, UVEC )

         DLT = VDOT ( STARG(4), UVEC ) / CLIGHT() 
C
C        Correct the state transformation.
C
         CALL ZZCORSXF ( .FALSE., DLT, XFORM, TMPXFM )

         CALL MOVED ( TMPXFM, 36, XFORM )

      END IF

C
C     Create a body-fixed state vector for the normal vector.
C     Convert the normal vector to unit length for safety.
C
      CALL VHAT   ( NORMAL, FXNSTA    )
      CALL CLEARD ( 3,      FXNSTA(4) )

C
C     Transform the state of the normal vector to the inertial
C     frame.
C
      CALL MXVG ( XFORM,  FXNSTA, 6, 6, NRMSTA )

C
C     We also must adjust the state of the illumination source for the
C     rate of change with respect to ET of the observer-surface point
C     light time. The velocity portion of the state we've computed is
C     the derivative with respect to ETSURF (time at the surface point)
C     of the surface point-illumination source vector. We must convert
C     this to a derivative with respect to ET.
C
C     This code assumes reception corrections.
C
      IF ( USELT ) THEN
C
C        ETSURF = ET - LT, so
C
C        d(ETSURF) / d(ET) = ( 1 - DLT ) 
C
         CALL VSCLIP ( 1.D0 - DLT,  SRCSTA(4) )

      END IF
      
C
C     The surface-point observer state we wish to use is the negative
C     of the observer-surface point state.
C
      CALL VMINUG ( STARG, 6, OBSSTA )

C
C     Compute the state (value and rate of change ) 
C     of the phase angle.
C
      PHSSTA( 1 ) =  VSEP  ( OBSSTA, SRCSTA )
      PHSSTA( 2 ) =  DVSEP ( OBSSTA, SRCSTA )

C
C     Compute the state of the illumination source
C     incidence angle.
C
      INCSTA( 1 ) =  VSEP  ( NRMSTA, SRCSTA )
      INCSTA( 2 ) =  DVSEP ( NRMSTA, SRCSTA )

C
C     Compute the state of the emission angle.
C
      EMISTA( 1 ) =  VSEP  ( NRMSTA, OBSSTA )
      EMISTA( 2 ) =  DVSEP ( NRMSTA, OBSSTA )


      CALL CHKOUT ( 'ZZILUSTA' )
      RETURN
      END


