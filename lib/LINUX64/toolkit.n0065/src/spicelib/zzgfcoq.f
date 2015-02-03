C$Procedure      ZZGFCOQ ( GF, return coordinate quantity )
 
      SUBROUTINE ZZGFCOQ ( VECDEF, METHOD, TRGID, ET,      
     .                     REF,    ABCORR, OBSID, DREF,  
     .                     DVEC,   CRDSYS, CTRID, RE,   
     .                     F,      CRDNAM, VALUE, FOUND )
 
C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Return the value of a specified coordinate of a vector.
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

      CHARACTER*(*)         VECDEF
      CHARACTER*(*)         METHOD
      INTEGER               TRGID
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      INTEGER               OBSID
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      CHARACTER*(*)         CRDSYS
      INTEGER               CTRID
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      F
      CHARACTER*(*)         CRDNAM
      DOUBLE PRECISION      VALUE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VECDEF     I   Vector definition.
C     METHOD     I   Computation method.
C     TRGID      I   Target ID code.
C     ET         I   Computation epoch.
C     REF        I   Reference frame name.
C     ABCORR     I   Aberration correction.
C     OBSID      I   Observer ID code.
C     DREF       I   Reference frame of ray's direction vector.
C     DVEC       I   Ray's direction vector.
C     CRDSYS     I   Coordinate system name.
C     CTRID      I   Frame center ID code.
C     RE         I   Equatorial radius of central body.
C     F          I   Flattening coefficient of central body.
C     CRDNAM     I   Coordinate name.
C     VALUE      O   Coordinate value.
C     FOUND      O   Flag indicating if coordinate was computed.
C
C$ Detailed_Input
C
C
C     VECDEF     Every coordinate computed by this routine is a
C                function of an underlying vector. VECDEF is a short
C                string describing the means by which the vector of
C                interest is defined. Only parameters from the Fortran
C                INCLUDE file zzgf.inc should be used. Parameter names
C                and meanings are:
C
C                   POSDEF               Vector is position of
C                                        target relative to observer.
C
C                   SOBDEF               Vector is sub-observer
C                                        point on target body.  Vector
C                                        points from target body
C                                        center to sub-observer point.
C                                        The target must be an extended
C                                        body modeled as a triaxial
C                                        ellipsoid.
C
C                   SINDEF               Vector is ray-surface intercept
C                                        point on target body. Vector
C                                        points from target body
C                                        center to sub-observer point.
C                                        The target must be an extended
C                                        body modeled as a triaxial
C                                        ellipsoid.
C
C                Case, leading and trailing blanks ARE significant
C                in the string VECDEF.
C
C
C     METHOD     is a string specifying the computational method
C                applicable to the vector of interest. When VECDEF
C                is the parameter  
C
C                   SOBDEF
C
C                METHOD should be set to one of the values accepted
C                by the SPICELIB routine SUBPNT.
C
C                When VECDEF is the parameter
C
C                   SINDEF
C
C                METHOD should be set to one of the values accepted
C                by the SPICELIB routine SINCPT.
C
C                METHOD is ignored if VECDEF is set to
C
C                   POSDEF
C
C                Case, leading and trailing blanks are not significant
C                in the string METHOD.
C
C
C     TRGID      is the NAIF ID code of the target object.
C
C
C     ET         is the time, expressed as ephemeris seconds past J2000
C                TDB, at which the specified coordinate is to be
C                computed.
C
C
C     REF        is the name of the reference frame relative to which
C                the vector of interest is specified.  The specified
C                condition applies to the specified coordinate of 
C                of this vector in frame REF.
C
C                When geodetic or planetographic coordinates are used,
C                the reference ellipsoid is assumed to be that
C                associated with the central body of the frame
C                designated by REF. In this case, the central body of
C                the frame must be an extended body.
C
C                Case, leading and trailing blanks are not significant
C                in the string REF.
C
C     
C     ABCORR     indicates the aberration corrections to be applied to
C                the state of the target body to account for one-way
C                light time and stellar aberration.  The orientation
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
C     DREF       is the name of the reference frame relative to which a
C                ray's direction vector is expressed. This may be any
C                frame supported by the SPICE system, including
C                built-in frames (documented in the Frames Required
C                Reading) and frames defined by a loaded frame kernel
C                (FK). The string DREF is case-insensitive, and leading
C                and trailing blanks in FIXREF are not significant.
C
C                When DREF designates a non-inertial frame, the
C                orientation of the frame is evaluated at an epoch
C                dependent on the frame's center and, if the center is
C                not the observer, on the selected aberration
C                correction. See the description of the direction
C                vector DVEC for details.
C
C
C     DVEC       Ray direction vector emanating from the observer. The
C                intercept with the target body's surface of the ray
C                defined by the observer and DVEC is sought.
C
C                DVEC is specified relative to the reference frame
C                designated by DREF.
C
C                Non-inertial reference frames are treated as follows:
C                if the center of the frame is at the observer's
C                location, the frame is evaluated at ET. If the frame's
C                center is located elsewhere, then letting LTCENT be
C                the one-way light time between the observer and the
C                central body associated with the frame, the
C                orientation of the frame is evaluated at ET-LTCENT,
C                ET+LTCENT, or ET depending on whether the requested
C                aberration correction is, respectively, for received
C                radiation, transmitted radiation, or is omitted.
C                LTCENT is computed using the method indicated by
C                ABCORR.
C                 
C
C     CRDSYS     is the name of the coordinate system to which the 
C                coordinate of interest belongs. Allowed values are
C                those defined in the GF Fortran INCLUDE file
C
C                   zzgf.inc.
C                  
C                Case, leading and trailing blanks ARE significant
C                in the string CRDSYS.
C
C
C     CTRID      is the NAIF ID code of the input frame REF's center.
C
C
C     RE         is the equatorial radius associated with the body
C                designated by CTRID. RE is used only when the 
C                coordinate system is GEOSYS or PGRSYS; otherwise
C                RE may be set to 0.D0.
C
C     F          is the flattening coefficient associated with the body
C                designated by CTRID. RE is used only when the
C                coordinate system is GEOSYS or PGRSYS; otherwise RE
C                may be set to 0.D0.
C
C
C     CRDNAM     is the name of the coordinate of interest:  this is
C                the coordinate to which the specified condition
C                applies.  The set of coordinate names is a function of
C                the coordinate system. Allowed values are those
C                defined in the GF Fortran INCLUDE file
C
C                   zzgf.inc.
C
C                Case, leading and trailing blanks ARE significant
C                in the string CRDNAM.
C
C
C$ Detailed_Output
C
C     VALUE     is the specified coordinate, evaluated at the epoch ET.
C               Coordinates having dimensions of length have units of
C               km. Coordinates having angular dimensions have units of
C               radians.
C
C               VALUE is defined if and only if the output argument
C               FOUND is set to .TRUE.
C
C
C     FOUND     is a logical flag indicating whether the requested 
C               coordinate could be computed. FOUND is set to .FALSE.
C               if and only if the vector definition is SINDEF and
C               either
C
C                  - no surface intercept is found
C      
C                  - the velocity of the surface intercept is not
C                    computable
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the vector definition VECDEF is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     2)  If the vector definition is either SOBDEF or SINDEF
C         and the computation method METHOD is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     3)  If the aberration correction ABCORR is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     4)  If the coordinate system name CRDSYS is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     5)  If the coordinate name CRDNAM is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     6)  If the frame REF is not recognized by the frames subsystem,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     7)  If VECDEF calls for a computation involving a target surface
C         point and the radii defining the reference ellipsoid
C         associated with the target body are not available in the
C         kernel pool, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C     8)  If VECDEF calls for a computation involving a target surface
C         point and the name and ID code of the frame associated with
C         the target body is not available from the frame subsystem,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     9)  If ephemeris data are required but not available to compute
C         the state of the target, the coordinate frame REF's center,
C         or the input ray's frame DREF's center relative to the
C         observer, the error will be diagnosed by routines in the call
C         tree of this routine.
C
C     10) If orientation data for the frame REF are not available,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     11) If orientation data for the frame DREF are required but 
C         not available, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C$ Files
C
C     This routine doesn't directly participate in SPICE kernel loading
C     or unloading.  However, a variety of SPICE kernels must be loaded
C     in order for this routine to work:
C
C        - Since all coordinate computations supported by this routine
C          depend on observer-target vectors, at a minimum, SPK files
C          providing ephemeris data enabling computation of these
C          vectors are required.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C        - If the coordinate of interest is defined in terms of a target
C          surface point, then (currently) a PCK providing radii for a
C          triaxial shape model must be loaded.
C
C        - If geodetic or planetographic coordinates are used, then a
C          PCK providing radii for a triaxial shape model must be
C          loaded.
C
C     See the Files section of GFEVNT's header for further information.
C
C$ Particulars
C
C     This routine is used by the GF coordinate utility routines in
C     order to solve for time windows on which specified mathematical
C     conditions involving coordinates are satisfied.
C
C$ Examples
C
C     See ZZGFCOU.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
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
C     compute coordinates of a vector
C
C-&       

 
C
C     SPICELIB functions
C
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

C
C     Local variables
C
      CHARACTER*(CRDLEN)    CRDNMS ( 3, NSYS )
      CHARACTER*(BDNMLN)    CTRNAM
      CHARACTER*(BDNMLN)    OBSNAM
      CHARACTER*(CRDLEN)    SYSNMS (    NSYS )
      CHARACTER*(CRDLEN)    SYSNAM
      CHARACTER*(BDNMLN)    TRGNAM

      DOUBLE PRECISION      COORDS ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )

      INTEGER               CRDIDX
      INTEGER               PRVCTR
      INTEGER               PRVOBS
      INTEGER               PRVTRG
      INTEGER               SYSIDX

      LOGICAL               FIRST

C
C     Saved variables
C
      SAVE                  CRDNMS
      SAVE                  CTRNAM
      SAVE                  FIRST
      SAVE                  OBSNAM
      SAVE                  PRVCTR
      SAVE                  PRVOBS
      SAVE                  PRVTRG
      SAVE                  SYSNMS
      SAVE                  TRGNAM

C
C     Initial values 
C
      DATA                  FIRST / .TRUE. /
C
C     The Ith coordinate system in the array SYSNMS has coordinates
C     in the Ith row of the array CRDNMS. This association must be
C     preserved when this routine is updated.
C
      DATA                  SYSNMS /  RECSYS, 
     .                                LATSYS, 
     .                                RADSYS, 
     .                                SPHSYS, 
     .                                CYLSYS, 
     .                                GEOSYS,
     .                                PGRSYS  /

C
C     The order of the coordinate names in the Ith row of this array
C     matches the order of the outputs of the corresponding
C     SPICELIB routine REC*, which maps a Cartesian vector to
C     the Ith coordinate system in the array SYSNMS. Again, this
C     order must be preserved.
C     
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD,
     .                                RADCRD,   LONCRD,   LATCRD,
     .                                RNGCRD,   RACRD,    DECCRD,
     .                                RADCRD,   CLTCRD,   LONCRD,
     .                                RADCRD,   LONCRD,   ZCRD,
     .                                LONCRD,   LATCRD,   ALTCRD,
     .                                LONCRD,   LATCRD,   ALTCRD  /

      DATA                  PRVCTR /  0 /
      DATA                  PRVOBS /  0 /
      DATA                  PRVTRG /  0 /

      DATA                  OBSNAM / ' ' /
      DATA                  TRGNAM / ' ' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFCOQ' ) 

C
C     No result was found yet.
C
      FOUND = .FALSE.

C
C     Find the index of the coordinate system name in the list of
C     supported names.
C
      SYSIDX = ISRCHC ( CRDSYS, NSYS, SYSNMS )

      IF ( SYSIDX .EQ. 0 ) THEN
C
C        We don't recognize this system name.
C
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                       )
         CALL CHKOUT ( 'ZZGFCOQ'                                   )
         RETURN

      END IF

      SYSNAM = SYSNMS(SYSIDX)

C
C     Find the index of the coordinate name in the list of
C     supported names.
C
      CRDIDX = ISRCHC ( CRDNAM, 3, CRDNMS(1,SYSIDX) )

      IF ( CRDIDX .EQ. 0 ) THEN
C
C        We don't recognize this coordinate name.
C
         CALL SETMSG ( 'The coordinate name # belonging to the '  //
     .                 'coordinate system # is not recognized.'   )
         CALL ERRCH  ( '#',  CRDNAM                               )
         CALL ERRCH  ( '#',  CRDSYS                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZGFCOQ'                                  )
         RETURN

      END IF

C
C     Look up the target and observer names if these will be
C     needed. The SUBPNT and SINCPT interfaces require them.
C     The RECPGR interface requires the frame center ID code
C     as well.
C
      IF (      ( VECDEF .EQ. SOBDEF ) 
     .     .OR. ( VECDEF .EQ. SINDEF )  
     .     .OR. ( SYSNAM .EQ. PGRSYS )  ) THEN

         IF (  FIRST  .OR. ( TRGID .NE. PRVTRG )  ) THEN

            CALL BODC2S ( TRGID, TRGNAM )         

            PRVTRG = TRGID

         END IF


         IF (  FIRST  .OR. ( OBSID .NE. PRVOBS )  ) THEN

            CALL BODC2S ( OBSID, OBSNAM )

            PRVOBS = OBSID
           
         END IF

         IF (  FIRST  .OR. ( CTRID .NE. PRVCTR )  ) THEN

            CALL BODC2S ( CTRID, CTRNAM )

            PRVCTR = CTRID

         END IF

         FIRST = .FALSE.

      END IF

      IF ( VECDEF .EQ. POSDEF  ) THEN
C
C        Find the observer-target position vector.
C
         CALL SPKEZP ( TRGID, ET, REF, ABCORR, OBSID, POS, LT )


      ELSE IF ( VECDEF .EQ. SOBDEF ) THEN
C
C        The caller has requested a sub-observer point coordinate
C        computation.
C
         CALL SUBPNT ( METHOD, TRGNAM, ET,  REF,
     .                 ABCORR, OBSNAM, POS, TRGEPC, SRFVEC )


      ELSE IF ( VECDEF .EQ. SINDEF ) THEN
C
C        The caller has requested a surface intercept point coordinate
C        computation.
C
         CALL SINCPT ( METHOD,  TRGNAM, ET,     REF, 
     .                 ABCORR,  OBSNAM, DREF,   DVEC,    
     .                 POS,     TRGEPC, SRFVEC, FOUND  )

C
C        Without an intercept, there's nothing left to do here.
C
         IF ( .NOT. FOUND ) THEN
            CALL CHKOUT ( 'ZZGFCOQ' )
            RETURN
         END IF

      ELSE 

         CALL SETMSG ( 'The coordinate quantity # is not recognized.' )
         CALL ERRCH  ( '#',  VECDEF                                   )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                          )
         CALL CHKOUT ( 'ZZGFCOQ'                                      )
         RETURN

      END IF

C
C     If we already encountered an error while trying to compute 
C     the vector of interest, return now.
C
      IF (  FAILED() ) THEN
          CALL CHKOUT (  'ZZGFCOQ' )
          RETURN
      END IF 

C
C     At this point we assume the vector whose coordinate is 
C     to be computed resides in POS. Convert POS to the
C     specified coordinate system.
C
      IF ( SYSNAM .EQ. RECSYS ) THEN
C
C        No conversion needed for rectangular coordinates.
C
         CALL MOVED ( POS, 3, COORDS )


      ELSE IF ( SYSNAM .EQ. LATSYS ) THEN

         CALL RECLAT ( POS, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. RADSYS ) THEN

         CALL RECRAD ( POS, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. SPHSYS ) THEN

         CALL RECSPH ( POS, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. CYLSYS ) THEN

         CALL RECCYL ( POS, COORDS(1), COORDS(2), COORDS(3) )

            
      ELSE IF ( SYSNAM .EQ. GEOSYS ) THEN
        
         CALL RECGEO ( POS, RE, F, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SYSNAM .EQ. PGRSYS ) THEN

         CALL RECPGR ( CTRNAM,    POS,       RE,       F, 
     .                 COORDS(1), COORDS(2), COORDS(3)    )

      ELSE
C
C        We should never arrive here.
C
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFCOQ'                                   )
         RETURN

      END IF

C
C     Set the return value.
C
C     CRDIDX indicates the index of the coordinate of interest
C     in the list of coordinates for the input coordinate system.
C      
      VALUE = COORDS(CRDIDX)

C
C     Having made it this far means the result was found.
C
      FOUND = .TRUE.

      CALL CHKOUT  ( 'ZZGFCOQ' ) 
      RETURN
      END
