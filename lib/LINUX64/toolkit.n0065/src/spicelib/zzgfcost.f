C$Procedure      ZZGFCOST ( GF, coordinate definition state )
 
      SUBROUTINE ZZGFCOST ( VECDEF, METHOD, TRGID, ET,      
     .                      REF,    ABCORR, OBSID, DREF,  
     .                      DCTR,   DVEC,   RADII, STATE, FOUND )
 
C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     Return a state vector used to define coordinates referenced in a
C     GF search.
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
      INTEGER               DCTR
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
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
C     DCTR       I   ID code of ray frame's center.
C     DVEC       I   Ray's direction vector.
C     RADII      I   Radii of reference ellipsoid.
C     STATE      O   State used to define coordinates.
C     FOUND      O   Flag indicating if state was computed.
C
C$ Detailed_Input
C
C
C     VECDEF     States computed by this routine consist of a an
C                underlying vector and the vector's velocity. VECDEF is
C                a short string describing the means by which the
C                vector of interest is defined. Only parameters from
C                the Fortran INCLUDE file zzgf.inc should be used.
C                Parameter names and meanings are:
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
C                TDB, at which the specified state is to be computed.
C
C
C     REF        is the name of the reference frame relative to which
C                the state of interest is specified.
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
C                and trailing blanks in DREF are not significant.
C
C                When DREF designates a non-inertial frame, the
C                orientation of the frame is evaluated at an epoch
C                dependent on the frame's center and, if the center is
C                not the observer, on the selected aberration
C                correction. See the description of the direction
C                vector DVEC for details.
C
C
C     DCTR       is the ID code of the object at which the frame
C                designated by DREF is centered. Although DCTR
C                can be derived from DREF, in the interest of 
C                efficiency, DCTR is obtained by the caller,
C                normally during search initialization.
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
C     RADII      is a double precision array containing the three
C                radii of a reference ellipsoid associated with
C                the target body. 
C
C                RADII is ignored if the input vector definition
C                is POSDEF; in this case the caller may set the
C                elements of RADII to zero.
C
C                 
C$ Detailed_Output
C
C     STATE      is the specified state vector, evaluated at the epoch
C                ET. The position component of STATE is the vector
C                defined by VECDEF and the other inputs. The velocity
C                component of STATE is the derivative with respect to
C                time of the position component. Units are km and km/s.
C
C                STATE is defined if and only if the output argument
C                FOUND is set to .TRUE.
C
C
C     FOUND      is a logical flag indicating whether the requested
C                state could be computed. FOUND is set to .FALSE. if
C                and only if the vector definition is SINDEF and either
C
C                - the surface intercept is not found
C
C                - the surface intercept velocity is not computable
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
C     2)  If the computation method METHOD is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     3)  If the aberration correction ABCORR is not recognized,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     4)  If the frame REF is not recognized by the frames subsystem,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     5)  If VECDEF calls for a computation involving a target surface
C         point and the name and ID code of the frame associated with
C         the target body is not available from the frame subsystem,
C         the error SPICE(NOFRAME) is signaled.
C
C     6)  If VECDEF calls for a computation involving a target surface
C         point and ID codes of target and observer can't be converted
C         to names, the error  will be diagnosed by routines in the
C         call tree of this routine.
C
C     7)  If ephemeris data are required but not available to compute
C         the state of the target, the coordinate frame REF's center,
C         or the input ray's frame DREF's center relative to the
C         observer, the error will be diagnosed by routines in the call
C         tree of this routine.
C
C     8)  If orientation data for the frame REF are not available,
C         the error will be diagnosed by routines in the call tree
C         of this routine.
C
C     9)  If orientation data for the frame DREF are required but 
C         not available, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C     10) If the input radii don't define a valid triaxial ellipsoid,
C         the error will be diagnosed by routines in the call tree of
C         this routine.
C
C$ Files
C
C     This routine doesn't directly participate in SPICE kernel loading
C     or unloading.  However, a variety of SPICE kernels must be loaded
C     in order for this routine to work:
C
C        - SPK files providing ephemeris data enabling computation of
C          the specified state vector are required.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C        - If the state of interest is defined in terms of a target
C          surface point, then (currently) a PCK providing radii for a
C          triaxial shape model must be loaded.
C
C     See the Files section of GFEVNT's header for further information.
C
C$ Particulars
C
C     This routine is used by the GF coordinate utility routines in
C     order to solve for time windows on which specified mathematical
C     conditions involving coordinates are satisfied. The role of
C     this routine is to provide Cartesian state vectors enabling
C     the GF coordinate utilities to determine the signs of the
C     derivatives with respect to time of coordinates of interest.
C
C     This routine has a secondary purpose: enabling the GF system
C     to determine, via a binary state search, the window over 
C     which a coordinate of interest is computable. This "computability
C     window" must be found before any search involving a constraint
C     on a coordinate of a surface intercept point can be performed.
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
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
C
C-&

C$ Index_Entries
C
C     compute state defining coordinate
C
C-&       

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      LT

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFCOST' ) 

C
C     No result was found yet.
C
      FOUND = .FALSE.

      
      IF ( VECDEF .EQ. POSDEF  ) THEN
C
C        Find the observer-target state vector.
C
         CALL SPKEZ ( TRGID, ET, REF, ABCORR, OBSID, STATE, LT )

         FOUND = .TRUE.

      ELSE IF ( VECDEF .EQ. SOBDEF ) THEN
C
C        The caller has requested the state of a sub-observer point.
C
         CALL ZZGFSSOB ( METHOD, TRGID, ET,    REF, 
     .                   ABCORR, OBSID, RADII, STATE )

         FOUND = .TRUE.


      ELSE IF ( VECDEF .EQ. SINDEF ) THEN
C
C        The caller has requested the state of a surface intercept
C        point.
C
         CALL ZZGFSSIN ( METHOD, TRGID, ET,   REF,   ABCORR, OBSID,  
     .                   DREF,   DCTR,  DVEC, RADII, STATE,  FOUND )

      ELSE 

         CALL SETMSG ( 'The coordinate quantity # is not recognized.' )
         CALL ERRCH  ( '#',  VECDEF                                   )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                          )
         CALL CHKOUT ( 'ZZGFCOST'                                     )
         RETURN

      END IF

C
C     At this point, one of the following is true:
C
C        - the state vector was found and 
C          FOUND is .TRUE.
C
C        - FOUND is .FALSE.
C
C        - a SPICE error occurred
C

      CALL CHKOUT  ( 'ZZGFCOST' ) 
      RETURN
      END
