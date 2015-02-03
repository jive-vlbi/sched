C$Procedure ZZGFCOU ( GF, coordinate utility package )
 
      SUBROUTINE ZZGFCOU ( VECDEF, METHOD, TARGET, ET,     REF,     
     .                     ABCORR, OBSRVR, DREF,   DVEC,   CRDSYS,  
     .                     CRDNAM, DECRES, CRDVAL, CRDFND, UDFUNC )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     This is the umbrella routine for the entry points needed by
C     GFEVNT (or other GF routines) in order to solve for time windows
C     on which specified mathematical conditions involving coordinates
C     are satisfied.
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
C     GF
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
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

      CHARACTER*(*)         VECDEF
      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         DREF
      DOUBLE PRECISION      DVEC   ( 3 )
      CHARACTER*(*)         CRDSYS
      CHARACTER*(*)         CRDNAM
      LOGICAL               DECRES
      DOUBLE PRECISION      CRDVAL
      LOGICAL               CRDFND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points 
C     --------  ---  --------------------------------------------------
C     VECDEF     I   COIN
C     METHOD     I   COIN
C     TARGET     I   COIN
C     ET         I   COIN, CODC, COG, COCD, COCG, COSD,
C                    COSG, COEX
C     REF        I   COIN
C     ABCORR     I   COIN
C     OBSRVR     I   COIN
C     DREF       I   COIN
C     DVEC       I   COIN
C     CRDSYS     I   COIN
C     CRDNAM     I   COIN
C     DECRES     O   CODC, COCD, COSD
C     CRDVAL     O   COG,  COCG, COSG
C     CRDFND     O   COEX
C
C$ Detailed_Input
C
C     See individual entry points.
C
C$ Detailed_Output
C
C     See individual entry points.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called directly, the error
C        SPICE(BOGUSENTRY) is signaled.
C
C     See the entry points for descriptions of exceptions specific
C     to those routines.
C
C$ Files
C
C     This suite of routines doesn't directly participate in SPICE
C     kernel loading or unloading.  However, a variety of SPICE kernels
C     must be loaded in order for these utilities to work:
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
C        - If geodetic coordinates are used, then a PCK providing radii
C          for a triaxial shape model must be loaded.
C
C     See the Files section of GFEVNT's header for further information.
C
C$ Particulars
C
C     This routine serves as the umbrella routine for entry points
C     needed by GFEVNT or other GF routines in order to solve for time
C     windows on which specified mathematical conditions involving
C     coordinates are satisfied.  For brevity, we may refer to such a
C     time window as the "solution window" or "coordinate solution
C     window."
C
C     The entry points of this package are
C
C        ZZGFCOIN      an initialization routine that must be called
C                      to define the coordinate of interest.  This
C                      routine must be called at least once before
C                      any of the other entry points are called, but
C                      it may be called as many times as necessary
C                      to initialize new computations.
C
C                      Below, the phrase "the coordinate" refers
C                      to the coordinate established by the latest
C                      call to ZZGFCOIN.  For example, the coordinate
C                      may be the "geodetic latitude of the sub-moon 
C                      point on the earth, relative to the IAU_EARTH
C                      reference frame, computed using light time and
C                      stellar aberration corrections."
C
C        ZZGFCODC      indicates whether the coordinate is strictly
C                      decreasing as a function of time, at a specified
C                      time.
C
C        ZZGFCOG       returns the coordinate value at a specified
C                      time.
C
C        ZZGFCOEX      indicates whether the coordinate is computable
C                      at a specified time. ZZGFCOEX is used to 
C                      determine the time window over which a specified
C                      target surface intercept and its time derivative
C                      is computable.
C
C
C        The following entry points support solution window
C        computations for conditions involving longitude or right
C        ascension.  They may have applications for relations involving
C        other angular coordinates.
C     
C        ZZGFCOCD      indicates whether the cosine of the coordinate is
C                      strictly decreasing as a function of time, at a
C                      specified time. 
C
C        ZZGFCOSD      indicates whether the sine of the coordinate is
C                      strictly decreasing as a function of time, at a
C                      specified time. 
C
C        ZZGFCOCG      returns the cosine of the coordinate at a
C                      specified time.
C
C        ZZGFCOSG      returns the sine of the coordinate at a
C                      specified time.
C
C$ Examples
C
C     See the code of GFEVNT and ZZGFLONG for usage examples.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice. These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C-    SPICELIB version 3.0.0 05-APR-2011 (EDW)
C
C        Code edits to implement use of ZZGFRELX.
C        These edits include removal of unneeded routines:
C
C           ZZGFCOUR
C           ZZGFCOLT
C           ZZGFCOCL
C           ZZGFCOSL      
C
C        and corresponding unused variables.
C
C        Corresponding update to header entries.
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
C     umbrella routine for finding coordinate events
C
C-&      

      EXTERNAL              UDFUNC

C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

      INTEGER               ISRCHC

      LOGICAL               BODFND
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

C
C     Local parameters
C

C
C     Length of an aberration correction name string. 
C
      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 20 )

C
C     Length of a reference frame name.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Length of a body name.
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

C
C     Length of a coordinate system name.
C
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

C
C     Length of a vector definition name.
C
      INTEGER               VDFLEN
      PARAMETER           ( VDFLEN = 32 )

C
C     Number of recognized coordinate systems.
C
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

C
C     Maximum length of a coordinate name.
C
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )
      
C
C     Maximum length of computation method name.
C
      INTEGER               METLEN
      PARAMETER           ( METLEN = 200 )
      
C
C     Time string length.
C
      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 40 )

C
C     Local Variables
C
      CHARACTER*(CRDLEN)    CRDNMS ( 3, NSYS )
      CHARACTER*(ABCLEN)    SVCORR
      CHARACTER*(CRDLEN)    SVCRD
      CHARACTER*(SYSLEN)    SVCSYS
      CHARACTER*(FRNMLN)    SVDREF
      CHARACTER*(METLEN)    SVMETH
      CHARACTER*(BDNMLN)    SVRCNM
      CHARACTER*(FRNMLN)    SVREF
      CHARACTER*(VDFLEN)    SVVDEF
      CHARACTER*(SYSLEN)    SYSNMS ( NSYS )
      CHARACTER*(TIMLEN)    TIMSTR
      
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      COORDS ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      SVDVEC ( 3 )
      DOUBLE PRECISION      SVF
      DOUBLE PRECISION      SVRADI ( 3 )
      DOUBLE PRECISION      SVRE
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      Y      ( 3 )

      INTEGER               CDSIGN ( 3 )
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               FRCODE
      INTEGER               N
      INTEGER               SVCIDX
      INTEGER               SVDCTR
      INTEGER               SVOBS
      INTEGER               SVRCTR
      INTEGER               SVSENS
      INTEGER               SVTARG
      INTEGER               SYSIDX
 
      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      

C
C     Saved Variables
C
      SAVE                  CRDNMS
      SAVE                  SVCIDX
      SAVE                  SVCORR
      SAVE                  SVCRD
      SAVE                  SVCSYS
      SAVE                  SVDCTR
      SAVE                  SVDREF
      SAVE                  SVDVEC
      SAVE                  SVF
      SAVE                  SVMETH
      SAVE                  SVOBS
      SAVE                  SVRADI
      SAVE                  SVRCNM
      SAVE                  SVRCTR
      SAVE                  SVRE
      SAVE                  SVREF
      SAVE                  SVSENS
      SAVE                  SVTARG
      SAVE                  SVVDEF
      SAVE                  SYSNMS
      SAVE                  Y

C
C     Initial values 
C

C
C     Names of supported coordinate systems.
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
C     Names of coordinate triples for the supported coordinate
C     systems.
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


      DATA                  Y      /  0.D0, 1.D0, 0.D0 /


C
C     This routine should never be called.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFCOU'           )
      RETURN
 



C$Procedure ZZGFCOIN ( GF, coordinate search initialization )
 
      ENTRY ZZGFCOIN ( VECDEF, METHOD, TARGET, REF,    ABCORR, OBSRVR, 
     .                 DREF,   DVEC,   CRDSYS, CRDNAM )
 
C$ Abstract
C
C     Initialize a coordinate search.
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
C     ROOT
C
C$ Declarations
C
C     CHARACTER*(*)         VECDEF
C     CHARACTER*(*)         METHOD
C     CHARACTER*(*)         TARGET
C     CHARACTER*(*)         REF
C     CHARACTER*(*)         ABCORR
C     CHARACTER*(*)         OBSRVR
C     CHARACTER*(*)         DREF
C     DOUBLE PRECISION      DVEC
C     CHARACTER*(*)         CRDSYS
C     CHARACTER*(*)         CRDNAM
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     VECDEF     I   Vector definition.
C     METHOD     I   Computation method.
C     TARGET     I   Target name.
C     REF        I   Reference frame name.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Observer name.
C     DREF       I   Ray's direction vector frame.
C     DVEC       I   Ray's direction vector.
C     CRDSYS     I   Coordinate system name.
C     CRDNAM     I   Coordinate name.
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
C     TARGET     is the name of the target object.
C
C
C     REF        is the name of the reference frame relative to which
C                the vector of interest is specified. The specified
C                condition applies to the specified coordinate of 
C                of this vector in frame REF.
C
C                When geodetic coordinates are used, the reference
C                ellipsoid is assumed to be that associated with
C                the central body of the frame designated by REF.
C                In this case, the central body of the frame must
C                be an extended body.
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
C                  'NONE'          No correction.
C                  'LT'            Light time only.
C                  'LT+S'          Light time and stellar aberration.
C                  'CN'            Converged Newtonian (CN) light time.
C                  'CN+S'          CN light time and stellar aberration.
C
C                Supported aberration correction options for
C                transmission (case where radiation is emitted from
C                observer at ET) are:
C
C                  'XLT'           Light time only.
C                  'XLT+S'         Light time and stellar aberration.
C                  'XCN'           Converged Newtonian (CN) light time.
C                  'XCN+S'         CN light time and stellar aberration.
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
C     OBSRVR     is the name of the observer. 
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
C                Note that when geodetic or planetograhic coordinates
C                are used, the reference ellipsoid is that associated
C                with the central body of the reference frame
C                designated by REF. The central body must be an
C                extended body in this case.
C
C                Case, leading and trailing blanks are not significant
C                in the string CRDSYS.
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
C                Case, leading and trailing blanks are not significant
C                in the string CRDNAM.
C
C$ Detailed_Output
C
C     None.  This routine operates by side effects.  See Particulars
C     for a description of the action of this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either the observer or target names cannot be mapped
C         to ID codes, the error SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If the observer and target have the same ID codes, the
C         error SPICE(BODIESNOTDISTINCT) is signaled.
C
C     3)  If the vector definition VECDEF is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     4)  If the computation method METHOD is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     5)  If the aberration correction ABCORR is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     6)  If the coordinate system name CRDSYS is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     7)  If the coordinate name CRDNAM is not recognized,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     8)  If the frame REF is not recognized by the frames subsystem,
C         the error SPICE(NOFRAME) will be signaled.
C
C     9)  If VECDEF calls for a computation involving a target surface
C         intercept point and the name and ID code of the frame DREF
C         associated with the target body are not available from the
C         frame subsystem, the error SPICE(NOFRAME) is signaled.
C
C     10) If VECDEF calls for a computation involving a target surface
C         intercept point and the direction vector DVEC is the zero
C         vector, the error SPICE(ZEROVECTOR) is signaled.
C
C     11) If VECDEF calls for a computation involving a target surface
C         point and the radii defining the reference ellipsoid
C         associated with the target body are not available in the
C         kernel pool, the error will be diagnosed by routines in the
C         call tree of this routine.
C
C     12) If VECDEF calls for a computation involving a target surface
C         point and the frame REF is not centered on the target body,
C         the error SPICE(INVALIDFRAME) will be signaled.
C
C     13) If geodetic or planetographic coordinates are used and the
C         radii defining the reference ellipsoid associated with the
C         center of the frame REF are not available in the kernel pool,
C         the error will be diagnosed by routines in the call tree of
C         this routine.
C
C     14) If geodetic or planetographic coordinates are used and the
C         first equatorial radius of the reference ellipsoid associated
C         with the center of the frame REF is zero, the error
C         SPICE(DIVIDEBYZERO) is signaled.
C
C     15) If geodetic or planetographic coordinates are used and the
C         equatorial radii of the reference ellipsoid associated
C         with the center of the frame REF are unequal, the error
C         SPICE(NOTSUPPORTED) is signaled.
C
C     16) If geodetic or planetographic coordinates are used and the
C         reference ellipsoid associated with the center of the frame
C         REF is degenerate (one or more radii are non-positive),
C         the error SPICE(DEGENERATECASE) is signaled.
C
C$ Files
C
C     See the discussion in the Files section of the header of the
C     umbrella subroutine ZZGFCOU.
C
C$ Particulars
C
C     This routine's main purpose is to support GFEVNT.  Many of
C     the geometric quantities supported by GFEVNT are simply
C     coordinates of a vector in some reference frame.
C
C     The entry points that deal with sines and cosines of coordinates
C     support solving problems involving constraints on 
C     longitude or right ascension.  See ZZGFLONG for usage examples. 
C
C$ Examples
C
C     See GFEVNT and ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
C
C     3)  This routine has the following couplings with other 
C         SPICE routines:
C
C            - The set of allowed aberration corrections must
C              be kept in sync with the set supported by the
C              SPK API routines.
C
C            - The set of vector definitions must be kept in
C              sync with the set supported by GFEVNT.
C
C            - The set of supported coordinate systems must be kept in
C              sync with the set supported by zzgf.inc.
C
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
C-    SPICELIB version 3.0.0 05-APR-2011 (EDW)
C
C        REFVAL removed from routine argument list due to use
C        of ZZGFRELX to calculate the events.
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
C     coordinate initialization routine
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF
      
      CALL CHKIN ( 'ZZGFCOIN' )

C
C     Find NAIF IDs for TARGET and OBSRVR.
C
      CALL BODS2C ( TARGET, SVTARG, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The target object, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFCOIN'                                 )
         RETURN

      END IF
 

      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFCOIN'                                 )
         RETURN

      END IF

C
C     Make sure the observer and target are distinct.
C
      IF ( SVTARG .EQ. SVOBS ) THEN

         CALL SETMSG ( 'The observer and target must be '
     .   //            'distinct objects, but are not: '
     .   //            'OBSRVR = #; TARGET = #.'           )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFCOIN'                          )
         RETURN

      END IF

C
C     Squeeze all blanks out of the aberration correction
C     string; ensure the string is in upper case.
C     
      CALL CMPRSS ( ' ', 0, ABCORR, SVCORR )
      CALL UCASE  ( SVCORR,         SVCORR )

C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C     
      CALL ZZVALCOR ( SVCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFCOIN' )
         RETURN
      END IF

C
C     Store a compressed, upper case, left-justified copy of VECDEF.
C
      CALL LJUST ( VECDEF,          SVVDEF )
      CALL CMPRSS ( ' ', 1, SVVDEF, SVVDEF )
      CALL UCASE ( SVVDEF,          SVVDEF )

C
C     Check SVVDEF.
C
      IF (       ( SVVDEF .NE. POSDEF ) 
     .     .AND. ( SVVDEF .NE. SOBDEF )
     .     .AND. ( SVVDEF .NE. SINDEF ) ) THEN

C
C        We don't recognize this vector definition.
C
         CALL SETMSG ( 'The vector definition # is not supported.' )
         CALL ERRCH  ( '#',  VECDEF                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                       )
         CALL CHKOUT ( 'ZZGFCOIN'                                  )
         RETURN

      END IF

C
C     Store a compressed, upper case, left-justified copy of CRDSYS.
C
      CALL LJUST ( CRDSYS,          SVCSYS )
      CALL CMPRSS ( ' ', 0, SVCSYS, SVCSYS )
      CALL UCASE ( SVCSYS,          SVCSYS )

      SYSIDX = ISRCHC ( SVCSYS, NSYS, SYSNMS )

      IF ( SYSIDX .EQ. 0 ) THEN
C
C        We don't recognize this system name.
C
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                       )
         CALL CHKOUT ( 'ZZGFCOIN'                                  )
         RETURN

      END IF

C
C     Store a compressed, upper case, left-justified copy of CRDNAM.
C
      CALL LJUST  ( CRDNAM,        SVCRD )
      CALL CMPRSS ( ' ', 1, SVCRD, SVCRD )
      CALL UCASE  ( SVCRD,         SVCRD )

C
C     Find and save the index of the coordinate name in the list of
C     supported names.
C
      SVCIDX = ISRCHC ( SVCRD, 3, CRDNMS(1,SYSIDX) )

      IF ( SVCIDX .EQ. 0 ) THEN
C
C        We don't recognize this coordinate name.
C
         CALL SETMSG ( 'The coordinate name # belonging to the '  //
     .                 'coordinate system # is not recognized.'   )
         CALL ERRCH  ( '#',  CRDNAM                               )
         CALL ERRCH  ( '#',  CRDSYS                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZGFCOIN'                                 )
         RETURN

      END IF
 
C
C     Store an upper case, left-justified copy of REF.
C
      CALL LJUST ( REF,   SVREF )
      CALL UCASE ( SVREF, SVREF )

C
C     The remaining work is a function of the vector definition
C     and the coordinate system.
C
      IF (      ( SVVDEF .EQ. SOBDEF ) 
     .     .OR. ( SVVDEF .EQ. SINDEF )
     .     .OR. ( SVCSYS .EQ. GEOSYS )
     .     .OR. ( SVCSYS .EQ. PGRSYS )  ) THEN

C
C        The coordinate is defined using a sub-observer point or
C        a surface intercept point, OR we're using geodetic or
C        planetographic coordinates. In any of these cases, we
C        need the center of the input reference frame and the
C        radii associated with this center.
C
         CALL NAMFRM ( SVREF, FRCODE )

C
C        Save the frame REF's center ID in SVRCTR.
C
         CALL FRINFO ( FRCODE, SVRCTR, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', REF                                  )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFCOIN'                                )
            RETURN

         END IF

C
C        For sub-observer point and surface intercept vector
C        definitions, make sure the input frame's center is
C        the target body.
C
         IF (      ( VECDEF .EQ. SOBDEF )
     .        .OR. ( VECDEF .EQ. SINDEF )  ) THEN

            IF ( SVRCTR .NE. SVTARG ) THEN

               CALL SETMSG ( 'Vector definition method is #, '
     .         //            'but input reference frame # '
     .         //            'has center #. For this vector '
     .         //            'definition, the frame must be '
     .         //            'centered on the target body #.' )

               CALL ERRCH  ( '#',  VECDEF                     )
               CALL ERRCH  ( '#',  REF                        )
               CALL ERRINT ( '#',  SVRCTR                     )
               CALL ERRCH  ( '#',  TARGET                     )
               CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
               CALL CHKOUT ( 'ZZGFCOIN'                       )
               RETURN

            END IF

         END IF

C
C        At this point, we know the frame REF is centered on the
C        target if the computation method is SINDEF or SOBDEF.
C        Fetch the radii of the body acting as the frame center.
C

C
C        Ensure the radii data exists. If not, return an error message
C        with useful information.
C
         IF ( .NOT. BODFND( SVRCTR, 'RADII' ) ) THEN

            IF ( ( SVCSYS .EQ. GEOSYS ) .OR. ( SVCSYS .EQ. PGRSYS ) )
     .      THEN

               CALL SETMSG ( 'No RADII data in kernel pool for frame '
     .            //         '''#'' center body #. Geodetic and '
     .            //         'planetographic coordinates require a '
     .            //         'reference frame centered on a finite '
     .            //         'body. Confirm the proper input frame. '
     .            //         'Bodies {0,..,9} represent barycenters '
     .            //         'and so lack physical properties.' )
               CALL ERRCH  ( '#',  REF                               )
               CALL ERRINT ( '#',  SVRCTR                            )
               CALL SIGERR ( 'SPICE(BADFRAME)'                       )
               CALL CHKOUT ( 'ZZGFCOIN'                              )
            
            ELSE

               CALL SETMSG ( 'No RADII data in kernel pool for frame '
     .            //         '''#'' center body #. Confirm the '
     .            //         'proper input frame. Bodies {0,..,9} '
     .            //         'represent barycenters and so lack '
     .            //         'physical properties.'                 )
               CALL ERRCH  ( '#',  REF                              )
               CALL ERRINT ( '#',  SVRCTR                           )
               CALL SIGERR ( 'SPICE(BADFRAME)'                      )
               CALL CHKOUT ( 'ZZGFCOIN'                             )
            
            END IF

            RETURN
         
         END IF


C
C        We know the kernel pool contains data for body SVRCTR.
C
         CALL BODVCD ( SVRCTR, 'RADII', 3, N, SVRADI )

         IF (  FAILED() ) THEN
             CALL CHKOUT ( 'ZZGFCOIN' )
             RETURN
         END IF 


C
C        Make sure we obtained three radii.
C
         IF ( N .NE. 3 ) THEN

            CALL SETMSG ( 'Expected to find three radii defining '
     .      //            'triaxial ellipsoidal shape model for '
     .      //            'body # but instead found #.'           )
            CALL ERRINT ( '#',  SVRCTR                            )
            CALL ERRINT ( '#',  N                                 )
            CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'               )
            CALL CHKOUT ( 'ZZGFCOIN'                              )
            RETURN

         END IF

C
C        Check the radii.
C
         IF ( SVRADI(1) .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Cannot compute flattening factor. '
     .      //            'Radii are # # #.'                   )
            CALL ERRDP  ( '#', SVRADI(1)                       )
            CALL ERRDP  ( '#', SVRADI(2)                       )
            CALL ERRDP  ( '#', SVRADI(3)                       )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                )
            CALL CHKOUT ( 'ZZGFCOIN'                           )
            RETURN

         ELSE IF (      ( SVRADI(1) .LT. 0.D0 )
     .             .OR. ( SVRADI(2) .LE. 0.D0 )
     .             .OR. ( SVRADI(3) .LE. 0.D0 )  ) THEN

            CALL SETMSG ( 'Degenerate ellipsoid: radii '
     .      //            'are # # #.'                    )
            CALL ERRDP  ( '#', SVRADI(1)                  )
            CALL ERRDP  ( '#', SVRADI(2)                  )
            CALL ERRDP  ( '#', SVRADI(3)                  )
            CALL SIGERR ( 'SPICE(DEGENERATECASE)'         )
            CALL CHKOUT ( 'ZZGFCOIN'                      )
            RETURN

         END IF


C
C        For geodetic and planetographic coordinates, we need to save
C        the equatorial radius and flattening coefficient. For other
C        coordinate systems, these quantities aren't needed.
C
C        At this point, we also check for unequal equatorial radii,
C        which are not allowed with geodetic or planetographic
C        coordinates.
C        
         IF ( ( SVCSYS .EQ. GEOSYS ) .OR. ( SVCSYS .EQ. PGRSYS ) ) THEN

            IF ( SVRADI(1) .NE. SVRADI(2) ) THEN

               CALL SETMSG ( 'Central body # of reference frame # '
     .         //            'has radii # # #. Unequal equatorial '
     .         //            'ellipsoid radii are not supported for '
     .         //            '# coordinates. '                       )
               CALL ERRINT ( '#', SVRCTR                             )
               CALL ERRCH  ( '#', REF                                )
               CALL ERRDP  ( '#', SVRADI(1)                          )
               CALL ERRDP  ( '#', SVRADI(2)                          )
               CALL ERRDP  ( '#', SVRADI(3)                          )
               CALL ERRCH  ( '#', CRDSYS                             )
               CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                   )
               CALL CHKOUT ( 'ZZGFCOIN'                              )
               RETURN

            END IF

C
C           Save the equatorial radius of the central body.
C
            SVRE = SVRADI(1)

C
C           Save the flattening coefficient of the central body. Note
C           that we've ensured the denominator is non-zero.
C
            SVF  =  ( SVRADI(1) - SVRADI(3) ) / SVRADI(1)

         ELSE

            SVRE = 0.D0
            SVF  = 0.D0

         END IF

C
C        Save the computation method, if required.
C
         IF (      ( VECDEF .EQ. SOBDEF )
     .        .OR. ( VECDEF .EQ. SINDEF )  ) THEN
C
C           The coordinate is defined using a sub-observer point or
C           a surface intercept point.
C
C           Store an upper case, left-justified copy of METHOD.
C
            CALL LJUST ( METHOD, SVMETH )
            CALL UCASE ( SVMETH, SVMETH )

         ELSE
C
C           Simply initialize SVMETH with a blank string.
C
            SVMETH = ' '

         END IF

C
C        If we're using planetographic coordinates, we'll need the
C        longitude sense. Recall that the body with which these
C        coordinates are associated is the center of REF. Find the
C        longitude of the +Y axis.
C
         IF ( SVCSYS .EQ. PGRSYS ) THEN

            CALL BODC2S ( SVRCTR, SVRCNM )

            CALL RECPGR ( SVRCNM, Y, SVRE, SVF, LON, LAT, ALT )

C
C           Planetographic longitude ranges from 0 to 2*pi, so
C           longitudes corresponding to positive Y values are
C           in the range pi to 2*pi.
C            
            IF ( LON .GT. PI() ) THEN
               SVSENS =  -1
            ELSE
               SVSENS =   1
            END IF

         ELSE

            SVSENS = 0

         END IF

      END IF

C
C     If we're using a surface intercept vector definition, we'll
C     need to check and store the variables associated with the
C     ray.
C
      IF ( SVVDEF .EQ. SINDEF ) THEN

         IF ( VZERO(DVEC) ) THEN

            CALL SETMSG ( 'Ray''s direction vector is the zero '
     .      //            'vector. This variable might be '
     .      //            'uninitialized.'                      )
            CALL SIGERR ( 'SPICE(ZEROVECTOR)'                   )

         END IF

C
C        Save DVEC and DREF.
C
         CALL MOVED  ( DVEC, 3, SVDVEC )

         SVDREF = DREF

C
C        Save the center of DREF.
C
         CALL NAMFRM ( SVDREF, FRCODE )

         CALL FRINFO ( FRCODE, SVDCTR, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', DREF                                 )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFCOIN'                                )
            RETURN

         END IF

      ELSE
C
C        Simply give initial values to SVDREF, SVDCTR, and SVDVEC.
C
         SVDREF = ' '
         SVDCTR = 0

         CALL CLEARD ( 3, SVDVEC )
         
      END IF

      CALL CHKOUT ( 'ZZGFCOIN' )
      RETURN
 
 


C$Procedure ZZGFCOG ( GF, get coordinate )
 
      ENTRY ZZGFCOG ( ET, CRDVAL )
 
C$ Abstract
C
C     Compute the coordinate defined by the last call to ZZGFCOIN is at
C     the specified epoch.
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
C     ROOT
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     DOUBLE PRECISION      CRDVAL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Computation epoch.
C     CRDVAL     O   Coordinate at epoch.
C
C$ Detailed_Input
C
C     ET             is the computation epoch, expressed as seconds
C                    past J2000 TDB.
C
C$ Detailed_Output
C
C     CRDVAL         is the coordinate defined by the previous call to
C                    ZZGFCOIN, evaluated at the epoch ET.
C        
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the coordinate cannot be computed at ET, the
C         error SPICE(NOTCOMPUTABLE) is signaled.
C
C     2)  If an error occurs while this routine computes the coordinate
C         defined by ZZGFCOIN, the error will be diagnosed by routines
C         in the call tree of this routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFCOU.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     See ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C     get coordinate
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'ZZGFCOG' ) 
      END IF

      CALL ZZGFCOQ ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,      
     .               SVCORR, SVOBS,  SVDREF, SVDVEC, SVCSYS,
     .               SVRCTR, SVRE,   SVF,    SVCRD,  CRDVAL, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOG'                                     )
         RETURN

      END IF
      
      CALL CHKOUT  ( 'ZZGFCOG' ) 
      RETURN




C$Procedure ZZGFCODC ( GF, is coordinate decreasing? )
 
      ENTRY ZZGFCODC ( UDFUNC, ET, DECRES )
 
C$ Abstract
C
C     Indicate whether the coordinate defined by the last call to
C     ZZGFCOIN is decreasing at the specified epoch.
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
C     ROOT
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     LOGICAL               DECRES
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Computation epoch.
C     DECRES     O   Flag indicating if coordinate is decreasing.
C
C$ Detailed_Input
C
C     ET             is the computation epoch, expressed as seconds
C                    past J2000 TDB.
C
C$ Detailed_Output
C
C     DECRES         is a logical flag indicating whether
C                    the coordinate defined by the previous call to
C                    ZZGFCOIN is strictly decreasing at the epoch ET.
C                    DECRES is .FALSE. if the coordinate
C                    is decreasing and .TRUE. otherwise.
C
C                    In cases where the coordinate is undefined
C                    at ET, DECRES is set to .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) In cases where the any intermediate quantity required by
C        this routine is undefined, DECRES is set to .FALSE.  This
C        situation occurs when the Jacobian of the coordinate system
C        with respect to rectangular coordinates is undefined at ET.
C
C     2) If an error occurs while this routine computes the coordinate 
C        defined by ZZGFCOIN, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     3) If an error occurs while this routine computes the derivative
C        with respect to time of the coordinate defined by ZZGFCOIN, the
C        error will be diagnosed by routines in the call tree of this
C        routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFCOU.
C
C$ Particulars
C
C     A function f(x) is strictly decreasing at x0 if and only if there
C     exists some delta > 0 such that for all dx satisfying
C
C        0  <  dx  < delta
C
C     we have
C
C        f(x0)       <  f(x0 + dx)
C
C     and
C
C        f(x0 - dx)  <  f(x)
C
C     Note that a strictly decreasing function need not be
C     differentiable in a neighborhood of x0; it can have jump
C     discontinuities in any neighborhood of x0 and even at x0.
C
C$ Examples
C
C     See ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C-    SPICELIB version 2.0.0 05-APR-2011 (EDW)
C
C        Added UDFUNC to argument list for use of ZZGFRELX when
C        calculating the events.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
C
C-&

C$ Index_Entries
C
C     is coordinate decreasing
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCODC' ) 

C
C     Fetch the state from which the coordinate is derived. If the
C     state can't be computed, we consider the coordinate to be
C     "not decreasing."
C
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR,
     .                SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE,  
     .                FOUND                                          )

      IF ( .NOT. FOUND ) THEN

         DECRES = .FALSE.

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCODC'                                    )
         RETURN

      END IF

C
C     Compute the proxy for the derivative with respect to time of the
C     coordinate. This proxy gives us the sign of the derivative, which
C     is all we need to determine whether the coordinate is decreasing.
C
      CALL ZZGFCPRX ( STATE, SVCSYS, SVRE, SVF, SVSENS, CDSIGN )

C
C     The quantity is decreasing if and only if the derivative
C     is negative. This is indicated by a "sign" of -1.
C
      DECRES  =  CDSIGN(SVCIDX) .EQ. -1

      CALL CHKOUT  ( 'ZZGFCODC' )
      RETURN





C$Procedure ZZGFCOEX ( GF, does coordinate state exist? )
 
      ENTRY ZZGFCOEX ( UDFUNC, ET, CRDFND )
 
C$ Abstract
C
C     Indicate whether the state of coordinate defined by the last call
C     to ZZGFCOIN is computable at the specified epoch.
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
C     ROOT
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     LOGICAL               CRDFND
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Computation epoch.
C     CRDFND     O   Flag indicating if coordinate state is computable.
C
C$ Detailed_Input
C
C     ET             is the computation epoch, expressed as seconds
C                    past J2000 TDB.
C
C$ Detailed_Output
C
C     CRDFND         is a logical flag indicating whether the state of
C                    the coordinate defined by the previous call to
C                    ZZGFCOIN is computable at the epoch ET. DECRES is
C                    .TRUE. if the coordinate is computable and .FALSE.
C                    otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If an error occurs while this routine attempts to compute the
C        coordinate defined by ZZGFCOIN, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFCOU.
C
C$ Particulars
C
C     This routine is used by the GF system to compute a time window
C     over which a specified coordinate state is computable.
C
C     Coordinates defined by surface intercepts may fail to be
C     computable because either
C
C        - the surface intercept does not exist
C
C        - the velocity of the intercept is not computable
C
C$ Examples
C
C     See ZZGFCSLV.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C-    SPICELIB version 2.0.0 05-APR-2011 (EDW)
C
C        Added UDFUNC to argument list for use of ZZGFRELX when
C        calculating the events.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
C
C-&

C$ Index_Entries
C
C     does coordinate state exist
C     is coordinate state computable
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOEX' ) 

C
C     Simply attempt to compute the state. The returned found flag
C     is the result.
C
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR,
     .                SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE,  
     .                CRDFND                                          )

      CALL CHKOUT  ( 'ZZGFCOEX' )
      RETURN



 

C$Procedure ZZGFCOCG ( GF, get cosine of coordinate )
 
      ENTRY ZZGFCOCG ( ET, CRDVAL )
 
C$ Abstract
C
C     Compute the cosine of the coordinate defined by the last call to
C     ZZGFCOIN is at the specified epoch.
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
C     ROOT
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     DOUBLE PRECISION      CRDVAL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Computation epoch.
C     CRDVAL     O   Cosine of coordinate at epoch.
C
C$ Detailed_Input
C
C     ET             is the computation epoch, expressed as seconds
C                    past J2000 TDB.
C
C$ Detailed_Output
C
C     CRDVAL         is the cosine of the coordinate defined by the
C                    previous call to ZZGFCOIN, evaluated at the epoch
C                    ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If an error occurs while this routine computes the coordinate 
C        defined by ZZGFCOIN, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFCOU.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     See ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C     get cosine of coordinate
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOCG' ) 

      CALL ZZGFCOQ ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,      
     .               SVCORR, SVOBS,  SVDREF, SVDVEC, SVCSYS,
     .               SVRCTR, SVRE,   SVF,    SVCRD,  VALUE, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOCG'                                    )
         RETURN

      END IF

      CRDVAL = COS ( VALUE )

      CALL CHKOUT  ( 'ZZGFCOCG' ) 
      RETURN






C$Procedure ZZGFCOSG ( GF, get sine of coordinate )
 
      ENTRY ZZGFCOSG ( ET, CRDVAL )
 
C$ Abstract
C
C     Compute the sine of the coordinate defined by the last call to
C     ZZGFCOIN is at the specified epoch.
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
C     ROOT
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     DOUBLE PRECISION      CRDVAL
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Computation epoch.
C     CRDVAL     O   Sine of coordinate at epoch.
C
C$ Detailed_Input
C
C     ET             is the computation epoch, expressed as seconds
C                    past J2000 TDB.
C
C$ Detailed_Output
C
C     CRDVAL         is the sine of the coordinate defined by the
C                    previous call to ZZGFCOIN, evaluated at the epoch
C                    ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If an error occurs while this routine computes the coordinate 
C        defined by ZZGFCOIN, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFCOU.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     See ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C     get sine of coordinate
C
C-&

      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOSG' ) 

      CALL ZZGFCOQ ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,      
     .               SVCORR, SVOBS,  SVDREF, SVDVEC, SVCSYS,
     .               SVRCTR, SVRE,   SVF,    SVCRD,  VALUE, FOUND )   

      IF ( .NOT. FOUND ) THEN

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOSG'                                    )
         RETURN

      END IF   

      CRDVAL = SIN ( VALUE )

      CALL CHKOUT ( 'ZZGFCOSG' ) 
      RETURN





C$Procedure ZZGFCOCD ( GF, is cosine of coordinate decreasing? )
 
      ENTRY ZZGFCOCD ( UDFUNC, ET, DECRES )
 
C$ Abstract
C
C     Indicate whether the cosine of the coordinate defined by the
C     last call to ZZGFCOIN is decreasing at the specified epoch.
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
C     ROOT
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     LOGICAL               DECRES
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Computation epoch.
C     DECRES     O   Flag indicating if cos of coordinate is decreasing.
C
C$ Detailed_Input
C
C     ET             is the computation epoch, expressed as seconds
C                    past J2000 TDB.
C
C$ Detailed_Output
C
C     DECRES         is a logical flag indicating whether the cosine of
C                    the coordinate defined by the previous call to
C                    ZZGFCOIN is strictly decreasing at the epoch ET.
C                    DECRES is .FALSE. if the cosine of the coordinate
C                    is decreasing and .TRUE. otherwise.
C
C                    In cases where the coordinate is undefined
C                    at ET, DECRES is set to .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) In cases where the any intermediate quantity required by
C        this routine is undefined, DECRES is set to .FALSE.  This
C        situation occurs when the Jacobian of the coordinate system
C        with respect to rectangular coordinates is undefined at ET.
C
C     2) If an error occurs while this routine computes the coordinate 
C        defined by ZZGFCOIN, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     3) If an error occurs while this routine computes the derivative
C        with respect to time of the coordinate defined by ZZGFCOIN, the
C        error will be diagnosed by routines in the call tree of this
C        routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFCOU.
C
C$ Particulars
C
C     A function f(x) is strictly decreasing at x0 if and only if there
C     exists some delta > 0 such that for all dx satisfying
C
C        0  <  dx  < delta
C
C     we have
C
C        f(x0)       <  f(x0 + dx)
C
C     and
C
C        f(x0 - dx)  <  f(x)
C
C     Note that a strictly decreasing function need not be
C     differentiable in a neighborhood of x0; it can have jump
C     discontinuities in any neighborhood of x0 and even at x0.
C
C$ Examples
C
C     See ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C-    SPICELIB version 2.0.0 05-APR-2011 (EDW)
C
C        Added UDFUNC to argument list for use of ZZGFRELX when
C        calculating the events.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
C
C-&

C$ Index_Entries
C
C     is cosine of coordinate decreasing
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOCD' ) 

C
C     The derivative of cosine of the coordinate Q is
C
C         - sin ( Q(ET) ) * d( Q(ET) )/d(ET)
C
C     Look up the individual terms. Start with the Cartesian
C     state vector from whose position component Q is
C     derived.
C
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR,
     .                SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE,  
     .                FOUND                                          )
      
      IF ( .NOT. FOUND ) THEN

         DECRES = .FALSE.

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOCD'                                    )
         RETURN

      END IF         

C
C     At this point we assume the state whose coordinate is to be
C     computed resides in STATE. Convert the position portion of STATE
C     to the specified coordinate system.
C
      IF ( SVCSYS .EQ. RECSYS ) THEN
C
C        No conversion needed for rectangular coordinates.
C
         CALL MOVED ( STATE, 3, COORDS )


      ELSE IF ( SVCSYS .EQ. LATSYS ) THEN

         CALL RECLAT ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. RADSYS ) THEN

         CALL RECRAD ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. SPHSYS ) THEN

         CALL RECSPH ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. CYLSYS ) THEN

         CALL RECCYL ( STATE, COORDS(1), COORDS(2), COORDS(3) )

            
      ELSE IF ( SVCSYS .EQ. GEOSYS ) THEN
        
         CALL RECGEO ( STATE,     SVRE,      SVF, 
     .                 COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. PGRSYS ) THEN

         CALL RECPGR ( SVRCNM,    STATE,     SVRE,     SVF, 
     .                 COORDS(1), COORDS(2), COORDS(3)     )

      ELSE
C
C        We should never arrive here.
C
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFCOCD'                                  )
         RETURN

      END IF

C
C     Pick off the coordinate value.
C
      VALUE = COORDS( SVCIDX )

C
C     Compute the proxy for the derivative with respect to time of the
C     coordinate. This proxy gives us the sign of the derivative, which
C     is all we need to determine whether the coordinate is decreasing.
C
      CALL ZZGFCPRX ( STATE, SVCSYS, SVRE, SVF, SVSENS, CDSIGN )

C
C     The derivative of the coordinate is negative if the "sign" is -1.
C
      DECRES  =  ( - SIN(VALUE) * CDSIGN(SVCIDX) )  .LT.  0.D0


      CALL CHKOUT  ( 'ZZGFCOCD' )
      RETURN




C$Procedure ZZGFCOSD ( GF, is sine of coordinate decreasing? )
 
      ENTRY ZZGFCOSD ( UDFUNC, ET, DECRES )
 
C$ Abstract
C
C     Indicate whether the sine of the coordinate defined by the
C     last call to ZZGFCOIN is decreasing at the specified epoch.
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
C     ROOT
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C     LOGICAL               DECRES
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Computation epoch.
C     DECRES     O   Flag indicating if sine of coordinate is 
C                    decreasing.
C
C$ Detailed_Input
C
C     ET             is the computation epoch, expressed as seconds
C                    past J2000 TDB.
C
C$ Detailed_Output
C
C     DECRES         is a logical flag indicating whether the sine
C                    of the coordinate defined by the previous call to
C                    ZZGFCOIN is strictly decreasing at the epoch ET.
C                    DECRES is .FALSE. if the sine of the coordinate is
C                    decreasing and .TRUE. otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) In cases where the any intermediate quantity required by
C        this routine is undefined, DECRES is set to .FALSE.  This
C        situation occurs when the Jacobian of the coordinate system
C        with respect to rectangular coordinates is undefined at ET.
C
C     2) If an error occurs while this routine computes the coordinate 
C        defined by ZZGFCOIN, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     3) If an error occurs while this routine computes the derivative
C        with respect to time of the coordinate defined by ZZGFCOIN, the
C        error will be diagnosed by routines in the call tree of this
C        routine.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFCOU.
C
C$ Particulars
C
C     A function f(x) is strictly decreasing at x0 if and only if there
C     exists some delta > 0 such that for all dx satisfying
C
C        0  <  dx  < delta
C
C     we have
C
C        f(x0)       <  f(x0 + dx)
C
C     and
C
C        f(x0 - dx)  <  f(x)
C
C     Note that a strictly decreasing function need not be
C     differentiable in a neighborhood of x0; it can have jump
C     discontinuities in any neighborhood of x0 and even at x0.
C
C$ Examples
C
C     See ZZGFLONG.
C
C$ Restrictions
C
C     1)  The interface and functionality of this set of routines may
C         change without notice.  These routines should be called only
C         by SPICELIB routines.
C
C     2)  ZZGFCOIN must be called prior to use of any of the other
C         entry points.
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
C-    SPICELIB version 2.0.0 05-APR-2011 (EDW)
C
C        Added UDFUNC to argument list for use of ZZGFRELX when
C        calculating the events.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
C
C-&

C$ Index_Entries
C
C     is sine of coordinate decreasing
C
C-&

C
C     Standard SPICE error handling.
C
 
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFCOSD' ) 

C
C     The derivative of the sine of the coordinate Q is
C
C        cos ( Q(ET) ) * d( Q(ET) )/d(ET)
C
C     Look up the individual terms. Start with the Cartesian state
C     vector from whose position component Q is derived.
C
      CALL ZZGFCOST ( SVVDEF, SVMETH, SVTARG, ET,     SVREF,  SVCORR,
     .                SVOBS,  SVDREF, SVDCTR, SVDVEC, SVRADI, STATE,  
     .                FOUND                                          )

      
      IF ( .NOT. FOUND ) THEN

         DECRES = .FALSE.

         CALL ETCAL  ( ET, TIMSTR )

         CALL SETMSG ( 'Coordinate # could not be computed at # TDB' )
         CALL ERRCH  ( '#', SVCRD                                    )
         CALL ERRCH  ( '#', TIMSTR                                   )
         CALL SIGERR ( 'SPICE(NOTCOMPUTABLE)'                        )
         CALL CHKOUT ( 'ZZGFCOSD'                                    )
         RETURN

      END IF         

C
C     At this point we assume the state whose coordinate is to be
C     computed resides in STATE. Convert the position portion of STATE
C     to the specified coordinate system.
C
      IF ( SVCSYS .EQ. RECSYS ) THEN
C
C        No conversion needed for rectangular coordinates.
C
         CALL MOVED ( STATE, 3, COORDS )


      ELSE IF ( SVCSYS .EQ. LATSYS ) THEN

         CALL RECLAT ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. RADSYS ) THEN

         CALL RECRAD ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. SPHSYS ) THEN

         CALL RECSPH ( STATE, COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. CYLSYS ) THEN

         CALL RECCYL ( STATE, COORDS(1), COORDS(2), COORDS(3) )

            
      ELSE IF ( SVCSYS .EQ. GEOSYS ) THEN
        
         CALL RECGEO ( STATE,     SVRE,      SVF, 
     .                 COORDS(1), COORDS(2), COORDS(3) )


      ELSE IF ( SVCSYS .EQ. PGRSYS ) THEN

         CALL RECPGR ( SVRCNM,    STATE,     SVRE,     SVF, 
     .                 COORDS(1), COORDS(2), COORDS(3)     )

      ELSE
C
C        We should never arrive here.
C
         CALL SETMSG ( 'The coordinate system # is not supported.' )
         CALL ERRCH  ( '#',  CRDSYS                                )
         CALL SIGERR ( 'SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFCOSD'                                  )
         RETURN

      END IF

C
C     Pick off the coordinate value.
C
      VALUE = COORDS( SVCIDX )

C
C     Compute the proxy for the derivative with respect to time of the
C     coordinate. This proxy gives us the sign of the derivative, which
C     is all we need to determine whether the coordinate is decreasing.
C
      CALL ZZGFCPRX ( STATE, SVCSYS, SVRE, SVF, SVSENS, CDSIGN )

C
C     The derivative of the coordinate is negative if the "sign" is -1.
C
      DECRES  =  ( COS(VALUE) * CDSIGN(SVCIDX) )  .LT.  0.D0

      CALL CHKOUT  ( 'ZZGFCOSD' )
      RETURN


      END
