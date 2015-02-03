C$Procedure ILLUMG ( Illumination angles, general source )
 
      SUBROUTINE ILLUMG ( METHOD, TARGET, ILLUM,  ET,     
     .                    FIXREF, ABCORR, OBSRVR, SPOINT,  
     .                    TRGEPC, SRFVEC, PHASE,  INCDNC, EMISSN )
 
C$ Abstract
C
C     Find the illumination angles (phase, incidence, and
C     emission) at a specified surface point of a target body.
C     The illumination source is a specified ephemeris object.
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
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     MOSPICE
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzabcorr.inc'
      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      INCDNC
      DOUBLE PRECISION      EMISSN
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body.
C     ILLUM      I   Name of illumination source.
C     ET         I   Epoch in ephemeris seconds past J2000 TDB.
C     FIXREF     I   Body-fixed, body-centered target body frame.
C     ABCORR     I   Desired aberration correction.
C     OBSRVR     I   Name of observing body.
C     SPOINT     I   Body-fixed coordinates of a target surface point.
C     TRGEPC     O   Target surface point epoch.
C     SRFVEC     O   Vector from observer to target surface point.
C     PHASE      O   Phase angle at the surface point.
C     INCDNC     O   Source incidence angle at the surface point.
C     EMISSN     O   Emission angle at the surface point.
C
C$ Detailed_Input
C
C
C     METHOD      is a short string providing parameters defining
C                 the computation method to be used. Parameters
C                 include, but are not limited to, the shape model
C                 used to represent the surface of the target body.
C
C                 The only choice currently supported is
C
C                    'Ellipsoid'        The illumination angle
C                                       computation uses a triaxial
C                                       ellipsoid to model the surface
C                                       of the target body. The
C                                       ellipsoid's radii must be
C                                       available in the kernel pool.
C
C                 Neither case nor white space are significant in 
C                 METHOD. For example, the string ' eLLipsoid ' is 
C                 valid.                 
C
C
C     TARGET      is the name of the target body. TARGET is
C                 case-insensitive, and leading and trailing blanks in
C                 TARGET are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object. For example both 'MOON' and '301' are
C                 legitimate strings that indicate the Moon is the
C                 target body.
C
C
C     ILLUM       is the name of the illumination source. This source
C                 may be any ephemeris object. Case, blanks, and
C                 numeric values are treated in the same way as for the
C                 input TARGET.
C                 
C
C     ET          is the epoch, expressed as seconds past J2000 TDB,
C                 for which the apparent illumination angles at the
C                 specified surface point on the target body, as seen
C                 from the observing body, are to be computed.
C
C
C     FIXREF      is the name of the body-fixed, body-centered
C                 reference frame associated with the target body. The
C                 input surface point SPOINT and the output vector
C                 SRFVEC are expressed relative to this reference
C                 frame. The string FIXREF is case-insensitive, and
C                 leading and trailing blanks in FIXREF are not
C                 significant.
C
C
C     ABCORR      is the aberration correction to be used in computing
C                 the position and orientation of the target body and
C                 the location of the illumination source.
C         
C                 For remote sensing applications, where the apparent
C                 illumination angles seen by the observer are desired,
C                 normally either of the corrections 
C              
C                    'LT+S' 
C                    'CN+S'
C     
C                 should be used. These and the other supported options
C                 are described below. ABCORR may be any of the 
C                 following:
C
C                    'NONE'     No aberration correction.
C
C                 Let LT represent the one-way light time between the
C                 observer and SPOINT (note: NOT between the observer
C                 and the target body's center). The following values
C                 of ABCORR apply to the "reception" case in which
C                 photons depart from SPOINT at the light-time
C                 corrected epoch ET-LT and *arrive* at the observer's
C                 location at ET:
C
C                    'LT'       Correct both the position of SPOINT as
C                               seen by the observer, and the position
C                               of the illumination source as seen by
C                               the target, for light time.
C
C                    'LT+S'     Correct both the position of SPOINT as
C                               seen by the observer, and the position
C                               of the illumination source as seen by
C                               the target, for light time and stellar
C                               aberration.
C
C                    'CN'       Converged Newtonian light time
C                               correction. In solving the light time
C                               equations for target and the
C                               illumination source, the "CN"
C                               correction iterates until the solution
C                               converges.
C
C                    'CN+S'     Converged Newtonian light time and
C                               stellar aberration corrections. This
C                               option produces a solution that is at
C                               least as accurate at that obtainable
C                               with the 'LT+S' option. Whether the
C                               'CN+S' solution is substantially more
C                               accurate depends on the geometry of the
C                               participating objects and on the
C                               accuracy of the input data. In all
C                               cases this routine will execute more
C                               slowly when a converged solution is
C                               computed.
C
C                 Neither case nor white space are significant in
C                 ABCORR. For example, the string 
C
C                   'Lt + s'
C
C                 is valid.
C                               
C
C     OBSRVR      is the name of the observing body. The observing body
C                 is an ephemeris object: it typically is a spacecraft,
C                 the earth, or a surface point on the earth. OBSRVR is
C                 case-insensitive, and leading and trailing blanks in
C                 OBSRVR are not significant. Optionally, you may
C                 supply a string containing the integer ID code for
C                 the object. For example both 'MOON' and '301' are
C                 legitimate strings that indicate the Moon is the
C                 observer.
C
C                 OBSRVR may be not be identical to TARGET.
C
C
C     SPOINT      is a surface point on the target body, expressed in
C                 Cartesian coordinates, relative to the body-fixed
C                 target frame designated by FIXREF.
C
C                 SPOINT need not be visible from the observer's
C                 location at the epoch ET.
C
C                 The components of SPOINT have units of km.
C
C
C$ Detailed_Output
C
C
C     TRGEPC      is the "surface point epoch." TRGEPC is defined as
C                 follows: letting LT be the one-way light time between
C                 the observer and the input surface point SPOINT,
C                 TRGEPC is either the epoch ET-LT or ET depending on
C                 whether the requested aberration correction is,
C                 respectively, for received radiation or omitted. LT
C                 is computed using the method indicated by ABCORR.
C
C                 TRGEPC is expressed as seconds past J2000 TDB.
C
C
C     SRFVEC      is the vector from the observer's position at ET to
C                 the aberration-corrected (or optionally, geometric)
C                 position of SPOINT, where the aberration corrections
C                 are specified by ABCORR. SRFVEC is expressed in the
C                 target body-fixed reference frame designated by
C                 FIXREF, evaluated at TRGEPC.
C  
C                 The components of SRFVEC are given in units of km.
C
C                 One can use the SPICELIB function VNORM to obtain the
C                 distance between the observer and SPOINT:
C
C                    DIST = VNORM ( SRFVEC )
C
C                 The observer's position OBSPOS, relative to the
C                 target body's center, where the center's position is
C                 corrected for aberration effects as indicated by
C                 ABCORR, can be computed via the call:
C
C                    CALL VSUB ( SPOINT, SRFVEC, OBSPOS )
C
C                 To transform the vector SRFVEC to a time-dependent
C                 reference frame REF at ET, a sequence of calls is
C                 required. For example, let XFORM be 3x3 matrix
C                 describing the transformation between the target
C                 body-fixed frame at TRGEPC to the time-dependent
C                 frame REF at ET. Then SRFVEC can be transformed to
C                 the result REFVEC as follows:
C
C                     CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM )
C                     CALL MXV    ( XFORM,  SRFVEC, REFVEC )
C
C
C     PHASE       is the phase angle at SPOINT, as seen from OBSRVR at
C                 time ET. This is the angle between the negative of
C                 the vector SRFVEC and the SPOINT-source vector at
C                 TRGEPC. Units are radians. The range of PHASE is
C                 [0, pi].  
C
C     INCDNC      is the illumination source incidence angle at SPOINT,
C                 as seen from OBSRVR at time ET. This is the angle
C                 between the surface normal vector at SPOINT and the
C                 SPOINT-source vector at TRGEPC. Units are radians.
C                 The range of INCDNC is [0, pi].
C
C     EMISSN      is the emission angle at SPOINT, as seen from OBSRVR
C                 at time ET. This is the angle between the surface
C                 normal vector at SPOINT and the negative of the
C                 vector SRFVEC. Units are radians. The range of EMISSN
C                 is [0, pi]. 
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C
C     1)  If the specified aberration correction is relativistic or
C         calls for stellar aberration but not light time correction,
C         the error SPICE(NOTSUPPORTED) is signaled. If the specified
C         aberration correction is any other unrecognized value, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     2)  If any of the target, observer, or illumination source 
C         input strings cannot be converted to an integer ID code, the
C         error SPICE(IDCODENOTFOUND) is signaled.
C
C     3)  If OBSRVR and TARGET map to the same NAIF integer ID code,
C         the error SPICE(BODIESNOTDISTINCT) is signaled.
C
C     4)  If the input target body-fixed frame FIXREF is not
C         recognized, the error SPICE(NOFRAME) is signaled. A frame
C         name may fail to be recognized because a required frame
C         specification kernel has not been loaded; another cause is a
C         misspelling of the frame name.
C
C     5)  If the input frame FIXREF is not centered at the target body,
C         the error SPICE(INVALIDFRAME) is signaled.
C
C     6)  If the input argument METHOD is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C     7)  If the target and observer have distinct identities but are
C         at the same location (for example, the target is Mars and the
C         observer is the Mars barycenter), the error
C         SPICE(NOSEPARATION) is signaled.
C
C     8)  If insufficient ephemeris data have been loaded prior to
C         calling ILLUMG, the error will be diagnosed and signaled by a
C         routine in the call tree of this routine. Note that when
C         light time correction is used, sufficient ephemeris data must
C         be available to propagate the states of observer, target, and
C         the illumination source to the solar system barycenter.
C
C     9)  If the computation method specifies an ellipsoidal target
C         shape and triaxial radii of the target body have not been
C         loaded into the kernel pool prior to calling ILLUMG, the
C         error will be diagnosed and signaled by a routine in the call
C         tree of this routine.
C
C     10) The target must be an extended body: if any of the radii of
C         the target body are non-positive, the error will be
C         diagnosed and signaled by routines in the call tree of this
C         routine.
C
C     11) If PCK data specifying the target body-fixed frame
C         orientation have not been loaded prior to calling ILLUMG,
C         the error will be diagnosed and signaled by a routine in the
C         call tree of this routine.
C
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target, observer, and the
C          illumination source must be loaded. If aberration
C          corrections are used, the states of target, observer, and
C          the illumination source relative to the solar system
C          barycenter must be calculable from the available ephemeris
C          data. Typically ephemeris data are made available by loading
C          one or more SPK files via FURNSH.
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
C
C$ Particulars
C
C
C     The term "illumination angles" refers to following set of
C     angles:
C
C
C        phase angle              Angle between the vectors from the
C                                 surface point to the observer and
C                                 from the surface point to the 
C                                 illumination source.
C
C        incidence angle          Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 illumination source.
C
C        emission angle           Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 observer.
C 
C     The diagram below illustrates the geometric relationships
C     defining these angles. The labels for the incidence, emission,
C     and phase angles are "inc.", "e.", and "phase".
C
C
C                                                      *
C                                              illumination source
C
C                    surface normal vector
C                              ._                 _.
C                              |\                 /|  illumination 
C                                \    phase      /    source vector
C                                 \   .    .    /
C                                 .            .
C                                   \   ___   /
C                              .     \/     \/
C                                    _\ inc./
C                             .    /   \   /
C                             .   |  e. \ /
C         *             <--------------- *  surface point on
C      viewing            vector            target body
C      location           to viewing
C      (observer)         location
C
C
C     Note that if the target-observer vector, the target normal vector
C     at the surface point, and the target-illumination source vector
C     are coplanar, then phase is the sum of the incidence and emission
C     angles. This rarely occurs; usually
C
C        phase angle  <  incidence angle + emission angle
C
C     All of the above angles can be computed using light time
C     corrections, light time and stellar aberration corrections, or no
C     aberration corrections. In order to describe apparent geometry as
C     observed by a remote sensing instrument, both light time and
C     stellar aberration corrections should be used.
C     
C     The way aberration corrections are applied by this routine
C     is described below.
C
C        Light time corrections
C        ======================
C
C           Observer-target surface point vector
C           ------------------------------------
C
C           Let ET be the epoch at which an observation or remote
C           sensing measurement is made, and let ET - LT ("LT" stands
C           for "light time") be the epoch at which the photons
C           received at ET were emitted from the surface point SPOINT.
C           Note that the light time between the surface point and
C           observer will generally differ from the light time between
C           the target body's center and the observer.
C
C
C           Target body's orientation
C           -------------------------
C
C           Using the definitions of ET and LT above, the target body's
C           orientation at ET - LT is used. The surface normal is
C           dependent on the target body's orientation, so the body's
C           orientation model must be evaluated for the correct epoch.
C
C
C           Target body -- illumination source vector
C           -----------------------------------------
C
C           The surface features on the target body near SPOINT will
C           appear in a measurement made at ET as they were at ET-LT.
C           In particular, lighting on the target body is dependent on
C           the apparent location of the illumination source as seen
C           from the target body at ET-LT. So, a second light time
C           correction is used to compute the position of the
C           illumination source relative to the surface point.
C
C
C        Stellar aberration corrections
C        ==============================
C
C        Stellar aberration corrections are applied only if
C        light time corrections are applied as well.
C
C           Observer-target surface point body vector
C           -----------------------------------------
C
C           When stellar aberration correction is performed, the
C           direction vector SRFVEC is adjusted so as to point to the
C           apparent position of SPOINT: considering SPOINT to be an
C           ephemeris object, SRFVEC points from the observer's
C           position at ET to the light time and stellar aberration
C           corrected position of SPOINT.
C
C           Target body-illumination source vector
C           --------------------------------------
C
C           The target body-illumination source vector is the apparent
C           position of the illumination source, corrected for light
C           time and stellar aberration, as seen from the target body
C           at time ET-LT.
C
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1) Find the phase, solar incidence, and emission angles at the
C        sub-solar and sub-spacecraft points on Mars as seen from the
C        Mars Global Surveyor spacecraft at a specified UTC time. Use
C        light time and stellar aberration corrections.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C 
C
C        KPL/MK
C
C           File: illumg.tm
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
C              de421.bsp                     Planetary ephemeris
C              pck00010.tpc                  Planet orientation and
C                        radii
C              naif0010.tls                  Leapseconds
C              mgs_ext13_ipng_mgs95j.bsp     MGS ephemeris
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00010.tpc',
C                                  'naif0010.tls',
C                                  'mgs_ext13_ipng_mgs95j.bsp'  )
C           \begintext
C
C
C        Example code begins here.
C
C
C           PROGRAM ANGLES
C           IMPLICIT NONE
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      DPR
C     C
C     C     Local parameters
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META   = 'illumg.tm' )
C
C           INTEGER               NAMLEN
C           PARAMETER           ( NAMLEN = 32 )
C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 25 )
C
C           INTEGER               CORLEN
C           PARAMETER           ( CORLEN = 5 )
C     C
C     C     Local variables
C     C
C           CHARACTER*(CORLEN)    ABCORR
C           CHARACTER*(NAMLEN)    OBSRVR
C           CHARACTER*(NAMLEN)    TARGET
C           CHARACTER*(TIMLEN)    UTC
C
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      SRFVEC ( 3 )
C           DOUBLE PRECISION      SSCEMI
C           DOUBLE PRECISION      SSCPHS
C           DOUBLE PRECISION      SSCPT  ( 3 )
C           DOUBLE PRECISION      SSCSOL
C           DOUBLE PRECISION      SSLEMI
C           DOUBLE PRECISION      SSLPHS
C           DOUBLE PRECISION      SSLSOL
C           DOUBLE PRECISION      SSOLPT ( 3 )
C           DOUBLE PRECISION      TRGEPC
C
C     C
C     C     Load kernel files.
C     C
C           CALL FURNSH ( META )
C     C
C     C     Convert the UTC request time string to seconds past 
C     C     J2000 TDB.
C     C
C           UTC = '2004 JAN 1 12:00:00'
C
C           CALL UTC2ET ( UTC, ET )
C
C     C
C     C     Assign observer and target names. The acronym MGS
C     C     indicates Mars Global Surveyor. See NAIF_IDS for a
C     C     list of names recognized by SPICE. Also set the
C     C     aberration correction flag.
C     C
C           TARGET = 'Mars'
C           OBSRVR = 'MGS'
C           ABCORR = 'CN+S'
C     C
C     C     Find the sub-solar point on the Earth as seen from
C     C     the MGS spacecraft at ET. Use the "near point: ellipsoid"
C     C     style of sub-point definition. This makes it easy
C     C     to verify the solar incidence angle.
C     C
C           CALL SUBSLR ( 'Near point: ellipsoid',
C          .              TARGET,  ET,      'IAU_MARS',
C          .              ABCORR,  OBSRVR,  SSOLPT, TRGEPC, SRFVEC )
C     C
C     C     Now find the sub-spacecraft point.
C     C
C           CALL SUBPNT ( 'Near point: ellipsoid',
C          .              TARGET,  ET,     'IAU_MARS',
C          .              ABCORR,  OBSRVR, SSCPT,   TRGEPC, SRFVEC )
C     C
C     C     Find the phase, solar incidence, and emission
C     C     angles at the sub-solar point on the Earth as seen
C     C     from MGS at time ET.
C     C
C           CALL ILLUMG ( 'Ellipsoid', TARGET, 'SUN',  ET,    
C          .              'IAU_MARS',  ABCORR, OBSRVR, SSOLPT, 
C          .              TRGEPC,      SRFVEC, SSLPHS, SSLSOL, SSLEMI )
C     C
C     C     Do the same for the sub-spacecraft point.
C     C
C           CALL ILLUMG ( 'Ellipsoid', TARGET, 'SUN',  ET,  
C          .              'IAU_MARS',  ABCORR, OBSRVR, SSCPT, 
C          .               TRGEPC,     SRFVEC, SSCPHS, SSCSOL, SSCEMI )
C     C
C     C     Convert the angles to degrees and write them out.
C     C
C           SSLPHS = DPR() * SSLPHS
C           SSLSOL = DPR() * SSLSOL
C           SSLEMI = DPR() * SSLEMI
C
C           SSCPHS = DPR() * SSCPHS
C           SSCSOL = DPR() * SSCSOL
C           SSCEMI = DPR() * SSCEMI
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'UTC epoch is ', UTC
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Illumination angles at the sub-solar point:'
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Phase angle           (deg.): ', SSLPHS
C           WRITE (*,*) 'Solar incidence angle (deg.): ', SSLSOL
C           WRITE (*,*) 'Emission angle        (deg.): ', SSLEMI
C           WRITE (*,*) ' '
C           WRITE (*,*) 'The solar incidence angle should be 0.'
C           WRITE (*,*) 'The emission and phase angles should be equal.'
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Illumination angles at the sub-s/c point:'
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Phase angle           (deg.): ', SSCPHS
C           WRITE (*,*) 'Solar incidence angle (deg.): ', SSCSOL
C           WRITE (*,*) 'Emission angle        (deg.): ', SSCEMI
C           WRITE (*,*) ' '
C           WRITE (*,*) 'The emission angle should be 0.'
C           WRITE (*,*) 'The solar incidence and phase angles should '
C          .//          'be equal.'
C           END
C
C
C     When this program was executed on a PC/Linux/gfortran platform,
C     the output was:
C
C
C        UTC epoch is 2004 JAN 1 12:00:00
C
C        Illumination angles at the sub-solar point:
C
C        Phase angle           (deg.):    115.54199464940093
C        Solar incidence angle (deg.):   8.27288196025359598E-015
C        Emission angle        (deg.):    115.54199464940093
C
C        The solar incidence angle should be 0.
C        The emission and phase angles should be equal.
C
C        Illumination angles at the sub-s/c point:
C
C        Phase angle           (deg.):    62.083997890874976
C        Solar incidence angle (deg.):    62.083997892615827
C        Emission angle        (deg.):   2.13680201386761237E-009
C
C        The emission angle should be 0.
C        The solar incidence and phase angles should be equal.
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
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-MAR-2014 (NJB)(BVS)
C
C-&
 
C$ Index_Entries
C
C     illumination angles general source
C     lighting angles general source
C     phase angle general source
C     incidence angle general source
C     emission angle general source
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEP
      
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  =  'ILLUMG' )
     
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Saved body name length.
C
      INTEGER               MAXL
      PARAMETER           ( MAXL   = 36 )

C
C     Saved frame name length.
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )



C
C     Local variables
C
      CHARACTER*(LNSIZE)    LOCMTH
      CHARACTER*(LNSIZE)    PRVMTH
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTI
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      OPSTAT ( 6 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      TISTAT ( 6 )

      INTEGER               CENTER
      INTEGER               N
      INTEGER               OBSCDE
      INTEGER               REFCDE
      INTEGER               TRGCDE
      INTEGER               TYPE
      INTEGER               TYPEID
      
      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               ELIPSD
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               USELT
      LOGICAL               XMIT

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

C
C     Saved frame name/ID item declarations.
C
      INTEGER               SVCTR3 ( CTRSIZ )
      CHARACTER*(FRNMLN)    SVFREF
      INTEGER               SVREFC



C
C     Saved variables
C
      SAVE                  CENTER
      SAVE                  ELIPSD
      SAVE                  FIRST
      SAVE                  PRVCOR
      SAVE                  PRVMTH
      SAVE                  TRGCDE
      SAVE                  USELT

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

C
C     Saved frame name/ID items.
C
      SAVE                  SVCTR3
      SAVE                  SVFREF
      SAVE                  SVREFC



C
C     Note: XMIT need not be saved, since it's used only 
C     for error checking when an aberration correction flag
C     is parsed.
C

C
C     Initial values
C
      DATA                  ELIPSD  / .TRUE. /
      DATA                  FIRST   / .TRUE. /
      DATA                  PRVCOR  / ' '    /
      DATA                  PRVMTH  / 'Ellipsoid' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )
  

C
C     Counter initialization is done separately.
C
      IF ( FIRST ) THEN

C
C        Initialize counters.
C
         CALL ZZCTRUIN( SVCTR1 )
         CALL ZZCTRUIN( SVCTR2 )
         CALL ZZCTRUIN( SVCTR3 )

      END IF

C
C     If necessary, parse the aberration correction flag.
C
      IF (  FIRST  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN

C
C        The aberration correction flag differs from the value it
C        had on the previous call, if any. Analyze the new flag.
C
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

C
C        The aberration correction flag is recognized; save it.
C
         PRVCOR = ABCORR

C
C        Set logical flags indicating the attributes of the requested
C        correction:
C
C           XMIT is .TRUE. when the correction is for transmitted
C           radiation.
C
C           USELT is .TRUE. when any type of light time correction
C           (normal or converged Newtonian) is specified.
C
C           USESTL indicates stellar aberration corrections.
C
C
C        The above definitions are consistent with those used by
C        ZZVALCOR.
C 
         XMIT    =  ATTBLK ( XMTIDX )
         USELT   =  ATTBLK ( LTIDX  )
C
C        Reject an aberration correction flag calling for transmission
C        corrections.
C
         IF ( XMIT ) THEN

            CALL SETMSG ( 'Aberration correction flag # calls for '
     .      //            'transmission-style corrections.'        )
            CALL ERRCH  ( '#', ABCORR                              )
            CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                    )
            CALL CHKOUT ( RNAME                                    )
            RETURN

         END IF

C
C       We do NOT set FIRST to .FALSE. here, since we're not
C       yet done with it.
C
      END IF

C
C     If necessary, parse the method specification. PRVMTH
C     and the derived flags NEAR and ELIPSD start out with
C     valid values. PRVMTH records the last valid value of
C     METHOD; ELIPSD is the corresponding shape flag.
C
      IF (  FIRST  .OR.  ( METHOD .NE. PRVMTH )  ) THEN         

C
C        Parse the computation method specification. Work with a local
C        copy of the method specification that contains no leading or
C        embedded blanks.
C
         CALL LJUCRS ( 0, METHOD, LOCMTH )

C
C        Check the shape specification.
C
         IF ( LOCMTH .NE. 'ELLIPSOID'  ) THEN

            CALL SETMSG ( 'Computation method argument was <#>; this ' 
     .      //            'string must specify a supported shape '
     .      //            'model and computation type. See the '
     .      //            'header of SUBSLR for details.'            )
            CALL ERRCH  ( '#',  METHOD                               )
            CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                     )
            CALL CHKOUT ( RNAME                                      )
            RETURN

         END IF

C
C        At this point the method specification has passed our tests.
C        Use the flag ELIPSD to indicate that the shape is modeled as
C        an ellipsoid (which is true, for now).
C
         ELIPSD = .TRUE.
C
C        Save the current value of METHOD.
C
         PRVMTH = METHOD

      END IF

C
C     We're done with all tasks that must be executed on the first
C     pass.
C
      FIRST = .FALSE.

C
C     Obtain integer codes for the target, observer, and
C     illumination source.
C 
      CALL ZZBODS2C ( SVCTR1, SVTARG, SVTCDE, SVFND1,
     .                TARGET, TRGCDE, FND    )
            
      IF ( .NOT. FND ) THEN
      
         CALL SETMSG ( 'The target, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF
      
      
      CALL ZZBODS2C ( SVCTR2, SVOBSR, SVOBSC, SVFND2,
     .                OBSRVR, OBSCDE, FND    )
      
      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF
      
C
C     Check the observer and target body codes. If they are equal,
C     signal an error.
C
      IF ( OBSCDE .EQ. TRGCDE ) THEN

         CALL SETMSG ( 'In computing illumination angles, '   
     .   //            'the observing body and target body are the '
     .   //            'same. Both are #.'                          )
         CALL ERRCH  ( '#',  OBSRVR                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                   )
         CALL CHKOUT ( RNAME                                        )
         RETURN

      END IF

C
C     Determine the attributes of the frame designated by FIXREF.
C 
      CALL ZZNAMFRM ( SVCTR3, SVFREF, SVREFC, FIXREF, REFCDE )

      CALL FRINFO ( REFCDE, CENTER, TYPE, TYPEID, FND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      IF ( .NOT. FND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' //
     .                 'the SPICE frame subsystem. Possibly '    //
     .                 'a required frame definition kernel has ' //
     .                 'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(NOFRAME)'                          )
         CALL CHKOUT ( RNAME                                     )
         RETURN

      END IF
 
C
C     Make sure that FIXREF is centered at the target body's center.
C
      IF ( CENTER .NE. TRGCDE ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' 
     .   //            'target body #. The ID code of the frame '
     .   //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  CENTER                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( RNAME                                      )
         RETURN

      END IF


C
C     Get the sign S prefixing LT in the expression for TRGEPC.
C     When light time correction is not used, setting S = 0
C     allows us to seamlessly set TRGEPC equal to ET.
C
C     We don't support transmission corrections, so S is never
C     set to 1.
C
      IF ( USELT ) THEN
         S = -1.D0         
      ELSE
         S =  0.D0
      END IF
 
C
C     Look up the state of the surface point relative to the observer.
C     The body-fixed frame of the surface point is to be evaluated
C     at the epoch of the surface point, not at the epoch of the 
C     center of the frame; we indicate this by setting the input
C     argument EVLREF to 'TARGET'.
C
      CALL SPKCPT ( SPOINT,   TARGET, FIXREF, ET,     FIXREF,
     .              'TARGET', ABCORR, OBSRVR, OPSTAT, LT     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C
C     TRGEPC is the epoch associated with the surface point. Below,
C     since S is set to 0.D0 if no aberration corrections are used, we
C     require no logical branch.
C      
      TRGEPC = ET  +  ( S * LT )

C
C     Now find the state of the illumination source as seen by
C     the surface point at TRGEPC. We want to evaluate the orientation
C     of the body-fixed frame of the surface point at the epoch
C     associated with the surface point, not at the epoch associated
C     with the frame's center;  we indicate this by setting the input
C     argument EVLREF to 'OBSERVER'.
C
      CALL SPKCPO ( ILLUM,  TRGEPC, FIXREF, 'OBSERVER', ABCORR,
     .              SPOINT, TARGET, FIXREF, TISTAT,     LTI     )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

C 
C     Find the surface normal at SPOINT. This computation depends
C     on target body shape model.  
C
      IF ( ELIPSD ) THEN
C
C        We'll need the radii of the target body.
C
         CALL BODVCD ( TRGCDE, 'RADII', 3, N, RADII )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

         CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
       
      ELSE
C
C        We've already checked the computation method input argument,
C        so we don't expect to arrive here. This code is present for
C        safety.
C      
         CALL SETMSG ( 'The computation method # was not recognized. ' )
         CALL ERRCH  ( '#',  METHOD                                    )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                          )
         CALL CHKOUT ( RNAME                                           )
         RETURN
         
      END IF

C
C     We'll need the negative of the observer-surface point position
C     for the following angle computation. Set the output SRFVEC while
C     we're at it.
C
      CALL VEQU   ( OPSTAT, SRFVEC )
      CALL VMINUS ( SRFVEC, OBSPOS )

C
C     Find the illumination angles. VSEP will give us angular
C     separation in radians.
C
      PHASE   =  VSEP ( OBSPOS, TISTAT )
      INCDNC  =  VSEP ( NORMAL, TISTAT )
      EMISSN  =  VSEP ( NORMAL, OBSPOS )

C
C     TRGEPC has already been set.
C
      CALL CHKOUT ( RNAME )
      RETURN
      END
