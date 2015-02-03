C$Procedure ILLUM ( Illumination angles )
 
      SUBROUTINE ILLUM ( TARGET,  ET,     ABCORR,  OBSRVR,  
     .                   SPOINT,  PHASE,  SOLAR,   EMISSN )
 
C$ Abstract
C
C     Deprecated: This routine has been superseded by the SPICELIB
C     routine ILUMIN. This routine is supported for purposes of
C     backward compatibility only.
C
C     Find the illumination angles at a specified surface point of a
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
C     KERNEL
C     NAIF_IDS
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

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         TARGET
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      EMISSN
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARGET     I   Name of target body.
C     ET         I   Epoch in ephemeris seconds past J2000.
C     ABCORR     I   Desired aberration correction.
C     OBSRVR     I   Name of observing body.
C     SPOINT     I   Body-fixed coordinates of a target surface point.
C     PHASE      O   Phase angle at the surface point.
C     SOLAR      O   Solar incidence angle at the surface point.
C     EMISSN     O   Emission angle at the surface point.
C
C$ Detailed_Input
C
C     TARGET         is the name of the target body.  TARGET is
C                    case-insensitive, and leading and trailing blanks
C                    in TARGET are not significant. Optionally, you may
C                    supply a string containing the integer ID code for
C                    the object.  For example both 'MOON' and '301' are
C                    legitimate strings that indicate the moon is the
C                    target body.
C
C     ET             is the epoch, specified in ephemeris seconds past
C                    J2000, at which the apparent illumination angles at
C                    the specified surface point on the target body, as
C                    seen from the observing body, are to be computed.
C
C     ABCORR         is the aberration correction to be used in
C                    computing the location and orientation of the
C                    target body and the location of the Sun.  Possible
C                    values are:
C
C                       'NONE'        No aberration correction.
C
C                       'LT'          Correct the position and
C                                     orientation of target body for
C                                     light time, and correct the
C                                     position of the Sun for light
C                                     time.
C
C                       'LT+S'        Correct the observer-target vector
C                                     for light time and stellar
C                                     aberration, correct the
C                                     orientation of the target body
C                                     for light time, and correct the
C                                     target-Sun vector for light time
C                                     and stellar aberration.
C
C                       'CN'          Converged Newtonian light time
C                                     correction. In solving the light
C                                     time equation, the 'CN'
C                                     correction iterates until the
C                                     solution converges (three
C                                     iterations on all supported
C                                     platforms). Whether the 'CN+S'
C                                     solution is substantially more
C                                     accurate than the 'LT' solution
C                                     depends on the geometry of the
C                                     participating objects and on the
C                                     accuracy of the input data. In
C                                     all cases this routine will
C                                     execute more slowly when a
C                                     converged solution is computed.
C                                     See the Particulars section of
C                                     SPKEZR for a discussion of
C                                     precision of light time
C                                     corrections.
C
C                                     Both the state and rotation of
C                                     the target body are corrected for
C                                     light time.
C
C                          'CN+S'     Converged Newtonian light time
C                                     correction and stellar aberration
C                                     correction.
C
C                                     Both the state and rotation of
C                                     the target body are corrected for
C                                     light time.
C
C     OBSRVR         is the name of the observing body, typically a
C                    spacecraft, the earth, or a surface point on the
C                    earth.  OBSRVR is case-insensitive, and leading
C                    and trailing blanks in OBSRVR are not significant.
C                    Optionally, you may supply a string containing the
C                    integer ID code for the object.  For example both
C                    'EARTH' and '399' are legitimate strings that
C                    indicate the earth is the observer.
C
C                    OBSRVR may be not be identical to TARGET.
C
C     SPOINT         is a surface point on the target body, expressed
C                    in rectangular body-fixed (body equator and prime
C                    meridian) coordinates.  SPOINT need not be visible
C                    from the observer's location at time ET.
C
C$ Detailed_Output
C
C
C     PHASE          is the phase angle at SPOINT, as seen from OBSRVR
C                    at time ET.  This is the angle between the
C                    SPOINT-OBSRVR vector and the SPOINT-Sun vector.
C                    Units are radians.  The range of  PHASE is [0, pi].
C                    See Particulars below for a detailed discussion of
C                    the definition.
C
C     SOLAR          is the solar incidence angle at SPOINT, as seen
C                    from OBSRVR at time ET.  This is the angle
C                    between the surface normal vector at SPOINT and the
C                    SPOINT-Sun vector.  Units are radians.  The range
C                    of SOLAR is [0, pi]. See Particulars below for a
C                    detailed discussion of the definition.
C
C     EMISSN         is the emission angle at SPOINT, as seen from
C                    OBSRVR at time ET.  This is the angle between the
C                    surface normal vector at SPOINT and the
C                    SPOINT-observer vector.  Units are radians.  The
C                    range of EMISSN is [0, pi]. See Particulars below
C                    for a detailed discussion of the definition.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If TARGET and OBSRVR are not distinct, the error
C         SPICE(BODIESNOTDISTINCT) will be signaled.
C
C     2)  If no SPK (ephemeris) data are available for the observer,
C         target, and Sun at the time specified by ET, the error will
C         be diagnosed by routines called by this routine.  If light
C         time corrections are used, SPK data for the target body must
C         be available at the time ET - LT, where LT is the one-way
C         light time from the target to the observer at ET.
C         Additionally, SPK data must be available for the Sun at the
C         time ET - LT - LT2, where LT2 is the light time from the Sun
C         to the target body at time ET - LT.
C
C     3)  If PCK data defining the orientation or shape of the target
C         body are unavailable, the error will be diagnosed by routines
C         called by this routine.
C
C     4)  If no body-fixed frame is associated with the target body,
C         the error SPICE(NOFRAME) is signaled.
C
C     5) If name of target or observer cannot be translated to its
C        NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled.
C
C$ Files
C
C     No files are input to this routine.  However, ILLUM expects
C     that the appropriate SPK and PCK files have been loaded via
C     FURNSH.
C
C$ Particulars
C
C
C     The term "illumination angles" refers to following set of
C     angles:
C
C
C        solar incidence angle    Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 Sun.
C
C        emission angle           Angle between the surface normal at
C                                 the specified surface point and the
C                                 vector from the surface point to the
C                                 observer.
C
C        phase angle              Angle between the vectors from the
C                                 surface point to the observing body's
C                                 location and from the surface point
C                                 to the Sun.
C
C 
C     The diagram below illustrates the geometrical relationships
C     defining these angles.  The labels for the solar incidence,
C     emission, and phase angles are "s.i.", "e.", and "phase".
C
C
C                                                      *
C                                                     Sun
C
C                    surface normal vector
C                              ._                 _.
C                              |\                 /|  Sun vector
C                                \    phase      /
C                                 \   .    .    /
C                                 .            .
C                                   \   ___   /
C                              .     \/     \/
C                                    _\ s.i./
C                             .    /   \   /
C                             .   |  e. \ /
C         *             <--------------- *  surface point on
C      viewing            vector            target body
C      location           to viewing
C      (observer)         location
C
C
C     Note that if the target-observer vector, the target normal vector
C     at the surface point, and the target-sun vector are coplanar,
C     then phase is the sum of incidence and emission.  This is rarely
C     true; usually
C
C        phase angle  <  solar incidence angle + emission angle
C
C     All of the above angles can be computed using light time
C     corrections, light time and stellar aberration corrections, or
C     no aberration corrections.  The way aberration corrections
C     are used is described below.
C
C     Care must be used in computing light time corrections.  The
C     guiding principle used here is "describe what appears in
C     an image."  We ignore differential light time; the light times
C     from all points on the target to the observer are presumed to be
C     equal.
C
C
C        Observer-target body vector
C        ---------------------------
C
C        Let ET be the epoch at which an observation or remote
C        sensing measurement is made, and let ET - LT ("LT" stands
C        for "light time") be the epoch at which the photons received
C        at ET were emitted from the body (we use the term "emitted"
C        loosely here).
C
C        The correct observer-target vector points from the observer's
C        location at ET to the target body's location at ET - LT.
C        The target-observer vector points in the opposite direction.
C
C        Since light time corrections are not symmetric, the correct
C        target-observer vector CANNOT be found by computing the light
C        time corrected position of the observer as seen from the
C        target body.
C
C
C        Target body's orientation
C        -------------------------
C
C        Using the definitions of ET and LT above, the target
C        body's orientation at ET - LT is used.  The surface
C        normal is dependent on the target body's orientation, so
C        the body's orientation model must be evaluated for the correct
C        epoch.
C
C
C        Target body -- Sun vector
C        -------------------------
C
C        All surface features on the target body will appear in
C        a measurement made at ET as they were at ET-LT.  In
C        particular, lighting on the target body is dependent on
C        the apparent location of the Sun as seen from the target
C        body at ET-LT.  So, a second light time correction is used
C        in finding the apparent location of the Sun.
C
C
C     Stellar aberration corrections, when used, are applied as follows:
C
C
C        Observer-target body vector
C        ---------------------------
C
C        In addition to light time correction, stellar aberration is
C        used in computing the apparent target body position as seen
C        from the observer's location at time ET.  This apparent
C        position defines the observer-target body vector.
C
C
C        Target body-Sun vector
C        ----------------------
C
C        The target body-Sun vector is the apparent position of the Sun,
C        corrected for light time and stellar aberration, as seen from
C        the target body at time ET-LT.  Note that the target body's
C        position is not affected by the stellar aberration correction
C        applied in finding its apparent position as seen by the
C        observer.
C
C
C     Once all of the vectors, as well as the target body's
C     orientation, have been computed with the proper aberration
C     corrections, the element of time is eliminated from the
C     computation.  The problem becomes a purely geometrical one,
C     and is described by the diagram above.
C
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
C     Find the phase, solar incidence, and emission angles at the
C     sub-solar and sub-spacecraft points on Mars as seen from the
C     Mars Global Surveyor spacecraft at a specified UTC time.
C     Use light time and stellar aberration corrections.
C
C           PROGRAM ANGLES
C           IMPLICIT NONE
C     C
C     C     SPICELIB functions
C     C
C           DOUBLE PRECISION      DPR
C
C     C
C     C     Local parameters
C     C
C           INTEGER               NAMLEN
C           PARAMETER           ( NAMLEN = 32 )
C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 25 )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(NAMLEN)    OBSRVR
C           CHARACTER*(NAMLEN)    TARGET
C           CHARACTER*(TIMLEN)    UTC
C
C           DOUBLE PRECISION      ALT
C           DOUBLE PRECISION      ET
C           DOUBLE PRECISION      SSCEMI
C           DOUBLE PRECISION      SSCPHS
C           DOUBLE PRECISION      SSCSOL
C           DOUBLE PRECISION      SSLEMI
C           DOUBLE PRECISION      SSLPHS
C           DOUBLE PRECISION      SSLSOL
C           DOUBLE PRECISION      SSOLPT ( 3 )
C           DOUBLE PRECISION      SSCPT  ( 3 )
C
C     C
C     C     Load kernel files.
C     C
C           CALL FURNSH ( 'naif0007.tls'                   )
C           CALL FURNSH ( 'pck00007.tpc'                   )
C           CALL FURNSH ( 'spk_m_031103-040201_030502.bsp' )
C
C
C     C
C     C     Convert our UTC time to ephemeris seconds past J2000.
C     C
C           UTC = '2004 JAN 1 12:00:00'
C
C           CALL UTC2ET ( UTC, ET )
C
C     C
C     C     Assign observer and target names.  The acronym MGS 
C     C     indicates Mars Global Surveyor.  See NAIF_IDS for a
C     C     list of names recognized by SPICE.
C     C
C           TARGET = 'Mars'
C           OBSRVR = 'MGS'
C
C     C
C     C     Find the sub-solar point on the Earth as seen from
C     C     the MGS spacecraft at ET.  Use the "surface intercept" 
C     C     style of sub-point definition. This makes it easy
C     C     to verify the solar incidence angle.
C     C
C           CALL SUBSOL ( 'Near point', TARGET,  ET,
C          .              'LT+S',       OBSRVR,  SSOLPT  )
C
C     C
C     C     Now find the sub-spacecraft point.  Use the 
C     C     "nearest point" definition of the sub-point 
C     C     here---this makes it easy to verify the emission angle.
C     C
C           CALL SUBPT ( 'Near point',  TARGET,  ET,
C          .             'LT+S',        OBSRVR,  SSCPT,  ALT )
C
C     C
C     C     Find the phase, solar incidence, and emission
C     C     angles at the sub-solar point on the Earth as seen
C     C     from Mars Observer at time ET.
C     C
C           CALL ILLUM ( TARGET, ET,     'LT+S', OBSRVR, 
C          .             SSOLPT, SSLPHS, SSLSOL, SSLEMI )
C
C     C
C     C     Do the same for the sub-spacecraft point.
C     C
C           CALL ILLUM ( TARGET, ET,     'LT+S', OBSRVR, 
C          .             SSCPT,  SSCPHS, SSCSOL, SSCEMI )
C
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
C           WRITE (*,*) 'The solar incidence and phase angles should '//
C          .            'be equal.'
C
C           END
C
C
C     When this program is executed, the output will be: 
C
C
C        UTC epoch is 2004 JAN 1 12:00:00
C
C        Illumination angles at the sub-solar point:
C
C        Phase angle           (deg.):   150.210714
C        Solar incidence angle (deg.):   6.3735213E-15
C        Emission angle        (deg.):   150.210714
C
C        The solar incidence angle should be 0.
C        The emission and phase angles should be equal.
C
C        Illumination angles at the sub-s/c point:
C
C        Phase angle           (deg.):   123.398202
C        Solar incidence angle (deg.):   123.398202
C        Emission angle        (deg.):   6.36110936E-15
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
C     C.H. Acton     (JPL)
C     B.V. Semenov   (JPL)
C     N.J. Bachman   (JPL)
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
C-    SPICELIB Version 1.2.2, 18-MAY-2010 (BVS) 
C
C        Index lines now state that this routine is deprecated.
C
C-    SPICELIB Version 1.2.1, 07-FEB-2008 (NJB) 
C
C        Abstract now states that this routine is deprecated.
C
C-    SPICELIB Version 1.2.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSUB calls.  Replaced call to BODVAR with call to BODVCD.
C
C-    SPICELIB Version 1.1.0, 22-JUL-2004 (NJB) 
C
C        Updated to support representations of integers in the input
C        arguments TARGET and OBSRVR.  
C
C-    SPICELIB Version 1.0.2, 27-JUL-2003 (NJB) (CHA)
C
C        Various header corrections were made.  The example program
C        was upgraded to use real kernels, and the program's output is
C        shown.
C
C-    SPICELIB Version 1.0.1, 10-JUL-2002 (NJB)
C
C        Updated Index_Entries header section.
C
C-    SPICELIB Version 1.0.0, 21-MAR-1999 (NJB)
C
C        Adapted from the MGSSPICE version dated 10-MAR-1992.
C-&
 
C$ Index_Entries
C
C     DEPRECATED illumination angles
C     DEPRECATED lighting angles
C     DEPRECATED phase angle
C     DEPRECATED solar incidence angle
C     DEPRECATED emission angle
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSUB calls.  Replaced call to BODVAR with call to BODVCD.
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
      DOUBLE PRECISION      VSEP
      
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               SUN
      PARAMETER           ( SUN    =  10 )
 
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
      DOUBLE PRECISION      LTS
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSVEC ( 3 )
      DOUBLE PRECISION      OFFOBS ( 3 )
      DOUBLE PRECISION      OFFSUN ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      SUNVEC ( 3 )
      DOUBLE PRECISION      TEPOCH
      DOUBLE PRECISION      TSTATE ( 6 )
 
      INTEGER               FRCODE
      INTEGER               OBSCDE
      INTEGER               TRGCDE
      INTEGER               N
      
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
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ILLUM' )
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
         CALL CHKOUT ( 'ILLUM'                                    )
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
         CALL CHKOUT ( 'ILLUM'                                    )
         RETURN
      
      END IF
      
      
C
C     The observer and target must be distinct.
C
      IF ( TRGCDE .EQ. OBSCDE ) THEN
 
         CALL SETMSG ( 'Target is #; observer is #.' )
         CALL ERRCH  ( '#', TARGET                   )
         CALL ERRCH  ( '#', OBSRVR                   )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'    )
         CALL CHKOUT ( 'ILLUM'                       )
         RETURN
 
      END IF
 
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
         CALL CHKOUT ( 'ILLUM'                                      )
         RETURN
 
      END IF
 
C
C     Find the body-fixed state of the target as seen from the observer 
C     at ET.  The appropriate aberration corrections will be used in
C     evaluating this state.
C
      CALL SPKEZ ( TRGCDE, ET, FRNAME, ABCORR, OBSCDE, TSTATE, LT )
 
C
C     Determine the epoch to be used in computing the target-Sun vector.
C
      IF (  EQSTR( ABCORR, 'NONE' )  ) THEN
         TEPOCH = ET
      ELSE
         TEPOCH = ET - LT
      END IF
 
C
C     Find the body-fixed state of the Sun as seen from the target at 
C     TEPOCH. 
C
      CALL SPKEZ  ( SUN, TEPOCH, FRNAME, ABCORR, TRGCDE, SSTATE, LTS )
 
C
C     Grab the position portions of the states (the first three
C     elements of each state).  Negate the observer-target vector,
C     since the vector required for the illumination angle
C     computation is the target-observer vector.  The vectors we've
C     found point from the target body center to the observer and
C     Sun, and already take light time corrections into account.
C
      CALL VMINUS ( TSTATE, OBSVEC )
      CALL VEQU   ( SSTATE, SUNVEC )
  
C
C     Now we'll modify target-observer and target-Sun vectors to
C     take into account the offset between the target center and the
C     surface point of interest; we want the vectors to point from
C     the surface point to the observer and Sun respectively.
C
      CALL VSUB ( OBSVEC, SPOINT, OFFOBS )
      CALL VSUB ( SUNVEC, SPOINT, OFFSUN )
 
C
C     Find the surface normal at SPOINT.  We'll need the radii of the
C     target body.
C
      CALL BODVCD ( TRGCDE,  'RADII',   3,  N,    RADII )
 
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
 
C
C     Find the illumination angles.  VSEP will give us angular
C     separation in radians.
C
      PHASE   =  VSEP ( OFFSUN, OFFOBS )
      SOLAR   =  VSEP ( NORMAL, OFFSUN )
      EMISSN  =  VSEP ( NORMAL, OFFOBS )
 
      CALL CHKOUT ( 'ILLUM' )
      RETURN
      END
