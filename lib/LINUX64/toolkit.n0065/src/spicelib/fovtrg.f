C$Procedure      FOVTRG ( Is target in FOV at time? )

      SUBROUTINE FOVTRG ( INST,   TARGET, TSHAPE, TFRAME,
     .                    ABCORR, OBSRVR, ET,     VISIBL )

C$ Abstract
C
C     Determine if a specified ephemeris object is within the
C     field-of-view (FOV) of a specified instrument at a given time.
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
C     CK
C     FRAMES
C     KERNEL
C     NAIF_IDS
C     PCK
C     SPK
C     TIME  
C
C$ Keywords
C
C     EVENT
C     FOV
C     GEOMETRY
C     INSTRUMENT      
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE              'gf.inc'

      CHARACTER*(*)         INST
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         TSHAPE
      CHARACTER*(*)         TFRAME
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      LOGICAL               VISIBL

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  -------------------------------------------------
C     INST       I   Name or ID code string of the instrument.
C     TARGET     I   Name or ID code string of the target.
C     TSHAPE     I   Type of shape model used for the target.
C     TFRAME     I   Body-fixed, body-centered frame for target body.
C     ABCORR     I   Aberration correction flag.
C     OBSRVR     I   Name or ID code string of the observer.
C     ET         I   Time of the observation (seconds past J2000).
C     VISIBL     O   Visibility flag (true/false).
C
C$ Detailed_Input
C
C     INST       indicates the name of an instrument, such as a
C                spacecraft-mounted framing camera. The field of view
C                (FOV) of the instrument will be used to determine if
C                the target is visible with respect to the instrument.
C
C                The position of the instrument INST is considered to
C                coincide with that of the ephemeris object OBSRVR (see
C                description below).
C
C                The size of the instrument's FOV is constrained by the
C                following: There must be a vector A such that all of
C                the instrument's FOV boundary vectors have an angular
C                separation from A of less than (pi/2)-MARGIN radians
C                (see description below). For FOVs that are circular or
C                elliptical, the vector A is the boresight. For FOVs
C                that are rectangular or polygonal, the vector A is
C                calculated.
C
C                See the header of the SPICELIB routine GETFOV for a
C                description of the required parameters associated with
C                an instrument.
C
C                Both object names and NAIF IDs are accepted. For
C                example, both 'CASSINI_ISS_NAC' and '-82360' are
C                accepted. Case and leading or trailing blanks are not
C                significant in the string.
C
C     TARGET     is the name of the target body. This routine determines
C                if the target body appears in the instrument's field of
C                view. 
C
C                Both object names and NAIF IDs are accepted. For
C                example, both 'Moon' and '301' are accepted. Case and
C                leading or trailing blanks are not significant in the
C                string.
C
C     TSHAPE     is a string indicating the geometric model used to
C                represent the shape of the target body. The supported
C                options are:
C
C                   'ELLIPSOID'     Use a triaxial ellipsoid model,
C                                   with radius values provided via the
C                                   kernel pool. A kernel variable 
C                                   having a name of the form
C
C                                      'BODYnnn_RADII' 
C
C                                   where nnn represents the NAIF
C                                   integer code associated with the
C                                   body, must be present in the kernel
C                                   pool. This variable must be
C                                   associated with three numeric
C                                   values giving the lengths of the
C                                   ellipsoid's X, Y, and Z semi-axes.
C
C                   'POINT'         Treat the body as a single point.
C
C                Case and leading or trailing blanks are not
C                significant in the string.
C
C     TFRAME     is the name of the body-fixed, body-centered reference
C                frame associated with the target body. Examples of
C                such names are 'IAU_SATURN' (for Saturn) and 'ITRF93'
C                (for Earth).
C
C                If the target body is modeled as a point, TFRAME
C                is ignored and should be left blank. (Ex: ' ').
C
C                Case and leading or trailing blanks bracketing a
C                non-blank frame name are not significant in the string.
C
C     ABCORR     indicates the aberration corrections to be applied
C                when computing the target's position and orientation.
C         
C                For remote sensing applications, where the apparent
C                position and orientation of the target seen by the
C                observer are desired, normally either of the
C                corrections:
C              
C                   'LT+S' 
C                   'CN+S'
C     
C                should be used. These and the other supported options
C                are described below. 
C
C                Supported aberration correction options for
C                observation (the case where radiation is received by
C                observer at ET) are:
C
C                   'NONE'         No correction.
C                   'LT'           Light time only
C                   'LT+S'         Light time and stellar aberration.
C                   'CN'           Converged Newtonian (CN) light time.
C                   'CN+S'         CN light time and stellar aberration.
C
C                Supported aberration correction options for
C                transmission (the case where radiation is emitted from
C                observer at ET) are:
C
C                   'XLT'          Light time only.
C                   'XLT+S'        Light time and stellar aberration.
C                   'XCN'          Converged Newtonian (CN) light time.
C                   'XCN+S'        CN light time and stellar aberration.
C
C                Case, leading and trailing blanks are not significant
C                in the string.
C
C     OBSRVR     is the name of the body from which the target is
C                observed. The instrument INST is treated as if it were
C                co-located with the observer.
C
C                Both object names and NAIF IDs are accepted. For
C                example, both 'CASSINI' and '-82' are accepted. Case
C                and leading or trailing blanks are not significant in
C                the string.
C
C     ET         is the observation time in seconds past the J2000
C                epoch.
C   
C$ Detailed_Output
C
C     VISIBL     is .TRUE. if TARGET is fully or partially in the 
C                field-of-view of INST at the time ET. Otherwise,
C                VISIBL is .FALSE.
C
C$ Parameters
C
C     MAXVRT     is the maximum number of vertices that may be used
C                to define the boundary of the specified instrument's
C                field of view. See the INCLUDE file gf.inc for details.
C
C     MARGIN     is a small positive number used to constrain the
C                orientation of the boundary vectors of polygonal
C                FOVs. Such FOVs must satisfy the following constraints:
C
C                   1)  The boundary vectors must be contained within
C                       a right circular cone of angular radius less
C                       than (pi/2) - MARGIN radians; in other
C                       words, there must be a vector A such that all
C                       boundary vectors have angular separation from
C                       A of less than (pi/2)-MARGIN radians.
C
C                   2)  There must be a pair of boundary vectors U, V 
C                       such that all other boundary vectors lie in
C                       the same half space bounded by the plane
C                       containing U and V. Furthermore, all other
C                       boundary vectors must have orthogonal
C                       projections onto a specific plane normal to
C                       this plane (the normal plane contains the angle
C                       bisector defined by U and V) such that the
C                       projections have angular separation of at least
C                       2*MARGIN radians from the plane spanned by U
C                       and V.
C
C                MARGIN is currently set to 1.D-6.
C
C$ Exceptions
C
C     1)  If the name of either the target or observer cannot be
C         translated to a NAIF ID code, the error will be diagnosed by
C         a routine in the call tree of this routine.
C         
C     2)  If the specified aberration correction is an unrecognized
C         value, the error will be diagnosed and signaled by a routine
C         in the call tree of this routine.
C
C     3)  If the radii of a target body modeled as an ellipsoid cannot
C         be determined by searching the kernel pool for a kernel
C         variable having a name of the form
C
C            'BODYnnn_RADII' 
C
C         where nnn represents the NAIF integer code associated with
C         the body, the error will be diagnosed by a routine in the
C         call tree of this routine.
C
C     4)  If the target and observer bodies are the same, the error will
C         be diagnosed by a routine in the call tree of this routine.
C
C     5)  If the body model specifier TSHAPE is invalid, the error will
C         be diagnosed either here or by a routine in the call tree of
C         this routine.
C
C     6)  If a target body-fixed reference frame associated with a 
C         non-point target is not recognized, the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     7)  If a target body-fixed reference frame is not centered at
C         the corresponding target body, the error will be
C         diagnosed by a routine in the call tree of this routine.
C
C     8)  If the instrument name INST does not have a corresponding NAIF
C         ID code, the error will be diagnosed by a routine in the call
C         tree of this routine.
C
C     9)  If the FOV parameters of the instrument are not present in
C         the kernel pool, the error will be diagnosed by routines
C         in the call tree of this routine.
C
C     10) If the FOV boundary has more than MAXVRT vertices, the error
C         will be diagnosed by routines in the call tree of this
C         routine.
C
C     11) If the instrument FOV shape is a polygon or rectangle, and 
C         this routine cannot find a ray R emanating from the FOV
C         vertex such that maximum angular separation of R and any FOV
C         boundary vector is within the limit (pi/2)-MARGIN radians,
C         the error will be diagnosed by a routine in the call tree of
C         this routine. If the FOV is any other shape, the same error
C         check will be applied with the instrument boresight vector
C         serving the role of R.
C
C     12) If the loaded kernels provide insufficient data to compute a
C         requested state vector, the error will be diagnosed by a
C         routine in the call tree of this routine.
C
C     13) If an error occurs while reading an SPK or other kernel file,
C         the error will be diagnosed by a routine in the call tree 
C         of this routine.
C
C$ Files
C
C     Appropriate SPICE kernels must be loaded by the calling program
C     before this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target and observer that 
C          describe the ephemerides of these objects at the time ET.
C          If aberration corrections are used, the states of
C          target and observer relative to the solar system barycenter
C          must be calculable from the available ephemeris data.
C
C        - Frame data: if a frame definition is required to convert
C          the observer and target states to the body-fixed frame of
C          the target, that definition must be available in the kernel
C          pool. Typically the definitions of frames not already
C          built-in to SPICE are supplied by loading a frame kernel.
C
C        - Data defining the reference frame in which the instrument's
C          FOV is defined must be available in the kernel pool.
C          Additionally the name INST must be associated with an ID
C          code.
C
C        - IK data: the kernel pool must contain data such that
C          the SPICELIB routine GETFOV may be called to obtain
C          parameters for INST.
C
C     The following data may be required:
C
C        - PCK data: bodies modeled as triaxial ellipsoids must have
C          orientation data provided by variables in the kernel pool.
C
C          Bodies modeled as triaxial ellipsoids must have radii
C          lengths provided by variables in the kernel pool.
C
C        - CK data: if the frame in which the instrument's FOV is
C          defined is fixed to a spacecraft, at least one CK file will
C          be needed to permit transformation of vectors between that
C          frame and both J2000 and the target body-fixed frame.
C
C        - SCLK data: if a CK file is needed, an associated SCLK
C          kernel is required to enable conversion between encoded SCLK
C          (used to time-tag CK data) and barycentric dynamical time
C          (TDB).
C
C     Kernel data are normally loaded via FURNSH once per program run,
C     NOT every time this routine is called.
C
C$ Particulars
C
C     To treat the target as a ray rather than as an ephemeris object,
C     use the higher-level SPICELIB routine FOVRAY. FOVRAY may be used
C     to determine if distant target objects such as stars are visible
C     in an instrument's FOV at a given time, as long as the direction
C     from the observer to the target can be modeled as a ray.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C     1) A spectacular picture was taken by Cassini's
C        narrow-angle camera on Oct. 6, 2010 that shows
C        six of Saturn's moons. Let's verify that the moons
C        in the picture are Epimetheus, Atlas, Daphnis, Pan
C        Janus, and Enceladus.
C
C        To see this picture, visit:
C        http://photojournal.jpl.nasa.gov/catalog/PIA12741
C        or go to the PDS Image Node's Image Atlas at
C        http://pds-imaging.jpl.nasa.gov/search/search.html.
C        Select Cassini as the mission, ISS as the instrument,
C        and enter 1_N1665078907.122 as the Product ID in the
C        Product tab. Note: these directions may change as the
C        PDS Imaging Node changes.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels. For project meta-kernels similar to the one shown
C        below, please see the PDS section of the NAIF FTP server.
C        For example, look at the following path for Cassini
C        meta-kernels: ftp://naif.jpl.nasa.gov//pub/naif/pds/data/
C        co-s_j_e_v-spice-6-v1.0/cosp_1000/extras/mk
C
C           KPL/MK
C
C           File name: fovtrg_ex.tm
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
C              naif0010.tls                  Leapseconds
C              cpck*.tpc                     Satellite orientation and
C                                            radii
C              pck00010.tpc                  Planet orientation and
C                                            radii
C              cas_rocks_v18.tf              FK for small satellites
C                                            around Saturn
C              cas_v40.tf                    Cassini FK
C              cas_iss_v10.ti                Cassini ISS IK
C              cas00149.tsc                  Cassini SCLK
C              *.bsp                         Ephemeris for Cassini,
C                                            planets, and satellites
C              10279_10284ra.bc              Orientation for Cassini
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'naif0010.tls'
C                                  'cpck14Oct2010.tpc'
C                                  'cpck_rock_21Jan2011_merged.tpc'
C                                  'pck00010.tpc'
C                                  'cas_rocks_v18.tf'
C                                  'cas_v40.tf'
C                                  'cas_iss_v10.ti'
C                                  'cas00149.tsc'
C                                  '110317AP_RE_90165_18018.bsp'
C                                  '110120BP_IRRE_00256_25017.bsp'
C                                  '101210R_SCPSE_10256_10302.bsp'
C                                  '10279_10284ra.bc'              )
C
C           \begintext
C
C        Example code begins here.
C
C           PROGRAM FOVTRG_EX
C           IMPLICIT NONE
C
C     C
C     C     Local parameters
C     C
C           CHARACTER*(*)         META
C           PARAMETER           ( META   =  'fovtrg_ex.tm' )
C
C           CHARACTER*(*)         TIMFMT
C           PARAMETER           ( TIMFMT =
C          .      'YYYY-MON-DD HR:MN:SC.#####::TDB (TDB)' )
C
C     C
C     C     This is the spacecraft clock time of the image.
C     C
C           CHARACTER*(*)         SCLK
C           PARAMETER           ( SCLK = '1665078907.122' )
C
C     C
C     C     Local variables
C     C
C           CHARACTER*(32)        BODY
C           CHARACTER*(32)        FRNAME
C           CHARACTER*(32)        TIME
C           DOUBLE PRECISION      ET
C           INTEGER               BODYID
C           INTEGER               CAS_ID
C           INTEGER               FRCODE
C           LOGICAL               FOUND
C           LOGICAL               VISIBL
C
C     C
C     C     Load the kernels.
C     C
C           CALL FURNSH ( META )
C
C     C
C     C     Retrieve Cassini's NAIF ID.
C     C
C           CALL BODN2C ( 'CASSINI', CAS_ID, FOUND )
C
C           IF (.NOT. FOUND) THEN
C              CALL SETMSG ( 'Could not find ID code for Cassini.' )
C              CALL SIGERR ( 'SPICE(NOTRANSLATION)' )
C           ENDIF
C
C     C
C     C     Convert the image tag SCLK to ET.
C     C
C           CALL SCS2E ( CAS_ID, SCLK, ET )
C
C     C
C     C     Convert the ET to a string format for the output.
C     C
C           CALL TIMOUT ( ET, TIMFMT, TIME )
C
C     C
C     C     Search through all of Saturn's moons to see if each
C     C     satellite was in the ISS NAC's field-of-view at
C     C     the image time. We're going to take advantage of the
C     C     fact that all Saturn's moons have a NAIF ID of 6xx.
C     C
C           WRITE (*,*) 'At time ', TIME, ' the following were '
C           WRITE (*,*) 'in the field of view of CASSINI_ISS_NAC'
C
C           DO BODYID = 600, 699
C     C
C     C        Check to see if the BODYID has a translation.
C     C
C              CALL BODC2N ( BODYID, BODY, FOUND )
C              IF ( FOUND ) THEN
C     C
C     C           Check to see if a body-fixed frame for this ID exists.
C     C           If the frame is not in the kernel pool, we cannot
C     C           perform the visibility test. The main cause of a
C     C           failure is a missing kernel.
C     C
C                 CALL CIDFRM ( BODYID, FRCODE, FRNAME, FOUND )
C                 IF ( FOUND ) THEN
C     C
C     C              Is this body in the field-of-view of Cassini's
C     C              ISS narrow-angle camera?
C     C
C                    CALL FOVTRG ( 'CASSINI_ISS_NAC',
C          .                        BODY,  'ellipsoid', FRNAME,
C          .                       'CN+S', 'CASSINI', ET, VISIBL )
C
C     C
C     C              Report results.
C     C
C                    IF ( VISIBL ) THEN
C                       WRITE (*,*) '  ', BODY
C
C                    END IF
C                 END IF
C              END IF
C           END DO
C
C           END
C
C        When this program was executed using gfortran on a PC Linux
C        64 bit environment, the output was:
C
C           At time 2010-OCT-06 17:09:45.34695 (TDB) the following were
C           in the field of view of CASSINI_ISS_NAC
C             ENCELADUS
C             JANUS
C             EPIMETHEUS
C             ATLAS
C             PAN
C             DAPHNIS
C             ANTHE
C
C        Note: there were actually 7 of Saturn's satellites in the
C        field-of-view of Cassini's narrow-angle camera. However, Anthe
C        is very small and was probably obscured by other objects or
C        shadow.
C
C$ Restrictions
C
C     The reference frame associated with INST must be centered at the
C     observer or must be inertial. No check is done to ensure this.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     S.C. Krening  (JPL)
C     N.J. Bachman  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0  15-FEB-2012 (SCK) (NJB)
C
C-&


C$ Index_Entries
C
C     Target in instrument FOV at specified time
C     Target in instrument field_of_view at specified time
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
      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local variables
C
C     Ray direction vector required by ZZGFFVIN. This is 
C     an unused variable as far is this routine is concerned:
C     the target is an ephemeris object. We initialize the
C     ray to prevent portability problems.
C
      DOUBLE PRECISION      RAYDIR ( 3 )

C
C     Saved variables
C
      SAVE                  RAYDIR

C
C     Initial values
C
      DATA                  RAYDIR / 3 * 0.D0 /

C  
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'FOVTRG' ) 

C
C     Reject the target shape 'RAY'.
C     
      IF (  EQSTR( TSHAPE, RYSHAP )  ) THEN

         CALL SETMSG ( 'The target shape RAY is not supported '
     .   //            'by this routine. Use the routine '
     .   //            'FOVRAY instead.'                       )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)'                  )
         CALL CHKOUT ( 'FOVTRG'                                )
         RETURN

      END IF

C
C     Note to maintenance programmer: input exception checks
C     are delegated to ZZGFFVU. If the implementation of that
C     routine changes, or if this routine is modified to call
C     a different routine in place of ZZGFFVU, then the error 
C     handling performed by ZZGFFVU will have to be performed
C     here or in a routine called by this routine.
C
C     Initialize the visibility calculation.
C  
      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,   
     .                TARGET, TFRAME, ABCORR, OBSRVR )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'FOVTRG' )
         RETURN
      END IF
C
C     Calculate the visibility state.
C
      CALL ZZGFFVST ( ET, VISIBL )

      CALL CHKOUT ( 'FOVTRG' )
      RETURN
      END
