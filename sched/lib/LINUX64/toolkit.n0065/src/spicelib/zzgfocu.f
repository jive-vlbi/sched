C$Procedure ZZGFOCU ( GF, occultation utilities )
 
      SUBROUTINE ZZGFOCU ( OCCTYP, FRONT,  FSHAPE, FFRAME, 
     .                     BACK,   BSHAPE, BFRAME, OBSRVR, 
     .                     ABCORR, TIME,   OCSTAT          )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine contains the entry points that produce the
C     computations needed for solving for occultation states
C     in the geometry finding routines.
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
C     NAIF_IDS
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     SEARCH
C     GEOMETRY
C     OCCULTATION
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzocced.inc'
      INCLUDE 'gf.inc'
      
      CHARACTER*(*)         OCCTYP
      CHARACTER*(*)         FRONT
      CHARACTER*(*)         FSHAPE
      CHARACTER*(*)         FFRAME
      CHARACTER*(*)         BACK
      CHARACTER*(*)         BSHAPE
      CHARACTER*(*)         BFRAME
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         ABCORR
      DOUBLE PRECISION      TIME
      LOGICAL               OCSTAT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     OCCTYP     I   ZZGFOCIN
C     FRONT      I   ZZGFOCIN
C     FSHAPE     I   ZZGFOCIN
C     FFRAME     I   ZZGFOCIN
C     BACK       I   ZZGFOCIN
C     BSHAPE     I   ZZGFOCIN
C     BFRAME     I   ZZGFOCIN
C     OBSRVR     I   ZZGFOCIN
C     ABCORR     I   ZZGFOCIN
C     TIME       I   ZZGFOCST
C     OCSTAT     O   ZZGFOCST
C
C$ Detailed_Input
C
C     See entry points.
C
C$ Detailed_Output
C
C     See entry points.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See entry points.
C
C$ Files
C
C     Appropriate SPK and PCK kernels must be loaded by the calling
C     program before the entry points of this routine are called.
C
C     The following data are required:
C
C        - SPK data: the calling application must load ephemeris data
C          for the target, source and observer that cover the time
C          period specified by the window CNFINE. If aberration
C          corrections are used, the states of target and observer
C          relative to the solar system barycenter must be calculable
C          from the available ephemeris data. Typically ephemeris data
C          are made available by loading one or more SPK files via
C          FURNSH.
C
C        - PCK data: bodies modeled as triaxial ellipsoids must have
C          semi-axis lengths provided by variables in the kernel pool.
C          Typically these data are made available by loading a text
C          PCK file via FURNSH.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time the entry points of this routine are called.  
C
C$ Particulars
C
C     This routine is designed to determine whether a specified
C     type of occultation or transit is in progress at a specified
C     epoch. Two methods of modeling the shapes of the target
C     bodies are supported:
C
C        1)  Model both target bodies as triaxial ellipsoids. For this
C            case, the user may choose between occultations that are
C            partial, full or annular. See the entry header for
C            ZZGFOCIN for an explanation of these terms.
C
C        2)  Treat one target body as a point object and the other
C            target body is a triaxial ellipsoid. The only supported
C            occultation type is "ANY" for this case.
C
C     This routine contains two entry points that support searches
C     for occultations performed using ZZGFSOLV: 
C
C        ZZGFOCIN   Saves the user-supplied inputs defining the
C                   occultation computation to be performed.
C                   Initializes the occultation search.
C
C        ZZGFOCST   Returns the occultation state for a specified
C                   time.
C
C$ Examples
C
C     See GFOCCE.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB) 
C
C        Bug fix: in entry point ZZGFOCIN, corrected long error message
C        for the case in which a body-fixed reference frame is not
C        centered on the correct body.
C
C-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ISRCHC
      INTEGER               ZZOCCED

      LOGICAL               FAILED
      LOGICAL               RETURN


C
C     Local parameters
C
C
C     ALPHA is a bound for the fraction of the speed of light
C     at which target body may move, relative to the solar
C     system barycenter.
C
      DOUBLE PRECISION      ALPHA
      PARAMETER           ( ALPHA = 1.D-2 )

C
C     ATOL is a tolerance value for computing arc sine.
C
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-12 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               POSLEN
      PARAMETER           ( POSLEN = 10 )

C
C     Local variables
C
      CHARACTER*(FRNMLN)    FIXFRM
      CHARACTER*(POSLEN)    POSNAM
      CHARACTER*(SHPLEN)    SHAPE
      CHARACTER*(FRNMLN)    SVBFRM
      CHARACTER*(BDNMLN)    SVBNAM
      CHARACTER*(SHPLEN)    SVBSHP
      CHARACTER*(CORLEN)    SVCORR
      CHARACTER*(FRNMLN)    SVFFRM
      CHARACTER*(BDNMLN)    SVFNAM
      CHARACTER*(SHPLEN)    SVFSHP
      CHARACTER*(BDNMLN)    SVONAM
      CHARACTER*(OCLLN)     SVTYPE
      CHARACTER*(OCLLN)     SVTYPS ( NOCTYP )

      DOUBLE PRECISION      BCKFRT ( 3 )
      DOUBLE PRECISION      BCKOBS ( 3 )
      DOUBLE PRECISION      BCKPOS ( 3 )
      DOUBLE PRECISION      BDIST
      DOUBLE PRECISION      BSMAXS ( 3, 3 )
      DOUBLE PRECISION      ETBCOR
      DOUBLE PRECISION      ETFCOR
      DOUBLE PRECISION      FDIST
      DOUBLE PRECISION      FRTBCK ( 3 )
      DOUBLE PRECISION      FRTOBS ( 3 )
      DOUBLE PRECISION      FRTPOS ( 3 )
      DOUBLE PRECISION      FSMAXS ( 3, 3 )
      DOUBLE PRECISION      LTBACK
      DOUBLE PRECISION      LTFRNT
      DOUBLE PRECISION      MAXANG
      DOUBLE PRECISION      MINANG
      DOUBLE PRECISION      MTEMP  ( 3, 3 )
      DOUBLE PRECISION      SVORIG ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SRAD
      DOUBLE PRECISION      SVBRAD ( 3 )
      DOUBLE PRECISION      SVFRAD ( 3 )
      DOUBLE PRECISION      SVMNBR
      DOUBLE PRECISION      SVMNFR
      DOUBLE PRECISION      SVMXBR
      DOUBLE PRECISION      SVMXFR
      DOUBLE PRECISION      T2SEP
      DOUBLE PRECISION      TDIST
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGSEP 

      INTEGER               CENTER
      INTEGER               CLSSID
      INTEGER               FFRMID
      INTEGER               FRCLSS
      INTEGER               I
      INTEGER               IDBACK
      INTEGER               IDFRNT
      INTEGER               IDOBS
      INTEGER               LOC
      INTEGER               N
      INTEGER               OCCNUM
      INTEGER               OCCODE
      INTEGER               SVBACK
      INTEGER               SVFRNT
      INTEGER               SVOBS
      INTEGER               TRGID

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND
      LOGICAL               PNTOCC

C
C     Saved variables
C 
      SAVE                  SVBACK
      SAVE                  SVBFRM
      SAVE                  SVBNAM
      SAVE                  SVBRAD
      SAVE                  SVBSHP
      SAVE                  SVCORR
      SAVE                  SVFFRM 
      SAVE                  SVFNAM
      SAVE                  SVFRAD
      SAVE                  SVFRNT
      SAVE                  SVFSHP
      SAVE                  SVMNBR
      SAVE                  SVMNFR
      SAVE                  SVMXBR
      SAVE                  SVMXFR
      SAVE                  SVOBS
      SAVE                  SVONAM
      SAVE                  SVORIG
      SAVE                  SVTYPE
      SAVE                  SVTYPS
     
C
C     Initial values
C
      DATA                  SVORIG / 3 * 0.D0 /

C
C     Below we initialize the list of occultation types.
C
      DATA                  SVTYPS / ANNULR, ANY, PARTL, FULL /
      
C
C     This routine should never be called directly.
C
      
      CALL CHKIN  ( 'ZZGFOCU' )

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'ZZGFOCU' )
      RETURN



 
C$Procedure  ZZGFOCIN ( GF, occultation initialization )
 
      ENTRY ZZGFOCIN  ( OCCTYP, FRONT,  FSHAPE, FFRAME, 
     .                  BACK,   BSHAPE, BFRAME, OBSRVR, 
     .                  ABCORR                         )
 
C$ Abstract
C
C    Perform initialization functions for occultation state 
C    determination.
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
C     NAIF_IDS
C     FRAMES
C     PCK
C     SPK
C     TIME
C
C$ Keywords
C
C     SEARCH
C     GEOMETRY
C     OCCULTATION
C
C$ Declarations
C
C     CHARACTER*(*)         OCCTYP
C     CHARACTER*(*)         FRONT
C     CHARACTER*(*)         FSHAPE
C     CHARACTER*(*)         FFRAME
C     CHARACTER*(*)         BACK
C     CHARACTER*(*)         BSHAPE
C     CHARACTER*(*)         BFRAME
C     CHARACTER*(*)         OBSRVR
C     CHARACTER*(*)         ABCORR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OCCTYP     I   Type of occultation.
C     FRONT      I   Name of body occulting the other.
C     FSHAPE     I   Type of shape model used for front body.
C     FFRAME     I   Body-fixed, body-centered frame for front body.
C     BACK       I   Name of body occulted by the other.
C     BSHAPE     I   Type of shape model used for back body.
C     BFRAME     I   Body-fixed, body-centered frame for back body.
C     OBSRVR     I   Name of the observing body.
C     ABCORR     I   Aberration correction flag.
C
C$ Detailed_Input
C
C
C     OCCTYP     indicates the type of occultation that is to be found.
C                The full set of possible values of OCCTYP may be used
C                when both target bodies are modeled as ellipsoids.
C                When either target is modeled as a point, OCCTYP must
C                be set to 'ANY' (see description below).
C
C                Supported values of OCCTYP and corresponding
C                definitions are:
C
C                   'FULL'               denotes the full occultation
C                                        of the body designated by 
C                                        BACK by the body designated
C                                        by FRONT, as seen from
C                                        the location of the observer.
C                                        In other words, the occulted
C                                        body is completely invisible
C                                        as seen from the observer's
C                                        location.
C
C                   'ANNULAR'            denotes an annular
C                                        occultation: the body
C                                        designated by FRONT blocks
C                                        part of, but not the limb of,
C                                        the body designated by BACK,
C                                        as seen from the location of
C                                        the observer.
C
C                   'PARTIAL'            denotes an partial,
C                                        non-annular occultation: the
C                                        body designated by FRONT
C                                        blocks part, but not all, of
C                                        the limb of the body
C                                        designated by BACK, as seen
C                                        from the location of the
C                                        observer.
C
C                   'ANY'                denotes any of the above three
C                                        types of occultations:
C                                        'PARTIAL', 'ANNULAR', or
C                                        'FULL'.
C
C                                        'ANY' should be used to search
C                                        for times when the body 
C                                        designated by FRONT blocks
C                                        any part of the body designated
C                                        by BACK.
C
C                                        The option 'ANY' MUST be used
C                                        if either the front or back
C                                        target body is modeled as
C                                        a point.
C
C                Case and leading or trailing blanks are not
C                significant in the string OCCTYP.
C
C
C     FRONT      is the name of the target body that occults---that is,
C                passes in front of---the other. Optionally, you may
C                supply the integer NAIF ID code for the body as a
C                string. For example both 'MOON' and '301' are
C                legitimate strings that designate the Moon.
C
C                Case and leading or trailing blanks are not
C                significant in the string FRONT.
C
C
C     FSHAPE     is a string indicating the geometric model used
C                to represent the shape of the front body. The
C                supported options are:
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
C                                   When a point target is specified,
C                                   the occultation type must be
C                                   set to 'ANY'.
C                                   
C                At least one of the target bodies FRONT and BACK must
C                be modeled as an ellipsoid.
C
C                Case and leading or trailing blanks are not
C                significant in the string FSHAPE.
C
C
C     FFRAME     is the name of the body-fixed, body-centered reference
C                frame associated with the front target body. Examples
C                of such names are 'IAU_SATURN' (for Saturn) and
C                'ITRF93' (for the Earth).
C
C                If the front target body is modeled as a point, FFRAME
C                should be left blank.
C
C                Case and leading or trailing blanks bracketing a
C                non-blank frame name are not significant in the string
C                FFRAME.
C
C
C     BACK       is the name of the target body that is occulted
C                by---that is, passes in back of---the other.
C                Optionally, you may supply the integer NAIF ID code
C                for the body as a string. For example both 'MOON' and
C                '301' are legitimate strings that designate the Moon.
C
C                Case and leading or trailing blanks are not
C                significant in the string BACK.
C
C
C     BSHAPE     is the shape specification for the body designated
C                by BACK. See the description of FSHAPE above for
C                details.
C                
C
C     BFRAME     is the name of the body-fixed, body-centered reference
C                frame associated with the ``back'' target body.
C                Examples of such names are 'IAU_SATURN' (for Saturn)
C                and 'ITRF93' (for the Earth).
C
C                If the back target body is modeled as a point, BFRAME
C                should be left blank.
C
C                Case and leading or trailing blanks bracketing a
C                non-blank frame name are not significant in the string
C                BFRAME.
C                
C
C     OBSRVR     is the name of the body from which the occultation is
C                observed. Optionally, you may supply the integer NAIF
C                ID code for the body as a string.
C
C                Case and leading or trailing blanks are not
C                significant in the string OBSRVR.
C
C
C     ABCORR     indicates the aberration corrections to be applied to
C                the state of the target body to account for one-way
C                light time.  Stellar aberration corrections are
C                ignored if specified, since these corrections don't
C                improve the accuracy of the occultation determination.
C
C                See the header of the SPICE routine SPKEZR for a
C                detailed description of the aberration correction
C                options. For convenience, the options supported by
C                this routine are listed below:
C
C                   'NONE'     Apply no correction.   
C
C                   'LT'       "Reception" case:  correct for
C                              one-way light time using a Newtonian
C                              formulation.
C
C                   'CN'       "Reception" case:  converged
C                              Newtonian light time correction.
C
C                   'XLT'      "Transmission" case:  correct for
C                              one-way light time using a Newtonian
C                              formulation.
C
C                   'XCN'      "Transmission" case:  converged
C                              Newtonian light time correction.
C
C                Case and blanks are not significant in the string
C                ABCORR.
C
C$ Detailed_Output
C
C     None.  
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If name of either target or the observer cannot be translated
C         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is
C         signaled.
C
C     2)  If either of the target bodies FRONT or BACK coincides with
C         the observer body OBSRVR, or if the targets coincide,
C         the error SPICE(BODIESNOTDISTINCT) will be signaled.
C
C     3)  If either of the body model specifiers FSHAPE or BSHAPE
C         is not recognized, the error SPICE(INVALIDSHAPE) will be
C         signaled.
C
C     4)  If both of the body model specifiers FSHAPE and BSHAPE
C         specify point targets, the error SPICE(INVALIDSHAPECOMBO)
C         will be signaled.
C
C     5)  If an unrecognized value of OCCTYP is seen, the error
C         SPICE(INVALIDOCCTYPE) is signaled.
C
C     6)  If one target body is modeled as a point and OCCTYP is not
C         set to 'ANY', the error SPICE(BADTYPESHAPECOMBO) is signaled.
C
C     7)  If a target indicated to be an ellipsoid by its shape
C         specification argument does not have three associated
C         positive radii, the error SPICE(DEGENERATECASE) will be
C         signaled.
C
C     8)  If the number of radii associated with a target body is
C         not three, the error SPICE(BADRADIUSCOUNT) will be 
C         signaled.
C
C     9)  If a target body-fixed reference frame associated with a 
C         non-point target is not recognized, the error 
C         SPICE(INVALIDFRAME) will be signaled.
C
C     10) If a target body-fixed reference frame is not centered at
C         the corresponding target body, the error 
C         SPICE(INVALIDFRAME) will be signaled.
C
C     11) If the aberration correction string is invalid, the error
C         will be diagnosed by a routine in the call tree of this
C         routine.                 
C         
C$ Files
C
C     See the header of the umbrella routine ZZGFOCU.
C
C$ Particulars
C
C     This entry point initializes the parameters needed by the
C     occultation state determination entry point ZZGFOCST.
C
C$ Examples
C
C     See implementation of GFOCCE.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C 
C$ Version
C
C-    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB) 
C
C        Bug fix: corrected long error message for the case in which a
C        body-fixed reference frame is not centered on the correct
C        body.
C
C-    SPICELIB Version 1.0.0, 15-APR-2009 (LSE) (WLT) (NJB) (EDW)
C
C-&
 

C$ Revisions
C 
C     None.
C
C-& 


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFOCIN' ) 

C
C     Find NAIF IDs for FRONT, BACK, and OBSRVR.
C
      CALL BODS2C ( FRONT, IDFRNT, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The front target object, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', FRONT                                 )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFOCIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( BACK, IDBACK, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The back target object, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', BACK                                  )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFOCIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, IDOBS, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The observer, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFOCIN'                                 )
         RETURN

      END IF

C
C     Make sure the observer and both targets are distinct.
C
      IF (      ( IDFRNT .EQ. IDBACK )
     .     .OR. ( IDFRNT .EQ. IDOBS  ) 
     .     .OR. ( IDBACK .EQ. IDOBS  ) ) THEN

         CALL SETMSG ( 'The observer and both targets must be '
     .   //            'distinct objects, but are not: '
     .   //            'OBSRVR = #; FRONT = #; BACK = #.'      )
         CALL ERRCH  ( '#', OBSRVR                             )
         CALL ERRCH  ( '#', FRONT                              )
         CALL ERRCH  ( '#', BACK                               )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'              )
         CALL CHKOUT ( 'ZZGFOCIN'                              )
         RETURN

      END IF

C
C     Save the objects' names. We'll need these if
C     we need to call SINCPT.
C
      SVFNAM = FRONT
      SVBNAM = BACK
      SVONAM = OBSRVR

C
C     Store the ID codes, shape specifications, and body-fixed,
C     body-centered frame names of the objects involved in this event.
C
      SVFRNT = IDFRNT
      SVFFRM = FFRAME

      CALL LJUST ( FSHAPE, SVFSHP )
      CALL UCASE ( SVFSHP, SVFSHP )

      SVBACK = IDBACK
      SVBFRM = BFRAME

      CALL LJUST ( BSHAPE, SVBSHP )
      CALL UCASE ( SVBSHP, SVBSHP )

      SVOBS  = IDOBS

C
C     Note for maintenance programmer: these checks will
C     require modification to handle DSK-based shapes.
C
      IF (      ( SVFSHP .NE. PTSHAP ) 
     .    .AND. ( SVFSHP .NE. EDSHAP )  ) THEN 

         CALL SETMSG ( 'The front target shape specification, '
     .   //            '''#'', is not a recognized.'           )
         CALL ERRCH  ( '#', FSHAPE                             )
         CALL SIGERR ( 'SPICE(INVALIDSHAPE)'                   )
         CALL CHKOUT ( 'ZZGFOCIN'                              )
         RETURN

      END IF

      IF (      ( SVBSHP .NE. PTSHAP ) 
     .    .AND. ( SVBSHP .NE. EDSHAP )  ) THEN 

         CALL SETMSG ( 'The back target shape specification, '
     .   //            '''#'', is not a recognized.'           )
         CALL ERRCH  ( '#', BSHAPE                             )
         CALL SIGERR ( 'SPICE(INVALIDSHAPE)'                   )
         CALL CHKOUT ( 'ZZGFOCIN'                              )
         RETURN

      END IF

      IF (      ( SVFSHP .EQ. PTSHAP ) 
     .    .AND. ( SVBSHP .EQ. PTSHAP )  ) THEN 

         CALL SETMSG ( 'The front and back target shape '
     .   //            'specifications are both PTSHAP; '
     .   //            'at least one of these targets '
     .   //            'must be an extended object.'     )
         CALL SIGERR ( 'SPICE(INVALIDSHAPECOMBO)'        )
         CALL CHKOUT ( 'ZZGFOCIN'                        )
         RETURN

      END IF

C
C     Save a single upper-case character representing the occultation
C     type string.
C
      CALL LJUST ( OCCTYP, SVTYPE )
      CALL UCASE ( SVTYPE, SVTYPE )

C
C     Check the occultation type.
C
      OCCNUM = ISRCHC ( SVTYPE, NOCTYP, SVTYPS )

      IF ( OCCNUM .EQ. 0 ) THEN

         CALL SETMSG ( 'The occultation type # is not '     
     .   //            'recognized.  Supported types are: '
     .   //            '#, #, #,  #.'                      )
         CALL ERRCH  ('#', OCCTYP                          )

         DO I = 1, NOCTYP
            CALL ERRCH  ('#', SVTYPS(I)                    )
         END DO

         CALL SIGERR ('SPICE(INVALIDOCCTYPE)'              )
         CALL CHKOUT ('ZZGFOCIN'                           )
         RETURN  

      END IF 

C
C     If we have a point target, the occultation type must
C     be 'ANY'.
C
      IF (  ( SVFSHP .EQ. PTSHAP ) .OR. ( SVBSHP .EQ. PTSHAP )  ) THEN

         IF ( SVTYPE .NE. ANY ) THEN

            CALL SETMSG ( 'Occultation type # is not allowed when '
     .      //            'either target body is modeled as a '
     .      //            'point. Set OCCTYP to ANY for use with '
     .      //            'point targets.'                         )
            CALL ERRCH  ( '#',  OCCTYP                             )
            CALL SIGERR ( 'SPICE(BADTYPESHAPECOMBO)'               )
            CALL CHKOUT ( 'ZZGFOCIN'                               )
            RETURN

         END IF

      END IF

C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C     
      CALL ZZVALCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFOCIN' )
         RETURN
      END IF

C
C     Create a local aberration correction string without
C     a stellar aberration correction specifier.
C
      IF ( ATTBLK(GEOIDX) ) THEN

         SVCORR = 'NONE'

      ELSE
C
C        The correction string specified either Newtonian or converged
C        light time correction.
C        
         IF ( ATTBLK(XMTIDX) ) THEN
            SVCORR = 'X'
         ELSE
            SVCORR = ' '
         END IF

         IF ( ATTBLK(CNVIDX) ) THEN
            CALL SUFFIX ( 'CN', 0, SVCORR )
         ELSE
            CALL SUFFIX ( 'LT', 0, SVCORR )
         END IF

      END IF

C
C     Check the front and back targets' shapes, frames
C     and radii.
C
      DO I = 1, 2

         IF ( I .EQ. 1 ) THEN

            POSNAM = 'front'
            FIXFRM = FFRAME
            TRGID  = IDFRNT
            SHAPE  = SVFSHP

         ELSE

            POSNAM = 'back'
            FIXFRM = BFRAME
            TRGID  = IDBACK
            SHAPE  = SVBSHP

         END IF

         IF ( SHAPE .EQ. EDSHAP ) THEN
C
C           Fetch and check the radii.
C
            CALL BODVCD ( TRGID, 'RADII', 3, N, RADII )

C
C           Check the count of the radii.
C
            IF ( N .NE. 3 )  THEN
 
               CALL SETMSG ( 'Target # should have 3 radii but '
     .         //            'actually has #. This may be due '
     .         //            'to an error in a PCK file used '
     .         //            'to provide the radii.'             )
               CALL ERRCH  ( '#', POSNAM                         )
               CALL ERRINT ( '#', N                              )
               CALL SIGERR ( 'SPICE(BADRADIUSCOUNT)'             )
               CALL CHKOUT ( 'ZZGFOCIN'                          )
               RETURN

            END IF

C
C           Check to make sure the current target has 3 positive
C           semi-axis lengths.
C
            IF (     ( RADII(1) .LE. 0.D0 ) 
     .          .OR. ( RADII(2) .LE. 0.D0 )
     .          .OR. ( RADII(3) .LE. 0.D0 ) ) THEN

               CALL SETMSG ( 'One or more semi-axis lengths of '
     .         //            'the # target body are non-'
     .         //            'positive: 1 = #, 2 = #, 3 = #. '  )
               CALL ERRCH  ( '#', POSNAM                        )
               CALL ERRDP  ( '#', RADII (1)                     )
               CALL ERRDP  ( '#', RADII (2)                     )
               CALL ERRDP  ( '#', RADII (3)                     )
               CALL SIGERR ( 'SPICE(DEGENERATECASE)'             )
               CALL CHKOUT ( 'ZZGFOCIN'                          )
               RETURN

            END IF

C
C           Checks of radii have been completed.
C
            IF ( I .EQ. 1 ) THEN

               CALL MOVED ( RADII,  3,  SVFRAD )
C
C              Select smallest and largest semi-axis lengths of body
C              for later tests.
C
               CALL MINAD ( SVFRAD, 3,  SVMNFR, LOC )
               CALL MAXAD ( SVFRAD, 3,  SVMXFR, LOC )

            ELSE

               CALL MOVED ( RADII,  3,  SVBRAD )

               CALL MINAD ( SVBRAD, 3,  SVMNBR, LOC )
               CALL MAXAD ( SVBRAD, 3,  SVMXBR, LOC )

            END IF

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFOCIN' )
               RETURN
            END IF

C
C           The target is ellipsoidal; there must be 
C           a target body-fixed frame associated with this
C           body.
C
            IF ( FIXFRM .EQ. ' ' ) THEN

               CALL SETMSG ( 'The # target is modeled as an '
     .         //            'ellipsoid, but the associated '
     .         //            'body-fixed frame name is blank.' )
               CALL SIGERR ( 'SPICE(INVALIDFRAME)'             )
               CALL ERRCH  ( '#',  POSNAM                      )
               CALL CHKOUT ( 'ZZGFOCIN'                        )
               RETURN

            ELSE
C
C              Look up the target's body-fixed frame ID code.
C
               CALL NAMFRM ( FIXFRM, FFRMID )

               IF ( FFRMID .EQ. 0 ) THEN

                  CALL SETMSG ( 'The # target''s body-fixed frame '
     .            //            'name # is not recognized.'        )
                  CALL ERRCH  ( '#',  POSNAM                       )
                  CALL ERRCH  ( '#',  FIXFRM                       )
                  CALL SIGERR ( 'SPICE(INVALIDFRAME)'              )
                  CALL CHKOUT ( 'ZZGFOCIN'                         )
                  RETURN

               END IF

C
C              Obtain the center of the frame and verify it's the
C              Ith target.
C
               CALL FRINFO ( FFRMID, CENTER, FRCLSS, CLSSID, FOUND )

               IF ( .NOT. FOUND ) THEN
C
C                 Since we mapped the frame name to an ID code, we
C                 expect to find the frame info. So control should
C                 never reach this point.
C
                  CALL SETMSG ( 'Frame ID found for # body-fixed '
     .            //            'frame # but FRINFO couldn''t '
     .            //            'find frame info.'                 )
                  CALL ERRCH  ( '#',  POSNAM                       )
                  CALL ERRCH  ( '#',  FIXFRM                       )
                  CALL SIGERR ( 'SPICE(BUG)'                       )
                  CALL CHKOUT ( 'ZZGFOCIN'                         )
                  RETURN

               END IF

               IF ( CENTER .NE. TRGID ) THEN
C
C                 The body-fixed frame for the current target
C                 isn't actually centered on the body.
C
                  CALL SETMSG ( 'Supposed body-fixed frame # for '
     .            //            '# target is actually centered '
     .            //            'on body #.'                       )
                  CALL ERRCH  ( '#',  FIXFRM                       )
                  CALL ERRCH  ( '#',  POSNAM                       )
                  CALL ERRINT ( '#',  CENTER                       )
                  CALL SIGERR ( 'SPICE(INVALIDFRAME)'              )
                  CALL CHKOUT ( 'ZZGFOCIN'                         )
                  RETURN

               END IF


            END IF
C
C           We've performed radii and frame checks for an ellipsoidal
C           target.
C

         ELSE IF ( SHAPE .EQ. PTSHAP ) THEN
C
C           Zero out radius values for this target; set the
C           frame to blank.
C
            IF ( I .EQ. 1 ) THEN

               CALL CLEARD ( 3, SVFRAD )

               SVMNFR = 0.D0
               SVMXFR = 0.D0               
               SVFFRM = ' '

            ELSE

               CALL CLEARD ( 3, SVBRAD )

               SVMNBR = 0.D0
               SVMXBR = 0.D0               
               SVBFRM = ' '

            END IF

         ELSE
C
C           We have an unsupported target shape.
C
            CALL SETMSG ( 'The # target body has shape #; the only '
     .      //            'supported shapes are ELLIPSOID and '
     .      //            'POINT.'                                  )
            CALL ERRCH  ( '#',  POSNAM                              )
            CALL ERRCH  ( '#',  SHAPE                               )
            CALL SIGERR ( 'SPICE(INVALIDSHAPE)'                     )
            CALL CHKOUT ( 'ZZGFOCIN'                                )
            RETURN

         END IF
C
C        We've performed shape, and if applicable, frame and radii 
C        checks for the Ith target.
C
      END DO
C
C     We've performed shape, and if applicable, frame and radii 
C     checks for both targets.
C
      CALL CHKOUT ( 'ZZGFOCIN' )
      RETURN
 
 
 
C$Procedure ZZGFOCST ( GF, "in occultation?"  )
 
      ENTRY ZZGFOCST ( TIME, OCSTAT )
 
C$ Abstract
C
C     See if the object is currently occulted.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     SEARCH
C     GEOMETRY
C     OCCULTATION
C
C$ Declarations
C
C     DOUBLE PRECISION      TIME
C     LOGICAL               OCSTAT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TIME       I   TDB epoch (in seconds past J2000)
C     OCSTAT     O   .TRUE. if the object is occulted, .FALSE. 
C                    otherwise.
C
C$ Detailed_Input
C
C     TIME       is the epoch of interest in TDB seconds past the
C                J2000 epoch.
C
C$ Detailed_Output
C
C     OCSTAT     is a logical flag indicating the state of
C                occultation. If the configuration initialized by
C                ZZGFOCIN is in occultation at the epoch TIME, OCSTAT is
C                returned with the value .TRUE. Otherwise it is
C                returned with the value .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If any SPK lookup fails, the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     2) If any frame transformation lookup fails, the error will be
C        diagnosed by routines in the call tree of this routine.
C
C     3) If any occultation computation is done for ellipsoidal 
C        targets, and if either semi-axis matrix is invalid, the error
C        will be diagnosed by routines in the call tree of this
C        routine.
C
C     4) If any two of the bodies defining the occultation geometry
C        intersect, either error SPICE(NOTDISJOINT) will be
C        signaled by this routine, or the error will be diagnosed by
C        routines in the call tree of this routine.
C
C     5)  If the body model specifiers FSHAPE and BSHAPE don't specify
C         either two ellipsoidal targets or one ellipsoidal target and
C         one point target, the error SPICE(INVALIDSHAPECOMBO)
C         will be signaled.
C
C$ Files
C
C     See the Files header section of the umbrella routine ZZGFOCU.
C
C$ Particulars
C
C     This routine determines the occultation state of the
C     configuration specified by the last call to ZZGFOCIN and the
C     input time value.
C
C$ Examples
C
C     See the umbrella routine ZZGFOCU.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 30-DEC-2008 (NJB) (LSE) (WLT) (EDW)
C
C-&
 

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFOCST' ) 

C
C     Initialize the state output.
C
      OCSTAT = .FALSE.

C
C     Get the apparent positions of FRONT and BACK as seen from the
C     observer.
C
      CALL SPKEZP ( SVFRNT,  TIME,   'J2000',  SVCORR, 
     .              SVOBS,   FRTPOS, LTFRNT           )

      CALL SPKEZP ( SVBACK,  TIME,   'J2000',  SVCORR, 
     .              SVOBS,   BCKPOS, LTBACK           )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFOCST' )
         RETURN
      END IF

C
C     Handle the cases of one and two extended targets
C     separately.
C
      IF (       ( SVBSHP .EQ. EDSHAP )
     .     .AND. ( SVFSHP .EQ. EDSHAP )  ) THEN
C
C        The caller has selected a test for a partial, annular or full
C        occultation using ellipsoidal shape models.
C
C        Look up the axes of each target body in the J2000 frame at the
C        light time corrected epoch for that body.
C
         CALL ZZCOREPC ( SVCORR,  TIME,   LTBACK, ETBCOR )

         CALL PXFORM   ( SVBFRM, 'J2000', ETBCOR, MTEMP  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF
C
C        Scale the columns of MTEMP by the axis lengths of the back
C        target.
C
         DO I = 1, 3
            CALL VSCL ( SVBRAD(I), MTEMP(1,I), BSMAXS(1,I) )
         END DO


         CALL ZZCOREPC ( SVCORR,  TIME,   LTFRNT, ETFCOR )
         CALL PXFORM   ( SVFFRM, 'J2000', ETFCOR, MTEMP  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF
C
C        Scale the columns of MTEMP by the axis lengths of the second
C        target.
C
         DO I = 1, 3
            CALL VSCL ( SVFRAD(I), MTEMP(1,I), FSMAXS(1,I) )
         END DO

C
C        Classify the occultation state of BACK by FRONT as seen from
C        the observer.
C
         OCCODE = ZZOCCED ( SVORIG, BCKPOS, BSMAXS, FRTPOS, FSMAXS )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF


         IF ( OCCODE .EQ. NOOCC ) THEN
C
C           Neither body occults the other.
C
            OCSTAT = .FALSE.


         ELSE IF (       ( SVTYPE .EQ. ANY ) 
     .             .AND. ( OCCODE .LT. 0   ) ) THEN
C
C           The "of" body (target 1) is at least partially occulted by
C           the BY object.
C
            OCSTAT = .TRUE.


         ELSE IF (       ( SVTYPE .EQ.  FULL   ) 
     .             .AND. ( OCCODE .EQ.  TOTAL1 ) ) THEN
C
C           The BACK body is in total occultation.
C

            OCSTAT = .TRUE.


         ELSE IF (       ( SVTYPE .EQ. ANNULR )
     .             .AND. ( OCCODE .EQ. ANNLR1 ) ) THEN
C
C           The  BACK body is in annular occultation.
C
            OCSTAT = .TRUE.


         ELSE IF (       ( SVTYPE .EQ. PARTL  )  
     .             .AND. ( OCCODE .EQ. PARTL1 ) ) THEN
C
C           The BACK body is partially occulted.
C
            OCSTAT = .TRUE.


         ELSE 
C
C           The occultation state doesn't match the requested state.
C
            OCSTAT = .FALSE.

         END IF

         CALL CHKOUT ( 'ZZGFOCST' )
         RETURN


      ELSE IF (     (       ( SVFSHP .EQ. EDSHAP ) 
     .                .AND. ( SVBSHP .EQ. PTSHAP ) )
     .         .OR. (       ( SVFSHP .EQ. PTSHAP ) 
     .                .AND. ( SVBSHP .EQ. EDSHAP ) )   ) THEN
C
C        One of the targets is modeled as a point; the other is
C        modeled as an ellipsoid. 
C
C        If the front target is an ellipsoid and the back target
C        is a point, we'll classify the geometry as a "point
C        occultation." Otherwise we have a "point transit" case.
C        We'll set the logical flag PNTOCC to .TRUE. to indicate
C        a point occultation.
C
         PNTOCC = SVBSHP .EQ. PTSHAP

C
C        We're going to start out by doing some error checking.
C        We're looking for intersections of the participating
C        objects: these should never occur.
C
C        Let BDIST, FDIST be the distances from the observer
C        to the back and front targets, respectively.
C
         BDIST  =  VNORM ( BCKPOS )
         FDIST  =  VNORM ( FRTPOS )

C
C        Find the vector from BACK to FRONT.  We'll use this later,
C        but we want it now in order to make sure that BACK doesn't
C        intersect FRONT.
C
         CALL VSUB ( FRTPOS, BCKPOS, BCKFRT )


         IF ( PNTOCC ) THEN
C
C           The front target is an ellipsoid.
C
            IF ( FDIST .LE. SVMNFR ) THEN
C
C              The observer is INSIDE the front target. We
C              treat this as an error.
C
               CALL SETMSG ( 'Observer is inside front target body.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                    )
               CALL CHKOUT ( 'ZZGFOCST'                              )
               RETURN 
 
            ELSE IF ( BDIST .EQ. 0.D0 ) THEN

               CALL SETMSG ( 'Back target coincides with observer.'  )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                    )
               CALL CHKOUT ( 'ZZGFOCST'                              )
               RETURN 

            ELSE IF ( VNORM(BCKFRT) .LE. SVMNFR ) THEN

               CALL SETMSG ( 'BACK target is inside FRONT target.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                  )
               CALL CHKOUT ( 'ZZGFOCST'                            )
               RETURN   

            END IF

         ELSE
C
C           The back target is an ellipsoid.
C
            IF ( BDIST .LE. SVMNBR ) THEN
C
C              The observer is INSIDE the back target. We
C              treat this as an error.
C
               CALL SETMSG ( 'Observer is inside back target body.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                   )
               CALL CHKOUT ( 'ZZGFOCST'                             )
               RETURN 
 
            ELSE IF ( FDIST .EQ. 0.D0 ) THEN

               CALL SETMSG ( 'Front target coincides with observer.'  )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                     )
               CALL CHKOUT ( 'ZZGFOCST'                               )
               RETURN 

            ELSE IF ( VNORM(BCKFRT) .LE. SVMNBR ) THEN

               CALL SETMSG ( 'FRONT target is inside BACK target.' )
               CALL SIGERR ( 'SPICE(NOTDISJOINT)'                  )
               CALL CHKOUT ( 'ZZGFOCST'                            )
               RETURN   

            END IF

         END IF

C
C        Find angular separation of the target centers as
C        seen by the observer.
C
         TRGSEP = VSEP ( BCKPOS, FRTPOS )

C
C        Find angular radius of the outer bounding sphere of the
C        ellipsoid, as seen by the observer. 
C
C        In computing this angular radius, scale up the bounding
C        sphere to compensate for the light time error we've made
C        by computing light time to the target's center. The
C        correct value to use is light time to the limb point having
C        minimum angular separation from the point target.
C        
C        Presuming the ellipsoidal target can move no faster than
C        alpha*c (where c represents the speed of light in a vacuum),
C        and considering the fact that the light time error cannot
C        exceed r/c, where r is the radius of the outer bounding sphere
C        of the ellipsoid, we find that the magnitude of the position
C        error of the ellipsoid cannot exceed alpha*r. Then the
C        correctly positioned ellipsoid---that is, located at
C        the position corresponding to the correct light time 
C        correction---must be contained in the outer bounding
C        sphere we've found, if we scale the sphere up by 1+alpha.
C
C        Perform the test only if the observer is outside the
C        outer bounding sphere of the ellipsoidal target.
C
         IF ( PNTOCC ) THEN

            SRAD  = (1+ALPHA) * SVMXFR
            TDIST = FDIST
         ELSE
            SRAD  = (1+ALPHA) * SVMXBR
            TDIST = BDIST
         END IF

         IF ( SRAD .LT. TDIST ) THEN

            MAXANG = DASINE ( SRAD / TDIST,  ATOL )

            IF ( TRGSEP .GT. MAXANG ) THEN
C
C              No occultation is possible.
C       
               OCSTAT = .FALSE.

               CALL CHKOUT ( 'ZZGFOCST' )
               RETURN

            END IF

         END IF


         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF

C
C        We'll need the negatives of the observer-target vectors in
C        several places later, so compute them now.
C
         CALL VMINUS ( FRTPOS, FRTOBS )
         CALL VMINUS ( BCKPOS, BCKOBS )

C
C        Now check for an occulted state assuming a spherical extended
C        body with radius equal to the minimum semi-axis. Again, 
C        adjust the sphere for our light time error.
C
         IF ( PNTOCC ) THEN
            MINANG = DASINE ( (1-ALPHA)*SVMNFR / FDIST, ATOL )
         ELSE
            MINANG = DASINE ( (1-ALPHA)*SVMNBR / BDIST, ATOL )
         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
         END IF


         IF ( TRGSEP .LT. MINANG ) THEN
C
C           The targets must overlap as seen from the observer.
C
            IF ( PNTOCC ) THEN
C
C              Examine the angle between the vector from FRONT to the
C              observer and the vector from FRONT to BACK.  If that
C              angle is greater than or equal to the complement of the
C              angular radius of FRONT, then FRONT occults BACK. First
C              find the position of FRONT and BACK relative to each
C              other.
C
               CALL VMINUS  ( BCKFRT, FRTBCK )

               T2SEP = VSEP ( FRTOBS, FRTBCK )
       
               IF (  T2SEP  .GT.  ( HALFPI() - MINANG )  ) THEN
C
C                 There must be an occultation.
C 
                  OCSTAT = .TRUE.

               ELSE
C
C                 There can't be an occultation: the "back" object
C                 is actually in transit across the "front" object.
C            
                  OCSTAT = .FALSE.

               END IF

            ELSE
C
C              We're looking for a point transit condition.
C
               T2SEP = VSEP ( BCKOBS, BCKFRT )
       
               IF (  T2SEP  .LT.  ( HALFPI() - MINANG )  ) THEN
C
C                 There must be a transit.
C 
                  OCSTAT = .TRUE.

               ELSE
C
C                 There can't be a transit: the "back" object
C                 actually occults the "front" object.
C            
                  OCSTAT = .FALSE.

               END IF

            END IF

C
C           OCSTAT has been set.
C
            CALL CHKOUT ( 'ZZGFOCST' )
            RETURN
  
         END IF

C
C        If we've reached this point, we have a situation where we
C        can't classify the geometry using bounding spheres. Instead,
C        we'll see whether the observer-point target vector intersects
C        the ellipsoidal body.
C 
         IF ( PNTOCC ) THEN
C
C           The front body is the ellipsoid.
C
            CALL SINCPT ( 'Ellipsoid', SVFNAM,  TIME,    SVFFRM, 
     .                    SVCORR,      SVONAM,  'J2000', BCKPOS, 
     .                    SPOINT,      TRGEPC,  SRFVEC,  FOUND )


            IF ( FAILED()) THEN
               CALL CHKOUT ( 'ZZGFOCST' )
               RETURN
            END IF

            IF ( FOUND ) THEN
C
C              There's an intercept. If the distance from the observer
C              to the intercept is less than the distance from the
C              observer to the back target, then the back target is
C              occulted; otherwise there's a point transit, which is
C              not considered an occultation in this case.
C
               OCSTAT  =  VNORM(SRFVEC) .LT. BDIST 

            ELSE
C
C              There's no overlap and hence no occultation.
C
               OCSTAT = .FALSE.

            END IF
            
         ELSE
C
C           The back body is the ellipsoid.
C
            CALL SINCPT ( 'Ellipsoid', SVBNAM,  TIME,    SVBFRM, 
     .                    SVCORR,      SVONAM,  'J2000', FRTPOS, 
     .                    SPOINT,      TRGEPC,  SRFVEC,  FOUND )

            IF ( FAILED()) THEN
               CALL CHKOUT ( 'ZZGFOCST' )
               RETURN
            END IF

            IF ( FOUND ) THEN
C
C              There's an intercept. If the distance from the observer
C              to the intercept is greater than the distance from the
C              observer to the front target, then the front target is
C              in transit across the back target; otherwise there's a
C              point occultation, which is not considered a transit in
C              this case.
C
               OCSTAT  =  VNORM(SRFVEC) .GT. FDIST 

            ELSE
C
C              There's no overlap and hence no occultation.
C
               OCSTAT = .FALSE.

            END IF

         END IF

      ELSE
C
C        Bad combination of shapes. We expect this situation to have
C        been caught at initialization time, but make this check for
C        safety.
C
         CALL SETMSG ( 'The combination of shapes of front and '
     .   //            'back targets is not supported: front '
     .   //            'shape = #; back shape = #.'             )
         CALL ERRCH  ( '#', SVFSHP                              )
         CALL ERRCH  ( '#', SVBSHP                              )
         CALL SIGERR ( 'SPICE(INVALIDSHAPECOMBO)'               )
         CALL CHKOUT ( 'ZZGFOCST'                               )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZGFOCST' )
      RETURN
      END
