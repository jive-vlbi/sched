C$Procedure      ZZGFILU ( GF, illumination angle utilities )
 
      SUBROUTINE ZZGFILU ( METHOD, ANGTYP, TARGET, ILLUM, FIXREF, 
     .                     ABCORR, OBSRVR, SPOINT, ET,    UDFUNC,  
     .                     DECRES, ANGLE                          )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine for the entry points used by
C     GFEVNT in order to find illumination angle events.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     ANGLE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         ANGTYP
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      ET
      EXTERNAL              UDFUNC
      LOGICAL               DECRES
      DOUBLE PRECISION      ANGLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     METHOD     I   ZZGFILIN
C     ANGTYP     I   ZZGFILIN
C     TARGET     I   ZZGFILIN
C     ILLUM      I   ZZGFILIN
C     FIXREF     I   ZZGFILIN
C     ABCORR     I   ZZGFILIN
C     OBSRVR     I   ZZGFILIN
C     SPOINT     I   ZZGFILIN
C     ET         I   ZZGFILLT, ZZGFILGQ
C     UDFUNC     I   ZZGFILDC
C     REF        I   ZZGFILIN
C     DECRES     O   ZZGFILDC
C     ANGLE      O   ZZGFILGQ
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
C     See individual entry points.
C
C$ Files
C
C     Appropriate kernels must be loaded by the calling program before
C     this routine is called.
C
C     The following data are required:
C
C        - SPK data: ephemeris data for target, observer, and
C          illumination source, for the times at which state or
C          positions are computed, must be loaded. If aberration
C          corrections are used, the states of target and observer
C          relative to the solar system barycenter must be calculable
C          from the available ephemeris data. Typically ephemeris data
C          are made available by loading one or more SPK files via
C          FURNSH.
C
C        - If non-inertial reference frames are used, then PCK
C          files, frame kernels, C-kernels, and SCLK kernels may be
C          needed.
C
C     In all cases, kernel data are normally loaded once per program
C     run, NOT every time this routine is called.
C
C$ Particulars
C
C     This is an umbrella for routines required by the GF scalar 
C     quantity search algorithm to support searches involving
C     illumination angle constraints. 
C
C     The entry points of this routine are:
C
C        ZZGFILIN   Saves the user-supplied inputs defining the
C                   illumination angle computation to be performed.
C                   Initializes the illumination angle search.
C
C        ZZGFILDC   Determines whether or not a specified illumination
C                   angle is decreasing at a specified epoch.
C
C        ZZGFILGQ   Returns the specified illumination angle
C                   at a specified epoch.
C
C$ Examples
C
C     See GFEVNT.
C
C$ Restrictions
C
C     This is a SPICELIB private routine; it should not be called by
C     user applications.
C
C     ZZGFILIN must be called prior to use of any of the other
C     entry points.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 23-MAY-2012 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     umbrella routine for finding illumination angle events.
C
C-&      


C
C     SPICELIB functions
C
      INTEGER               ESRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 200 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10 )

C
C     Indices of illumination angles in the ANGLES
C     array:
C
      INTEGER               PHSIDX
      PARAMETER           ( PHSIDX = 1 )

      INTEGER               INCIDX
      PARAMETER           ( INCIDX = 2 )

      INTEGER               EMTIDX
      PARAMETER           ( EMTIDX = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 50 )

C
C     Local Variables
C
      CHARACTER*(NAMLEN)    ANGNMS ( 3 )
      CHARACTER*(CORLEN)    SVCORR
      CHARACTER*(BDNMLN)    SVINAM
      CHARACTER*(MTHLEN)    SVMETH
      CHARACTER*(BDNMLN)    SVONAM
      CHARACTER*(FRNMLN)    SVREF
      CHARACTER*(BDNMLN)    SVTNAM

      DOUBLE PRECISION      ANGLES ( 3 )
      DOUBLE PRECISION      EMISTA ( 2 )
      DOUBLE PRECISION      ETTARG
      DOUBLE PRECISION      INCSTA ( 2 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PHSSTA ( 2 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SVNRML ( 3 )
      DOUBLE PRECISION      SVSSPT ( 3 )

      INTEGER               N
      INTEGER               FXFCDE
      INTEGER               FXCLSS
      INTEGER               FXTYID
      INTEGER               FXCENT
      INTEGER               SVAIDX
      INTEGER               SVOBS
      INTEGER               SVILUM
      INTEGER               SVTARG

      LOGICAL               FOUND
      LOGICAL               SVABLK ( NABCOR )

C
C     Saved Variables
C
      SAVE                  ANGNMS
      SAVE                  SVABLK
      SAVE                  SVAIDX
      SAVE                  SVCORR
      SAVE                  SVILUM
      SAVE                  SVINAM
      SAVE                  SVMETH
      SAVE                  SVNRML
      SAVE                  SVOBS
      SAVE                  SVONAM
      SAVE                  SVREF
      SAVE                  SVSSPT
      SAVE                  SVTARG
      SAVE                  SVTNAM

C
C     Initial values
C      
      DATA                  ANGNMS / 'PHASE', 
     .                               'INCIDENCE',
     .                               'EMISSION'   /

C
C     This routine should never be called directly.
C
      CALL CHKIN  ( 'ZZGFILU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFILU'           )
      RETURN


 
C$Procedure  ZZGFILIN ( GF, illumination angle utility initialization )
 
      ENTRY ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,
     .                 FIXREF, ABCORR, OBSRVR, SPOINT )
 
C$ Abstract
C
C     Initialize the GF illumination angle constraint search utilities.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     ANGLE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C      CHARACTER*(*)         TARGET
C      CHARACTER*(*)         ILLUM
C      CHARACTER*(*)         ABCORR
C      CHARACTER*(*)         OBSRVR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TARGET     I   Target body name.
C     TARGET     I   Illumination source name.
C     ABCORR     I   Aberration correction specifier.
C     OBSRVR     I   Observer name.
C
C$ Detailed_Input
C
C     TARGET     is the name of a target body. Optionally, you may
C                supply the integer ID code for the object as
C                an integer string. For example both 'MOON' and
C                '301' are legitimate strings that indicate the 
C                moon is the target body.
C
C                The target and observer define a position vector
C                which points from the observer to the target.
C
C                Case and leading or trailing blanks are not
C                significant in the string TARGET.
C
C
C     ILLUM      is the name of the illumination source. This source
C                may be any ephemeris object. Case, blanks, and
C                numeric values are treated in the same way as for the
C                input TARGET.
C
C
C     ABCORR     indicates the aberration corrections to be applied
C                when computing the target's position and orientation.
C                Any value accepted by SPKEZR may be used.
C                
C                See the header of the SPICE routine SPKEZR for a
C                detailed description of the aberration correction
C                options. 
C
C                Case and embedded blanks are not significant in
C                ABCORR.
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
C     1)  If name of the target, observer, or illumination source
C         cannot be translated to a NAIF ID code, the error
C         SPICE(IDCODENOTFOUND) is signaled.
C
C     2)  If  target body coincides with the observer body OBSRVR or
C         the illumination source SOURCE, error
C         SPICE(BODIESNOTDISTINCT) will be signaled.
C
C     3)  If transmission-style aberration corrections are requested,
C         the error SPICE(NOTSUPPORTED) is signaled.
C
C     4)  If the aberration correction string is invalid, the error
C         will be diagnosed by a routine in the call tree of this
C         routine.                 
C
C     5)  If the illumination angle type is not recognized, the error
C         SPICE(NOTSUPPORTED) will be signaled.
C
C     6)  If the input target body-fixed frame FIXREF is not
C         recognized, the error SPICE(UNKNOWNFRAME) is signaled. A
C         frame name may fail to be recognized because a required frame
C         specification kernel has not been loaded; another cause is a
C         misspelling of the frame name.
C
C     7)  If the input frame FIXREF is not centered at the target body,
C         the error SPICE(INVALIDFRAME) is signaled.
C
C     8)  If the input argument METHOD is not recognized, the error
C         SPICE(INVALIDMETHOD) is signaled.
C
C
C$ Files
C
C     See the header of the umbrella routine ZZGFILU.
C
C$ Particulars
C
C     This routine must be called once before each GF search for
C     illumination angle events.
C
C$ Examples
C
C     See GFEVNT.
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 23-MAY-2012 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     illumination angle initialization routine
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFILIN' )

C
C     Find NAIF IDs for TARGET, OBSRVR, and ILLUM.
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
         CALL CHKOUT ( 'ZZGFILIN'                                 )
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
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( ILLUM, SVILUM, FOUND )

      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The illumination source, '
     .   //            '''#'', is not a recognized name for an '
     .   //            'ephemeris object. The cause of this '
     .   //            'problem may be that you need an updated '
     .   //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', ILLUM                                 )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
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
         CALL CHKOUT ( 'ZZGFILIN'                          )
         RETURN

      END IF

C
C     Make sure the target and illumination source are distinct.
C
      IF ( SVTARG .EQ. SVILUM ) THEN

         CALL SETMSG ( 'The target and illumination source must '
     .   //            'be distinct objects, but are not: '
     .   //            'TARGET = #; ILLUM = #.'                   )
         CALL ERRCH  ( '#', TARGET                                )
         CALL ERRCH  ( '#', ILLUM                                 )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'                 )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF

C
C     Save the observer, target, and illumination source names.
C     
      SVONAM = OBSRVR
      SVTNAM = TARGET
      SVINAM = ILLUM

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
      CALL ZZVALCOR ( SVCORR, SVABLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

C
C     Reject transmission corrections.
C
      IF ( SVABLK(XMTIDX) ) THEN

         CALL SETMSG ( 'Aberration correction was #; '
     .   //            'transmission corrections are not '
     .   //            'allowed by this routine.'        )
         CALL ERRCH  ( '#',  ABCORR                      )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'             )
         CALL CHKOUT ( 'ZZGFILIN'                        )
         RETURN
         
      END IF

C
C     Look up the radii for the target body.
C
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )

C
C     Find the surface normal at the surface point. Create a
C     body-fixed state vector for the normal.
C
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

      CALL VEQU ( NORMAL, SVNRML )

C
C     Save the surface point in the body-fixed reference frame.
C
      CALL VEQU ( SPOINT, SVSSPT)

C
C     Save a left-justified, upper case copy of the computation method
C     for the illumination angles.
C
      CALL LJUST ( METHOD, SVMETH )
      CALL UCASE ( SVMETH, SVMETH )

      IF ( SVMETH .NE. 'ELLIPSOID' ) THEN

         CALL SETMSG ( 'The only supported computation method '
     .   //            'is ELLIPSOID; the input method was #.' )
         CALL ERRCH  ( '#',  METHOD                            )
         CALL SIGERR ( 'SPICE(INVALIDMETHOD)'                  )
         CALL CHKOUT ( 'ZZGFILIN'                              )
         RETURN

      END IF

C
C     Save a left-justified, upper case copy of the reference frame
C     name.
C
      CALL LJUST ( FIXREF, SVREF )
      CALL UCASE ( SVREF,  SVREF )

C
C     Look up the frame attributes; make sure the frame is centered
C     on the target body.
C
C
C     Determine the attributes of the frame designated by FIXREF.
C
      CALL NAMFRM ( FIXREF, FXFCDE )

      CALL FRINFO ( FXFCDE, FXCENT, FXCLSS, FXTYID, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFILIN' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Reference frame # is not recognized by ' //
     .                 'the SPICE frame subsystem. Possibly '    //
     .                 'a required frame definition kernel has ' //
     .                 'not been loaded.'                        )
         CALL ERRCH  ( '#',  FIXREF                              )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                     )
         CALL CHKOUT ( 'ZZGFILIN'                                )
         RETURN

      END IF

C
C     Make sure that FIXREF is centered at the target body's center.
C
      IF ( FXCENT .NE. SVTARG ) THEN

         CALL SETMSG ( 'Reference frame # is not centered at the ' 
     .   //            'target body #. The ID code of the frame '
     .   //            'center is #.'                             )
         CALL ERRCH  ( '#',  FIXREF                               )
         CALL ERRCH  ( '#',  TARGET                               )
         CALL ERRINT ( '#',  FXCENT                               )
         CALL SIGERR ( 'SPICE(INVALIDFRAME)'                      )
         CALL CHKOUT ( 'ZZGFILIN'                                 )
         RETURN

      END IF

C
C     Save the index of the angle type.
C
      SVAIDX = ESRCHC ( ANGTYP, 3, ANGNMS )

      IF ( SVAIDX .EQ. 0 ) THEN

         CALL SETMSG ( 'Illumination angle type # is not recognized.' )
         CALL ERRCH  ( '#',  ANGTYP                                   )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                          )
         CALL CHKOUT ( 'ZZGFILIN'                                     )
         RETURN

      END IF 

 
      CALL CHKOUT ( 'ZZGFILIN' )
      RETURN





C$Procedure ZZGFILDC ( GF, is illumination angle decreasing? )
 
      ENTRY ZZGFILDC ( UDFUNC, ET, DECRES )
 
C$ Abstract
C
C     Indicate whether a specified illumination angle is decreasing at
C     a specified time.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     ANGLE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C     EXTERNAL              UDFUNC
C     DOUBLE PRECISION      ET
C     LOGICAL               DECRES
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UDFUNC     I   Placeholder external routine argument.
C     ET         I   Ephemeris seconds past J2000 TDB.
C     DECRES     O   Flag indicating whether illumination angle is
C                    decreasing.
C
C$ Detailed_Input
C
C     UDFUNC     is a placeholder subroutine argument. This argument is
C                provided for compatibility with ZZGFSOLVX. It is not
C                used by the entry points of this package.
C
C     ET         is the time, expressed as seconds past J2000 TDB, at
C                which to determine whether or not the illumination
C                angle between the observer and target is decreasing.
C
C$ Detailed_Output
C
C     DECRES     is a logical flag that indicates whether the
C                observer-target illumination angle is decreasing at
C                ET. The observer, target, and aberration correction
C                used to compute the illumination angle are defined by
C                the latest call to the initialization entry point
C                ZZGFILIN.
C
C                DECRES is .TRUE. if and only if the observer-target
C                illumination angle is decreasing at ET.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the state of the target relative to the observer
C        at ET can not be found due to an SPK lookup failure,
C        the error will be diagnosed by routines in the call
C        tree of this routine.
C
C$ Files
C
C     See the header of the umbrella routine ZZGFILU.
C
C$ Particulars
C
C     This routine is used by ZZGFRELX, and indirectly by GFILUM, to
C     determine the time intervals, within the confinement window, on
C     which the observer-target illumination angle is monotone
C     increasing or monotone decreasing.
C
C$ Examples
C
C     See GFILUM, GFEVNT, ZZGFRELX.
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 23-MAY-2012 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     indicate whether illumination angle is decreasing
C
C-&


C
C     Standard SPICE error handling.
C 
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFILDC' ) 

C
C     Compute the rates of change of all of the illumination angles.
C
      CALL ZZILUSTA ( SVMETH, SVTNAM, SVINAM, ET,  
     .                SVREF,  SVCORR, SVONAM, SVSSPT,
     .                SVNRML, PHSSTA, INCSTA, EMISTA  )

      IF ( SVAIDX .EQ. PHSIDX ) THEN

         RATE = PHSSTA(2) 

      ELSE IF ( SVAIDX .EQ. INCIDX ) THEN

         RATE = INCSTA(2)

      ELSE IF ( SVAIDX .EQ. EMTIDX ) THEN
         
         RATE = EMISTA(2)

      ELSE
C
C        We should never get here.
C
         CALL SETMSG ( 'Unexpected value of SVAIDX: #.' )
         CALL ERRINT ( '#',  SVAIDX                     )
         CALL SIGERR ( 'SPICE(BUG)'                     )

      END IF
C
C     The observer-target illumination angle is decreasing if and only
C     the derivative of the angle with respect to time is negative.
C 
      DECRES  =  RATE  .LT.  0.D0

      CALL CHKOUT ( 'ZZGFILDC' )
      RETURN
 


C$Procedure ZZGFILGQ ( GF, get illumination angle )
 
      ENTRY ZZGFILGQ ( ET, ANGLE )
 
C$ Abstract
C
C     Return the specified illumination angle at a specified epoch.
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
C     SPK
C     TIME
C
C$ Keywords
C
C     ILLUMINATION ANGLE
C     EPHEMERIS
C     GEOMETRY
C     SEARCH
C
C$ Declarations
C
C      DOUBLE PRECISION      ET
C      DOUBLE PRECISION      ANGLE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     ANGLE      O   Illumination angle at time ET.
C
C$ Detailed_Input
C
C     ET         is the time, expressed as seconds past J2000 TDB, at
C                which the illumination angle is to be computed.
C
C$ Detailed_Output
C
C     ANGLE      is the illumination angle as seen by the observer at
C                time ET. The observer, target, and aberration
C                correction used to compute the illumination angle are
C                defined by the latest call to the initialization entry
C                point ZZGFILIN.
C
C                Units are radians.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the position of the target relative to the observer
C        at ET can not be found due to an SPK lookup failure,
C        the error will be diagnosed by routines in the call
C        tree of this routine.
C
C$ Files
C
C     See the header of the umbrella routine ZZGFILU.
C
C$ Particulars
C
C     This routine determines the apparent illumination angle as seen
C     from the observer at time ET. This functionality supports GFREL's
C     comparisons of relative extrema in order to determine absolute
C     extrema.
C
C$ Examples
C
C     See GFREL.
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 23-MAY-2012 (NJB) (EDW)
C    
C-&

C$ Index_Entries
C
C     return illumination angle
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFILGQ' )

      CALL ILLUMG ( SVMETH, SVTNAM,    SVINAM,    ET,      SVREF,  
     .              SVCORR, SVONAM,    SVSSPT,    ETTARG,  
     .              SRFVEC, ANGLES(1), ANGLES(2), ANGLES(3)       )
 

      ANGLE = ANGLES( SVAIDX ) 

      CALL CHKOUT ( 'ZZGFILGQ' )
      RETURN
      END


