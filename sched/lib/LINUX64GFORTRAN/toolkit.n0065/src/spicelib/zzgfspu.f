C$Procedure ZZGFSPU ( Private - GF, angular separation routines )

      SUBROUTINE ZZGFSPU ( OF,     FROM,   SHAPE,  FRAME,  ET,
     .                     UDFUNC, ABCORR, DECRES, SEP,
     .                     XABCR,  XBOD,   YREF,   XREF,
     .                     XOBS,   XRAD,   XSHP )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the umbrella routine for the entry points needed by
C     GFEVNT in order to find angular separation events.
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
C     None.
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     ROOT
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         OF    (2)
      CHARACTER*(*)         FROM
      CHARACTER*(*)         SHAPE (2)
      CHARACTER*(*)         FRAME (2)
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR
      LOGICAL               DECRES
      DOUBLE PRECISION      SEP
      CHARACTER*(*)         XABCR
      INTEGER               XBOD  (2)
      CHARACTER*(*)         YREF
      CHARACTER*(*)         XREF  (2)
      INTEGER               XOBS
      DOUBLE PRECISION      XRAD  (2)
      INTEGER               XSHP  (2)

C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     OF         I   ZZGFSPIN
C     FROM       I   ZZGFSPIN
C     SHAPE      I   ZZGFSPIN
C     FRAME      I   ZZGFSPIN
C     ET         I   ZZGFSPDC, ZZGFSPGQ
C     ABCORR     I   ZZGFSPIN
C     UDFUNC     I   ZZGFSPDC
C     DECRES     O   ZZGFSPDC
C     SEP        O   ZZGFSPGQ
C
C$ Detailed_Input
C
C     OF       the string array naming the bodies whose angular
C              separation is of interest.
C
C     FROM     the string naming the observer.
C
C     SHAPE    the string array naming the geometric model used to
C              represent the shapes of OF. The relation between SHAPE
C              and OF is 1-to-1.
C
C              Models supported by this routine:
C
C                 'SPHERE'        Treat the body as a sphere with
C                                 radius equal to the maximum value of
C                                 BODYnnn_RADII
C
C                 'POINT'         Treat the body as a single point;
C                                 radius has value zero.
C
C              The SHAPE string lacks sensitivity to case and leading
C              or trailing blank.
C
C     FRAME    the string array naming the body-fixed reference frames
C              corresponding to OF. The relation between FRAME
C              and OF is 1-to-1.
C
C     ET       is the time in second past J2000 at which one wants
C              to determine an event condition.
C
C     ABCORR   the string description of the aberration corrections
C              to apply to the state evaluations to account for
C              one-way light time and stellar aberration.
C
C              This routine accepts the same aberration corrections
C              as does the SPICE routine SPKEZR. See the header of
C              SPKEZR for a detailed description of the aberration
C              correction options. For convenience, the options are
C              listed below:
C
C                 'NONE'     Apply no correction.
C
C                 'LT'       "Reception" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case:  converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case:  converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 The ABCORR string lacks sensitivity to case, leading
C                 and trailing blanks.
C
C     DECRES   is .TRUE. if the angular separation between the
C              objects is decreasing.  Otherwise it is .FALSE.
C
C     SEP      is the angular separation between SVBOD1 and SVBOD2 as
C              seen from SVOBS at time ET.
C
C     For more information, see individual entry points.
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
C     1) SPICE(BOGUSENTRY) signals if a direct call to ZZGFSPU occurs.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine serves as the umbrella routine for 4 entry points
C     needed by GFEVNT in solving for angular separation conditions.
C
C     The five entry points are
C
C        ZZGFSPIN  --- an initialization routine that must be called
C                     prior to attempting to solve for any angular
C                     separation event.
C
C        ZZGFSPDC --- determines whether or not angular separation is
C                     decreasing at some time.
C
C        ZZGFSPGQ --- returns the angular separation of the two
C                     objects of interest as a function of ET.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     ZZGFSPIN must be called prior to use of any of the
C     other entry points.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 2.0.0 27-JUN-2012 (EDW)
C
C        Code edits to implement use of ZZGFRELX.
C        These edits include removal of unneeded routines:
C
C           ZZGFSPUR
C           ZZGFSPLT
C
C        and corresponding unused variables.
C
C        Routine ZZGFGSEP renamed to ZZGFSPGQ to match geometry finder
C        naming convention.
C
C        Implemented a proper Exceptions section. Update to header
C        entries.
C
C-    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW)
C
C        Edited argument descriptions. Removed mention of "ELLIPSOID"
C        shape from SHAPE as that option is not yet implemented.
C
C        Added an error check on body frame centers to enforce
C        a body frame center is the body. This check does not apply
C        to "POINT" or "SPHERE" shape targets, and so will not
C        execute for this version of the routine.
C
C        Rename of the ZZDHA call to DHFA.
C
C-    SPICELIB Version 1.0.0 19-FEB-2009 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     umbrella routine for finding angular separation events
C
C-&

      EXTERNAL              UDFUNC


C
C     SPICELIB functions
C
      INTEGER               ISRCHC
      LOGICAL               FAILED
      LOGICAL               RETURN
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      DHFA

C
C     Local Variables
C
      DOUBLE PRECISION      AXES1 (3)
      DOUBLE PRECISION      AXES2 (3)

      DOUBLE PRECISION      PV1   (6)
      DOUBLE PRECISION      PV2   (6)

      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      LT

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND

      INTEGER               FCODE1
      INTEGER               FCODE2
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CTR1
      INTEGER               CTR2


C
C     Saved Variables
C
      CHARACTER*(32)        SVABCR
      SAVE                  SVABCR

      INTEGER               SVBOD1
      SAVE                  SVBOD1

      INTEGER               SVBOD2
      SAVE                  SVBOD2

      CHARACTER*(32)        SVREF
      SAVE                  SVREF

      CHARACTER*(32)        SVREF1
      SAVE                  SVREF1

      CHARACTER*(32)        SVREF2
      SAVE                  SVREF2

      INTEGER               SVOBS
      SAVE                  SVOBS

      DOUBLE PRECISION      SVRAD1
      SAVE                  SVRAD1

      DOUBLE PRECISION      SVRAD2
      SAVE                  SVRAD2

      INTEGER               SVSHP1
      SAVE                  SVSHP1

      INTEGER               SVSHP2
      SAVE                  SVSHP2

      CHARACTER*(5)         REF
      SAVE                  REF

C
C     Below we initialize the list of shape names.
C
      INTEGER               SPSHPN
      PARAMETER           ( SPSHPN = 2 )

      CHARACTER*(32)        SVSHAP ( SPSHPN )

C
C     Define integer ID parameters for the shape names in
C     SVSHAP.
C
      INTEGER               POINT
      PARAMETER           ( POINT = 1 )

      INTEGER               SPHERE
      PARAMETER           ( SPHERE = 2 )

      SAVE                  SVSHAP

      DATA                  SVSHAP / 'POINT',
     .                               'SPHERE' /

      DATA                  REF    / 'J2000'/
C
C     Never directly call this routine.
C
      CALL CHKIN ( 'ZZGFSPU'           )
      CALL SIGERR( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT( 'ZZGFSPU'           )

      RETURN




C$Procedure ZZGFSPIN ( Private - GF, angular separation initialization )

      ENTRY ZZGFSPIN ( OF, FROM, SHAPE, FRAME, ABCORR )

C$ Abstract
C
C     This routine initializes variables that describe an angular
C     separation event of interest for solution by ZZGFSOLV.
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
C     None.
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     ROOT
C
C$ Declarations
C
C      CHARACTER*(*)         OF   ( 2 )
C      CHARACTER*(*)         FROM
C      CHARACTER*(*)         SHAPE( 2 )
C      CHARACTER*(*)         FRAME( 2 )
C      CHARACTER*(*)         ABCORR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OF         I   Body id's of the angular separation objects
C     FROM       I   Observer name
C     SHAPE      I   Array of shape IDs corresponding to OF
C     FRAME      I   Array of frame names corresponding to OF
C     ABCORR     I   Aberration correction flag.
C
C$ Detailed_Input
C
C     OF       the string array naming the bodies whose angular
C              separation is of interest.
C
C     FROM     the string naming the observer.
C
C     SHAPE    the string array naming the geometric model used to
C              represent the shapes of OF. The relation between SHAPE
C              and OF is 1-to-1 and onto.
C
C              Models supported by this routine:
C
C                 'SPHERE'        Treat the body as a sphere with
C                                 radius equal to the maximum value of
C                                 BODYnnn_RADII
C
C                 'POINT'         Treat the body as a single point;
C                                 radius has value zero.
C
C              The SHAPE string lacks sensitivity to case and leading
C              or trailing blank.
C
C     FRAME    the string array naming the body-fixed reference frames
C              corresponding to OF. The relation between FRAME
C              and OF is 1-to-1.
C
C     ABCORR   the string description of the aberration corrections
C              to apply to the state evaluations to account for
C              one-way light time and stellar aberration.
C
C              This routine accepts the same aberration corrections
C              as does the SPICE routine SPKEZR. See the header of
C              SPKEZR for a detailed description of the aberration
C              correction options. For convenience, the options are
C              listed below:
C
C                 'NONE'     Apply no correction.
C
C                 'LT'       "Reception" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'LT+S'     "Reception" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'CN'       "Reception" case:  converged
C                            Newtonian light time correction.
C
C                 'CN+S'     "Reception" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 'XLT'      "Transmission" case:  correct for
C                            one-way light time using a Newtonian
C                            formulation.
C
C                 'XLT+S'    "Transmission" case:  correct for
C                            one-way light time and stellar
C                            aberration using a Newtonian
C                            formulation.
C
C                 'XCN'      "Transmission" case:  converged
C                            Newtonian light time correction.
C
C                 'XCN+S'    "Transmission" case:  converged
C                            Newtonian light time and stellar
C                            aberration corrections.
C
C                 The ABCORR string lacks sensitivity to case, leading
C                 and trailing blanks.
C
C$ Detailed_Output
C
C     None
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) SPICE(IDCODENOTFOUND) signals if the object name for target 1,
C        OF(1), is not a recognized name.
C
C     2) SPICE(IDCODENOTFOUND) signals if the object name for target 2,
C        OF(2), is not a recognized name.
C
C     3) SPICE(IDCODENOTFOUND) signals if the object name for the
C        observer, FROM, is not a recognized name.
C
C     4) SPICE(BODIESNOTDISTINCT) signals if the three objects
C        associated with an ANGULAR SEPARATION search are not distinct.
C
C     5) SPICE(NOTRECOGNIZED) signals if the body shape for target 1,
C        SHAPE(1), is not recognized.
C
C     6) SPICE(BUG) signals if the SHAPE(1) value lacks a corresponding
C        case block. This indicates a programming error.
C
C     7) SPICE(NOTRECOGNIZED) signals if the body shape for target 2,
C        SHAPE(2), is not recognized.
C
C     8) SPICE(BUG) signals if the SHAPE(2) value lacks a corresponding
C        case block. This indicates a programming error.
C
C     9) SPICE(NOFRAME) signals if frame subsystem did not recognize
C         frame name FRAME(1).
C
C     10) SPICE(INVALIDFRAME) signals if the reference frame associated
C         with target body 1, OF(1), is not centered on target body 1.
C
C     11) SPICE(NOFRAME) signals if frame subsystem did not recognize
C         frame name FRAME(2).
C
C     12) SPICE(INVALIDFRAME) signals if the reference frame associated
C         with target body 2, OF(2), is not centered on target body 2.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 2.0.0 27-JUN-2012 (EDW)
C
C        REFVAL removed from routine argument list due to the use
C        of ZZGFRELX to calculate the events.
C
C        Implemented a proper Exceptions section. Update to
C        Author_and_Institution section.
C
C-    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW)
C
C        Edited argument descriptions. Removed mention of "ELLIPSOID"
C        shape from SHAPE as that option is not yet implemented.
C
C        Added an error check on body frame centers to enforce
C        a body frame center is the body. This check does not apply
C        to "POINT" or "SPHERE" shape targets, and so will not
C        execute for this version of the routine.
C
C-    SPICELIB Version 1.0.0 14-APR-2008 (NJB) (EDW)
C
C-&

C$ Index_Entries
C
C     angular separation initialization routine
C
C-&

      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN( 'ZZGFSPIN' )
      END IF

      CALL BODS2C ( OF(1), SVBOD1, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 1, '
     .      //         '''#'', is not a recognized name for an '
     .      //         'ephemeris object. The cause of this '
     .      //         'problem may be that you need an updated '
     .      //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', OF( 1 )                            )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZGFSPIN'                              )
         RETURN

      END IF


      CALL BODS2C ( OF(2), SVBOD2, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 2, '
     .      //         '''#'', is not a recognized name for an '
     .      //         'ephemeris object. The cause of this '
     .      //         'problem may be that you need an updated '
     .      //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', OF ( 2 )                           )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZGFSPIN'                              )
         RETURN

      END IF


      CALL BODS2C ( FROM, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The object name for the observer, '
     .      //            '''#'', is not a recognized name for an '
     .      //            'ephemeris object. The cause of this '
     .      //            'problem may be that you need an updated '
     .      //            'version of the SPICE Toolkit.'          )
         CALL ERRCH  ( '#', FROM                                   )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                     )
         CALL CHKOUT ( 'ZZGFSPIN'                                  )
         RETURN

      END IF


C
C     Confirm the three bodies have unique IDs.
C
      IF      (       ( SVOBS  .EQ. SVBOD1 )
     .           .OR. ( SVOBS  .EQ. SVBOD2 )
     .           .OR. ( SVBOD1 .EQ. SVBOD2) ) THEN

         CALL SETMSG( 'All three objects associated '
     .      //           'with an ANGULAR SEPARATION search must be '
     .      //           'distinct. The objects whose angular '
     .      //           'separation is of interest were # and #. '
     .      //           'The observer was #.'                      )
         CALL ERRINT( '#', SVBOD1                                   )
         CALL ERRINT( '#', SVBOD2                                   )
         CALL ERRINT( '#', SVOBS                                    )
         CALL SIGERR( 'SPICE(BODIESNOTDISTINCT)'                    )
         CALL CHKOUT( 'ZZGFSPIN'                                    )
         RETURN

      END IF

C
C     Squeeze all blanks out of the aberration correction
C     string; ensure the string is in upper case.
C
      CALL CMPRSS ( ' ', 0, ABCORR, SVABCR )
      CALL UCASE  ( SVABCR, SVABCR )

C
C     Check the aberration correction. If SPKEZR can't handle it,
C     neither can we.
C
      CALL ZZVALCOR ( SVABCR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFSPIN' )
         RETURN
      END IF

      SVREF  = REF
      SVREF1 = FRAME(1)
      SVREF2 = FRAME(2)

C
C     Check shapes...
C
      CALL LJUST ( SHAPE(1), SHAPE(1) )
      CALL UCASE ( SHAPE(1), SHAPE(1) )

C
C     If we pass the error check, then SHAPE(1) exists in SVSHAP.
C
      SVSHP1 = ISRCHC( SHAPE(1), SPSHPN, SVSHAP)

      IF ( SVSHP1 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not recognized.  '
     .      //         'Supported quantities are: '
     .      //         'POINT, SPHERE.'                        )
         CALL ERRCH  ( '#', SHAPE(1)                           )
         CALL SIGERR ( 'SPICE(NOTRECOGNIZED)'                  )
         CALL CHKOUT ( 'ZZGFSPIN'                              )
         RETURN

      ELSE IF ( SVSHP1 . EQ. POINT ) THEN

         SVRAD1 = 0.D0

      ELSE IF ( SVSHP1 . EQ. SPHERE ) THEN

         CALL ZZGFTREB( SVBOD1, AXES1 )

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZGFSPIN' )
            RETURN
         END IF

         SVRAD1 = MAX( AXES1(1), AXES1(2), AXES1(3) )

      ELSE

C
C        This code executes only if someone adds a new shape
C        name to SVSHAP then fails to update the SVSHP1 condition
C        block to respond to the name. Fortran needs SWITCH...CASE.
C
         CALL SETMSG ('Encountered uncoded shape ID for #. '     //
     .                'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAPE(1)                               )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFSPIN'                                 )
         RETURN

      END IF

      CALL LJUST ( SHAPE(2), SHAPE(2) )
      CALL UCASE ( SHAPE(2), SHAPE(2) )

C
C     If we pass the error check, then SHAPE(2) exists in SVSHAP.
C
      SVSHP2 = ISRCHC( SHAPE(2), SPSHPN, SVSHAP)

      IF ( SVSHP2 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not '                 //
     .                 'recognized.  Supported quantities are: '   //
     .                 'POINT, SPHERE.'                             )
         CALL ERRCH ( '#', SHAPE(2)                                 )
         CALL SIGERR( 'SPICE(NOTRECOGNIZED)'                        )
         CALL CHKOUT( 'ZZGFSPIN'                                    )
         RETURN

      ELSE IF ( SVSHP2 .EQ. POINT ) THEN

         SVRAD2 = 0.D0

      ELSE IF ( SVSHP2 .EQ. SPHERE ) THEN

         CALL ZZGFTREB( SVBOD2, AXES2)

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZGFSPIN' )
            RETURN
         END IF

         SVRAD2 = MAX( AXES2(1), AXES2(2), AXES2(3) )

      ELSE

C
C        This code executes only if someone adds a new shape
C        name to SVSHAP then fails to update the SVSHP2 condition
C        block to respond to the name. Fortran needs SWITCH...CASE.
C
         CALL SETMSG ('Encountered uncoded shape ID for #. '     //
     .                'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAPE(2)                               )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFSPIN'                                 )
         RETURN

      END IF


C
C     Confirm the center of the input reference frames correspond
C     to the target bodies for non-point, non-spherical bodies.
C
C        FRAME1 centered on TARG1
C        FRAME2 centered on TARG2
C
C     This check does not apply to POINT or SPHERE shapes.
C

      IF ( (SVSHP1 .NE. POINT)  .AND. (SVSHP1 .NE. SPHERE) ) THEN

         CALL NAMFRM ( SVREF1, FCODE1 )

         CALL FRINFO ( FCODE1, CTR1, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', SVREF1                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFSPIN'                                )
            RETURN

         END IF

         IF ( SVBOD1 .NE. CTR1 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' //
     .                'target body #2 is not centered on #2. The '  //
     .                'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', SVREF1                     )
            CALL ERRCH  ( '#2', OF(1)                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZGFSPIN'                       )
            RETURN

         END IF

      END IF


      IF ( (SVSHP2 .NE. POINT)  .AND. (SVSHP2 .NE. SPHERE) ) THEN

         CALL NAMFRM ( SVREF2, FCODE2 )

         CALL FRINFO ( FCODE2, CTR2, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', SVREF2                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFSPIN'                                )
            RETURN

         END IF

         IF ( SVBOD2 .NE. CTR2 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' //
     .                'target body #2 is not centered on #2. The '  //
     .                'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', SVREF2                     )
            CALL ERRCH  ( '#2', OF(2)                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZGFSPIN'                       )
            RETURN

         END IF

      END IF

      CALL CHKOUT( 'ZZGFSPIN' )
      RETURN




C$Procedure ZZGFSPDC ( Private - GF, angular separation decreasing)

      ENTRY ZZGFSPDC ( UDFUNC, ET, DECRES )

C$ Abstract
C
C     Computes whether or not the angular separation between SVBOD1 and
C     SVBOD2 is decreasing at time ET.
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
C     None.
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
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
C     ET         I   Ephemeris seconds past J2000 TDB.
C     DECRES     O   .TRUE. if angular separation is decreasing .FALSE.
C                    otherwise.
C
C$ Detailed_Input
C
C     ET         time in seconds past J2000 at which one wishes to
C                determine whether or not the angular separation of the
C                two bodies is decreasing.
C
C$ Detailed_Output
C
C     DECRES     is .TRUE. if the angular separation between the objects
C                is decreasing.  Otherwise it is .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If the observer is inside one of the objects, the object will
C     be regarded as having a 90 degree apparent radius.
C
C$ Files
C
C     None.
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
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
C
C        Added UDFUNC to argument list for use of ZZGFRELX when
C        calculating the events.
C
C-    SPICELIB Version 1.0.1 06-JUL-2009 (NJB) (EDW)
C
C        Rename of the ZZDHA call to DHFA.
C
C-    SPICELIB Version 1.0.0 29-APR-2008 (NJB)
C
C-&

C$ Index_Entries
C
C     angular separation is decreasing
C
C-&

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN( 'ZZGFSPDC' )
      END IF

      CALL SPKEZ ( SVBOD1, ET, SVREF, SVABCR, SVOBS, PV1, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFSPDC' )
         RETURN
      END IF

      CALL SPKEZ ( SVBOD2, ET, SVREF, SVABCR, SVOBS, PV2, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFSPDC' )
         RETURN
      END IF

C
C     The angular separation between the bodies has the value
C
C        theta = sep - alpha1 - alpha2
C
C     With alpha1 the half angle of SVBOD1, alpha2 the half
C     angle of SVBOD2, half angle defined as (for spheres):
C
C        sin(alpha) = body_radius
C                     -----------
C                     range_to_body
C
C     The corresponding time derivative of theta:
C
C        d(theta) = d(sep) - d(alpha1) - d(alpha2)
C        --------   ------   ---------   ---------
C        dt         dt       dt          dt
C
C     Note, alpha1, alpha2 and their derivatives have value zero
C     for point objects.
C
      DTHETA = DVSEP(PV1, PV2)

C
C     Check for a failure caused by a numerical event.
C
      IF (  FAILED() ) THEN
         DECRES = .TRUE.
         CALL CHKOUT ( 'ZZGFSPDC' )
         RETURN
      END IF

      DTHETA = DTHETA - DHFA(PV1, SVRAD1) - DHFA(PV2, SVRAD2)

      IF ( DTHETA .LT. 0 ) THEN
         DECRES = .TRUE.
      ELSE
         DECRES = .FALSE.
      END IF

      CALL CHKOUT ( 'ZZGFSPDC' )
      RETURN




C$Procedure ZZGFSPGQ ( Private - GF, calculate angular separation )

      ENTRY ZZGFSPGQ ( ET, SEP )

C$ Abstract
C
C     Determine the angular separation between the limbs of the two
C     bodies.
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
C     None.
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     ROOT
C
C$ Declarations
C
C      DOUBLE PRECISION      ET
C      DOUBLE PRECISION      SEP
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris seconds past J2000 TDB.
C     SEP        O   Separation at time ET.
C
C$ Detailed_Input
C
C     ET         time in ephemeris seconds past J2000 when the
C                angular separation between the two bodies is
C                to be computed.
C
C$ Detailed_Output
C
C     SEP        is the angular separation between SVBOD1 and SVBOD2 as
C                seen from SVOBS at time ET.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine determines the apparent angular separation between
C     the limbs of bodies SVBOD1 and SVBOD2 as seen from SVOBS at
C     time ET.
C
C$ Examples
C
C     None.
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0 17-FEB-2011 (EDW)
C
C        Routine renamed from ZZGFGSEP to ZZGFSPGQ to match geometry
C        finder naming convention.
C
C-    SPICELIB Version 1.0.0 26-AUG-2003 (LSE)
C
C-&

C$ Index_Entries
C
C     angular separation between two bodies
C
C-&
      CALL ZZGFSPQ ( ET,
     .               SVBOD1, SVBOD2,
     .               SVRAD1, SVRAD2,
     .               SVOBS,  SVABCR, SVREF, SEP )

      RETURN




C$Procedure ZZGFSPX ( Private -- GF, retrieve ZZGFSPIN values )

      ENTRY ZZGFSPX ( XABCR, XBOD, YREF, XREF,
     .                XOBS,  XRAD, XSHP )

C$ Abstract
C
C     Retrieve values set in ZZGFSPIN.
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
C     None.
C
C$ Keywords
C
C     None.
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     XABCR      O   Saved value for
C     XBOD1      O   Saved value for
C     XBOD2      O   Saved value for
C     XREF       O   Saved value for
C     XREF1      O   Saved value for
C     XREF2      O   Saved value for
C     XOBS       O   Saved value for
C     XRAD1      O   Saved value for
C     XRAD2      O   Saved value for
C     XSHP1      O   Saved value for
C     XSHP2      O   Saved value for
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     XABCR    initialized via ZZGFSPIN.
C
C     XBOD1    initialized via ZZGFSPIN.
C
C     XBOD2    initialized via ZZGFSPIN.
C
C     XREF     initialized via ZZGFSPIN.
C
C     XREF1    initialized via ZZGFSPIN.
C
C     XREF2    initialized via ZZGFSPIN.
C
C     XOBS     initialized via ZZGFSPIN.
C
C     XRAD1    initialized via ZZGFSPIN.
C
C     XRAD2    initialized via ZZGFSPIN.
C
C     XSHP1    initialized via ZZGFSPIN.
C
C     XSHP2    initialized via ZZGFSPIN.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.0.0 24-SEP-2012 (EDW)
C
C-&

C$ Index_Entries
C
C     get saved separation angle parameters
C
C-&

      XABCR   = SVABCR
      XBOD(1) = SVBOD1
      XBOD(2) = SVBOD2
      YREF    = SVREF
      XREF(1) = SVREF1
      XREF(2) = SVREF2
      XOBS    = SVOBS
      XRAD(1) = SVRAD1
      XRAD(2) = SVRAD2
      XSHP(1) = SVSHP1
      XSHP(2) = SVSHP2

      RETURN
      END
