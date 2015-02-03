C$Procedure      FRMGET ( Frame get transformation )

      SUBROUTINE FRMGET ( INFRM, ET, XFORM, OUTFRM, FOUND )

C$ Abstract
C
C     Find the transformation from a user specified frame to
C     another frame at a user specified epoch.
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
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'frmtyp.inc'

      INTEGER               INFRM
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      XFORM ( 6, 6 )
      INTEGER               OUTFRM
      LOGICAL               FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INFRM      I   The integer code for a SPICE reference frame.
C     ET         I   An epoch in seconds past J2000.
C     XFORM      O   A state transformation matrix.
C     OUTFRM     O   The frame that XFORM transforms INFRM to.
C     FOUND      O   TRUE if a frame transformation can be found.
C
C$ Detailed_Input
C
C     INFRM       is the SPICE ID-code for some reference frame.
C
C     ET          is an epoch in ephemeris seconds past J2000 at
C                 which the user wishes to retrieve a state
C                 transformation matrix.
C
C$ Detailed_Output
C
C     XFORM       is a 6x6 matrix that transforms states relative to
C                 INFRM to states relative to OUTFRM. (Assuming such a
C                 transformation can be found.)
C
C     OUTFRM      is the SPICE ID-code of a reference frame. The 6x6
C                 matrix XFORM transforms states relative to INFRM to
C                 states relative to OUTFRM. The state transformation
C                 is achieved by multiplying XFORM on the right by a
C                 state relative to INFRM.  This is easily accomplished
C                 via the subroutine call shown below.
C
C                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
C
C     FOUND       is a logical flag indicating whether or not a
C                 transformation matrix could be found from INFRM to
C                 some other frame. If a transformation matrix cannot
C                 be found OUTFRM will be set to zero, FOUND will be
C                 set to FALSE and XFORM will be returned as the zero
C                 matrix.
C
C$ Parameters
C
C     See include file.
C
C$ Exceptions
C
C     1) If a transformation matrix cannot be located, then FOUND will
C        be set to FALSE, OUTFRM will be set to zero and XFORM will be
C        set to the zero 6x6 matrix.
C
C     2) If the class of the requested frame is not recognized the
C        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signaled.
C
C$ Files
C
C     LSK, SCLK, PCK, FK, SPK, and/or CK kernels may need to be loaded
C     to provide the needed frame definition and transformation data.
C
C$ Particulars
C
C     This is a low level routine used for determining a chain of state
C     transformation matrices from one frame to another.
C
C$ Examples
C
C     See FRMCHG.
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
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.0, 05-JAN-2014 (BVS)
C
C        To prevent operations with un-initialized DP numbers, wrapped
C        IF ( .NOT. FAILED() ) ... END IF around output matrix
C        transposition/re-assignment operations in all branches where
C        the routine returning the matrix might fail.
C
C        Added zero-ing out the output matrix in cases of a failed or
C        .NOT. FOUND transformation look ups.
C
C-    SPICELIB Version 3.0.0, 18-DEC-2004 (NJB)
C
C        Added the new frame type 'DYN' to the list of frame
C        types recognized by FRMGET.
C
C-    SPICELIB Version 2.0.0, 03-APR-1997 (WLT)
C
C        Added the new frame type 'TK' to the list of frame
C        types recognized by FRMGET.  In addition the routine
C        now checks FAILED after "getting" the frame transformation.
C
C-    SPICELIB Version 1.0.0, 20-OCT-1994 (WLT)
C
C
C-&

C$ Index_Entries
C
C     Find a frame transformation matrix from a specified frame
C
C-&

C
C     SPICELIB Functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED

C
C     Local Variables
C
      CHARACTER*(6)         VERSN

      INTEGER               CENTER
      INTEGER               I
      INTEGER               J
      INTEGER               TYPE
      INTEGER               TYPEID

      DOUBLE PRECISION      TSIPM  ( 6, 6 )
      DOUBLE PRECISION      ROTATE ( 3, 3 )


C
C     Set version and output flag.
C
      VERSN = '4.0.0'
      FOUND = .FALSE.

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'FRMGET' )

C
C     Get all the needed information about this frame.
C
      CALL FRINFO ( INFRM, CENTER, TYPE, TYPEID, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL CLEARD ( 36, XFORM )
         OUTFRM = 0

         CALL CHKOUT ( 'FRMGET' )
         RETURN

      END IF

C
C     FOUND was set to true by the FRINFO call. Compute transformation
C     based on the frame class.
C
      IF ( TYPE .EQ. INERTL ) THEN

         CALL IRFROT ( INFRM, 1, ROTATE )

         IF ( .NOT. FAILED() ) THEN

            DO I = 1,3
               DO J = 1,3
                  XFORM(I,  J  ) = ROTATE(I,J)
                  XFORM(I+3,J+3) = ROTATE(I,J)
                  XFORM(I+3,J  ) = 0.0D0
                  XFORM(I,  J+3) = 0.0D0
               END DO
            END DO

            OUTFRM = 1

         END IF


      ELSE IF ( TYPE .EQ. PCK ) THEN

         CALL TISBOD ( 'J2000', TYPEID, ET, TSIPM  )

         IF ( .NOT. FAILED() ) THEN

            CALL INVSTM ( TSIPM, XFORM )

            OUTFRM = 1

         END IF


      ELSE IF ( TYPE .EQ. CK ) THEN

         CALL CKFXFM ( TYPEID, ET, XFORM, OUTFRM, FOUND )


      ELSE IF ( TYPE .EQ. TK ) THEN

         CALL TKFRAM  ( TYPEID, ROTATE, OUTFRM, FOUND )

         IF ( .NOT. FAILED() ) THEN

            DO I = 1,3
               DO J = 1,3
                  XFORM(I,  J  ) = ROTATE(I,J)
                  XFORM(I+3,J+3) = ROTATE(I,J)
                  XFORM(I+3,J  ) = 0.0D0
                  XFORM(I,  J+3) = 0.0D0
               END DO
            END DO

         END IF


      ELSE IF ( TYPE .EQ. DYN ) THEN

C
C        Unlike the other frame classes, the dynamic frame evaluation
C        routine ZZDYNFRM requires the input frame ID rather than the
C        dynamic frame class ID. ZZDYNFRM also requires the center ID
C        we found via the FRINFO call.

         CALL ZZDYNFRM ( INFRM, CENTER, ET, XFORM, OUTFRM )

C
C        The FOUND flag was set by FRINFO earlier; we don't touch
C        it here. If ZZDYNFRM signaled an error, FOUND will be set
C        to .FALSE. at end of this routine.
C

      ELSE

         CALL CLEARD ( 36, XFORM )
         OUTFRM = 0
         FOUND  = .FALSE.

         CALL SETMSG ( 'The reference frame # has class id-code #. '
     .   //            'This form of reference frame is not '
     .   //            'supported in version # of FRMGET. You need '
     .   //            'to update your version of SPICELIB to the '
     .   //            'latest version in order to support this '
     .   //            'frame. ' )

         CALL ERRINT ( '#', INFRM )
         CALL ERRINT ( '#', TYPE  )
         CALL ERRCH  ( '#', VERSN )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAMETYPE)' )
         CALL CHKOUT ( 'FRMGET'                  )
         RETURN

      END IF

C
C     Make sure to clear outputs in case of a failure as defined in
C     in the header.
C
      IF ( FAILED() .OR. .NOT. FOUND ) THEN

         CALL CLEARD ( 36, XFORM )
         OUTFRM = 0
         FOUND  = .FALSE.

      END IF

      CALL CHKOUT ( 'FRMGET' )
      RETURN
      END
