C$Procedure      ROTGET ( Frame get rotation )

      SUBROUTINE ROTGET ( INFRM, ET, ROTATE, OUTFRM, FOUND )

C$ Abstract
C
C     Find the rotation from a user specified frame to another frame at
C     a user specified epoch.
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
      DOUBLE PRECISION      ROTATE ( 3, 3 )
      INTEGER               OUTFRM
      LOGICAL               FOUND

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INFRM      I   The integer code for a SPICE reference frame.
C     ET         I   An epoch in seconds past J2000.
C     ROTATE     O   A rotation matrix.
C     OUTFRM     O   The frame that ROTATE transforms INFRM to.
C     FOUND      O   TRUE if a rotation can be found.
C
C$ Detailed_Input
C
C     INFRM       is the SPICE ID-code for some reference frame.
C
C     ET          is an epoch in ephemeris seconds past J2000 at which
C                 the user wishes to retrieve a rotation matrix.
C
C$ Detailed_Output
C
C     ROTATE      is a 3x3 matrix that rotates positions relative to
C                 INFRM to positions relative to OUTFRM. (Assuming such
C                 a rotation can be found.)
C
C     OUTFRM      is the SPICE ID-code of a reference frame. The 3x3
C                 matrix ROTATE rotates positions relative to INFRM to
C                 positions relative to OUTFRM. The positions
C                 transformation is achieved by multiplying ROTATE on
C                 the right by a position relative to INFRM. This is
C                 easily accomplished via the subroutine call shown
C                 below.
C
C                    CALL MXV  ( ROTATE, INPOS,  OUTPOS )
C
C     FOUND       is a logical flag indicating whether or not a
C                 rotation matrix could be found from INFRM to some
C                 other frame. If a rotation matrix cannot be found
C                 OUTFRM will be set to zero, FOUND will be set to
C                 FALSE and ROTATE will be returned as the zero matrix.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If a rotation matrix cannot be located, then FOUND will be set
C        to FALSE, OUTFRM will be set to zero and ROTATE will be set to
C        the zero 3x3 matrix.
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
C     This is a low level routine used for determining a chain of
C     position transformation matrices from one frame to another.
C
C$ Examples
C
C     See REFCHG.
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
C-    SPICELIB Version 4.0.0, 21-MAR-2014 (BVS)
C
C        To prevent operations with un-initialized DP numbers, wrapped
C        IF ( .NOT. FAILED() ) ... END IF around output matrix
C        transposition operation in the PCK frame branch where the
C        routine returning the matrix might fail.
C
C        Incremented major version token by 2 to sync up versions with
C        FRMGET.
C
C-    SPICELIB Version 2.1.0, 02-MAR-2010 (NJB)
C
C        Bug fix: frame ID rather than frame class ID
C        is now passed to dynamic frame evaluation
C        routine ZZDYNROT. Order of header sections was
C        corrected.
C
C-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
C
C        Added the new frame type 'DYN' to the list of frame
C        types recognized by ROTGET.
C
C-    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT)
C
C-&

C$ Index_Entries
C
C     Find a rotation matrix from a specified frame
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
      INTEGER               TYPE
      INTEGER               TYPEID

      DOUBLE PRECISION      TIPM  ( 3, 3 )


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

      CALL CHKIN ( 'ROTGET' )

C
C     Get all the needed information about this frame.
C
      CALL FRINFO ( INFRM, CENTER, TYPE, TYPEID, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL CLEARD ( 9, ROTATE )
         OUTFRM = 0

         CALL CHKOUT ( 'ROTGET' )
         RETURN

      END IF

C
C     FOUND was set to true by the FRINFO call. Compute rotation based
C     on the frame class.
C
      IF ( TYPE .EQ. INERTL ) THEN

         CALL IRFROT ( INFRM, 1, ROTATE )

         IF ( .NOT. FAILED() ) THEN

            OUTFRM = 1

         END IF


      ELSE IF ( TYPE .EQ. PCK ) THEN

         CALL TIPBOD ( 'J2000', TYPEID, ET, TIPM   )

         IF ( .NOT. FAILED() ) THEN

            CALL XPOSE ( TIPM, ROTATE )

            OUTFRM = 1

         END IF


      ELSE IF ( TYPE .EQ. CK ) THEN

         CALL CKFROT ( TYPEID, ET, ROTATE, OUTFRM, FOUND )


      ELSE IF ( TYPE .EQ. TK ) THEN

         CALL TKFRAM  ( TYPEID, ROTATE, OUTFRM, FOUND )


      ELSE IF ( TYPE .EQ. DYN ) THEN

C
C        Unlike the other frame classes, the dynamic frame evaluation
C        routine ZZDYNROT requires the input frame ID rather than the
C        dynamic frame class ID. ZZDYNROT also requires the center ID
C        we found via the FRINFO call.

         CALL ZZDYNROT ( INFRM, CENTER, ET, ROTATE, OUTFRM )

C
C        The FOUND flag was set by FRINFO earlier; we don't touch
C        it here. If ZZDYNROT signaled an error, FOUND will be set
C        to .FALSE. at end of this routine.
C

      ELSE

         CALL CLEARD ( 9, ROTATE )
         OUTFRM = 0
         FOUND  = .FALSE.

         CALL SETMSG ( 'The reference frame # has class id-code #. '
     .   //            'This form of reference frame is not '
     .   //            'supported in version # of ROTGET. You need '
     .   //            'to update your version of SPICELIB to the '
     .   //            'latest version in order to support this '
     .   //            'frame. ' )

         CALL ERRINT ( '#', INFRM )
         CALL ERRINT ( '#', TYPE  )
         CALL ERRCH  ( '#', VERSN )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAMETYPE)' )
         CALL CHKOUT ( 'ROTGET'                  )
         RETURN

      END IF

C
C     Make sure to clear outputs in case of a failure as defined in
C     in the header.
C
      IF ( FAILED() .OR. .NOT. FOUND ) THEN

         CALL CLEARD ( 9, ROTATE )
         OUTFRM = 0
         FOUND  = .FALSE.

      END IF


      CALL CHKOUT ( 'ROTGET' )
      RETURN
      END
