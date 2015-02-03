C$Procedure      ZZROTGT1 (Frame get transformation)
 
      SUBROUTINE ZZROTGT1 ( INFRM, ET, ROTATE, OUTFRM, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Find the rotation from a user specified frame to
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
C     None.
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
C     INFRM       is the SPICE id-code for some reference frame.
C
C     ET          is an epoch in ephemeris seconds past J2000 at
C                 which the user wishes to retrieve a transformation
C                 matrix.
C
C$ Detailed_Output
C
C     ROTATE      is a 3x3 matrix that transforms positions relative to
C                 INFRM to positions relative to OUTFRM.  (Assuming such
C                 a rotation can be found.)
C
C     OUTFRM      is a reference frame.  The 3x3 matrix ROTATE rotates
C                 positions relative to INFRM to positions relative
C                 to OUTFRM.
C                 The positions transformation is achieved by
C                 multiplying
C                 ROTATE on the right by a position relative to INFRM.
C                 This
C                 is easily accomplished via the subroutine call
C                 shown below.
C
C                    CALL MXV  ( ROTATE, INPOS,  OUTPOS )
C
C     FOUND       is a logical flag indicating whether or not a
C                 rotation matrix could be found from INFRM
C                 to some other frame.  If a rotation matrix
C                 cannot be found OUTFRM will be set to zero, FOUND
C                 will be set to FALSE and ROTATE will be returned
C                 as the zero matrix.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If a rotation matrix cannot be located, then
C        FOUND will be set to FALSE, OUTFRM will be set to zero
C        and ROTATE will be set to the zero 3x3 matrix.
C
C     2) If the class of the requested frame is not recognized the
C        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signalled.
C
C     3) If the reference frame REF is dynamic, the error
C        SPICE(RECURSIONTOODEEP) will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a low level routine used for determining a chain of
C     position transformation matrices from one frame to another.
C
C$ Examples
C
C     See FRMCHG.
C
C$ Restrictions
C
C     1) SPICE Private routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 02-MAR-2010 (NJB)
C
C        Order of header sections was corrected.
C
C-    SPICELIB Version 1.0.0, 12-DEC-2004 (NJB)
C
C       Based on SPICELIB Version 2.0.0, 21-JUN-2004 (NJB)
C
C-&
 
C$ Index_Entries
C
C     Find a rotation matrix from a specified frame
C
C-&
 
C
C     Spicelib Functions
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
      INTEGER               I
      INTEGER               J
 
      DOUBLE PRECISION      TIPM  ( 3, 3 )
 
 
 
 
 
      VERSN = '1.0.0'
      FOUND = .FALSE.
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZROTGT1' )
C
C     Get all the needed information about this frame.
C
      CALL FRINFO ( INFRM, CENTER, TYPE, TYPEID, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         DO I = 1,3
            DO J = 1,3
               ROTATE(I,J) = 0.0D0
            END DO
         END DO
 
         CALL CHKOUT ( 'ZZROTGT1' )
         RETURN
      END IF
 
 
 
      IF ( TYPE .EQ. INERTL ) THEN
 
         CALL IRFROT ( INFRM, 1, ROTATE )
         FOUND  = .TRUE.
         OUTFRM = 1
 
      ELSE IF ( TYPE .EQ. PCK ) THEN
 
         CALL TIPBOD ( 'J2000', TYPEID, ET, TIPM   )
         CALL XPOSE  (  TIPM,               ROTATE )
         CALL NAMFRM ( 'J2000',             OUTFRM )
 
         FOUND = .NOT. FAILED()
 
      ELSE IF ( TYPE .EQ. CK ) THEN
 
         CALL CKFROT ( TYPEID, ET, ROTATE, OUTFRM, FOUND )
 
      ELSE IF ( TYPE .EQ. TK ) THEN
 
         CALL TKFRAM  ( TYPEID, ROTATE, OUTFRM, FOUND )
 
      ELSE IF ( TYPE .EQ. DYN ) THEN
 
         CALL SETMSG ( 'The reference frame # is a dynamic frame. '
     .   //            'Dynamic frames may not be used at '
     .   //            'recursion level 1.'                      ) 
         CALL ERRINT ( '#', INFRM                                )
         CALL SIGERR ( 'SPICE(RECURSIONTOODEEP)'                 )
         CALL CHKOUT ( 'ZZROTGT1'                                )
         RETURN
 
      ELSE
 
         CALL SETMSG('The reference frame # has class id-code #. '
     .   //          'This form of reference frame is not '
     .   //          'supported in version # of ZZROTGT1. You need '
     .   //          'to update your version of SPICELIB to the '
     .   //          'latest version in order to support this '
     .   //          'frame. ' )
 
         CALL ERRINT ( '#', INFRM )
         CALL ERRINT ( '#', TYPE  )
         CALL ERRCH  ( '#', VERSN )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAMETYPE)' )
         CALL CHKOUT ( 'ZZROTGT1'                  )
         RETURN
 
      END IF
 
      IF ( FAILED() .OR. .NOT. FOUND ) THEN
 
         DO I = 1,3
            DO J = 1,3
               ROTATE(I,J) = 0.0D0
            END DO
         END DO
 
         FOUND = .FALSE.
 
      END IF
 
 
      CALL CHKOUT ( 'ZZROTGT1' )
      RETURN
      END
