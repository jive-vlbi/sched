C$Procedure      ZZFRMGT1 (Frame get transformation)
 
      SUBROUTINE ZZFRMGT1 ( INFRM, ET, XFORM, OUTFRM, FOUND )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
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
C     INFRM       is the SPICE id-code for some reference frame.
C
C     ET          is an epoch in ephemeris seconds past J2000 at
C                 which the user wishes to retrieve a state
C                 transformation matrix.
C
C$ Detailed_Output
C
C     XFORM       is a 6x6 matrix that transforms states relative to
C                 INFRM to states relative to OUTFRM.  (Assuming such
C                 a transformation can be found.)
C
C     OUTFRM      is a reference frame.  The 6x6 matrix XFORM transforms
C                 states relative to INFRM to states relative to OUTFRM.
C                 The state transformation is achieved by multiplying
C                 XFORM on the right by a state relative to INFRM.  This
C                 is easily accomplished via the subroutine call
C                 shown below.
C
C                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE )
C
C     FOUND       is a logical flag indicating whether or not a
C                 transformation matrix could be found from INFRM
C                 to some other frame.  If a transformation matrix
C                 cannot be found OUTFRM will be set to zero, FOUND
C                 will be set to FALSE and XFORM will be returned
C                 as the zero matrix.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If a transformation matrix cannot be located, then
C        FOUND will be set to FALSE, OUTFRM will be set to zero
C        and XFORM will be set to the zero 6x6 matrix.
C
C     2) If the class of the requested frame is not recognized the
C        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signalled.
C
C        of this routine.
C
C     3) If the reference frame REF is dynamic, the error
C        SPICE(RECURSIONTOODEEP) will be signaled.
C
C
C$ Particulars
C
C     This is a low level routine used for determining a chain
C     of state transformation matrices from one frame to another.
C
C$ Examples
C
C     See FRMCHG.
C
C$ Restrictions
C
C     1) SPICE Private routine.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-DEC-2004 (NJB)
C
C        Based on SPICELIB Version 3.0.0, 21-JUN-2004 (NJB)
C
C-&
 
C$ Index_Entries
C
C     Find a frame transformation matrix from a specified frame
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
 
      INTEGER               CENT
      INTEGER               I
      INTEGER               J
      INTEGER               TYPE
      INTEGER               TYPEID
 
      DOUBLE PRECISION      TSIPM ( 6, 6 )
      DOUBLE PRECISION      ROT   ( 3, 3 )
 
 
 
 
 
      VERSN = '2.0.0'
      FOUND = .FALSE.
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZFRMGT1' )

C
C     Get all the needed information about this frame.
C
      CALL FRINFO ( INFRM, CENT, TYPE, TYPEID, FOUND )
 
      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'ZZFRMGT1' )
         RETURN
      END IF
 
 
 
      IF ( TYPE .EQ. PCK ) THEN
 
         CALL TISBOD ( 'J2000', TYPEID, ET, TSIPM  )
         CALL INVSTM (  TSIPM,              XFORM  )
         CALL NAMFRM ( 'J2000',             OUTFRM )
 
      ELSE IF ( TYPE .EQ. INERTL ) THEN
 
         CALL IRFROT ( INFRM, 1, ROT )
 
         DO I = 1,3
            DO J = 1,3
 
               XFORM(I,  J  ) = ROT(I,J)
               XFORM(I+3,J+3) = ROT(I,J)
               XFORM(I+3,J  ) = 0.0D0
               XFORM(I,  J+3) = 0.0D0
            END DO
         END DO
 
         OUTFRM = 1
 
      ELSE IF ( TYPE .EQ. CK ) THEN
 
         CALL CKFXFM ( TYPEID, ET, XFORM, OUTFRM, FOUND )
 
 
      ELSE IF ( TYPE .EQ. TK ) THEN
 
         CALL TKFRAM  ( TYPEID, ROT, OUTFRM, FOUND )
 
         DO I = 1,3
            DO J = 1,3
               XFORM(I,  J  ) = ROT(I,J)
               XFORM(I+3,J+3) = ROT(I,J)
               XFORM(I+3,J  ) = 0.0D0
               XFORM(I,  J+3) = 0.0D0
            END DO
         END DO


      ELSE IF ( TYPE .EQ. DYN ) THEN
 
         CALL SETMSG ( 'The reference frame # is a dynamic frame. '
     .   //            'Dynamic frames may not be used at '
     .   //            'recursion level 1.'                      ) 
         CALL ERRINT ( '#', INFRM                                )
         CALL SIGERR ( 'SPICE(RECURSIONTOODEEP)'                 )
         CALL CHKOUT ( 'ZZFRMGT1'                                )
         RETURN

      ELSE
 
         CALL SETMSG('The reference frame # has class id-code #. '
     .   //          'This form of reference frame is not '
     .   //          'supported in version # of ZZFRMGT1. You need '
     .   //          'to update your version of SPICELIB to the '
     .   //          'latest version in order to support this '
     .   //          'frame. ' )
 
         CALL ERRINT ( '#', INFRM )
         CALL ERRINT ( '#', TYPE  )
         CALL ERRCH  ( '#', VERSN )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAMETYPE)' )
         CALL CHKOUT ( 'ZZFRMGT1'                  )
         RETURN
 
      END IF
 
      IF ( FAILED() ) THEN
 
         FOUND = .FALSE.
 
      END IF
  
      CALL CHKOUT ( 'ZZFRMGT1' )
      RETURN
      END
