C$Procedure      ZZREFCH0 (Reference frame Change)
 
      SUBROUTINE ZZREFCH0 ( FRAME1, FRAME2, ET, ROTATE )
 
C$ Abstract
C
C     Return the transformation matrix from one
C     frame to another.
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

      INCLUDE              'errhnd.inc'
      INCLUDE              'frmtyp.inc'

      INTEGER               FRAME1
      INTEGER               FRAME2
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROTATE ( 3, 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FRAME1     I   the frame id-code for some reference frame
C     FRAME2     I   the frame id-code for some reference frame
C     ET         I   an epoch in TDB seconds past J2000.
C     ROTATE     O   a rotation matrix
C
C$ Detailed_Input
C
C     FRAME1      is the frame id-code in which some positions
C                 are known.
C
C     FRAME2      is the frame id-code for some frame in which you
C                 would like to represent positions.
C
C     ET          is the epoch at which to compute the transformation
C                 matrix.  This epoch should be in TDB seconds past
C                 the ephemeris epoch of J2000.
C
C$ Detailed_Output
C
C     ROTATE      is a 3 x 3 rotaion matrix that can be used to
C                 transform positions relative to the frame
C                 correspsonding to frame FRAME2 to positions relative
C                 to the frame FRAME2.  More explicitely, if POS is
C                 the position of some object relative to the
C                 reference frame of FRAME1 then POS2 is the position
C                 of the same object relative to FRAME2 where POS2 is
C                 computed via the subroutine call below
C
C                    CALL MXV ( ROTATE, POS, POS2 )
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If either of the reference frames is unrecognized, the error
C        SPICE(UNKNOWNFRAME) will be signalled.
C
C     2) If the auxillary information needed to compute a non-inertial
C        frame is not available an error will be diagnosed and signalled
C        by a routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to compute the rotation matrix
C     between two reference frames.
C
C
C$ Examples
C
C     Suppose that you have a position POS1 at epoch ET
C     relative to  FRAME1 and wish to determine its representation
C     POS2 relative to FRAME2.  The following subroutine calls
C     would suffice to make this rotation.
C
C        CALL REFCHG ( FRAME1, FRAME2, ET,   ROTATE )
C        CALL MXV    ( ROTATE, POS1,   POS2 )
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 14-DEC-2008 (NJB)
C
C        Upgraded long error message associated with frame
C        connection failure.
C
C-    SPICELIB Version 1.2.0, 26-APR-2004 (NJB)
C
C        Another typo was corrected in the long error message, and
C        in a comment.
C
C-    SPICELIB Version 1.1.0, 23-MAY-2000 (WLT)
C
C        A typo was corrected in the long error message.
C
C-    SPICELIB Version 1.0.0, 9-JUL-1998 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Rotate positions from one frame to another
C
C-&
 
C
C     SPICE functions
C
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Paramters
C
      INTEGER               MAXNOD
      PARAMETER           ( MAXNOD = 10 )
 
      INTEGER               MAXXFM
      PARAMETER           ( MAXXFM = MAXNOD + 4 )

C
C     The root of all reference frames is J2000 (Frame ID = 1).
C
      INTEGER               ROOTF
      PARAMETER           ( ROOTF = 1 )
 
C
C     Local Variables
C
      CHARACTER*(LMSGLN)    ERRMSG
 
C
C     ROT contains the rotations from FRAME1 to FRAME2
C     ROT(1...3,1...3,I) has the rotation from FRAME(I)
C     to FRAME(I+1).  We make extra room in ROT because we
C     plan to add rotations beyond the obvious chain from
C     FRAME1 to a root node.
C
      DOUBLE PRECISION      ROT ( 3, 3, MAXXFM )

C
C     ROT2 is used to store intermediate rotation from
C     FRAME2 to some node in the chain from FRAME1 to PCK or
C     INERTL frames.
C 
      DOUBLE PRECISION      ROT2  ( 3, 3, 2     )
      DOUBLE PRECISION      TMPROT( 3, 3        )

      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CMNODE
      INTEGER               GET
C
C     FRAME contains the frames we transform from in going from
C     FRAME1 to FRAME2.  FRAME(1) = FRAME1 by  construction.
C
      INTEGER               FRAME ( MAXNOD )
      INTEGER               I
      INTEGER               INC
      INTEGER               J
C
C     NODE counts the number of rotations needed to go
C     from FRAME1 to FRAME2.
C
      INTEGER               NODE
      INTEGER               PUT
      INTEGER               RELTO
      INTEGER               THIS

      LOGICAL               DONE
      LOGICAL               FOUND
      LOGICAL               GOTONE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZREFCH0')
C
C     Do the obvious thing first.  If FRAME1 and FRAME2 are the
C     same then we simply return the identity matrix.
C
      IF ( FRAME1 .EQ. FRAME2 ) THEN
 
         CALL IDENT  ( ROTATE )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN
 
      END IF
C
C     Now perform the obvious check to make sure that both
C     frames are recognized.
C
      CALL FRINFO ( FRAME1, CENT, CLASS, CLSSID, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The number # is not a recognized id-code '
     .   //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME1 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN
 
      END IF
 
 
      CALL FRINFO ( FRAME2, CENT, CLASS, CLSSID, FOUND )
 
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'The number # is not a recognized id-code '
     .   //            'for a reference frame. ' )
         CALL ERRINT ( '#', FRAME2 )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'  )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN
 
      END IF
 
 
 
      NODE        = 1
      FRAME(NODE) = FRAME1
      FOUND       = .TRUE.
C
C     Follow the chain of rotations until we run into
C     one that rotates to J2000 (frame id = 1) or we hit FRAME2.
C
      DO WHILE (       FRAME(NODE) .NE. ROOTF
     .           .AND. NODE        .LT. MAXNOD
     .           .AND. FRAME(NODE) .NE. FRAME2
     .           .AND. FOUND   )
C
C        Find out what rotation is available for this
C        frame.
C
         CALL ZZROTGT0 ( FRAME(NODE),   ET,
     .                   ROT(1,1,NODE), FRAME(NODE+1), FOUND  )
 
         IF ( FOUND ) THEN
C
C           We found a rotation matrix.  ROT(1,1,NODE)
C           now contains the rotation from FRAME(NODE)
C           to FRAME(NODE+1).  We need to look up the information
C           for the next NODE.
C
            NODE = NODE + 1
 
         END IF
 
      END DO
 
      DONE =      FRAME(NODE) .EQ. ROOTF
     .       .OR. FRAME(NODE) .EQ. FRAME2
     .       .OR. .NOT. FOUND
 
 
      DO WHILE ( .NOT. DONE )
C
C        The only way to get to this point is to have run out of
C        room in the array of reference frame rotation
C        buffers.  We will now build the rotation from
C        the previous NODE to whatever the next node in the
C        chain is.  We'll do this until we get to one of the
C        root classes or we run into FRAME2.
C
         CALL ZZROTGT0 ( FRAME(NODE), ET, ROT(1,1,NODE), RELTO, FOUND )
 
 
         IF ( FOUND  ) THEN
C
C           Recall that ROT(1,1,NODE-1) contains the rotation
C           from FRAME(NODE-1) to FRAME(NODE).  We are going to replace
C           FRAME(NODE) with the frame indicated by RELTO.  This means
C           that ROT(1,1,NODE-1) should be replaced with the
C           rotation from FRAME(NODE) to RELTO.
C
            FRAME(NODE) = RELTO
            CALL ZZRXR ( ROT(1,1,NODE-1), 2, TMPROT )
 
            DO I = 1,3
               DO J = 1,3
                  ROT(I,J,NODE-1) = TMPROT(I,J)
               END DO
            END DO
 
         END IF
C
C        We are done if the class of the last frame is J2000
C        or if the last frame is FRAME2 or if we simply couldn't get
C        another rotation.
C
         DONE =        FRAME(NODE) .EQ. ROOTF
     .            .OR. FRAME(NODE) .EQ. FRAME2
     .            .OR. .NOT. FOUND
 
      END DO
 
C
C     Right now we have the following situation.  We have in hand
C     a collection of rotations between frames. (Assuming
C     that is that NODE .GT. 1.  If NODE .EQ. 1 then we have
C     no rotations computed yet.
C
C
C     ROT(1...3, 1...3, 1    )    rotates FRAME1   to FRAME(2)
C     ROT(1...3, 1...3, 2    )    rotates FRAME(2) to FRAME(3)
C     ROT(1...3, 1...3, 3    )    rotates FRAME(3) to FRAME(4)
C        .
C        .
C        .
C     ROT(1...3, 1...3, NODE-1 )  rotates FRAME(NODE-1)
C                                   to         FRAME(NODE)
C
C
C     One of the following situations is true.
C
C     1)  FRAME(NODE) is the root of all frames, J2000.
C
C     2)  FRAME(NODE) is the same as FRAME2
C
C     3)  There is no rotation from FRAME(NODE) to another
C         more fundamental frame.  The chain of rotations
C         from FRAME1 stops at FRAME(NODE).  This means that the
C         "frame atlas" is incomplete because we can't get to the
C         root frame.
C
C     We now have to do essentially the same thing for FRAME2.
C
 
      IF ( FRAME(NODE) .EQ. FRAME2 ) THEN
C
C        We can handle this one immediately with the private routine
C        ZZRXR which multiplies a series of matrices.
C
         CALL ZZRXR (  ROT, NODE-1, ROTATE )
         CALL CHKOUT ( 'ZZREFCH0' )
         RETURN
 
      END IF
 
C
C     We didn't luck out above.  So we follow the chain of
C     rotation for FRAME2.  Note that at the moment the
C     chain of rotations from FRAME2 to other frames
C     does not share a node in the chain for FRAME1.
C    ( GOTONE = .FALSE. ) .
C
      THIS   =  FRAME2
      GOTONE = .FALSE.
 
C
C     First see if there is any chain to follow.
C
      DONE   =  THIS .EQ. ROOTF
 
C
C     Set up the matrices ROT2(,,1) and ROT(,,2)  and set up
C     PUT and GET pointers so that we know where to GET the partial
C     rotation from and where to PUT partial results.
C
      IF ( .NOT. DONE ) THEN
 
         PUT = 1
         GET = 1
         INC = 1
 
      END IF
 
C
C     Follow the chain of rotations until we run into
C     one that rotates to the root frame or we land in the
C     chain of nodes for FRAME1.
C
C     Note that this time we will simply keep track of the full
C     rotation from FRAME2 to the last node.
C
      DO WHILE (  .NOT. DONE )
C
C        Find out what rotation is available for this
C        frame.
C
         IF ( THIS  .EQ. FRAME2 ) THEN
C
C           This is the first pass, just put the rotation
C           directly into ROT2(,,PUT).
C
            CALL ZZROTGT0 ( THIS,  ET, ROT2(1,1,PUT), RELTO, FOUND  )
 
 
            IF ( FOUND ) THEN
               THIS   = RELTO
               GET    = PUT
               PUT    = PUT + INC
               INC    =     - INC
               CMNODE = ISRCHI ( THIS, NODE, FRAME )
               GOTONE = CMNODE .GT. 0
            END IF
 
         ELSE
C
C           Fetch the rotation into a temporary spot TMPROT
C
            CALL ZZROTGT0 ( THIS,  ET, TMPROT, RELTO,  FOUND  )
 
            IF ( FOUND ) THEN
C
C              Next multiply TMPROT on the right by the last partial
C              product (in ROT2(,,GET) ).  We do this in line.
C
               DO I=1,3
                  DO J=1,3
 
                     ROT2(I,J,PUT) = TMPROT(I,1)*ROT2(1,J,GET)
     .                             + TMPROT(I,2)*ROT2(2,J,GET)
     .                             + TMPROT(I,3)*ROT2(3,J,GET)
                  END DO
               END DO
 
 
 
C
C              Adjust GET and PUT so that GET points to the slots
C              where we just stored the result of our multiply and
C              so that PUT points to the next available storage
C              locations.
C
               GET    = PUT
               PUT    = PUT + INC
               INC    =     - INC
               THIS   = RELTO
 
               CMNODE = ISRCHI ( THIS, NODE, FRAME )
               GOTONE = CMNODE .GT. 0
 
            END IF
 
         END IF
 
C
C        See if we have a common node and determine whether or not
C        we are done with this loop.
C
         DONE   =       THIS  .EQ. ROOTF
     .            .OR.  GOTONE
     .            .OR. .NOT. FOUND
 
 
      END DO
 
C
C     There are two possible scenarios.  Either the chain of
C     rotations from FRAME2 ran into a node in the chain for
C     FRAME1 or it didn't.  (The common node might very well be
C     the root node.)  If we didn't run into a common one, then
C     the two chains don't intersect and there is no way to
C     get from FRAME1 to FRAME2.
C
      IF ( .NOT. GOTONE ) THEN
 
         CALL ZZNOFCON ( ET, FRAME1, FRAME(NODE), FRAME2, THIS, ERRMSG )

         IF ( FAILED() ) THEN
C
C           We were unable to create the error message. This
C           unfortunate situation could arise if a frame kernel 
C           is corrupted.
C
            CALL CHKOUT ( 'ZZREFCH0' )
            RETURN

         END IF
C
C        The normal case: signal an error with a descriptive long
C        error message.
C 
         CALL SETMSG ( ERRMSG                  )
         CALL SIGERR ( 'SPICE(NOFRAMECONNECT)' )
         CALL CHKOUT ( 'ZZREFCH0'              )
         RETURN
 
      END IF
 
C
C     Recall that we have the following.
C
C     ROT(1...3, 1...3, 1    )    rotates FRAME(1) to FRAME(2)
C     ROT(1...3, 1...3, 2    )    rotates FRAME(2) to FRAME(3)
C     ROT(1...3, 1...3, 3    )    rotates FRAME(3) to FRAME(4)
C
C     ROT(1...3, 1...3, CMNODE-1) rotates FRAME(CMNODE-1)
C                                   to         FRAME(CMNODE)
C
C     and that ROT2(1,1,GET) rotates from FRAME2 to CMNODE.
C     Hence the inverse of ROT2(1,1,GET) rotates from CMNODE
C     to FRAME2.
C
C     If we compute the inverse of ROT2 and store it in
C     the next available slot of ROT (.i.e. ROT(1,1,CMNODE)
C     we can simply apply our custom routine that multiplies a
C     sequence of rotation matrices together to get the
C     result from FRAME1 to FRAME2.
C
      CALL XPOSE ( ROT2(1,1,GET), ROT(1,1,CMNODE) )
      CALL ZZRXR ( ROT,   CMNODE, ROTATE          )
 
      CALL CHKOUT ( 'ZZREFCH0' )
      RETURN
 
      END
