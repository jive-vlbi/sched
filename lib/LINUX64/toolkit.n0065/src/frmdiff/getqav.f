C$Procedure      GETQAV ( Compute/not-compute Q and AV )

      SUBROUTINE GETQAV( CHECK, FFRAME, TFRAME, EPOCH, N,
     .                   AVFLG, Q, AV, OK, ERROR )

C$ Abstract
C
C     This routine either computes attitude quaternions and,
C     optionally, angular velocities for given ``from'' and ``to''
C     frames at given epochs or verifies that they cannot be computed.
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

      IMPLICIT NONE

      INCLUDE               'errhnd.inc'

      LOGICAL               CHECK
      CHARACTER*(*)         FFRAME
      CHARACTER*(*)         TFRAME
      DOUBLE PRECISION      EPOCH  ( * )
      INTEGER               N
      LOGICAL               AVFLG
      DOUBLE PRECISION      Q      ( 4, * )
      DOUBLE PRECISION      AV     ( 3, * )
      LOGICAL               OK
      CHARACTER*(*)         ERROR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CHECK      I   Run mode: .TRUE. for check, .FALSE. for compute
C     FFRAME     I   Name of the ``from'' frame.
C     TFRAME     I   Name of the ``to'' frame.
C     EPOCH      I   Buffer of epochs (ETs).
C     N          I   Number of epochs.
C     AVFLG      I   AV flag: .TRUE. to compute, .FALSE. to not compute
C     Q          O   Output quaternion buffer
C     AV         O   Output AV buffer
C     OK         O   Success flag
C     ERROR      O   Error message
C
C$ Detailed_Input
C
C     CHECK       is the run mode: .TRUE. for check, .FALSE. for
C                 compute.
C
C     FFRAME      is the name of the ``from'' frame.
C
C     TFRAME      is the name of the ``to'' frame.
C
C     EPOCH       is the buffer of epochs (ETs).
C
C     N           is the number of epochs.
C
C     AVFLG       is the angular velocity flag: .TRUE. for computing
C                 AVs, .FALSE. for not computing AVs.
C
C$ Detailed_Output
C
C     Q           is the array of output quaternions rotating from the
C                 ``from'' frame to the ``to'' frames at each of the
C                 epochs. Undefined if routine is run in check mode.
C
C     AV          is the array of output angular velocities of the
C                 ``to'' frame w.r.t. the ``from'' frame, in the
C                 ``from'' frame.
C
C     OK          is the success flag: .TRUE. for success, .FALSE. for
C                 failure.
C
C     ERROR       is the error message. Blank if OK is .TRUE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If any of the transformations successfully computed by PXFORM
C        or SXFORM not a rotation and for this reason cannot be
C        converted to a quaternion by M2Q, the error
C        SPICE(NOTAROTATION) is signaled.
C
C     The routine uses error/report mode RETURN/NONE and returns
C     success/failure flag and error message as outputs.
C
C$ Files
C
C     All applicable kernels must be loaded prior to calling this
C     routine.
C
C$ Particulars
C
C     This routine resets error handling to RETURN and error report to
C     NONE and attempts to compute a 3x3 or 6x6 rotation from the
C     ``from'' frame to the ``to'' frame using PXFORM or SXFORM for
C     each of the input epochs.
C
C     If this routine is called in check mode, it verifies that this
C     attempt failed for all epochs. If PXFORM/SXFORM call succeeded for
C     even one point, the routine returns failure status and
C     description of the error.
C
C     If this routine is called in compute mode, it verifies that this
C     attempt succeeded for all epochs. If yes, it returns with success
C     status and Q and AV buffers filled with rotations and AVs. If
C     PXFORM/SXFORM did not succeed for even one point, the routine
C     returns failure status and description of the error.
C
C     In either case, before returning the routine resets error handing
C     mode and report type to what they were before the call.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     All applicable kernels must loaded prior to calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    Version 1.1.0, 25-MAR-2014 (BVS)
C
C        BUG FIX: added an error signal for non-rotation cases.
C
C        Included 'errhnd.inc' and used LMSGLN to declare LONGMS.
C
C-    Version 1.0.0, 22-AUG-2008 (BVS)
C
C-&

C
C     Local parameters.
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

C
C     Local variables.
C
      CHARACTER*(LINLEN)    SAVACT
      CHARACTER*(LINLEN)    SAVRPT
      CHARACTER*(LINLEN)    TMPSTR
      CHARACTER*(LMSGLN)    LONGMS

      DOUBLE PRECISION      PM    ( 3, 3 )
      DOUBLE PRECISION      XM    ( 6, 6 )

      INTEGER               I

C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     SPICELIB functions.
C
      LOGICAL               RETURN
      LOGICAL               FAILED

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETQAV' )
      END IF

C
C     Save previous error and report modes and reset them to RETURN and
C     NONE.
C
      CALL ERRACT ( 'GET', SAVACT   )
      CALL ERRACT ( 'SET', 'RETURN' )

      CALL ERRPRT ( 'GET', SAVRPT   )
      CALL ERRPRT ( 'SET', 'NONE'   )

C
C     Loop over epochs and compute rotation for each of them.
C
      OK = .TRUE.
      ERROR = ' '
      I = 1

      DO WHILE ( I .LE. N .AND. OK )

C
C        Try to compute rotation.
C
         IF ( AVFLG ) THEN
            CALL SXFORM( FFRAME, TFRAME, EPOCH(I), XM )
         ELSE
            CALL PXFORM( FFRAME, TFRAME, EPOCH(I), PM )
         END IF

C
C        Did SXFORM/PXFORM fail or succeed? Are we checking or
C        computing?
C
         IF      (       CHECK .AND. .NOT. FAILED() ) THEN

C
C           Bad: we are checking and it did not fail. Set OK flag to
C           failure and put together an error description.
C
            OK = .FALSE.

            ERROR = '# transformation from ''#'' frame to ''#'' ' //
     .              'frame could be computed at ET #'
            IF ( AVFLG ) THEN
               CALL REPMC ( ERROR, '#', 'State (6x6)', ERROR )
            ELSE
               CALL REPMC ( ERROR, '#', 'Position (3x3)', ERROR )
            END IF
            CALL REPMC ( ERROR, '#', FFRAME, ERROR )
            CALL REPMC ( ERROR, '#', TFRAME, ERROR )
            CALL REPMF ( ERROR, '#', EPOCH(I), 14, 'E', ERROR )


         ELSE IF ( .NOT. CHECK .AND.       FAILED() ) THEN

C
C           Bad: we are computing and it failed. Set OK flag to failure
C           and put together an error description. Reset error handling.
C
            OK = .FALSE.

            ERROR = '# transformation from ''#'' frame to ''#'' ' //
     .              'frame could not be computed at ET #. # '     //
     .              'error was: #'
            IF ( AVFLG ) THEN
               CALL REPMC ( ERROR, '#', 'State (6x6)', ERROR )
            ELSE
               CALL REPMC ( ERROR, '#', 'Position (3x3)', ERROR )
            END IF
            CALL REPMC ( ERROR, '#', FFRAME, ERROR )
            CALL REPMC ( ERROR, '#', TFRAME, ERROR )
            CALL REPMF ( ERROR, '#', EPOCH(I), 14, 'E', ERROR )
            IF ( AVFLG ) THEN
               CALL REPMC ( ERROR, '#', 'SXFORM', ERROR )
            ELSE
               CALL REPMC ( ERROR, '#', 'PXFORM', ERROR )
            END IF
            CALL GETMSG( 'LONG', LONGMS )
            CALL REPMC ( ERROR, '#', LONGMS, ERROR )

            CALL RESET()


         ELSE IF (       CHECK .AND.       FAILED() ) THEN

C
C           Good: we are checking and it failed. Reset error handling
C           and move on to the next epoch.
C
            CALL RESET()


         ELSE IF ( .NOT. CHECK .AND. .NOT. FAILED() ) THEN

C
C           Good: we are computing and it did not fail. Compute and
C           buffer quaternion and AV and move on to the next epoch.
C
            IF ( AVFLG ) THEN

C
C              Decompose 6x6 matrix into 3x3 matrix and AV.
C
               CALL XF2RAV ( XM, PM, AV( 1, I ) )

            END IF

C
C           Buffer quaternion.
C
            CALL M2Q( PM, Q( 1, I ) )

C
C           M2Q will fail if PM is not a rotation. In such case all we
C           can do is to signal an error and stop the program.
C
            IF ( FAILED() ) THEN

C
C              Don't not save long error message because the only error
C              that M2Q can generate is SPICE(NOTAROTATION). Reset
C              error handling and signal an error.
C
               CALL RESET  ( )
               CALL ERRACT ( 'SET',  SAVACT )
               CALL ERRPRT ( 'SET',  SAVRPT )

               CALL SETMSG ( '3x3 transformation from ''#'' frame ' //
     .                       'to ''#'' frame at ET # is not a '     //
     .                       'rotation. SPICE may compute a '       //
     .                       'non-rotation transformation, for '    //
     .                       'example, when one of the frames '     //
     .                       'in the chain is a left-handed frame ' //
     .                       'or when the orientation of one of '   //
     .                       'the frames in the chain is computed ' //
     .                       'using bad kernel data. FRMDIFF '      //
     .                       'cannot handle such non-rotation '     //
     .                       'transformations.'                     )
               CALL ERRCH  ( '#', FFRAME                            )
               CALL ERRCH  ( '#', TFRAME                            )
               CALL ETCAL  ( EPOCH(I), TMPSTR                       )
               CALL ERRCH  ( '#', TMPSTR                            )
               CALL SIGERR ( 'SPICE(NOTAROTATION)'                  )
               
            END IF

         END IF

C
C        Move on to the next epoch.
C
         I = I + 1

      END DO

C
C     Restore original error and report modes.
C
      CALL ERRACT ( 'SET', SAVACT  )
      CALL ERRPRT ( 'SET', SAVRPT )

C
C     All done.
C
      CALL CHKOUT ( 'GETQAV' )
      RETURN
      END

