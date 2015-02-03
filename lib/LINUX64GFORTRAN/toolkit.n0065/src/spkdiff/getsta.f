C$Procedure      GETSTA ( Compute/not-compute states )

      SUBROUTINE GETSTA ( CHECK, BODID, CENID, FRAME, EPOCH, N,
     .                    STATE, OK, ERROR, ERRIDX )

C$ Abstract
C
C     This routine either computes geometric states of a given body
C     relative to a given center in a given reference frame at given
C     epochs or verifies that they cannot be computed.
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
      INTEGER               BODID
      INTEGER               CENID
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      EPOCH ( * )
      INTEGER               N
      DOUBLE PRECISION      STATE ( 6, * )
      LOGICAL               OK
      CHARACTER*(*)         ERROR
      INTEGER               ERRIDX

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CHECK      I   Run mode: .TRUE. for check, .FALSE. for compute
C     BODID      I   Body ID.
C     CENID      I   Center ID.
C     FRAME      I   Frame name.
C     EPOCH      I   Buffer of epochs (ETs).
C     N          I   Number of epochs.
C     STATE      O   Output state buffer
C     OK         O   Success flag
C     ERROR      O   Error message
C     ERRIDX     O   Index of the epoch at which error occured.
C
C$ Detailed_Input
C
C     CHECK       is the run mode: .TRUE. for check, .FALSE. for
C                 compute.
C
C     BODID       is the body ID.
C
C     CENID       is the center ID.
C
C     FRAME       is the frame name.
C
C     EPOCH       is the buffer of epochs (ETs).
C
C     N           is the number of epochs.
C
C$ Detailed_Output
C
C     STATE       is the array of output states of the given body
C                 relative to the given center in the given reference
C                 frame at each of the epochs. Undefined if routine is
C                 run in check mode.
C
C     OK          is the success flag: .TRUE. for success, .FALSE. for
C                 failure.
C
C     ERROR       is the error message. Blank if OK is .TRUE.
C
C     ERRIDX      is the index of the epoch from the EPOCH buffer at
C                 which error occured. Set to 0 if no error.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
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
C     NONE and attempts to compute the state of the given body relative
C     to the given center in the given reference frame at each of the
C     epochs.
C
C     If this routine is called in check mode, it verifies that this
C     attempt failes for all epochs. If SPKGEO call succeeded for even
C     one point, the routine returns failure status and description of
C     the error.
C
C     If this routine is called in compute mode, it verifies that this
C     attempt succeeds for all epochs. If yes, it returns with success
C     status and STATE buffers filled with valid states. If SPKGEO did
C     not succeed for even one point, the routine returns failure
C     status and description of the error.
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
C-    Version 1.0.0, 25-MAR-2014 (BVS)
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
      CHARACTER*(LMSGLN)    LONGMS

      DOUBLE PRECISION      HSTATE ( 6 )
      DOUBLE PRECISION      LT

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
         CALL CHKIN ( 'GETSTA' )
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
      ERRIDX = 0
      I = 1

      DO WHILE ( I .LE. N .AND. OK )

C
C        Try to compute state.
C
         CALL SPKGEO ( BODID, EPOCH(I), FRAME, CENID, HSTATE, LT )

C
C        Did SPKGEO fail or succeed? Are we checking or
C        computing?
C
         IF      (       CHECK .AND. .NOT. FAILED() ) THEN

C
C           Bad: we are checking and it did not fail. Set OK flag to
C           failure and put together an error description.
C
            OK = .FALSE.

            ERROR = 'Geometric state of # relative to # ' //
     .              'in ''#'' frame could be computed at ET #'
            CALL REPMI ( ERROR, '#', BODID, ERROR )
            CALL REPMI ( ERROR, '#', CENID, ERROR )
            CALL REPMC ( ERROR, '#', FRAME, ERROR )
            CALL REPMF ( ERROR, '#', EPOCH(I), 14, 'E', ERROR )

            ERRIDX = I

         ELSE IF ( .NOT. CHECK .AND.       FAILED() ) THEN

C
C           Bad: we are computing and it failed. Set OK flag to failure
C           and put together an error description. Reset error handling.
C
            OK = .FALSE.

            ERROR = 'Geometric state of # relative to # ' //
     .              'in ''#'' frame could not be computed at ET ' // 
     .              '#. SPKGEO error was: #'
            CALL REPMI ( ERROR, '#', BODID, ERROR )
            CALL REPMI ( ERROR, '#', CENID, ERROR )
            CALL REPMC ( ERROR, '#', FRAME, ERROR )
            CALL REPMF ( ERROR, '#', EPOCH(I), 14, 'E', ERROR )
            CALL GETMSG( 'LONG', LONGMS )
            CALL REPMC ( ERROR, '#', LONGMS, ERROR )

            ERRIDX = I

            CALL RESET()

         ELSE IF (       CHECK .AND.       FAILED() ) THEN

C
C           Good: we are checking and it failed. Reset error handling
C           and move on to the next epoch.
C
            CALL RESET()


         ELSE IF ( .NOT. CHECK .AND. .NOT. FAILED() ) THEN

C
C           Good: we are computing and it did not fail. Buffer this
C           state and move on to the next epoch.
C
            CALL MOVED( HSTATE, 6, STATE( 1, I ) )

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
      CALL CHKOUT ( 'GETSTA' )
      RETURN
      END

