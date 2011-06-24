      SUBROUTINE OUTPUT
C
C     Write out the standard setup files (from MAKESETUP).
C
      INCLUDE 'makeset.inc'
C
      INTEGER   IF, IL, ICH, LEN1
C ---------------------------------------------------------------------
C
      DO IF = 1, NF
         OPEN(UNIT=11,FILE=FILE(IF),FORM='FORMATTED',STATUS='UNKNOWN')
C
C        Insert warning about lost modifications.
C
         WRITE( 11, '(A,A)' ) '! Standard setup file: ', FILE(IF)
         WRITE( 11, '(A)' )   '!     (Produced by MAKESETUP)'
C
         DO IL = 1, FILEL(IF)
            ICH = LEN1(FILETEXT(IL,IF))
            WRITE(11,'(A)') FILETEXT(IL,IF)(1:ICH)
         END DO
C
C        Determine if there was a final "/".  With defaulting of
C        station names and use of the SHELL mode, there might not
C        be.
C
         ICH = LEN1(FILETEXT(FILEL(IF),IF))
         IF( FILETEXT(FILEL(IF),IF)(ICH:ICH) .NE. "/" ) THEN
            WRITE( 11, '(A)' ) '  /'
         END IF
C
         CLOSE( UNIT = 11 )
      END DO
C
  990 CONTINUE
C
      RETURN
      END
