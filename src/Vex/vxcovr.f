      SUBROUTINE VXCOVR( VXFILE )
C
C     Write the cover letter information to the desired output file.
C     This is a dump of what is in the cover letter scratch file.
C
      INCLUDE 'sched.inc'
C
      INTEGER      IOERR, VXFILE, VLBOPE, LEN1
      LOGICAL      DONECOV
      CHARACTER    LINE*256,COM*1
C --------------------------------------------------------------------
      COM = '*'
      IF( COVERLET ) THEN
         IOERR = VLBOPE( ICOV, 'schedcov.tmp', 'TEXT', 'OLD', LINE )
         IF( IOERR .NE. 1 ) THEN
            CALL WLOG( 1, LINE(1:LEN1(LINE)) )
            CALL ERRLOG( 'VXCOVR: Cannot open '//
     1       'cover letter scratch file.  This should not happen.' )
         END IF
C
C        Now transfer the cover letter to the scratch file.
C
         WRITE( VXFILE, '( A1 )' ) COM
         WRITE( VXFILE, '( A1, A, A, A )' ) COM, 
     1       '--------------------',
     2       ' cover  letter ',
     3       '--------------------' 
         DONECOV = .FALSE.
         DO WHILE( .NOT. DONECOV )
            READ( ICOV, '( A )', END = 200 ) LINE
            WRITE( VXFILE, '( A1, A )' ) COM, LINE(1:LEN1(LINE))
         END DO
  200    CONTINUE
         WRITE( VXFILE, '( A1, A, A, A )' ) COM, 
     1       '-------------------------',
     2       ' end ',
     3       '-------------------------' 
         CLOSE( UNIT=ICOV )
      END IF
C
      RETURN
      END
