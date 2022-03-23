      SUBROUTINE WRTCOV( IFILE )
C
C     Write the cover letter information to the desired output file.
C     This is a dump of what is in the cover letter scratch file.
C
      INCLUDE 'sched.inc'
C
      INTEGER      IOERR, IFILE, VLBOPE, LEN1
      LOGICAL      DONECOV
      CHARACTER    LINE*256
C --------------------------------------------------------------------
      IF( COVERLET ) THEN
         IOERR = VLBOPE( ICOV, 'schedcov.tmp', 'TEXT', 'OLD', LINE )
         IF( IOERR .NE. 1 ) THEN
            CALL WLOG( 1, LINE )
            CALL ERRLOG( 'WRTCOV: Cannot open '//
     1       'cover letter scratch file.  This should not happen.' )
         END IF
C
C        Now transfer the cover letter to the scratch file.
C
         DONECOV = .FALSE.
         DO WHILE( .NOT. DONECOV )
            READ( ICOV, '( A )', END = 200 ) LINE
            WRITE( IFILE, '( A )' ) LINE(1:LEN1(LINE))
         END DO
  200    CONTINUE
         CLOSE( UNIT=ICOV )
      END IF
C
      RETURN
      END
