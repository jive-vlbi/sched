      SUBROUTINE SCNRANGE
C
C     Adjust the scan range (SCAN1 and SCANL) for the output scans 
C     from SCHED, other than the .sum file, to reflect the 
C     specifications of PREEMPT and DOSCANS.
C
C     Consider having a time range some day, but that needs more 
C     complicated input.
C
C     Moved from MAIN (sched.f) to keep that routine simple.
C
      INCLUDE   'sched.inc'
C
      INTEGER   CORE1, COREL, ISCN
      LOGICAL   GOTC1
C -----------------------------------------------------------------

      IF( DOSCANS(1) .NE. 0 .AND. DOSCANS(2) .NE. 0 ) THEN
         MSGTXT = ' ' 
         WRITE( MSGTXT, '( A, I5, A, I5, A  )' )
     1      'SCHED: --- DOSCANS given to restrict output to scans ',
     2      DOSCANS(1), ' to ', DOSCANS(2), ' ---'
         CALL WLOG( 1, ' ' )
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '       --- This affects the .vex, .oms, '//
     1        'crd., sch., and .flag files ---' )
         CALL WLOG( 1, ' ' )
         SCAN1 = DOSCANS(1)
         SCANL = DOSCANS(2)
      ELSE IF( FUZZY ) THEN
         CORE1 = SCAN1
         COREL = SCAN1
         GOTC1 = .FALSE.
         DO ISCN = SCAN1, SCANL
            IF( .NOT. GOTC1 .AND. PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
               CORE1 = ISCN
               GOTC1 = .TRUE.
            END IF
            IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
               COREL = ISCN
            END IF
         END DO
         CALL WLOG( 1, ' ' )
         CALL WLOG( 1, 'SCNRANGE: --- PREEMPT=EXTRA scans were '//
     1      'specified but no DOSCANS were given.      --- ' )
         MSGTXT = ' ' 
         IF( CORE1 .GT. SCAN1 .AND. COREL .LT. SCANL ) THEN
            WRITE( MSGTXT, '( A, I5, A, I5, A, I5, A, I5, A )' )
     1        '          --- EXTRA scans ', SCAN1, ' to ', CORE1-1, 
     2        ' and ', COREL, ' to ', SCANL, ' will not be written. ---'
         ELSE IF( CORE1 .GT. SCAN1 ) THEN
            WRITE( MSGTXT, '( A, I5, A, I5, A )' )
     1        '          --- EXTRA scans ', SCAN1, ' to ', CORE1-1, 
     2        ' will not be written. ---'
         ELSE
            WRITE( MSGTXT, '( A, I5, A, I5, A )' )
     1        '          --- EXTRA scans ', COREL, ' to ', SCANL, 
     2        ' will not be written. ---'
         END IF
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '          --- This affects the .vex, .oms, '//
     1        'crd., sch., and .flag files           ---' )
         CALL WLOG( 1, ' ' )
         SCAN1 = CORE1
         SCANL = COREL
      END IF
C
      RETURN
      END
