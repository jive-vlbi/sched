      SUBROUTINE TAPEFLAG( ISCN, STRING, FLAG, VALUE, ERRS )
C
C     Routine for SCHED subroutine GETSTA that extracts the necessary
C     information for the possibly station dependent tape requests:
C     These are tape change, reverse, rewind, and fast forward.
C
C     Assumes that there are MAXSTA possible entries in VALUES for
C     this variable.
C
C     In the call, the first element of VALUE that applies to this
C     set of inputs (eg TAPE) should be specified.
C
      INCLUDE 'sched.inc'
C
      CHARACTER         STRING*(*), STANAM*8
      INTEGER           LEN1, ISCN, ISTA, KSTA, IT
      DOUBLE PRECISION  VALUE(*)
      LOGICAL           FLAG(MAXSTA), ERRS, DOIT
C  --------------------------------------------------------------
C     Deal with unset or globally set request.
C
      IF( VALUE(1) .EQ. 0.D0 .OR. VALUE(1) .EQ. UNSET ) THEN
         DO ISTA = 1, NSTA
            FLAG(ISTA) = STASCN(ISCN,ISTA) .AND. VALUE(1) .EQ. 0.D0 
         END DO
C
C     Deal with station dependent request.  Loop (IT) over entries
C     in the VALUE array from KEYIN.  Associate the request with
C     station ISTA.
C
      ELSE
         DO ISTA = 1, NSTA
            FLAG(ISTA) = .FALSE.
         END DO
         DO IT = 1, MAXSTA
            IF( VALUE(IT) .NE. UNSET ) THEN
               WRITE( STANAM, '(A8)' ) VALUE(IT)
               CALL UPCASE( STANAM )
C
               CALL STANO( STANAM, KSTA, ISTA, DOIT )
C
C              Only consider if DOSTA matched.
C
               IF( DOIT ) THEN
C
C                 Require that the station is known.
C
                  IF( ISTA .EQ. 0 ) THEN
                     WRITE( MSGTXT, '( 4A )' )
     1                    'GETSTA: ', STRING(1:LEN1(STRING)),
     2                    ' for station not in schedule: ', STANAM
                     CALL WLOG( 1, MSGTXT )
                     ERRS = .TRUE.
                  ELSE
C
C                    Require station is in scan.
C
                     IF( .NOT. STASCN(ISCN,ISTA) ) THEN
                        WRITE( MSGTXT, '( 4A )' )
     1                    'GETSTA: ', STRING(1:LEN1(STRING)),
     2                    ' for station not in scan: ', STANAM
                        CALL WLOG( 1, MSGTXT )
                        ERRS = .TRUE.
C
C                    Now can set the flag.
C
                     ELSE
                        FLAG(ISTA) = .TRUE.
                     END IF
C                     
                  END IF
               END IF
            END IF
         END DO
      END IF
C
      RETURN
      END
