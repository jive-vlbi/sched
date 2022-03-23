      SUBROUTINE PKFINISH
C
C     Routine for SCHED that ties up loose ends on setting up for      
C     automatic insertion of reference pointing scans.  It is put
C     after the reading of the inputs to insure that the station
C     and source catalogs have been read.
C
      INCLUDE      'sched.inc'
      INCLUDE      'schpeak.inc'
C
      INTEGER           ISTA, KSTA, IGRP, IPKSTA
      LOGICAL           DOIT
C ---------------------------------------------------------------------
C
C     Get the full station names for any stations that were
C     specified using station codes.  Keep 'VLBA' as a way of
C     specifying any VLBA station not otherwise listed.  
C
      IF( NPKGRP .GT. 0 ) THEN
         DO IGRP = 1, NPKGRP
            IF( NPKSTA(IGRP) .GT. 0 ) THEN
               DO IPKSTA = 1, NPKSTA(IGRP)
                  IF( PKSTA(IPKSTA,IGRP) .NE. 'VLBA' ) THEN
                     CALL STANO( PKSTA(IPKSTA,IGRP), KSTA, ISTA, DOIT )
                     IF( KSTA .EQ. 0 ) THEN
                        MSGTXT = 'PKFINISH: Pointing group station ' //
     1                      PKSTA(IPKSTA,IGRP) //
     2                      ' not found in station catalog.'
                        CALL ERRLOG( MSGTXT )
                     ELSE
                        PKSTA(IPKSTA,IGRP) = STATION(KSTA)
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END IF
C
C     Get pointers from stations to groups.  Note that PKGROUP's
C     dimension (MPKSTA) will be assured to be .GE. MAXSTA by
C     an error message in STMSG.  So use PKGROUP as if it is 
C     dimensioned MAXSTA.  Note this enforces that each station
C     is only in one group.  Take some care to allow a 'VLBA'
C     default and have that pick up only stations not in other
C     groups.
C
      DO ISTA = 1, NSTA
         IF( NPKGRP .GT. 0 ) THEN
C
            DO IGRP = 1, NPKGRP
               IF( NPKSTA(IGRP) .GT. 0 ) THEN
                  DO IPKSTA = 1, NPKSTA(IGRP)
                     IF( PKSTA(IPKSTA,IGRP) .EQ. STANAME(ISTA) ) THEN
                        PKGROUP(ISTA) = IGRP
                     END IF
                  END DO
               END IF
            END DO
C
C           Now pick up the VLBA defaulted stations.
C
            DO IGRP = 1, NPKGRP
               IF( NPKSTA(IGRP) .GT. 0 ) THEN
                  DO IPKSTA = 1, NPKSTA(IGRP)
                     IF( PKSTA(IPKSTA,IGRP) .EQ. 'VLBA' .AND.
     1                   STANAME(ISTA)(1:4) .EQ. 'VLBA' .AND.
     2                   PKGROUP(ISTA) .EQ. 0 ) THEN
                        PKGROUP(ISTA) = IGRP
                     END IF
                  END DO
               END IF
            END DO
C
         END IF
      END DO
C
C     The pointing source catalog will be read by later routines.
C
      RETURN
      END
