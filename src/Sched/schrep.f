      SUBROUTINE SCHREP( ISCN, IREP, VALUE, KC, KI, START, STOP, 
     1                   DAY, YEAR )
C
C     Routine for SCHED, called by SCHIN, that sets up loops.
C     WARNING.  ISCN is reset by this routine to the value of last scan
C     of the loop.  Don't do anything after the call to SCHREP that
C     uses ISCN!
C
C     Note that SCHDUP is also used by some optimization routines.
C
      INCLUDE 'sched.inc'
C
      INTEGER      K, L, ISCN, IREP, KEYPTR
      INTEGER      INGRP, NREP, JREP, IS, INREP
      INTEGER      KI(*), DAY(*), YEAR(*)
      CHARACTER    KC(*)*(*)
      DOUBLE PRECISION   VALUE(*), START(*), STOP(*)
      SAVE         INGRP, NREP, INREP, JREP
C -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SCHREP: Starting.' )
C
C     Decode looping request.  Nesting not allowed.
C
      INREP = VALUE( KEYPTR( 'REPeat', KC, KI ) )
      IF( IREP .EQ. 0 ) THEN
         INGRP = VALUE( KEYPTR( 'GROUP', KC, KI ) )
         NREP = INREP
         IF( NREP.NE.1 ) THEN
            IREP = ISCN
            JREP = IREP + INGRP - 1
         END IF
      ELSE IF( IREP .GT. 0 .AND. INREP .GT. 1 ) THEN
         CALL ERRLOG( 'SCHREP: Cannot do nested or overlapping loops.')
      END IF
C
      IF( DEBUG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5, A, 2I5 )' )
     1     'SCHREP: Scan - ', ISCN, '  Repeat and group: ', NREP, INGRP
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     When scans to be duplicated are read, duplicate them.
C
      IF( IREP.NE.0 .AND. ISCN.GE.JREP ) THEN
         DO  K = 2,NREP
            DO  L = IREP,JREP
               IS = JREP + INGRP*(K-2) + L-IREP+1
C
C              Copy the items in the include file.
C
               CALL SCNDUP( IS, L, .FALSE., 'SCHREP' )
C
C              Act as if start, stop, and date were not given.
C              Note that DAY and YEAR are not in the include file.
C
               START(IS) = UNSET
               STOP(IS)  = UNSET
               DAY(IS)   = DAY(L)
               YEAR(IS)  = YEAR(L)
C
C              Also copy the COMMENT (ANNOT) which would not be copied
C              with the .FALSE. in the SCNDUP call.
C
               ANNOT(IS) = ANNOT(L)
C
            END DO
         END DO
C
C        Set scan number for next scan and reinitialize loop counters.
C
         ISCN = ISCN + INGRP * (NREP - 1)
         IREP = 0
         JREP = 0
      END IF
C
      RETURN
      END
