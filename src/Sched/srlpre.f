      SUBROUTINE SRLPRE( PRECDATE, OBSTIME )
C
C     Routine for SCHED that precesses all sources in the full
C     catalog source list.  That list is only used for plotting
C     as of when this routine was written.  It is kept in 
C     low precision to save space for what could be a large list.
C
      INCLUDE         'srlist.inc'
C
      INTEGER            I
      DOUBLE PRECISION   RA8, DEC8, RAJ, DECJ, PRECDATE, OBSTIME
C ----------------------------------------------------------------------
      DO I = 1, SRLN
         IF( SRLEPO(I) .NE. 'J' ) THEN
            RA8 = SRLRA(I)
            DEC8 = SRLDEC(I)
            IF( SRLEPO(I) .EQ.' B' ) THEN
               CALL SLA_FK45Z( RA8, DEC8, PRECDATE, RAJ, DECJ )
            ELSE
               CALL SLA_AMP( RA8, DEC8, OBSTIME, 2000.D0, RAJ, DECJ )
            END IF            
            SRLRA(I) = RAJ
            SRLDEC(I) = DECJ
            SRLEPO(I) = 'J'
         END IF
C
      END DO
C
      RETURN
      END
