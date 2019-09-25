      SUBROUTINE SRCFLG( GOTALL )
Cf2py intent(out) GOTALL
C
C     Routine for SCHED called by SRREAD and SRFINISH that compares 
C     catalog entries with sources requested in the schedule and sets
C     the appropriate pointers.  It is called again later by SCHOPT
C     after the optimization routines are run and after pointing 
C     sources are added.
C
C     It marks which alias is used.  Marking whether or not the source
C     was used (SUSED) is handled elsewhere.
C
C     It also checks whether all sources have been found.
C     MSRC is assumed to be greater than 0 on the call.
C
C     This routine is called more than once with source lists that 
C     may have changed and/or grown.  Therefore it resets everything
C     from scratch.
C
      INCLUDE  'sched.inc'
C
      INTEGER        KSRC, ISRC, INAME
      LOGICAL        GOTALL
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SRCFLG starting' )
C
C     Initialize the pointers and flags.
C
      DO ISRC = 1, MSRC
         SRLSTN(ISRC) = 0
         DO INAME = 1, MALIAS
            CSUSED(INAME,ISRC) = ' '
         END DO
      END DO
      DO KSRC = 1, NSRC
         SRCATN(KSRC) = 0 
      END DO
C
C     Loop over sources in schedule.  
C
      DO KSRC = 1, NSRC
C
C        Loop over catalog sources we have at this point and loop
C        over the aliases.
C
         DO ISRC = 1, MSRC
            DO INAME = 1, MALIAS
C 
C              Check for match between schedule and catalog sources.
C 
               IF( SRCNAME(KSRC) .EQ. SOURCE(INAME,ISRC) ) THEN
C
C                 Get used flags and pointer to schedule source for
C                 the catalog source.
C
                  CSUSED(INAME,ISRC) = '*'
                  SRLSTN(ISRC) = KSRC
C
C                 Get the pointer to the catalog source for the 
C                 schedule source.
C
                  SRCATN(KSRC) = ISRC
C
                  GO TO 100
               END IF
            END DO
         END DO
  100    CONTINUE
      END DO
C
C     Check if have all schedule sources in the catalog.  Don't complain
C     yet because we may have a planet or satellite and that will
C     be checked separately.
C
      GOTALL = .TRUE.
      DO KSRC = 1, NSRC
         IF( SRCATN(KSRC) .EQ. 0 ) THEN
            GOTALL = .FALSE.
         END IF
      END DO
C
      RETURN
      END

