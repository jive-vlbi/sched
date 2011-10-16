      SUBROUTINE VXMODE
C
C     Routine for the VEX extension of SCHED.
C     Finds out how many different modes are to be used in this vex
C     file. Stores for each mode the setups that each antenna was using
C     to find the different $BLOCKS per antenna for each mode
C     H.J.van Langevelde JIVE, 130596
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
C     variables
C     
      INTEGER   I, ISETFL, ISET, ICH1, ICH2, ISTA, ISCAT
      INTEGER   LEN1
      CHARACTER NAME*32
      LOGICAL   NEWVXMD
C ----------------------------------------------------------------------
C     There is a comment below claiming that SETNAME can be blank.  That
C     surprises me so look for it.  RCW Oct. 15, 2011.
C
      DO ISET = 1, NSET
         IF( SETNAME(ISET) .EQ. ' ' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, I4 )' )
     1         'VXMODE: Info for programmer - blank SETNAME found.',
     2         '  Setup group ', ISET
            CALL WLOG( 1, MSGTXT )
         END IF
      END DO
C
C
C
C     A (VEX)$MODE is the status of a scan and should define something for
C     all antennas. One (SCHED)SETup cannot be more than 1 MODE, but
C     usually 1 MODE has more than 1 SET, defining different antenas.
C     NSETF is usually the number of modes, but we have to build a list
C     of antenna setups:
C
      NMDVEX = 1      
      DO ISETFL = 1, NSETF
C
C        It seems sometimes setname can be empty, be careful.
C
         NAME = ' ' 
         ICH2 = LEN1( SETFILE(ISETFL) )
         IF( SETFILE(ISETFL)(ICH2-3:ICH2) .EQ. '.set' 
     1        .OR. SETFILE(ISETFL)(ICH2-3:ICH2) .EQ. '.SET' )
     2        ICH2 = ICH2 - 4
         ICH1 = 1
         DO I = 1, ICH2
            IF( SETFILE(ISETFL)(I:I).EQ.'/'.OR.
     1           SETFILE(ISETFL)(I:I).EQ.'\\') ICH1 = I+1
         END DO
         IF ( ICH2 .GT. ICH1+1 .AND. ICH2 .NE. 80) THEN
C
C           truncate to 32 chars
C
            ICH2 = MIN( (ICH1+32), ICH2 )
            NAME = SETFILE(ISETFL)(ICH1:ICH2) 
         ENDIF
C
C        for all antennas, each mode has 1 original sched-set
C        unfortunately they don't match right up
C
         NEWVXMD = .FALSE.
         DO ISET = 1, NSET
            IF( USED(ISET) ) THEN 
C
C              Only if this group is in this file.
C
               IF( SETNAME(ISET) .EQ. SETFILE(ISETFL) ) THEN
                  DO ISTA = 1, NSTA
                     ISCAT = STANUM(ISTA)
C
                     IF ( STATION(ISCAT) .EQ. SETSTA(1,ISET) ) THEN
C
C                    We should keep this the lowest matching number, don't set
C                    again when already set.
C
                       IF( MODSET(ISTA,NMDVEX) .EQ. 0 ) THEN
                          MODSET(ISTA,NMDVEX) = ISET
                          NEWVXMD = .TRUE.
                       END IF
                     ENDIF
                  END DO
               END IF
            END IF
         END DO
C
C        every used file is a new mode
C
         IF( NEWVXMD ) THEN
           MDISFIL(NMDVEX) = ISETFL
           IF( NMDVEX .GT. MAXMOD ) CALL ERRLOG(
     1         'VXMODE: Number of $MODE defs '//
     2         'exceeding MAXMOD, need to re-compile...')
           MDLINK(NMDVEX) = NAME
           CALL VXUNQL( NMDVEX, MDLINK )
           NMDVEX = NMDVEX + 1
         END IF
      END DO
C
C     Last increment of NMDVEX is never used, so reduce by 1
C
      NMDVEX = NMDVEX - 1
C
      RETURN
      END




