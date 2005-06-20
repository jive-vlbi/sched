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
      INTEGER   LEN1, J
      CHARACTER NAME*32
C ----------------------------------------------------------------------
C
C
C
C     A (VEX)$MODE is the status of a scan and should define something for
C     all antennas. One (SCHED)SETup cannot be more than 1 MODE, but
C     usually 1 MODE has more than 1 SET, defining different antenas.
C     NSETF is usually the number of modes, but we have to build a list
C     of antenna setups:
C
      NMDVEX = 0      
      DO ISETFL = 1, NSETF
C
C        It seems sometimes setname can be empty, be careful
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
C        every file is a new mode
C
         NMDVEX = ISETFL
         MDISFIL(NMDVEX) = ISETFL
         IF( NMDVEX .GT. MAXMOD ) CALL ERRLOG(
     1       'VXMODE: Number of $MODE defs '//
     2       'exceeding MAXMOD, need to re-compile...')
         MDLINK(NMDVEX) = NAME
         CALL VXUNQL( NMDVEX, MDLINK )
C
C        for all antennas, each mode has 1 original sched-set
C        unfortunately they don't match right up
C
         DO ISET = 1, NSET
            IF( USED(ISET) ) THEN 
C
C              Only if this group is in this file
C
               IF( SETNAME(ISET) .EQ. SETFILE(ISETFL) ) THEN
                  DO ISTA = 1, NSTA
                     ISCAT = STANUM(ISTA)
C
C                    This covers VLBA (changed CR 19 Oct 2004):
C
                     IF ( STATION(ISCAT) .EQ. SETSTA(1,ISET) .OR.
     1                   (STATION(ISCAT)(1:4) .EQ. 'VLBA' .AND.
     2                   SETSTA(1,ISET)(1:4) .EQ. 'VLBA' .AND.
     3                   .NOT. (VLBASSTA(ISTA, ISET) .OR. 
     4                   VLBASSET(ISET) )) ) THEN
C
C                    We should keep this the lowest matching number, don't set
C                    again when already set.
C
                       IF( MODSET(ISTA,NMDVEX) .EQ. 0 ) THEN
                          MODSET(ISTA,NMDVEX) = ISET
                       END IF
                     ENDIF
                  END DO
               END IF
            END IF
         END DO
      END DO
C
      RETURN
      END




