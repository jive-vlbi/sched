      SUBROUTINE PLSTAS
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
C     Display STATIONS/SOURCE Area
C
C     To add or modify the options in the STATIONS Panel, check also
C     the INCLUDE file 'plot.inc' and the soubroutine 'plinit.f'
C
      CHARACTER*40  STRING
      INTEGER       REV, STLEN 
      REAL          XL, XT, YT, CSIZ
      LOGICAL       PBTR
C ----------------------------------------------------------------------
C
C     Set Font dimension, Select Buttons Policy
C     and Label offset.
C
      CSIZ  = PPNDIM(3)
      XL    = 0.02
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Clear Area if Needed
C
      IF( PRWARN ) THEN
         CALL PGSFS( 1 )
         CALL PGSCI( 0 )
         CALL PGRECT( PPSDIM(1), PPSDIM(2), PPSDIM(3), PPSDIM(4) )
      END IF
C
C     Titles
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      CALL PGSCI( PPNCOL(7) )
C
C     Stations
C
      STRING = 'Select Stations to Plot and Highlight'
      STLEN = 40
      IF( PSTSCR ) STLEN = 15
      XT = PPSDIM(1)
      YT = PPSDIM(4)
      CALL PGPTXT(XT, YT, 0.0, 0.0, STRING(1:STLEN))
C
C     Baselines
C
      YT = PSTBXB(4,2)
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Baselines:' )
C
C     Sources
C
      YT = PSOSBR(3)
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Select Sources' )
C
C     Stations Baselines Box
C
      PBTR  = .TRUE.
      REV = 0
      IF ( PSTBAS ) REV = 1
      CALL PLBUTS( PBTR, PSTBXB(1,1), PSTBXB(2,1), PSTBXB(3,1),
     1             PSTBXB(4,1), XL, CSIZ, PSTLAB(3), 0, REV)
      REV = 1
      IF ( PSTBAS ) REV = 0
      CALL PLBUTS( PBTR, PSTBXB(1,2), PSTBXB(2,2), PSTBXB(3,2),
     1             PSTBXB(4,2), XL, CSIZ, PSTLAB(4), 0, REV)
C
C     Stations Box and Scroll Bar
C
      CALL PLSTSL( 0 )
C
C     SET/UNSET Box
C
      PBTR  = .FALSE.
      CALL PLBUTS( PBTR, PSTSUN(1,1), PSTSUN(2,1), PSTSUN(3,1),
     1            PSTSUN(4,1), XL, CSIZ, ' ', 0, 0)
      CALL PLBUTS( PBTR, PSTSUN(1,2), PSTSUN(2,2), PSTSUN(3,2),
     1                PSTSUN(4,2), XL, CSIZ, PSTLAB(1), 0, 0)
      CALL PLBUTS( PBTR, PSTSUN(1,3), PSTSUN(2,3), PSTSUN(3,3),
     1            PSTSUN(4,3), XL, CSIZ, ' ', 0, 0)
      CALL PLBUTS( PBTR, PSTSUN(1,4), PSTSUN(2,4), PSTSUN(3,4),
     1                PSTSUN(4,4), XL, CSIZ, PSTLAB(2), 0, 0)
C
C     Sources Box and Scroll Bar
C
      CALL PLSOSL( 0 )
C
C     SET/UNSET Box
C
      PBTR  = .FALSE.
      CALL PLBUTS( PBTR, PSOSUN(1,1), PSOSUN(2,1), PSOSUN(3,1),
     1             PSOSUN(4,1), XL, CSIZ, PSTLAB(1), 0, 0)
      CALL PLBUTS( PBTR, PSOSUN(1,2), PSOSUN(2,2), PSOSUN(3,2),
     1             PSOSUN(4,2), XL, CSIZ, PSTLAB(2), 0, 0)
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
