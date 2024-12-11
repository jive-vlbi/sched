      SUBROUTINE BBCKVSR( KS )
C
C
C     Routine for Sched called by SETBBC that assigns video converters
C     for Zelenchk system.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    I, KS, NNBBC
      INTEGER    MAXBBC, MAXIF, NIF
      PARAMETER  (MAXBBC=16)
      PARAMETER  (MAXIF=4)
      INTEGER    IFBBC(MAXBBC,MAXIF)
      CHARACTER  IFNAM(MAXIF)*2
      LOGICAL    UBBC(MAXBBC)
      DATA  (IFBBC(I,1),I=1,MAXBBC) / 1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0 /
      DATA  (IFBBC(I,2),I=1,MAXBBC) / 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1 /
      DATA  (IFBBC(I,3),I=1,MAXBBC) / 1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0 /
      DATA  (IFBBC(I,4),I=1,MAXBBC) / 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1 /
      DATA  (IFNAM(I),I=1,MAXIF) / 'A', 'B', 'C', 'D' /
c$$$      DATA (IFCH(I),I=1,16)  / 'A','B','A','B','A','B','A','B', 
c$$$     1 'A','B','A','B','C','D','C','D' /
c$$$      DATA (BBCS(I),I=1,16) / 1,  9,  1,  9,  2, 10,  2, 10,  
c$$$     1 3, 11,  3, 11,  4, 12,  4, 12 /


C -------------------------------------------------------------------  
C     Software check:
C
      IF( NBBC(ISETSTA(KS)) .GT. MAXBBC ) THEN
         WRITE( MSGTXT, '( 3A, I4 )' )
     1       'BBCM4: Number of VCs at ', SETSTA(1,KS), 
     2       ' Larger than maximum expected: ', MAXBBC
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '   Catalog or programming problem ' )
         CALL ERRSET( KS )
      END IF
C
      NNBBC = NBBC(ISETSTA(KS))
      NIF = 4
C
C     Call BBCALT to do the work.
C
      CALL BBCALT( KS, MAXBBC, NIF, IFBBC, IFNAM, UBBC, NNBBC, ' ',
     1    'BBCKVSR' )
C
      RETURN 
      END
