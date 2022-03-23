      SUBROUTINE BBCGEO( KS )
C
C     Routine for SCHED, called by SETBBC, that sets the BBC
C     assignments for VLBA systems with geodetic wiring.  For these
C     systems (from Ed Himwich email of 20 Oct 1997):
C     BBCs                  IFs
C     1,2                   A,B,C,D
C     3,4,5,6,7,8           A,C
C     9,10,11,12,13,14      B,D
C
C     "I believe all VLBA racks outside the VLBA itself 
C     (and the pseudo-VLBA stations: Green Bank 140', Effelsberg, 
C     and VLA?) have this wiring. (Hmmm, how about SEST?)".
C
C     The code needed, once the arrays are established, is the same
C     as for BBCM4 (Mark IV), so it has been put in BBCALT, which
C     is shared.
C
C     There is an additional complication.  There are not enough
C     sampler inputs to send 2 bit signals from more than 8 BBCs
C     to the formatter.  For now (1997+-) someone changes a plug
C     for two options.  The first is 2 bit on 8 BBCs.  The second
C     is one bit on all BBCs.  Deal with this by sensing 2 bit
C     observations and restricting the number of BBC's to 8.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    I, KS
      INTEGER    MAXBBC, MAXIF, NNBBC
      PARAMETER  (MAXBBC=14)
      PARAMETER  (MAXIF=4)
      INTEGER    IFBBC(MAXBBC,MAXIF)
      CHARACTER  IFNAM(MAXIF)*2, WARNING*4
      LOGICAL    UBBC(MAXBBC)
      DATA  (IFBBC(I,1),I=1,MAXBBC) / 1,1,1,1,1,1,1,1,0,0,0,0,0,0 /
      DATA  (IFBBC(I,2),I=1,MAXBBC) / 1,1,0,0,0,0,0,0,1,1,1,1,1,1 /
      DATA  (IFBBC(I,3),I=1,MAXBBC) / 1,1,1,1,1,1,1,1,0,0,0,0,0,0 /
      DATA  (IFBBC(I,4),I=1,MAXBBC) / 1,1,0,0,0,0,0,0,1,1,1,1,1,1 /
      DATA  (IFNAM(I),I=1,MAXIF) / 'A', 'B', 'C', 'D' /
C
C     Note that the above data statement is also in CHKGDAR.  Changes
C     should be made in both places.
C -------------------------------------------------------------------  
C     Software check:
C
      IF( NBBC(ISETSTA(KS)) .GT. MAXBBC ) THEN
         WRITE( MSGTXT, '( 3A, I4 )' )
     1       'BBCGEO: Number of BBCs at ', SETSTA(1,KS), 
     2       ' Larger than maximum expected: ', MAXBBC 
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '   Catalog or programming problem ' )
         CALL ERRSET( KS )
      END IF
C
C     Set the maximum number of BBC's based on the number of bits.
C
      IF( BITS(1,KS) .EQ. 2 .AND. NCHAN(KS) .GT. 8 ) THEN
         NNBBC = MIN( NBBC(ISETSTA(KS)), 8 )
         WARNING = 'GEO2'
      ELSE
         NNBBC = NBBC(ISETSTA(KS))
         WARNING = 'GEO1'
      END IF
C
C     Call BBCALT to do the work.
C
      CALL BBCALT( KS, MAXBBC, MAXIF, IFBBC, IFNAM, UBBC, NNBBC,
     1             WARNING, 'BBCGEO' )
C
      RETURN 
      END
