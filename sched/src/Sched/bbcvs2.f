      SUBROUTINE BBCVS2( KS )
C
C     Routine for SCHED, called by SETBBC, that sets the BBC
C     assignments for S2 systems attached to VLBA DAR's.
C     Only allow IF's A and C.  That way it works for both VLBA
C     and VLBAG systems.
C
C     The code needed, once the arrays are established, is the same
C     as for BBCM4 (Mark IV), so it has been put in BBCALT, which
C     is shared.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    I, KS
      INTEGER    MAXBBC, MAXIF, NNBBC
C
C   ******   Set MAXBBC smaller once the catalogs are changed.
C            Dec2004  What does this mean?  What catalog change?
C
      PARAMETER  (MAXBBC=14)
      PARAMETER  (MAXIF=2)
      INTEGER    IFBBCD(MAXBBC,MAXIF), IFBBCS(MAXBBC,MAXIF)
      CHARACTER  IFNAM(MAXIF)*2, WARNING*4
      LOGICAL    UBBC(MAXBBC)
      DATA  (IFBBCD(I,1),I=1,MAXBBC) / 1,0,1,0,1,0,1,0,1,0,1,0,1,0 /
      DATA  (IFBBCD(I,2),I=1,MAXBBC) / 0,1,0,1,0,1,0,1,0,1,0,1,0,1 /
      DATA  (IFNAM(I),I=1,MAXIF) / 'C', 'A' /
      DATA  (IFBBCS(I,1),I=1,MAXBBC) / 1,0,1,0,1,0,1,0,1,0,1,0,1,0 /
      DATA  (IFBBCS(I,2),I=1,MAXBBC) / 1,0,1,0,1,0,1,0,1,0,1,0,1,0 /
C
C     Why can the even BBCs not be used for single pol obs?
C
C     The manual claims that there is information in the document 
C     ``Compatibility of S2 and VSOPT recordings at S2 and VSOPT
C     Correlators'' by R.\ Wietfeldt, available at
C     ftp::s2.sgl.ists.ca:pub/s2/svlbi/s2vsop\_compat\_memo\_v*.ps.Z.
C     but the link appears to have expired as of the end of 2004.
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
      WARNING = 'S2'
C
C     Set the number of BBC's.
C
      NNBBC = NBBC(ISETSTA(KS))
C
C     Call BBCALT to do the work.
C
      IF( DUALPOL(KS) ) THEN
         CALL BBCALT( KS, MAXBBC, MAXIF, IFBBCD, IFNAM, UBBC, NNBBC,
     1             WARNING, 'BBCVS2-1' )
      ELSE
         CALL BBCALT( KS, MAXBBC, MAXIF, IFBBCS, IFNAM, UBBC, NNBBC,
     1             WARNING, 'BBCVS2-2' )
      END IF
C
      RETURN 
      END
