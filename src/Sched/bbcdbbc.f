      SUBROUTINE BBCDBBC( KS )
C
C
C     Routine for Sched called by SETBBC that assigns video converters
C     for Gino Tuccari's DBBC system, as used at Effelsberg.
C
C     There are two personalities for the DBBC. The difference is that the 
C     IF selection for the BBCs by unit number is somewhat more restricted.
C     The PFB personality is essentially the same as the RDBE, so just run
C     through the same code for that.  The DDC is quite different.
C
C     CR March, 2013: There are essentially 3 flavours (DBBCVER) of DBBC
C     with different IF->BBC mappings. They are described below as
C     'astro', 'geo' or 'hybrid'. Note that the astro flavour can have
C     either 2 or 4 IFs, but for now we assume 4 IFs are available. The
C     code below forms the masks for allowed IF->BBC mappings then
C     passes that to BBCALT in common with MkIV, etc.
C
C     Each IF also has 4 inputs, one of which we must select to get the
C     right signal out.
C
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER     KS, I
      INTEGER     MAXBBC, MAXIF
      PARAMETER   (MAXBBC=16)
      PARAMETER   (MAXIF=4)
      LOGICAL     UBBC(MAXBBC)
      CHARACTER   IFNAM(MAXIF)*2
      CHARACTER   WARNING*4, MYDBBCVER*8
      INTEGER     IFBBC(MAXBBC, MAXIF), MIF
      DATA        (IFNAM(I),I=1,MAXIF) / 'A', 'B', 'C', 'D' /
C Note that IFNAM is also defined in CHKDBBC (should match!)

C
C -------------------------------------------------------------------  
C     Software check:
C
      IF( DEBUG ) CALL WLOG( 1, 'BBCDBBC: Starting' )
C
      MYDBBCVER = DBBCVER(ISETSTA(KS))
C
C     Check against the maximum number of BBCs.
C
      IF( NBBC(ISETSTA(KS)) .GT. MAXBBC ) THEN
         WRITE( MSGTXT, '( 3A, I4 )' )
     1       'BBCDBBC: Number of VCs at ', SETSTA(1,KS), 
     2       ' Larger than maximum expected: ', MAXBBC
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '   Catalog or programming problem ' )
         CALL ERRSET( KS )
      END IF
C
C     Which BBC can see which IF is dependent on the DBBC flavour. 
C     Put the (complicated) IF->BBC mapping in external routine so it
C     can be reused by chkdbbc.
C
      CALL IFDBBC( MYDBBCVER, MAXBBC, MAXIF, IFBBC, MIF )
C
      WARNING = 'DBBC'
C
C     Use the standard routine to set the BBC->IF mappings.
C
      CALL BBCALT( KS, MAXBBC, MIF, IFBBC, IFNAM, UBBC, MAXBBC,
     1          WARNING, 'BBCDBBC')
C
      RETURN 
      END
