      SUBROUTINE DISKPOS( ISCN, ISTA, LASTISCN )
C
C     Routine for SCHED called by SCHTAPE that deals with the
C     accounting for disk based systems.  Basically it just gets
C     the byte count at the end of the scan.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER   KS, ISCN, ISTA, LASTISCN(MAXSTA)
      REAL      RECTIME, TBR
C ---------------------------------------------------------------------
C     Initialize GBYTES.
C
      IF( LASTISCN(ISTA) .EQ. 0 ) THEN
         GBYTES(ISCN,ISTA) = 0
      ELSE
         GBYTES(ISCN,ISTA) = GBYTES(LASTISCN(ISTA),ISTA)
      END IF
C
C     Now add on the contribution from this scan.  The 1000 is
C     the conversion from Mbps to Gbps.  8 is the conversion to bytes.
C     1.008 is for headers.
C
C     We've already selected on STASCN and NOREC before this routine
C     was called.
C
      RECTIME = ( STOPJ(ISCN) - STARTJ(ISCN) + TPSTART(ISCN,ISTA) )
      KS = NSETUP(ISCN,ISTA)
      IF( RECUSED(KS) ) THEN
         TBR =  NCHAN(KS) * BITS(1,KS) * SAMPRATE(KS)
         GBYTES(ISCN,ISTA) = GBYTES(ISCN,ISTA) + 
     1       RECTIME * 86400.0 * TBR * 1.008 / ( 1000.D0 * 8 )
      END IF
C
      RETURN
      END
