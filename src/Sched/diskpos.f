      SUBROUTINE DISKPOS( ISCN, ISTA, LASTISCN )
Cf2py intent(in) ISCN, ISTA, LASTISCN
C
C     Routine for SCHED called by SCHTAPE that deals with the
C     accounting for disk based systems.  Basically it just gets
C     the byte count at the end of the scan.
C
C     Distinguish between core and extras.  But need to only count
C     extras that are before or after the core, not during.
C     Don't try to separate core from extra scans (see PREEMPT) here
C     because this routine is called before the handling of PREEMPT.
C
C     Note that GBYTES here is the value at the end of the scan when
C     this routine returns.  In the VEX file, the value shown needs
C     to be for the start of the scan.  AARG.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER   KS, ISCN, ISTA, KSTA, LASTISCN(MAXSTA)
      DOUBLE PRECISION  RECSTART
      REAL      RECTIME
C ---------------------------------------------------------------------
C     Initialize GBYTES and EGBYTES to the value at the start
C     of the scan.  Will add this scan below.
C
      IF( LASTISCN(ISTA) .EQ. 0 ) THEN
         GBYTES(ISCN,ISTA) = 0
      ELSE
         GBYTES(ISCN,ISTA) = GBYTES(LASTISCN(ISTA),ISTA)
      END IF
C
C     Now add on the contribution from this scan.
C
C     We've already selected on STASCN and NOREC before this routine
C     was called.
C
C     This has gotten somewhat more complicated the fact that 
C     STARTJ(ISCN) - TPSTART(ISCN,ISTA) is no longer the start of
C     recording for some VEX controlled stations.  These include
C     the VLA and the MARK5C system on the VLBA.
C     Also pay attention to NOREC.
C
      IF( NOREC(ISCN) ) THEN
         RECTIME = 0.D0
      ELSE
         KSTA = STANUM(ISTA)
         IF( USEONSRC(KSTA) ) THEN
            RECSTART = MAX( STARTJ(ISCN) - TPSTART(ISCN,ISTA), 
     1             TONSRC(ISCN,ISTA) )
         ELSE
            RECSTART = STARTJ(ISCN) - TPSTART(ISCN,ISTA)
         END IF
         RECTIME = DNINT( ( STOPJ(ISCN) - RECSTART ) * 86400.D0 )
      END IF
C
C     Get the actual bits recorded.  The factor 1000 is
C     the conversion from Mbps to Gbps.  Note that this is the
C     decimal version, not the binary one which may be what is
C     wanted.  The factor of 8 is the conversion to bytes.
C     Use WRTBPS here because it includes the FORMAT-specific
C     contribution from headers and we're interested in bytes on
C     the recording.
C
C     Nov. 1, 2013.  For some reason, the above comment previously
C     suggested not using WRTBPS here.  But the uses of GBYTES all
C     seem more appropriate with it and that is what was in the line
C     of code, so I changed the comment.  RCW.
C
      KS = NSETUP(ISCN,ISTA)
      IF( RECUSED(KS) ) THEN
         GBYTES(ISCN,ISTA) = GBYTES(ISCN,ISTA) + 
     1       RECTIME * WRTBPS(KS) / ( 1000.0 * 8. )
      END IF
C
      RETURN
      END

