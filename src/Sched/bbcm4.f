      SUBROUTINE BBCM4( KS )
C
C
C     Routine for Sched called by SETBBC that assigns video converters
C     for Mark IV systems.
C
C     Mark IV is a bit more complicated than VLBA because there
C     is a fixed wiring between video converters (MarkIII/IV
C     term for what the VLBA (and SCHED) call a BBC) and the IFs.
C     IFs 1N and 1A are attached to the odd BBCs while 2N and 2A
C     are attached to the even BBC's.  Use M4VC1 and M4VC2 to
C     keep track of the next available odd and even BBC.
C
C     On top of that, no more than 8 BBCs may use FORMAT=MKIV1:4.
C     A mistake on this one will be caught by CHK4DAR.  But try
C     to prevent for many cases by setting NNBBC to 8 for bit rates 
C     less than or equal to 512 Mbps and for sample rates of 32 Mbps.
C     I don't like waiting until the format is being set to apply
C     this constraint.  It affects to much else.  So prevent what
C     I can here, and force the user to worry about it later if 
C     something slips through.
C
C     If the experiment has only one polarization, it may need
C     more than just the odd or even BBC's to cover all the
C     channels.  In this case, different BBC's will be assigned
C     different IFCHANs to get the same signal!  There is some
C     hoop jumping to set this up.
C
C     The original version of this routine was rather complicated.
C     A more general scheme was needed for the geodetic wired VLBA
C     systems, so that is used (in BBCALT) for both.
C     This routine and BBCGEO just set up the data arrays.
C
C     It seems that many EVN stations only properly maintain the
C     lower 8 BBC's so they want BBC assignments restricted to those
C     when possible.  This means that single polarization 
C     observations should pick up the alternate inputs to stay 
C     below 8.  I think I can deal with the variety of implications
C     by claiming there are only "N" IFs for dual polarization 
C     observations but both "N" and "A" IFs for single polarization
C     observations.  In BBCALT, whichever is available to a given
C     BBC will be taken.  In the usual case, channel N will end up
C     on BBC N for single polarization, but there will be slight
C     scrambling for dual (because RCP usually comes first, but
C     that would have to be on an A IF if the BBCs were in order.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    I, KS, NNBBC
      INTEGER    MAXBBC, MAXIF, NIF
      PARAMETER  (MAXBBC=14)
      PARAMETER  (MAXIF=4)
      INTEGER    IFBBC(MAXBBC,MAXIF)
      CHARACTER  IFNAM(MAXIF)*2
      LOGICAL    UBBC(MAXBBC)
      DATA  (IFBBC(I,1),I=1,MAXBBC) / 1,0,1,0,1,0,1,0,1,0,1,0,1,0 /
      DATA  (IFBBC(I,2),I=1,MAXBBC) / 0,1,0,1,0,1,0,1,0,1,0,1,0,1 /
      DATA  (IFBBC(I,3),I=1,MAXBBC) / 1,0,1,0,1,0,1,0,1,0,1,0,1,0 /
      DATA  (IFBBC(I,4),I=1,MAXBBC) / 0,1,0,1,0,1,0,1,0,1,0,1,0,1 /
      DATA  (IFNAM(I),I=1,MAXIF) / '1N', '2N', '1A', '2A' /
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
C     Set the number of BBC's.  There is a restriction in the Mark IV
C     systems that 1:4 fan out cannot be used on VCs higher than 9.
C     Therefore, claim that the maximum number of VCs is 8 if using
C     MKIV1:4 or using a mode that is likely to use MKIV1:4.  This
C     protection won't be perfect, but should work much of the time.
C
      IF( FORMAT(KS) .EQ. 'MKIV1:4' .OR. 
     1  ( DAR(ISETSTA(KS)) .EQ. 'MKIV' .AND. SAMPRATE(KS).GE.32.0 .AND.
     2  NCHAN(KS) * BITS(1,KS) * SAMPRATE(KS) .LE. 512.0 ) ) THEN
         NNBBC = MIN( NBBC(ISETSTA(KS)), 8 )
      ELSE
         NNBBC = NBBC(ISETSTA(KS))
      END IF
C
C     Deal with the desire to stay in lower 8 BBCs.
C
      IF( DUALPOL(KS) ) THEN
         NIF = 2
      ELSE
         NIF = 4
      END IF
C
C     Call BBCALT to do the work.
C
      CALL BBCALT( KS, MAXBBC, NIF, IFBBC, IFNAM, UBBC, NNBBC, ' ',
     1     'BBCM4' )
C
      RETURN 
      END
