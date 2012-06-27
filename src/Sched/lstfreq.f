      SUBROUTINE LSTFREQ( IPRT, SFREQ, SBBC, SBW, ICH1, NCHL, ILINE,
     1       MLL, MFRL, DOWRTF, DOWRTB, LABFR, LABBB, LABBW )
C
C     Routine to write the lists of frequencies in the summary file,
C     sch files, and maybe elsewhere.  Called by PRTSCH, PRTFREQ.
C
C     This used to be simple formatted writes in the parent routines.
C     But the need to deal with a possible large number of significant 
C     figures with the DDC made it much more complicated so it was
C     put here.
C
C     Note that for most uses, we want lists with about 8 frequencies
C     per line.  But the OMS file wants everything on one line.
C     Hence MLL and MFRL are inputs.
C
C     Parameters (all input - output is to print file).
C        IPRT - output file unit number
C        SFREQ - the channel LO sums
C        SBBC  - the channel BBC frequencies
C        SBW   - the channel bandwidths
C        ICH1  - the number of characters reserved for the line labels.
C        NCHL  - the number of channels
C        ILINE - the line of the page we're on.
C        MLL   - the maximum line length
C        MFRL  - the maximum number of frequencies per line.
C        DOWRTF - logical - write the LO sums and BB frequencies
C        DOWRTB - logical - write the bandwidths.
C        LABFR  - label for frequency line
C        LABBB  - label for the baseband freq line.
C        LABBW  - label for the bandwidth line
C
      INTEGER            IPRT, ICH1, NCHL, ILINE, INLINE
      INTEGER            NPER, NFC1, NFCN, MLL, MFRL
      INTEGER            MCH, ICH, IFREQ
      INTEGER            LEN1
      LOGICAL            DOWRTF, DOWRTB
      DOUBLE PRECISION   SFREQ(*), SBBC(*), SBW(*)
      CHARACTER          PRLINE*1024, FRTXT*16
      CHARACTER          LABFR*(*), LABBB*(*), LABBW*(*)
C -------------------------------------------------------------------
      NPER = 7
C
C     Sanity check.
C
      IF( MLL .GT. 1025 ) CALL ERRLOG( 'LSTFREQ: Line too long' )
C
C     Get the length of the first item printed to help with 
C     formatting.
C
      CALL FRCHAR( SFREQ(1), NPER, NFC1, NFCN, FRTXT )
      MCH = NFC1
C
C     Write the frequencies.
C
      IF( DOWRTF ) THEN
         PRLINE = ' '
         PRLINE = LABFR
         ICH  = ICH1
         INLINE = 0
         DO IFREQ = 1, NCHL
            CALL FRCHAR( SFREQ(IFREQ), NPER, NFC1, NFCN, FRTXT )
            NFC1 = MIN( NFC1, MCH )
            IF( ICH+NFCN-NFC1 .GT. MLL .OR. INLINE .GE. MFRL  ) THEN
               WRITE( IPRT, '( A )' ) PRLINE(1:ICH)
               ILINE = ILINE + 1
               PRLINE = ' '
               ICH = ICH1
               INLINE = 0
            END IF
            PRLINE = PRLINE(1:ICH)//'  '//FRTXT(NFC1:NFCN)
            ICH = MAX( LEN1( PRLINE ), ICH1 )
            INLINE = INLINE + 1
         END DO
         IF( ICH .GT. ICH1 ) THEN
            WRITE( IPRT, '( A )' ) PRLINE(1:ICH)
            ILINE = ILINE + 1
         END IF
C
         PRLINE = ' '
         PRLINE = LABBB
         ICH  = ICH1
         INLINE = 0
         DO IFREQ = 1, NCHL
            CALL FRCHAR( SBBC(IFREQ), NPER, NFC1, NFCN, FRTXT )
            NFC1 = MIN( NFC1, MCH )
            IF( ICH+NFCN-NFC1 .GT. MLL .OR. INLINE .GE. MFRL  ) THEN
               WRITE( IPRT, '( A )' ) PRLINE(1:ICH)
               ILINE = ILINE + 1
               PRLINE = ' '
               ICH = ICH1
               INLINE = 0
            END IF
            PRLINE = PRLINE(1:ICH)//'  '//FRTXT(NFC1:NFCN)
            ICH = MAX( LEN1( PRLINE ), ICH1 )
            INLINE = INLINE + 1
         END DO
         IF( ICH .GT. ICH1 ) THEN
            WRITE( IPRT, '( A )' ) PRLINE(1:ICH)
            ILINE = ILINE + 1
         END IF
      END IF
C
C     Write the bandwidths.
C
      IF( DOWRTB ) THEN
         PRLINE = ' '
         PRLINE = LABBW
         ICH  = ICH1
         INLINE = 0
         DO IFREQ = 1, NCHL
            CALL FRCHAR( SBW(IFREQ), NPER, NFC1, NFCN, FRTXT )
            NFC1 = MIN( NFC1, MCH )
            IF( ICH+NFCN-NFC1 .GT. MLL .OR. INLINE .GE. MFRL  ) THEN
               WRITE( IPRT, '( A )' ) PRLINE(1:ICH)
               ILINE = ILINE + 1
               PRLINE = ' '
               ICH = ICH1
               INLINE = 0
            END IF
            PRLINE = PRLINE(1:ICH)//'  '//FRTXT(NFC1:NFCN)
            ICH = MAX( LEN1( PRLINE ), ICH1 )
            INLINE = INLINE + 1
         END DO
         IF( ICH .GT. ICH1 ) THEN
            WRITE( IPRT, '( A )' ) PRLINE(1:ICH)
            ILINE = ILINE + 1
         END IF
      END IF
C
C
      RETURN
      END
