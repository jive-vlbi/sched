      SUBROUTINE VXWRFQ
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the FQ = $FREQ section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Increased digits for channel bandwidth.  Not enough
C     for 62.5 kHz.  RCW Aug. 31, 2009.
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
C     Huib's local variables 
C      
      INTEGER   IFQ, KS, ICH, LPOS
      INTEGER   LEN1, NPER, NFC1, NFCN
      CHARACTER LINE*132, FRTXT*16
C ----------------------------------------------------------------------
C
      LINE = ' ' 
C
C     Write the FREQ section
C
      WRITE( IVEX, '( A, A1 )' ) '$FREQ', SEP     
      DO IFQ = 1, NFQVEX
         KS = FQISSET(IFQ)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        FQLINK(IFQ)(1:LEN1(FQLINK(IFQ))), SEP
         CALL VXSTLI( IFQ, NSTAFQ, ISTAFQ )
C     
C        first the sample rate and bits/sample
C            
         WRITE( IVEX, '( 5X, A, F9.3, A, A1, 2X, A1, A, I1, A )' ) 
     1        'sample_rate = ',SAMPRATE(KS),' Ms/sec', SEP, COM,
     2        ' (', BITS(1,KS) ,'bits/sample)'
C
C        channel definitions, find number of BBC's on the fly
C
         IF( NCHAN(KS) .NE. NVXCHN(IFQ) ) CALL ERRLOG(' VXWRFQ: '//
     1        'NCHAN in set does not match NCHAN in FQ def ')
         DO ICH = 1, NVXCHN(IFQ)
C
C           most things set through IFQ now, use KS for items fixed in Set
C
            LINE = ' '
            WRITE( LINE(6:16), '( A )' ) 'chan_def = '
C
C           must find band somewhere, is in crd file, but it is only
C           there to find right band for source catalogue; not important.
C               
            LPOS = LEN1(LINE)+1
            WRITE( LINE(LPOS:LPOS+3), '( A1, A1 )' ) 
     1          BLN, COL
C
C           next is Sky freq:
C           Make the number of digits depend on how many are needed.
C
            NPER = 7
            CALL FRCHAR( VXLOSUM(ICH,IFQ), NPER, NFC1, NFCN, FRTXT )
            IF( NFCN .GT. NPER + 6 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I3, A, I3, A, F15.8)' )
     1            ' Channel ', ICH, ' of freq set ', IFQ, 
     2            ' not an integer Hz: ', VXLOSUM(ICH,IFQ)
               CALL ERRLOG( MSGTXT )
            END IF
C
C           Now actually write the frequency.  Allow plenty of
C           room.  The next LPOS will truncate the blanks.
C
            LPOS = LEN1(LINE)+1
            WRITE( LINE(LPOS:LPOS+25), '( A, A, A )' ) 
     1             FRTXT(1:NFCN),' MHz ', COL
C
C           and sideband (net)
C
            LPOS = LEN1(LINE)+1
            WRITE( LINE(LPOS:LPOS+3), '( 1X, A1, 1X, A1 )' ) 
     1           VXNETSID(ICH,IFQ), COL
C
C           the channel width
C
            LPOS = LEN1(LINE)+1
            IF( VXBBFILT(ICH,IFQ) .GE. 1.0 ) THEN
               WRITE( LINE(LPOS:LPOS+12), '( F7.2, 1X, A3, 1X, A1 )')
     1             VXBBFILT(ICH,IFQ), 'MHz', COL
            ELSE
               WRITE( LINE(LPOS:LPOS+12), '( F7.3, 1X, A3, 1X, A1 )')
     1             VXBBFILT(ICH,IFQ) * 1000.0, 'kHz', COL
            END IF
C     
C           and channel and BBC links
C
            LPOS = LEN1(LINE)+1
            WRITE( LINE(LPOS:LPOS+16), '( 1X, A1, A2, I2.2, 1X, A1, 
     1           1X, A1, A3, I2.2, 1X, A1 )' ) LNK, 'CH', ICH, COL, 
     2           LNK, 'BBC', BBC(ICH,KS), COL
C
C           write the PH-link that is involved
C
            LPOS = LEN1(LINE)+1
            WRITE( LINE(LPOS:LPOS+7), '( 1X, A1, A5, A1 )' )
     1           LNK, TONLNK(TONCHN(ICH,IFQ),FQTOPH(IFQ)), SEP
C
C           add polarization in a comment:
C
            LPOS = LEN1(LINE)+1
            WRITE( LINE(LPOS:LPOS+5), '( 1X, A1, A1, A2 )' )
     1          COM, POL(ICH,KS)(1:1), 'cp'
C
C           It's quite a nice line, write to file
C
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
      ENDDO
C
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END

