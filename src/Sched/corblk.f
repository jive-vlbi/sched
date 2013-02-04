      SUBROUTINE CORBLK
C
C     Routine to produce a summary of the times when new correlation
C     blocks will be required.  These are times when parameters like
C     speedup, bandwidth etc change.
C
C     According to Paul Dyer on Feb. 4, 2013, this is no longer needed.
C     So I will comment out the call, and require that this routine 
C     be retrieved from SVN or an old version if it is needed again.
C     First commit with this comment, then delete.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           ISCN, ISTA, ICH
      INTEGER           KS, KF, NACC
      DOUBLE PRECISION  LSTART, LSTOP, START, STOP
      DOUBLE PRECISION  LOSUM(MCHAN)
      DOUBLE PRECISION  BBBF(MCHAN), BBBW(MCHAN)
      DOUBLE PRECISION  LBW(MCHAN)
      REAL              LSAMPR, LSPEED
      INTEGER           LNCHAN, LEN1
      LOGICAL           FIRSTS
      LOGICAL           NEWNCH, NEWBW, NEWSAMPR, NEWSPD, NEWPOL
      INTEGER           YEAR1, YEAR2, DAY1, DAY2, DOY1, DOY2, JD, IM
      CHARACTER         MONTH1*3, MONTH2*3, DNAME1*3, DNAME2*3
      CHARACTER         TFORM*8, TCHAR1*8, TCHAR2*8, REASON*256
      CHARACTER         LPOL(MCHAN)*4
C
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'CORBLK starting. ' )
C
C     May need to protect against subarray conditions.
C
C     Don't do this for pointing etc.
C
      IF( .NOT. VLBITP .OR. NOSET ) RETURN
C
C     Put a title line in the sum file.
C
      WRITE( ISUM, '( 1X, /, 1X, /, A, /, 1X )' )
     1    ' CORRELATION BLOCKS REQUIRED BY SETUP CHANGES'
C
C     Initialize the relevant parameters to the first recording scan.
C
      FIRSTS = .TRUE.
      NACC = 0
      DO ISCN = SCAN1, SCANL
C
C        Initialize the flags.
C
         NEWBW = .FALSE.
         NEWNCH = .FALSE.
         NEWSAMPR = .FALSE.
         NEWSPD = .FALSE.
         NEWPOL = .FALSE.
C
C        Loop over stations - but will jump out if a change is found
C        so changes are not double counted.

         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. .NOT. NOREC(ISCN) ) THEN
               NACC = NACC + 1
C
C              Initialize the string to give the reason for a break.
C
               REASON = ' '
               REASON = ' Change due to: '
C
C              Get some setup info for this scan/station.
C
               KS     = NSETUP(ISCN,ISTA)
               KF     = FSETI(ISCN,ISTA)
               CALL FSFREQ( KF, LOSUM, BBBF, BBBW )
C
C              Save the initial parameters for the first scan.
C
               IF( FIRSTS ) THEN
                  LSTART = STARTJ(ISCN)
                  LSTOP = STOPJ(ISCN)
                  LNCHAN = NCHAN(KS)
                  LSAMPR = SAMPRATE(KS)
                  LSPEED = SPEEDUP(KS)
                  DO ICH = 1, NCHAN(KS)
                     LBW(ICH) = BBBW(ICH) 
                     LPOL(ICH) = POL(ICH,KS)
                  END DO
                  FIRSTS = .FALSE.
               END IF
C
C              Check for a change in number of channels.
C
               IF( NCHAN(KS) .NE. LNCHAN ) THEN
                  NEWNCH = .TRUE.
                  REASON = REASON(1:LEN1(REASON))//' Channels'
               END IF
C
C              Check for a change in the sample rate.
C
               IF( SAMPRATE(KS) .NE. LSAMPR ) THEN
                  NEWSAMPR = .TRUE.
                  REASON = REASON(1:LEN1(REASON))//
     1                   ' Sample rate'
               END IF
C
C              Check for a change in speedup.
C
               IF( SPEEDUP(KS) .NE. LSPEED ) THEN
                  NEWSPD = .TRUE.
                  REASON = REASON(1:LEN1(REASON))//
     1                   ' Speedup'
               END IF
C
C              Check for a change in channel bandwidth.
C
               DO ICH = 1, NCHAN(KF)
                  IF( BBBW(ICH) .NE. LBW(ICH) ) THEN
                     NEWBW = .TRUE.
                     REASON = REASON(1:LEN1(REASON))//' Bandwidth'
                     GO TO 50
                  END IF
               END DO
   50          CONTINUE
C
C              Check for a change in channel polarization.
C              This will hopefully isolate DELZN sections.
C
               DO ICH = 1, NCHAN(KS)
                  IF( POL(ICH,KS) .NE. LPOL(ICH) ) THEN
                     NEWPOL = .TRUE.
                     REASON = REASON(1:LEN1(REASON))//' Polarization'
                     GO TO 51
                  END IF
               END DO
   51          CONTINUE
C
C              Deal with a change if any were found.
C              Specify the time in a format like:
C                 2009JAN29 at 23:45:00 to 2009JAN30 at 10:00:00
C
               IF( NEWNCH .OR. NEWBW .OR. NEWSPD .OR. NEWSAMPR .OR.
     1             NEWPOL ) THEN
C
                  CALL TIMEJ( LSTART, YEAR1, DOY1, START )
                  DAY1 = DOY1
                  IM = 1
                  CALL TDATECW( YEAR1, IM, DAY1, JD, MONTH1, DNAME1 )
                  CALL UPCASE( MONTH1 )
                  TCHAR1 = TFORM( START, 'T', 0, 2, 2, '::@' )
C
                  CALL TIMEJ( LSTOP, YEAR2, DOY2, STOP )
                  DAY2 = DOY2
                  IM = 1
                  CALL TDATECW( YEAR2, IM, DAY2, JD, MONTH2, DNAME2 )
                  CALL UPCASE( MONTH2 )
                  TCHAR2 = TFORM( STOP, 'T', 0, 2, 2, '::@' )
C
                  WRITE( ISUM, 
     1              '( A, I4, A3, I2.2, 3A, I4, A3, I2.2, 2A )' )
     2              'UTTIMERANGE ', YEAR1, MONTH1, DAY1, ' at ', TCHAR1,
     3              ' to ', YEAR2, MONTH2, DAY2, ' at ', TCHAR2
                  WRITE( ISUM, '( A )' ) REASON(1:LEN1(REASON))
C
C                 Reinitialize the "current" parameters
C
                  LSTART = STARTJ(ISCN)
                  LNCHAN = NCHAN(KS)
                  LSAMPR = SAMPRATE(KS)
                  LSPEED = SPEEDUP(KS)
                  DO ICH = 1, NCHAN(KS)
                     LBW(ICH) = BBBW(ICH) 
                     LPOL(ICH) = POL(ICH,KS)
                  END DO
               END IF
C
C              Save the stop time of the scan as the possible end time
C              of the block.  A change is detected in a following scan.
C
               LSTOP = STOPJ(ISCN)
C
C              Only process one station per scan.
C
               GO TO 100
C
            END IF
C
C         
         END DO
C
C        Jump here after processing the one station of the scan.
C
  100    CONTINUE
C
C     End the scan loop.
C
      END DO
C
      IF( NACC .GE. 1 ) THEN
C
C        Write the last line.  This is always needed because any
C        previous lines written are for preceeding scans.  With 
C        old code, this was skipped if the last scan was the only
C        one in the block, which was not good.
C
         CALL TIMEJ( LSTART, YEAR1, DOY1, START )
         DAY1 = DOY1
         IM = 1
         CALL TDATECW( YEAR1, IM, DAY1, JD, MONTH1, DNAME1 )
         CALL UPCASE( MONTH1 )
         TCHAR1 = TFORM( START, 'T', 0, 2, 2, '::@' )
C     	
         CALL TIMEJ( LSTOP, YEAR2, DOY2, STOP )
         DAY2 = DOY2
         IM = 1
         CALL TDATECW( YEAR2, IM, DAY2, JD, MONTH2, DNAME2 )
         CALL UPCASE( MONTH2 )
         TCHAR2 = TFORM( STOP, 'T', 0, 2, 2, '::@' )
C     	
         WRITE( ISUM, 
     1     '( A, I4, A3, I2.2, 3A, I4, A3, I2.2, 2A )' )
     2     'UTTIMERANGE ', YEAR1, MONTH1, DAY1, ' at ', TCHAR1,
     3     ' to ', YEAR2, MONTH2, DAY2, ' at ', TCHAR2
      ELSE
         WRITE( ISUM, '( A, A )' ) '  There were no recording scans so',
     1       ' there will be no correlation.'
      END IF
C
      RETURN
      END

