      SUBROUTINE PRTFREQ( KS, IUNIT )
C
C     Subroutine for SCHED called by PRTSET that writes frequency
C     related information from the setup file into the .sum file.
C
C     It is also used to write to the log file when there are 
C     problems detected with the setup.  IUNIT is the output
C     unit number.  KS is the setup group.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER            I, IUNIT, KS, KF, NNCHAN, KSCN, LEN1, IC
      INTEGER            IP, IPC, ISSTA
      DOUBLE PRECISION   BBCFREQ(MCHAN), BBCBW(MCHAN)
      DOUBLE PRECISION   LOSUM(MCHAN)
      LOGICAL            GOTIT
      CHARACTER          RESULT*100
C----------------------------------------------------------------------
      NNCHAN = NCHAN(KS)
      IF( DEBUG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, 3I7 )' ) 'PRTFREQ starting. ', 
     1       NNCHAN, NFSET, MAXPC
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     Write to the output file.
C
      IF( NNCHAN .GT. 0 ) THEN
         WRITE( IUNIT, '( 1X )' )
         WRITE( IUNIT, '( 2X, A, 8( F10.2, :), /, 10X, 8( F10.2, :), ' 
     1           // ' /, 10X, 8( F10.2, :), /, 10X, 8( F10.2, :) )' )
     2           ' 1st LO=', ( FIRSTLO(I,KS), I = 1, NNCHAN )
         WRITE( IUNIT, '( 2X, A, 8( 9X, A1, :), /, 10X, 8( 9X, A1, :), '
     1           // ' /, 10X, 8( 9X, A1, :), /, 10X, 8( 9X, A1, :) )' )
     2           ' Net SB=', ( NETSIDE(I,KS), I = 1, NNCHAN )
         WRITE( IUNIT, '( 2X, A, 8(6X,A4,:), /, 10X, 8( 6X, A4, :), '
     1           // '/, 10X, 8( 6X, A4, :), /, 10X, 8( 6X, A4, :) )' )
     2           ' Pol.  =', ( POL(I,KS), I = 1, NNCHAN )
         WRITE( IUNIT, '( 2X, A, 8( I10, :), /, 10X, 8( I10, :), ' 
     1           // ' /, 10X, 8( I10, :), /, 10X, 8( I10, :) )' )
     2           ' BBC   =', ( BBC(I,KS), I = 1, NNCHAN )
         WRITE( IUNIT, '( 2X, A, 8( 9X, A1, :), /, 10X, 8( 9X, A1, :), '
     1           // ' /, 10X, 8( 9X, A1, :), /, 10X, 8( 9X, A1, :) )' )
     2           ' BBC SB=', ( SIDEBD(I,KS), I = 1, NNCHAN )
         WRITE( IUNIT, '( 2X, A, 8( 8X, A2, :), /, 10X, 8( 8X, A2, :), '
     1           // ' /, 10X, 8( 8X, A2, :), /, 10X, 8( 8X, A2, :) )' )
     2           ' IF    =', ( IFCHAN(I,KS), I = 1, NNCHAN )
      ELSE
         CALL WLOG( 0, 'PRTFREQ:  No channels!  Program problem.' )
      END IF
C
C     Some non-channel dependent data for vlba stations.
C
      IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
         WRITE( IUNIT, '( 1X, /, 2X, A, 4(5X,A4) )' )
     1           ' VLBA FE=', (FE(I,KS),I=1,4)
         WRITE( IUNIT, '( 2X, A, F6.1, 2F9.1 )' )
     1           ' VLBA Synth=', (SYNTH(I,KS),I=1,3)
      END IF
C
C     List the frequency sets.
C
      IF( NFSET .GT. 0 ) THEN
         WRITE( IUNIT, '( 1X, /, A, A )' ) '  The following ',
     1         'frequency sets based on this setup were used.'
         DO KF = 1, NFSET
            IF( FSETKS(KF) .EQ. KS ) THEN
               KSCN = FSETSCN(KF)
C
C              Get the frequencies to be used for this frequency set.
C
               CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW )
C        
C              Write a label that depends on whether this is a 
C              default set.
C        
               IF( FREQ(1,KSCN) .NE. 0.D0 .OR.
     1             BW(1,KSCN) .NE. 0.D0 ) THEN
                  WRITE( MSGTXT, '( A, I4, A )' )
     1                '  Frequency Set:', KF, 
     2                '  Based on FREQ, BW, and/or DOPPLER in schedule.'
C
C                 See if a warning is needed concerning IF ranges.
C
                  CALL CHKIF( KS, RESULT, LOSUM, BBCBW )
C
C                 Warn directly of a problem.  Later also put warning
C                 in the summary file.
C
                  IF( RESULT .NE. 'OK' ) THEN
                     SETMSG = ' '
                     WRITE( SETMSG, '( A, I5, A )' )
     1                    'PRTFREQ:  **** Warning - in frequency set ',
     2                    KF, ' some requested frequencies are '
                     CALL WLOG( 1, SETMSG )
                     CALL WLOG( 1, '         outside the available '//
     1                     'IFs.  See summary file for details.' )
                  END IF
               ELSE
                  WRITE( MSGTXT, '( A, I4, A )' )
     1                '   Frequency Set:', KF, '  Setup file default.'
                  RESULT = 'OK'
               END IF
C        
C              Add the pcal sets used.
C        
               IF( DAR(ISETSTA(KS))(1:4) .EQ. 'VLBA' ) THEN
C        
                  IC = LEN1( MSGTXT ) + 1
                  WRITE( MSGTXT(IC:LEN(MSGTXT)), '( A )' ) 
     1                 '  Used pcal sets:'
C        
                  IF( FSETPS(1,KF) .NE. 0 ) THEN
                     IC = LEN1( MSGTXT ) + 1
                     WRITE( MSGTXT(IC:IC+3), '( I4 )' ) FSETPS(1,KF)
                  END IF
C        
                  IF( FSETPS(2,KF) .NE. 0 ) THEN
                     IC = LEN1( MSGTXT ) + 1
                     WRITE( MSGTXT(IC:IC+3), '( I4 )' ) FSETPS(2,KF)
                  END IF
C        
                  IF( FSETPS(3,KF) .NE. 0 ) THEN
                     IC = LEN1( MSGTXT ) + 1
                     WRITE( MSGTXT(IC:IC+3), '( I4 )' ) FSETPS(3,KF)
                  END IF
C        
               ELSE
C
C                  Take out this statement.  It just worried users who
C                  were expecting detection at the correlator.
C
C                  IC = LEN1( MSGTXT ) + 1
C                  WRITE( MSGTXT(IC:LEN(MSGTXT)), '( A )' ) 
C     1             '  No pcal detection.'
               END IF
C        
C              Write the header line and a warning if there is one.
C
               WRITE( IUNIT, '( 1X, /, A )' ) MSGTXT(1:LEN1(MSGTXT))
               IF( RESULT .NE. 'OK' ) THEN
                  WRITE( IUNIT, '( 1X, A, A )' ) '   **** Warning ',
     1               RESULT(1:LEN1(RESULT) )
               END IF
C        
C              Write the frequencies etc for this frequency set.
C        
               IF( NNCHAN .GT. 0 ) THEN
                  WRITE( IUNIT, '( A, 8F10.2,:, 6(/,11X,8(F10.2,:)) )' )
     1             '   LO sum= ', ( LOSUM(I), I  =1, NNCHAN )
                  WRITE( IUNIT, '( A, 8F10.2,:, 6(/,11X,8(F10.2,:)) )' )
     1             '   BBC fr= ', ( BBCFREQ(I), I = 1, NNCHAN )
                  WRITE( IUNIT, '( A, 8F10.3,:, 6(/,11X,8(F10.3,:)) )' )
     1             '   Bandwd= ', ( BBCBW(I), I = 1, NNCHAN )
               ELSE
                  CALL WLOG( 0, 'PRTFREQ:  Still no channels' )
               END IF
            END IF
         END DO
      ELSE
         IF( IUNIT .NE. 6 .AND. IUNIT .NE. ILOG ) THEN
            MSGTXT = 'PRTFREQ:  No frequency sets!  Program problem.'
            CALL WLOG( 1, MSGTXT )
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         ELSE
            CALL WLOG( 0, 
     1          '      The frequency sets are not yet known.' )
            CALL WLOG( 0,
     1          '      The following are the setup file frequencies.' )
            WRITE( IUNIT, '( A, 8F10.2,:, 6(/,11X,8(F10.2,:)) )' )
     1         '   Ref fr= ', ( FREQREF(I,KS), I  =1, NNCHAN )
            WRITE( IUNIT, '( A, 8F10.2,:, 6(/,11X,8(F10.2,:)) )' )
     1         '   BBC fr= ', ( BBSYN(I,KS), I = 1, NNCHAN )
            WRITE( IUNIT, '( A, 8F10.3,:, 6(/,11X,8(F10.3,:)) )' )
     1         '   Bandwd= ', ( BBFILT(I,KS), I = 1, NNCHAN )
         END IF
      END IF
C
      ISSTA = ISETSTA(KS)
      IF( ISSTA .EQ. 0 ) THEN
         CALL WLOG( 1, 'PRTFREQ:  Do not know stations yet.  '//
     1      'Cannot check pulse cal sets.' )
      ELSE IF( DAR(ISSTA)(1:4) .EQ. 'VLBA' ) THEN
C
C        Pulse cal information.
C
         WRITE( IUNIT, '( 1X, /, A )' ) 
     1     '  The following pulse cal sets were used with this setup:'
C
         DO IP = 1, NPSET
            GOTIT = .FALSE.
            KF = 1
            DO WHILE( KF .LE. NFSET .AND. .NOT. GOTIT )
               IF( FSETKS(KF) .EQ. KS ) THEN
                  DO IPC = 1, 3
                     IF( FSETPS(IPC,KF) .EQ. IP ) GOTIT = .TRUE.
                  END DO
               END IF
               KF = KF + 1
            END DO
C
C           This pcal set was used with this setup.
C
            IF( GOTIT .AND. MAXPC .GT. 0 ) THEN
               WRITE( IUNIT, '( 1X, /, A, I4, A, A )' )
     1          '   Pulse cal detection set:', IP, 
     2             '  PCAL = ', PSPCAL(IP)
               WRITE( IUNIT, '( 2X, A, 8( A6, :), /, 11X, 8( A6, :) )' )
     1           '  PCALXB1=', ( PSX1(I,IP), I = 1, MAXPC )
               WRITE( IUNIT, '( 2X, A, 8( A6, :), /, 11X, 8( A6, :) )' )
     1           '  PCALXB2=', ( PSX2(I,IP), I = 1, MAXPC )
               WRITE( IUNIT, '( 2X, A, 8( I6, :), /, 11X, 8( I6, :) )' )
     1           '  PCALFR1=', ( PSFR1(I,IP), I = 1, MAXPC )
               WRITE( IUNIT, '( 2X, A, 8( I6, :), /, 11X, 8( I6, :) )' )
     1           '  PCALFR2=', ( PSFR2(I,IP), I = 1, MAXPC )
            ELSE IF( GOTIT .AND. MAXPC .LE. 0 ) THEN
               CALL WLOG( 1, 
     1             'PRTFREQ: No pulse cal sets!  Program problem.' )
               WRITE( IUNIT, '( A )' )
     1             'PRTFREQ: No pulse cal sets!  Program problem.'
C            
            END IF
         END DO
C
      END IF
C
      RETURN
      END
