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
      INTEGER            I, IUNIT, KS, JS, KF, NNCHAN, KSCN, LEN1, IC
      INTEGER            JF, ICH, ICH1, ILINE, CRSETC(MAXCHN), KSTA
      DOUBLE PRECISION   BBCFREQ(MCHAN), BBCBW(MCHAN)
      DOUBLE PRECISION   LOSUM(MCHAN)
      CHARACTER          RESULT*100, FSMATSTR*132
      INTEGER            CRDN
      DOUBLE PRECISION   CRDF(MCHAN), CRDB(MCHAN), CRDLOSUM(MCHAN)
      CHARACTER          CRDS(MCHAN)*1
CC----------------------------------------------------------------------
      NNCHAN = NCHAN(KS)
      KSTA = ISETSTA(KS)
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
         WRITE( IUNIT, '( 2X, A, 8( 9X, A1, :), /, 10X, 8( 9X, A1, :), '
     1           // ' /, 10X, 8( 9X, A1, :), /, 10X, 8( 9X, A1, :) )' )
     2           ' IF SB =', ( SIDE1(I,KS), I = 1, NNCHAN )
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
         IF( ABS( SYNTH(1,KS) * 10.D0 - DNINT( SYNTH(1,KS) * 10.D0 ) )
     1       .GT. 1.D-6 .OR.
     2       ABS( SYNTH(2,KS) * 10.D0 - DNINT( SYNTH(2,KS) * 10.D0 ) )
     3       .GT. 1.D-6 ) THEN
            WRITE( IUNIT, '( 2X, A, 2F12.5, F9.1 )' )
     1           ' VLBA Synth=', (SYNTH(I,KS),I=1,3)
         ELSE
            WRITE( IUNIT, '( 2X, A, F6.1, 2F9.1 )' )
     1           ' VLBA Synth=', (SYNTH(I,KS),I=1,3)
         END IF
      END IF
C
C     List the frequency sets.  Try to catch all sets that use this
C     basic setup, even if not used by the station of this setup.
C     LISTKS was set in SCHSUM to record which setup of those that
C     are similar got fully listed.  FSETKS records which setup a
C     frequency set comes from.  Require that LISTKS be the same 
C     for the current setup KS and the one a frequency set is from
C     to be sure they are basically the same.  Meanwhile, actually
C     list the frequency set information for the lowest numbered
C     frequency set of those that are the same, as recorded in
C     FSSAME which was set in GETFSET.
C
      IF( NFSET .GT. 0 ) THEN
         WRITE( IUNIT, '( 1X, /, A, A )' ) '  The following ',
     1         'frequency sets based on these setups were used.'
         IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
            WRITE( IUNIT, '( A, A )' )
     1         '     See the crd files for VLBA legacy system ',
     2         'setups and pcal detection details.'
         END IF
         DO KF = 1, NFSET
            JS = FSETKS(KF)
            IF( LISTKS(JS) .EQ. LISTKS(KS) .AND. 
     1          FSSAME(KF) .EQ. KF ) THEN
               KSCN = FSETSCN(KF)
C
C              Get the frequencies to be used for this frequency set.
C
               CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW,
     1                      CRDN, CRDF, CRDB, CRDS, CRDLOSUM, CRSETC )
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
C              The details of the pcal detection on the VLBA used to
C              be included here, but that is getting messier as the
C              legacy system channels are no longer the same frequency
C              or bandwidth as the main schedule channels.  So remove
C              that information.  In fact, the whole concept of 
C              pcal sets is being removed.
C        
C              Note if used with CRD parameters.
C
               IF( GOTCRD(KSCN) ) THEN
                  IC = LEN1( MSGTXT ) + 1
                  WRITE( MSGTXT(IC:LEN(MSGTXT)), '( A )' ) 
     1                 '  Used with CRDFREQ or CRDDOP.'
               END IF                  
C
C              Add the pcal state for this freqency set.
C
               IC = LEN1( MSGTXT ) + 1
               WRITE( MSGTXT(IC:LEN(MSGTXT)), '( A, A )' ) 
     1                 '  Used with PCAL = ', FSPCAL(KF)
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
C              ILINE is a dummy here.
C        
               IF( NNCHAN .GT. 0 ) THEN
                  ICH1 = 11
                  ILINE = 1
                  CALL LSTFREQ( IUNIT, LOSUM, BBCFREQ, BBCBW, ICH1, 
     1                NNCHAN, ILINE, 100, 8, .TRUE., .TRUE., 
     2                '   LO sum= ',
     3                '   BBC fr= ',
     4                '   Bandwd= ' )
C
               ELSE
                  CALL WLOG( 0, 'PRTFREQ:  Still no channels' )
               END IF
C
C              Deal with the crd information for VLBA stations when
C              using the RDBE.
C
               IF( CONTROL(KSTA) .EQ. 'VLBA' .AND. 
     1             DBE(KS)(1:4) .EQ. 'RDBE' ) THEN
                  WRITE( IUNIT, '( A, I2, A, 8I3 )' )
     1                '   VLBA legacy crd files using ', CRDN, 
     2                ' channels based on RDBE channels: ',
     3                (CRSETC(I),I=1,CRDN)
                  WRITE( IUNIT, '( A, 8F10.2 )' )
     1                '   CRD fr= ', (CRDF(I),I=1,CRDN)
                  WRITE( IUNIT, '( A, 8F10.2 )' )
     1                '   CRD bw= ', (CRDB(I),I=1,CRDN)
               END IF
C
C              Write the matching frequency sets.
C
               WRITE( FSMATSTR, '( A )' )  
     1              '    Matching frequency sets:'
               ICH = LEN1( FSMATSTR ) + 1
               DO JF = 1, NFSET
                  IF( FSSAME(JF) .EQ. KF .AND. ICH .LE. 129 ) THEN
                     WRITE( FSMATSTR(ICH:ICH+4), '( I4 )' ) JF
                     ICH = ICH + 4
                  END IF
               END DO    
               WRITE( IUNIT, '( A )' ) FSMATSTR(1:LEN1(FSMATSTR))
            END IF
         END DO
C
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
C     The pulse cal information has been removed.  For the VLBA legacy
C     system, the user will need to check the crd files for what got
C     used.
C
      RETURN
      END
