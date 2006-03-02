      SUBROUTINE CHKSOC
C
C     Routine for SCHED that checks some parameters if the Socorro
C     correlator is to be used.  This includes that some of the cabling 
C     restrictions on the Socorro VLBA correlator are not violated.
C     See the comments in the code for individual cases.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER        KS, I, J, IP, POLERR, ROLLERR, IGRP, JGRP
      INTEGER        IFFT, IAVG, MINFFT
      LOGICAL        SPMOD2, PADLOSS
      CHARACTER      CPOL*3
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKSOC: starting.' )
C
C     First, only do this if the experiment is to be processed
C     on the VLBA correlator.
C
      IF( ( CORREL(1:7) .EQ. 'SOCORRO' .OR. CORREL(1:4) .EQ. 'VLBA' ) 
     1      .AND. .NOT. NOTAPE ) THEN
C
C        The ith tracks from each VLBA track group cannot be routed
C        to the same delay center.  This restricts where channels
C        that are to be polarization pairs can be recorded.
C
C        If polarization processing is requested, identify the 
C        polarization pairs in each setup and make sure they have
C        allowed track assignments.
C
C        Skip over any setups with formats other than Mark IV or
C        or VLBA.  Warn in such cases, but assume a translator
C        will be used.
C
         POLERR = 0
         ROLLERR = 0
         IF( CORPOL ) THEN
            DO KS = 1, NSET
               IF( FORMAT(KS)(1:4) .EQ. 'VLBA' .OR.
     1             FORMAT(KS)(1:7) .EQ. 'MARKIII' .OR. 
     2             FORMAT(KS)(1:4) .EQ. 'MKIV' ) THEN
                  DO I = 1, NCHAN(KS) - 1
                     DO J = I + 1, NCHAN(KS)
                        IF( FREQREF(I,KS) .EQ. FREQREF(J,KS) .AND. 
     1                      NETSIDE(I,KS) .EQ. NETSIDE(J,KS) .AND.
     2                      POL(I,KS) .NE. POL(J,KS) ) THEN
                           SPMOD2 = FANOUT(KS) .EQ. 4.0 .AND. 
     1                              BITS(1,KS) .EQ. 2
                           IF( SPMOD2 ) THEN
C     	       
C                             This is special mode 2 (Wrobel mode). Such
C                             observations use a whole head group to
C                             record a channel so the restrictions below
C                             are automatically violated.  A hardware
C                             fix was provided to get around this.  The
C                             remaining restriction is that, with roll
C                             by 16, the two channels must be in the
C                             same roll group (either both even or both
C                             odd.
C     	       
                              IF( BARREL(KS) .EQ. 'roll_16' .OR.
     1                            BARREL(KS) .EQ. 'roll_auto' ) THEN
                                 DO IP = 1, TAPEMODE(KS)
                                    IGRP = MOD( TRACK(I,IP,KS), 2 )
                                    JGRP = MOD( TRACK(J,IP,KS), 2 )
                                    IF( IGRP .NE. JGRP ) ROLLERR = KS
                                 END DO
                              END IF
                           ELSE
                              DO IP = 1, TAPEMODE(KS)
C     	       
C                                Check that the tracks are not from the
C                                same numbered head of different groups.
C                                IGRP and JGRP are the head number 
C                                within the head group.
C     	       
                                 IGRP = INT( (TRACK(I,IP,KS) - 2 ) / 2 )
                                 IGRP = MOD( IGRP, 8 )
                                 JGRP = INT( (TRACK(J,IP,KS) - 2 ) / 2 )
                                 JGRP = MOD( JGRP, 8 )
                                 IF( IGRP .EQ. JGRP ) POLERR = KS
C     	       
C                                The tracks for a polarization pair must
C                                be from the same roll group.  Check. 
C                                For all cases, the two channels of a 
C                                pair must both be even or odd.  For the
C                                8 track roll, they must both be in the
C                                same 8 track group.  Note that the
C                                criteria here for roll_auto picking an
C                                8 channel roll are not complete.
C     	       
                                 IF( BARREL(KS) .NE. 'roll_off' ) THEN
                                    IGRP = MOD( TRACK(I,IP,KS), 1 )
                                    JGRP = MOD( TRACK(J,IP,KS), 1 )
                                    IF( IGRP .NE. JGRP ) ROLLERR = KS
                                 END IF
                                 IF( BARREL(KS) .EQ. 'roll_8' .OR.
     1                               ( BARREL(KS) .EQ. 'roll_auto' .AND.
     2                               ( INT( NCHAN(KS) * FANOUT(KS) * 
     3                                   BITS(I,KS) ) .LT. 16 ) ) ) THEN
                                    IGRP = INT((TRACK(I,IP,KS) - 2) / 2)
                                    IGRP = IGRP / 8
                                    JGRP = INT((TRACK(J,IP,KS) - 2) / 2)
                                    JGRP = JGRP / 8
                                    IF( IGRP .NE. JGRP ) ROLLERR = KS
                                 END IF
                              END DO
                           END IF
                        END IF
                     END DO
                  END DO
               ELSE IF( FORMAT(KS)(1:4) .NE. 'NONE' ) THEN
                  CALL WLOG( 1, 'CHKSOC:  ***** Socorro correlator ' //
     1              'requested for format: ' // FORMAT(KS) )
                  CALL WLOG( 1, '         Was this intended?' )
               END IF
            END DO
         END IF
C
C        Write the error messages.
C
         IF( POLERR .NE. 0 ) THEN
            CALL WRTMSG( 'CHKSOC', 'polerr' )
            CALL ERRSET( POLERR )
         END IF
C
         IF( ROLLERR .NE. 0 ) THEN
            CALL WRTMSG( 'CHKSOC', 'rollerr' )
            CALL ERRSET( ROLLERR )
         END IF
C
C        It is also impossible to do 2k FFT's when using the 16 track
C        barrel roll.  Try to block this case.  Only do test for
C        VLBA and MARKN formats.
C
         DO KS = 1, NSET
            IF( ( FORMAT(KS)(1:4) .EQ. 'VLBA' .OR.
     1              FORMAT(KS)(1:4) .EQ. 'MARK' ) .AND.
     2          ( CORCHAN .GE. 1000 .AND. 
     3          ( BARREL(KS) .EQ. 'roll_16' .OR. 
     4            ( BARREL(KS) .EQ. 'roll_auto' .AND.
     5              INT( NCHAN(KS) * FANOUT(KS) * BITS(1,KS) ) .GE. 16 )
     6           ) ) ) THEN
               CALL WLOG( 1, 'CHKSOC:   The requested 2K FFTs (1024 ' //
     1             'channel spectra) cannot be done on the Socorro' )
               CALL WLOG( 1, '          correlator when the 16 track '
     1             // 'barrel roll is in use.' )
               IF( BARREL(KS) .EQ. 'roll_auto' ) THEN
                  CALL WLOG( 1,'          For this setup, the automatic'
     1                // ' roll specification (default) will probably' )
                  CALL WLOG( 1, '          give a 16 track roll. ' )
               END IF
               CALL WLOG( 1, '          Please specify ' //
     1                '''BARREL=roll_8'' or ''BARREL=roll_off'' ' //
     2                'in your setup file.' )
               CALL ERRSET( KS )
            END IF
         END DO
C
C        It is also impossible to do 2k FFT's when using 16X 
C        oversampling and 1:1 fanout.
C
         DO KS = 1, NSET
            IF( ( FORMAT(KS)(1:4) .EQ. 'VLBA' .OR.
     1              FORMAT(KS)(1:4) .EQ. 'MARK' ) .AND.
     2          ( CORCHAN * 2.0 * SAMPRATE(KS) / 
     3             ( 2.0 * BBFILT(1,KS) * FANOUT(KS) ) 
     4          .GE. 20000. ) ) THEN
C
               CALL WLOG( 1, 'CHKSOC:   The requested 2K FFTs (1024 ' //
     1             'channel spectra) cannot be done on the Socorro' )
               CALL ERRSET( KS )
               CALL WLOG( 1, '          correlator when 16X ' //
     1           'oversampling is in use.' )
            END IF
         END DO
C
C        For full polarization processing, the number of spectral
C        channels must be 128 or fewer.
C        This is not really a setup file problem, but it is 
C        Socorro specific.
C
         IF( CORCHAN .GT. 128 .AND. CORPOL ) THEN
            CALL WLOG( 1, 'CHKSOC:  Full polarization processing in '//
     1          'Sororro is limited to 128 spectral channels' )
            CALL WLOG( 1, '         per baseband channel. ' )
            CALL ERRLOG( 'CHKSOC:  Adjust your correlator parameters '//
     1          'and try again.' )
         END IF
C
C        Don't let them request more than 2000 channels
C
         IF( CORCHAN .GT. 1024 ) THEN
            CALL WLOG( 1, 'CHKSOC:  Correlation  in '//
     1          'Sororro is limited to 1024 spectral channels' )
            CALL WLOG( 1, '         per baseband channel. ' )
            CALL ERRLOG( 'CHKSOC:  Adjust your correlator parameters '//
     1          'and try again.' )
         END IF
C
C        For 1:4 fan out, half the data are lost when using the ZEROPAD
C        window function.  Issue a warning.
C
         PADLOSS = .FALSE.
         DO KS = 1, NSET
            IF( FANOUT(KS) .EQ. 4.0 .AND. CORWTFN .EQ. 'ZEROPAD' ) THEN
               PADLOSS = .TRUE.
            ENDIF
         END DO
         IF( PADLOSS ) THEN
            CALL WRTMSG( 'CHKSOC', 'padloss' )
         END IF
C
C        Check some of the correlation FFT size and averaging requests.
C        These were moved from OMSCOR.  This partially duplicates some
C        of the tests above so this should all be rationalized some
C        day.
C
         IF( CORPOL ) THEN
            CPOL = 'YES'
            MINFFT = 256
         ELSE
            CPOL = 'NO '
            MINFFT = 512
         END IF
         IF( 2 * CORCHAN .GE. MINFFT ) THEN
            IFFT = 2 * CORCHAN
            IAVG = 1
         ELSE
            IFFT = MINFFT
            IAVG = MINFFT / ( 2 * CORCHAN )
         END IF
         IF( IFFT .NE. 64 .AND. IFFT .NE. 128 .AND. IFFT .NE. 256 .AND.
     1       IFFT .NE. 512 .AND. IFFT .NE. 1024 .AND. IFFT .NE. 1024
     2       .AND. IFFT. NE. 2048 ) THEN
            MSGTXT = ' ' 
            WRITE( MSGTXT, '( A, I5, A )' ) 'CHKSOC:  Implied FFT size',
     1         IFFT, ' not an allowed value.  Bad CORCHAN?'
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( A, A )' ) '         CORCHAN should be ',
     1         'a power of 2 between 4 and 1024'
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'Fix CORCHAN and try again.' )
         END IF
         IF( IAVG .NE. 1 .AND. IAVG .NE. 2 .AND. IAVG .NE. 4 .AND.
     1       IAVG .NE. 8 .AND. IAVG .NE. 16 .AND. IAVG .NE. 32 ) THEN
            WRITE( MSGTXT, '( A, I5, A )' ) 
     1         'CHKSOC:  Implied spectral averaging ',
     1         IAVG, ' not an allowed value.  Bad CORCHAN?'
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( A, A )' ) '         CORCHAN should be ',
     1         'a power of 2 between 4 and 1024'
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'Fix CORCHAN and try again.' )
         END IF
C
C        Check the fanout vs averaging time request.
C
         DO KS = 1, NSET
            IF( ( CORAVG .LT. 0.26 .AND. SPEEDUP(KS) .GE. 2.0 ) .OR.
     1          ( CORAVG .LT. 0.52 .AND. SPEEDUP(KS) .GE. 4.0 ) ) THEN
               CALL WLOG( 1, 'CHKSOC: Correlator average time ' //
     1            '(CORAVG) must be greater than ' )
               CALL WLOG( 1, '        the speedup factor times ' //
     1            '0.13 seconds.' )
               CALL ERRSET( KS )
            END IF
         END DO
C
      END IF
C
      RETURN
      END

