      SUBROUTINE VXWRTR
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the TR = $TRACKS section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C      
      INTEGER   ITR, KS, ICH, IBIT, IFAN, IP, HEDSTK, NS2USD
      INTEGER   LPOS, I, ISTA, TRACK1, ITRK
      INTEGER   LEN1
      INTEGER   ISCAT
      CHARACTER LINE*132, THEBIT*4, TPSUBP, S2MDNM*7, IS2USD(16)*4
      CHARACTER THEDAR*5
      LOGICAL   S2OK, LOWBBC, HIBBC
C
C ----------------------------------------------------------------------
C
      LINE = ' ' 
C
C     Nov 2011 - MarkIII is now ancient history so a abort if FORMAT=
C     MARKIII was no longer needed.
      WRITE( IVEX, '( A, A1 )' ) '$TRACKS', SEP     
C
C     Deal with no requested track information - like pointing.
C
C      IF( NTRVEX .EQ. 0 .OR. OBSTYP .EQ. 'NONE' .OR. 
C     1      OBSTYP .EQ. 'PTVLBA' ) THEN
      IF( NTRVEX .EQ. 0 ) THEN
C
C        Try to support pointing observations (no recording) with 
C        dummy format specification.  RCW  Nov. 21, 2011.
C

      write(*,*) 'vxwrtr remove this bit if track sections below work'
         IF( OBSTYP .EQ. 'PTVLBA' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A )' )
     1          'VXWRTR: Setting format to NONE for ',
     2          'pointing observations.'
            CALL WLOG( 1, MSGTXT )
            WRITE( IVEX, '( A )' )
     1         'def TRACKS.NONE;'
            WRITE( IVEX, '( A1, A, A )' ) COM,
     1          ' This is a fake format for ',
     2          'non-recording observations.'
            WRITE( IVEX, '( 5X, A, A, A1 )' )
     1          'track_frame_format = ','NONE', SEP
C
C           Wrap up.
C
            WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
            WRITE( IVEX, '( A )' ) COMLIN
         ELSE
            CALL ERRLOG( 'VSWRTR:  No VEX TRACK blocks requested. '//
     1       'Something wrong.' )
         END IF
C
C        Jump to end of routine.  This should be an IF/THEN/ELSE,
C        but I did not want to indent the whole rest of the routine
C        again.  RCW, Nov 2011
C
         GO TO 999
      END IF
C
C     start with the right business
C
      DO ITR = 1, NTRVEX
         KS = TRISSET(ITR)
C
C        Find a station using this setup so you can check its medium later
C
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,KS) )
     4      ISCAT = ISTA
         END DO
C
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        TRLINK(ITR)(1:LEN1(TRLINK(ITR))), SEP
         CALL VXSTLI( ITR, NSTATR, ISTATR )
C
C        Write the DBE if one was specified and the FIRMFILE, if 
C        specified.
C
         IF( DBE(KS) .NE. ' ' )
     1       WRITE( IVEX, '( A1, A, A, A )' ) COM,
     2         '    firmware_type = ', DBE(KS)(1:LEN1(DBE(KS))), SEP
         IF( FIRMFILE(KS) .NE. ' ' )
     1       WRITE( IVEX, '( A1, A, A, A )' ) COM,
     2         '    firmware_file = ', 
     3         FIRMFILE(KS)(1:LEN1(FIRMFILE(KS))), SEP
C
C        If format is none, write a minimal track def.
C
         IF( FORMAT(KS)(1:4) .EQ. 'NONE' ) THEN
            WRITE( IVEX, '( A1, A, A )' ) COM,
     1          ' This is a fake format for ',
     2          'non-recording observations.'
            WRITE( IVEX, '( 5X, A, A, A1 )' )
     1          'track_frame_format = ','NONE', SEP
            WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
         ELSE
C
C          ignore fan in's for now, do fan-outs, figure out what it is
C                
           IF( .NOT. (FORMAT(KS)(1:6) .EQ. 'VLBA1:' .OR. 
     1                FORMAT(KS)(1:7) .EQ. 'MARKIII' .OR.
     2                FORMAT(KS)(1:4) .EQ. 'VDIF' .OR.
     3          FORMAT(KS)(1:2) .EQ. 'S2' .OR.
     4          FORMAT(KS)(1:3) .EQ. 'LBA' .OR.
     5          FORMAT(KS)(1:6) .EQ. 'MKIV1:' .OR.
     6          FORMAT(KS)(1:6) .EQ. 'MARK5B' ) .OR. 
     7         ( FANOUT(KS) .LT. 0.9 .AND.
     8         FORMAT(KS)(1:2) .NE. 'S2' )) THEN
              WRITE( MSGTXT, '( A, A, F6.2 )' )
     1            'VXWRTR: unsupported recording mode/fan: ', 
     2            FORMAT(KS), FANOUT(KS)
              CALL ERRLOG( MSGTXT )
           END IF
           WRITE( IVEX, '( A1, 4X, A, A, A, I1.1 )' ) COM,
     1          'format = ', FORMAT(KS)(1:LEN1(FORMAT(KS))), 
     2          ', and fan-out = ', NINT(FANOUT(KS))
C          
C          write a comment about the tpspeed and data rate
C                
           IF( FORMAT(KS)(1:2) .NE. 'S2' ) THEN
                    IF( USETAPE(ISCAT) ) THEN
                 WRITE( IVEX, '( A1, 4X, A, F6.2, A, I4, A, I4, A )' ) 
     1               COM, 'mode requires ', SAMPRATE(KS)/FANOUT(KS), 
     2               'Mb/s/tr; tape speed low dens:', NINT(SPEEDL(KS)),
     3               'ips, high dens:', NINT(SPEEDH(KS)), 'ips'
              ELSE IF( USEDISK(ISCAT) ) THEN
                 WRITE( IVEX, '( A1, 4X, A, F6.2, A )' ) 
     1               COM, 'mode requires ', SAMPRATE(KS)/FANOUT(KS), 
     2               'Mb/s/tr; stations using disks'
              END IF
C          
C             Now the format and modulation
C                
              IF( FORMAT(KS)(1:7) .EQ. 'MARKIII' ) THEN
                       WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'track_frame_format = ','Mark3A', SEP
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'data_modulation = ','off', SEP
              ELSE IF( FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'track_frame_format = ','VLBA', SEP
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1                  'data_modulation = ','on', SEP
              ELSE IF( FORMAT(KS)(1:4) .EQ. 'MKIV' ) THEN
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'track_frame_format = ','Mark4', SEP
C          
C                and awaiting a MkIV firmware upgrade it is off
C                
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1                     'data_modulation = ','off', SEP
              ELSE IF( FORMAT(KS)(1:6) .EQ. 'MARK5B' ) THEN
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'track_frame_format = ','MARK5B', SEP
              ELSE IF( FORMAT(KS)(1:4) .EQ. 'VDIF' ) THEN
C
C                VDIF5032 is appropriate for RDBE/DDC and WIDAR,
C                but might need something else for other 
C                systems.
C
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'track_frame_format = ','VDIF5032', SEP
           
              ELSE IF( FORMAT(KS)(1:3) .EQ. 'LBA' ) THEN
C                       WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1              'S2_recording_mode = ', 'LBA', SEP
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'S2_data_source = ','VLBA', SEP
                 WRITE( IVEX, '( 5X, A, A, A1 )' )
     1               'S2_recording_mode = ','none', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1              'track_frame_format = ', 'LBA', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'data_modulation = ','off', SEP
              ELSE
                 WRITE( MSGTXT, '( A, A )' )
     1               'VXWRTR: unsupported recording mode: ', FORMAT(KS)
                 CALL ERRLOG( MSGTXT )
              END IF
           ELSE
C          
C             write S2 data soure command, need to know DAR:
C                
              DO ISTA = 1, NSTA
                       IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,KS) )
     1               THEDAR = DAR(STANUM(ISTA))
              END DO
C          
              WRITE( IVEX, '( A1, 4X, A, F5.2, A )' ) 
     1                  COM, 'mode requires ', SAMPRATE(KS), 
     2            'Mb/s/IF;'
C          
C             Then find S2 mode
C                
              CALL VXS2MD( KS, S2MDNM, S2OK, NS2USD, IS2USD, .TRUE. )
                    IF( .NOT. S2OK ) 
     1            CALL ERRLOG('VXWRTR: inconsistent or impossible'//
     2            ' S2 mode ')
              WRITE( LINE, '( 5X, A, A, A1 )' )
     1            'S2_recording_mode = ', S2MDNM(1:LEN1(S2MDNM)), SEP
              LPOS = LEN1(LINE) + 1
              WRITE( LINE(LPOS:), '( 1X, A1, 1X, A )' ) COM, 'streams'
C          
C             write up to 8 input channels in comment
C                
              IF( NS2USD .GT. 8 ) THEN
C                
C                can only be 16, write on two lines
C                
                 DO I = 1, 8
                          LPOS = LEN1(LINE) + 1
                    IF( IS2USD(I) .NE. ' ' ) THEN
                       WRITE( LINE(LPOS:), '( A1, A2, I2.2)' ) 
     1                     COL, 'IN', I-1
                    END IF
                 END DO
                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
C          
C                Now other line:
C                
                 LINE = ' '
                       WRITE( LINE, '( A1, 37X )' ) COM
                 DO I = 9, 16
                    LPOS = LEN1(LINE) + 1
                    IF( IS2USD(I) .NE. ' ' ) THEN
                       WRITE( LINE(LPOS:), '( A1, A2, I2.2 )' ) 
     1                     COL, 'IN', I-1
                    END IF
                 END DO
                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
              ELSE
C          
C                there are less than 9 inputs active, out of 16
C                
                 DO I = 1, 16
                          LPOS = LEN1(LINE) + 1
                    IF( IS2USD(I) .NE. ' ' ) THEN
                       WRITE( LINE(LPOS:), '( A1, A2, I2.2 )' ) 
     1                     COL, 'IN', I-1
                    END IF
                 END DO
                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
              END IF
C          
C             write the S2_data_source
C                
              WRITE( LINE, '( 5X, A )' )
     1                  'S2_data_source = '
              LPOS = LEN1(LINE) + 1
              IF( THEDAR .EQ. 'MKIV' ) THEN
C          
C                Any BBC can be send to output; Sched supports 1+2 
C                      channel BBC 1+2 or 1+3 or 2+4
C                pending further development
C          
                 WRITE( LINE(LPOS:), '( 1X, A, 1X, A1, 1X, A1, A, I2.2,
     1                     1X, A1, 1X, A1, A, I2.2, A1 )') 
     1               'Mark4_formatter', COL, LNK, 'BBC', BBC(1,KS),
     2               COL, LNK, 'BBC', BBC(2,KS), SEP
              ELSE IF( THEDAR .EQ. 'VLBA' .OR. 
     1               THEDAR .EQ. 'VLBAG' ) THEN
C          
C                could be BBC 1-4 or 5-8
C                
                 LOWBBC = .FALSE.
                       HIBBC = .FALSE.
                 DO ICH = 1, NCHAN(KS)
                    IF( BBC(ICH,KS) .LT. 5 .AND. BBC(ICH,KS) .GE. 1 ) 
     1                  THEN
                       IF( HIBBC ) CALL ERRLOG('VXWRTR: Inconsistent '//
     1                     'DAR connection')
                       LOWBBC = .TRUE.
                    ELSE IF( BBC(ICH,KS) .LT. 9 
     1                     .AND. BBC(ICH,KS) .GE. 5 ) THEN
                       IF( LOWBBC ) CALL ERRLOG('VXWRTR: Inconsistent '
     1                     //'DAR connection')
                       HIBBC = .TRUE.
                    ELSE 
                       CALL ERRLOG(
     1                      'VXWRTR: Inconsistent DAR connection')
                    END IF
                 END DO
                 IF( HIBBC ) THEN
                    WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
     1                  'VLBA_BBC_5-8', SEP
                 ELSE
                    WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
     1                  'VLBA_BBC_1-4', SEP
                 ENDIF
              ELSE
                 CALL WLOG( 1,'VXWRTR: WARNING, un-documented '//
     1               'S2_data_source, may not be recognized by PCFS ')
C          
C                A secial case of undocummented is NONE => none
C                
                 IF( THEDAR .EQ. 'NONE') THEN
                          WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
     1                  'none', SEP
                 ELSE
                    WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
     1                  THEDAR, SEP
                 END IF
              ENDIF
              WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
C          
C             write the obsolete S2_record_source in comment
C                
              WRITE( LINE, '( A1, 4X, A )' )
     1                  COM, 'S2_record_source =obsolete, but'
              LPOS = LEN1(LINE) + 1
              IF( NS2USD .GT. 8 ) THEN
C          
C                can only be 16, write on two lines
C                
                 DO I = 1, 8
                          LPOS = LEN1(LINE) + 1
                    IF( IS2USD(I) .NE. ' ' ) THEN
                       WRITE( LINE(LPOS:), '( A1, A4 )' ) 
     1                     COL, IS2USD(I)
                    END IF
                 END DO
                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
C          
C                and the 2nd line:
C                
                 LINE = ' '
                       WRITE( LINE, '( A1, 36X )' ) COM
                 DO I = 9, 16
                    LPOS = LEN1(LINE) + 1
                    IF( IS2USD(I) .NE. ' ' ) THEN
                       WRITE( LINE(LPOS:), '( A1, A4 )' ) 
     1                     COL, IS2USD(I)
                    END IF
                 END DO
              ELSE
C          
C                there are less than 9 inputs active, out of 16
C                
                 DO I = 1, 16
                          LPOS = LEN1(LINE) + 1
                    IF( IS2USD(I) .NE. ' ' ) THEN
                       WRITE( LINE(LPOS:), '( A1, A4 )' ) 
     1                     COL, IS2USD(I)
                    END IF
                 END DO
                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
              END IF
C          
C          done with S2
C                
           END IF
C                
C          This version should allow a second head set
C                
           IF( FORMAT(KS)(1:2) .NE. 'S2' ) THEN
                    LINE = ' '
C          
C             only tapemode 1,2 4, 8 supported in SCHED, PCFS does
C                   4 max, see vxwrpo
C          
              DO IP = 1, TAPEMODE(KS)
                       TPSUBP = 'X'
C          
C
                 IF( USETAPE(ISCAT) ) THEN
                    IF( IP .EQ. 1) TPSUBP = 'A'
                    IF( IP .EQ. 2) TPSUBP = 'B'
                    IF( IP .EQ. 3) TPSUBP = 'C'
                    IF( IP .EQ. 4) TPSUBP = 'D'
                    IF( IP .EQ. 5) TPSUBP = 'E'
                    IF( IP .EQ. 6) TPSUBP = 'F'
                    IF( IP .EQ. 7) TPSUBP = 'G'
                    IF( IP .EQ. 8) TPSUBP = 'H'
                 ELSE IF( USEDISK(ISCAT) ) THEN
                    TPSUBP = ' '
                 END IF
C          
                 DO ICH = 1, NCHAN(KS)
                    DO IBIT = 1, BITS(1,KS) 
C          
C                      different bits per channel not supported (ICH,KS)
C                
                       LINE = ' '
                             THEBIT = 'huhh'
                       IF( IBIT .EQ. 1) THEBIT='sign'
                       IF( IBIT .EQ. 2) THEBIT=' mag'
                       IF( TWOHEAD .AND.
     1                     TRACK(ICH,IP,KS) .GT. 33 ) THEN
                          HEDSTK = 2
                          TRACK1 = TRACK(ICH,IP,KS) - 64
                       ELSE
                          HEDSTK = 1
                          TRACK1 = TRACK(ICH,IP,KS)
                       ENDIF
                       WRITE( LINE(1:39), '( 5X, A, 1X, A1, 1X, A1, 
     1                     1X, A1, A, I2.2, 1X, A1, 1X, A, 1X, A1, 
     2                     1X, I1.1, 1X )' ) 
     3                     'fanout_def =', TPSUBP, COL, LNK, 'CH', ICH, 
     4                     COL, THEBIT, COL, HEDSTK
                       IF( FORMAT(KS)(1:3) .NE. 'LBA' .AND. 
     1                     FORMAT(KS)(1:6) .NE. 'MARK5B' .AND.
     2                     FORMAT(KS)(1:4) .NE. 'VDIF' ) THEN
                          DO IFAN = 1, NINT(FANOUT(KS))
C          
C                         here are some implicit assumptions made, 
C                               about VLBA (and MkIV!) standard modes, 
C                               see MkIV Memo 230
C          
                             LPOS = LEN1(LINE)+1
                                   WRITE( LINE(LPOS:LPOS+4), '(
     1                           A1, 1X, I2, 1X )' ) COL, 
     2                           TRACK1+(IFAN-1)*2+
     3                           (IBIT-1)*2*NINT(FANOUT(KS))
                          END DO
                       ELSE IF( FORMAT(KS)(1:4) .EQ. 'VDIF' ) THEN
C
C                         For VDIF format, just set the track number
C                         to the channel number.   That was set in settrk.f.
C                         (W. Brisken email May 14, 2012.  Change by RCW)
C
                          LPOS = LEN1(LINE)+1
                          WRITE( LINE(LPOS:LPOS+4), '(A1, 1X, I2)') 
     1                       COL, TRACK1
                       ELSE 
C                         For LBA and Mark5B, the layout is simpler - 
C                         There is no fanout and simple mapping of 
C                         track to channel that we set in settrk.f.
C          
                          LPOS = LEN1(LINE)+1
                          ITRK = TRACK1 + IBIT - 1
C                          IF( FORMAT(KS)(1:6) .EQ. 'MARK5B' )
C     1                       ITRK = ITRK + 2
                          WRITE( LINE(LPOS:LPOS+4), '(A1, 1X, I2)') 
     1                       COL, ITRK
                       END IF
                       WRITE( IVEX, '( A, A1 )' ) LINE(1:LEN1(LINE)), 
     1                      SEP
                    END DO
                 END DO
              END DO 
           END IF
           WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
C           
        END IF   
      END  DO
      WRITE( IVEX, '( A )' ) COMLIN
C
C     Jump to here if NTRVEX was zero.
C
  999 CONTINUE
      RETURN
      END
