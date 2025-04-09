      SUBROUTINE VXWRDS
C 
C     Routine specific for the VEX2 extension of SCHED. 
C     Replaces a specific section of the VEX2 file 
C     Based on the VEX block
C     In this case the TR = $TRACKS section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
C     Adapted by Adriana Escobar
C
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink2.inc' 
C      
      INTEGER   ITR, KS, ICH, IBIT, IFAN, IP, HEDSTK, NS2USD
      INTEGER   LPOS, I, ISTA
      INTEGER   LEN1
      INTEGER   ISCAT
      CHARACTER LINE*132, THEBIT*4, TPSUBP, S2MDNM*7, IS2USD(16)*4
      CHARACTER THEDAR*5
      LOGICAL   S2OK, LOWBBC, HIBBC
      INTEGER   IDS
C
C ----------------------------------------------------------------------
C
      LINE = ' ' 
C
C     Nov 2011 - MarkIII is now ancient history so a abort if FORMAT=
C     MARKIII was no longer needed.
      WRITE( IVEX, '( A, A1 )' ) '$DATASTREAMS', SEP     
C
C     Deal with no requested datastream information - like pointing.
C
C      IF( NTRVEX .EQ. 0 .OR. OBSTYP .EQ. 'NONE' .OR. 
C     1      OBSTYP .EQ. 'PTVLBA' ) THEN
      IF( NTRVEX .EQ. 0 ) THEN
C
C        Try to support pointing observations (no recording) with 
C        dummy format specification.  RCW  Nov. 21, 2011.
C

      write(*,*) 'vxwrdr remove this bit if track sections below work'
         IF( OBSTYP .EQ. 'PTVLBA' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A )' )
     1          'VXWRDS: Setting format to NONE for ',
     2          'pointing observations.'
            CALL WLOG( 1, MSGTXT )
            WRITE( IVEX, '( A )' )
     1         'def DATASTREAMS.NONE;'
            WRITE( IVEX, '( A1, A, A )' ) COM,
     1          ' This is a fake format for ',
     2          'non-recording observations.'
            WRITE( IVEX, '( 5X, A, A1, A, I1.1, 1X, A1, 1X, A, A1)' ) 
     1          'datastream = ', LNK, 'DS', IDS, COL,  
     2          FORMAT(KS)(1:LEN1(FORMAT(KS))), SEP
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
         IDS = 1
         KS = TRISSET(ITR)
C
C        Find a station using this setup so you can check its medium later
C
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,KS) )
     4      ISCAT = ISTA
         END DO
C
C        Overwrite the name of the block if format
C
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        DSLINK(ITR)(1:LEN1(DSLINK(ITR))), SEP
         CALL VXSTLI2( ITR, NSTATR, ISTATR )
      
C
C        Write the DBE if one was specified and the FIRMFILE, if 
C        specified.
C
         IF( DBE(KS) .NE. ' ' )
     1       WRITE( IVEX, '( A1, A, A, A )' ) COM,
     2         '    firmware_type = ', DBE(KS)(1:LEN1(DBE(KS))), SEP
C         IF( FIRMFILE(KS) .NE. ' ' )
C     1       WRITE( IVEX, '( A1, A, A, A )' ) COM,
C     2         '    firmware_file = ', 
C     3         FIRMFILE(KS)(1:LEN1(FIRMFILE(KS))), SEP
C
C        If format is none, write a a generic datastream block
C
         IF( FORMAT(KS)(1:4) .EQ. 'NONE' ) THEN
            WRITE( IVEX, '( A1, A, A )' ) COM,
     1          ' This is a fake format for ',
     2          'non-recording observations.'
            WRITE( IVEX, '( 5X, A, A1, A, I1.1, 1X, A1, 1X, A, A1)' ) 
     1          'datastream = ', LNK, 'DS', IDS, COL,  
     2          FORMAT(KS)(1:LEN1(FORMAT(KS))), SEP
            DO ICH = 1, NCHAN(KS)
               WRITE( LINE, '( 5X, A, 1X, A1, A, I1.1, 1X, 
     1                A1, 1X, A1, A, I1.1, 1X, A1, 1X, I1.1,
     2                1X, A1, 1X, I1.1, 1X, A1, F6.1, 1X, A,  
     3                1X, A1, 1X, I1.1, 1X, A1, 1X, A, 1X, 
     4                A1, 1X, I4, A1 )' ) 
     5                'thread =', LNK, 'DS', IDS, COL, LNK,
     6                'thread', ICH-1, COL, ICH-1, COL, 1, COL,
     7                SAMPRATE(KS), 'Ms/sec', 
     8                COL, BITS(1,KS), COL, 'real', COL, 
     9                5000, SEP
               WRITE( IVEX, '( A, A1 )' ) LINE(1:LEN1(LINE))
               WRITE( LINE, '( 5X, A, 1X, A1, A, I1.1, 1X, 
     1                A1, 1X, A1, A, I1.1, 1X, A1, 1X,
     2                A1, A, I2.2, 1X, A1, 1X, I1.1,  
     3                A1 )' ) 
     4                'channel =', LNK, 'DS', IDS, COL, LNK,
     5                'thread', ICH-1, COL, LNK, 'CH', ICH, COL,
     6                0, SEP
               WRITE( IVEX, '( A, A1 )' ) LINE(1:LEN1(LINE))
            END DO
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
     1            'VXWRDS: unsupported recording mode/fan: ', 
     2            FORMAT(KS), FANOUT(KS)
              CALL ERRLOG( MSGTXT )
           END IF
           WRITE( IVEX, '( 5X, A, A1, A, I1.1, 1X, A1, 1X, A, A1)' ) 
     1          'datastream = ', LNK, 'DS', IDS, COL,  
     2          FORMAT(KS)(1:LEN1(FORMAT(KS))), SEP
C           WRITE( IVEX, '( A1, 4X, A, A, A, I1.1 )' ) COM,
C     1          'format = ', FORMAT(KS)(1:LEN1(FORMAT(KS))), 
C     2          ', and fan-out = ', NINT(FANOUT(KS))
C          
C          write a comment about the tpspeed and data rate
C                
           IF( FORMAT(KS)(1:2) .NE. 'S2' ) THEN
C                    IF( USETAPE(ISCAT) ) THEN
C                 WRITE( IVEX, '( A1, 4X, A, F6.2, A, I4, A, I4, A )' ) 
C     1               COM, 'mode requires ', SAMPRATE(KS)/FANOUT(KS), 
C     2               'Mb/s/tr; tape speed low dens:', NINT(SPEEDL(KS)),
C     3               'ips, high dens:', NINT(SPEEDH(KS)), 'ips'
C              ELSE IF( USEDISK(ISCAT) ) THEN
C                 WRITE( IVEX, '( A1, 4X, A, F6.2, A )' ) 
C     1               COM, 'mode requires ', SAMPRATE(KS)/FANOUT(KS), 
C     2               'Mb/s/tr; stations using disks'
C              END IF
C          
C             Now the format and modulation
C
C             NOTE: This class was adapted from the TRACKS section
C              the below commented fields were left in case there is 
C              modulation to be implement in the future for the
C              datastream block.
C                
              IF( FORMAT(KS)(1:7) .EQ. 'MARKIII' ) THEN
C                       WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'track_frame_format = ','Mark3A', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'data_modulation = ','off', SEP
              ELSE IF( FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'track_frame_format = ','VLBA', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1                  'data_modulation = ','on', SEP
              ELSE IF( FORMAT(KS)(1:4) .EQ. 'MKIV' ) THEN
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'track_frame_format = ','Mark4', SEP
C          
C                and awaiting a MkIV firmware upgrade it is off
C                
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1                     'data_modulation = ','off', SEP
              ELSE IF( FORMAT(KS)(1:6) .EQ. 'MARK5B' ) THEN
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'track_frame_format = ','MARK5B', SEP
              ELSE IF( FORMAT(KS)(1:4) .EQ. 'VDIF' ) THEN
C
C                VDIF5032 is appropriate for RDBE/DDC and WIDAR,
C                but might need something else for other 
C                systems.
C
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'track_frame_format = ','VDIF5032', SEP
C           
             ELSE IF( FORMAT(KS)(1:3) .EQ. 'LBA' ) THEN
C                       WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1              'S2_recording_mode = ', 'LBA', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'S2_data_source = ','VLBA', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'S2_recording_mode = ','none', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1              'track_frame_format = ', 'LBA', SEP
C                 WRITE( IVEX, '( 5X, A, A, A1 )' )
C     1               'data_modulation = ','off', SEP
              ELSE
                 WRITE( MSGTXT, '( A, A )' )
     1               'VXWRDS: unsupported recording mode: ', FORMAT(KS)
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
C              WRITE( IVEX, '( A1, 4X, A, F5.2, A )' ) 
C     1                  COM, 'mode requires ', SAMPRATE(KS), 
C     2            'Mb/s/IF;'
C          
C             Then find S2 mode
C                
              CALL VXS2MD( KS, S2MDNM, S2OK, NS2USD, IS2USD, .TRUE. )
                    IF( .NOT. S2OK ) 
     1            CALL ERRLOG('VXWRDS: inconsistent or impossible'//
     2            ' S2 mode ')
C              WRITE( LINE, '( 5X, A, A, A1 )' )
C     1            'S2_recording_mode = ', S2MDNM(1:LEN1(S2MDNM)), SEP
              LPOS = LEN1(LINE) + 1
C              WRITE( LINE(LPOS:), '( 1X, A1, 1X, A )' ) COM, 'streams'
C          
C             write up to 8 input channels in comment
C                
              IF( NS2USD .GT. 8 ) THEN
C                
C                can only be 16, write on two lines
C                
                 DO I = 1, 8
                          LPOS = LEN1(LINE) + 1
C                    IF( IS2USD(I) .NE. ' ' ) THEN
C                       WRITE( LINE(LPOS:), '( A1, A2, I2.2)' ) 
C     1                     COL, 'IN', I-1
C                    END IF
                 END DO
C                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
C          
C                Now other line:
C                
                 LINE = ' '
C                       WRITE( LINE, '( A1, 37X )' ) COM
                 DO I = 9, 16
                    LPOS = LEN1(LINE) + 1
C                    IF( IS2USD(I) .NE. ' ' ) THEN
C                       WRITE( LINE(LPOS:), '( A1, A2, I2.2 )' ) 
C     1                     COL, 'IN', I-1
C                    END IF
                 END DO
C                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
              ELSE
C          
C                there are less than 9 inputs active, out of 16
C                
                 DO I = 1, 16
                          LPOS = LEN1(LINE) + 1
C                    IF( IS2USD(I) .NE. ' ' ) THEN
C                       WRITE( LINE(LPOS:), '( A1, A2, I2.2 )' ) 
C     1                     COL, 'IN', I-1
C                    END IF
                 END DO
C                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
              END IF
C          
C             write the S2_data_source
C                
C              WRITE( LINE, '( 5X, A )' )
C     1                  'S2_data_source = '
              LPOS = LEN1(LINE) + 1
              IF( THEDAR .EQ. 'MKIV' ) THEN
C          
C                Any BBC can be send to output; Sched supports 1+2 
C                      channel BBC 1+2 or 1+3 or 2+4
C                pending further development
C          
C                 WRITE( LINE(LPOS:), '( 1X, A, 1X, A1, 1X, A1, A, I2.2,
C     1                     1X, A1, 1X, A1, A, I2.2, A1 )') 
C     1               'Mark4_formatter', COL, LNK, 'BBC', BBC(1,KS),
C     2               COL, LNK, 'BBC', BBC(2,KS), SEP
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
                       IF( HIBBC ) CALL ERRLOG('VXWRDS: Inconsistent '//
     1                     'DAR connection')
                       LOWBBC = .TRUE.
                    ELSE IF( BBC(ICH,KS) .LT. 9 
     1                     .AND. BBC(ICH,KS) .GE. 5 ) THEN
                       IF( LOWBBC ) CALL ERRLOG('VXWRDS: Inconsistent '
     1                     //'DAR connection')
                       HIBBC = .TRUE.
                    ELSE 
                       CALL ERRLOG(
     1                      'VXWRDS: Inconsistent DAR connection')
                    END IF
                 END DO
C                 IF( HIBBC ) THEN
C                    WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
C     1                  'VLBA_BBC_5-8', SEP
C                 ELSE
C                    WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
C     1                  'VLBA_BBC_1-4', SEP
C                 ENDIF
              ELSE
                 CALL WLOG( 1,'VXWRDS: WARNING, un-documented '//
     1               'S2_data_source, may not be recognized by PCFS ')
C          
C                A secial case of undocummented is NONE => none
C                
C                 IF( THEDAR .EQ. 'NONE') THEN
C                          WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
C    1                  'none', SEP
C                 ELSE
C                    WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
C     1                  THEDAR, SEP
C                 END IF
              ENDIF
              WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
C          
C             write the obsolete S2_record_source in comment
C                
C              WRITE( LINE, '( A1, 4X, A )' )
C     1                  COM, 'S2_record_source =obsolete, but'
              LPOS = LEN1(LINE) + 1
              IF( NS2USD .GT. 8 ) THEN
C          
C                can only be 16, write on two lines
C                
                 DO I = 1, 8
                          LPOS = LEN1(LINE) + 1
C                    IF( IS2USD(I) .NE. ' ' ) THEN
C                       WRITE( LINE(LPOS:), '( A1, A4 )' ) 
C     1                     COL, IS2USD(I)
C                    END IF
                 END DO
C                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
C          
C                and the 2nd line:
C                
                 LINE = ' '
C                       WRITE( LINE, '( A1, 36X )' ) COM
                 DO I = 9, 16
                    LPOS = LEN1(LINE) + 1
C                    IF( IS2USD(I) .NE. ' ' ) THEN
C                       WRITE( LINE(LPOS:), '( A1, A4 )' ) 
C     1                     COL, IS2USD(I)
C                    END IF
                 END DO
              ELSE
C          
C                there are less than 9 inputs active, out of 16
C                
                 DO I = 1, 16
                          LPOS = LEN1(LINE) + 1
C                    IF( IS2USD(I) .NE. ' ' ) THEN
C                       WRITE( LINE(LPOS:), '( A1, A4 )' ) 
C     1                     COL, IS2USD(I)
C                    END IF
                 END DO
C                 WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
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
                    IF ( DBE(KS) .EQ. 'VNDA' ) THEN
                      WRITE( LINE, '( 5X, A, 1X, A1, A, I1.1, 1X, 
     1                      A1, 1X, A1, A, I1.1, 1X, A1, 1X, I1.1,
     2                      1X, A1, 1X, I1.1, 1X, A1, F6.1, 1X, A,  
     3                      1X, A1, 1X, I1.1, 1X, A1, 1X, A, 1X, 
     4                      A1, 1X, I4, A1 )' ) 
     5                     'thread =', LNK, 'DS', IDS, COL, LNK,
     6                     'thread', ICH-1, COL, ICH-1, COL, 1, COL,
     7                     SAMPRATE(KS)/FANOUT(KS), 'Ms/sec', 
     8                     COL, BITS(1,KS), COL, 'complex', COL, 
     9                     8000, SEP
                       WRITE( IVEX, '( A, A1 )' ) LINE(1:LEN1(LINE))
                       WRITE( LINE, '( 5X, A, 1X, A1, A, I1.1, 1X, 
     1                      A1, 1X, A1, A, I1.1, 1X, A1, 1X,
     2                      A1, A, I2.2, 1X, A1, 1X, I1.1,  
     3                      A1 )' ) 
     4                     'channel =', LNK, 'DS', IDS, COL, LNK,
     5                     'thread', ICH-1, COL, LNK, 'CH', ICH, COL,
     6                     0, SEP
                       WRITE( IVEX, '( A, A1 )' ) LINE(1:LEN1(LINE))
                    ELSE
                        WRITE( LINE, '( 5X, A, 1X, A1, A, I1.1, 1X, 
     1                      A1, 1X, A1, A, I1.1, 1X, A1, 1X, I1.1,
     2                      1X, A1, 1X, I1.1, 1X, A1, F6.1, 1X, A,  
     3                      1X, A1, 1X, I1.1, 1X, A1, 1X, A, 1X, 
     4                      A1, 1X, I4, A1 )' ) 
     5                     'thread =', LNK, 'DS', IDS, COL, LNK,
     6                     'thread', ICH-1, COL, ICH-1, COL, 1, COL,
     7                     SAMPRATE(KS)/FANOUT(KS), 'Ms/sec', 
     8                     COL, BITS(1,KS), COL, 'real', COL, 
     9                     5000, SEP
                       WRITE( IVEX, '( A, A1 )' ) LINE(1:LEN1(LINE))
                       WRITE( LINE, '( 5X, A, 1X, A1, A, I1.1, 1X, 
     1                      A1, 1X, A1, A, I1.1, 1X, A1, 1X,
     2                      A1, A, I2.2, 1X, A1, 1X, I1.1,  
     3                      A1 )' ) 
     4                     'channel =', LNK, 'DS', IDS, COL, LNK,
     5                     'thread', ICH-1, COL, LNK, 'CH', ICH, COL,
     6                     0, SEP
                       WRITE( IVEX, '( A, A1 )' ) LINE(1:LEN1(LINE))
                    END IF
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
