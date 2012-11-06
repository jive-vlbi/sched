      SUBROUTINE CHKVLA( KS, ERRS )
C
C     Subroutine for SCHED called by CHKSET that checks some
C     VLA specific setup file parameters.
C
C     If ERRS is returned as TRUE, CHKSET will give the setup
C     file name and then crash the program.
C
C     The VLA routines are getting major surgery for the switch from
C     the old VLA system to the JVLA.  The VLA now uses VEX for 
C     control and WIDAR for the DAR.  For now (Oct 2012), the VLA
C     is restricted to 4 channels.  Each must be from a separate
C     IF and they must be in polarization pairs.  The channels must
C     be net USB.
C
C     Conversion to new system started May 9, 2012 RCW.  
C     For the old code, look for versions of SCHED 10.1 or earlier.
C     More work in Oct. 2012.  RCW
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER     KS, ICH, JCH
      LOGICAL     ERRS, GOTPAIR
C  --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVLA: Starting.' )
C
C     It is not wise to make VLA schedules longer than 24 hours.
C     There is no day number on the scans so, if there is a restart,
C     they can end up on the wrong day.
C
C     Still current for JVLA?  Leave it just in case.
C
      IF( TEND - TFIRST .GT. 1.D0 ) THEN
         CALL WRTMSG( 0, 'CHKVLA', 'vlaover24' )
      END IF
C
C     Check VLARFANT.  Not sure if this will do anything for JVLA.
C     This parameter is not used for the new system, so ignore it.
C     Keep the code in case it comes back.
C
C      IF( VLARFANT .LT. 1 .OR. VLARFANT .GT. 28 ) THEN
C         SETMSG = ' '
C         WRITE( SETMSG, '( A, I7, A )' ) 'CHKVLA: VLARFANT ', 
C     1      VLARFANT, ' out of range 1 to 28.'
C         CALL WLOG( 1, SETMSG )
C         ERRS = .TRUE.
C      END IF
C
C     Check the maximum number of channels.
C
      IF( NCHAN(KS) .GT. 4 ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, I7, A )' ) 'CHKVLA: NCHAN ', 
     1      NCHAN(KS), ' more than the maximum of 4 for the VLA.'
         CALL WLOG( 1, SETMSG )
         ERRS = .TRUE.
      END IF
C
C     Check valid IFs.
C
      DO ICH = 1, NCHAN(KS)
         IF( IFCHAN(ICH,KS) .NE. 'A' .AND. IFCHAN(ICH,KS) .NE. 'B' .AND.
     1       IFCHAN(ICH,KS) .NE. 'C' .AND. IFCHAN(ICH,KS) .NE. 'D' ) 
     2          THEN
            SETMSG = ' '
            WRITE( SETMSG, '( A, I4, 3A )' ) 
     1         'CHKVLA: IFCHAN for channel ', 
     2         ICH, ' at the VLA is ', IFCHAN(ICH,KS), 
     3         ' VLA which is not A, B, C, or D. '
            CALL WLOG( 1, SETMSG )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Check for unique IFCHAN for each channel.
C     While at it, check for RCP in A or B and LCP in C or D
C
      IF( NCHAN(KS) .GT. 1 ) THEN
         DO ICH = 1, NCHAN(KS) - 1
            DO JCH = ICH + 1, NCHAN(KS)
               IF( IFCHAN(ICH,KS) .EQ. IFCHAN(JCH,KS) ) THEN
                  SETMSG = ' '
                  WRITE( SETMSG, '( 3A )' ) 'CHKVLA: For the ',
     1            'VLA, each baseband channel must be from a different',
     2            ' IF channel (IFCHAN).'
                  CALL WLOG( 1, SETMSG )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           Check the polarization vs IF.
C
            IF( .NOT. ( ( POL(ICH,KS) .EQ. 'RCP' .AND. 
     1        ( IFCHAN(ICH,KS) .EQ. 'A' .OR. IFCHAN(ICH,KS) .EQ. 'B' ) )
     2          .OR. ( POL(ICH,KS) .EQ. 'LCP' .AND. 
     3        ( IFCHAN(ICH,KS) .EQ. 'C' .OR. IFCHAN(ICH,KS) .EQ. 'D' ) )
     4          ) ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( 2A )' )
     1           'CHKVLA: For VLA, RCP must be IF A or B and LCP must ',
     2           'be IF C or D.'
               CALL WLOG( 1, SETMSG )
               ERRS = .TRUE.
            END IF
         END DO         
      END IF
C
C     Require dual polarization pairs.
C     Do this a bit crudely - go through all channels looking for the
C     partner.  This is twice as many checks as required, but it is
C     such a short loop it doesn't matter.
C     I am also not checking the frequency sets.  Maybe should.
C
      IF( NCHAN(KS) .GT. 1 ) THEN
         DO ICH = 1, NCHAN(KS)
            GOTPAIR = .FALSE.
            DO JCH = 1, NCHAN(KS)
               IF( ICH .NE. JCH .AND.
     1             FREQREF(ICH,KS) .EQ. FREQREF(JCH,KS) .AND.
     2             BBFILT(ICH,KS) .EQ. BBFILT(JCH,KS) .AND.
     3             POL(ICH,KS) .NE. POL(JCH,KS) .AND.
     4             NETSIDE(ICH,KS) .EQ. NETSIDE(JCH,KS) ) THEN
                  GOTPAIR = .TRUE.
               END IF
            END DO
            IF( .NOT. GOTPAIR ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( 2A, I4, A )' ) 
     1           'CHKVLA: At the VLA, baseband channels must be ', 
     2           'polarization pairs.  Channel ', ICH,
     3           ' does not have a partner.'
               CALL WLOG( 1, SETMSG )
               ERRS = .TRUE.
            END IF
         END DO      
      END IF
C
C     Require net upper sideband.
C
      DO ICH = 1, NCHAN(KS)
         IF( NETSIDE(ICH,KS) .NE. 'U' ) THEN
            SETMSG = ' '
            WRITE( SETMSG, '( 2A, I4, A )' ) 
     1        'CHKVLA: At the VLA, baseband channels must be ', 
     2        'net upper sideband.  Channel ', ICH,
     3        ' is not.'
            CALL WLOG( 1, SETMSG )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Consider checking the frequency ranges at some point, but it is not
C     critical for now because the VLA can hit any frequency that the VLBA
C     can.  The main problems will occur with the sideband restrictions 
C     which will not be compatible with WIDAR (USB) and the PFB on the VLBA
C     when the IF is upper sideband.  We will need the sideband inverting 
C     mode for that.
C
      RETURN
      END
