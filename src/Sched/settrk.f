      SUBROUTINE SETTRK( NCHAN, TAPEMODE, FORMAT, BITS, TRACK, MCHAN,
     1           KBBC, SIDEBAND, DBE, DAR, KS, TWOHEAD, DEBUG, ILOG )
Cf2py intent(out) TRACK
C
C     Routine for SCHED, called by SETDEFS, that sets the track 
C     assignments, if needed.  It will only be called if
C     TRACK(1,1,KS) is 0 which is the trigger to use this routine.
C
C     Note that SETDEF calls this routine with the element of the 
C     arrays in the call argument for a particular setup.  That means
C     that the number of indices will be one less than in the 
C     include files.  I don't think I'd do that if writing it again.
C
C
      INTEGER    MCHAN, KS, ILOG
      INTEGER    NCHAN, TAPEMODE, BITS, TRACK(MCHAN,8), KBBC(MCHAN)
      CHARACTER  FORMAT*8, SIDEBAND(MCHAN)*1, MSG*80
      CHARACTER  DAR*(*), DBE*(*)
      LOGICAL    TWOHEAD, DEBUG
C
      INTEGER    ICH, ICH1, IP, MAXCHN, CHPASS
      CHARACTER  TEXT*80
C
C     For Mark5B
C     Cannot use a call argument as a dimension in f77 on Sun (g77 
C     and gfortran didn't complain).  Set the dimension higher than
C     any likely channel setting, and check it later.
C
      INTEGER    MMCH
      PARAMETER  (MMCH=32)
      INTEGER    MK5BCH(MMCH), M5BBBC, IM5, ICHMK5B(MMCH)
C 
C     For LBA
      INTEGER    LBACH(MMCH), LBABBC, ILBA, ICHLBA(MMCH)
C
C     M5BCH is the sequence number of this SCHED channel in the
C     Mark5B channels, which go by BBC number with upper sidebands
C     first, then lower sidebands.
C
C     Specify the track patterns in DATA statements in order to
C     make it totally clear what is going on.  They could be
C     derived in algorithms, but they would then be harder to 
C     understand.  The DATA statements give the patterns for
C     the 32 track or 16 channel modes (the limits).  The 
C     multiple pass (TAPEMODE passes) modes are subsets and are 
C     treated as such.  For the MarkIII modes, extra channels
C     can be added and recorded in on the tracks that true
C     Mark III doesn't use.
C
C     The two tape modes on the VLBA specify the tracks of the
C     second drive by adding 64 to the track number that would
C     be used on the first drive.
C
C     The track number specified is the first of the group used
C     for that channel.  The channel uses contiguous even or 
C     odd channels (evens and odds are never mixed).
C
      INTEGER    A8(8), A4(16), A2(32), A1(32), AN(32)
      INTEGER    M3A(32), M3B1(16), M3B2(16)
      INTEGER    M3E1(8), M3E2(8), M3E3(8), M3E4(8)
C
      SAVE       A8, A4, A2, A1, M3A, M3B1, M3B2
      SAVE       M3E1, M3E2, M3E3, M3E4
C
C     8 track per channel:  (VLBA1:4, 2 bit)
C
      DATA A8 / 2, 18, 3, 19,  66, 82, 67, 83 /
C
C     4 track per channel:  (VLBA1:4, 1 bit), (VLBA1:2, 2 bit)
C
      DATA A4 /  2, 10, 18, 26,  3, 11, 19, 27,
     1          66, 74, 82, 90, 67, 75, 83, 91  /
C
C     2 track per channel: (VLBA1:2, 1 bit), (VLBA1:1, 2 bit)
C
      DATA A2 / 2, 6,10,14,18,22,26,30, 3, 7,11,15,19,23,27,31, 
     1         66,70,74,78,82,86,90,94,67,71,75,79,83,87,91,95 /
C                    
C     One track per channel: (VLBA1:1, 1 bit)
C     
      DATA A1 /2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,
     1          3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33/
C
C     Fan in - May never happen.
C
C
C
C     Mark III mode A: This is included for the VEX stations
C     that can actually do it.  As for all, the VLBA 36 track
C     numbering system is used.  With this track scheme, the
C     first 14 channels are the Mark III B equivalents (odd
C     video converters).  The next 14 are the extras, normally
C     4 VCs lower than the M3B frequecies and 3 VC's higher.
C     The final 4 heads are extras for a 32 track system.
C
      DATA M3A / 18, 4,20, 6,22, 8,24,10,26,12,28,14,30,16,
     1           19, 5,21, 7,23, 9,25,11,27,13,29,15,31,17,
     2           32,2,33,3 /
C
C     Mark III mode B: Use a pattern of net lower followed by 
C     net upper sideband.  IE, channels in ascending frequency
C     order.  Test by checking that the sidebands are 
C     alternating.  Mode C uses the same pattern, but with 
C     RCP/LCP pairs instead of LSB/USB pairs.
C
      DATA M3B1 /18, 4,20, 6,22, 8,24,10,26,12,28,14,30,16,32,2/
      DATA M3B2 /19, 5,21, 7,23, 9,25,11,27,13,29,15,31,17,33,3/
C
C     Mark III mode E:
C
      DATA M3E1 /  4,  6,  8, 10, 12, 14, 16,  2 /
      DATA M3E2 / 18, 20, 22, 24, 26, 28, 30, 32 /
      DATA M3E3 /  5,  7,  9, 11, 13, 15, 17,  3 /
      DATA M3E4 / 19, 21, 23, 25, 27, 29, 31, 33 /
C
C----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0,'SETTRK: Starting.  Format: ' // FORMAT )
C
C     Check for inappropriate MMCH parameter and for too many channels
C     in the requested data.
C
      IF( MCHAN .GT. MMCH ) THEN
         WRITE( TEXT, '( A, I4, A, I4 )' )
     1          'SETTRK: Programming problem: local PARAMETER MMCH=',
     2          MMCH, ' too small for MCHAN = ', MCHAN
         CALL WLOG( 1, TEXT )
         CALL ERRSET( KS )
      END IF
C  
      IF( NCHAN .GT. MCHAN ) THEN
         WRITE( TEXT, '( A, I4, A, I4 )' )
     1          'SETTRK: Too many channels:', NCHAN, '  Max:', MCHAN
         CALL WLOG( 1, TEXT )
         CALL ERRSET( KS )
      END IF
C
C     Step through the possible VLBA modes.
C
      IF( FORMAT(1:4) .EQ. 'VLBA' ) THEN
         IF( FORMAT .EQ. 'VLBA1:4' .AND. BITS .EQ. 2 ) THEN
            MAXCHN = 4
            IF( TWOHEAD ) MAXCHN = 8
            DO ICH = 1, MAXCHN
               AN(ICH) = A8(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'VLBA1:4' .AND. BITS .EQ. 1 .OR.
     1            FORMAT .EQ. 'VLBA1:2' .AND. BITS .EQ. 2 ) THEN
            MAXCHN = 8
            IF( TWOHEAD ) MAXCHN = 16
            DO ICH = 1, MAXCHN
               AN(ICH) = A4(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'VLBA1:2' .AND. BITS .EQ. 1 .OR.
     1            FORMAT .EQ. 'VLBA1:1' .AND. BITS .EQ. 2 ) THEN
            MAXCHN = 16
            IF( TWOHEAD ) MAXCHN = 32
            DO ICH = 1, MAXCHN
               AN(ICH) = A2(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'VLBA1:1' .AND. BITS .EQ. 1 ) THEN
            MAXCHN = 32  !  Will flag by station ability.
            DO ICH = 1, MAXCHN
               AN(ICH) = A1(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'VLBA2:1' .OR.
     1            FORMAT .EQ. 'VLBA4:1' ) THEN
            CALL WLOG( 1, 'SETTRK: Automatic track assignment not'//
     1             ' available for fan in modes yet.' )
            CALL ERRSET( KS )
         ELSE
            WRITE( TEXT, '( A, A, A, I3 )' ) 
     1          'SETTRK: Invalid VLBA FORMAT ', FORMAT, ' or bits ',
     2          BITS
            CALL WLOG( 1, TEXT )
            CALL ERRSET( KS )
         END IF
C
C        Now set TRACK based on appropriate subsets of the default
C        channels for VLBA cases.
C
         IF( TAPEMODE * NCHAN .GT. MAXCHN ) THEN
            WRITE( MSG, '( A, I3, A, I3, A, I3, A )' )
     1             'SETTRK: tpmode (', TAPEMODE, ') * nchan (', NCHAN,
     2             ') too large. Max: (', MAXCHN, ').'
            CALL WLOG( 1, MSG )
            CALL ERRSET( KS )
         END IF
         IF( TAPEMODE .GT. 0 ) THEN
            CHPASS = MAXCHN / TAPEMODE
         ELSE
            CALL WLOG( 1,
     1           'SETTRK: Programming error - TAPEMODE below 1' )
            CALL PRTSET( KS, ILOG )
            CALL ERRSET( KS )
         END IF
         DO IP = 1, TAPEMODE
            DO ICH = 1, NCHAN
               ICH1 = ( IP - 1 ) * CHPASS
               TRACK(ICH,IP) = AN(ICH1+ICH)
            END DO
         END DO
C
      ELSE IF( FORMAT(1:4) .EQ. 'MKIV' ) THEN
C
C        Do the same thing for Mark IV.  This is kept separate
C        in case it proves necessary eventually to use
C        different numbers.
C
         IF( FORMAT .EQ. 'MKIV1:4' .AND. BITS .EQ. 2 ) THEN
            MAXCHN = 4
            IF( TWOHEAD ) MAXCHN = 8
            DO ICH = 1, MAXCHN
               AN(ICH) = A8(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'MKIV1:4' .AND. BITS .EQ. 1 .OR.
     1            FORMAT .EQ. 'MKIV1:2' .AND. BITS .EQ. 2 ) THEN
            MAXCHN = 8
            IF( TWOHEAD ) MAXCHN = 16
            DO ICH = 1, MAXCHN
               AN(ICH) = A4(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'MKIV1:2' .AND. BITS .EQ. 1 .OR.
     1            FORMAT .EQ. 'MKIV1:1' .AND. BITS .EQ. 2 ) THEN
            MAXCHN = 16
            IF( TWOHEAD ) MAXCHN = 32
            DO ICH = 1, MAXCHN
               AN(ICH) = A2(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'MKIV1:1' .AND. BITS .EQ. 1 ) THEN
            MAXCHN = 32  !  Will flag by station ability later
            DO ICH = 1, MAXCHN
               AN(ICH) = A1(ICH)
            END DO
         ELSE IF( FORMAT .EQ. 'MKIV2:1' .OR.
     1            FORMAT .EQ. 'MKIV4:1' ) THEN
            CALL WLOG( 1, 'SETTRK: Automatic track assignment not'//
     1             ' available for fan in modes yet.' )
            CALL ERRSET( KS )
         ELSE
            WRITE( TEXT, '( A, A, A, I3 )' ) 
     1          'SETTRK: Invalid MKIV FORMAT ', FORMAT, ' or bits ',
     2          BITS
            CALL WLOG( 1, TEXT )
            CALL ERRSET( KS )
         END IF
C
C        Now set TRACK based on appropriate subsets of the default
C        channels for MKIV cases.
C
         IF( TAPEMODE * NCHAN .GT. MAXCHN ) THEN
            WRITE( MSG, '( A, I3, A, I3, A, I3, A )' )
     1             'SETTRK: tpmode (', TAPEMODE, ') * nchan (', NCHAN,
     2             ') too large. Max: (', MAXCHN, ').'
            CALL WLOG( 1, MSG )
            CALL ERRSET( KS )
         END IF
         IF( TAPEMODE .GT. 0 ) THEN
            CHPASS = MAXCHN / TAPEMODE
         ELSE
            CALL WLOG( 1,
     1           'SETTRK: Programming error - TAPEMODE below 1' )
            CALL PRTSET( KS, ILOG )
            CALL ERRSET( KS )
         END IF
         DO IP = 1, TAPEMODE
            DO ICH = 1, NCHAN
               ICH1 = ( IP - 1 ) * CHPASS
               TRACK(ICH,IP) = AN(ICH1+ICH)
            END DO
         END DO
C
      ELSE IF( FORMAT .EQ. 'MARK5B' ) THEN
C
         IF( DBE(1:4) .EQ. 'RDBE' .OR. DAR(1:4) .EQ. 'NONE' ) THEN
C
C           Deal with the NRAO RDBE which simply wants the tracks
C           numbered following the channel ordering.  Also use this
C           scheme for no, or unknown, formatters.
C
            IF( BITS .EQ. 2 ) THEN
               DO ICH = 1, NCHAN
                  TRACK(ICH,1) = 2*ICH
               END DO
            ELSE
               DO ICH = 1, NCHAN
                  TRACK(ICH,1) = ICH + 1
               END DO
            END IF

         ELSE
C
C           Full MARK5B systems.
C
C           The wiring is fixed between the BBC's and the "tracks".  
C           We need the channels in order of BBC number upper sideband
C           followed by BBC number, lower sideband.  I'll be crude and 
C           just loop through the channels finding the order.  MK5BCH
C           is the sequence number of this sched channel in the Mark5B
C           channels.  It is basically the index of the appropriate 
C           track assignment array to use.
C           IM5 Index for looping over the Mark5B channels
C           ICHM5B  SCHED channel of Mark5B channel
C           MK5BCH  Mark5B channel of SCHED channel
C      	 
            DO ICH = 1, NCHAN
               MK5BCH(ICH) = 0
               ICHMK5B(ICH) = 0
            END DO
            DO IM5 = 1, NCHAN
               M5BBBC = 100000
               DO ICH = 1, NCHAN
                  IF( SIDEBAND(ICH) .EQ. 'U' .AND. 
     1                KBBC(ICH) .LT. M5BBBC .AND. 
     2                MK5BCH(ICH) .EQ. 0 ) THEN
                     ICHMK5B(IM5) = ICH
                     M5BBBC = KBBC(ICH)
                  END IF
               END DO
C      	 
C              Process lower sidebands only when uppers are done.
C      	 
               IF( ICHMK5B(IM5) .EQ. 0 ) THEN
                  M5BBBC = 100000
                  DO ICH = 1, NCHAN
                     IF( SIDEBAND(ICH) .EQ. 'L' .AND. 
     1                   KBBC(ICH) .LT. M5BBBC .AND. 
     2                   MK5BCH(ICH) .EQ. 0 ) THEN
                        ICHMK5B(IM5) = ICH
                        M5BBBC = KBBC(ICH)
                     END IF
                  END DO
               END IF
               MK5BCH(ICHMK5B(IM5)) = IM5
            END DO
C      	 
C           Now assign the tracks
C      	 
            DO ICH = 1, NCHAN
               IF( BITS .EQ. 1 ) THEN
                  TRACK(ICH,1) = MK5BCH(ICH)
               ELSE IF ( BITS .EQ. 2 ) THEN
                  TRACK(ICH,1) = MK5BCH(ICH)*2
               END IF
            END DO
C
         END IF
C
      ELSE IF( FORMAT .EQ. 'LBA' ) THEN
C        Tracks (bitstreams) are simply ordered by BBC. Similar to
C        Mark5b but the two sidebands for a single BBC follow each
C        other. Note also that the dual sidebands from an LBA BBC can
C        both be USB. 2 bits is the only option.
         DO ICH = 1, NCHAN
            LBACH(ICH) = 0
         END DO
         DO ILBA = 1, NCHAN
            LBABBC = 100000
            DO ICH = 1, NCHAN
               IF( KBBC(ICH) .LT. LBABBC .AND. 
     1             LBACH(ICH) .EQ. 0 ) THEN
                  ICHLBA(ILBA) = ICH
                  LBABBC = KBBC(ICH)
               END IF
            END DO
            LBACH(ICHLBA(ILBA)) = ILBA
         END DO
         DO ICH = 1, NCHAN
C           First bitstream is 0
            TRACK(ICH,1) = LBACH(ICH)*2-2
         END DO
C
      ELSE IF( FORMAT .EQ. 'VDIF' ) THEN
C
C        Simply set the tracks for both bits to be the channel number.
C        (W. Brisken email May 14, 2012.  Change by RCW)
C
         DO ICH = 1, NCHAN
            TRACK(ICH,1) = ICH
         END DO
C
      ELSE IF( FORMAT .EQ. 'MARKIII' ) THEN
C
C        Now deal with Mark III.
C        Don't allow 2 bit data.
C
         IF( BITS .GT. 1 ) THEN
            CALL WLOG( 1, 'SETTRK: 2 bit Mark III - You''re kidding!' )
            CALL ERRSET( KS )
         END IF
C
         IF( TAPEMODE .EQ. 1 ) THEN
C
C           Test for the alternating sidebands.  Improve this some
C           day to allow for dual polarization.
C
            DO ICH = 1, NCHAN, 2
               IF( SIDEBAND(ICH) .EQ. SIDEBAND(ICH+1) ) THEN
                  CALL WLOG( 1, 'SETTRK: Auto track assignments are'//
     1              ' for alternating sidebands in MarkIII A mode. ' )
                  CALL ERRSET( KS )
               END IF
            END DO
C
C           Test number of channels.
C
            IF( NCHAN .GT. 32 ) THEN
               CALL WLOG( 1, ' SETTRK: Too many channels for Mark III'//
     1                 ' mode A. ' )
               CALL ERRSET( KS )
            END IF
C
C           Set channel assignments.
C
            DO ICH = 1, NCHAN
               TRACK(ICH,1) = M3A(ICH)
            END DO
         ELSE IF( TAPEMODE .EQ. 2 ) THEN
C
C           Test for the alternating sidebands.
C
            DO ICH = 1, NCHAN, 2
               IF( SIDEBAND(ICH) .EQ. SIDEBAND(ICH+1) ) THEN
                  CALL WLOG( 1, 'SETTRK: Auto track assignments are'//
     1              ' for alternating sidebands in MarkIII mode B. ' )
                  CALL WLOG( 1, '        This setup does not '//
     1              'alternate sidebands (is it Mode C?).' )
                  CALL ERRSET( KS )
               END IF
            END DO
C
C           Test number of channels.
C
            IF( NCHAN .GT. 16 ) THEN
               CALL WLOG( 1, ' SETTRK: Too many channels for Mark III'//
     1              ' mode B or C. ' )
               CALL ERRSET( KS )
            END IF
C
C           Set channel assignments.
C
            DO ICH = 1, NCHAN
               TRACK(ICH,1) = M3B1(ICH)
               TRACK(ICH,2) = M3B2(ICH)
            END DO
         ELSE IF( TAPEMODE .EQ. 4 ) THEN
C
C           Test number of channels.
C
            IF( NCHAN .GT. 8 ) THEN
               CALL WLOG( 1, ' SETTRK: Too many channels for Mark III'//
     1              ' mode E. ' )
               CALL ERRSET( KS )
            END IF
C
C           Set channel assignments.
C
            DO ICH = 1, NCHAN
               TRACK(ICH,1) = M3E1(ICH)
               TRACK(ICH,2) = M3E2(ICH)
               TRACK(ICH,3) = M3E3(ICH)
               TRACK(ICH,4) = M3E4(ICH)
            END DO
         END IF
C
      ELSE
         TEXT = FORMAT
         CALL WLOG( 1, 'SETTRK: Invalid FORMAT ' // TEXT(1:8) )
         CALL ERRSET( KS )
      END IF
C
      RETURN
      END




