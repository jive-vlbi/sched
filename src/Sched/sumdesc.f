      CHARACTER*100 FUNCTION SUMDESC( ITEM, LENGTH, LABFLAG )
C
C     Make the summary file descriptions for the line items.
C
      INCLUDE        'sched.inc'
C
      CHARACTER      ITEM*(*), SITEM*20
      INTEGER        LEN1, LENGTH
      LOGICAL        LABFLAG
C --------------------------------------------------------------------
C     Make copy of ITEM so it can be concatenated (g77 won't 
C     concatinate variables of unknown length).
C
      SITEM = ITEM
      LABFLAG = .FALSE.
C
C     Make the summary line.
C
      IF( ITEM .EQ. 'EL1' ) THEN
         SUMDESC = 'Start elevation.'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'EL2' ) THEN
         SUMDESC = 'End elevation.'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'ELA' ) THEN
         SUMDESC = 'Average elevation.'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'AZ1' ) THEN
         SUMDESC = 'Start azimuth.'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'AZ2' ) THEN
         SUMDESC = 'End azimuth.'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'AZA' ) THEN
         SUMDESC = 'Average azimuth.'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'PA1' ) THEN
         SUMDESC = 'Start paralactic angle.'
C
      ELSE IF( ITEM .EQ. 'PA2' ) THEN
         SUMDESC = 'Stop paralactic angle.'
C
      ELSE IF( ITEM .EQ. 'HA1' ) THEN
         SUMDESC = 'Start hour angle.'
C
      ELSE IF( ITEM .EQ. 'HA2' ) THEN
         SUMDESC = 'Stop hour angle.'
C
C     The following sections are tape items.  Remove some day.
C
      ELSE IF( ITEM .EQ. 'TAPE1' ) THEN
         IF( NOSET ) THEN
            CALL ERRLOG( 'SUMDESC:  Cannot use SUMITEM=TAPE1 and ' //
     1           'NOSETUP together. ' )
         END IF
         SUMDESC = 'Gbytes at scan end.  Better to use DISK in SUMITEM.'
C
      ELSE IF( ITEM .EQ. 'TAPE2' ) THEN
         IF( NOSET ) THEN
            CALL ERRLOG( 'SUMDESC:  Cannot use SUMITEM=TAPE2 and ' //
     1           'NOSETUP together. ' )
         END IF
         SUMDESC = 'Not used - no tapes used in this observation.'
C
      ELSE IF( ITEM .EQ. 'DISK' .OR. ITEM .EQ. 'DISC' ) THEN
         SUMDESC = 'Disk total GBytes at end of scan.'
C
      ELSE IF( ITEM .EQ. 'TPSTART' ) THEN
         SUMDESC = 'Seconds recording starts before scan start time.'
C
      ELSE IF( ITEM .EQ. 'EARLY' ) THEN
         SUMDESC = 'Seconds antenna is on source before scan start.'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'DWELL ' ) THEN
         SUMDESC = 'Seconds antenna is on source during scan. '//
     1      '0 shown if down, rise, or set'
         LABFLAG = .TRUE.
C
      ELSE IF( ITEM .EQ. 'SLEW' ) THEN
         SUMDESC = 'Slew time in seconds from previous source.'
C
      ELSE IF( ITEM .EQ. 'SYNC' ) THEN
         SUMDESC = 'Seconds the correlator is expected to be ' // 
     1        'synced up during the scan.'
C
      ELSE IF( ITEM .EQ. ' ' ) THEN
         SUMDESC = 'Nothing requested with SUMITEM.'
C
      ELSE
C
C        Unrecognized item.
C
         CALL ERRLOG( 'SUMDESC: Summary item not recognized: '//
     1         SITEM )
      END IF
C
C     Get the length
C
      LENGTH = LEN1( SUMDESC )
C
      RETURN
      END

