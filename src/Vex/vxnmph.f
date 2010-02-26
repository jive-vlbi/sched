      CHARACTER*32 FUNCTION VXNMPH( IPH, XTRAFQ )
C
C     function generates a name for PH block IPH (phase cal)
C            (HP corrected to PH  RCW Feb. 9, 2010)
C     By H.J. van Langevelde, JIVE, 300496
C     IPH, is number of LINK, necessary to find SET
C     XTRAFQ = .TRUE. used for extra frequencies detected
C     in main schedule, uses VXFQ etc
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      LOGICAL XTRAFQ
      INTEGER IPH, KS, ICH
      LOGICAL UPSIDE, LOSIDE, PHDETECT
      CHARACTER NAME*32
C ----------------------------------------------------------------------
C     
C     These are named according to increment and sideband, but they could
C     depend on frequency setting
C
      NAME = ' '
      KS = PHISSET( IPH ) 
C
C     first, what sideband
C
      UPSIDE = .FALSE.
      LOSIDE = .FALSE.
      DO ICH = 1, NCHAN(KS)
         IF( XTRAFQ ) THEN
            IF( VXNETSID(ICH,IPH) .EQ. 'U' ) UPSIDE = .TRUE.
            IF( VXNETSID(ICH,IPH) .EQ. 'L' ) LOSIDE = .TRUE.
         ELSE
            IF( NETSIDE(ICH,KS) .EQ. 'U' ) UPSIDE = .TRUE.
            IF( NETSIDE(ICH,KS) .EQ. 'L' ) LOSIDE = .TRUE.
         END IF
      END DO
      IF( UPSIDE .AND. LOSIDE ) THEN
         NAME(1:3) = 'Dsb'
      ELSE
         IF( UPSIDE ) THEN
            NAME(1:3) = 'Usb'
         ELSE IF( LOSIDE ) THEN 
            NAME(1:3) = 'Lsb'
         ELSE
            CALL ERRLOG('VXNMPH: No sideband set ')
         END IF
      END IF
C
C     and increment
C
      IF( XTRAFQ ) THEN
C
         PHDETECT = DODETECT(IPH)
      ELSE
         PHDETECT = .TRUE.
         IF( SPCAL(KS) .EQ. 'off' ) PHDETECT = .FALSE.
         IF( SPCAL(KS) .EQ. '1MHz' ) PHDETECT = .TRUE.
         IF( SPCAL(KS) .EQ. '5MHz' ) PHDETECT = .TRUE.
         IF( SPCAL(KS) .EQ. ' ' ) PHDETECT = .TRUE.
      END IF
C     
C     so an appropriate name is
C        
      NAME(4:12) = 'NotSet' 
      IF( PHDETECT ) THEN
         NAME(4:13) = 'Detect'
      ELSE
         NAME(1:11) = 'NoDetect'
      END IF
C
      VXNMPH = NAME
C
      RETURN
      END
