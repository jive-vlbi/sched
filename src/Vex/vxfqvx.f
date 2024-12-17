      SUBROUTINE VXFQVX
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020596 
C     Sets up a few basic features for FQ and PH section
C     to be indexed by VEX internal def section, 
C     rather than through SCHED SETs in order to enable scan by scan 
C     changes of frequency and phase cal.
C     Updated for 1.5 240796, Huib
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER   IIF, IFQ, IPH, KS, ISIDE1, ICH
      CHARACTER UPCAL*4
C ----------------------------------------------------------------------
C
      WRITE( MSGTXT, '( A )' ) 
     1    'VXFQVX: Start digesting frequency and phasecal links '
      IF( DEBUG ) CALL WLOG( 1, MSGTXT )
C
C     set phasecal in numerical variable for all IF sections
C
      DO IIF = 1, NIFVEX
         KS = IFISSET(IIF)
         UPCAL = SPCAL(KS)
         CALL UPCASE(UPCAL)
         IF( UPCAL .EQ. 'OFF' ) THEN
            TONEINT(IIF) = 0.
         ELSE IF( UPCAL .EQ. '1MHZ' ) THEN 
            TONEINT(IIF) = 1.
         ELSE IF( UPCAL .EQ. '5MHZ' ) THEN
            TONEINT(IIF) = 5.D0
         ELSE IF( UPCAL .EQ. ' ' ) THEN
            TONEINT(IIF) = 1.
         ELSE 
            CALL ERRLOG( 'VXFQVX: Invalid PCAL: '//SPCAL(KS)//
     1           ' - Must be 1MHz, 5MHz, or off.' )
         END IF      
      END DO
C     
C     Need to find a few things for each FQ
C
      DO IFQ = 1, NFQVEX
         KS = FQISSET(IFQ)
         NVXCHN(IFQ) = NCHAN(KS)
         DO ICH = 1, NCHAN(KS)
            IF( SIDE1(ICH,KS) .EQ. 'U' ) THEN
               ISIDE1 = 1
            ELSE IF( SIDE1(ICH,KS) .EQ. 'L' ) THEN
               ISIDE1 = -1
            ELSE
               CALL ERRLOG('VXFQVX: First LO sideband problem.' )
            END IF
            VXLOSUM(ICH,IFQ) = FIRSTLO(ICH,KS) + ISIDE1 * BBSYN(ICH,KS)
            VXNETSID(ICH,IFQ) = NETSIDE(ICH,KS)
            VXBBFILT(ICH,IFQ) = BBFILT(ICH,KS)
         END DO
      END DO
C
C     For PH need to know whether will detect or not 
C
      DO IPH = 1, NPHVEX
         KS = PHISSET(IPH)
         UPCAL = SPCAL(KS)
         CALL UPCASE(UPCAL)
         IF( UPCAL .EQ. 'OFF' ) THEN
            DODETECT(IPH) = .FALSE.
         ELSE IF( UPCAL .EQ. '1MHZ' ) THEN 
            DODETECT(IPH) = .TRUE.
         ELSE IF( UPCAL .EQ. '5MHZ' ) THEN
            DODETECT(IPH) = .TRUE.
         ELSE IF( UPCAL .EQ. ' ' ) THEN
            DODETECT(IPH) = .TRUE.
         ELSE 
            CALL ERRLOG( 'VXFQVX: Invalid PCAL: '//SPCAL(KS)//
     1           ' - Must be 1MHz, 5MHz, or off.' )
         END IF      
C
      END DO
      RETURN
      END




