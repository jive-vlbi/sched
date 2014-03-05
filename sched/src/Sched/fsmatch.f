      LOGICAL FUNCTION FSMATCH( KF, JF )
C
C     Check if two frequency sets are the same.
C     The frequency sets and the pulse cal sets need to have 
C     already been established.  This allows, for example, the 
C     xx.sum file to only list the details for one of the frequency
C     sets and just flag the rest as being the same.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER   KF, JF
      INTEGER   ICH, KS, JS, NNCHAN, NCCHAN
      LOGICAL   SAMESET
      DOUBLE PRECISION   KBBCFREQ(MCHAN), KBBCBW(MCHAN)
      DOUBLE PRECISION   KLOSUM(MCHAN)
      DOUBLE PRECISION   JBBCFREQ(MCHAN), JBBCBW(MCHAN)
      DOUBLE PRECISION   JLOSUM(MCHAN)
      INTEGER            KCRDN, JCRDN, KCRSETC(MAXCHN), JCRSETC(MAXCHN)
      DOUBLE PRECISION   KCRDF(MCHAN), KCRDB(MCHAN), KCRDLOSU(MCHAN)
      DOUBLE PRECISION   JCRDF(MCHAN), JCRDB(MCHAN), JCRDLOSU(MCHAN)
      CHARACTER          KCRDS(MCHAN)*1, JCRDS(MCHAN)*1
C -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'FSMATCH starting' )
C
C     Get the setup group numbers these are from.
C
      KS = FSETKS(KF)
      JS = FSETKS(JF)
C
C     Initialize the output using FSMATCH, which checks if the
C     setup groups are basically the same except the station and
C     the information in the frequency set.  This also assures
C     that the number of channels is the same.
C
      FSMATCH = SAMESET( KS, JS )
C
C     Now compare the frequency set information as delivered
C     by FSFREQ.  This includes LOSUM, FREQ, and BW for the main
C     observing and the CRD parameters for the VLBA station
C     legacy systems.  The CRD parameters should automatically match
C     for non-VLBA stations because of the way FSFREQ deals with them.
C
      IF( FSMATCH ) THEN
         CALL FSFREQ( KF, KLOSUM, KBBCFREQ, KBBCBW,
     1         KCRDN, KCRDF, KCRDB, KCRDS, KCRDLOSU, KCRSETC )
         CALL FSFREQ( JF, JLOSUM, JBBCFREQ, JBBCBW,
     1         JCRDN, JCRDF, JCRDB, JCRDS, JCRDLOSU, JCRSETC )
C
C        Loop over the main setup channels.  Recall that SAMESET
C        assures that NCHAN(KS) = NCHAN(JS)
         NNCHAN = NCHAN(KS)
         DO ICH = 1, NNCHAN
            IF( ABS( KLOSUM(ICH) - JLOSUM(ICH) ) .GT. 1.0D-6 .OR.
     1          ABS( KBBCFREQ(ICH) - JBBCFREQ(ICH) ) .GT. 1.0D-6 .OR.
     2          ABS( KBBCBW(ICH) - JBBCBW(ICH) ) .GT. 1.0D-6 ) THEN
               FSMATCH = .FALSE.
            END IF
         END DO
C
C        Compare the CRD channels.
C
         FSMATCH = FSMATCH .AND. KCRDN .EQ. JCRDN
         NCCHAN = KCRDN
         IF( FSMATCH .AND. NCCHAN .GT. 0 ) THEN
            DO ICH = 1, NCCHAN
               IF( ABS( KCRDF(ICH) - JCRDF(ICH) ) .GT. 1.0D-6 .OR.
     1             ABS( KCRDB(ICH) - JCRDB(ICH) ) .GT. 1.0D-6 .OR.
     2             KCRDS(ICH) .NE. JCRDS(ICH) .OR.
     3             ABS( KCRDLOSU(ICH) - JCRDLOSU(ICH) ) .GT. 1.0D-6 ) 
     4                THEN
                  FSMATCH = .FALSE.
               END IF
            END DO
         END IF
      END IF
C
C     Check for same pcal generator setting.  Any specified pcalx 
C     settings are in the setup groups and compared there.  If those
C     have to be modified because of a scan specified PCAL, the nature
C     of the new ones is forced in FSFREQ by the setting of PCAL.
C     to check for same settings, it is only necessary that the 
C     frequency set and the PCAL setting are the same.
C
      FSMATCH = FSMATCH .AND. FSPCAL(KF) .EQ. FSPCAL(JF)
C
      RETURN
      END
