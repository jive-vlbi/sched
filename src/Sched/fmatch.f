      SUBROUTINE FMATCH( KS, KF, OVERLAP, CENTER, OKIF, USEIF )
C
C     Routine for SCHED called by SETFCAT that finds the best IF
C     channel from the frequency catalog to use for a setup file 
C     channel. 
C
C     KS is the setup group.
C     KF is the frequency catalog group.
C     OVERLAP is the amount of bandwidth overlap.
C     CENTER is offset of the center of the channel from the center
C            of the IF band.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER           KS, KF, IIF, ICH, USEIF(MCHAN)
      REAL              OVERLAP, CENTER
      REAL              BESTCN
      REAL              CHOVER(MCHAN,MFIF), CHCENT(MCHAN,MFIF)
      LOGICAL           OKIF(MCHAN,MFIF)
      DOUBLE PRECISION  FRQ1(MCHAN), FRQ2(MCHAN)
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'FMATCH starting' )
C
C     Loop over the channels.
C
      DO ICH = 1, NCHAN(KS)
C
C        Get the minimum and maximum frequency in each channel.
C
         IF( NETSIDE(ICH,KS) .EQ. 'L' ) THEN
            FRQ1(ICH) = FREQREF(ICH,KS) - BBFILT(ICH,KS)
            FRQ2(ICH) = FREQREF(ICH,KS)
         ELSE
            FRQ1(ICH) = FREQREF(ICH,KS)
            FRQ2(ICH) = FREQREF(ICH,KS) + BBFILT(ICH,KS)
         END IF
C
C        Loop over the frequency catalog IF channels looking for
C        a match.  Default to USEIF(ICH) = 1 it there is no match.
C
         USEIF(ICH) = 1
         DO IIF = 1, MFIF
C
C           Be sure this is an ok match according to FCOMPARE.
C
            IF( OKIF(ICH,IIF) ) THEN
C
C              Check how much bandwidth overlaps.  
C
               IF( FRQ2(ICH) .GT. FRF1(IIF,KF) .AND. 
     1             FRQ1(ICH) .LT. FRF2(IIF,KF) ) THEN
                  CHOVER(ICH,IIF) = MIN( FRQ2(ICH), FRF2(IIF,KF) ) - 
     1                              MAX( FRQ1(ICH), FRF1(IIF,KF) )
               ELSE
                  CHOVER(ICH,IIF) = 0.0
               END IF
C
C              Get centering - the offset between the middle of the IF
C              and the middle of the baseband.
C
               CHCENT(ICH,IIF) = ABS( ( FRF1(IIF,KF)+FRF2(IIF,KF) )/2.0
     1                - ( FRQ1(ICH) + FRQ2(ICH) ) / 2.0 )
C
               IF( SDEBUG ) THEN
                  SETMSG = ' '
                  WRITE( SETMSG, '( A, 2I4, 4F10.3)' ) 
     1                  'FMATCH: Channel ',
     2                  ICH, IIF, FRQ1(ICH), FRQ2(ICH), 
     3                  CHOVER(ICH,IIF),CHCENT(ICH,IIF)
                  CALL WLOG( 0, SETMSG )
               END IF
            END IF
         END DO
C
C        Get the best IF for the channel.
C        Try for best frequency match (best centered with all other
C        parameters right (OKIF true).
C
         BESTCN = 1.E5
         DO IIF = 1, MFIF
            IF( OKIF(ICH,IIF) ) THEN
               IF( CHCENT(ICH,IIF) .LT. BESTCN ) THEN
                  BESTCN = CHCENT(ICH,IIF)
                  USEIF(ICH) = IIF
               END IF
            END IF
         END DO
C
      END DO
C
C     Loop over the channels and get the total overlap bandwidth
C     and the worst center.  These will be used to select the best
C     frequency catalog group.
C
      OVERLAP = 0.0
      CENTER = 0.0
      DO ICH = 1, NCHAN(KS)
         IIF = USEIF(ICH)
         OVERLAP = OVERLAP + CHOVER(ICH,IIF)
         CENTER = MAX( CENTER, CHCENT(ICH,IIF) )
      END DO
C
      RETURN
      END
