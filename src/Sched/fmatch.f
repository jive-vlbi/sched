      SUBROUTINE FMATCH( KS, KF, OVERLAP, CENTER, OKIF, USEIF )
C
C   ****  Something is dangling here for RDBE_DDC - CROSS is not yet used.
C          CROSS in RDBEMTCH   Fixed mar 2014?
C        
C     Routine for SCHED called by SETFCAT that finds the best IF
C     channel from the frequency catalog to use for a setup file 
C     channel.   By this point, FREQREF, BBFILT, and NETSIDE 
C     are known.  Other items may still be unset.
C
C     KS is the setup group.
C     KF is the frequency catalog group.
C     OVERLAP is the amount of bandwidth overlap.
C     CENTER is maximum offset of the center of the any channel from 
C            the center of the IF band.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER           KS, KF, IIF, ICH
      INTEGER           USEIF(MCHAN)
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
C        Also get the baseband converter frequency.
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
C        a match.  Default to USEIF(ICH) = 0 if there is no match for now.
C        Recall that OKIF means the channel matches except that the 
C        frequency might be off by some tolerance set in FCOMPARE. 
C        That tolerance was 20% when this was written.
C        Set the default CHOVER and CHCENT to discourage a channel with
C        false OKIF.
C
         USEIF(ICH) = 0
         DO IIF = 1, FNIF(KF)
            CHOVER(ICH,IIF) = 0.0
            CHCENT(ICH,IIF) = 1.E5
C
C           Be sure this is an ok match according to FCOMPARE.
C
            IF( OKIF(ICH,IIF) ) THEN
C
C              Check how much bandwidth overlaps.
C              For the RDBE, only count the part between the
C              LO sum and a crossover if one is in the channel.
C
               IF( FRQ2(ICH) .GT. FRF1(IIF,KF) .AND. 
     1             FRQ1(ICH) .LT. FRF2(IIF,KF) ) THEN
                  CHOVER(ICH,IIF) = MIN( FRQ2(ICH), FRF2(IIF,KF) ) - 
     1                              MAX( FRQ1(ICH), FRF1(IIF,KF) )
                  IF( DBE(KS)(1:4) .EQ. 'RDBE' ) THEN
                     CALL RDBEMTCH( KS, KF, ICH, IIF, 
     1                   FRF1(IIF,KF), FRF2(IIF,KF),
     2                   FRQ1(ICH), FRQ2(ICH), CHOVER(ICH,IIF) )
                  END IF
               ELSE
                  CHOVER(ICH,IIF) = 0.0
               END IF
C
C              Get centering - the offset between the middle of the IF
C              and the middle of the baseband.
C
               CHCENT(ICH,IIF) = ABS( 
     1                  ( FRF1(IIF,KF) + FRF2(IIF,KF) )/2.0
     2                - ( FRQ1(ICH) + FRQ2(ICH) ) / 2.0 )
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
         DO IIF = 1, FNIF(KF)
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
C     frequency catalog group.  If a channel did not fit all, even
C     with the frequency tolerance USEIF(ICH) will be zero.
C     Allow for that possibility in the calling routine.
C
      OVERLAP = 0.0
      CENTER = 0.0
      DO ICH = 1, NCHAN(KS)
         IIF = USEIF(ICH)
         IF( IIF .GT. 0 ) THEN
            OVERLAP = OVERLAP + CHOVER(ICH,IIF)
            CENTER = MAX( CENTER, CHCENT(ICH,IIF) )
         END IF
      END DO
C
      RETURN
      END
