      SUBROUTINE BBCDBBC( KS )
C
C
C     Routine for Sched called by SETBBC that assigns video converters
C     for Gino Tuccari's DBBC system, as used at Effelsberg.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER   KS, ICH, JCH
      INTEGER   MAXBBC, MAXIF, I
      PARAMETER (MAXBBC=16)
      PARAMETER (MAXIF=4)
      INTEGER IBBCA, IBBCB, IBBCC, IBBCD
C
C -------------------------------------------------------------------  
C     Software check:
C
      call wlog(1, 'In bbcdbbc.f')
      IF( NBBC(ISETSTA(KS)) .GT. MAXBBC ) THEN
         WRITE( MSGTXT, '( 3A, I4 )' )
     1       'BBCDBBC: Number of VCs at ', SETSTA(1,KS), 
     2       ' Larger than maximum expected: ', MAXBBC
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '   Catalog or programming problem ' )
         CALL ERRSET( KS )
      END IF
      IBBCA = 1
      IBBCB = 5
      IBBCC = 9
      IBBCD = 13

      DO ICH = 1, NCHAN(KS)
         IF (ifchan(ICH, KS).EQ.' ') THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3 )' )
     1          'BBCDBBC: IFNAME not set for channel ', ICH
            CALL WLOG(0, MSGTXT)
            CALL ERRSET( KS )
         ELSE
C     See if we can reuse a BBC with the same FREQREF and IFCHAN
            IF( BBC(ICH,KS) .EQ. 0 .AND. ICH .GT. 1 ) THEN
               DO JCH = 1, ICH-1
                  IF( FREQREF(ICH,KS) .EQ. FREQREF(JCH,KS) .AND.
     1                 IFCHAN(ICH,KS) .EQ. IFCHAN(JCH,KS) ) THEN
                     BBC(ICH,KS) = BBC(JCH,KS)
                     GO TO 100
                  END IF
               END DO
  100          CONTINUE
            END IF
C           If BBC still not set:        
            IF( BBC(ICH,KS) .EQ. 0 ) THEN
               write(msgtxt, '( A, I3 )')
     1           'BBCDBBC: doing ', ICH
               call wlog(1, MSGTXT)
               IF ((ifchan(ICH, KS).EQ.'A').AND.(IBBCA.LE.4)) THEN
                  BBC(ICH, KS) = IBBCA
                  IBBCA = IBBCA + 1
               ELSE IF ((ifchan(ICH, KS).EQ.'B').AND.(IBBCB.LE.8)) THEN
                  BBC(ICH, KS) = IBBCB
                  IBBCB = IBBCB + 1
               ELSE IF ((ifchan(ICH, KS).EQ.'C').AND.(IBBCC.LE.12)) THEN
                  BBC(ICH, KS) = IBBCC
                  IBBCB = IBBCB + 1
               ELSE IF ((ifchan(ICH, KS).EQ.'D').AND.(IBBCD.LE.16)) THEN
                  BBC(ICH, KS) = IBBCD
                  IBBCB = IBBCB + 1
               ELSE
                  write(msgtxt, '( A, I3 )')
     1                 'BBCDBBC: Oops! ', ICH
                  call wlog(1, MSGTXT)
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I3, 2A )' )
     1                 'BBCDBBC: Error setting IF for channel ', ICH,
     2                 ' IFCHAN=', IFCHAN(ICH, KS)
                  CALL WLOG(0, MSGTXT)
                  CALL ERRSET( KS )
               END IF
            END IF
         END IF
      END DO
      call wlog(1, 'Leaving bbcdbbc.f')

C
      RETURN 
      END
