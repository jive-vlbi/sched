      SUBROUTINE BBCALT( KS, MAXBBC, MAXIF, IFBBC, IFNAM, UBBC, NNBBC,
     1                   WARNING )
C
C     Routine for SCHED called by BBCGEO, BBCM4, and BBCVS2 to assign 
C     BBC's to channels.  BBCGEO is for VLBA systems with geodetic 
C     wiring.  BBCM4 is for Mark IV systems.  BBCVS2 is for S2 systems
C     attached to VLBA DAR's.  They are similar in many ways
C     and the code needed to process them is the same.  They differ
C     in the wiring configurations as embodied in the IFBBC and IFNAM
C     arrays.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ICH, JCH, KS, IBBC, NNBBC, IIF
      INTEGER    MAXBBC, MAXIF
      INTEGER    IFBBC(MAXBBC,MAXIF)
      CHARACTER  IFNAM(MAXIF)*2, WARNING*(*)
      LOGICAL    UBBC(MAXBBC)
C -------------------------------------------------------------------  
      IF( DEBUG ) CALL WLOG( 0, 'BBCALT starting.' )
C
C     Initialize the array that flags used BBC's.
C
      DO IBBC = 1, NNBBC
         UBBC(IBBC) = .FALSE.
      END DO
C
C     Flag those that have been used.
C
      DO ICH = 1, NCHAN(KS)
         IF( BBC(ICH,KS) .GT. 0 ) UBBC(BBC(ICH,KS)) = .TRUE.
      END DO
C
C     Loop through channels assigning the channel to the
C     same BBC as a previous one with the same FREQREF and IFCHAN,
C     if there is one (can be difference sideband), or to a new BBC.
C
      DO ICH = 1, NCHAN(KS)
         IF( BBC(ICH,KS) .EQ. 0 .AND. ICH .GT. 1 ) THEN
            DO JCH = 1, ICH-1
               IF( FREQREF(ICH,KS) .EQ. FREQREF(JCH,KS) ) THEN
                  IF( IFCHAN(ICH,KS) .EQ. IFCHAN(JCH,KS) ) THEN
                     BBC(ICH,KS) = BBC(JCH,KS)
                     GO TO 100
                  ELSE IF( ALTIFC(ICH,KS) .EQ. IFCHAN(JCH,KS) ) THEN
                     BBC(ICH,KS) = BBC(JCH,KS)
                     IFCHAN(ICH,KS) = ALTIFC(ICH,KS)
                     GO TO 100
                  END IF
               END IF
            END DO
  100       CONTINUE
         END IF
C
C        If BBC is still 0, assign the next one that can see this IF.
C
         IF( BBC(ICH,KS) .EQ. 0 ) THEN
C
C           Find the next available BBC that can see this IFCHAN.
C           Take the alt chan if available and necessary.
C
            DO IBBC = 1, NNBBC
               IF( .NOT. UBBC(IBBC) ) THEN
                  DO IIF = 1, MAXIF
                     IF( IFCHAN(ICH,KS) .EQ. IFNAM(IIF) .AND.
     1                   IFBBC(IBBC,IIF) .EQ. 1 ) THEN
                        BBC(ICH,KS) = IBBC                  
                        UBBC(IBBC) = .TRUE.
                        GO TO 200
                     END IF
                     IF( ALTIFC(ICH,KS) .EQ. IFNAM(IIF) .AND.
     1                   IFBBC(IBBC,IIF) .EQ. 1 ) THEN
                        BBC(ICH,KS) = IBBC                  
                        UBBC(IBBC) = .TRUE.
                        IFCHAN(ICH,KS) = ALTIFC(ICH,KS)
                        GO TO 200
                     END IF
                  END DO
               END IF
            END DO
C
C           If get here, did not assign a BBC.
C
            CALL WLOG( 1, ' ' )
            CALL WLOG( 1, 'BBCALT:  BBC setting problem: ' )
            CALL WLOG( 1, '         Setup file: '//SETNAME(KS) )
            CALL WLOG( 1, '         Station: '//SETSTA(1,KS) )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, A, A, A, A, I3 )' )
     1          '         ICHAN=', ICH, '  IFNAME=', IFCHAN(ICH,KS), 
     2          '  ALTIFN=', ALTIFC(ICH,KS), '  NNBBC=', NNBBC
            CALL WLOG( 0, MSGTXT )
            CALL WRTMSG( 0, 'BBCALT', 'noassignbbc' )
C
C           A number of special cases need more explanation.
C
            IF( WARNING .EQ. 'GEO1' .OR. WARNING .EQ. 'GEO2' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 3A )' ) '        ',
     1           'Note that ''geodetic'' VLBA systems',
     2           ' have restrictions on IF assignments.' 
               CALL WLOG( 0, MSGTXT )
               CALL WLOG( 0, '        See IFCHAN in the manual.' )
            END IF
            IF( WARNING .EQ. 'GEO2' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 3A )' ) '        ',
     1           '  They also cannot use 2 bits from more than 8 BBCs.'
               CALL WLOG( 0, MSGTXT )
            END IF
            IF( WARNING .EQ. 'S2' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' )
     1                'Note that the S2 system is not hooked to all ',
     2                'BBCs.'              
               CALL WLOG( 0, MSGTXT )
            END IF
            CALL ERRSET( KS )
C
         END IF
C
C        Jump here when have an assignment.
C
  200    CONTINUE
C
      END DO
C
      RETURN 
      END
