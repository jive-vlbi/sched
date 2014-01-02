      SUBROUTINE BBCALT( KS, MAXBBC, MAXIF, IFBBC, IFNAM, UBBC, NNBBC,
     1                   WARNING, CALLER )
C
C     Routine for SCHED called by BBCGEO, BBCM4, BBCVS2, BBCDBBC and
C     BBCLBA to assign BBC's to IF channels.  BBCGEO is for VLBA systems
C     with geodetic wiring.  BBCM4 is for Mark IV systems.  BBCVS2 is
C     for S2 systems attached to VLBA DAR's. BBCDBBC is for Hat-lab's
C     (Tuccari) DBBC. BBCLBA is for the Australian LBA DAS. They are
C     similar in many ways and the code needed to process them is the
C     same.  They differ in the wiring configurations as embodied in the
C     IFBBC and IFNAM arrays.  This routine is not used for VLBA systems.
C
C     Inputs are:
C        KS            Setup group
C        MAXBBC        The maximum number of BBCs available.
C        MAXIF         The maximum number of IFs available.
C        IFBBC         Array of flags (1,0) MAXBBC by MAXIF indicating
C                      whether the BBC can see the IF
C        IFNAM         The names of the IFs
C        CALLER        The calling subroutine name to help debugging.
C     Outputs are:
C        UBBC          Flag for BBCs that were assigned.
C        NNBBC         
C        WARNING       Text indicating the backend type.
C        
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ICH, JCH, KS, IBBC, NNBBC, IIF, i
      INTEGER    MAXBBC, MAXIF, MAXIFI
      PARAMETER  (MAXIFI=8)
      INTEGER    IFBBC(MAXBBC,MAXIF)
      CHARACTER  IFNAM(MAXIF)*2, WARNING*(*), CHKNAM*2
      CHARACTER  IFINPUT(MAXIFI)*2, CALLER*(*)
      LOGICAL    UBBC(MAXBBC)
C -------------------------------------------------------------------  
      IF( DEBUG ) THEN
         MSGTXT = 'BBCALT starting. Called by '// CALLER
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     Since MAXIF could not be used for IFINPUT, check that MAXIFI
C     is big enough.
C
      IF( MAXIF .GT. MAXIFI ) CALL ERRLOG( 'BBCALT:  MAXIF bigger '//
     1  'than MAXIFI - programming error' )
C
C     Initialize the array that flags used BBC's.
C
      DO IBBC = 1, NNBBC
         UBBC(IBBC) = .FALSE.
      END DO
C
C     Initialize the array that flags used IF inputs on the DBBC
C
      DO IIF = 1, MAXIF
         IFINPUT(IIF) = 'ZZ'
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
C
C                    For DBBC only first letter of IFCHAN is
C                    significant, unless an input has already been
C                    assigned for this IF.
C
C                    From Uwe Bach email of Dec. 7, 2013, at least at
C                    Effelsberg, RCP is on A1 and B1 while LCP is on
C                    A3 and B3.  So it looks like the number is also
C                    important.  The IFNAMs defined in BBCDBBC are
C                    A, B, C, D, so I'm not quite sure how this relates.
C                    I smell a possible naming mess.
C
C                    Meanwhile, I'm trying to force the IF names in
C                    hsa1cm.key and it is not accepting what I give.
C                    So at least fix that.
C
                     CHKNAM = IFNAM(IIF)
                     IF( WARNING .EQ. 'DBBC' ) THEN
                        IF( IFINPUT(IIF) .EQ. 'ZZ' ) THEN
                           CHKNAM = CHKNAM(1:1)//IFCHAN(ICH,KS)(2:2)
                        ELSE
                           CHKNAM=IFINPUT(IIF)
                        END IF
                     END IF
C
                     IF( IFCHAN(ICH,KS) .EQ. CHKNAM .AND.
     1                   IFBBC(IBBC,IIF) .EQ. 1 ) THEN
                        BBC(ICH,KS) = IBBC                  
                        UBBC(IBBC) = .TRUE.
                        IFINPUT(IIF) = IFCHAN(ICH,KS)
                        GO TO 200
                     END IF
                     IF( WARNING .EQ. 'DBBC' ) THEN
                        IF( IFINPUT(IIF) .EQ. 'ZZ' ) THEN
                           CHKNAM = CHKNAM(1:1)//ALTIFC(ICH,KS)(2:2)
                        ELSE
                           CHKNAM=IFINPUT(IIF)
                        END IF
                     END IF
C
                     IF( ALTIFC(ICH,KS) .EQ. CHKNAM .AND.
     1                   IFBBC(IBBC,IIF) .EQ. 1 ) THEN
                        BBC(ICH,KS) = IBBC                  
                        UBBC(IBBC) = .TRUE.
                        IFCHAN(ICH,KS) = ALTIFC(ICH,KS)
                        IFINPUT(IIF) = IFCHAN(ICH,KS)
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
     2          '  ALTIFC=', ALTIFC(ICH,KS), '  NNBBC=', NNBBC
            CALL WLOG( 0, MSGTXT )
            CALL WRTMSG( 0, 'BBCALT', 'noassignbbc' )
            IF( ICH .GT. 5 ) CALL WLOG( 1, 
     1          '         Maybe too many channels are requested'//
     2          ' for the same IF channel.' )
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
