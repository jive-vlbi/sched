      SUBROUTINE GINTENT( ISCN, KD, KC, KI, BLANK )
C
C     Get the INTENTS from the scan input.
C
C     There will be a master list of INTENTS encountered.  That list
C     is stored in INTENT(MINTENT)*80.  It contains NINTENT strings
C     of up to 80 characters.
C
C     For each scan, there is a list of pointers to the intents
C     used.  That list is in ISCINT(MSCINT,MAXSCN) and contains
C     NSCINT(MAXSCN) entries for each scan.
C
C     Initially, MINTENT=100, MSCINT=25. (in sched.inc).
C
C     There is a SCHED input INTENTs (ie the "s" is optional) that
C     has MSCINT entries of up to 80 characters each.  If none
C     are given, the last scan's will be kept.  Otherwise, all
C     must be given again.  This may not be the desirable behavior
C     in the long run, but I think anything else requires parsing
C     the intents, which I don't want to do yet.
C
C     Started May 8, 2012.  RCW.
C
      INCLUDE             'sched.inc'
C
      INTEGER             ISCN, KI(*)
      CHARACTER           KC(*)*(*) 
      DOUBLE PRECISION    KD(*), BLANK
C
      INTEGER             I, I1, J, J1, K, KEYPTR
      LOGICAL             GOTNONE
      CHARACTER           INTEXT*80, CAPTEXT*80
C ----------------------------------------------------------------
C
C     The inputs are reset each scan.  Check if there is anything
C     new.  Assume that that no one starts with more than 8 blanks.
C
      GOTNONE = .FALSE.
      I1 = KEYPTR( 'INTENTs', KC, KI )
      IF( KD(I1) .NE. BLANK ) THEN
         NSCINT(ISCN) = 0
         DO I = 1, MSCINT
            J1 = I1 + ( I - 1 ) * 10
C
C           Extract the string.  Normally KCHAR would be used for
C           this, but that doesn't work for arrays of strings.
C           So use a simple write, as I did for source names in
C           rdsrc.f.
C
            WRITE( INTEXT, '(10A8)' ) ( KD(J1+J-1), J=1,10 )
            IF( INTEXT .NE. ' ' ) THEN
C
C              Got another intent so set the number for the scan.
C
               NSCINT(ISCN) = I
C
C              If the list is empty, add this as the first and
C              set the pointer to it.
C
               IF( NINTENT .EQ. 0 ) THEN
                  INTENT(1) = INTEXT
                  NINTENT = 1
                  ISCINT(I,ISCN) = 1
               ELSE
C
C                 Look for this intent in the list.  If there,
C                 add the index to the scan info.
C
                  DO K = 1, NINTENT
                     IF( INTEXT .EQ. INTENT(K) ) THEN
                        ISCINT(I,ISCN) = K
                        GO TO 100
                     END IF
                  END DO
  100             CONTINUE
C
C                 If the intent is new, add it to the list.
C                 Protect against too many.
C
                  IF( ISCINT(I,ISCN) .EQ. 0 ) THEN
                     NINTENT = NINTENT + 1
                     IF( NINTENT .GT. MINTENT ) THEN
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( 2A, I4, A )' )
     1                    'GINTENT: Too many distinct intents.',
     2                    ' Max ', MINTENT, 
     3                    ' Report need for more.'
                        CALL ERRLOG( MSGTXT )
                     END IF
                     INTENT(NINTENT) = INTEXT
C
C                    Set the scan pointer to the new one.
C
                     ISCINT(I,ISCN) = NINTENT
C
                  END IF
               END IF
C
C              Detect an intent of 'NONE', which means clear the intents.
C              Don't actually clear until we know if there are others.  We'll
C              want a warning if that happens.
C
               CAPTEXT = INTEXT
               CALL UPCASE( CAPTEXT )
               IF( CAPTEXT .EQ. 'NONE' ) GOTNONE = .TRUE.
            END IF
         END DO
C
C        Deal with a "NONE".
C
         IF( GOTNONE ) THEN
            IF( NSCINT(ISCN) .GE. 2 ) THEN
               CALL ERRLOG( 'GINTENT:  Please do not mix ''NONE'' ' //
     1             'with other INTENTs' )
            END IF
            ISCINT(1,ISCN) = 0
            NSCINT(ISCN) = 0
         END IF         
      ELSE
C
C        Nothing new.  Transfer the previous scan's values.
C        Note later as pointing scans and such are added, we
C        need to be careful.  When this is called, SCAN1 will
C        always be 1.
C
         IF( ISCN .GT. SCAN1 ) THEN
            NSCINT(ISCN) = NSCINT(ISCN-1) 
            DO I = 1, NSCINT(ISCN-1)
               ISCINT(I,ISCN) = ISCINT(I,ISCN-1)
            END DO
         END IF
      END IF
C
      RETURN
      END
