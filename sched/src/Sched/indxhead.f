      SUBROUTINE INDXHEAD( TAPEMD, IPASS, SYSTEM, INDX, HEAD, SETN )
C
C     Routine for SCHED called by TPSCH and perhaps others that
C     returns the head index position and head group (which N
C     to use for TRACKN) for a given TAPEMD (passes per head
C     position) and IPASS (pass number).  The hook is there for
C     more than one scheme, but only VLBA14 is currently 
C     supported.  The setup name SETN is passed for error messages.
C
C     Data statements are used instead of clever integer arithmetic
C     for clarity.
C
C     For tpmode=n:
C       INDXn(i) is the head index position for the i'th pass.
C       IHEADn(i) is the head group (TRACKn in the setup files) for
C          pass i.
C
      INTEGER     TAPEMD, IPASS, INDX, HEAD, LEN1, NC
      CHARACTER   SYSTEM*(*), SETN*(*), MSG*132
      LOGICAL     GOTERR
C
C     Data for SYSTEM=VLBA14
C
      INTEGER  N1, N2, N4, N8
      PARAMETER (N1=14)
      PARAMETER (N2=28)
      PARAMETER (N4=56)
      PARAMETER (N8=112)
      INTEGER  INDX1(14), IHEAD1(14)
      INTEGER  INDX2(28), IHEAD2(28)
      INTEGER  INDX4(56), IHEAD4(56)
      INTEGER  INDX8(112),IHEAD8(112)
      SAVE INDX1, IHEAD1, INDX2, IHEAD2, INDX4, IHEAD4, INDX8, IHEAD8
C
      DATA  INDX1  / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14 /
      DATA  IHEAD1 / 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /
C
      DATA  INDX2  / 1, 2, 1, 2, 3, 4, 3, 4, 5, 6, 5, 6, 7, 8, 7, 8,
     1               9,10, 9,10,11,12,11,12,13,14,13,14 /
      DATA  IHEAD2 / 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2,
     1               1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2 /
C
      DATA  INDX4  / 1, 2, 1, 2, 1, 2, 1, 2, 3, 4, 3, 4, 3, 4, 3, 4,
     1               5, 6, 5, 6, 5, 6, 5, 6, 7, 8, 7, 8, 7, 8, 7, 8,
     2               9,10, 9,10, 9,10, 9,10,11,12,11,12,11,12,11,12,
     3              13,14,13,14,13,14,13,14 /
      DATA  IHEAD4 / 1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4,
     1               1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4,
     2               1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4,
     3               1, 1, 2, 2, 3, 3, 4, 4 /
C
      DATA  INDX8  / 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 
     1               3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4,
     2               5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 5, 6, 
     3               7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8,
     4               9,10, 9,10, 9,10, 9,10, 9,10, 9,10, 9,10, 9,10,
     5              11,12,11,12,11,12,11,12,11,12,11,12,11,12,11,12,
     6              13,14,13,14,13,14,13,14,13,14,13,14,13,14,13,14 /
      DATA  IHEAD8 / 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
     1               1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
     2               1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
     3               1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
     4               1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
     5               1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
     6               1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8 /
C
C  --------------------------------------------------------------------
C     No includes, no DEBUG.
C
      GOTERR = .FALSE.
C
      IF( SYSTEM .EQ. 'VLBA14' ) THEN
C
C        Get the heads index position and group for the scan.
C
         IF( TAPEMD .EQ. 1 ) THEN
            IF( IPASS .GE. 1 .AND. IPASS .LE. N1 ) THEN
               INDX = INDX1( IPASS )
               HEAD = IHEAD1( IPASS )
            ELSE
               GOTERR = .TRUE.
            END IF
         ELSE IF( TAPEMD .EQ. 2 ) THEN
            IF( IPASS .GE. 1 .AND. IPASS .LE. N2 ) THEN
               INDX = INDX2( IPASS )
               HEAD = IHEAD2( IPASS )
            ELSE
               GOTERR = .TRUE.
            END IF
         ELSE IF( TAPEMD .EQ. 4 ) THEN
            IF( IPASS .GE. 1 .AND. IPASS .LE. N4 ) THEN
               INDX = INDX4( IPASS )
               HEAD = IHEAD4( IPASS )
            ELSE
               GOTERR = .TRUE.
            END IF
         ELSE IF( TAPEMD .EQ. 8 ) THEN
            IF( IPASS .GE. 1 .AND. IPASS .LE. N8 ) THEN
               INDX = INDX8( IPASS )
               HEAD = IHEAD8( IPASS )
            ELSE
               GOTERR = .TRUE.
            END IF
         ELSE
C
C           Bad TAPEMD
C
            NC = MIN( LEN1( SETN ), 70 )
            WRITE( MSG, '( I6, A, A )' ) TAPEMD, ' in ', SETN(1:NC)
            CALL ERRLOG( 'INDXHEAD: Bad tpmode: '//MSG )
         END IF
C
      ELSE IF( SYSTEM .EQ. 'MKIV2H' ) THEN
C
C        Get the heads index position and group for the scan.
C
         IF( TAPEMD .EQ. 1 ) THEN
            IF( IPASS .GE. 1 .AND. IPASS .LE. N1 ) THEN
               INDX = INDX1( IPASS )
               HEAD = IHEAD1( IPASS )
            ELSE
               GOTERR = .TRUE.
            END IF
C
         ELSE
            WRITE( MSG, '( A, A )' )
     1         'INDXHEAD: Mark IV two head mode used with more than ',
     2         'one pass per head position (TAPEMD>1).'
            CALL WLOG( 1, MSG )
            CALL ERRLOG( ' Adjust tape modes' )
         END IF
C
      ELSE
C
C        Bad system.
C
         MSG = SYSTEM // ' in ' // SETN
         CALL ERRLOG( 'INDXHEAD: Unknown head position system: '//
     1         MSG(1:LEN1(SYSTEM)) )
      END IF
C
C     Pass/mode combination bad.
C
      IF( GOTERR ) THEN
         WRITE( MSG, '(A, I6, A, I6, 2X, 3A )' ) 
     1         'Bad combination - Mode ', TAPEMD, ' Pass ', IPASS,
     2          SYSTEM, ' in ', SETN
         CALL ERRLOG( 'INDXHEAD: '//MSG )
      END IF
C
      RETURN
      END




