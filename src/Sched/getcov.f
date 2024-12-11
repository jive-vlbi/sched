      SUBROUTINE GETCOV( VALUE, KC, KI )
C
C     This is a routine for SCHED that processes the input information
C     of the cover letter type and creates the output for including in
C     the output files.
C
      INCLUDE 'sched.inc'
C
      INTEGER            KI(*), KEYPTR, I, LEN1
      DOUBLE PRECISION   VALUE(*)
      CHARACTER          KC(*)*8, KCHAR*256
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GETCOV starting.' )
C
      SCHVER     = VALUE( KEYPTR( 'VERSION', KC, KI ) )
      PINAME     = KCHAR( 'PINAME',   64, .FALSE., VALUE, KC, KI )
      ADDRESS(1) = KCHAR( 'ADDRESS1', 64, .FALSE., VALUE, KC, KI )
      ADDRESS(2) = KCHAR( 'ADDRESS2', 64, .FALSE., VALUE, KC, KI )
      ADDRESS(3) = KCHAR( 'ADDRESS3', 64, .FALSE., VALUE, KC, KI )
      ADDRESS(4) = KCHAR( 'ADDRESS4', 64, .FALSE., VALUE, KC, KI )
      PHONE      = KCHAR( 'PHONE',    64, .FALSE., VALUE, KC, KI )
      EMAIL      = KCHAR( 'EMAIL',    64, .FALSE., VALUE, KC, KI )
      FAX        = KCHAR( 'FAX',      64, .FALSE., VALUE, KC, KI )
      OBSPHONE   = KCHAR( 'OBSPHONE', 49, .FALSE., VALUE, KC, KI )
      OBSMODE    = KCHAR( 'OBSMODE',  58, .FALSE., VALUE, KC, KI )
      NOTE(1)    = KCHAR( 'NOTE1',    128, .FALSE., VALUE, KC, KI )
      NOTE(2)    = KCHAR( 'NOTE2',    128, .FALSE., VALUE, KC, KI )
      NOTE(3)    = KCHAR( 'NOTE3',    128, .FALSE., VALUE, KC, KI )
      NOTE(4)    = KCHAR( 'NOTE4',    128, .FALSE., VALUE, KC, KI )
C
C     Make up the cover message for printouts.  Done with arrays to
C     help enclose in location specific comment indicators etc.
C
      WRITE( COVER(1), '( A, F10.2 )' ) 'Schedule Version: ', SCHVER
      WRITE( COVER(2), '( A, F6.2, 2X, A )' ) 
     1      'Processed by SCHED version: ', VERNUM, 
     2      VERSION(1:LEN1(VERSION))
      COVER(3)  = 'PI:       ' // PINAME
      COVER(4)  = 'Address:  ' // ADDRESS(1)
      COVER(5)  = '          ' // ADDRESS(2)
      COVER(6)  = '          ' // ADDRESS(3)
      COVER(7)  = '          ' // ADDRESS(4)
      COVER(8)  = 'Phone:    ' // PHONE
      COVER(9)  = 'EMAIL:    ' // EMAIL
      COVER(10) = 'Fax:      ' // FAX
      COVER(11) = 'Phone during observation: ' // OBSPHONE
      COVER(12) = 'Observing mode: ' // OBSMODE
      COVER(13) = 'Notes:    ' // NOTE(1)
      COVER(14) = '          ' // NOTE(2)
      COVER(15) = '          ' // NOTE(3)
      COVER(16) = '          ' // NOTE(4)
C
C     Warn if cover information is missing.
C
      MISSING = .FALSE. 
      IF( DEBUG ) CALL WLOG( 0, 'GETCOV: Checking cover information.' )
C
      IF( SCHVER .EQ. 0.D0 ) THEN
         CALL WLOG( 1, '     Schedule version is missing. ' )
         MISSING = .TRUE.
      END IF
C
      IF( PINAME .EQ. ' ' ) THEN
         CALL WLOG( 1, '     No PINAME given. ' )
         MISSING = .TRUE.
      END IF
C
      IF( ADDRESS(1) .EQ. ' ' ) THEN
         CALL WLOG( 1, '     No address specified.' )
         MISSING = .TRUE.
      END IF
C
      IF( PHONE .EQ. ' ' ) THEN
         CALL WLOG( 1, '     No PI phone number specified.' )
         MISSING = .TRUE.
      END IF
C
      IF( EMAIL .EQ. ' ' .AND. FAX .EQ. ' ' ) THEN
         CALL WLOG( 1, '     No email address or fax number specified.')
         MISSING = .TRUE.
      END IF
C
      IF( MISSING ) THEN
         CALL WLOG( 1, 'GETCOV:  Cover information incomplete or '//
     1              'missing.' )
         IF( .NOT. NOTAPE .AND. .NOT. PLOT ) CALL ERRLOG( 
     1         'GETCOV: Cover information '//
     2         'is required for VLBI observations.' )
         IF( PLOT ) THEN
            CALL WLOG( 1, 'GETCOV: Sched will plot, but not write ' //
     2         'telescope control files.' )
         END IF
      END IF
C
C     Trap "!" in any of the strings.  They can cause problems for the
C     VLBA on-line system.
C
      DO I = 1, MCOVER
         IF( INDEX( COVER(I), '!' ) .NE. 0 ) THEN
            CALL WLOG( 1, 'GETCOV: Please do not use exclamation ' //
     1             'marks in the cover information.' )
            CALL WLOG( 1, '        They mess up the parsing of the' //
     1             ' CRD files at the VLBA stations.' )
            CALL WLOG( 1, '        One was found in the line: ' )
            CALL WLOG( 1, COVER(I) )
            CALL ERRLOG( 'SCHIN: Remove exclamation marks.' )
         END IF
      END DO
C
      RETURN
      END
