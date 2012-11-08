      SUBROUTINE VXWRDA
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the DA = $DAS section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Fix 1.5 version for DAS = none 070397
C
C     Removed much tape handling July 22, 2010  RCW.
C     Initialize HEAD for pointing type observations.  Nov. 11, 2011 RCW.
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
C     Huib's local variables 
C      
      INTEGER   IXX, ISTA, I, ISCAT, KS, IDRIV, JHEAD
      INTEGER   LEN1
      CHARACTER HEAD*10, WRDISK*6
      LOGICAL   VIOLFS
C ----------------------------------------------------------------------
C     write $DAS section 
C     
      VIOLFS = .FALSE.
      WRITE( IVEX, '( A, A1 )' ) '$DAS', SEP     
C
C     first, for recording system
C
      DO IXX = 1, NDAVEX
C
C        find a station to which this refers
C
         ISTA = -1
         DO I = 1, NSTA
            IF( ISTADA(I) .EQ. IXX ) ISTA = I
         END DO
         ISCAT = STANUM(ISTA)
C
         IF( ISTA .LT. 0 ) 
     1       CALL ERRLOG(' VXWRDA: no station for $DAS def ')
C
C        Prevent use of MkIII recorder and DAS with VEX in current version
C        proper implementation reuires work on chk3dar and a lot on
C        chk3dar, if PCFS will ever support this...
C
C        MKIII blocked at schedule read time so removed here.  July 22, 2010
C        RCW.
C
         ISCAT = STANUM(ISTA)
C
C        start with regular stuff
C
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' )  'def ', 
     1       DALINK(IXX)(1:LEN1(DALINK(IXX))),SEP
C
C        Start by finding a set in which this station occurs,
C        because some things need to know eg. FORMAT
C        RCW  Dec 2011.  Don't avoid FORMAT=NONE.
C
         DO I = 1, NSET
C            IF ( USED(I) .AND. ( FORMAT(I) .NE. 'NONE' .OR.
C     1          OBSTYP .EQ. 'PTVLBA' ) .AND.
C     1          ( STATION(ISCAT) .EQ. SETSTA(1,I) )) KS = I
            IF ( USED(I) .AND. STATION(ISCAT) .EQ. SETSTA(1,I) ) KS = I
         END DO
C
C        Now write recorder.
C
         IF( USEDISK(ISTA) ) THEN
             IF( DISK(STANUM(ISTA)) .EQ. 'MARK5A' .OR.
     1           DISK(STANUM(ISTA)) .EQ. 'MARK5B' .OR.
     2           DISK(STANUM(ISTA)) .EQ. 'MARK5C'    ) THEN
                WRDISK = 'Mark5'//DISK(STANUM(ISTA))(6:6)//' '
                WRITE( IVEX, '( 5X, A, A, A1 )' )
     1              'record_transport_type = ',
     2              WRDISK(1:LEN1(WRDISK)), SEP
             ELSE IF( DISK(STANUM(ISTA)) .EQ. 'LBADR' ) THEN
                WRITE( IVEX, '( 5X, A, A, A1 )' )
     1              'record_transport_type = ',
     2              'S2', SEP
C     2              'Mark5A', SEP
             ELSE
                CALL ERRLOG(' VXWRDA: Unknown recorder of type: '//
     1              RECORDER(ISCAT) )
             END IF
         END IF
C
C        Next electronic rack
C
         IF( DAR(ISCAT) .EQ. 'VLBA' .OR. 
     1       DAR(ISCAT) .EQ. 'RDBE' .OR. 
     2       DAR(ISCAT) .EQ. 'RDBE2' .OR. 
     3       DAR(ISCAT) .EQ. 'S2' .OR. 
     4       DAR(ISCAT) .EQ. 'VLBAG' .OR.
     5       DAR(ISCAT) .EQ. 'VLBA4' .OR. 
     6       DAR(ISCAT) .EQ. 'K4' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          DAR(ISCAT)(1:LEN1(DAR(ISCAT))), SEP
         ELSE IF( DAR(ISCAT) .EQ. 'MKIII' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'Mark3A', SEP
         ELSE IF( DAR(ISCAT) .EQ. 'MKIV' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'Mark4', SEP
         ELSE IF( DAR(ISCAT) .EQ. 'WIDAR' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'WIDAR', SEP
         ELSE IF( DAR(ISCAT) .EQ. 'LBA' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'LBA', SEP
         ELSE IF( DAR(ISCAT) .EQ. 'NONE' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'none', SEP
         ELSE IF( DAR(ISCAT) .EQ. 'DBBC' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1 )' ) 
     1          'electronics_rack_type = ',
     2          'DBBC', SEP
C
C    2011-09-30 small@astron:
C    Also add support for KVAZAR stations here.
C    (Note that "KVAZAR" is a back-transliteration 
C     of the Cyrillic transliteration of "quasar".
C     "KVAZAR" is apparently preferred at JIVE, although
C     the Russians write "QUASAR" in English.)
C
         ELSE IF( DAR(ISCAT) .EQ. 'R1002' ) THEN
            WRITE( IVEX, '( 5X, A, A, A1)' )
     1          'electronics_rack_type = ',
     2          'Mark4', SEP
         ELSE
            CALL ERRLOG(' VXWRDA: Unknown DAR of type: '//
     1          DAR(ISCAT) )
         END IF
C
C        S2 blocked on input so don't worry about it here.
C
C        Write number of drives.
C
         WRITE( IVEX, '( 5X, A, I1, A1 )' ) 'number_drives = ',
     1       STNDRIV(STANUM(ISTA)), SEP
C
C        Check if this fits the PCFS
C
         IF (NHEADS(STANUM(ISTA)) .GT. 2 ) THEN
            VIOLFS = .TRUE.
            WRITE( MSGTXT, '( A, A, A, I3, A )' ) 
     1          'VXWRDA: WARNING: More than 2 heads on ',
     2          STATION(STANUM(ISTA)), ' recorder (', 
     3          NHEADS(STANUM(ISTA)), ')'
            CALL WLOG( 1, MSGTXT )
         END IF            
C     
C        And the headstacks for each
C
         IF( USEDISK(ISTA) .OR. OBSTYP .EQ. 'NONE' .OR. 
     1       OBSTYP .EQ. 'PTVLBA' .OR. FORMAT(KS)(1:4) .EQ. 'NONE' ) 
     2       THEN
C           WRITE (HEAD, '( A )' ) ''
            HEAD = ' '
         END IF
         IF( STNDRIV(STANUM(ISTA)) .LT. 1 .OR. 
     1       STNDRIV(STANUM(ISTA)) .GT. 9 .OR.
     2       NHEADS(STANUM(ISTA)) .LT. 1 .OR.
     3       NHEADS(STANUM(ISTA)) .GT. 9 ) THEN
            CALL ERRLOG('VXWRDA inconsitent number of'//
     1          ' drives/headstacks')
         ELSE
            DO IDRIV = 1, STNDRIV(STANUM(ISTA)) 
               DO JHEAD = 1, NHEADS(STANUM(ISTA))
                  WRITE( IVEX, '( 5X, A, I1, 1X, A1, 1X, A, 
     1                1X, A1, 1X, I1, 1X, A1 )' ) 
     2                'headstack = ', 
     3                JHEAD + NHEADS(STANUM(ISTA))*(IDRIV-1), COL,
     4                HEAD, COL, IDRIV-1, SEP
               END DO
            END DO
         END IF
C
C        tapemotion can be start/stop or adaptive but fixed params!
C        Use this for disks as well, as 0 second gaps seem to cause a
C        problem - allow switching it off by using VEXTEST in case
C        this gets fixed
C
         IF( USETAPE(ISTA) .OR. .NOT. VEXTEST ) THEN
            WRITE( IVEX, '( 5X, A, A, A1, I2, A, A1, I2, A, A1, 
     1          I3, A, A1 )' ) 'tape_motion = ', 
     2          'adaptive ', COL, 0, ' min', COL,
     3          0, ' min', COL, 10, ' sec', SEP
         END IF
C
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
C
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
      IF( VIOLFS )
     1    CALL WLOG( 1,'VXWRDA: More than 2 headstacks '//
     2    'not supported in PCFS; this VEX will NOT run!!!!')
      RETURN
      END
