      SUBROUTINE V2DOUT
C
C     Routine to write a v2d template file.  That is the file used
C     to pass new or modified information to the correlator setup
C     to supplement the vex file.  It will also be a path for 
C     passing source lists for multiple field processing.
C
C     Much borrowed from OMSOUT
C
      INCLUDE           'sched.inc'
      INCLUDE           'schset.inc'
      INCLUDE           'vxlink.inc'
C
      LOGICAL           EXISTS
      INTEGER           LEN1, VLBOPE, IOERR, ICH, NCH, ISTA, ISET
      INTEGER           ISRC, KSRC, ICSRC
      INTEGER           IPAIR, CCHAN
      CHARACTER         V2DFILE*80, OPSTAT*4, OPTEXT*255
      INTEGER           JSRC
      CHARACTER         TRA*16, TDEC*16, TFORM*16, UPCODE*3
C
C----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'V2DOUT starting' )
C
C     Construct the name of the v2d file.
C     
      WRITE( V2DFILE, '(A,A)' )  EXPCODE(1:LEN1(EXPCODE)), '.TV2D'
      CALL DWCASE( V2DFILE )
      IF( DEBUG ) CALL WLOG( 0, 'V2DOUT: Opening ' //
     1      V2DFILE(1:LEN1(V2DFILE)) )
C     
C     Find out if the .v2d file already exists.
C     
      INQUIRE( FILE=V2DFILE, EXIST=EXISTS )
      IF( EXISTS .AND. OVERWRIT ) THEN
         OPSTAT = 'OLD'
      ELSE IF( EXISTS ) THEN
         WRITE( MSGTXT, '( A, A, A )' )  'V2DOUT: ', 
     1       V2DFILE(1:LEN1(V2DFILE)), ' already exists.'
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( 
     1      'V2DOUT: You need to delete old output files '//
     2      'or use OVERWRIT.')
      ELSE
         OPSTAT = 'NEW'
      END IF
C     
C     Announce your intentions.
C     
      CALL WLOG( 0, 'V2DOUT:  Writing V2D file ' //
     1     V2DFILE(1:LEN1(V2DFILE)) )
C     
C     Open V2D file.
C      
      IOERR = VLBOPE( IV2D, V2DFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' Open problem:'//OPTEXT )
C
C     What is this file?
C
      WRITE( IV2D, '( A, A )' )  
     1   '#  Template v2d file for DiFX correlation of ', 
     2   EXPCODE(1:LEN1(EXPCODE))
C
C     Fill in vex file name and antennas.
C
      WRITE( IV2D, '( 1X, /, A, A, A )' ) 'vex = ', 
     1     VEXFILE(1:LEN1(VEXFILE)), '.obs'
C
C     Give the station codes.  
C
      MSGTXT = ' '
      UPCODE = STCODE(STANUM(1))
      CALL UPCASE( UPCODE )
      WRITE( MSGTXT, '( A, A )' ) 'antennas = ', UPCODE
      NCH = LEN( MSGTXT )
      IF( NSTA .GE. 2 ) THEN
         DO ISTA = 2, NSTA
            ICH = LEN1( MSGTXT) 
            UPCODE = STCODE(STANUM(ISTA))
            CALL UPCASE( UPCODE )
            WRITE( MSGTXT(ICH+1:NCH), '( A, A )' ) ', ', 
     1           UPCODE
         END DO
      END IF
      ICH = LEN1( MSGTXT) 
      WRITE( IV2D, '( A )' ) MSGTXT(1:ICH)      
C
C     Make antenna stub blocks:
C
         DO ISTA = 1, NSTA
            ICH = LEN1( MSGTXT) 
            UPCODE = STCODE(STANUM(ISTA))
            CALL UPCASE( UPCODE )
            WRITE( IV2D, '( A, A, A )' ) 'ANTENNA ', 
     1           UPCODE(1:LEN1(UPCODE)), ' { }'
         END DO
C
C     Write channel setup.
C     Include some hints for anyone modifying the file.
C
      WRITE( IV2D, '( 1X, /, A, /, A, /, A, /, A, /, A )' )
     1   '# The nChan should never be less than 128.',
     2   '# For numbers of channels < 128, set specAvg so nChan/specAvg'
     3   ,'# gives the desired number of channels',
     4   'SETUP default', '{'
      WRITE( IV2D, '( A, F7.3 )' ) '  tInt =', CORAVG
C     Format change email of 6/6/2001 Deller - old:  
C     WRITE( IV2D, '( A, I6 )' ) '  nChan = ', CORFFT
      WRITE( IV2D, '( A, I6 )' ) '  nFFTChan = ', CORFFT
      IF( CORCHAN .GT. 0 ) THEN
         CCHAN = CORCHAN
      ELSE 
         CCHAN = 16
      END IF
C
C     Format change email of 6/6/2001 Deller - old:  
C      WRITE( IV2D, '( A, I4 )' ) '  specAvg =', CORFFT / CCHAN
      WRITE( IV2D, '( A, I4 )' ) '  nChan =', CCHAN
C
C      WRITE( IV2D, '( 2A )' )
C     1     'numBufferedFFTs = 10 #This is a DiFX parameter, ', 
C     2     'defaulting to 10 is good'
C
      IF( CORPOL ) THEN         
         WRITE( IV2D, '( 2A )' ) '  doPolar = True ',
     1      '# Full stokes'
      ELSE
         WRITE( IV2D, '( 2A )' ) '  doPolar = False ',
     1      '# No cross hand correlations'
      END IF
      WRITE( IV2D, '( A )' ) '}'
C
C     RULE needed.  I don't know much about it yet.
C
      WRITE( IV2D, '( 1X, /, A, /, A, /, A, /, A, /, A )' )
     1   '# This, along with SETUP default above, should always be done'
     2   ,'RULE default',
     3   '{',
     4   '  setup = default',
     5   '}'
C
C     SETUP placeholders.
C
      WRITE( IV2D, '( 1X, /, A )' ) '#  SETUP place holders (commented)'
      DO ISET = 1, NSETF
         WRITE( IV2D, '( A, A, A )' ) '# SETUP ',
     1        SETFILE(ISET)(1:LEN1(SETFILE(ISET))), ' {}'
      END DO
C
C     Write sources.  First those without extra phases centers, then
C     those that have offset phase centers.
C
C     Use the SRCNAME in the schedule source list to get the name 
C     actually used.  That is element KSRC.  That avoids searching 
C     through for the right alias.
C
      WRITE( IV2D, '( 1X, /, A )' )
     1  '# Sources (pointing centers) with recorded data but no ' //
     2  'offset pointing centers:'
      DO ISRC = 1, MSRC
         IF( USEDREC(ISRC) .AND. .NOT. USEDCENT(ISRC) ) THEN
            KSRC = SRLSTN(ISRC)
            IF( CALCODE(ISRC) .EQ. ' ' ) THEN
               WRITE( IV2D, '( 5A )' ) 'SOURCE ', 
     1            SRCNAME(KSRC)(1:LEN1(SRCNAME(KSRC))), ' { }'
            ELSE
               WRITE( IV2D, '( 5A )' ) 'SOURCE ', 
     1            SRCNAME(KSRC)(1:LEN1(SRCNAME(KSRC))), ' { calCode = ',
     2            CALCODE(ISRC)(1:LEN1(CALCODE(ISRC))), ' }'
            END IF
         END IF
      END DO
C
C     Now the multiple phase centers.
C
      IF( NPAIR .GE. 1 ) THEN
         WRITE( IV2D, '( 1X, /, A )' )
     1     '# Sources (pointing centers) with recorded data and ' //
     2     'offset pointing centers:'
C
C        Finally time to write the information source/centers data.
C
         DO IPAIR = 1, NPAIR
            KSRC = SRLSTN(PAIRSRC(IPAIR))
            WRITE( IV2D, '( 3A )' ) 'SOURCE ', 
     1         SRCNAME(KSRC)(1:LEN1(SRCNAME(KSRC))), ' {'
            IF( PAIRSRC(IPAIR) .EQ. CTRSRCI(1,PAIRCENT(IPAIR)) ) THEN
               WRITE( IV2D, '( A )' ) '  doPointingCenter = True'
            ELSE
               WRITE( IV2D, '( A )' ) '  doPointingCenter = False'
            END IF
            DO ICSRC = 1, NCSRC(PAIRCENT(IPAIR))
               JSRC = CTRSRCI(ICSRC,PAIRCENT(IPAIR))
               TRA =  TFORM( RA2000(JSRC), 'T', 0, 2, 10, '::@' )
               TDEC = TFORM( D2000(JSRC), ' ', 1, 2, 9, '::@' )
               IF( TDEC(1:1) .EQ. ' ' ) TDEC = TDEC(2:16)//' '
               WRITE( IV2D, '( 6A )' )
     1             '  addPhaseCentre = name@', 
     2             CTRSRCN(ICSRC,PAIRCENT(IPAIR))(1:LEN1(
     3                CTRSRCN(ICSRC,PAIRCENT(IPAIR)))),
     4             '/ra@', TRA, '/dec@', TDEC
            END DO
            WRITE( IV2D, '( A )' ) '}'
         END DO
      END IF
C
C
      RETURN
      END
