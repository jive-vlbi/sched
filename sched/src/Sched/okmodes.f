      SUBROUTINE OKMODES( KS )
C
C     Subroutine for SCHED that checks that the requested mode is one
C     approved for use on the VLBA correlator.  As of March 99, all
C     possible modes are approved, so this shouldn't do anything.  But
C     the JIVE correlator is coming on-line and may need something
C     similar, so I'll leave the code available.
C
C     Also, prevent delay changes in mixed VLBA/MKIV modes.
C
C     Distinguish the modes with the setup file parameters BITS,
C     NCHAN, SAMPRATE, and SPEEDUP.  The allowed modes will be stored
C     in local arrays NCH, SAM, SPD, BIT, OKV, and OK4 (the OK's
C     being logicals indicating that the mode is ok for VLBA or
C     MarkIV.
C
C     Updates:
C       97Jun12 Original.  RCW
C       97Jun25 After FT001 tests.  RCW  (no alternate modes added).
C       97Sep19 After FT002.  (again no alternates for Mark IV).
C       99Feb26 After more tests.  99mar22 ditto.
C       03May13 Allow 256 Mbps/2 drive modes - I guess I overlooked 
C           this when I allowed 512 Mbps.
C       08Jan24 Allow the 2-8-32-1  512 mode on MKIV.  I don't know
C           why it was blocked.
C       08Oct18 Allow the other 512 modes on MKIV.
C
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER      KS, MMODES, I, M
      PARAMETER    (MMODES=100)
      INTEGER      LEN1 
      REAL         LSPEEDUP
      LOGICAL      OKMODE, MIXED, CHGSPD, MWARNED
      INTEGER      NCH(MMODES), BIT(MMODES)
      REAL         SAM(MMODES), SPD(MMODES)
      LOGICAL      OKV(MMODES), OK4(MMODES)
      CHARACTER    LFORMAT*4
C
      SAVE         LSPEEDUP, LFORMAT, MIXED, CHGSPD, MWARNED
      DATA         MWARNED / .FALSE. /
C
C     1 bit, 1 and 2 channels.
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=1,10)
     1       /  1,  1,  32.,  1.,  .TRUE.,  .TRUE.,
     2          1,  1,  16.,  2.,  .TRUE.,  .TRUE.,
     3          1,  1,   8.,  4.,  .TRUE.,  .TRUE.,
     4          1,  1,   4.,  4.,  .TRUE.,  .TRUE.,
     5          1,  1,   2.,  4.,  .TRUE.,  .TRUE.,
     5          1,  2,  32.,  1.,  .TRUE.,  .TRUE.,
     7          1,  2,  16.,  2.,  .TRUE.,  .TRUE.,
     8          1,  2,   8.,  4.,  .TRUE.,  .TRUE.,
     9          1,  2,   4.,  4.,  .TRUE.,  .TRUE.,
     A          1,  2,   2.,  4.,  .TRUE.,  .TRUE.  /
C
C     1 bit, 1 and 2 channels.  Alternate fan outs (speedups).
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=51,60)
     1       /  1,  1,  16.,  1.,  .TRUE.,  .TRUE.,
     2          1,  1,   8.,  1.,  .TRUE.,  .TRUE.,
     3          1,  1,   8.,  2.,  .TRUE.,  .TRUE.,
     4          1,  1,   4.,  2.,  .TRUE.,  .TRUE.,
     5          1,  2,  16.,  1.,  .TRUE.,  .TRUE.,
     6          1,  2,   8.,  1.,  .TRUE.,  .TRUE.,
     7          1,  2,   8.,  2.,  .TRUE.,  .TRUE.,
     8          1,  2,   4.,  2.,  .TRUE.,  .TRUE.,
     9          0,  0,   0.,  0.,  .FALSE., .FALSE., 
     A          0,  0,   0.,  0.,  .FALSE., .FALSE.   /
C
C     1 bit, 4 and 8 channels.
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=11,20)
     1       /  1,  4,  32.,  1.,  .TRUE.,  .TRUE.,
     2          1,  4,  16.,  2.,  .TRUE.,  .TRUE.,
     3          1,  4,   8.,  4.,  .TRUE.,  .TRUE.,
     4          1,  4,   4.,  4.,  .TRUE.,  .TRUE.,
     5          1,  4,   2.,  4.,  .TRUE.,  .TRUE.,
     5          1,  8,  32.,  1.,  .TRUE.,  .TRUE.,
     7          1,  8,  16.,  2.,  .TRUE.,  .TRUE.,
     8          1,  8,   8.,  4.,  .TRUE.,  .TRUE.,
     9          1,  8,   4.,  4.,  .TRUE.,  .TRUE.,
     A          1,  8,   2.,  4.,  .TRUE.,  .TRUE.  /
C
C     1 bit, 4 and 8 channels.  Alternate fan outs (speedups)
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=61,70)
     1       /  1,  4,  16.,  1.,  .TRUE.,  .TRUE.,
     2          1,  4,   8.,  1.,  .TRUE.,  .TRUE.,
     3          1,  4,   8.,  2.,  .TRUE.,  .TRUE.,
     4          1,  4,   4.,  2.,  .TRUE.,  .TRUE.,
     5          1,  8,  16.,  1.,  .TRUE.,  .TRUE.,
     6          1,  8,   8.,  1.,  .TRUE.,  .TRUE.,
     7          1,  8,   8.,  2.,  .TRUE.,  .TRUE.,
     8          1,  8,   4.,  2.,  .TRUE.,  .TRUE.,
     9          0,  0,   0.,  0.,  .FALSE., .FALSE., 
     A          0,  0,   0.,  0.,  .FALSE., .FALSE.   /
C
C     1 and 2 bit, 16 channels.  Note that these speed up factors
C     are per tape while those in OK_modes.vlba include the need for
C     double pass processing.
C     The one un-allowed mode is 1 Gbps.  The EVN may allow this soon.
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=21,30)
     1       / 1, 16,  32.,  1.,  .TRUE.,  .TRUE.,
     2         1, 16,  16.,  1.,  .TRUE.,  .TRUE.,
     3         1, 16,   8.,  2.,  .TRUE.,  .TRUE.,
     4         1, 16,   4.,  4.,  .TRUE.,  .TRUE.,
     5         1, 16,   2.,  4.,  .TRUE.,  .TRUE.,
     5         2, 16,  32.,  1.,  .FALSE., .FALSE.,
     7         2, 16,  16.,  1.,  .TRUE.,  .TRUE.,
     8         2, 16,   8.,  1.,  .TRUE.,  .TRUE.,
     9         2, 16,   4.,  2.,  .TRUE.,  .TRUE.,
     A         2, 16,   2.,  4.,  .TRUE.,  .TRUE.  /
C
C     1 and 2 bit, 16 channels.  Alternate fan outs (speedups).  Note 
C     that 16 channel, 2 bit with one tape requires all 32 tracks.
C     Some of these are two head (or two tape) modes.  There are also
C     some 256 Mbps, two tape (head) modes here for use during 
C     512 Mbps experiments.
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=71,80)
     1       /  1, 16,  16.,  2.,  .TRUE.,  .TRUE., 
     2          1, 16,   8.,  1.,  .TRUE.,  .TRUE.,
     3          1, 16,   4.,  2.,  .TRUE.,  .TRUE.,
     4          2, 16,   8.,  2.,  .TRUE.,  .TRUE.,
     5          0,  0,   0.,  0.,  .FALSE., .FALSE., 
     6          0,  0,   0.,  0.,  .FALSE., .FALSE.,
     7          0,  0,   0.,  0.,  .FALSE., .FALSE., 
     8          0,  0,   0.,  0.,  .FALSE., .FALSE.,
     9          0,  0,   0.,  0.,  .FALSE., .FALSE., 
     A          0,  0,   0.,  0.,  .FALSE., .FALSE.   /
C
C     2 bit, 1 and 2 channels.
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=31,40)
     1       /  2,  1,  32.,  1.,  .TRUE.,  .TRUE.,
     2          2,  1,  16.,  2.,  .TRUE.,  .TRUE.,
     3          2,  1,   8.,  4.,  .TRUE.,  .TRUE.,
     4          2,  1,   4.,  4.,  .TRUE.,  .TRUE.,
     5          2,  1,   2.,  4.,  .TRUE.,  .TRUE.,
     5          2,  2,  32.,  1.,  .TRUE.,  .TRUE.,
     7          2,  2,  16.,  2.,  .TRUE.,  .TRUE.,
     8          2,  2,   8.,  4.,  .TRUE.,  .TRUE.,
     9          2,  2,   4.,  4.,  .TRUE.,  .TRUE.,
     A          2,  2,   2.,  4.,  .TRUE.,  .TRUE.  /
C
C     2 bit, 1 and 2 channels.  Alternate fan outs (speedups)
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=81,90)
     1      /   2,  1,  16.,  1.,  .TRUE.,  .TRUE.,
     2          2,  1,   8.,  1.,  .TRUE.,  .TRUE.,
     3          2,  1,   8.,  2.,  .TRUE.,  .TRUE.,
     4          2,  1,   4.,  2.,  .TRUE.,  .TRUE.,
     5          2,  2,  16.,  1.,  .TRUE.,  .TRUE.,
     6          2,  2,   8.,  1.,  .TRUE.,  .TRUE.,
     7          2,  2,   8.,  2.,  .TRUE.,  .TRUE.,
     8          2,  2,   4.,  2.,  .TRUE.,  .TRUE.,
     9          0,  0,   0.,  0.,  .FALSE., .FALSE., 
     A          0,  0,   0.,  0.,  .FALSE., .FALSE.   /
C
C     2 bit, 4 and 8 channels.
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=41,50)
     1       /  2,  4,  32.,  1.,  .TRUE.,  .TRUE.,
     2          2,  4,  16.,  2.,  .TRUE.,  .TRUE.,
     3          2,  4,   8.,  4.,  .TRUE.,  .TRUE.,
     4          2,  4,   4.,  4.,  .TRUE.,  .TRUE.,
     5          2,  4,   2.,  4.,  .TRUE.,  .TRUE.,
     5          2,  8,  32.,  1.,  .TRUE.,  .TRUE.,
     7          2,  8,  16.,  1.,  .TRUE.,  .TRUE.,
     8          2,  8,   8.,  2.,  .TRUE.,  .TRUE.,
     9          2,  8,   4.,  4.,  .TRUE.,  .TRUE.,
     A          2,  8,   2.,  4.,  .TRUE.,  .TRUE.  /
C
C     2 bit, 4 and 8 channels.  Alternate fan outs (speedups).  A
C     256 Mbps, two tape (head) mode is included.
C
      DATA  (BIT(I), NCH(I), SAM(I), SPD(I), OKV(I), OK4(I), I=91,100)
     1      /   2,  4,  16.,  1.,  .TRUE.,  .TRUE.,
     2          2,  4,   8.,  1.,  .TRUE.,  .TRUE.,
     3          2,  4,   8.,  2.,  .TRUE.,  .TRUE.,
     4          2,  4,   4.,  2.,  .TRUE.,  .TRUE.,
     5          2,  8,  16.,  1.,  .TRUE.,  .TRUE.,
     6          2,  8,   8.,  1.,  .TRUE.,  .TRUE.,
     7          2,  8,   4.,  2.,  .TRUE.,  .TRUE.,
     8          2,  8,  16.,  2.,  .TRUE.,  .TRUE., 
     9          0,  0,   0.,  0.,  .FALSE., .FALSE., 
     A          0,  0,   0.,  0.,  .FALSE., .FALSE.   /
C
      DATA  LFORMAT, MIXED  / '    ', .FALSE. /
      DATA  LSPEEDUP, CHGSPD  / 0.0, .FALSE. /
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'OKMODES: Starting' )
C
C
C     Write a frightening message to those using MODETEST. I've
C     found it being abused.
C
      IF( MODETEST(KS) .AND. .NOT. MWARNED ) THEN
         CALL WRTMSG( 0, 'OKMODES', 'modtst' )
         MWARNED = .TRUE.
      END IF
C
C     Skip if not VLBA or MKIV format, if the project is not to
C     be correlated on the VLBA correlator, or if the override
C     was requested.
C
      IF( ( FORMAT(KS)(1:4) .EQ. 'VLBA' .OR. 
     1      FORMAT(KS)(1:4) .EQ. 'MKIV' ) .AND.
     2    ( CORREL(1:7) .EQ. 'SOCORRO' .OR. 
     3      CORREL(1:4) .EQ. 'VLBA' ) .AND.
     4      .NOT. MODETEST(KS) ) THEN
C
         OKMODE = .FALSE.
C
C        Identify the mode
C
         DO M = 1, MMODES
            IF( NCHAN(KS) .EQ. NCH(M) .AND.
     1          BITS(1,KS) .EQ. BIT(M) .AND.
     2          SAMPRATE(KS) .EQ. SAM(M) .AND.
     3          SPEEDUP(KS) .EQ. SPD(M) ) THEN
               IF( FORMAT(KS)(1:4) .EQ. 'VLBA' .AND.
     1             OKV(M) ) OKMODE = .TRUE.
               IF( FORMAT(KS)(1:4) .EQ. 'MKIV' .AND.
     1             OK4(M) ) OKMODE = .TRUE.
            END IF
         END DO
C
C        Now throw a fit if OKMODE is still false.
C
         IF( .NOT. OKMODE ) THEN
            CALL WLOG( 1, 'OKMODES: Untested recording mode requested '
     1          // 'with correlation in Socorro.' )
            WRITE( MSGTXT, '( 8X, A, A )' ) 'Format: ', FORMAT(KS)
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( 8X, A, I3 )' ) 'Number of channels: ',
     1             NCHAN(KS)
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( 8X, A, F6.1 )' ) 'Sample rate: ', 
     1             SAMPRATE(KS)
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( 8X, A, F7.2 )' ) 'Speedup factor: ', 
     1             SPEEDUP(KS)
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '        File: ' // 
     1                SETNAME(KS)(1:LEN1(SETNAME(KS))) )
            CALL WLOG( 1, '        Please use another mode.' )
            CALL WLOG( 1, '        Note: The modes restrictions are '
     1          // 'meant to be temporary.' )
            CALL WLOG( 1, '              SCHED mode defaulting does not'
     1          // ' know about them.' )
            CALL WLOG( 1, '              A simple change, like forcing'
     1          // ' a different fan out, may fix your problem.' )
C
C           Deal with a likely problem.
C
            IF( NCHAN(KS) .NE. 1 .AND. NCHAN(KS) .NE. 2 .AND.
     1          NCHAN(KS) .NE. 4 .AND.
     2          NCHAN(KS) .NE. 8 .AND. NCHAN(KS) .NE. 16 ) THEN
               CALL WLOG( 1, '        The number of channels is '//
     1                'not a power of 2.' )
               CALL WLOG( 1, '            This may be ok but the '//
     1                'mode is not in the okmodes table.' )
               CALL WLOG( 1, '            If you are sure it is ok, '//
     1                'and the number of channels is the only ' //
     2                ' problem, use MODETEST.' )
            END IF
            CALL WLOG( 1, '        Testers, see parameter MODETEST.' )
            CALL ERRLOG( 'OKMODES: Untested recording mode. ' )
         END IF
C
C        Don't allow a speedup change if Mark IV and VLBA are used.
C        This is because there are large clock jumps and the correlator
C        does not deal with it cleanly.
C
C        First detect if we have mixed formats in this experiment.
C
         IF( FORMAT(KS)(1:4) .NE. LFORMAT .AND. LFORMAT .NE. '    ' )
     1      MIXED = .TRUE.
         IF( SPEEDUP(KS) .NE. LSPEEDUP .AND. LSPEEDUP .NE. 0.0 ) 
     1      CHGSPD = .TRUE.
C
C        Detect if there was a tape speed (SPEEDUP) change.
C
         IF( MIXED .AND. CHGSPD ) THEN
            CALL WLOG( 1, 'OKMODES: This project uses VLBA and MarkIV '
     1          // 'formats and has a tape speed change.' )
            CALL WLOG( 1, '        This causes large delay jumps. ' )
            CALL WLOG( 1, '        Until this is sorted out at the '
     1         // 'correlator, it should be avoided.' )
            CALL WLOG( 1, '        Please choose setups with the same '
     1         // 'speedup factor.' )
            CALL ERRLOG( 'OKMODES:  Tape speed change with mixed '
     1         // 'format observations.' )
         END IF
C
C        Reset the memories.
C
         LFORMAT = FORMAT(KS)(1:4)
         LSPEEDUP = SPEEDUP(KS)
C
      END IF
C
      RETURN
      END
