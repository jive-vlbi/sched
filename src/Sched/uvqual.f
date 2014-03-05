      SUBROUTINE UVQUAL( IC, FLAG, QUALITY, TOP5, INFOLINE )
C
C     Routine for the UV coverage optimization that measures the
C     quality of an array.
C
C     IC is a loop index over combinations.
C
C     FLAG tells which stations to use.  If 0, don't.  If 1, will
C     use in all configurations (maybe set up to calcualate once),
C     If 2, use in this array.
C
C     QUALITY is any quality measure where high is good.
C
C     In this incarnation, the number of sampled cells will be
C     counted.  The cells are on a polar, logarithmic grid.
C     Many parameters will be hard wired for now, but maybe 
C     change that later.
C
C     Note that calculations are done for both ends of scans, so
C     it is probably best to have DUR = GAP in the schedule.
C
      INCLUDE       'sched.inc'
      INCLUDE       'plot.inc'
C
      INTEGER       IC, FLAG(*)
      REAL          QUALITY
      REAL          QUALSRC(MAXSRC), TOP5(5,*), BUFFER
      INTEGER       ISTA, JSTA, ISRC, ISCN, IE, C1, CN, I
      INTEGER       LINELEN
      REAL          U1(MAXSTA), U2(MAXSTA), V1(MAXSTA), V2(MAXSTA)
      REAL          UU(2), VV(2)
      LOGICAL       USIT(MAXSTA)
      CHARACTER     INFOLINE*(*)
      REAL          MFS1, MFSINC
C
C     Set up the grid.  Use an even number of theta bins or the
C     fast hermitian scheme won't work (need symmetric).
C
      INTEGER       NR, NT, MNR, MNT, MSR
      PARAMETER     (MNR=100)
      PARAMETER     (MNT=180)
      PARAMETER     (MSR=10)
      REAL          LFMIN, LFMAX, LFINC, RCELL
      REAL          TRINC, RCELL2, FBW
      REAL          GRSUM, GRSUM2, AVG, RMS, VX
      INTEGER       CELLACC(MNR,MNT), IR, IT, W02, IB
      INTEGER       CELLALL(MNR,MNT,MSR), NSR
      LOGICAL       FIRST, NEWBAS, VLAPASS
C --------------------------------------------------------------------
      IF( GRIDMEAS .NE. 'RMS' .AND. GRIDMEAS .NE. 'COUNT' ) THEN
         MSGTXT = 'UVQUAL: Incorrect GRIDMEAS - ' // GRIDMEAS 
         CALL ERRLOG( MSGTXT )
      END IF
      FIRST = IC .EQ. 1
C
C     Hard wire in whether to limit to VLA baselines.
C
      IF( GRIDVLA ) THEN
         IF( FIRST ) WRITE( IOPT, * ) 'UVQUAL: Only using VLA baselines'
         IF( MOD(IC,100) .EQ. 0 ) THEN 
            CALL WLOG( 1, 'UVQUAL: Only using VLA baselines' )
         END IF
      END IF
C
C     Get parameters for testing of multiple lines for MFS.
C     MFSRAT is the ratio of the upper to the lower frequency.
C     NMFS is the number of frequencies to use.  Both are from the user.
C
      IF( NMFS .GT. 1 ) THEN
         MFS1 = 2.0 / ( 1.0 + MFSRAT )
         MFSINC = MFS1 * ( MFSRAT - 1.0 ) / ( NMFS - 1 )
      ELSE
         MFS1 = 1.0
         MFSINC = 0.0
      END IF
C
C     Get some parameters from the includes into the short name
C     versions to help once we are deep in nested DO's and IF's
C     and have to live with the limited line length.
C
      NSR = 0
      NR = GRIDNR
      NT = GRIDNT
      IF( NR .GT. MNR .OR. NT .GT. MNT ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, 4I7 )' ) 
     1       'UVQUAL: GRIDNR or GRIDNT too big: ', GRIDNR,
     2       GRIDNT, MNR, MNT
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( 'UVQUAL:  Decrease inputs or up limits.' )
      END IF
C
C     Loop over sources, only keeping those selected for plotting.
C
      W02 = GRIDW0**2
      LFMIN = LOG( GRIDMIN + SQRT( GRIDMIN**2 + W02 ) )
      LFMAX = LOG( GRIDMAX + SQRT( GRIDMAX**2 + W02 ) )
      LFINC = ( LFMAX - LFMIN ) / NR
      TRINC = 360.0 * RADDEG / NT
      QUALITY = 0.0
      DO ISRC = 1, NSRC
         IF( PSOBCK(ISRC) .EQ. 1 ) THEN
            NSR = NSR + 1
C
C           Get the factor to stretch the grid in V.
C           This is rather experimental at the moment.
C
            VX = 0.9 / COS( 34.0 * RADDEG - DECP(SRCATN(ISRC)) )
C
C           Initialize the accumulator
C
            DO IT = 1, NT
               DO IR = 1, NR
                  CELLACC(IR,IT) = 0
                  IF( FIRST ) CELLALL(IR,IT,NSR) = 0
               END DO
            END DO
C
C           Loop over scans keeping only those on the target source.
C
            DO ISCN = SCAN1, SCANL
               IF( SRLSTN(SRCNUM(ISCN)) .EQ. ISRC ) THEN
C
C                 Loop over stations to determine if they are used
C                 and to collect the station UV values.
C      
                  DO ISTA = 1, NSTA
                     USIT(ISTA) = STASCN(ISCN,ISTA) .AND. 
     1                   UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2                   UP2(ISCN,ISTA) .EQ. ' ' .AND.
     3                   FLAG(ISTA) .GT. 0
                     IF( USIT(ISTA) ) THEN
                        CALL STAUV( ISCN, ISTA, U1(ISTA), U2(ISTA), 
     1                              V1(ISTA), V2(ISTA) ) 
                     END IF
                  END DO
C
C                 Now get the UV values
C
                  DO ISTA = 1, NSTA - 1
                     IF( USIT(ISTA) ) THEN
                        DO JSTA = ISTA + 1, NSTA
C
C                          Some restrictions.  Flag baselines that
C                          might change so we can avoid repeating those
C                          that don't.  Also have option to restrict
C                          to baselines to the VLA.
C
                           NEWBAS = FLAG(ISTA) .EQ. 2 .OR. 
     1                          FLAG(JSTA) .EQ. 2 
                           VLAPASS = .NOT. GRIDVLA .OR.
     1                          ( STANAME(ISTA)(1:3) .EQ. 'VLA' .OR.
     1                          STANAME(ISTA)(1:3) .EQ. 'VLA' )
C
                           IF( USIT(JSTA) .AND. ( FIRST .OR. 
     1                          NEWBAS ) .AND. VLAPASS ) THEN
C
C                             Get the baseline U and V, with V
C                             scaled to take into account 
C                             foreshortening.  I think this is a
C                             faster way to do it than changing the
C                             grid.
C
                              UU(1) = U1(ISTA) - U1(JSTA)
                              VV(1) = VX * ( V1(ISTA) - V1(JSTA) )
                              UU(2) = U2(ISTA) - U2(JSTA)
                              VV(2) = VX * ( V2(ISTA) - V2(JSTA) )
C                   
C                             Do some bandwidth synthesis as specified
C                             by the user with UVMFS
C
                              DO IB = 1, NMFS
                                 FBW = MFS1 + ( IB - 1 ) * MFSINC
C
C                                Increment the cell counts.
C                                Don't forget the conjugate points.
C                                If R is out of range, don't count.
C                           
                                 DO IE = 1, 2
                                    RCELL2 = ( FBW * UU(IE) )**2 + 
     1                                       ( FBW * VV(IE) )**2
                                    RCELL = ( LOG( SQRT( RCELL2 ) +
     1                                   SQRT( RCELL2 + W02 ) )
     2                                   - LFMIN ) / LFINC
                                    IR = RCELL + 1.0
                                    IF( IR .GE. 1 .AND. 
     1                                  IR .LE. NR ) THEN
                                       IT = 1 + ATAN2(VV(IE),UU(IE)) / 
     1                                      TRINC
                                       IF( IT .LT. 1 ) 
     1                                     IT = IT + NT
                                       IF( IT .GT. NT ) THEN
                                           MSGTXT = ' '
                                           WRITE( MSGTXT, '( A, 2I5 )' )
     1                                        'UVQUAL bad NT ', NT, IT
                                           CALL WLOG( 1, MSGTXT )
                                       END IF
                                       IF( NEWBAS ) THEN
                                          CELLACC(IR,IT) = 
     1                                      CELLACC(IR,IT) + 1
                                       ELSE
                                          CELLALL(IR,IT,NSR) = 
     1                                      CELLALL(IR,IT,NSR) + 1
                                       END IF
C                           
C                                      Get the hermitian point.
C                           
                                       IT = IT + NT / 2
                                       IF( IT .GT. NT ) IT = IT - NT
                                       IF( NEWBAS ) THEN
                                          CELLACC(IR,IT) = 
     1                                      CELLACC(IR,IT) + 1
                                       ELSE
                                          CELLALL(IR,IT,NSR) = 
     1                                      CELLALL(IR,IT,NSR) + 1
                                       END IF
                                    END IF
                                 END DO
C                           
C                                This UV point accumulated.
C                   
                              END DO
                           END IF
                        END DO
                     END IF
                  END DO
C
C                 All stations done.
C
               END IF
            END DO
C
C           All scans done.
C
            IF( GRIDMEAS .EQ. 'COUNT' ) THEN
C
C              Get the quality measure for this source using count 
C              of filled cells.
C
               QUALSRC(ISRC) = 0.0
               DO IT = 1, NT
                  DO IR = 1, NR
                     IF( CELLACC(IR,IT) + CELLALL(IR,IT,NSR) .GE. 1 )
     1                   THEN
                        QUALSRC(ISRC) = QUALSRC(ISRC) + 1.0
                     END IF
                  END DO
               END DO
               QUALSRC(ISRC) = QUALSRC(ISRC) / ( NR * NT )
               QUALITY = QUALITY + QUALSRC(ISRC)
C
            ELSE IF( GRIDMEAS .EQ. 'RMS' ) THEN
C
C              Try an rms quality measure.  Note the rest of the
C              program is set up to take the highest value as
C              the best while we want the smallest RMS.  So invert.
C
               QUALSRC(ISRC) = 0.0
               GRSUM = 0.0
               GRSUM2 = 0.0
               DO IT = 1, NT
                  DO IR = 1, NR
                     GRSUM = GRSUM + CELLACC(IR,IT) + CELLALL(IR,IT,NSR)
                     GRSUM2 = GRSUM2 + 
     1                   ( CELLACC(IR,IT) + CELLALL(IR,IT,NSR) )**2
                  END DO
               END DO
               AVG = GRSUM / ( NR * NT )
               RMS = SQRT( ( GRSUM2 / ( NR * NT ) ) - AVG**2 )
               QUALSRC(ISRC) = SQRT( AVG ) / RMS
               QUALITY = QUALITY + QUALSRC(ISRC)
            ELSE
               CALL WLOG( 1, 'UVQUAL: Bad quality measure spec.' //
     1              '  Programming problem.' )
               STOP
            END IF
         END IF
      END DO
C
C     All sources done.
C
C     Write a line of information for the best arrays.
C
      LINELEN = LEN( INFOLINE )
      INFOLINE = ' '
      WRITE( INFOLINE, '( I7, F7.4 )' ) IC, QUALITY
      CN = 14
      DO ISRC = 1, NSRC
         IF( PSOBCK(ISRC) .EQ. 1 ) THEN
            C1 = CN + 1
            CN = C1 + 6
            WRITE( INFOLINE(C1:CN), '( F7.4 )' ) QUALSRC(ISRC)
         END IF
      END DO
      DO ISTA = 1, NSTA
         IF( FLAG(ISTA) .GE. 2 ) THEN
            C1 = CN + 2
            CN = C1 + 7
            IF( CN .LE. LINELEN ) THEN
               WRITE( INFOLINE(C1:CN), '( A )' ) STANAME(ISTA)
            END IF
         END IF
      END DO      
C
C     Get the current best values for each source
C
      DO ISRC = 1, NSRC
         IF( PSOBCK(ISRC) .EQ. 1 .AND. QUALSRC(ISRC) .NE. 0.0 ) THEN
            IF( QUALSRC(ISRC) .GT. TOP5(5,ISRC) ) 
     1           TOP5(5,ISRC) = QUALSRC(ISRC)
            DO I = 4, 1, -1
               IF( TOP5(I+1,ISRC) .GT. TOP5(I,ISRC) ) THEN
                  BUFFER = TOP5(I+1,ISRC)
                  TOP5(I+1,ISRC) = TOP5(I,ISRC)
                  TOP5(I,ISRC) = BUFFER
               END IF
            END DO
         END IF
      END DO
C
C     Use the MAXSRC cell of TOP5 to store similar info
C     for the overall quality factor.
C
      ISRC = MAXSRC
      IF( QUALITY .GT. TOP5(5,ISRC) ) 
     1     TOP5(5,ISRC) = QUALITY
      DO I = 4, 1, -1
         IF( TOP5(I+1,ISRC) .GT. TOP5(I,ISRC) .AND. 
     1         QUALITY .NE. 0 ) THEN
            BUFFER = TOP5(I+1,ISRC)
            TOP5(I+1,ISRC) = TOP5(I,ISRC)
            TOP5(I,ISRC) = BUFFER
         END IF
      END DO
C
      RETURN
      END
