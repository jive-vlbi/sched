      SUBROUTINE SBHOURS( ISRC, TSCAN, TBASE, ESCAN, EBASE, 
     1                    ISET, KFSETS)
C
C     Routine for sched, called by srclst, that gets the number 
C     of hours in scans and in baselines for each source using a
C     setup file.  It gets it both for the core and for all scans
C     including the PREEMPT = EXTRA scans.
C
C     KFSETS is an ascii list of the frequency sets used with 
C     this source and setup.
C
C     Core scans are those with PREEMPT .NE. 'EXTRA'.  
C     TSCAN is the output scan hours for the core scans. 
C     TBASE is the baseline hours for the core scans.
C     ESCAN is the scan hours for all scans if DOSCANS is not
C           used.  Otherwise it is the hours for scans in 
C           the DOSCANS range.
C     EBASE is the baseline hours for the same scans as ESCAN.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     MFP
      PARAMETER   ( MFP = 26 )   !  1/3 dimension of KFSETS
      INTEGER     I, ISCN, ISTA, JSTA, ISRC, ISET, NC
      INTEGER     NFP, IFP, IFS, IFSETS(MFP)
      LOGICAL     GOTFS, SCNUSED
      DOUBLE PRECISION   TBASE, TSCAN, TBSTRT, ESCAN, EBASE
      CHARACTER   KFSETS*(*), IFMT*6
C -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'SBHOURS starting.' )
      TSCAN = 0.D0
      TBASE = 0.D0
      ESCAN = 0.D0
      EBASE = 0.D0
      NFP = 0
      DO I = 1, MFP
         IFSETS(I) = 0 
      END DO
C
C     Loop over scans.
C
      DO ISCN = SCAN1, SCANL
C
C        Select recording scans on the current source.
C        Note that pure pointing schedules tend to have NOREC set 
C        to false while recording schedules set NOREC true for 
C        scans like pointing scans (true as of July 2011).  I
C        might change that but I'm not sure that is not what I want.
C
C        Don't count scans that are skipped by all stations being 
C        processed.
C
         SCNUSED = .FALSE.
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) SCNUSED = .TRUE.
         END DO
C
         IF( SCNUSED .AND. SRCNUM(ISCN) .EQ. ISRC .AND. 
     1       ( .NOT. NOREC(ISCN) ) .AND.
     2       SETNUM(ISCN) .EQ. ISET ) THEN
C
C           Add the scan time.
C           The "E" variables are either all scans or DOSCANS scans
C           depending on if DOSCANS is in use.  The "T" variables are
C           for the "core" - scans with PREEMPT not set to EXTRA.
C
            IF( DOSCANS(1) .EQ. 0 ) THEN
               ESCAN = ESCAN + ( STOPJ(ISCN) - STARTJ(ISCN) )
            ELSE IF( ISCN .GE. DOSCANS(1) .AND. 
     1               ISCN .LE. DOSCANS(2) ) THEN
               ESCAN = ESCAN + ( STOPJ(ISCN) - STARTJ(ISCN) )
            END IF
            IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
               TSCAN = TSCAN + ( STOPJ(ISCN) - STARTJ(ISCN) )
            END IF
C
C           Get baseline time using double loop over stations.
C           Require source be up at both ends of scan.
C
            IF( NSTA .GT. 1 ) THEN
               DO ISTA = 1, NSTA -1
                  IF( STASCN(ISCN,ISTA) .AND. 
     1                UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2                UP2(ISCN,ISTA) .EQ. ' ' ) THEN
                     DO JSTA = ISTA + 1, NSTA
                        IF( STASCN(ISCN,JSTA) .AND.
     1                    UP1(ISCN,JSTA) .EQ. ' ' .AND.
     2                    UP2(ISCN,JSTA) .EQ. ' ' .AND.
     3                    TONSRC(ISCN,ISTA).LT.STOPJ(ISCN) .AND.
     4                    TONSRC(ISCN,JSTA).LT.STOPJ(ISCN)
     5                         ) THEN
C
C                          Get start time of data for the baseline.
C
                           TBSTRT = MAX( STARTJ(ISCN), 
     1                                  TONSRC(ISCN,ISTA) )
                           TBSTRT = MAX( TBSTRT,
     1                                  TONSRC(ISCN,JSTA) )
C
C                          Get baseline hours.
C
                           IF( DOSCANS(1) .EQ. 0 ) THEN
                              EBASE = EBASE + STOPJ(ISCN) - TBSTRT
                           ELSE IF( ISCN .GE. DOSCANS(1) .AND. 
     1                              ISCN .LE. DOSCANS(2) ) THEN
                              EBASE = EBASE + STOPJ(ISCN) - TBSTRT
                           END IF
                           IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                              TBASE = TBASE + STOPJ(ISCN) - TBSTRT
                           END IF
C
                        END IF
                     END DO
                  END IF
               END DO
            END IF                  
C
C           Now get the FSETS used.  Only require that the setup
C           is used at one station.  For many pointing observations,
C           there is only one station so the 2 station scheme gets
C           into trouble.
C
C           Don't try to distinguish core from all from DOSCANS 
C           for this.
C
            IF( NSTA .GE. 1 ) THEN
               DO ISTA = 1, NSTA
                  IF( STASCN(ISCN,ISTA) .AND. 
     1                UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2                UP2(ISCN,ISTA) .EQ. ' ' .AND. 
     3                TONSRC(ISCN,ISTA).LT.STOPJ(ISCN) ) THEN
C
C                          Get the FSETS used - check both
C                          stations.  Use FSETI(ISCN,ISTA).
C                          This is a bit tedious, but simple.
C
                     IF( NFP .EQ. 0 ) THEN
                        IFSETS(1) = FSETI(ISCN,ISTA)
                        NFP = 1
                     END IF                          
C
C                    Check ISTA
C
                     GOTFS = .FALSE.
                     DO IFS = 1, NFP
                        IF( FSETI(ISCN,ISTA) .EQ. IFSETS(IFS) )
     1                     GOTFS = .TRUE. 
                     END DO
                     IF( .NOT. GOTFS .AND. NFP .LT. MFP ) THEN
                        NFP = NFP + 1
                        IFSETS(NFP) = FSETI(ISCN,ISTA)
                     END IF
                  END IF
               END DO
            END IF                  
         END IF
      END DO
C
C     Write the character string KFSETS based on the frequency sets.
C     Only write the ones that don't have a matching one of smaller 
C     number as determined elsewhere and encoded in FSSAME.
C     But need to include the smaller number one somewhere - it might
C     not be in IFSETS.  Minimize the number of blanks.
C
      I = 1
      NC = 1
      KFSETS = ' '
      DO IFS = 1, NFSET
         DO IFP = 1, NFP
            IF( FSSAME(IFSETS(IFP)) .EQ. IFS ) THEN
               IF( IFS .GE. 10 ) NC = 2
               IF( IFS .GE. 100 ) NC = 3
               IF( IFS .GE. 1000 ) NC = 4
               WRITE( IFMT, '( A, I1, A )' ) '( I', NC+1, ' )'
               WRITE( KFSETS(I:I+NC), IFMT ) IFS
               I = I + NC + 1
               GO TO 300
            END IF
         END DO
  300    CONTINUE
      END DO
C
      RETURN
      END
