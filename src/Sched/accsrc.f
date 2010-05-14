      SUBROUTINE ACCSRC( INCLPTG )
C
C     Subroutine for SCHED that accumulates a list of all sources
C     that have been used in the schedule.  It looks for the main
C     sources in scans, doppler sources, and vla phasing sources.
C
C     Optionally, it can look for sources that might get used for
C     reference pointing.  These need to be counted when reading
C     the catalogs so they won't get left out.  But after the schedule
C     optimization and reference insertion is done, only the ones
C     that made it to scans are needed, and they are counted anyway.
C
C     The name of any source found is put in SRCNAME and counted with
C     NSRC.  
C
C     This routine is run at various points in SCHED when the current 
C     list of sources is needed.  It is run before the main source 
C     catalog is read so that the catalog reading routine can limit 
C     which sources it keeps (the catalog can be very large).  It 
C     is also run late in SCHOPT after the final scans list is known.
C     Each time it is run, it rebuilds all the lists and pointers.
C
C     Eventually the array SRCATN will be used to associate these
C     sources with catalog entries.  That is filled by SRCFLG later.
C     It has to be done after all catalogs are read.
C
C     Do the types of source sequentially so a new one is not counted
C     multiple times.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schpeak.inc'
C
      INTEGER    ISCN, KSRC, PSRC, IGRP, JCENT, ICSRC
      LOGICAL    GOTSS, GOTSD, GOTSP, GOTPTG, INCLPTG, GOTPHS
C ---------------------------------------------------------------------
C     Initialize counters etc.  Recall that this routine is called
C     more than once.
C
      NSRC = 0
C
C     Loop over SCAN1 to SCANL as will be needed in the call from
C     SCHOPT.
C
      DO ISCN = SCAN1, SCANL
C
C        The main scan sources.
C
         GOTSS = .FALSE.
         IF( NSRC .GE. 1 ) THEN
            DO KSRC = 1, NSRC
               IF( SCNSRC(ISCN) .EQ. SRCNAME(KSRC) ) GOTSS = .TRUE.
            END DO
         END IF
         IF( .NOT. GOTSS ) THEN
            IF( NSRC .EQ. MAXSRC ) GO TO 900
            NSRC = NSRC + 1
            SRCNAME(NSRC) = SCNSRC(ISCN)
         END IF
C
C        The Doppler sources.
C
         GOTSD = .FALSE.
         IF( NSRC .GE. 1 ) THEN
            DO KSRC = 1, NSRC
               IF( DOPSRC(ISCN) .EQ. SRCNAME(KSRC) ) GOTSD = .TRUE.
            END DO
         END IF
         IF( .NOT. GOTSD ) THEN
            IF( NSRC .EQ. MAXSRC ) GO TO 900
            NSRC = NSRC + 1
            SRCNAME(NSRC) = DOPSRC(ISCN)
         END IF
C
C        The VLA phasing sources.
C
         GOTSP = .FALSE.
         IF( NSRC .GE. 1 ) THEN
            DO KSRC = 1, NSRC
               IF( VLAPHS(ISCN) .EQ. SRCNAME(KSRC) ) GOTSP = .TRUE.
            END DO
         END IF
         IF( .NOT. GOTSP ) THEN
            IF( NSRC .EQ. MAXSRC ) GO TO 900
            NSRC = NSRC + 1
            SRCNAME(NSRC) = VLAPHS(ISCN)
         END IF
C
C        The multiple phase center sources.
C
         ICENT(ISCN) = 0
         IF( CENTERS(ISCN) .NE. ' ' ) THEN
            DO JCENT = 1, NCENT
               IF( CENTERS(ISCN) .EQ. CTRNAME(JCENT) ) THEN
                  ICENT(ISCN) = JCENT
                  DO ICSRC = 1, NCSRC(JCENT)
                     GOTPHS = .FALSE.
                     IF( NSRC .GE. 1 ) THEN
                        DO KSRC = 1, NSRC
                           IF( CTRSRCN(ICSRC,JCENT) .EQ. 
     1                           SRCNAME(KSRC) ) THEN 
                              GOTPHS = .TRUE.
                           END IF
                        END DO                        
                     END IF
                     IF( .NOT. GOTPHS ) THEN
                        IF( NSRC .EQ. MAXSRC ) GO TO 900
                        NSRC = NSRC + 1
                        SRCNAME(NSRC) = CTRSRCN(ICSRC,JCENT)
                     END IF
                  END DO
               END IF
            END DO
         END IF

      END DO
C
C     The sources that might get used for reference pointing.
C     Loop over reference pointing groups and sources in the groups.
C
      IF( INCLPTG ) THEN
         IF( AUTOPEAK .AND. NPKGRP .GT. 0 ) THEN
            DO IGRP = 1, NPKGRP
               IF( NPKSRC(IGRP) .GT. 0 ) THEN
                  DO PSRC = 1, NPKSRC(IGRP)
                     GOTPTG = .FALSE.
                     IF( NSRC .GE. 1 ) THEN  
                        DO KSRC = 1, NSRC
                           IF( PKSRC(PSRC,IGRP) .EQ. SRCNAME(KSRC) ) 
     1                            THEN
                              GOTPTG = .TRUE.
                           END IF
                        END DO
                     END IF
                     IF( .NOT. GOTPTG ) THEN
                        IF( NSRC .EQ. MAXSRC ) GO TO 900
                        NSRC = NSRC + 1
                        SRCNAME(NSRC) = PKSRC(PSRC,IGRP)
                     END IF
                  END DO
               END IF
            END DO
         END IF
      END IF
C
      RETURN
C
C     Jump here if NSRC got too big.
C
  900 CONTINUE
      WRITE( MSGTXT, '( A, I6 )' ) 
     1     'ACCSRC: Too many sources needed by schedule.  Max', MAXSRC
      CALL ERRLOG( MSGTXT )
C
      RETURN
      END

