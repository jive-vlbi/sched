      SUBROUTINE ACCSRC( INCLPTG )
Cf2py intent(in) INCLPTG
C
C     Subroutine for SCHED that accumulates a list of all sources
C     that have been used in the schedule.  It looks for the main
C     sources in scans, doppler sources, vla phasing sources, and
C     multiple phase center sources.
C
C     Optionally, it can look for sources that might get used for
C     reference pointing or for insertion of geodetic segments.  
C     These need to be counted when reading the catalogs so they 
C     won't get left out.  But after the schedule optimization and 
C     reference insertion is done, only the ones that made it to 
C     scans are needed, and they are counted anyway.
C
C     The name of any source found is put in SRCNAME and counted with
C     NSRC.  
C
C     This routine is run at various points in SCHED when the current 
C     list of sources is needed.  It is run by INPUT before the main 
C     source catalog is read so that the catalog reading routine 
C     can limit which sources it keeps (the catalog can be very large).
C     It is also run late in SCHOPT after the final scans list is known.
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
      INTEGER    ISCN, KSRC, PSRC, IGRP, JCENT, ICSRC, IGEO
      LOGICAL    GOTSS, GOTSD, GOTSP, GOTPTG, INCLPTG, GOTPHS, GOTGEO
      LOGICAL    GOTPCEN
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
C
C        If a centers has been specified, find its sources.
C
         IF( CENTERS(ISCN) .NE. ' ' ) THEN
            GOTPCEN = .FALSE.
            DO JCENT = 1, NCENT
               IF( CENTERS(ISCN) .EQ. CTRNAME(JCENT) ) THEN
                  GOTPCEN = .TRUE.
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
C
C           Complain if centers not found.
C
            IF( .NOT. GOTPCEN ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' )
     1            'ACCSRC: Could not find pcenters ', 
     2            CENTERS(ISCN)
               CALL ERRLOG( MSGTXT )
            END IF
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
C
C        Also look for sources that might be involved in insertion
C        of geodetic segments.  They are in GEOSRC.
C
         IF( ANYGEO ) THEN
            DO IGEO = 1, NGEO
               GOTGEO = .FALSE.
               IF( NSRC .GE. 1 ) THEN
                  DO KSRC = 1, NSRC 
                     IF( GEOSRC(IGEO) .EQ. SRCNAME(KSRC) ) 
     1                    GOTGEO = .TRUE.
                  END DO
               END IF
               IF( .NOT. GOTGEO ) THEN
                  IF( NSRC .EQ. MAXSRC ) GO TO 900
                  NSRC = NSRC + 1
                  SRCNAME(NSRC) = GEOSRC(IGEO)
               END IF
            END DO
         END IF
C
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

