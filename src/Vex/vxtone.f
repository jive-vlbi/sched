      SUBROUTINE VXTONE
C
C     Subroutine sets up names, and values of tones needed for Vex
C     because the cross-referencing is complicated, not done on the fly
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 130596 
C
C     Allow this to run with FORMAT='NONE' if OBSTYP='PTVLBA' - single
C     dish VLBA observations.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER IPH, IFQ, IIF, I, J, ICH, NUP, NLO, ILPH, ILFQ, ILIF
      INTEGER IMODE
C From commented code: INTEGER  VXGTST, ISET
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'VXTONE: Starting' )
C
C     first find out which PH, there are at least as many FQ sections
C
      DO IMODE = 1, NMDVEX
C 
C     first check that it is not using FORMAT=NONE
C     RCW  Dec 2011.  Remove this check so all scans can be configured.
C     The stations that don't want FORMAT=NONE scans will not be included
C     in them in the $SCHED section.
C
C         ISET = VXGTST( IMODE )
C         IF( FORMAT(ISET)(1:4) .NE. 'NONE' .OR. 
C     1       OBSTYP .EQ. 'PTVLBA' ) THEN
C
C          in this mode find all IPH for which there are all antennas
C
           DO I = 1, NMODPH(IMODE)
              IPH = IMODPH(I, IMODE)
              DO ILPH= 1, NSTAPH(IPH,IMODE)               
C
C             now find same antennas in one of the FQ in the same MODE
C
                 DO J = 1, NMODFQ(IMODE)
                    IFQ = IMODFQ(J, IMODE)
                    DO ILFQ= 1, NSTAFQ(IFQ,IMODE)
C
C                      matching stations define links.
C                      Huibhier...
C
                       IF( ISTAFQ(ILFQ,IFQ,IMODE) .EQ. 
     1                      ISTAPH(ILPH,IPH,IMODE) ) THEN
                          IF( FQTOPH(IFQ) .EQ. 0) THEN
                             FQTOPH(IFQ) = IPH
                          ELSE
                             IF( FQTOPH(IFQ) .NE. IPH ) THEN
                               CALL ERRLOG(
     2                           'VXTONE: different PHASE_CAL scheme '//
     3                           'within FREQUENCY block')
C
C                               when different phase cal schemes for
C                               different telescopes need to be enabled,
C                               the simplest thing is to have vxcffq to
C                               look at the tone and detection schemes.
C
                             END IF
                          END IF
                       END IF
                    END DO
                 END DO
C
C             loop PH stations
C
              END DO
           END DO
C                     END IF
      END DO
C
C     first find out which IF, there are at least as many FQ sections
C
      DO IMODE = 1, NMDVEX
C 
C     first check that it is not using FORMAT=NONE, unless it is a 
C     VLBA pointing project.
C     RCW Dec 2011.  Remove the FORMAT=NONE check here too.
C
C         ISET = VXGTST( IMODE )
C         IF( FORMAT(ISET)(1:4) .NE. 'NONE' .OR. 
C     1         OBSTYP .EQ. 'PTVLBA' ) THEN
C
C          in this mode find all IIF for which there are all antennas
C
           DO I = 1, NMODIF(IMODE)
              IIF = IMODIF(I, IMODE)
              DO ILIF= 1, NSTAIF(IIF,IMODE)               
C
C             now find same antennas in one of the FQ in the same MODE
C
                 DO J = 1, NMODFQ(IMODE)
                    IFQ = IMODFQ(J, IMODE)
                    DO ILFQ= 1, NSTAFQ(IFQ,IMODE)
C
C                      matching stations defined links.
C
                       IF( ISTAFQ(ILFQ,IFQ,IMODE) .EQ. 
     1                      ISTAIF(ILIF,IIF,IMODE) ) THEN
                          IF( FQTOIF(IFQ) .EQ. 0) THEN
                             FQTOIF(IFQ) = IIF
                          ELSE
C
C                            one FQ can refer to many IF, let's assume
C                            tones are identical and register first
C
                             IF( FQTOIF(IFQ) .NE. IIF ) THEN
                                IF( TONEINT(IIF) .NE. 
     1                              TONEINT(FQTOIF(IFQ)) ) THEN
                                   CALL ERRLOG(
     1                                 'VXTONE: sorry different LO '//
     2                                 'MUST have same TONE insertion')
                                END IF
                             END IF
                          END IF
                       END IF
                    END DO
                 END DO
C
C                loop IF stations
C
              END DO
           END DO
C                 END IF
C
C        loop Modes
C
      END DO
C
C     go through all PH sections
C
      DO IPH = 1, NPHVEX
C
C        find all FQ's that match
C
         DO IFQ = 1, NFQVEX
            IF( IPH .EQ. FQTOPH(IFQ) ) THEN
C     
C              Do  all channels and setup tonedetections
C
               CALL VXTON2(-1, FQISSET(IFQ), NVXCHN(IFQ), 
     1             VXLOSUM(1,IFQ), VXNETSID(1,IFQ), VXBBFILT(1,IFQ), 
     2             TONEINT(FQTOIF(IFQ)), NTONES(IPH), TONCHN(1,IFQ),
     3             NTONDET(1,IPH), ITONDET(1,1,IPH) )
C
C              Name the tone links, we have matching IPH and IFQ
C     
               NUP = 0
               NLO = 0
               DO I = 1, NTONES(IPH)
                  IF( TONEINT(FQTOIF(IFQ)) .GT. 1e-15 ) THEN
                     DO ICH = 1, NVXCHN(IFQ)
                        IF( TONCHN(ICH,IFQ) .EQ. I ) THEN
                           IF( VXNETSID(ICH,IFQ) .EQ. 'U' ) THEN
                              NUP = NUP + 1
                              WRITE( TONLNK(I,IPH), 
     1                             '( A1, I1, A3 )' ) 
     2                             'U', NUP, 'Cal'
                           ELSE
                              NLO = NLO + 1
                              WRITE( TONLNK(I,IPH), 
     1                             '( A1, I1, A3 )' ) 
     2                             'L', NLO, 'Cal'
                           END IF
C
C                          found 1 that's enough, jump out
C
                           GOTO 100
                        END IF
                     END DO
 100                 CONTINUE
                  ELSE
                     TONLNK(I,IPH) = 'NoCal'
                  END IF
               END DO
C
C           close matching FQ
C
            END IF
         END DO
C
C        if it is simply 1 up and/or 1 lo
C
         IF( NUP .EQ. 1 .OR. NLO .EQ. 1 ) THEN
            DO ICH = 1, NTONES(IPH)
               TONLNK(ICH,IPH)(2:2) = '_'
            END DO
         END IF
C     
      END DO
C
      RETURN
      END


