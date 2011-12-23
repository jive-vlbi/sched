      SUBROUTINE VXSORT( BLOCK, NXXVEX, XXISSET, NMODXX, IMODXX,
     1     NSTAXX, ISTAXX, XXLINK )
C
C     Routine for SCHED that goes through the setup groups and
C     finds out how many sections of type XX (e.g. FQ) etc there will be.
C     This is complicated, the code can be compacter, but than it is
C     really incomprehensible. Note that some more MODE, FREQ, PHASECAL
C     sections are to be discovered in VXSCNS.
C
C     Dec 2011.  Allow retention of FORMAT=NONE modes.  RCW.
C
C     Input:
C        BLOCK = Two letter code, denoting which $BLOCK we are sorting
C     Output:
C        NXXVEX = Number of defs in $BLOCK
C        XXISSET = List of sets each $BLOCK def refers to
C        NMODXX(IMODE) = Number of BLOCK defs refered in mode IMODE
C        IMODXX(I,IMODE) = List of BLOCK defs referred in mode IMODE
C        NSTAXX(IXX,IMODE) = Number of Station in $BLOCK IXX used by IMODE
C        ISTAXX(ISTA,IXX,IMODE) = List of stations in BLOCK IXX used in IMOD
C        XXLINK(IXX) = Name of link IXX.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      CHARACTER BLOCK*2, XXLINK(MAXSTA)*32
      INTEGER NXXVEX
      INTEGER NMODXX(MAXMOD), IMODXX(MAXMOD,MAXMOD)
      INTEGER XXISSET(MAXMOD), NSTAXX(MAXMOD,MAXMOD), 
     .    ISTAXX(MAXSTA,MAXMOD,MAXMOD)
      INTEGER VXGTST
C
C     local variables
C     
      INTEGER   I, J, KS, JS, ISTA, JSTA, ISCAT, IMODE, IXX, JXX
      INTEGER   SETISXX(MSET), LEN1
      LOGICAL NEWFND, VXCFFQ, VXCFIF, VXCFBB, VXCFTR, VXCFHP
      LOGICAL VXCFRL, VXCFPO, VXCFPH
      LOGICAL PROBLEM
C ----------------------------------------------------------------------
C
C     Safe to initialize counters
C
      DO IMODE = 1,MAXMOD
         DO IXX = 1,MAXMOD
            NSTAXX(IXX,IMODE)= 0
         ENDDO
      ENDDO
C
C     First find out how many $BLOCKS there are and keep one
C     of the sets that specifies it (many sets can use one $FREQ or $IF)
C
      WRITE( MSGTXT, '( A, A, A )' ) 
     1    ' VXSORT: Start sorting ', BLOCK, ' definition'
      IF( DEBUG ) CALL WLOG( 1, MSGTXT(1:LEN1(MSGTXT) ))
      NXXVEX = 0
      DO KS = 1, NSET
C
C        Obviously a new block when it's the first.
C        Keep regardless of FORMAT=NONE status.
C
C         IF( USED(KS) .AND. ( FORMAT(KS)(1:4) .NE. 'NONE' .OR.
C     1         OBSTYP .EQ. 'PTVLBA' ) ) THEN
         IF( USED(KS) ) THEN
            IF( NXXVEX .EQ. 0 ) THEN
               NEWFND = .TRUE.
            ELSE
C
C              Find if any set JS has same characteristics as KS = 1 to JS-1 
C
               NEWFND = .TRUE.
               DO IXX = 1, NXXVEX
                  JS = XXISSET(IXX)
                  IF( USED(JS) ) THEN 
C
C                    testing done in one of the functions, 
C                    store which block covers a certain set that is not unique
C
                     IF( BLOCK .EQ. 'FQ' ) THEN
                        IF( VXCFFQ( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE IF( BLOCK .EQ. 'IF' ) THEN
                        IF( VXCFIF( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE IF( BLOCK .EQ. 'BB' ) THEN
                        IF( VXCFBB( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE IF( BLOCK .EQ. 'TR' ) THEN
                        IF( VXCFTR( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE IF( BLOCK .EQ. 'HP' ) THEN
                        IF( VXCFHP( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE IF( BLOCK .EQ. 'PH' ) THEN
                        IF( VXCFPH( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE IF( BLOCK .EQ. 'PO' ) THEN
                        IF( VXCFPO( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE IF( BLOCK .EQ. 'RL' ) THEN
                        IF( VXCFRL( KS, JS ) ) THEN
                           NEWFND = .FALSE.
                           SETISXX(KS) = IXX
                        END IF
                     ELSE
                        CALL ERRLOG(' VXSORT: unsupported BLOCK def ')
                     END IF
                  END IF
               END DO
            END IF
C
C           If it is new, take action
C
            IF( NEWFND ) THEN
C
C           fill a new block and give it a name
C
               NXXVEX = NXXVEX + 1
               IF( NXXVEX .GT. MAXMOD ) CALL ERRLOG(
     1             'VXSORT: Number of $'//BLOCK//' defs '//
     1             'exceeding MAXMOD, need to re-compile...')
C
C              Store which set has pars for this block,
C
               XXISSET(NXXVEX) = KS
C
C              and which block covers this set (for local use only),
C
               SETISXX(KS) = NXXVEX
C
C              and think up a name (there are a few cases that these
C              won't actually be used, can't help that here).
C
               CALL VXNMXX( BLOCK, NXXVEX, XXLINK )
C
            END IF
         END IF
      END DO
C
C     now in a mode, there can be a number of e.g. FREQ settings
C     each for a group of antennas
C     first determine which defs are referenced where
C
      DO IMODE = 1, NMDVEX
         NMODXX(IMODE) = 0
C
C        Go through the antennas in a experiment
C
         DO ISTA = 1, NSTA
C
            KS = MODSET(ISTA,IMODE)
C
C           Note that, if the station is not used with the mode
C           (egvsop.key does that), then KS can end up zero, which
C           is trouble below.  Jump to next station if this happens.
C           RCW Nov 2011
C
            IF( KS .NE. 0 ) THEN
C               IF( USED(KS) .AND. ( FORMAT(KS)(1:4) .NE. 'NONE' .OR.
C     1            OBSTYP .EQ. 'PTVLBA' ) ) THEN
               IF( USED(KS) ) THEN
C
C              Obviously there is a new SET when there is no previous one
C
                  IF( MODSET(ISTA,IMODE) .NE. 0 .AND. 
     1                   NMODXX(IMODE) .EQ. 0 ) THEN
                     NEWFND = .TRUE.
                  ELSE
C 
C                    Find if a set already used for a previous antenna:
C
                     IF( MODSET(ISTA,IMODE) .EQ. 0 ) THEN
                        NEWFND = .FALSE.
                     ELSE
                        NEWFND = .TRUE.
                        DO J = 1, NMODXX(IMODE)
C
C                       Check if XX already used, note many modes 
C                       may use 1 XX
C
                           IF( SETISXX(MODSET(ISTA,IMODE)) .EQ. 
     1                         IMODXX(J,IMODE) )
     2                         NEWFND = .FALSE.
                        END DO
                     END IF
                  END IF
C
C                 Now take the appropriate action
C
                  IF( NEWFND ) THEN
C
C                    so a new block def is found for this mode, remember
C                    which block it is that is referenced in this mode
C
                     NMODXX(IMODE) = NMODXX(IMODE) + 1
                     IF( NMODXX(IMODE) .GT. MAXMOD ) CALL ERRLOG(
     1                 'VXSORT: Number of $'//BLOCK//' defs in $MODE '//
     2                 'exceeding MAXMOD, should never happen...')
                     IMODXX(NMODXX(IMODE),IMODE) = 
     1                  SETISXX(MODSET(ISTA,IMODE))
                  END IF
               END IF
            END IF
         END DO
      END DO
C     
C     Now we need to figure out which antennas are used in this 
C     list of blocks. Some defs (IXX) use different lists of antennas
C     in different modes, this is now implemented.
C
      DO IMODE = 1, NMDVEX
         DO I = 1, NMODXX(IMODE)
C
C           find each block def used in this mode, for short
C
            IXX = IMODXX(I,IMODE)
C
C           loop through stations and find their names
C
            DO ISTA = 1, NSTA
               ISCAT = STANUM(ISTA)
C
C              now KS = XXISET(IXX) has a description of the settings
C              for this block, but the station name can occur in many 
C              of the sets SETISXX(I) that use IXX (sorry it's tough).
C
               NEWFND = .FALSE.
C
C              first find an exact match:
C              should be possible to make clearer
C
               DO KS = 1, NSET
C                  IF( USED(KS) .AND. ( FORMAT(KS)(1:4) .NE. 'NONE' .OR.
C     1                OBSTYP .EQ. 'PTVLBA' ) ) THEN
                  IF( USED(KS) ) THEN
                     IF( SETISXX(KS) .EQ. IXX .AND. 
     1                   KS .EQ. MODSET(ISTA,IMODE)) THEN
                        IF( STATION(ISCAT) .EQ. SETSTA(1,KS) ) THEN
                           NEWFND = .TRUE.
                           IF( NSTAXX(IXX,IMODE) .GE. 1 ) THEN
                              DO J = 1, NSTAXX(IXX,IMODE)
                                 IF( ISTA .EQ. 
     1                               ISTAXX(J,IXX,IMODE) ) THEN
                                    NEWFND = .FALSE.
                                 END IF
                              END DO
                           END IF
                        ELSE IF( STATION(ISCAT)(1:3) .EQ. 'VLA' .AND.
     1                         SETSTA(1,KS)(1:3) .EQ. 'VLA' ) THEN
                           CALL WLOG( 1,' VXSORT: Warning: '//
     1                         'VLA names should match '//
     2                         'exactly for VEX extension ')
                        END IF                     
                     END IF
                  END IF
               END DO
C
C              so we have found a new station for this def
C
               IF( NEWFND ) THEN
                  NSTAXX(IXX,IMODE) = NSTAXX(IXX,IMODE) + 1
                  ISTAXX(NSTAXX(IXX,IMODE),IXX,IMODE) = ISTA
               END IF
            END DO
         END DO
      END DO
C
C     Test whether a complication with stations in different
C     defs used across modes occurs. Huib thinks this is solved
C     but lets keep the check.
C
      PROBLEM = .FALSE.
      IF( NMDVEX .GT. 1) THEN
         DO IMODE = 1, NMDVEX
           KS = VXGTST( IMODE ) 
           IF( USED(KS) .AND. ( FORMAT(KS)(1:4) .NE. 'NONE' .OR.
     1          OBSTYP .EQ. 'VLBA' ) ) THEN
             IF( NMODXX(IMODE) .GT. 1 ) THEN
                DO I = 2, NMODXX(IMODE)
                   IXX = IMODXX(I,IMODE)
                   DO ISTA = 1, NSTAXX(IXX,IMODE)
                      DO J = 1, I-1
                         JXX = IMODXX(J,IMODE)
                         DO JSTA = 1, NSTAXX(JXX,IMODE)
                            IF (ISTAXX(ISTA,IXX,IMODE) .EQ. 
     1                          ISTAXX(JSTA,JXX,IMODE)) THEN
                               PROBLEM = .TRUE.
                               WRITE (MSGTXT, '( A, A, A, A, A, A,
     .                             A, I2 )' ) 
     1                             'VXSORT: Problem with ', BLOCK, 
     2                             ' def, ref''d by ',
     3                             MDLINK(IMODE)(1:LEN1(MDLINK(IMODE))),
     4                             ' cannot resolve ', 
     5                          STATION(STANUM(ISTAXX(ISTA,IXX,IMODE))),
     6                             ' = ', ISTAXX(ISTA,IXX,IMODE)
                               CALL WLOG( 1, MSGTXT )
C
                               WRITE (MSGTXT, '( A, I2, A, I2, 
     .                             A, A, 
     .                             A, I2, A, I2, 
     .                             A, A, A )' ) 
     1                             'Occurs in  ',IXX,'-',IMODE,  
     2                             ' (',
     3                             XXLINK(IXX)(1:LEN1(XXLINK(IXX))),
     4                             ') and in ', JXX,'-',IMODE,
     4                             ' (',
     5                             XXLINK(JXX)(1:LEN1(XXLINK(JXX))),
     6                             ')'
                               CALL WLOG( 1, MSGTXT )
                            END IF
                         END DO
                      END DO
                   END DO
                END DO 
             END IF
           END IF
         END DO
      END IF
      IF( PROBLEM ) THEN
         WRITE (MSGTXT, '( A )' ) 
     1       'VXSORT: ERROR Schedule too '//
     2       'complicated to sort, requires a '//
     3       'Sched/VEX upgrade.'
         CALL ERRLOG( MSGTXT )
      END IF
C
C     Done...
C     
      RETURN
      END




