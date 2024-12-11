      SUBROUTINE VXWRMD
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the MD = $MODE section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Dec. 2011  RCW:  Write modes even for FORMAT=NONE.  The VLBA 
C     needs them.  In cases that don't use the VLBA, they are unlikely 
C     to be scheduled and, if they are, they will be safely ignored, 
C     I hope, as no scans using they will actually be written in the
C     $SCHED block.
C
C     Allow station list lines to go beyond 132 characters for projects
C     with large numbers of stations.  Apparently the VEX parsers only
C     care about the length of the parsable tokens which is what is
C     between the colons.  A comment is one parsable token, so that 
C     has problems if over 128 characters, but the station lists 
C     created here can be long.  Limit the length for the ref lines
C     to 512 characters, but make LINE a bit longer so we don't have
C     to worry about overflowing it on the last addition.  If 512 is
C     not adequate, simply increase the length of LINE and recompile.
C     Apr. 27, 2012.  RCW.
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   IXX, I, ISTA, IMODE, LPOS
      INTEGER   LEN1, NTAPEST, ISET, MAXLINE
      CHARACTER LINE*520, LSETNAME*80, DEL*1
      INTEGER   VXGTST
C     NTAPEST is a temporary counter for the number of stations in a
C     given mode that use tapes.
C ----------------------------------------------------------------------
C
      LINE = ' ' 
      MAXLINE = LEN( LINE )
C
C     Loop through the SETUP groups and write the modes
C
      WRITE( IVEX, '( A, A1 )' ) '$MODE', SEP
C
C     First issue a warning for more than 20 modes...
C     This is a field system issue.  The VLBA should take more.
C
      IF( NMDVEX .GT. 20 ) THEN
         MSGTXT = ' ' 
         WRITE( MSGTXT, '( A, I3, A )' ) 
     1       '++++ VXWRMD: WARNING: More than 20 VEX modes (', NMDVEX,
     2       ') in this schedule. '
         CALL WLOG( 1,MSGTXT)
         MSGTXT = ' ' 
         WRITE( MSGTXT, '( 2A )' ) 
     1       '             ',
     2       'This VEX will NOT run in the field system!' 
         CALL WLOG( 1,MSGTXT)
         MSGTXT = ' ' 
         WRITE( MSGTXT, '( 2A )' )
     1       '             ',
     2       'It should be ok on the VLBA.' 
         CALL WLOG( 1,MSGTXT)
      END IF
C
      LSETNAME = ' '
      DO IMODE = 1, NMDVEX
         IXX = IMODE
C
C        Find a station that uses this Vex mode so we can find its Sched
C        setup to check the FORMAT.  Add a comment for format=none, but
C        go ahead and write the mode.  Some stations (eg VLBA) may need
C        the scans fully defined for activities like single dish tests and
C        reference pointing.
C
         ISET = VXGTST( IMODE )
         IF( FORMAT(ISET)(1:4) .EQ. 'NONE' ) THEN
            WRITE( IVEX, '( A1 )' ) COM
            WRITE( IVEX, '( A1, A )' ) COM, 
     1         'The following mode is used with FORMAT=NONE.'
         END IF
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1           MDLINK(IMODE)(1:LEN1(MDLINK(IMODE))), SEP
C
C        Procedure name are synchronous with IMODE
C
         WRITE( LINE, '( 5X, A, A, A1 )' )
     1       'ref $PROCEDURES = ',
     2       PRLINK(IXX)(1:LEN1(PRLINK(IXX))), SEP
         WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
C
C        Other references have been sorted out in vxsort.
C        Write here the freq section that belongs to this mode
C
         DO I = 1, NMODFQ(IMODE)
            IXX = IMODFQ(I,IMODE)
C
C           if there is just 1 FQ, no need to split Ants
C           but for 1.5 the complete list is still required:
C
C            IF( NMODFQ(IMODE) .EQ. 1 ) THEN
C
            IF( NMODFQ(IMODE) .EQ. -999 ) THEN
               WRITE( LINE, '( 5X, A, A  )' ) 
     1             'ref $FREQ = ',
     2             FQLINK(IXX)(1:LEN1(FQLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
               IF( NSTAFQ(IXX,IMODE) .NE. 0 ) THEN
                  WRITE( LINE, '( 5X, A, A )' ) 
     1                'ref $FREQ = ',
     2                FQLINK(IXX)(1:LEN1(FQLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTAFQ(IXX,IMODE)
                     DEL = COL
                     IF( ISTA .EQ. NSTAFQ(IXX,IMODE) ) DEL = SEP
                     WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                   STCODE(STANUM(ISTAFQ(ISTA,IXX,IMODE)))
C
C                    reduce the spaces
C
                     LPOS = LEN1(LINE)
                     WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
C
C                    Check for too long line.
C
                     IF( LPOS .GT. MAXLINE - 8 ) THEN
                        CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                    'for VEX MODE lines.  Change code?' )
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $FREQ = ',
     2                FQLINK(IXX)(1:LEN1(FQLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        write the IF links for this mode
C
         DO I = 1, NMODIF(IMODE)
            IXX = IMODIF(I,IMODE)
C
C           if there is just 1 IF, no need to split Ants
C
C           need all anyaway, set from 1 to -999
C
            IF( NMODIF(IMODE) .EQ. -999 ) THEN
               WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1             'ref $IF = ',
     2             IFLINK(IXX)(1:LEN1(IFLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
               IF( NSTAIF(IXX,IMODE) .NE. 0 ) THEN
                  WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1                'ref $IF = ',
     2                IFLINK(IXX)(1:LEN1(IFLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTAIF(IXX,IMODE)
                     DEL = COL
                     IF( ISTA .EQ. NSTAIF(IXX,IMODE)) DEL = SEP
                     WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                   STCODE(STANUM(ISTAIF(ISTA,IXX,IMODE)))
C
C                    reduce the spaces
C
                     LPOS = LEN1(LINE)
                     WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
C
C                    Check for too long line.
C
                     IF( LPOS .GT. MAXLINE - 8 ) THEN
                        CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                    'for VEX MODE lines.  Change code?' )
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $IF = ',
     2                IFLINK(IXX)(1:LEN1(IFLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        write the BB section for this mode
C
         DO I = 1, NMODBB(IMODE)
            IXX = IMODBB(I,IMODE)
C
C           if there is just 1 BB, no need to split Ants
C
C           need all anyaway, set from 1 to -999
C
            IF( NMODBB(IMODE).EQ.-999 ) THEN
               WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1             'ref $BBC = ',
     2             BBLINK(IXX)(1:LEN1(BBLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
               IF( NSTABB(IXX,IMODE) .NE. 0 ) THEN
                  WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1                'ref $BBC = ',
     2                BBLINK(IXX)(1:LEN1(BBLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTABB(IXX,IMODE)
                     DEL = COL
                     IF( ISTA .EQ. NSTABB(IXX,IMODE)) DEL = SEP
                     WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                   STCODE(STANUM(ISTABB(ISTA,IXX,IMODE)))
C
C                    reduce the spaces
C
                     LPOS = LEN1(LINE)
                     WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
C
C                    Check for too long line.
C
                     IF( LPOS .GT. MAXLINE - 8 ) THEN
                        CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                    'for VEX MODE lines.  Change code?' )
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $BBC = ',
     2                BBLINK(IXX)(1:LEN1(BBLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        write the Track section for this mode
C
         DO I = 1, NMODTR(IMODE)
            IXX = IMODTR(I,IMODE)
C
C           if there is just 1 TR, no need to split Ants
C
C           need all anyaway, set from 1 to -999
C
            IF( NMODTR(IMODE) .EQ. -999 ) THEN
               WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1             'ref $TRACKS = ',
     2             TRLINK(IXX)(1:LEN1(TRLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
               IF( NSTATR(IXX,IMODE) .NE. 0 ) THEN
                  WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1                'ref $TRACKS = ',
     2                TRLINK(IXX)(1:LEN1(TRLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTATR(IXX,IMODE)
                     DEL = COL
                     IF( ISTA .EQ. NSTATR(IXX,IMODE)) DEL = SEP
                     WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                   STCODE(STANUM(ISTATR(ISTA,IXX,IMODE)))
C
C                 reduce the spaces
C
                     LPOS = LEN1(LINE)
                     WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
C
C                    Check for too long line.
C
                     IF( LPOS .GT. MAXLINE - 8 ) THEN
                        CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                    'for VEX MODE lines.  Change code?' )
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $TRACKS = ',
     2                TRLINK(IXX)(1:LEN1(TRLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)                  
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        write the HP section for this mode
C
         DO I = 1, NMODHP(IMODE)
            IXX = IMODHP(I,IMODE)
C           Count the number of stations in this mode that are using
C           tapes. HEADPOS is irrelevant if there are none.
            NTAPEST = 0
            DO ISTA = 1, NSTAHP(IXX,IMODE)
               IF( USETAPE((ISTAHP(ISTA,IXX,IMODE))) .OR. 
     1           DISK(STANUM(ISTAHP(ISTA,IXX,IMODE)))
     2             .EQ. 'LBADR' ) THEN
                  NTAPEST = NTAPEST + 1
               END IF
            END DO
C
C           if there is just 1 HP, no need to split Ants
C
C           need all anyaway, set from 1 to -999
C
            IF( NMODHP(IMODE) .EQ. -999 ) THEN
               WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1             'ref $HEAD_POS = ',
     2             HPLINK(IXX)(1:LEN1(HPLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
               IF( NSTAHP(IXX,IMODE) .NE. 0 .AND. NTAPEST .GT. 0) THEN
                  WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1                'ref $HEAD_POS = ',
     2                HPLINK(IXX)(1:LEN1(HPLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTAHP(IXX,IMODE)
                     IF( USETAPE((ISTAHP(ISTA,IXX,IMODE))) .OR.
     1                   DISK((STANUM(ISTAHP(ISTA,IXX,IMODE)))) 
     2                   .EQ. 'LBADR' ) THEN
                        DEL = COL
                        IF( ISTA .EQ. NSTAHP(IXX,IMODE)) DEL = SEP
                        WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                      STCODE(STANUM(ISTAHP(ISTA,IXX,IMODE)))
C     
C                    reduce the spaces
C
                        LPOS = LEN1(LINE)
                        WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
C
C                    Check for too long line.
C
                        IF( LPOS .GT. MAXLINE - 8 ) THEN
                           CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                       'for VEX MODE lines.  Change code?' )
                     END IF
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $HEAD_POS = ',
     2                HPLINK(IXX)(1:LEN1(HPLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)                  
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        write the RL section for this mode
C
         DO I = 1, NMODRL(IMODE)
            IXX = IMODRL(I,IMODE)
C
C           if there is just 1 RL, no need to split Ants
C
C           need all anyaway, set from 1 to -999
C
            IF( NMODRL(IMODE) .EQ. -999 ) THEN
               WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1             'ref $ROLL = ',
     2             RLLINK(IXX)(1:LEN1(RLLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
               IF( NSTARL(IXX,IMODE) .NE. 0 ) THEN
                  WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1                'ref $ROLL = ',
     2                RLLINK(IXX)(1:LEN1(RLLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTARL(IXX,IMODE)
                     DEL = COL
                     IF( ISTA .EQ. NSTARL(IXX,IMODE)) DEL = SEP
                     WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                   STCODE(STANUM(ISTARL(ISTA,IXX,IMODE)))
C
C                 reduce the spaces
C
                     LPOS = LEN1(LINE)
                     WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
C
C                    Check for too long line.
C
                     IF( LPOS .GT. MAXLINE - 8 ) THEN
                        CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                    'for VEX MODE lines.  Change code?' )
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $ROLL = ',
     2                RLLINK(IXX)(1:LEN1(RLLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)                  
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        write the PO section for this mode
C
         DO I = 1, NMODPO(IMODE)
            IXX = IMODPO(I,IMODE)
C
C           Count the number of stations in this mode that are using
C           tapes. PASS_ORDER is irrelevant if there are none.
            NTAPEST = 0
            DO ISTA = 1, NSTAHP(IXX,IMODE)
               IF( USETAPE((ISTAHP(ISTA,IXX,IMODE))) .OR.
     1            DISK((STANUM(ISTAHP(ISTA,IXX,IMODE)))) 
     2                .EQ. 'LBADR' ) THEN
                  NTAPEST = NTAPEST + 1
               END IF
            END DO
C
C           if there is just 1 PO, no need to split Ants
C
C           need all anyaway, set from 1 to -999
C
            IF( NMODPO(IMODE) .EQ. -999 ) THEN
               WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1             'ref $PASS_ORDER = ',
     2             POLINK(IXX)(1:LEN1(POLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
              IF( NSTAPO(IXX,IMODE) .NE. 0 .AND. NTAPEST .GT. 0 ) THEN
                  WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1                'ref $PASS_ORDER = ',
     2                POLINK(IXX)(1:LEN1(POLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTAPO(IXX,IMODE)
                     IF( USETAPE((ISTAPO(ISTA,IXX,IMODE))) .OR.
     1                   DISK((STANUM(ISTAPO(ISTA,IXX,IMODE)))) 
     2                   .EQ. 'LBADR' ) THEN
                        DEL = COL
                        IF( ISTA .EQ. NSTAPO(IXX,IMODE)) DEL = SEP
                        WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                      STCODE(STANUM(ISTAPO(ISTA,IXX,IMODE)))
C
C                 reduce the spaces
C
                       LPOS = LEN1(LINE)
                       WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
                     END IF
C
C                    Check for too long line.
C
                     IF( LPOS .GT. MAXLINE - 8 ) THEN
                        CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                    'for VEX MODE lines.  Change code?' )
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $PASS_ORDER = ',
     2                POLINK(IXX)(1:LEN1(POLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)                  
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        write the PH section for this mode
C
         DO I = 1, NMODPH(IMODE)
            IXX = IMODPH(I,IMODE)
C
C           if there is just 1 PH, no need to split Ants
C
C           need all anyaway, set from 1 to -999
C
            IF( NMODPH(IMODE) .EQ. -999 ) THEN
               WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1             'ref $PHASE_CAL_DETECT = ',
     2             PHLINK(IXX)(1:LEN1(PHLINK(IXX)))
               LPOS = LEN1(LINE)
               WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) SEP
            ELSE
               IF( NSTAPH(IXX,IMODE) .NE. 0 ) THEN
                  WRITE( LINE, '( 5X, A, A, A1, A, A1 )' ) 
     1                'ref $PHASE_CAL_DETECT = ',
     2                PHLINK(IXX)(1:LEN1(PHLINK(IXX)))
                  LPOS = LEN1(LINE)
                  WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) COL
                  DO ISTA = 1, NSTAPH(IXX,IMODE)
                     DEL = COL
                     IF( ISTA .EQ. NSTAPH(IXX,IMODE)) DEL = SEP
                     WRITE( LINE(LPOS+2:LPOS+4), '( A )' )
     1                   STCODE(STANUM(ISTAPH(ISTA,IXX,IMODE)))
C
C                 reduce the spaces
C
                     LPOS = LEN1(LINE)
                     WRITE( LINE(LPOS+1:LPOS+1), '( A1 )' ) DEL
C
C                    Check for too long line.
C
                     IF( LPOS .GT. MAXLINE - 8 ) THEN
                        CALL ERRLOG( 'VXWRMD: Too many stations '//
     1                    'for VEX MODE lines.  Change code?' )
                     END IF
                  END DO
               ELSE
                  WRITE( LINE, '( A1, 4X, A, A, A )' ) 
     1                COM, 'ref $PHASE_CAL_DETECT = ',
     2                PHLINK(IXX)(1:LEN1(PHLINK(IXX))),
     3                ' <= obsolete definition'
                  LPOS = LEN1(LINE)                  
               END IF
            END IF
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        That's all, Folks!
C
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP  
      END DO
C
C     these were the SETUPS in mode section
C
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END





