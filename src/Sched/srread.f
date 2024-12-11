      SUBROUTINE SRREAD( ILIN, FILENAME, OPENIT, SELECT, THISCAT )
C
C     Subroutine for SCHED that reads source catalog.
C
C     So far, there are three times this routine might be called:
C       1.  Sources given in the main file (small).
C       2.  The main source catalog (huge).
C       3.  The pointing source catalog (small).
C     For 2 and 3, the file will need to be opened (OPENIT)
C     For 2, we wish to keep only those sources that are used (SELECT).
C            For 1 and 3, we don't know which will be used yet.
C
C     Input:
C
      INTEGER       ILIN    ! Catalog input unit number.
      CHARACTER*(*) FILENAME ! Catalog file name.
      LOGICAL       OPENIT  ! Open catalog on first read, close on last.
      LOGICAL       SELECT  ! Only take sources used in the schedule. 
      CHARACTER*1   THISCAT ! Code for output source lists that 
C                           ! indicates which catalog they were in.
C                           ! Put in variable WHICHCAT.
C
C     Include files for SCHED and for catalog reading.
C
      INCLUDE 'sched.inc'
      INCLUDE 'rdcat.inc'
C
      INTEGER        ISTAT, RDSRC, KSRC, LEN1, IV, ERR
      INTEGER        INAME, I1, I2, ICH
      REAL           CONT
      LOGICAL        GOTALL, SRUSED, EQWARN
      LOGICAL        READIT, KEEPIT, EPEQ
      CHARACTER      FILEUP*4, INFILE*80, CEQUINOX*5
      SAVE           CONT, EQWARN
C
C     Temporary stuff for looking at low error data.
C
C      integer        nlim
C      real           elim
C      data           nlim / 0 /
C
      DATA           EQWARN  / .TRUE. /
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SRREAD starting' )
      CALL KPACK( 'CONT', CONT )
C
C     Source flags etc were initialized at the start of SCHIN.
C
C     All sources used explicitly in the schedule should have been 
C     put in SRCNAME by ACCSRC, called by INPUTS.  Later SRCATN will 
C     contain the association between these sources and the catalog 
C     entries.
C
C
C     Determine if all sources are already available.
C     Note that SRCFLG does more than needed, but gets what we
C     need here.  Note SRCFLG sets SRCATN.  SRCFLG will be called
C     again so not all sources need be known at this point.
C
      IF( SELECT ) THEN
         IF( MSRC .GT. 0 ) THEN
            CALL SRCFLG( GOTALL )
         ELSE
            GOTALL = .FALSE.
         END IF
      ELSE
         GOTALL = .FALSE.
      END IF
C
C     Now read the catalog information.  Don't bother if all sources 
C     have been found already in previous calls or a catalog name of
C     'NONE' was specified.
C
C     Even if all sources have been found, read the catalog regardless 
C     if plotting is requested so that the calibrators will be 
C     available.
C
C     Determine whether to read data.
C
      IF( OPENIT ) THEN
         INFILE = FILENAME
      ELSE
         INFILE = 'Program_input'
      END IF
      FILEUP = INFILE
      CALL UPCASE( FILEUP )
      READIT = ( .NOT. OPENIT ) .OR. ( .NOT. GOTALL ) .OR. PLOT
      IF( OPENIT .AND. FILEUP .EQ. 'NONE' .AND. 
     1    LEN1(FILEUP) .EQ. 4 )  READIT = .FALSE. 
C
C     Read data if required.
C
      IF( READIT ) THEN 
C
C        Write message to screen.  Don't overflow.
C
         I2 = LEN1( INFILE )
         IF( I2 .GT. 100 - 24 ) THEN
            I1 = I2 - ( 100 - 24 ) + 1
         ELSE
            I1 = 1
         END IF
         MSGTXT = 'SRREAD:  Reading source catalog:  ' // INFILE(I1:I2)
         CALL WLOG( 0, MSGTXT )
C
C        Source reading loop begins.
C
  100    CONTINUE
C
C           Read next source.  Keep reading until EOF even if have
C           all sources so that RDSRC will close the file.  Otherwise
C           RESTART starts in middle.
C
            ISTAT = RDSRC( ILIN, OPENIT, INFILE )
            IF( ISTAT .NE. 0 ) THEN 
               GO TO 999
            END IF
C
C           Get the version of an external catalog.
C
            IF( OPENIT .AND. THISCAT .EQ. '1' ) THEN
               SRVER = SRCVER
            ELSE IF( OPENIT .AND. THISCAT .EQ. '3' ) THEN
               SRVER2 = SRCVER
            END IF
C
C           Require a source name.  Have them always in upper
C           case.  Case diversity makes matches hard later.
C           Don't allow "*" - the vex parser doesn't like it.
C           This is an issue for SGRA*, M81* etc.
C           
            DO INAME = 1, MALIAS
               CALL UPCASE( SRCNAM(INAME) )
C
               IF( INDEX( SRCNAM(INAME), ';' ) .NE. 0  .OR.
     1             INDEX( SRCNAM(INAME), ':' ) .NE. 0  .OR.
     2             INDEX( SRCNAM(INAME), '=' ) .NE. 0  .OR.
     3             INDEX( SRCNAM(INAME), '&' ) .NE. 0  .OR.
     4             INDEX( SRCNAM(INAME), '*' ) .NE. 0  .OR.
     5             INDEX( SRCNAM(INAME), '$' ) .NE. 0  .OR.
     5             INDEX( SRCNAM(INAME)(1:LEN1(SRCNAM(INAME))), ' ' ) 
     6                .NE. 0  .OR.
     7             INDEX( SRCNAM(INAME), '"' ) .NE. 0 ) THEN
                  CALL WLOG( 1, 
     1               'SRREAD: The source name '//SRCNAM(INAME)//
     2               ' contains an illegal character for Vex.' )
                  CALL WLOG( 1, 
     1               '        The source is in catalog: '// 
     2               INFILE )
                  CALL WLOG( 1, 
     1               '        The illegal characters are '//
     2               '(tab) (new line) ; : = & * $ " and (space).' )
                  CALL ERRLOG ( 'Change the source name and rerun. ' )
               END IF
C
            END DO
C
C           Temporary hook to look for the sources with low errors.
C
C            elim = 0.4
C            if( ( srcrae .le. elim .and. srcdece .le. elim ) .and.
C     1          ( srcrae .gt. 0.0 .and. srcdece .gt. 0.0 ) ) then
C               nlim = nlim + 1
C               write(*,'( A, 2F7.3, 10A )' ) 
C     1              srcnam(1), srcrae, srcdece, ' ', 
C     2              srceq(1:1), '  ', srcnam(2), '  ', srcnam(3),  '  ', 
C     3              srcnam(4), '  ', srcnam(5)
C            end if
C           
            IF( SRCNAM(1) .NE. 'NONAME' ) THEN
C           
C              If this is an in-line catalog, include the source in
C              the internal source list regardless.
C              If this is an external catalog, only include the 
C              source if it appears in the schedule.
C              Do not include a source from the external catalog that
C              was already in an in-line catalog.
C           
               SRUSED = .FALSE.
               KEEPIT = .FALSE.
               IF( SELECT ) THEN
C           
C                 Look for this source among the schedule sources
C                 that have not already been found.  Complain later
C                 if there are too many.
C           
                  DO KSRC = 1, NSRC
                     DO INAME = 1, MALIAS
                        IF( SRCATN(KSRC) .EQ. 0 .AND. 
     1                      SRCNAM(INAME) .EQ. SRCNAME(KSRC) ) THEN
C           
                           KEEPIT = .TRUE.
                           SRUSED = .TRUE.
                        END IF
                     END DO
                  END DO
               ELSE
                  KEEPIT = .TRUE.
               END IF
C
C              Unscramble the EPOCH and EQUINOX.  This is 
C              complicated by the fact that EPOCH used to
C              mean equinox and EQUINOX didn't exist.  I don't
C              want to break old schedules.  Abort if someone
C              is mixing both inputs meaning EQUINOX since I
C              can't tell which to use.  
C
C              This is done before the KEEPIT IF block so that 
C              sources acquired by SRLIST will be happy.
C              
C              Read EPOCH to test if it is one of the standard values
C              for EQUINOX.
C
               WRITE( CEQUINOX, '( A5 )' ) SRCEPOT
               CALL UPCASE( CEQUINOX )
               EPEQ = CEQUINOX .EQ. 'J2000' .OR. 
     1                CEQUINOX .EQ. 'B1950' .OR.
     2                CEQUINOX .EQ. 'DATE'
C
               IF( SRCEQ .NE. 'J2000' .AND. SRCEQ .NE. 'B1950' .AND.
     1             SRCEQ .NE. 'DATE' )  THEN
C              
C                 EQUINOX was not properly set.  
C                 See if they used EPOCH.
C              
                  IF( EPEQ ) THEN
C              
C                    EPOCH did make a good equinox so use it.
C                    and set the EPOCH to 0.D0 which will not be 
C                    accepted for proper motion work later.
C              
                     IF( EQWARN ) THEN
                        CALL WLOG( 1, 'SRREAD:  Please convert '
     1                  // 'source catalog from use of EPOCH to '
     2                  // 'EQUINOX.' )
                        EQWARN = .FALSE.
                     END IF
                     SRCEQ = CEQUINOX
                     SRCEPOT = 0.D0
C
                  ELSE
C              
C                    EPOCH was not a good alternate for EQUINOX.
C              
                     CALL WLOG( 1, 'SRREAD:  Source catalog ' //
     1                   'contains invalid equinox: '// SRCEQ )
                     CALL WLOG( 1, '         EPOCH also could ' //
     1                   'not be interpreted as a valid equinox.' )
                     CALL ERRLOG( ' EQUINOX must be J2000, ' //
     1                   'B1950, or DATE ' )
                  END IF
               ELSE
                  IF( EPEQ ) THEN
C
C                    Abort if both equinox and epoch used.
C
                     CALL WRTMSG( 0, 'SRREAD', 'eqep' )
                     CALL ERRLOG( 'SRREAD: See sched.runlog. ' //
     1                   'Convert source lists to use EQUINOX.' )
                  ELSE
C              
C                    Get the epoch date when equinox was used properly.
C              
                     IF( ABS( SRCEPOT ) .NE. 0.0 .AND. 
     1                   ( SRCEPOT .LT. 1900.D0 .OR. 
     2                     SRCEPOT .GT. 2100.D0 ) ) THEN
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( A, F15.8, 2A )' )
     1                       'SRREAD: *****   Very odd EPOCH: ', 
     2                       SRCEPOT, ' for source ', SRCNAM(1)
                        CALL WLOG( 1, MSGTXT )
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( 3A )' )
     1                       '                Interpreted as ',
     2                       'characters, it is: ', CEQUINOX
                        CALL WLOG( 1, MSGTXT )
                        CALL WLOG( 1, 
     1                    '                Please read sched.runlog.' )
                        CALL WRTMSG( 0, 'SRREAD', 'goofyepoch' )
                     END IF
                  END IF
               END IF
C           
C              This source is to be kept (in-line or a used one from
C              an external catalog).
C           
               IF( KEEPIT ) THEN
C           
                  MSRC = MSRC + 1
                  IF( MSRC .GT. MAXSRC ) THEN
                     WRITE( MSGTXT, '( A, I6 )' )
     1                   'Too many sources for SCHED.  Max =', 
     2                   MAXSRC
                     CALL ERRLOG( MSGTXT )
                  END IF
C           
C                 Put source in internal catalog.  Make sure two
C                 parameters that need to be the same are the same.
C           
                  IF( MAL .NE. MALIAS ) CALL ERROR( 
     1                 'SRREAD Programming error:  MAL NE MALIAS' )
                  DO INAME = 1, MALIAS
                     SOURCE(INAME,MSRC) = SRCNAM(INAME)
                  END DO
C
C                 Check if the default RA or DEC is being used.  This
C                 is not necessarily a problem, especially for 
C                 configuration testing, but might indicate that there
C                 was a problem reading the source list.  As you might
C                 guess, this test is the result of a request from a 
C                 user who got burned.
C
                  IF( SRCRA .EQ. 0.D0 .OR. 
     1                SRCDEC .LE. -1.57079632D0 ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 3A )' ) 
     1                  'SRREAD: WARNING: Default RA or Dec used for ',
     2                   SRCNAM(1), '.  Was that intended?'
                     CALL WLOG( 1, MSGTXT )
                  END IF
C           
C                 Flag where it came from.
C           
                  WHICHCAT(MSRC) = THISCAT
C           
C                 Get position information.  We won't get this far if
C                 the equinox is not one of the options below.
C           
                  C1950(MSRC) = ' '
                  C2000(MSRC) = ' '
                  CDATE(MSRC) = ' '
                  IF( SRCEQ .EQ. 'B1950' ) THEN
                     RA1950(MSRC) = SRCRA
                     D1950(MSRC)  = SRCDEC
                     C1950(MSRC)  = '*'
                  ELSE IF( SRCEQ .EQ. 'J2000' ) THEN
                     RA2000(MSRC) = SRCRA
                     D2000(MSRC)  = SRCDEC
                     C2000(MSRC)  = '*'
                  ELSE IF( SRCEQ .EQ. 'DATE') THEN
                     RAP(MSRC)    = SRCRA
                     DECP(MSRC)   = SRCDEC
                     CDATE(MSRC)  = '*'
                  ELSE
                     CALL ERRLOG( 'SRREAD: Programming error with '//
     1                     'equinox' )
                  END IF
                  RAERR(MSRC)  = SRCRAE
                  DECERR(MSRC) = SRCDECE
C
C                 Save the provided coordinates for later listings
C                 if proper motions were used.
C
                  RACAT(MSRC)  = SRCRA
                  DECCAT(MSRC) = SRCDEC
C
C                 Get the catalog source comment.
C           
                  REMARK(MSRC) = SRCRMK
C           
C                 Get calcode and be sure it is a number or capital
C                 letter (ASCII code 48 to 57 or 65 to 90).  Note it
C                 was upcased in RDSRC.
C           
                  CALCODE(MSRC) = SRCCAL
                  ICH = ICHAR( CALCODE(MSRC) )
                  IF( ICH .EQ. 32 .OR. 
     1                ( ICH .GE. 48 .AND. ICH .LE. 57 ) .OR.
     2                ( ICH .GE. 65 .AND. ICH .LE. 90 ) ) THEN
C                    OK
                  ELSE
                     CALL ERRLOG( 'SRREAD: Illegal character '
     1                  //SRCCAL//' in CALCODE for '
     2                  //SOURCE(1,MSRC) )
                  END IF
C           
C                 Get LSR velocities.  If some not given, set them
C                 equal to the first.  If first not given, set very
C                 big negative velocity.  DOPFQ will sense this.
C           
                  DO IV = 1, MAXLCH
                     VLSR(IV,MSRC) = SRCVEL(IV)
                     IF( SRCVEL(IV) .EQ. CONT ) THEN
                        IF( IV .EQ. 1 ) THEN
                           VLSR(IV,MSRC) = -1.E9
                        ELSE
                           VLSR(IV,MSRC) = VLSR(1,MSRC)
                        END IF
                     END IF
                  END DO
                  VELREF(MSRC) = SRCVREF
                  VELDEF(MSRC) = SRCVDEF
C           
C                 Planetary motion parameters.
C           
                  DRA(MSRC) = SRCDRA
                  DDEC(MSRC) = SRCDDEC
                  IF( DRA(MSRC) .NE. 0.0 .OR. DDEC(MSRC) .NE. 0.0 )
     1                  THEN
                     CALL SLA_CLDJ( SRCPMY, SRCPMM, SRCPMD, 
     1                   PMTIME(MSRC), ERR )
                     IF( ERR .NE. 0 .AND. ERR .NE. 3 ) 
     1                  CALL ERRLOG( ' SRREAD: Problem converting'//
     2                  ' planetary motion epoch to MJD' )
                     PMTIME(MSRC) = PMTIME(MSRC) + SRCPMT/TWOPI
                  END IF
C
C                 Proper motion parameters.
C
                  EPOCHT(MSRC) = SRCEPOT
                  PMRA(MSRC) = SRCPMRA
                  PMDEC(MSRC) = SRCPMDEC
                  PARALAX(MSRC) = SRCPARA
                  IF( ( PMRA(MSRC) .NE. 0.0 .OR. 
     1                  PMDEC(MSRC) .NE. 0.0 .OR.
     2                  PARALAX(MSRC) .NE. 0.0 ) .AND. 
     3                ( EPOCHT(MSRC) .LT. 1900.0 .OR.
     4                  EPOCHT(MSRC) .GT. 2100.0 ) ) THEN
                     CALL WLOG( 1, 'SRREAD: **** Proper motions ' //
     1                  'specified for ' // SRCNAM(1) )
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, F12.5 )' )
     1                '             But a very unlikely EPOCH given: ',
     2                  EPOCHT(MSRC)
                     CALL WLOG( 1, MSGTXT )
                  END IF
                     
C           
               END IF  !  KEEPIT
C           
C              Save low precision version of all sources for
C              plotting purposes.
C           
               CALL SRLIST( SRCNAM(1), SRCRA, SRCDEC, 
     1                      SRCRAE, SRCDECE, SRCEQ, SRCCAL, SRUSED )
C           
            ELSE
C
C              Warn of an unnamed source.
C
               CALL WLOG( 1, ' WARNING:  An unnamed source was ' //
     1            'found in the source catalog:' )
               CALL WLOG( 1, '           ' // INFILE )
               CALL WLOG( 1, '           There may be an extra ' //
     1            '''/'' somewhere.' )
            END IF  !  SRCNAM not NONAME
C           
C           Return for next source
C           
         GO TO 100
C           
C        End of source reading loop.  
C
      END IF  !  READIT 
C
  999 CONTINUE
C
C     Part of temporary sources with low error bars list.
C
C      write(*,*) 'Number below ', elim, ' is ', nlim
C
      RETURN
      END

