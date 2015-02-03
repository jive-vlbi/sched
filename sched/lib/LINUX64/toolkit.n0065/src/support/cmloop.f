C$Proceedure CMLOOP ( Command line loop )
C
      SUBROUTINE CMLOOP ( DELIM,  PROMPT, LOGNAM, VERSN,
     .                    GREET,  PREPRC, ACTION )
      IMPLICIT NONE
C
C$ Abstract
C
C     This routine handles the main processing loop of a
C     command driven program.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     INTERFACE
C
C$ Declarations
 
      CHARACTER*(*)         DELIM
      CHARACTER*(*)         PROMPT
      CHARACTER*(*)         LOGNAM
      CHARACTER*(*)         VERSN
 
      EXTERNAL              GREET
      EXTERNAL              PREPRC
      EXTERNAL              ACTION
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DELIM      I   Non-blank character used to delimit commands
C     PROMPT     I   Prompt to let the user know input is expected
C     LOGNAM     I   Name pattern of file where commands will be logged
C     VERSN      I   Program name and version
C     INTIZE     S   Subroutine that initializes I/O facilities
C     GREET      S   Displays a banner at program startup
C     ACTION     S   The command parser and processor.
C
C$ Detailed_Input
C
C     DELIM     is a character that will be used to tell the
C               program that a command has ended. Commands may
C               extend over as many lines as allowed by the
C               configuration include file.  They end on the
C               first line on which the delimiter character is
C               encountered. THIS CHARACTER MUST NOT BE "?"
C
C     PROMPT    is a string used to prompt the user for commands.
C               Typically, this is the name of the program that
C               calles CMLOOP.
C
C     LOGNAM    is a pattern to use when creating the name of
C               a file to which all commands will be written.
C               This can be hard coded in the calling
C               program, or may be determined by a file naming
C               convention such as is provided by Christen
C               and NOMEN.
C
C     VERSN     is a string that may contain anything you would
C               like to appear as descriptive text in the first
C               line of the log file (and possibly in the greeting
C               presented by the program)  Something like
C               '<program name> --- Version X.Y' would be appropriate.
C               For example if your programs name is KINDLE and you
C               are at version 4.2.3 of your program a good value for
C               VERSN would be
C
C               'KINDLE --- Version 4.2.3'
C
C               Your greeting routine can make use of this when
C               displaying your program's greeting.  In this way
C               you can centralize the name and version number of
C               your program at a high level or in a subroutine and
C               simply make the information available to CMLOOP so
C               that the automatic aspects of presenting this
C               information can be handled for you.
C
C
C     GREET     is a routine that displays a message at program
C               startup.  This should contain the version number
C               of the program, any general instructions such
C               as how to get help and who the author or organization
C               is that is responsible for this program. GREET
C               takes a single argument VERSN which you supply in
C               your call to CMLOOP.  It may also have
C               initializations that override various items set
C               up prior to the call to GREET such as the style
C               used for displaying error messages.  GREET
C               is the action taken by CMLOOP  before commencing the
C               loop of fetching and processing commands.
C
C     PREPRC    is a command preprocessor.  It might remove
C               non-printing characters such as TABS, resolve
C               symbols and convert units to expected ranges.
C
C     ACTION    is a routine responsible for action upon the commands
C               entered by a user at the keyboard. ACTION has two
C               arguments COMMAND a string input and ERROR a two
C               dimensional array for error and diagnostic output.
C               The first message should point to the the problem
C               assuming the user is aware of the context in which
C               the problem occurred.  The second message will
C               have more detailed information including trace
C               and other technical information.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     The parameters COMSIZ and ERRSIZ are given in the include
C     file commdpar.inc.
C
C     COMSIZ  is maximum number of characters that can be present
C             in a command.
C
C     ERRSIZ  is the maximum number of characters that can be used
C             when creating a diagnostic message.
C
C$ Exceptions
C
C     None.  This routine cannot detect any errors in its inputs
C     and all commands are regarded as legal input at this level.
C     Some can be acted on while others cannot.  Commands that
C     can not be exercised are expected to return diagnostics
C     in the array ERROR.  These are then reported by the
C     program to the user via his/her terminal.
C
C$ Files
C
C     The file specified by LOGFIL will be opened if possible
C     and all user commands and messages will be written to this
C     file.
C
C     Other files may be used a run time by "STARTing" a command
C     sequence file. Or by some result of the activity of the
C     user supplied routines ACTION, GREET, PREPRC.
C
C$ Particulars
C
C     This routine organizes the main loop of a command line
C     program so that the calling program can automatically
C     log files that a user enters, report errors in a uniform
C     manner and make use of sequences of commands stored in
C     files. The calling program supplies routines that handle
C     the chores of greeting the user and performing special
C     program initializations and performing actions based upon
C     the commands supplied by the user.  By making use of this
C     routine and its subordinates, the user inherits a flexible
C     I/O system and command interface freeing him/her to concentrate
C     on the actions of the program.
C
C     However, there is a minor price incurred by making use of
C     this routine.  Several commands have specific meanings that
C     the user cannot override.  They are commands that start with:
C
C        start
C        exit
C        stop
C        quit
C        echo
C        no echo
C        demo on
C        demo off
C        wait on
C        wait off
C        pause
C        ?
C     These commands are case insensitive with respect to the
C     words presented above.
C
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber    (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 6.0.0, 20-JUN-2000 (WLT)
C
C         Added the ability to run the loop without logging 
C         of inputs.
C
C-     Command Loop Configured Version 5.0.0, 23-MAR-2000 (WLT)
C
C         Modified the routine to call NSPEND instead of FINISH
C         now that NSPIO has been redone.
C
C-     Command Loop Configured Version 4.0.0, 20-NOV-1995 (WLT)
C
C         Added ability to run programs in batch mode and to
C         start procedures at program startup.
C
C-     Command Loop Configured Version 3.0.0, 1-AUG-1995 (WLT)
C
C         The routine was modified to better support command
C         pre-processing.  In particular symbol definition
C         and resolution is now supported.
C
C-     Command Loop Configured Version 2.0.0, 19-JUL-1995 (WLT)
C
C         A slight change was made so that the command delimiter
C         is now stored in the routine GETDEL.  Also errors
C         are now checked after command pre-processing has
C         been performed.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Beta Version 1.0.0, 8-OCT-1993 (WLT)
C
C-&
 
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )
 
      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )
 
      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )
 
      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )
      
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )
      
      CHARACTER*(FILSIZ)    USENAM
C
C     Language Sensitive Strings
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    ERRFLG
      
      
 
 
 
 
      INTEGER               ISRCHC
      INTEGER               LTRIM
      INTEGER               QRTRIM
 
      LOGICAL               HAVE
      LOGICAL               NO
      LOGICAL               CMMORE
      LOGICAL               DOLOG
 
      INCLUDE              'commdpar.inc'
 
      CHARACTER*(COMSIZ)    COMMND
      CHARACTER*(COMSIZ)    COM2DO
      CHARACTER*(ERRSIZ)    ERROR ( 2 )
      LOGICAL               PROBLM
 
      CHARACTER*(STYSIZ)    SSTYLE
      CHARACTER*(STYSIZ)    LSTYLE
      CHARACTER*(STYSIZ)    VSTYLE
      CHARACTER*(STYSIZ)    HSTYLE
 
      INTEGER               FROM
      INTEGER               L
      INTEGER               REST
 
      LOGICAL               LOG ( 0:3 )
      LOGICAL               TRAP
      LOGICAL               HIT
C
C     The following are for special commands that will not be
C     processed by ACTION.
C
      INTEGER               NSP
      PARAMETER           ( NSP = 2 )
 
      CHARACTER*(8)         SPCIAL ( NSP )
      SAVE
 
      DATA  SPCIAL / ' ', '?' /
C
C     Store the delimiter used by the program incase someone
C     else needs to know later on.
C
      CALL SETDEL ( DELIM )
 
 
C
C     First, set up the SPICELIB error handling.
C
      ERROR(1) = ' '
      ERROR(2) = ' '
      COMMND   = ' '
 
      LOG (NONE  )  = .FALSE.
      LOG (COMBUF)  = .FALSE.
      LOG (KEYBRD)  = .TRUE.
      LOG (INPFIL)  = .TRUE.
 
      CALL ERRACT ( 'SET', 'RETURN' )
      CALL ERRDEV ( 'SET', 'NULL'   )
 
C
C     Set the prompt for the program.
C
      CALL SETDAP ( DELIM, PROMPT )
 
 
C
C     The following styles are for reporting errors to the
C     screen and log file respectively.
C
      CALL TRNLAT ( 'ERRFLAG',  ERRFLG )
 
      SSTYLE = 'HARDSPACE ^ NEWLINE /cr '
     .//       'VTAB /vt FLAG ' // ERRFLG
 
      LSTYLE = 'HARDSPACE ^ NEWLINE /cr '
     .//       'VTAB /vt FLAG '
     .//        DELIM(1:1) // ERRFLG(1:QRTRIM(ERRFLG))
     .//       ' LEADER '  // DELIM(1:1) // '-- '
     .//       'LEFT 1 RIGHT 72 '
C
C     The following styles will be used for logging of
C     commands and for commenting them out.
C
      VSTYLE = 'LEFT 1 RIGHT 78 '
      HSTYLE = 'LEFT 1 RIGHT 78 LEADER ' // DELIM(1:1) // '-- '
 
      CALL NSPSTY ( SSTYLE, LSTYLE        )
      CALL NSPLGS ( VSTYLE, HSTYLE, DELIM )
      CALL NSPSLR ( 1, 78 )

C
C     See whether or not a log file should be used and if so
C     what it's name should be.
C
      CALL LOGCHK ( LOGNAM, USENAM, DOLOG )

C
C     Open a log file.
C
      IF ( DOLOG ) THEN
         CALL NSPOPL ( USENAM, VERSN )
      END IF
 
      IF ( HAVE(ERROR) ) THEN
         CALL NSPERR ( COMMND, ERROR )
      END IF
C
C     Present a greeting to the user and perform any override
C     or special initializations that need to be local to this
C     routine.
C
      CALL GREET ( VERSN )
C
C     Get the input command line.  This may have
C     several useful bits of information to tell us how
C     to run the program.
C
C     -b      means run the program in batch mode.  In this case
C             we should never prompt the user for information.
C
C     -start  means we have a startup file to use and we want to
C             use the name of that file to determine how to
C             proceed.
C
      CALL CMSTUP
 
C
C     Fetch and log the first command.
C
 
      TRAP = .TRUE.
C
C     Get the next command and resolve any symbols or
C     queries that might show up in it,
C
      DO WHILE ( TRAP )
 
         CALL GETCOM (                COM2DO,  FROM )
         CALL EDTCOM ( DELIM, PROMPT, COM2DO,  FROM )
 
         IF ( NO(ERROR) .AND. LOG(FROM) ) THEN
            CALL NSPLOG ( COM2DO,  .FALSE. )
         END IF
 
         IF ( NO(ERROR) ) THEN
            CALL RESSYM ( COM2DO, COMMND )
            CALL ECHO   ( COM2DO, COMMND )
         END IF
 
         IF ( NO(ERROR) ) THEN
            CALL CMREDO ( COMMND, FROM, TRAP )
         END IF
 
         IF ( HAVE(ERROR) ) THEN
            TRAP = .FALSE.
         END IF
 
      END DO
 
C
C     Now apply the user's preprocessing software
C     to the comman.
C
      COM2DO = COMMND
 
      CALL PREPRC ( COM2DO, COMMND )
 
C
C     Now process commands until we get an EXIT command.
C
      DO WHILE ( CMMORE( COMMND ) )
C
C        Perform any preprocessing that can be performed easily
C        on this command.
C
 
         IF ( NO(ERROR) ) THEN
            CALL BUILTN ( COMMND, HIT, ERROR )
         END IF
 
         IF ( NO(ERROR) .AND. .NOT. HIT ) THEN
 
            L    = LTRIM  ( COMMND)
            REST = QRTRIM ( COMMND ) + 1
 
            IF ( ISRCHC( COMMND(L:REST), NSP, SPCIAL ) .EQ. 0 ) THEN
               CALL ACTION ( COMMND, ERROR  )
            END IF
 
         END IF
 
         PROBLM = HAVE(ERROR)
C
C        Process any errors that were diagnosed.
C
         CALL NSPERR ( COMMND, ERROR )
C
C        Fetch and log the next command.
C
         TRAP = .TRUE.
 
         DO WHILE ( TRAP )
 
            CALL GETCOM (                COM2DO,  FROM )
            CALL EDTCOM ( DELIM, PROMPT, COM2DO,  FROM )
 
            IF ( NO(ERROR) .AND. LOG(FROM) ) THEN
               CALL NSPLOG ( COM2DO,  .FALSE. )
            END IF
 
            IF ( NO(ERROR) ) THEN
               CALL RESSYM ( COM2DO, COMMND )
               CALL ECHO   ( COM2DO, COMMND )
            END IF
 
            IF ( NO(ERROR) ) THEN
               CALL CMREDO ( COMMND, FROM, TRAP )
            END IF
 
            IF ( HAVE(ERROR) ) THEN
               TRAP = .FALSE.
            END IF
 
         END DO
C
C        Now apply the user's preprocessing software
C        to the comman.
C
         COM2DO = COMMND
         CALL PREPRC ( COM2DO, COMMND )
 
      END DO
C
C     Take care of closing files and so on.
C
 
      IF ( LOG(FROM) ) THEN
         CALL NSPEND
      END IF
 
      RETURN
      END
