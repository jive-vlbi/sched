C$Procedure      BUILTN ( Built in Commands )
 
      SUBROUTINE BUILTN ( COMMND, HIT, ERROR )
 
C$ Abstract
C
C     This routine handles the normal commands that every
C     command line based program will support if you
C     use the command loop software.
C
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C       Command Loop
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         COMMND
      LOGICAL               HIT
      CHARACTER*(*)         ERROR ( 2 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      COMMND     I   A command
C      HIT        O   Indicates if the command was a built in command
C      ERROR      O   Indicates any problems that occurred.
C
C$ Detailed_Input
C
C     COMMND      is a command that is to be processed to see if it
C                 is one of the command loop built in commands.
C
C$ Detailed_Output
C
C     HIT         is a logical variable.  If the input command is
C                 recognized and acted on, HIT is returned as .TRUE.
C                 Otherwise it is returned as .FALSE.
C
C     ERROR       is blank unless a built in command is recognized
C                 and causes an error to be triggered.  In the later
C                 case ERROR will contain the diagnostics associated
C                 with the error.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If a problem is detected, it is diagnosed and returned
C        in the variable ERROR
C
C$ Particulars
C
C     This routine handles the "built in" commands that are
C     automatically available with every command loop routine these
C     are:
C
C        SET  EDITOR (1:)@word
C        SHOW SYMBOL @word
C        SHOW ENVIRONMENT
C        SAVE TO @word
C        DISCARD
C
C     These built in functions can be overridden (turned off) through
C     the companion entry point BUILTO
C
C$ Examples
C
C     See the routine CMLOOP
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 22-APR-1997 (WLT)
C
C        Declares NSPWLN as external
C
C-    SPICELIB Version 1.0.0, 5-DEC-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Command Loop Built in Commands.
C
C-&
 
C
C     Spicelib functions
C
      INTEGER               RTRIM
      INTEGER               LTRIM
      INTEGER               ISRCHC
      INTEGER               TOUCHI
C
C     Error handling interface routines.
C
      LOGICAL               HAVE
C
C     META/2 Functions
C
      LOGICAL               M2XIST
 
C
C     Inspekt External Routines
C
      EXTERNAL              NSPWLN
 
C
C     Variables needed for syntax declarations.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               NUPPER
      PARAMETER           ( NUPPER =  5 )
 
      INTEGER               NSYN
      PARAMETER           ( NSYN   = NUPPER )
 
      INTEGER               SYNLEN
      PARAMETER           ( SYNLEN = 80 )
 
      CHARACTER*(WDSIZE)    SYNKEY ( LBCELL : NSYN )
      INTEGER               SYNPTR ( LBCELL : NSYN )
      CHARACTER*(SYNLEN)    SYNVAL ( LBCELL : NSYN )
 
C
C     The following are for special commands that will not be
C     processed by BUILTN.
C
      INTEGER               NSP
      PARAMETER           ( NSP = 2 )
 
      CHARACTER*(8)         SPCIAL ( NSP )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 512 )
 
 
      INTEGER               MAXLNS
      PARAMETER           ( MAXLNS =  3 )
 
      CHARACTER*(LNSIZE)    MYERR  ( 2      )
      CHARACTER*(80)        TEMPLT
      CHARACTER*(LNSIZE)    VALUES ( MAXLNS )
      CHARACTER*(WDSIZE)    NAMES  ( MAXLNS )
 
C
C     Other Local Variables
C
      INTEGER               E
      INTEGER               I
      INTEGER               L
      INTEGER               NITEMS
      INTEGER               REST
 
      LOGICAL               FOUND
      LOGICAL               FIRST
      LOGICAL               STATUS ( 3 )
 
      LOGICAL               DOSAV
      LOGICAL               DODISC
      LOGICAL               DOEDIT
      LOGICAL               DOSYM
      LOGICAL               DOENV
 
 
C
C     Save everything
C
      SAVE
 
      DATA     FIRST     /.TRUE./
 
      DATA     DOSAV      /.TRUE./
      DATA     DODISC     /.TRUE./
      DATA     DOEDIT     /.TRUE./
      DATA     DOSYM      /.TRUE./
      DATA     DOENV      /.TRUE./
 
      DATA (   SYNVAL(I), I = 1, NUPPER )
     .     /   'SET[set]   EDITOR[editor] (1:)@word[rest]',
     .         'SHOW[show] SYMBOL[symbol] @word[def]',
     .         'SHOW[show] ENVIRONMENT[env]',
     .         'SAVE[save] TO  @word[rest]',
     .         'DISCARD[discard]' /
 
      DATA  SPCIAL / ' ', '?' /
 
      CALL CHKIN  ( 'BUILTN' )
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
         I     =  0
         I     = TOUCHI ( I )
         CALL M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )
 
      END IF
 
      L    = LTRIM ( COMMND)
      REST = RTRIM ( COMMND ) + 1
 
      IF ( ISRCHC( COMMND(L:REST), NSP, SPCIAL ) .GT. 0 ) THEN
         CALL CHKOUT ( 'BUILTN' )
         RETURN
      END IF
C
C     There are no errors yet.
C
      ERROR(1) = ' '
      ERROR(2) = ' '
      HIT      = .FALSE.
C
C     Check the input command to see if it is recognizable
C
      CALL M2CHCK ( COMMND, SYNKEY, SYNPTR, SYNVAL, MYERR )
 
      IF ( MYERR(1) .NE. ' ' ) THEN
         CALL CHKOUT ( 'BUILTN' )
         RETURN
      END IF
 
 
      IF      ( M2XIST('set') .AND. DOEDIT ) THEN
 
         CALL M2VGET ( 'rest', 1, FOUND, REST, E )
         CALL SETEDT ( COMMND(REST:) )
         HIT = .TRUE.
 
      ELSE IF ( M2XIST('symbol') .AND. DOSYM ) THEN
 
         CALL M2GETC ( 'def', COMMND, FOUND, TEMPLT  )
         CALL SHOSYM ( TEMPLT  )
         HIT = .TRUE.
 
      ELSE IF ( M2XIST('env') .AND. DOENV ) THEN
 
         NITEMS   = 3
         NAMES(1) = 'Editor'
         NAMES(2) = 'Echoing Commands'
         NAMES(3) = 'Screen Output File'
 
 
         CALL GETEDT ( VALUES(1) )
         CALL GTECHO ( VALUES(2) )
 
         CALL NSPGST ( 'SAVE', STATUS )
 
         IF ( STATUS(1) .AND. STATUS(2) .AND. .NOT. STATUS(3) ) THEN
 
            CALL NSPPFL ( 'SAVE', VALUES(3) )
 
         ELSE
 
            VALUES(3) = 'No Current Screen Save File'
 
         END IF
 
         CALL NSPWLN ( ' ' )
         CALL NSPWLN ( 'Current Environment' )
         CALL NSPWLN ( ' ' )
         CALL FLGRPT ( NITEMS, NAMES, VALUES, NSPWLN )
         CALL NSPWLN ( ' ' )
         HIT = .TRUE.
 
      ELSE IF ( M2XIST('save') .AND. DOSAV     ) THEN
 
         CALL M2VGET ( 'rest', 1, FOUND, REST, E )
         CALL NSPSAV ( COMMND(REST:), ERROR )
         HIT = .TRUE.
 
      ELSE IF ( M2XIST('discard') .AND. DODISC ) THEN
 
         CALL NSPIOC ( 'SAVE' )
         HIT = .TRUE.
 
      END IF
 
      FOUND = HAVE(ERROR)
      CALL CHKOUT ( 'BUILTN' )
      RETURN
 
 
 
C$Procedure      BUILTO ( Built in commands off )
 
      ENTRY BUILTO ( COMMND )
 
C$ Abstract
C
C    Turn off built-in command loop commands.
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
C     COMMAND LOOP
C
C
C$ Declarations
C
C     CHARACTER*(*)         COMMND
C
C$ Brief_I/O
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     COMMND     I   A list of commands to turn off.
C
C$ Detailed_Input
C
C     COMMND     is a list of words that describes which built-in
C                commands to disable.  The words and commands
C                they turn off are:
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) No errors are detected.
C
C$ Particulars
C
C     This routine allows you to turn off selected built in commands
C     commands available through command loop programs.
C
C$ Examples
C
C     Suppose you want to turn off the SHOW ENVIRONMENT and
C     SET EDITOR commands.
C
C     Do this:
C
C     COMMAND = 'EDITOR ENVIRONMENT'
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Literature_References
C
C       None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 5-DEC-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Disable built in command loop commmands
C
C
C-&
C
C     We just look at command to see which of the built in
C     command should be disabled.
C
      DOSAV  = INDEX ( COMMND, 'SAVE'        ) .EQ. 0
      DOENV  = INDEX ( COMMND, 'ENVIRONMENT' ) .EQ. 0
      DOEDIT = INDEX ( COMMND, 'EDITOR'      ) .EQ. 0
      DOSYM  = INDEX ( COMMND, 'SYMBOL'      ) .EQ. 0
      DODISC = INDEX ( COMMND, 'DISCARD'     ) .EQ. 0
 
      RETURN
 
      END
