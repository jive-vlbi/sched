C$Procedure      NXTCOM ( Next command )
 
      SUBROUTINE NXTCOM ( PROMPT, DELIM, COMMND, SOURCE )
      IMPLICIT NONE
 
C$ Abstract
C
C     Get the next command from the keyboard or a file.
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
C     FILES
C     PARSING
C
C$ Declarations
 
      CHARACTER*(*)         PROMPT
      CHARACTER*(*)         DELIM
      CHARACTER*(*)         COMMND
      INTEGER               SOURCE
 
C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     PROMPT     I   SETDAP
C     DELIM      I   SETDAP
C     COMMND     O   GETCOM
C     SOURCE     O   GETCOM
C
C$ Detailed_Input
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Files
C
C     If the commands are contained in a file, they will be read from
C     that file. (The 'START' keyword indicates that commands are to
C     be read from a specified file.) If they are not contained in a
C     file, they are read from the keyboard.
C
C$ Exceptions
C
C     1) If NXTCOM is called directly, the error SPICE(BOGUSENTRY) is
C        signalled.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     The delimeter has a fixed length of one character. The prompt has
C     a fixed length of eighty characters.
C
C     The file name length has been parameterized internally to the
C     maximum file name length length on the VAX, 128 characters.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-     Commad Loop Version 2.0.0              19-NOV-1995 (WLT)
C
C         Added the batch mode capability.  If the BATCH function
C         returns TRUE then all keyboard routines return EXIT.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Beta Version 1.0.0, 02-DEC-1988 (HAN)
C
C-&
 
      INCLUDE           'commdpar.inc'
 
C
C     SPICELIB functions
C
      INTEGER               BRCKTI
      LOGICAL               BATCH
      LOGICAL               RETURN
 
C
C     Below are the various sources from which
C     commands might come.
C
C     NONE
C     COMBUF
C     KEYBRD
C     INPFIL
C
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )
 
      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )
 
      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )
 
      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )
 
 
C
C     Local variables
C
 
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
 
      CHARACTER*(     1)    SAVDLM
      CHARACTER*(    80)    SAVPMT
      CHARACTER*(    80)    WORD
      CHARACTER*(   300)    ERROR
      CHARACTER*(   300)    LNGMSG
      CHARACTER*(COMSIZ)    BUFFER (  MAXCOM )
      CHARACTER*(FILEN )    FILE
      CHARACTER*(FILEN )    REST
      CHARACTER*(WDSIZE)    EXIT
      CHARACTER*(WDSIZE)    SHTMSG
      CHARACTER*(WDSIZE)    START
      CHARACTER*(WDSIZE)    STOP
 
      INTEGER               BUFFED
      INTEGER               BUFSRC (  MAXCOM )
 
      LOGICAL               FIRST
      LOGICAL               NOCOM
      LOGICAL               READNG
 
      SAVE
C
C     Initial values
C
      DATA                  BUFFED        /  0        /
      DATA                  FIRST         / .TRUE.    /
      DATA                  READNG        / .FALSE.   /
      DATA                  SAVDLM        / ';'       /
      DATA                  SAVPMT        / ' '       /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NXTCOM' )
      END IF
 
 
C
C     This routine should never be called. If this routine is called,
C     an error is signalled.
C
      CALL SETMSG ( 'NXTCOM: You have called an entry which performs '//
     .              'no run-time function. This may indicate a bug. ' //
     .              'Please check the documentation for the '         //
     .              'subroutine NXTCOM.'  )
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'NXTCOM' )
      RETURN
 
 
 
 
C$Procedure GETCOM ( Get a command )
 
      ENTRY GETCOM ( COMMND, SOURCE )
 
C$ Abstract
C
C     Get a command from a file or the keyboard.
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
C     FILES
C     PARSING
C
C$ Declarations
C
C     CHARACTER*(*)         COMMND
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     COMMND     O   A command read from a file or from the screen.
C     SOURCE     O   The source of the command, file, terminal etc.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     COMMND     is the command which is parsed from a line read from
C                either the screen or a file.
C
C     SOURCE     Is an integer that indicates the source of the
C                command.  The table below shows the various
C                values and their meanings.
C
C                   0  ---   None, an error occurred
C                   1  ---   Command buffer
C                   2  ---   From standard input
C                   3  ---   From a STARTED File.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     If an error occurs in PRSTRT, the error SPICE(FILEREADFAILED)
C     is signalled. ( PRSTRT has not been modified to participate in the
C     new error handling. )
C
C$ Particulars
C
C     None.
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
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Beta Version 1.0.0, 29-NOV-1988 (HAN)
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETCOM' )
      END IF
 
      IF ( FIRST ) THEN
         CALL TRNLAT ( 'STOP',      STOP   )
         CALL TRNLAT ( 'EXIT',      EXIT   )
         CALL TRNLAT ( 'START',     START  )
         CALL TRNLAT ( 'DEFPROMPT', SAVPMT )
 
         FIRST = .FALSE.
      END IF
 
C
C     While we don't have a command, try to get one.  We look
C     in the command buffer first.
C
 
C
C     We don't have a command yet.
C
      NOCOM = .TRUE.
 
 
      DO WHILE ( NOCOM )
 
         IF ( BUFFED .GT. 0 ) THEN
 
            COMMND = BUFFER (  BUFFED   )
            SOURCE = BUFSRC (  BUFFED  )
            BUFFED = BUFFED -  1
 
         ELSE
 
C
C           If we're already reading from a file then just let PRREAD
C           take care of obtaining the command. If PRREAD reaches the
C           end of the current file, the previous file is popped off
C           the stack, and the next command from this file is read
C           instead. (If no files remain to be read, DELIM is returned.)
C           In that case we are no longer reading from files.
C
            IF ( READNG ) THEN
 
               CALL PRREAD ( SAVDLM, COMMND )
 
               SOURCE = INPFIL
 
               IF ( COMMND .EQ. SAVDLM ) THEN
               READNG = .FALSE.
               END IF
 
            END IF
 
 
C
C           If we're not reading from a file, get the command from the
C           keyboard. ( If the command was terminated by a blank line,
C           the command is returned as a blank. )
C
            IF ( .NOT. READNG ) THEN
 
               IF ( BATCH() ) THEN
                  COMMND = EXIT
               ELSE
                  CALL RDSTMT ( SAVPMT, SAVDLM, COMMND )
               END IF
 
               SOURCE = KEYBRD
 
            END IF
 
         END IF
 
C
C        We must have a command at this point.
C
         NOCOM = .FALSE.
 
C
C        We need to check to see if what we have is a control word.
C
         CALL NEXTWD ( COMMND, WORD, REST )
         CALL UCASE  ( WORD,   WORD       )
 
C
C        If the control word is 'START', we know that we will be
C        reading from a file. Let PRSTRT take care of keeping track of
C        the files being read from. If there's a problem in PRSTRT we
C        need to signal an error here due to PRSTRT's error handling.
C        Bail out if there's a problem. If all goes well in PRSTR,
C        we will read the first command in the file the next pass
C        through the DO LOOP.
C
 
         IF ( WORD .EQ. START ) THEN
C
C           We need to log this command commented out so that anyone
C           using the resulting log file, will not have to worry
C           about starting a file twice.
C
            CALL NSPLOG ( COMMND, .TRUE. )
 
            FILE = ' '
            CALL NEXTWD ( REST,  FILE, REST )
 
 
            IF ( FILE .EQ. ' ' ) THEN
 
               SOURCE = NONE
 
               CALL TRNLAT ( 'MISSINGFILELONG',  LNGMSG )
               CALL TRNLAT ( 'MISSINGFILESHORT', SHTMSG )
               CALL SETMSG ( LNGMSG    )
               CALL SIGERR ( SHTMSG    )
               CALL CHKOUT ( 'GETCOM'  )
               RETURN
 
            END IF
 
            CALL PRSTRT ( FILE , ERROR      )
 
C
C           If an error occurs in PRSTRT we're in trouble. Signal an
C           error and bail. If there's no problem, we're now reading
C           from a file.
C
            IF ( ERROR .NE. ' ' ) THEN
 
               SOURCE = NONE
 
               CALL TRNLAT (  'MISSINGFILESHORT', SHTMSG )
               CALL SETMSG ( ERROR    )
               CALL SIGERR ( SHTMSG   )
               CALL CHKOUT ( 'GETCOM' )
               RETURN
            ELSE
               READNG = .TRUE.
               NOCOM  = .TRUE.
            END IF
 
C
C        If the control word is 'STOP', clear the stack of files.
C        If we were reading commands from files, we won't be anymore.
C        If we were reading commands from the keyboard, the command to
C        return is 'STOP'.
C
 
         ELSE IF ( WORD .EQ. STOP ) THEN
 
 
            IF ( READNG ) THEN
               CALL PRCLR
               CALL NSPLOG ( COMMND, .TRUE. )
               READNG = .FALSE.
               NOCOM  = .TRUE.
            ELSE
               COMMND = WORD
            END IF
 
C
C        If the control word is 'EXIT', and we're reading from a file,
C        we need to remove that file from the stack. If we're reading
C        commands from the keyboard, we'll return the command 'EXIT'.
C
          ELSE IF ( WORD .EQ. EXIT ) THEN
 
             IF ( READNG ) THEN
                CALL PREXIT
                CALL NSPLOG ( COMMND, .TRUE. )
                NOCOM  = .TRUE.
             ELSE
                COMMND = WORD
             END IF
 
          END IF
 
 
      END DO
 
 
      CALL CHKOUT ( 'GETCOM' )
      RETURN
 
 
 
C$Procedure SETDAP ( Set the delimeter and prompt values )
 
      ENTRY SETDAP ( DELIM, PROMPT )
 
C$ Abstract
C
C     Set the delimeter and prompt values that are used for parsing
C     commands.
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
C     CHARACTER
C     PARSING
C
C$ Declarations
C
C     CHARACTER*1           DELIM
C     CHARACTER*80          PROMPT
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DELIM     I/O  Character delimiting the end of a command.
C     PROMPT    I/O  Character string indicating the beginning of a
C                    command.
C
C$ Detailed_Input
C
C     DELIM      is a single character delimiting the end of a command.
C                The default value of DELIM is ';'.
C
C     PROMPT     is a character string indicating the beginning of a
C                command. PROMPT has a maximum length of eighty
C                characters. The default value of PROMPT is 'Next? >'.
C
C$ Detailed_Output
C
C     DELIM      is the new character delimiting the end of a command.
C
C     PROMPT     is the new character string indicating the beginning
C                of a command. PROMPT has a maximum length of eighty
C                characters.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     DELIM and PROMPT have the default values of ';' and 'Next? >'
C     respectively. This module is called in order to change their
C     values.
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
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C-    Beta Version 1.0.0, 02-DEC-1988 (HAN)
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETDAP' )
      END IF
 
 
C
C     Set the values of the delimeter and prompt.
C
      SAVDLM = DELIM
      SAVPMT = PROMPT
 
      CALL TRNLAT ( 'STOP',  STOP  )
      CALL TRNLAT ( 'EXIT',  EXIT  )
      CALL TRNLAT ( 'START', START )
 
      IF ( SAVPMT .EQ. ' ' ) THEN
         CALL TRNLAT ( 'DEFPROMPT', SAVPMT )
      END IF
 
      FIRST = .FALSE.
 
      CALL CHKOUT ( 'SETDAP' )
      RETURN
 
C
C$ Procedure
C
      ENTRY PUTCOM ( COMMND, SOURCE )
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
      IF ( BUFFED .LT. MAXCOM ) THEN
 
         BUFFED            = BUFFED + 1
         BUFFER ( BUFFED ) = COMMND
         BUFSRC ( BUFFED ) = BRCKTI ( SOURCE, NONE, INPFIL )
         RETURN
 
      END IF
C
C     If you get to this point there's a problem.  No room
C     is left in the command buffer.
C
      CALL CHKIN  ( 'PUTCOM'                )
      CALL TRNLAT ( 'COMBUFFULLLNG', LNGMSG )
      CALL TRNLAT ( 'COMBUFFULLSHT', SHTMSG )
      CALL SETMSG (  LNGMSG                 )
      CALL SIGERR (  SHTMSG                 )
      CALL CHKOUT ( 'PUTCOM'                )
      RETURN
 
 
      END
