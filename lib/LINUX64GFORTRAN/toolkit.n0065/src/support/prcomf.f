C$ Procedure
C
      SUBROUTINE PRCOMF ( FILE, DELIM, COMMAND, ERROR, LEVEL )
C
C$ Abstract
C
C     Keep track of nested command files.
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
C$ Keywords
C
C     PARSE
C
C$ Declarations
      IMPLICIT NONE
      CHARACTER*(*)    FILE
      CHARACTER*1      DELIM
      CHARACTER*(*)    COMMAND
      CHARACTER*(*)    ERROR
      CHARACTER*(*)    LEVEL
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     FILE       I   Command file.
C     DELIM      I   Symbol delimiting the end of a command.
C     COMMAND    O   Command read from FILE.
C     ERROR      O   Error flag.
C     LEVEL      O   A list of all files currently open.
C
C$ Detailed_Input
C
C     FILE       is the name of a file from which a sequence of commands
C                is to be read. These commands may include commands to
C                read from other files.
C
C     DELIM      is the character which delimits the end of each
C                instruction in FILE.
C
C$ Detailed_Output
C
C     COMMAND    is a command read from the current file.
C                If no files are currently open, COMMAND = DELIM.
C
C     ERROR      is a descriptive error message, which is blank when
C                no error occurs.
C
C     LEVEL      is a list of the files currently open, in the order
C                in which they were opened. It is provided for trace-
C                back purposes.
C
C$ Detailed_Description
C
C     PRCOMF opens, reads, and closes sets of (possibly nested)
C     command files. For example, consider the following command
C     files.
C
C        FILE_A : A1             FILE_B : B1               FILE_C : C1
C                 A2                      START FILE_C              C2
C                 A3                      B2                        C3
C                 START FILE_B            B3
C                 A4                      B4
C                 A5
C
C     If the command 'START FILE_A' were issued, we would expect the
C     following sequence of commands to ensue:
C
C        A1, A2, A3, B1, C1, C2, C3, B2, B3, B4, A4, A5.
C
C     The first file immediately becomes, ipso facto, the current file.
C     Subsequently, instructions are read from the current file until
C     either a START or the end of the file is encountered. Each time
C     a new START is encountered, the current file (that is, the
C     location of the next command in the file) is placed on a stack,
C     and the first command is read from the new file (which then
C     becomes the current file). Each time the end of the current file
C     is encountered, the previous file is popped off the top of the
C     stack to become the current file. This continues until there are
C     no files remaining on the stack.
C
C     On occasion, the user may wish to exit from a file without
C     reading the rest of the file. In this case, the previous file
C     is popped off the stack without further ado.
C
C     Also, the user may wish to abruptly stop an entire nested
C     set of files. In this case, all of the files are popped off
C     the stack, and no further commands are returned.
C
C     PRCOMF and its entry points may be used to process any such
C     set of files. These entry points are:
C
C        - PRCLR ( ERROR )
C
C          This clears the stack. It may thus be used to implement
C          a STOP command. In any case, it must be called before
C          any of the other entry points are called.
C
C        - PRSTRT ( FILE, ERROR )
C
C          This introduces a new file, causing the current file (if
C          any) to be placed on the stack, and replacing it with FILE.
C          It may thus be used to implement a START command.
C
C          If the file cannot be opened, or the stack is already
C          full (it can hold up to seven files), ERROR will contain
C          a descriptive error message upon return. Otherwise, it
C          will be blank.
C
C        - PRREAD ( COMMAND )
C
C          This causes the next command to be read from the current
C          file. If the end of the current file is reached, the
C          previous file is popped off the stack, and the next command
C          from this file is read instead. (If no files remain to be
C          read, DELIM is returned.)
C
C        - PREXIT
C
C          This causes the previous file to be popped off the top of
C          the stack to replace the current file. It may thus be used
C          to implement an EXIT command.
C
C        - PRTRCE ( LEVEL )
C
C          Should an error occur during the execution of a nested
C          file, it may be helpful to know the sequence in which
C          the nested files were invoked. PRTRCE returns a list of
C          the files currently open, in the order in which they were
C          invoked.
C
C$ Input_Files
C
C     All files read by PRCOMF are opened with logical units
C     determined at run time.
C
C$ Output_Files
C
C     None.
C
C$ Input_Common
C
C     None.
C
C$ Output_Common
C
C     None.
C
C$ Examples
C
C     See Detailed_Description.
C
C$ Restrictions
C
C     The declared length of ERROR should be at least 80, to avoid
C     truncationof error messages.
C
C$ Author_and_Institution
C
C     W. L. Taber     (JPL)
C     I. M. Underwood (JPL)
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
C
C     Version 1, 6-SEP-1986
C
C-&
 
 
 
 
C
C   OPTLIB functions
C
      INTEGER               LASTNB
 
      LOGICAL               HAVE
 
C
C     Local variables
C
 
C
C     NFILES is the maximum number of files that may be open at
C     any given time. THus, nesting of procedures is limited to
C     a depth of NFILES.
C
      INTEGER          NFILES
      PARAMETER      ( NFILES = 8 )
 
C
C     NEST is the number of files currently open.
C
      INTEGER          NEST
 
C
C     FILES are the names of the files on the stack. UNITS are
C     the logical units to which they are connected.
C
 
      CHARACTER*80     FILES      ( NFILES )
      INTEGER          UNITS      ( NFILES )
 
 
      INTEGER          IOSTAT
      INTEGER          I
      INTEGER          J
      SAVE
 
      DATA                  NEST     / 0 /
 
      RETURN
 
C
C$ Procedure PRCLR
C
      ENTRY  PRCLR
C
C$ Abstract
C
C     Clear the file stack.
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
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     None.
C
C$ Detailed_Description
C
C     Pop all the files off the stack.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
C-
 
      DO WHILE ( NEST .GT. 0 )
 
         CLOSE ( UNITS (NEST) )
         NEST = NEST - 1
 
      END DO
 
      RETURN
 
 
 
C
C$ Procedure PRSTRT
C
      ENTRY  PRSTRT ( FILE, ERROR )
C
C$ Abstract
C
C     Put the current file on the stack, and replace it with FILE.
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
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     FILE       I   New command file.
C     ERROR      O   Error flag.
C
C$ Detailed_Input
C
C     FILE       is the new current file from which commands are
C                to be read.
C
C$ Detailed_Output
C
C     ERROR      is blank when no error occurs, and otherwise contains
C                a descriptive message. Possible errors are:
C
C                     - The stack is full.
C
C                     - FILE could not be opened.
C
C$ Input_Files
C
C     FILE is opened with a logical unit determined at run time.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     None.
C
C$ Detailed_Description
C
C     If the stack is full, return an error. Otherwise, try to open
C     FILE. If an error occurs, return immediately. Otherwise, put
C     the current file on the stack, and increase the nesting level.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
C-
 
C
C     No error yet.
C
      ERROR = ' '
 
C
C     Proceed only if the stack is not full.
C
      IF ( NEST .EQ. NFILES ) THEN
         ERROR = 'PRSTRT: Command files are nested too deeply.'
         RETURN
      ELSE
         NEST = NEST + 1
      END IF
 
C
C     Get a new logical unit. If none are available, abort.
C
      CALL TXTOPR ( FILE, UNITS(NEST) )
 
      IF ( HAVE(ERROR) ) THEN
         NEST        = NEST - 1
      ELSE
         FILES(NEST) = FILE
      END IF
 
      RETURN
 
 
C
C$ Procedure PRREAD
C
      ENTRY  PRREAD ( DELIM, COMMAND )
C
C$ Abstract
C
C     Read the next command from the current file.
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
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     DELIM      I   Character delimiting the end of a command.
C     COMMAND    O   Next command from the current file.
C
C$ Detailed_Input
C
C     DELIM      is the character used to delimit the end of a
C                command within a command file.
C
C$ Detailed_Output
C
C     COMMAND    is the next command read from the current file.
C                If there is no current file, COMMND = DELIM.
C
C$ Input_Files
C
C     All files read by PRCOMF are opened with logical units
C     determined at run time.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     None.
C
C$ Detailed_Description
C
C     Attempt to read the next statement from the current file.
C     If the end of the file is encountered, pop the previous file
C     off the top of the stack, and try to read from it. Keep this
C     up until a command is read, or until no files remain open.
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
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
C-
 
C
C     Don't even bother unless at least one file is open.
C
      IF ( NEST .EQ. 0 ) THEN
         COMMAND = DELIM
         RETURN
      END IF
 
C
C     Keep trying to read until we run out of files.
C
      READ (UNITS(NEST),FMT='(A)',IOSTAT=IOSTAT) COMMAND
 
      DO WHILE ( IOSTAT .NE. 0  .AND. NEST .GT. 0 )
 
         CLOSE ( UNITS(NEST) )
         NEST = NEST - 1
 
         IF ( NEST .GE. 1 ) THEN
            READ (UNITS(NEST),FMT='(A)',IOSTAT=IOSTAT) COMMAND
         END IF
 
      END DO
 
      CALL RSTBUF (         )
 
      IF ( NEST .EQ. 0 ) THEN
         COMMAND = DELIM
         CALL PUTBUF ( COMMAND )
         RETURN
      END IF
 
      CALL PUTBUF ( COMMAND )
 
C
C     Okay, we have something. Keep reading until DELIM is found.
C     (Or until the file ends.) Add each successive line read to
C     the end of COMMAND. Do not return the delimiter itself.
C
 
      J  = 1
 
      I = INDEX   ( COMMAND, DELIM )
 
      DO WHILE ( I .EQ. 0 .AND. IOSTAT .EQ. 0 )
 
         J            = LASTNB ( COMMAND ) + 1
         COMMAND(J:J) = ' '
         J            = J + 1
 
         READ (UNITS(NEST),FMT='(A)',IOSTAT=IOSTAT) COMMAND(J: )
 
         CALL PUTBUF ( COMMAND(J:)        )
         I =  INDEX  ( COMMAND,     DELIM )
 
      END DO
 
      IF ( I .GT. 0 ) THEN
         COMMAND(I: ) = ' '
      END IF
 
      RETURN
 
 
C
C$ Procedure PREXIT
C
      ENTRY  PREXIT
C
C$ Abstract
C
C     Replace the current file with the one at the top of the stack.
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
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     None.
C
C$ Detailed_Description
C
C     Close the current file. Pop the previous file off the top of
C     the stack. If there is no current file, of if there are no
C     files on the stack, that's cool too.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
C-
 
      IF ( NEST .GT. 0 ) THEN
 
         CLOSE ( UNITS (NEST) )
         NEST = NEST - 1
 
      END IF
 
      RETURN
 
 
C
C$ Procedure PRTRCE
C
      ENTRY  PRTRCE ( LEVEL )
C
C$ Abstract
C
C     Provide a list of the files currently open, in the order in
C     which they were opened.
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
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     LEVEL      O   List of all files currently open.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     LEVEL      A list of all files that are currently open, in
C                the order in which they were opened. For example,
C                if FILE_A starts FILE_B, and FILE_B starts FILE_C,
C                LEVEL would be 'FILE_A:FILE_B:_FILE_C'.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Input_Output_Common
C
C     None.
C
C$ Detailed_Description
C
C     Just step through the stack, Jack.
C
C$ Examples
C
C     See Detailed_Description.
C
C$ Restrictions
C
C     LEVEL should be declared to be at least CHARACTER*640 by the
C     calling program to ensure that enough space is available to
C     list all open files.
C$ Version
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
 
C-
 
C
C     Not much to explain. Use LBUILD to build a list, delimited
C     by colons.
C
      LEVEL = ' '
 
      IF ( NEST .GT. 0 ) THEN
         CALL LBUILD ( FILES, NEST, ':', LEVEL )
      END IF
 
      RETURN
      END
