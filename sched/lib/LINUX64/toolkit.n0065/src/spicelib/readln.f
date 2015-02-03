C$Procedure      READLN ( Read a text line from a logical unit )
 
      SUBROUTINE READLN ( UNIT, LINE, EOF )
 
C$ Abstract
C
C     This routine will read a single line of text from the Fortran
C     logical unit UNIT, reporting the end of file if it occurs.
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
C     ASCII
C     TEXT
C     FILES
C
C$ Declarations
C
      INTEGER            UNIT
      CHARACTER*(*)      LINE
      LOGICAL            EOF
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I    The Fortran unit number to use for input.
C      LINE      O    The line read from the file.
C      EOF       O    A logical flag indicating the end of file.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for the input. This may
C              be either the unit number for the terminal, or the
C              unit number of a previously opened text file.
C
C$ Detailed_Output
C
C     LINE     On output, this will contain the next text line
C              encountered when reading from UNIT.
C
C              If the length of the character string LINE is shorter
C              than the length of the current line in the text file, the
C              line is truncated on the right by the Fortran READ
C              statement, filling LINE with the first LEN(LINE)
C              characters from the current line in the file.
C
C              If an error or the end of file occurs during the
C              attempt to read from UNIT, the value of this variable
C              is not guaranteed.
C
C     EOF      On output, this variable will be set to .TRUE. if the
C              end of file ( IOSTAT < 0 ) is encountered during the
C              attempt to read from unit UNIT. Otherwise, this
C              variable will be set to .FALSE..
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If an error occurs while attempting to read from the text
C          file attached to UNIT, the error SPICE(FILEREADFAILED) will
C          be signalled.
C
C     This routine only checks in with the error handler in the event
C     that an error occurred. (Discovery check in)
C
C$ Files
C
C     None.
C
C$ Particulars
C
C      This routine will read a single line, a text record, from the
C      logical unit UNIT. UNIT may be the terminal, or it may be a
C      logical unit number obtained from a Fortran OPEN or INQUIRE
C      statement. This routine will set a logical flag, EOF, on output
C      if the end of the file is encountered during the read attempt.
C
C$ Examples
C
C      CALL READLN ( UNIT, LINE, EOF )
C
C      IF ( EOF ) THEN
C         < The end of file, deal with it appropriately >
C      END IF
C
C      You now have a line of text from unit UNIT.
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    SPICELIB     1.0.0, 20-DEC-1995 (KRG)
C
C        The routine graduated
C
C-    Beta Version 1.0.1, 22-NOV-1994 (KRG)
C
C        Cleaned up the comments a little bit. No code changes.
C
C-    Beta Version 1.0.0, 17-DEC-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      read a text line from a logical unit
C
C-&
 
C
C     Local variables
C
      INTEGER               IOSTAT
C
C     Standard SPICE error handling.
C
C
C     Read in the next line from the text file attached to UNIT.
C
      READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
C
C     Check to see if we got a read error, and signal it if we did.
C
      IF ( IOSTAT .GT. 0  ) THEN
 
         CALL CHKIN  ( 'READLN'                                  )
         CALL SETMSG ( 'Error reading from file: #. IOSTAT = #.' )
         CALL ERRFNM ( '#', UNIT                                 )
         CALL ERRINT ( '#', IOSTAT                               )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                   )
         CALL CHKOUT ( 'READLN'                                  )
         RETURN
 
      END IF
C
C     Check to see if we got the end of file, and set the logical
C     flag EOF if we did.
C
      IF ( IOSTAT .LT. 0 ) THEN
 
         EOF = .TRUE.
 
      ELSE
 
         EOF = .FALSE.
 
      END IF
 
      RETURN
      END
