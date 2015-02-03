C$Procedure   READLA   ( Read array of lines from a logical unit )
 
      SUBROUTINE READLA ( UNIT, MAXLIN, NUMLIN, ARRAY, EOF )
 
C$ Abstract
C
C     This routine reads lines from a Fortran logical unit placing
C     them into a character array buffer.
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
C     UTILITY
C
C$ Declarations
 
      INTEGER               UNIT
      INTEGER               MAXLIN
      INTEGER               NUMLIN
      CHARACTER*(*)         ARRAY(*)
      LOGICAL               EOF
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I    Fortran unit number to use for input.
C      MAXLIN    I    Maximum number of lines ARRAY can hold.
C      NUMLIN    O    Number of lines read from the file.
C      ARRAY     O    Array containing the lines read from the file.
C      EOF       O    Logical flag indicating the end of file.
C
C$ Detailed_Input
C
C     UNIT     The Fortran unit number for the input. This may
C              be either the unit number for the terminal, or the
C              unit number of a previously opened text file.
C
C     MAXLIN   The maximum number of text lines that can be placed
C              into the ARRAY.
C
C$ Detailed_Output
C
C     NUMLIN   The number of text lines read from the file attached to
C              UNIT and placed into ARRAY. 0 <= NUMLIN <= MAXLIN.
C
C              In the event of an error while attempting to read a line
C              from the text file attached to UNIT, NUMLIN will contain
C              the number of lines successfully read before the error
C              occurred.
C
C     ARRAY    The array which is to contain the lines of text read from
C              the text file attached to UNIT.
C
C              If an error or the end of file occurs while reading
C              from the text file attached to UNIT, this array will
C              contain the NUMLIN successfully read lines ARRAY(1)
C              through ARRAY(NUMLIN).
C
C     EOF      On output, this variable will be set to .TRUE. if the
C              end of file ( IOSTAT < 0 ) is encountered during an
C              attempt to read from UNIT. Otherwise, this variable
C              will be set to .FALSE..
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the maximum number of lines, MAXLIN, is not positive, the
C          error SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If an error occurs while attempting to read from the text
C          file attached to unit, a routine called by this routine will
C          detect and signal the error.
C
C$ Files
C
C     See the description of UNIT above.
C
C$ Particulars
C
C     This routine reads lines of text from a file, placing each line
C     into an element of a character string array.
C
C     An end of file flag will have the value .TRUE. if the end of file
C     is reached while reading. If the file contains more lines than the
C     character string array ARRAY can hold, as specified by the
C     argument MAXLIN, the routine will return and the end of file flag
C     will have the value .FALSE., indicating that there are more lines
C     of text that may be read from the file.
C
C     Upon successful completion, the variable NUMLIN will contain the
C     number of lines of text placed into the character string array.
C     This value may be zero.
C
C$ Examples
C
C     For the examples which follow, assume that we have a file named
C     'mary.txt' which contains the following lines of text:
C
C        <BOF>
C        Mary had a little lamb
C        Whose fleece was white as snow
C        And every where that Mary went
C        The lamb was sure to go
C        <EOF>
C
C     where
C
C        <BOF> marks the beginning of the file
C        <EOF> marks the end of the file
C
C     For each example, assume that we have opened the file 'mary.txt',
C     obtaining the Fortran logical unit TXTLUN, and that we are
C     positioned to begin reading at the beginning of the file, '<BOF>'.
C
C     For brevity, none of the examples perform any error handling
C     functions: they simply assume that everything will work.
C
C     Example 1: ARRAY is large enough to contain the entire contents of
C                the file.
C
C        CHARACTER*(80)        ARRAY(10)
C
C        INTEGER               NUMLIN
C
C        LOGICAL               EOF
C
C        CALL READLA ( TXTLUN, 10, NUMLIN, ARRAY, EOF )
C
C     At this point the output variables NUMLIN, ARRAY, and EOF have
C     the following values:
C
C        NUMLIN   = 4
C
C        ARRAY(1) = 'Mary had a little lamb'
C        ARRAY(2) = 'Whose fleece was white as snow'
C        ARRAY(3) = 'And every where that Mary went'
C        ARRAY(4) = 'The lamb was sure to go'
C
C        EOF      = .TRUE.
C
C     Example 2: ARRAY is not large enough to contain the entire
C                contents of the file -- perform multiple reads.
C
C        CHARACTER*(80)        ARRAY(3)
C
C        INTEGER               NUMLIN
C
C        LOGICAL               EOF
C
C        EOF = .FALSE.
C        DO WHILE ( .NOT. EOF )
C
C           CALL READLA ( TXTLUN, 3, NUMLIN, ARRAY, EOF )
C
C        END DO
C
C     Because the line buffer ARRAY may contain at most 3 lines and the
C     file contains 4 lines, the loop calling READLA will be executed
C     twice, terminating after the second call because EOF will be
C     true.
C
C     After the first call to READLA the output variables NUMLIN, ARRAY,
C     and EOF have the following values:
C
C        NUMLIN   = 3
C
C        ARRAY(1) = 'Mary had a little lamb'
C        ARRAY(2) = 'Whose fleece was white as snow'
C        ARRAY(3) = 'And every where that Mary went'
C
C        EOF      = .FALSE.
C
C     After the second call to READLA the output variables NUMLIN,
C     ARRAY, and EOF have the following values:
C
C        NUMLIN   = 1
C
C        ARRAY(1) = 'The lamb was sure to go'
C
C        EOF      = .TRUE.
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
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    SPICELIB     1.0.0, 20-DEC-1995 (KRG)
C
C        The routine graduated
C
C-    Beta Version 3.0.0, 9-JAN-1995 (KRG)
C
C        Added examples to the header.
C
C        Fixed some problems with the variable descriptions in the
C        $ Detailed_Input and $ Detailed_Output sections of the header.
C
C        Rearranged some of the code to be more aesthetically pleasing.
C
C-    Beta Version 2.0.0, 05-JAN-1995 (KRG)
C
C        This routine now participates fully with the SPICELIB error
C        handler, checking in on entry and checking out on exit. The
C        overhead associated with the error handler should not be
C        significant relative to the operation of this routine.
C
C        Moved the test for the end of file outside of the loop. There
C        is no need to test for it every time in the loop, because we
C        only do it to decrement the number of lines read by one to
C        account for the pre-increment befor the READ that set the end
C        of file.
C
C        Added a local variable MYEOF so that a value of the variable
C        EOF does not affect the termination of the read loop.
C
C-    Beta Version 1.0.0, 18-DEC-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      read an array of text lines from a logical unit
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local variables
C
      INTEGER               I
 
      LOGICAL               MYEOF
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'READLA' )
      END IF
C
C     Check to see if the maximum number of lines is positive.
C
      IF ( MAXLIN .LE. 0 ) THEN
 
         CALL SETMSG ( 'The maximum number of lines for the output' //
     .                 ' line array was not positive. It was: #.'    )
         CALL ERRINT ( '#', MAXLIN                                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'READLA'                                      )
         RETURN
 
      END IF
C
C     Begin reading in the lines from the text file attached to UNIT.
C     Stop when the array of lines is full, I = MAXLIN, or we hit the
C     end of file.
C
      MYEOF  = .FALSE.
      NUMLIN = 0
      I      = 1
 
      DO WHILE ( ( I .LE. MAXLIN ) .AND. ( .NOT. MYEOF ) )
 
         CALL READLN( UNIT, ARRAY(I), MYEOF )
 
         IF ( FAILED() ) THEN
C
C           If the read failed, an appropriate error message has already
C           been set, so we need to set the number of lines that have
C           been correctly read from the file and return.
C
            CALL CHKOUT ( 'READLA' )
            RETURN
 
         END IF
 
         NUMLIN = I
         I      = I + 1
 
      END DO
C
C     If we got to here, then we have either filled up the line buffer
C     or we reached the end of the file. If we reached the end of the
C     file we need to adjust the value of NUMLIN to remove the last read
C     attempt.
C
      IF ( MYEOF ) THEN
 
         NUMLIN = NUMLIN - 1
 
      END IF
 
      EOF = MYEOF
 
      CALL CHKOUT ( 'READLA' )
      RETURN
 
      END
