C$Procedure      WRITLA ( Write array of lines to a logical unit )
 
      SUBROUTINE WRITLA ( NUMLIN, ARRAY, UNIT )
 
C$ Abstract
C
C     This routine will write an array of text lines to a Fortran
C     logical unit.
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
 
      INTEGER               NUMLIN
      CHARACTER*(*)         ARRAY(*)
      INTEGER               UNIT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      NUMLIN    I    Number of lines to be written to the file.
C      ARRAY     I    Array containing the lines to be written.
C      UNIT      I    Fortran unit number to use for output.
C
C$ Detailed_Input
C
C
C     NUMLIN   The number of text lines in ARRAY which are to be
C              written to UNIT. NUMLIN > 0.
C
C     ARRAY    The array which contains the text lines to be written to
C              UNIT.
C
C              The contents of this variable are not modified.
C
C     UNIT     The Fortran unit number for the output. This may
C              be either the unit number for the terminal, or the
C              unit number of a previously opened text file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the number of lines, NUMLIN, is not positive, the error
C          SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If an error occurs while attempting to write to the text
C          file attached to UNIT, a routine called by this routine will
C          detect and signal the error.
C
C$ Files
C
C     See the description of UNIT above.
C
C$ Particulars
C
C     This routine writes an array of character strings to a specified
C     Fortran logical unit, writing each array element as a line of
C     output.
C
C$ Examples
C
C     The following example demonstrates the use of this routine,
C     displaying a short poem on the standard output device, typically a
C     terminal screen.
C
C           PROGRAM EXAMPL
C     C
C     C     Example program for WRITLA.
C     C
C           CHARACTER*(80) LINES(4)
C
C           LINES(1) = 'Mary had a little lamb'
C           LINES(2) = 'Whose fleece was white as snow'
C           LINES(3) = 'And everywhere that mary went'
C           LINES(4) = 'The lamb was sure to go'
C
C           CALL WRITLA ( 4, LINES, 6 )
C
C           END
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
C-    Beta Version 2.0.0, 13-OCT-1994 (KRG)
C
C        This routine now participates fully with the SPICELIB error
C        handler, checking in on entry and checking out on exit. The
C        overhead associated with the error handler should not be
C        significant relative to the operation of this routine.
C
C-    Beta Version 1.0.0, 18-DEC-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      write an array of text lines to a logical unit
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local variables
C
      INTEGER               I
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WRITLA' )
      END IF
C
C     Check to see if the maximum number of lines is positive.
C
      IF ( NUMLIN .LE. 0 ) THEN
 
         CALL SETMSG ( 'The number of lines to be written was not'  //
     .                 ' positive. It was #.'                        )
         CALL ERRINT ( '#', NUMLIN                                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'WRITLA'                                      )
         RETURN
 
      END IF
C
C     Begin writing the lines to UNIT. Stop when an error occurs, or
C     when we have finished writing all of the lines.
C
      DO I = 1, NUMLIN
 
         CALL WRITLN( ARRAY(I), UNIT )
 
         IF ( FAILED() ) THEN
C
C           If the write failed, an appropriate error message has
C           already been set, so we simply need to return.
C
            CALL CHKOUT ( 'WRITLA' )
            RETURN
 
         END IF
 
      END DO
 
      CALL CHKOUT ( 'WRITLA' )
      RETURN
 
      END
