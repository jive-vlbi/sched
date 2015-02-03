C$Procedure      IOERR ( I/O error message writer )
 
      SUBROUTINE IOERR ( ACTION, FILE, IOSTAT )
 
C$ Abstract
C
C      Set the long error message equal to a standard I/O error message
C      composed from an action, the name of a file, and a value of
C      IOSTAT.
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
C      ERROR,  FILES
C
C$ Declarations
 
      CHARACTER*(*)    ACTION
      CHARACTER*(*)    FILE
      INTEGER          IOSTAT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ACTION     I   Action which caused the error.
C      FILE       I   The name of the file involved.
C      IOSTAT     I   The value of IOSTAT returned by ACTION.
C
C$ Detailed_Input
C
C
C      ACTION    is the action which caused the error. This may
C                be the name of a basic operation, such as 'OPEN',
C                'READ', or 'WRITE', or may be more sophisticated,
C                for example, 'add an empty cluster header to'.
C
C      FILE      is the name of the file involved in the error.
C                This may be the system or logical name of a file
C                ('USER$DISK:[USER.SUB]TEMP.DAT', 'PLNEPH'), or one
C                of the standard files ('SYS$INPUT', 'SYS$OUTPUT').
C
C      IOSTAT    is the value of IOSTAT returned by ACTION. This
C                is appended to the end of the error message.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      The input arguments are inserted into the standard form shown
C      below.  Spaces are inserted where needed. Leading and trailing
C      spaces are removed.
C
C      The long error message is set equal to a standard I/O error
C      message, of the form:
C
C                  An error occurred while --------1----------
C                  -------2-------.  The value of IOSTAT returned
C                  was --3--.
C
C                where the values of ACTION, FILE, and IOSTAT are
C                assigned to positions 1, 2 and 3
C                respectively.
C
C      If the length of the entire composed message exceeds 320
C      characters, it is truncated.
C
C      SIGERR must be called following a call to this routine to
C      actually output the resulting long error message to the error
C      output device.
C
C$ Examples
C
C      The following example illustrates the use of IOERR.
C
C            CALL IOERR ( 'adding a new header to',
C                         EPHEM,
C                         24                      )
C
C      The resulting error message would be:
C
C            'An error occurred while adding a new header
C             to LIBDISK:[EPHEM.NESYS]VGR2_T860502.GEF.  The value
C             of IOSTAT returned was 24.'
C
C      Note that the user is not responsible for adding and eliminating
C      spaces to make the string readable. That is all done
C      automatically.
C
C      It is possible to omit the name of the file entirely, as in the
C      following (somewhat frivolous) example.
C
C            CALL IOERR ( 'cleaning a fish',
C                         ' ',
C                         -3                                )
C
C      The resulting error message would be:
C
C            'An error occurred while cleaning a fish.
C             The value of IOSTAT returned was -3.'
C
C      In fact, if the value of IOSTAT is zero, the last part of the
C      message is omitted entirely, as in the following example.
C
C            CALL IOERR ( 'writing the status line to',
C                         'SYS$OUTPUT',
C                         0                                 )
C
C      The resulting error message would be:
C
C            'An error occurred while writing the status
C             line to SYS$OUTPUT.'
C
C$ Restrictions
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     i/o error message writer
C
C-&
 
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 20-DEC-1988 (NJB)
C
C        IOERR now sets the long error message equal to the
C        constructed message, rather than returning the constructed
C        message to the caller.  IOERR's argument list has been
C        changed accordingly, and a call to SETMSG has been added.
C        Also, the name of the calling routine no longer appears
C        in the constructed message.
C-&
 
 
C
C     Local variables
C
      CHARACTER*10          IOCHAR
 
      INTEGER               LONGLN
      PARAMETER           ( LONGLN = 320 )
      CHARACTER*(LONGLN)    ERROR
 
C
C     First comes some standard stuff.
C
      ERROR =  'An error occurred while'
 
C
C     Next comes the action that caused the error, and the file name.
C     There should be at least one space between each of these pieces,
C     but not more than one.
C
      CALL SUFFIX ( ACTION, 1, ERROR )
      CALL SUFFIX ( FILE,   1, ERROR )
      CALL SUFFIX ( '.',    0, ERROR )
 
C
C     More standard stuff. If IOSTAT is zero, there is no need for this
C     part of the message.
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SUFFIX ( 'The value of IOSTAT returned was', 2, ERROR )
 
C
C        IOSTAT must be written to a character variable first.
C        Attempting to write it directly to ERROR could cause a
C        boo-boo if we have already overrun the length of ERROR.
C
         CALL INTSTR ( IOSTAT, IOCHAR )
 
         CALL SUFFIX ( IOCHAR, 1, ERROR )
         CALL SUFFIX ( '.',    0, ERROR )
 
      END IF
 
C
C     The message has been constructed.  Set the long error message
C     equal to the constructed message.
C
 
      CALL SETMSG ( ERROR )
 
      RETURN
      END
