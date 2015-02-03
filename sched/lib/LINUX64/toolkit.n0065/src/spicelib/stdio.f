C$Procedure      STDIO ( Standard IO )
 
      SUBROUTINE STDIO ( NAME, UNIT )
 
C$ Abstract
C
C    Return the logical unit associated with some standard input or
C    standard output.
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
C     I/O
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         NAME
      INTEGER               UNIT
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   is the name of a logical unit to return.
C     UNIT       O   is the logical unit associated with NAME.
C
C$ Detailed_Input
C
C     NAME       is the "name" of a FORTRAN unit to return.
C                Recognized names are 'STDIN' and 'STDOUT'.
C                The routine is case insensitive to NAME.
C
C                If NAME is not recognized the error
C                'SPICE(BADSTDIONAME)' is signalled and UNIT is
C                set to -100.
C
C$ Detailed_Output
C
C     UNIT       is the logical unit associated with NAME.  If
C                NAME is not recognized, UNIT is set to -100.
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
C     1)  If NAME is not recognized, the error 'SPICE(BADSTDIONAME)' is
C         signalled.
C
C$ Particulars
C
C     This is a low level utility for retrieving the logical units
C     associated with standard input and output.  It exists to
C     isolate SPICE based code from compiler writer choices in the
C     implementation of standard input and output.
C
C$ Examples
C
C     Suppose you would like to send a message to standard output
C     and that this message is contained in the array of N character
C     strings MESSGE.  The code below would handle the task.
C
C        CALL STDIO ( 'STDOUT', STDOUT )
C
C        DO I = 1, N
C           CALL WRITLN ( MESSGE(I), STDOUT )
C        END DO
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 18-SEP-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     logical units associated standard input and output
C
C-&
C
C     Spicelib Functions
C
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 8 )
 
      CHARACTER*(WDSIZE)    MYNAME
 
      CALL LJUST ( NAME,   MYNAME )
      CALL UCASE ( MYNAME, MYNAME )
 
      IF ( MYNAME .EQ. 'STDIN' ) THEN
         UNIT = 5
      ELSE IF ( MYNAME .EQ. 'STDOUT' ) THEN
         UNIT = 6
      ELSE IF ( RETURN() ) THEN
         RETURN
      ELSE
 
         CALL CHKIN  ( 'STDIO' )
         CALL SETMSG ( 'The only "names" recognized by STDIO are '
     .   //            '''STDIN'' and ''STDOUT'' you requested a '
     .   //            'unit for ''#''. '     )
         CALL ERRCH  ( '#', NAME              )
         CALL SIGERR ( 'SPICE(BADSTDIONAME)'  )
         CALL CHKOUT ( 'STDIO' )
      END IF
 
      RETURN
      END
