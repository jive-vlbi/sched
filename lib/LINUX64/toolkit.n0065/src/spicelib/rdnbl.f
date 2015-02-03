C$Procedure RDNBL ( Read non-blank line )
 
      SUBROUTINE RDNBL ( FILE, LINE, EOF )
 
C$ Abstract
C
C     Read the next non-blank line of text from a text file.
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
C     FILES,  TEXT
C
C$ Declarations
 
      CHARACTER*(*)       FILE
      CHARACTER*(*)       LINE
      LOGICAL             EOF
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      FILE       I   Input text file.
C      LINE       O   Next non-blank line from the input text file.
C      EOF        O   End-of-file indicator.
C
C$ Detailed_Input
C
C      FILE        is the name of the text file from which the next
C                  line is to be read. If the file is not currently
C                  open, it is opened with a logical unit determined
C                  at run time, and the first line of the file is
C                  returned. Otherwise, the next line not yet read
C                  from the file is read and returned.
C
C$ Detailed_Output
C
C      LINE        is next non-blank line of text in the specified file.
C
C      EOF         is true when the end of the file is reached, and is
C                  otherwise false.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If either the end of the file is reached or an error occurs
C        before a non-blank line is found, LINE is blank.
C
C$ Files
C
C     See input FILES.
C
C$ Particulars
C
C      RDNBL simply calls RDTEXT until one of two things happens:
C
C         1. A non-blank line is found (in which case the line
C            is returned).
C
C         2. The end of the file is reached (in which case the
C            file is closed, a blank line is returned, and the
C            end-of-file indicator becomes TRUE.)
C
C$ Examples
C
C      Let FILE.1 contain the following lines.
C
C         Mary had a little lamb
C
C         Everywhere that Mary went
C
C
C
C         Its fleece was white as snow.
C         The lamb was sure to go.
C
C      Then the code fragment
C
C         DO I = 1, 4
C            CALL RDNBL ( 'FILE.1', LINE, EOF )
C            WRITE (*,*) LINE
C         END DO
C
C      produces the following output:
C
C         Mary had a little lamb
C         Everywhere that Mary went
C         Its fleece was white as snow.
C         The lamb was sure to go.
C
C      In fact, the following code fragment removes all of the blank
C      lines from an arbitrary text file (FILE).
C
C         CALL RDNBL ( FILE, LINE, EOF )
C
C         DO WHILE ( .NOT. EOF )
C            WRITE (*,*) LINE( : RTRIM(LINE) )
C
C            CALL RDNBL ( FILE, LINE, EOF )
C         END DO
C
C      Note that because RDNBL calls RDTEXT, calls to either routine
C      can be interspersed. For example, RDNBL can be used to skip
C      blank lines at the beginning of the file, leaving the rest to
C      be processed:
C
C         CALL RDNBL ( FILE, LINE, EOF )
C
C         DO WHILE ( .NOT. EOF )
C            < do something with LINE >
C
C            CALL RDTEXT ( FILE, LINE, EOF )
C         END DO
C
C$ Restrictions
C
C      Any restrictions that apply to RDTEXT apply to RDNBL as well.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 07-AUG-1994 (IMU)
C
C-&

C$ Index_Entries
C
C     read a non-blank line from a text file
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDNBL' )
      END IF
 
C
C     Return as soon as a non-blank line is found. Otherwise, keep
C     looking until either the end of the file is reached or RDTEXT
C     manages to fail.
C
      CALL RDTEXT ( FILE, LINE, EOF )
 
      DO WHILE ( ( .NOT. EOF )  .AND.  ( .NOT. FAILED() ) )
 
         IF ( LINE .NE. ' ' ) THEN
            CALL CHKOUT ( 'RDNBL' )
            RETURN
 
         ELSE
            CALL RDTEXT ( FILE, LINE, EOF )
         END IF
 
      END DO
 
C
C     Didn't find anything?
C
      LINE = ' '
 
      CALL CHKOUT ( 'RDNBL' )
      RETURN
      END
