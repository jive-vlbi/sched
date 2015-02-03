 
C$Procedure CBGET ( Character buffer, get )
 
      SUBROUTINE CBGET_1 ( BEGIN, END, BUFFER, STRING  )
      IMPLICIT NONE
 
 
C$ Abstract
C
C     Get (return) a substring of a character buffer.
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
C     CB
C
C$ Keywords
C
C     ASCII
C     CHARACTER
C     STRING
C     TEXT
C
C$ Declarations
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0 )
 
      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BEGIN,
C     END        I   Initial, final buffer locations.
C     BUFFER     I   Character buffer.
C     STRING     O   String.
C
C$ Detailed_Input
C
C     BEGIN,
C     END         are the initial and final buffer locations of
C                 the string to be returned.
C
C     BUFFER      is a character buffer.
C
C$ Detailed_Output
C
C     STRING      is the string contained between locations BEGIN and
C                 END of BUFFER.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) The error 'SPICE(CBNOSUCHSTR)' is signalled whenever any of
C        the following conditions is detected:
C
C           -- BEGIN is less than one.
C
C           -- END is greater than the size of BUFFER.
C
C           -- BEGIN is greater than END.
C
C$ Particulars
C
C     If you think of the character buffer as a single character string,
C     this is exactly equivalent to the operation
C
C        STRING = BUFFER(BEGIN:END)
C
C     If shorter than the substring, STRING is truncated. If longer,
C     it is padded with blanks.
C
C$ Examples
C
C     The code fragment
C
C        STR = '..........................'
C
C        CALL CBPUT (  1, 13, 'ABCDEFGHIJKLM', BUFFER             )
C        CALL CBPUT ( 14, 26, 'NOPQRSTUVWXYZ', BUFFER             )
C        CALL CBGET (  1,  3,                  BUFFER, STR( 1:10) )
C        CALL CBGET (  1, 26,                  BUFFER, STR(11:13) )
C
C        WRITE (*,*) '+--------------------------+'
C        WRITE (*,*) '|'    // STR(1:26) //     '|'
C        WRITE (*,*) '+--------------------------+'
C
C     produces the following output.
C
C        +--------------------------+
C        |ABC       ABC.............|
C        +--------------------------+
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
C     Dagny Taggart, (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 19-JAN-1989 (DT)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               SIZECB_1
 
C
C     Local variables
C
      INTEGER               BUFLEN
      INTEGER               L
      INTEGER               B
      INTEGER               NEXT
      INTEGER               LAST
      INTEGER               I
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CBGET_1' )
 
         IF (      BEGIN  .LT.  1
     .        .OR. END    .GT.  SIZECB_1 ( BUFFER )
     .        .OR. BEGIN  .GT.  END                 ) THEN
 
            CALL SETMSG ( 'Tried to access locations #:#.' )
            CALL ERRINT ( '#', BEGIN                       )
            CALL ERRINT ( '#', END                         )
 
            CALL SIGERR ( 'SPICE(CBNOSUCHSTR)' )
            CALL CHKOUT ( 'CBGET_1'              )
            RETURN
         END IF
      END IF
 
C
C     Storage begins at location B in line L.
C
      BUFLEN = LEN ( BUFFER(1) )
 
      L      =     ( BEGIN - 1 ) / BUFLEN   + 1
      B      = MOD ( BEGIN - 1,    BUFLEN ) + 1
 
C
C     Assign one character at a time, changing input lines when
C     necessary. Do not assign any characters beyond the end of
C     the output string.
C
      NEXT = 1
      LAST = LEN ( STRING )
 
      DO I = BEGIN, END
 
         IF ( NEXT .LE. LAST ) THEN
            STRING(NEXT:NEXT) = BUFFER(L)(B:B)
            NEXT              = NEXT + 1
         END IF
 
         IF ( B .LT. BUFLEN ) THEN
            B = B + 1
         ELSE
            L = L + 1
            B = 1
         END IF
 
      END DO
 
C
C     Pad the output string with blanks, if necessary.
C
      IF ( NEXT .LE. LAST ) THEN
         STRING(NEXT: ) = ' '
      END IF
 
      CALL CHKOUT ( 'CBGET_1' )
      RETURN
      END
