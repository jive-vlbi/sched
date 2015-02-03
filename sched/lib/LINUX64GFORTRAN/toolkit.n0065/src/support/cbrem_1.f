 
C$Procedure CBREM ( Character buffer, remove )
 
      SUBROUTINE CBREM_1 ( BEGIN, END, BUFFER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Remove a string from a character buffer.
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
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BEGIN,
C     END        I   Initial, final buffer locations.
C     BUFFER    I,O  Character buffer.
C
C$ Detailed_Input
C
C     BEGIN,
C     END         are the initial and final locations within the
C                 character buffer bounding the part of the buffer
C                 to be removed.
C
C     BUFFER      is a character buffer.
C
C$ Detailed_Output
C
C     BUFFER      is the same character buffer, with the original
C                 contents of locations BEGIN through END removed.
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
C     this is exactly equivalent to the sequence
C
C        TEMP            = BUFFER(END+1: )
C        BUFFER(BEGIN: ) = TEMP
C
C     where TEMP is a string of infinite length.
C
C$ Examples
C
C     The code fragment
C
C       CALL CBPUT (  1, 26, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', BUFFER      )
C       CALL CBPUT ( 27, 52, '..........................', BUFFER      )
C       CALL CBREM (  2, 25,                               BUFFER      )
C       CALL CBGET (  1, 26,                               BUFFER, STR )
C
C       WRITE (*,*) '+--------------------------+'
C       WRITE (*,*) '|'    // STR(1:26) //     '|'
C       WRITE (*,*) '+--------------------------+'
C
C     produces the following output.
C
C       +--------------------------+
C       |AZ........................|
C       +--------------------------+
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
      INTEGER               ENDBUF
      INTEGER               L
      INTEGER               B
      INTEGER               NL
      INTEGER               NB
      INTEGER               I
 
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CBREM_1' )
 
         IF (      BEGIN  .LT.  1
     .        .OR. END    .GT.  SIZECB_1 ( BUFFER )
     .        .OR. BEGIN  .GT.  END                 ) THEN
 
            CALL SETMSG ( 'Tried to access locations #:#.' )
            CALL ERRINT ( '#', BEGIN                       )
            CALL ERRINT ( '#', END                         )
 
            CALL SIGERR ( 'SPICE(CBNOSUCHSTR)' )
            CALL CHKOUT ( 'CBREM_1'              )
            RETURN
         END IF
      END IF
 
C
C     Essential limits.
C
      BUFLEN = LEN    ( BUFFER(1) )
      ENDBUF = SIZECB_1 ( BUFFER    )
 
C
C     Each guy gets moved from location B in line L to location NB
C     in line NL. (N stands for New.)
C
      L      =     ( END ) / BUFLEN   + 1
      B      = MOD ( END,    BUFLEN ) + 1
 
      NL     =     ( BEGIN - 1 ) / BUFLEN   + 1
      NB     = MOD ( BEGIN - 1,    BUFLEN ) + 1
 
      DO I = END+1, ENDBUF
 
         BUFFER(NL)(NB:NB) = BUFFER(L)(B:B)
 
         IF ( B .LT. BUFLEN ) THEN
            B = B + 1
         ELSE
            L = L + 1
            B = 1
         END IF
 
         IF ( NB .LT. BUFLEN ) THEN
            NB = NB + 1
         ELSE
            NL = NL + 1
            NB = 1
         END IF
 
      END DO
 
C
C     Now we can just overwrite the vacated space at the end.
C
      CALL CBPUT_1 ( ENDBUF-(END-BEGIN), ENDBUF, ' ', BUFFER )
 
      CALL CHKOUT ( 'CBREM_1' )
      RETURN
      END
