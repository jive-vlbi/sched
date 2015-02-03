      SUBROUTINE SETCHR( CHR, AT, STRING )
C
C$ Abstract
C
C     Set a particular location in a string to be a specified
C     character.
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
C     String
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(1)         CHR
      INTEGER               AT
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CHR        I   character to put in a specified location of string
C     AT        I/O   place in string to put CHR
C     STRING    I/O  string to be modified by overwriting a character
C
C$ Detailed_Input
C
C     CHR        A character to overwrite a specified  character of
C                the input string.
C
C     AT         Location in the input string to  overwrite.
C
C     STRING     String that will have one character modified.
C
C$ Detailed_Output
C
C     AT         Incremented by  1 from its input value.
C
C     STRING     The input string after having set STRING(AT:AT) = CHR
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
C     1) If AT is before or after the end of the  string.  The
C        error 'SPICE(INDEXOUTOFRANGE)' will be signalled.  The
C        string will  not  be modified.
C
C$ Particulars
C
C     This is a "macro" subroutine that encapulates the operations:
C
C      1)  check to make sure AT is in range
C      2)  Overwrite STRING(AT:AT) with CHR
C      3)  Increment AT by 1.
C
C$ Examples
C
C     Here's how you can use this routine to copy the text from one
C     string into another a character at a time..  Variations can be
C     made on this example to handle specific tasks based upon the
C     value of the  character to be copied.  The example assumes
C     that INPUT and OUTPUT occupy  distinct memory.
C
C     GET = 1
C     AT  = 1
C
C     DO GET = 1, LEN(INPUT)
C        CALL SETCHR( INPUT(GET:GET), AT, OUTPUT )
C     END DO
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
C-    SPICELIB Version 1.0.0, 28-MAR-2003 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Overwrite a character in a string
C
C-&
      LOGICAL               RETURN
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      IF ( AT .LT. 0 .OR. AT .GT. LEN(STRING) ) THEN
         CALL CHKIN ( 'SETCHR' )
         CALL SETMSG ( 'A request has been made to set the #''th '
     .   //            'character of a string.  However the valid '
     .   //            'range of characters is from 0 to #.' )
         CALL ERRINT ( '#', AT )
         CALL ERRINT ( '#', LEN(STRING) )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)' )
         CALL CHKOUT ( 'SETCHR' )
         RETURN
      END IF
 
      STRING(AT:AT) = CHR
      AT            = AT + 1
      RETURN
      END
