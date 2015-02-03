C$Procedure      LBUILD ( Build a list in a character string )
 
      SUBROUTINE LBUILD ( ITEMS, N, DELIM, LIST )
 
C$ Abstract
C
C      Build a list of items delimited by a character.
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
C      CHARACTER,  LIST,  STRING
C
C$ Declarations
 
      CHARACTER*(*)    ITEMS ( * )
      INTEGER          N
      CHARACTER*(*)    DELIM
      CHARACTER*(*)    LIST
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ITEMS      I   Items in the list.
C      N          I   Number of items in the list.
C      DELIM      I   String used to delimit items.
C      LIST       O   List of items delimited by DELIM.
C
C$ Detailed_Input
C
C      ITEMS       are the items to be combined to make the output
C                  list. Leading and trailing blanks are ignored.
C                  (Only the non-blank parts of the items are used.)
C
C      N           is the number of items.
C
C      DELIM       is the string used to delimit the items in the
C                  output list. DELIM may contain any number of
C                  characters, including blanks.
C
C$ Detailed_Output
C
C      LIST        is the output list, containing the N elements of
C                  ITEMS delimited by DELIM. If LIST is not long enough
C                  to contain the output list, it is truncated on the
C                  right.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The non-blank parts of the elements of the ITEMS array are
C      appended to the list, one at a time, separated by DELIM.
C
C$ Examples
C
C      The following examples illustrate the operation of LBUILD.
C
C      1) Let
C               DELIM    = ' '
C
C               ITEMS(1) = 'A'
C               ITEMS(2) = '  number'
C               ITEMS(3) = 'of'
C               ITEMS(4) = ' words'
C               ITEMS(5) = 'separated'
C               ITEMS(6) = '  by'
C               ITEMS(7) = 'spaces'
C
C         Then
C               LIST  = 'A number of words separated by spaces'
C
C      2) Let
C               DELIM    = '/'
C
C               ITEMS(1) = ' '
C               ITEMS(2) = ' '
C               ITEMS(3) = 'option1'
C               ITEMS(4) = ' '
C               ITEMS(5) = 'option2'
C               ITEMS(6) = ' '
C               ITEMS(7) = ' '
C               ITEMS(8) = ' '
C
C         Then
C               LIST  = '//option1//option2///'
C
C      3) Let
C               DELIM    = ' and '
C
C               ITEMS(1) = 'Bob'
C               ITEMS(2) = 'Carol'
C               ITEMS(3) = 'Ted'
C               ITEMS(4) = 'Alice'
C
C         Then
C               LIST  = 'Bob and Carol and Ted and Alice'
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     build a list in a character_string
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          FRSTNB
      INTEGER          LASTNB
 
C
C     Local variables
C
      INTEGER          LPOS
      INTEGER          FIRST
      INTEGER          LAST
      INTEGER          DLEN
      INTEGER          LLEN
      INTEGER          ILEN
      INTEGER          I
 
 
 
C
C     Find the non-blank part of each item. Move it to the
C     end of the list, followed by a delimiter. If the item is
C     blank, don't move anything but the delimiter.
C
C     LPOS is the next position in the output list to be filled.
C     LLEN is the length of the output list.
C     DLEN is the length of DELIM.
C     ILEN is the length of the next item in the list.
C
      LIST = ' '
      LPOS = 1
      LLEN = LEN ( LIST  )
      DLEN = LEN ( DELIM )
 
      IF ( N .GT. 0 ) THEN
 
         DO I = 1, N
 
            IF ( LPOS .LE. LLEN ) THEN
 
               IF ( ITEMS(I) .EQ. ' ' ) THEN
                  LIST(LPOS: ) = DELIM
                  LPOS         = LPOS + DLEN
 
               ELSE
                  FIRST = FRSTNB ( ITEMS(I) )
                  LAST  = LASTNB ( ITEMS(I) )
                  ILEN  = LAST - FIRST + 1
 
                  LIST(LPOS: ) = ITEMS(I)(FIRST:LAST)
                  CALL SUFFIX ( DELIM, 0, LIST )
 
                  LPOS = LPOS + ILEN + DLEN
               END IF
 
            END IF
 
         END DO
 
C
C     We're at the end of the list. Right now, the list ends in
C     a delimiter. Drop it.
C
         IF ( LPOS-DLEN .LE. LLEN ) THEN
            LIST(LPOS-DLEN: ) = ' '
         END IF
 
      END IF
 
      RETURN
      END
