C$Procedure      LPARSS ( Parse a list of items; return a set. )
 
      SUBROUTINE LPARSS ( LIST, DELIMS, SET )
 
C$ Abstract
C
C     Parse a list of items delimited by multiple delimiters,
C     placing the resulting items into a set.
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
C     CELLS
C     SETS
C
C$ Keywords
C
C     CHARACTER
C     PARSING
C     SETS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         LIST
      CHARACTER*(*)         DELIMS
      CHARACTER*(*)         SET  ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LIST       I   List of items delimited by DELIMS on input.
C     DELIMS     I   Single characters which delimit items.
C     SET        O   Items in the list, validated, left justified.
C
C$ Detailed_Input
C
C     LIST        is a list of items delimited by any one of the
C                 characters in the string DELIMS. Consecutive
C                 delimiters, and delimiters at the beginning and
C                 end of the list, are considered to delimit blank
C                 items. A blank list is considered to contain
C                 a single (blank) item.
C
C     DELIMS      contains the individual characters which delimit
C                 the items in the list. These may be any ASCII
C                 characters, including blanks.
C
C                 However, by definition, consecutive blanks are NOT
C                 considered to be consecutive delimiters. Nor are
C                 a blank and any other delimiter considered to be
C                 consecutive delimiters. In addition, leading and
C                 trailing blanks are ignored.
C
C$ Detailed_Output
C
C     SET         is a set containing the items in the list, left
C                 justified. Any item in the list too long to fit
C                 into an element of SET is truncated on the right.
C                 The size of the set must be initialized prior
C                 to calling LPARSS.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the size of the set is not large enough to accommodate all
C        of the items in the set, the error is diagnosed by routines in
C        the call tree of this routine.
C  
C     2) If the string length of ITEMS is too short to accommodate
C        an item, the item will be truncated on the right.
C
C     3) If the string length of ITEMS is too short to permit encoding
C        of integers via ENCHAR, the error is diagnosed by routines in
C        the call tree of this routine.
C  
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     The following examples illustrate the operation of LPARSS.
C
C     1) Let
C              LIST        = 'A number of words   separated   by
C                              spaces.'
C              DELIMS      = ' ,.'
C              SIZE (SET)  = 20
C
C        Then
C
C              CARDC (SET) = 8
C
C              SET (1)     = ' '
C              SET (2)     = 'A'
C              SET (3)     = 'by'
C              SET (4)     = 'number'
C              SET (5)     = 'of'
C              SET (6)     = 'separated'
C              SET (7)     = 'spaces'
C              SET (8)     = 'words'
C
C
C     2) Let
C
C              LIST        = '  1986-187// 13:15:12.184 '
C              DELIMS      = ' ,/-:'
C              SIZE (SET)  = 20
C
C        Then
C
C              CARDC (SET) = 6
C
C              SET (1)     = ' '
C              SET (2)     = '12.184'
C              SET (3)     = '13'
C              SET (4)     = '15'
C              SET (5)     = '187'
C              SET (6)     = '1986'
C
C
C     3) Let   LIST        = '  ,This,  is, ,an,, example, '
C              DELIMS      = ' ,'
C              SIZE (SET)  = 20
C
C        Then
C              CARDC (SET) = 5
C
C              SET (1)     = ' '
C              SET (2)     = 'This'
C              SET (3)     = 'an'
C              SET (4)     = 'example'
C              SET (5)     = 'is'
C
C
C     4) Let   LIST        = 'Mary had a little lamb, little lamb
C                             whose fleece was white      as snow.'
C              DELIMS      = ' ,.'
C              SIZE (SET)  = 6
C
C        An error would be signaled because the set is not
C        large enough to accommodate all of the items in the
C        list.
C
C
C     5) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.'
C              DELIMS      = ' .'
C              SIZE (SET)  = 10
C
C        An error would be signaled because the set is not
C        large enough to accommodate all of the items in the
C        list. Note that delimiters at the end (or beginning)
C        of list are considered to delimit blank items.
C
C
C     6) Let   LIST        = '1 2 3 4 5 6 7 8 9 10.'
C              DELIMS      = '.'
C              SIZE (SET)  = 10
C
C        Then
C
C              CARDC (SET) = 2
C
C              SET (1)     = ' '
C              SET (2)     = '1 2 3 4 5 6 7 8 9 10'
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
C
C        Bug fix:  code was modified to avoid out-of-range
C        substring bound conditions.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) (IMU)
C
C-&
 
C$ Index_Entries
C
C     parse a list of items and return a set
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 26-OCT-2005 (NJB)
C
C        Bug fix:  code was modified to avoid out-of-range
C        substring bound conditions.  The previous version
C        of this routine used DO WHILE statements of the form
C
C                  DO WHILE (      ( B         .LE. EOL   )
C           .                .AND. ( LIST(B:B) .EQ. BLANK ) )
C
C        Such statements can cause index range violations when the 
C        index B is greater than the length of the string LIST.
C        Whether or not such violations occur is platform-dependent.
C
C
C-    Beta Version 2.0.0, 10-JAN-1989 (HAN)
C
C        Error handling was added, and old error flags and their
C        checks were removed. An error is signaled if the set
C        is not large enough to accommodate all of the items in
C        the list.
C
C        The header documentation was updated to reflect the error
C        handling changes, and more examples were added.
C
C-&
 
 
C
C     SPICELIB functions
C 
      INTEGER               LASTNB
      INTEGER               SIZEC

      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      CHARACTER*(1)         BLANK
      PARAMETER           ( BLANK = ' ' )

      INTEGER               ISPACE
      PARAMETER           ( ISPACE = 32 )

C
C     Local variables
C
      CHARACTER*(1)         BCHR
      CHARACTER*(1)         ECHR
 
      INTEGER               B
      INTEGER               E
      INTEGER               EOL
      INTEGER               N
      INTEGER               NMAX
 
      LOGICAL               VALID
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LPARSS' )
      END IF
 
C
C     Because speed is essential in many list parsing applications,
C     LPARSS, like LPARSE, parses the input list in a single pass.
C     What follows is nearly identical to LPARSE, except the FORTRAN
C     INDEX function is used to test for delimiters, instead of testing
C     each character for simple equality. Also, the items are inserted
C     into a set instead of simply placed at the end of an array.
C
C     No items yet.
C
      N = 0
 
C
C     What is the size of the set?
C
      NMAX  = SIZEC ( SET )
 
C
C     The array has not been validated yet.
C
      VALID = .FALSE.
 
C
C     Blank list contains a blank item.  No need to validate.
C
      IF ( LIST .EQ. BLANK ) THEN
 
         CALL SCARDC ( 0,     SET )
         CALL INSRTC ( BLANK, SET )
 
         VALID = .TRUE.
 
      ELSE
C
C        Eliminate trailing blanks.  EOL is the last non-blank
C        character in the list.
C
         EOL = LASTNB ( LIST )
 
C
C        As the King said to Alice: 'Begin at the beginning.
C        Continue until you reach the end. Then stop.'
C
C        When searching for items, B is the beginning of the current
C        item; E is the end.  E points to the next non-blank delimiter,
C        if any; otherwise E points to either the last character
C        preceding the next item, or to the last character of the list.
C
         B = 1
 
         DO WHILE ( B .LE. EOL )
C
C           Skip any blanks before the next item or delimiter.
C
C           At this point in the loop, we know 
C
C              B <= EOL
C           
            BCHR = LIST(B:B)

            DO WHILE (       ( B           .LE. EOL    ) 
     .                 .AND. ( ICHAR(BCHR) .EQ. ISPACE ) )

               B = B + 1

               IF ( B .LE. EOL ) THEN
                  BCHR = LIST(B:B)                  
               END IF

            END DO

C
C           At this point B is the index of the next non-blank
C           character BCHR, or else 
C 
C              B == EOL + 1
C
C           The item ends at the next delimiter.
C
            E = B

            IF ( E .LE. EOL ) THEN
               ECHR = LIST(E:E)
            ELSE
               ECHR = BLANK
            END IF

            DO WHILE (       (  E                     .LE. EOL ) 
     .                 .AND. (  INDEX( DELIMS, ECHR ) .EQ. 0   )  )

               E = E + 1

               IF ( E .LE. EOL ) THEN
                  ECHR = LIST(E:E)                  
               END IF

            END DO

C
C           (This is different from LPARSE. If the delimiter was
C           a blank, find the next non-blank character. If it's not
C           a delimiter, back up. This prevents constructions
C           like 'a , b', where the delimiters are blank and comma,
C           from being interpreted as three items instead of two.
C           By definition, consecutive blanks, or a blank and any
C           other delimiter, do not count as consecutive delimiters.)
C
            IF (       ( E           .LE. EOL    ) 
     .           .AND. ( ICHAR(ECHR) .EQ. ISPACE ) ) THEN
C
C              Find the next non-blank character.
C
               DO WHILE (       ( E           .LE. EOL    ) 
     .                    .AND. ( ICHAR(ECHR) .EQ. ISPACE )  )

                  E = E + 1

                  IF ( E .LE. EOL ) THEN
                     ECHR = LIST(E:E)                  
                  END IF

               END DO
 
               IF ( E .LE. EOL ) THEN

                  IF (  INDEX( DELIMS, ECHR ) .EQ. 0  ) THEN
C
C                    We're looking at a non-delimiter character.
C
C                    E is guaranteed to be > 1 if we're here, so the
C                    following subtraction is valid.
C
                     E = E - 1

                  END IF
 
               END IF

            END IF
 
C
C           The item now lies between B and E. Unless, of course, B and
C           E are the same character; this can happen if the list
C           starts or ends with a non-blank delimiter, or if we have
C           stumbled upon consecutive delimiters.
C
            IF ( .NOT. VALID ) THEN
C
C              If the array has not been validated, it's just an
C              array, and we can insert items directly into it.
C              Unless it's full, in which case we validate now and
C              insert later.
C
               IF ( N .LT. NMAX ) THEN
 
                  N = N + 1
 
                  IF ( E .GT. B ) THEN
                     SET(N) = LIST(B:E-1)
                  ELSE
                     SET(N) = BLANK
                  END IF
 
               ELSE
 
                  CALL VALIDC ( NMAX, NMAX, SET )
                  VALID = .TRUE.
 
               END IF
 
            END IF
 
C
C           Once the set has been validated, the strings are inserted
C           into the set if there's room. If there is not enough room
C           in the set, let INSRTC signal the error.
C
            IF ( VALID ) THEN
 
               IF ( E .GT. B ) THEN
                  CALL INSRTC ( LIST ( B:E-1 ),   SET )
               ELSE
                  CALL INSRTC ( BLANK,            SET )
               END IF
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'LPARSS' )
                  RETURN
               END IF
 
            END IF
 
C
C           If there are more items to be found, continue with the
C           character following E (which is a delimiter).
C
            B = E + 1
 
         END DO
 
C
C        If the array has not yet been validated, validate it before
C        returning.
C
         IF ( .NOT. VALID ) THEN
            CALL VALIDC ( NMAX, N, SET )
         END IF
 
C
C        If the list ended with a (non-blank) delimiter, insert a
C        blank item into the set. If there isn't any room, signal
C        an error.
C
         IF ( INDEX( DELIMS, LIST(EOL:EOL) ) .NE. 0 ) THEN
  
            CALL INSRTC ( BLANK, SET )
C
C           If INSRTC failed to insert the blank because the set
C           was already full, INSRTC will have signaled an error.
C           No action is necessary here.
C
         END IF

      END IF
 
 
      CALL CHKOUT ( 'LPARSS' )
      RETURN
      END
