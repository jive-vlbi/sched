C$Procedure  REPMCT  ( Replace marker with cardinal text )
 
      SUBROUTINE REPMCT ( IN, MARKER, VALUE, CASE, OUT )
 
C$ Abstract
C
C     Replace a marker with the text representation of a
C     cardinal number.
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
C     CHARACTER
C     CONVERSION
C     STRING
C
C$ Declarations
 
      CHARACTER*(*)         IN
      CHARACTER*(*)         MARKER
      INTEGER               VALUE
      CHARACTER*1           CASE
      CHARACTER*(*)         OUT
 
      INTEGER               MAXLCN
      PARAMETER           ( MAXLCN = 145 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input string.
C     MARKER     I   Marker to be replaced.
C     VALUE      I   Cardinal value.
C     CASE       I   Case of replacement text.
C     OUT        O   Output string.
C     MAXLCN     P   Maximum length of a cardinal number.
C
C$ Detailed_Input
C
C     IN             is an arbitrary character string.
C
C     MARKER         is an arbitrary character string. The first
C                    occurrence of MARKER in the input string is
C                    to be replaced by the text representation of
C                    the cardinal number VALUE.
C
C                    Leading and trailing blanks in MARKER are NOT
C                    significant. In particular, no substitution is
C                    performed if MARKER is blank.
C
C     VALUE          is an arbitrary integer.
C
C     CASE           indicates the case of the replacement text.
C                    CASE may be any of the following:
C
C                       CASE     Meaning        Example
C                       ----     -----------    -----------------------
C                       U, u     Uppercase      ONE HUNDRED FIFTY-THREE
C
C                       L, l     Lowercase      one hundred fifty-three
C
C                       C, c     Capitalized    One hundred fifty-three
C
C$ Detailed_Output
C
C     OUT            is the string obtained by substituting the text
C                    representation of the cardinal number VALUE for
C                    the first occurrence of MARKER in the input string.
C
C                    OUT and IN must be identical or disjoint.
C
C$ Parameters
C
C     MAXLCN         is the maximum expected length of any cardinal
C                    text. 145 characters are sufficient to hold the
C                    text representing any value in the range
C
C                      ( -10**12, 10**12 )
C
C                    An example of a number whose text representation
C                    is of maximum length is
C
C                       - 777 777 777 777
C
C$ Exceptions
C
C     1) If OUT does not have sufficient length to accommodate the
C        result of the substitution, the result will be truncated on
C        the right.
C
C     2) If MARKER is blank, or if MARKER is not a substring of IN,
C        no substitution is performed. (OUT and IN are identical.)
C
C     3) If the value of CASE is not recognized, the error
C        SPICE(INVALIDCASE) is signalled. OUT is not changed.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is one of a family of related routines for inserting values
C     into strings. They are typically used to construct messages that
C     are partly fixed, and partly determined at run time. For example,
C     a message like
C
C        'Fifty-one pictures were found in directory [USER.DATA].'
C
C     might be constructed from the fixed string
C
C        '#1 pictures were found in directory #2.'
C
C     by the calls
C
C        CALL REPMCT ( STRING, '#1', NPICS,  'C', STRING )
C        CALL REPMC  ( STRING, '#2', DIRNAM,      STRING )
C
C     which substitute the cardinal text 'Fifty-one' and the character
C     string '[USER.DATA]' for the markers '#1' and '#2' respectively.
C
C     The complete list of routines is shown below.
C
C        REPMC    ( Replace marker with character string value )
C        REPMD    ( Replace marker with double precision value )
C        REPMF    ( Replace marker with formatted d.p. value )
C        REPMI    ( Replace marker with integer value )
C        REPMCT   ( Replace marker with cardinal text)
C        REPMOT   ( Replace marker with ordinal text )
C
C$ Examples
C
C     The following examples illustrate the use of REPMCT to
C     replace a marker within a string with the cardinal text
C     corresponding to an integer.
C
C     Uppercase
C     ---------
C
C        Let
C
C           MARKER = '#'
C           IN     = 'INVALID COMMAND.  WORD # WAS NOT RECOGNIZED.'
C
C        Then following the call,
C
C           CALL REPMCT ( IN, '#', 5, 'U', IN  )
C
C        IN is
C
C           'INVALID COMMAND.  WORD FIVE WAS NOT RECOGNIZED.'
C
C     Lowercase
C     ---------
C
C        Let
C
C           MARKER = ' XX '
C           IN     = 'Word XX of the XX sentence was misspelled.'
C
C        Then following the call,
C
C           CALL REPMCT ( IN, '  XX  ', 5, 'L', OUT )
C
C        OUT is
C
C           'Word five of the XX sentence was misspelled.'
C
C
C     Capitalized
C     -----------
C
C        Let
C
C           MARKER = ' XX '
C           IN     = 'Name:  YY.  Rank:  XX.'
C
C        Then following the calls,
C
C           CALL REPMC  ( IN,  'YY', 'Moriarty', OUT )
C           CALL REPMCT ( OUT, 'XX',     1, 'C', OUT )
C
C        OUT is
C
C           'Name:  Moriarty.  Rank:  One.'
C
C$ Restrictions
C
C     1) VALUE must be in the range accepted by subroutine INTTXT.
C        This range is currently
C
C           ( -10**12, 10**12 )
C
C        Note that the endpoints of the interval are excluded.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS)
C
C        Minor efficiency update: the routine now looks up the first
C        and last non-blank characters only once.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) (IMU)
C
C-&
 
C$ Index_Entries
C
C     replace marker with cardinal text
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
      LOGICAL               RETURN
 
C
C     Local variables
C
      CHARACTER*(MAXLCN)    CARD
      CHARACTER*(1)         TMPCAS
 
      INTEGER               MRKNBF
      INTEGER               MRKNBL

      INTEGER               MRKPSB
      INTEGER               MRKPSE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REPMCT' )
      END IF
 
C
C     Bail out if CASE is not recognized.
C
      CALL LJUST ( CASE,   TMPCAS )
      CALL UCASE ( TMPCAS, TMPCAS )
 
      IF (        ( TMPCAS .NE. 'U' )
     .      .AND. ( TMPCAS .NE. 'L' )
     .      .AND. ( TMPCAS .NE. 'C' )  ) THEN
 
         CALL SETMSG ( 'Case (#) must be U, L, or C.' )
         CALL ERRCH  ( '#', CASE                      )
         CALL SIGERR ( 'SPICE(INVALIDCASE)'           )
         CALL CHKOUT ( 'REPMCT'                       )
         RETURN
 
      END IF
 
C
C     If MARKER is blank, no substitution is possible.
C
      IF ( MARKER .EQ. ' ' ) THEN
 
         OUT = IN
 
         CALL CHKOUT ( 'REPMCT')
         RETURN
 
      END IF
 
C
C     Locate the leftmost occurrence of MARKER, if there is one
C     (ignoring leading and trailing blanks). If MARKER is not
C     a substring of IN, no substitution can be performed.
C
      MRKNBF = FRSTNB(MARKER)
      MRKNBL = LASTNB(MARKER)

      MRKPSB = INDEX ( IN, MARKER ( MRKNBF : MRKNBL )  )
 
      IF  ( MRKPSB .EQ. 0 ) THEN
 
         OUT = IN

         CALL CHKOUT ( 'REPMCT' )
         RETURN
 
      END IF

      MRKPSE = MRKPSB + MRKNBL - MRKNBF
 
C
C     Okay, CASE is recognized and MARKER has been found.
C     Generate the cardinal text corresponding to VALUE.
C
      CALL INTTXT ( VALUE, CARD )
 
C
C     CARD is always returned in upper case; change to the specified
C     case, if required.
C
      IF ( TMPCAS .EQ. 'L' ) THEN
 
         CALL LCASE ( CARD, CARD )
 
      ELSE IF ( TMPCAS .EQ. 'C' ) THEN
 
         CALL LCASE ( CARD(2:), CARD(2:) )
 
      END IF
 
C
C     Replace MARKER with CARD.
C
      CALL REPSUB ( IN,
     .              MRKPSB,
     .              MRKPSE,
     .              CARD ( : LASTNB(CARD) ),
     .              OUT                                       )
 
      CALL CHKOUT ( 'REPMCT' )
      RETURN
      END
