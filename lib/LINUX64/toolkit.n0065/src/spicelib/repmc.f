C$Procedure  REPMC  ( Replace marker with character string )
 
      SUBROUTINE REPMC ( IN, MARKER, VALUE, OUT )
 
C$ Abstract
C
C     Replace a marker with a character string.
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
      CHARACTER*(*)         VALUE
      CHARACTER*(*)         OUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input string.
C     MARKER     I   Marker to be replaced.
C     VALUE      I   Replacement string.
C     OUT        O   Output string.
C
C$ Detailed_Input
C
C     IN             is an arbitrary character string.
C
C     MARKER         is an arbitrary character string. The first
C                    occurrence of MARKER in the input string is
C                    to be replaced by VALUE.
C
C                    Leading and trailing blanks in MARKER are NOT
C                    significant. In particular, no substitution is
C                    performed if MARKER is blank.
C
C     VALUE          is an arbitrary character string.
C
C                    Leading and trailing blanks in VALUE are NOT
C                    significant: the portion of VALUE that is
C                    substituted for MARKER extends from its first
C                    non-blank character to its last non-blank
C                    character.
C
C                    However, if VALUE is blank, a single blank is
C                    substituted for the first occurrence of MARKER.
C
C$ Detailed_Output
C
C     OUT            is the string obtained by substituting VALUE
C                    (leading and trailing blanks excepted) for
C                    the first occurrence of MARKER in the input
C                    string.
C
C                    OUT and IN must be identical or disjoint.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C     1) If OUT does not have sufficient length to accommodate the
C        result of the substitution, the result will be truncated on
C        the right.
C
C     2) If MARKER is blank, or if MARKER is not a substring of IN,
C        no substitution is performed. (OUT and IN are identical.)
C
C     3) If VALUE is blank, a single blank is substituted for the
C        first occurrence of MARKER.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is one of a family of related routines for inserting values
C     into strings. They are typically to construct messages that
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
C        CALL REPMCT ( STRING, '#1', N_PICS,  'C', STRING )
C        CALL REPMC  ( STRING, '#2', DIR_NAME,     STRING )
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
C     1. Let
C
C           MARKER = '#'
C           IN     = 'Invalid operation value.  The value was:  #'
C
C        Then following the call,
C
C           CALL REPMC ( IN, '#', 'append', IN  )
C
C        IN is
C
C           'Invalid operation value.  The value was:  append'
C
C
C     2. Let
C
C           MARKER = ' XX '
C           IN     = 'A syntax error occurred.  The token XX was not
C                     recognized.  Did you mean to say XX?'
C
C        Then following the call,
C
C           CALL REPMC ( IN, '  XX  ', '  FND  ', OUT )
C
C        OUT is
C
C           'A syntax error occurred.  The token FND was not
C            recognized.  Did you mean to say XX?'
C
C
C     3. Let
C
C           MARKER = '&'
C           NUM    = 23
C           CHANCE = 'fair'
C           SCORE  = 4.665D0
C
C        Then following the sequence of calls,
C
C           CALL REPMI ( 'There are & routines that have a '  //
C          .             '& chance of meeting your needs.'    //
C          .             'The maximum score was &.',
C          .             '&',
C          .             NUM,
C          .             MSG  )
C
C           CALL REPMC ( MSG, '&', CHANCE, MSG )
C
C           CALL REPMF ( MSG, '&', SCORE, 4, 'F', MSG )
C
C        MSG is
C
C           'There are 23 routines that have a fair chance of
C            meeting your needs.  The maximum score was 4.665.'
C
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
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 21-SEP-2013 (BVS)
C
C        Minor efficiency update: the routine now looks up the first
C        and last non-blank characters only once.
C
C-    SPICELIB Version 1.1.0, 15-AUG-2002 (WLT)
C
C        The routine is now error free.
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
C     replace marker with character_string
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
 
C
C     Local variables
C
      INTEGER               MRKNBF
      INTEGER               MRKNBL

      INTEGER               MRKPSB
      INTEGER               MRKPSE
 
 
C
C     If MARKER is blank, no substitution is possible.
C
      IF ( MARKER .EQ. ' ' ) THEN
 
         OUT = IN
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
         RETURN
 
      END IF

      MRKPSE = MRKPSB + MRKNBL - MRKNBF
 
C
C     Okay, MARKER is non-blank and has been found. If VALUE is
C     blank, substitute a single blank. (This removes the marker.)
C     Otherwise substitute the non-blank portion.
C
      IF ( VALUE .EQ. ' ' ) THEN
 
         CALL ZZREPSUB ( IN,
     .                   MRKPSB,
     .                   MRKPSE,
     .                   ' ',
     .                   OUT  )
 
      ELSE
 
         CALL ZZREPSUB ( IN,
     .                   MRKPSB,
     .                   MRKPSE,
     .                   VALUE( FRSTNB(VALUE) : LASTNB(VALUE) ),
     .                   OUT  )
 
      END IF
 
      RETURN
      END
