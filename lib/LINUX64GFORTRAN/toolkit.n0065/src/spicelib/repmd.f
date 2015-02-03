C$Procedure  REPMD  ( Replace marker with double precision number )
 
      SUBROUTINE REPMD ( IN, MARKER, VALUE, SIGDIG, OUT )
 
C$ Abstract
C
C     Replace a marker with a double precision number.
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
      DOUBLE PRECISION      VALUE
      INTEGER               SIGDIG
      CHARACTER*(*)         OUT
 
      INTEGER               MAXLDP
      PARAMETER           ( MAXLDP = 23 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input string.
C     MARKER     I   Marker to be replaced.
C     VALUE      I   Replacement value.
C     SIGDIG     I   Significant digits in replacement text.
C     OUT        O   Output string.
C     MAXLDP     P   Maximum length of a DP number.
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
C     VALUE          is an arbitrary double precision number.
C
C     SIGDIG         is the number of significant digits with
C                    which VALUE is to be represented. SIGDIG
C                    must be greater than zero and less than 15.
C
C$ Detailed_Output
C
C
C     OUT            is the string obtained by substituting the text
C                    representation of VALUE for the first occurrence
C                    of MARKER in the input string.
C
C                    The text representation of VALUE is in scientific
C                    notation, having the number of significant digits
C                    specified by SIGDIG.  The representation of VALUE
C                    is produced by the routine DPSTR; see that routine
C                    for details concerning the representation of
C                    double precision numbers.
C
C                    OUT and IN must be identical or disjoint.
C
C$ Parameters
C
C     MAXLDP         is the maximum expected length of the text
C                    representation of a double precision number.
C                    23 characters are sufficient to hold any result
C                    returned by DPSTR. (See $Restrictions.)
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
C          IN = 'Invalid operation value.  The value was #.'
C
C        Then following the call,
C
C           CALL REPMD ( IN, '#', 5.0D1, 2, IN  )
C
C        IN is
C
C           'Invalid operation value.  The value was 5.0E+01.'
C
C
C     2. Let
C
C          IN = 'Left endpoint exceeded right endpoint.  The left
C                endpoint was: XX.  The right endpoint was: XX.'
C
C        Then following the call,
C
C           CALL REPMD ( IN, '  XX  ',  -5.2D-9, 3, OUT )
C
C         OUT is
C
C           'Left endpoint exceeded right endpoint.  The left
C            endpoint was: -5.20E-09.  The right endpoint was: XX.'
C
C
C     3. Let
C
C          IN = 'Invalid operation value.  The value was #.'
C
C        Then following the call
C
C           CALL REPMD ( IN, '#', 5.0D1, 100, IN  )
C
C        IN is
C
C            'Invalid operation value.  The value was
C             5.0000000000000E+01.'
C
C        Note that even though 100 digits of precision were requested,
C        only 14 were returned.
C
C
C     4. Let
C
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
C           CALL REPMD ( MSG, '&', SCORE, 4, MSG )
C
C        MSG is
C
C           'There are 23 routines that have a fair chance of
C            meeting your needs.  The maximum score was 4.665E+00.'
C
C$ Restrictions
C
C     1) The maximum number of significant digits returned is 14.
C
C     2) This routine makes explicit use of the format of the string
C        returned by DPSTR; should that routine change, substantial
C        work may be required to bring this routine back up to snuff.
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
C-    SPICELIB Version 1.2.0, 23-SEP-2013 (BVS)
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
C     replace marker with d.p. number
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
C
      CHARACTER*(MAXLDP)    SUBSTR
 
      INTEGER               MRKNBF
      INTEGER               MRKNBL

      INTEGER               MRKPSB
      INTEGER               MRKPSE
 
      INTEGER               SUBNBF
      INTEGER               SUBNBL
 
C
C     If MARKER is blank, no substitution is possible.
C
      IF  ( MARKER .EQ. ' ' ) THEN
 
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
C     Okay, MARKER is non-blank and has been found. Convert the
C     number to text, and substitute the text for the marker.
C
      CALL DPSTR  ( VALUE, SIGDIG, SUBSTR )
 
      SUBNBF = FRSTNB(SUBSTR)
      SUBNBL = LASTNB(SUBSTR)

      IF ( SUBNBF .NE. 0 .AND. SUBNBL .NE. 0 ) THEN
 
         CALL ZZREPSUB ( IN,
     .                   MRKPSB,
     .                   MRKPSE,
     .                   SUBSTR( SUBNBF:SUBNBL ),
     .                   OUT  )

      END IF
  
      RETURN
      END
