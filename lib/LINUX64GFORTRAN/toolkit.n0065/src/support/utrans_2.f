C$Procedure      UTRANS_2 ( Translate Units To Default Units )
 
      SUBROUTINE UTRANS_2 ( STRING, PLACES )
      IMPLICIT NONE
 
C$ Abstract
C
C     This routine replaces quantities in STRING given in terms of UNITS
C     by the equivalent quantities given in terms of default units.
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
C$ Keywords
C
C      CHARACTERS,  CONVERSION, PARSING
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      DOUBLE PRECISION PLACES
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING    I/O  The input string before and after unit conversion.
C      PLACES     I   the number of significant figures in output values
C
C$ Detailed_Input
C
C      STRING     The input string before unit conversion.
C
C
C      PLACES     is the number of significant figures that will be
C                 used for the converted quantities.  The largest number
C                 that will be output is 14.  The number of characters
C                 actually used in the output number will be PLACES + 6
C                 for negative numbers, PLACES + 5 for positive numbers.
C
C$ Detailed_Output
C
C      STRING    the input string after unit conversion.
C
C$ Detailed_Description
C
C      This routine is supposed to help translate character strings
C      containing measurements in various units such as:
C
C          32.212 253.267 7628.7827 MILES  37683219.736 FEET
C
C      to character stings giving these measurements in terms of some
C      set of processing units.  For example in the case of the above
C      string, KM might be desirable:
C
C          5.184E+01 4.075937E+02 1.22773E+04 1.148584E+04
C
C      This example is intentded to be typical,  the units are left out
C      intentionally.  After all, this representation is intended to
C      be used only for internal processing by the application using
C      this routine.  After passing through this routine there should be
C      no question as to what units are associated with each of the
C      numeric strings.
C
C      To rigourously describe the function of this routine we need to
C      define a few terms.
C
C          A word within a string is a substring consisting entirely of
C          nonblank characters delimited by the ends of the string or
C          "white space" .
C
C          A numeric word is a word that can be successfully parsed
C          by the NAIF routine NPARSD  (all standard FORTRAN string
C          representations of numbers are numeric words).
C
C          A "measurement sequence" of words is a sequence of words in
C          the string that satisfies:
C
C             1.  the first word preceeding the sequence is a
C                 non-numeric word.
C
C             2.  the last word in the sequence is non-numeric
C                 and belongs to the collection of words given by
C                 the array UNITS. (UNITS would usually contain
C                 something like 'DEGREES', 'RADIANS', 'ARCSECONDS'.)
C
C             3.  All other words in the sequence are numeric and there
C                 is at least 1 numeric word.
C
C
C          The default sequence associated with each measurement
C          sequence is the sequence of numeric words obtained by
C          replacing each of the numeric words of the measurement
C          sequence by the product of that word and the value of
C          CONVERT associated with the unit of the measurement
C          sequence. The units of the measurement sequence are not
C          part of the associated default sequence.
C
C      Now that all of the terms have been described, the action of
C      this routine can be easily explained.  Given the input string
C      each measurement sequence is replaced by its associated
C      default sequence.  The numeric words in the associated default
C      sequences will be written in scientific notation with PLACES
C      significant digits.  The total number of characters needed for
C      each of the associated default sequence words is 6+PLACES
C
C$ Examples
C
C      Suppose that the input string is:
C
C      "LATITUDE: 32.2897 DEGREES    LONGITUDE: 45.28761 DEGREES
C       ALTITUDE: 100     FEET"
C
C      and that the arrays UNITS and CONVERT are given by:
C
C              UNITS    CONVERT
C           --------  --------
C           DEGREES   0.01745329   (conversion from degrees to radians)
C           MINUTES   0.00029088   (conversion from minutes to radians)
C           SECONDS   4.8481E-06   (conversion from seconds to radians)
C           FEET      0.30480061   (conversion from feet    to meteres)
C
C      then the output string will be:
C
C      "LATITUDE: 5.6356E-01   LONGITUDE: 7.38058E-01
C       ALTITUDE: 3.048E+01"
C
C
C$ Restrictions
C
C      The user should be sure that adequate space is available in
C      STRING to contain the translated string.
C
C      Also it is possible (even likely) that non-numeric words of
C      the STRING will be shifted from their original positions.
C      However, the order of the non-unit words will remain the same.
C
C$ Input_Files
C
C      None.
C
C$ Output_Files
C
C      None.
C
C$ Common_Variables
C
C      None.
C
C$ Author_and_Institution
C
C      W. L. Taber (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version_and_Date
C
C      Version 1, 26-JUN-1987
C
C-&
 
 
 
C
C     NAIFLIB functions
C
      LOGICAL               UNITP
      INTEGER               FRSTNB
      INTEGER               LASTNB
 
C
C     Local variables
C
      CHARACTER*(32)        DPNUM
      CHARACTER*(80)        MYERR
      CHARACTER*(127)       BASICS
 
      INTEGER               BEG
      INTEGER               BU
      INTEGER               END
      INTEGER               EU
      INTEGER               POINTR
      INTEGER               START
      INTEGER               F
      INTEGER               L
 
      LOGICAL               ERASED
      LOGICAL               MEASEQ
 
      DOUBLE PRECISION      X
      DOUBLE PRECISION      CONVERT
 
 
C
C     First thing, we left justify the command.
C
      CALL LJUST ( STRING, STRING )
 
      MEASEQ  = .FALSE.
      ERASED  = .FALSE.
 
C
C     Find the last word of the string.
C
      START = LEN ( STRING )   + 1
      CALL FNDPTK ( STRING, ' ', START, BEG, END )
 
      DO WHILE ( BEG .GT. 0 )
 
C
C        If we are in a measurement sequence, then we need to see if
C        the current word is a number.
C
         IF ( MEASEQ ) THEN
 
            MYERR = ' '
            CALL NPARSD ( STRING(BEG:END), X, MYERR, POINTR )
 
C
C           If no error occurred in the attempt to parse this number
C           the measurement sequence continues.
C
            IF ( MYERR .EQ. ' ' ) THEN
 
C
C              If we haven't already erased the current unit, do so
C              now and record our action.
C
               IF ( .NOT. ERASED ) THEN
 
                  STRING(BU:EU) = ' '
                  ERASED        = .TRUE.
 
               END IF
 
               STRING(BEG:END) = ' '
               X               = X * CONVERT
 
               CALL DPSTRF ( X,     PLACES, 'E', DPNUM        )
               CALL SIGDGT ( DPNUM,              DPNUM        )
               CALL PREFIX ( DPNUM, 1,           STRING(BEG:) )
 
C
C           If an error DID occur while attempting to parse the
C           current word, we are ending the current measurment
C           sequence.  However, we might be beginning another ...
C
            ELSE
 
C
C              ... search the list of recognized units for this word
C
C
               IF ( UNITP (STRING(BEG:END)) ) THEN
C                  WRITE (*,*) STRING(BEG:END)
                  BASICS = ' '
                  CALL TRANSU   (          STRING(BEG:END), BASICS )
                  F      = MAX(1,FRSTNB(BASICS) )
                  L      = MAX(1,LASTNB(BASICS) )
C                  WRITE (*,*) BASICS(F:L)
                  CALL CONVRT_2 ( 1.0D0,   STRING(BEG:END), BASICS(F:L),
     .                            CONVERT  )
                  MEASEQ =  .TRUE.
               ELSE
                  MEASEQ = .FALSE.
               END IF
 
C
C              ... if this word is on the list, record its place in the
C              string.
C
 
               IF ( MEASEQ ) THEN
 
                  BU     =  BEG
                  EU     =  END
 
C
C                 We haven't erased this unit from the string yet.
C                 Record this observation.
C
                  ERASED = .FALSE.
 
               END IF
 
            END IF
 
         ELSE
C
C           We were not in a measurment sequence, but we might be
C           starting one.  Search the list of known units for the
C           current word.
C
            IF ( UNITP (STRING(BEG:END)) ) THEN
C               WRITE (*,*) STRING(BEG:END)
               BASICS = ' '
               CALL TRANSU   (          STRING(BEG:END), BASICS )
               F      = MAX(1,FRSTNB(BASICS) )
               L      = MAX(1,LASTNB(BASICS) )
C               WRITE (*,*) BASICS(F:L)
               CALL CONVRT_2 ( 1.0D0,   STRING(BEG:END), BASICS(F:L),
     .                         CONVERT  )
               MEASEQ =  .TRUE.
            ELSE
               MEASEQ = .FALSE.
            END IF
 
 
            IF ( MEASEQ ) THEN
               BU = BEG
               EU = END
 
C
C              We certainly haven't erased this unit yet.
C
               ERASED = .FALSE.
 
            END IF
 
         END IF
 
C
C        Find the word previous to the current one.
C
         START = BEG
         CALL FNDPTK ( STRING, ' ', START, BEG, END )
 
      END DO
 
      RETURN
 
      END
