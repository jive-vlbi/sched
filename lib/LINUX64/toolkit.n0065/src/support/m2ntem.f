C$Procedure      M2NTEM ( Parse the numeric template of a @number )
 
      SUBROUTINE M2NTEM ( STRING, BASE, BEG, END, A, B )
      IMPLICIT NONE
 
C$ Abstract
C
C     Parse the numeric template of a META/2 @numeric META-KEY.
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
C     META/2 a language specification language.
C
C$ Keywords
C
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         BASE
      INTEGER               BEG
      INTEGER               END
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A META/2 language statement specification.
C     BASE       I   Type of META-KEY associated with this template.
C     BEG       I/0  The beginning of the substring on input and output
C     END       I/0  The end of the substring on input and output
C     A          O   Lower value of the numeric restriction template
C     B          O   Upper value of the numeric restriction template
C
C$ Detailed_Input
C
C     STRING(BEG:END)  is a word in the META/2 language.  Is a META/2
C                      range restriction template.  It has the form
C                      (A:B) where A and B are both chracter strings
C                      representing numbers.
C
C     BASE             is a character string and should be '@int' or
C                      '@number'.
C
C$ Detailed_Output
C
C     BEG        On ouput BEG points to the first character following
C                the input value of END.
C
C     END        is returned unchanged.
C
C     A          is the value represented by the first numeric string
C                of the restriction template.  If a numeric string
C                is not present, A is not assigned the minimum possible
C                value associated with the data type given in BASE.
C
C     B          is the value represented by the second numeric string
C                of the restriction template (if there is a second
C                numeric string)  If no numeric string is present B is
C                assigned the maximum possible value associated with
C                the data type given in BASE.
C
C
C$ Error_Handling
C
C     None.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C      The range restriction template is part of the META/2 language
C      and is  described in the required reading section.  Briefly it
C      is a string at the beginning of a word that has the form
C
C      (A:B)
C
C      where A is a string representing a positive integer, and
C      B the null string or a string representing a positive integer
C      greater than A.
C
C      This routine determines if a range template is present and if so
C      what the values of A and B are.  If A (or B )is the null string
C      it is assumed to represent the smallest possible (largest
C      possible ) number of the type indicated by BASE.
C
C$ Examples
C
C      None.
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C     Version B1.0.0, 23-MAR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               INTMAX
      INTEGER               INTMIN
 
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
 
C
C     Local variables
C
      INTEGER               BEGIN
      INTEGER               LAST
      INTEGER               J
      INTEGER               K
 
      CHARACTER*80          ERROR
 
      DOUBLE PRECISION      MINVAL
      DOUBLE PRECISION      MAXVAL
 
      SAVE
 
 
      LAST  = END - 1
      BEGIN = BEG + 1
 
C
C     First get the min and max's for this kind of word.
C
      IF ( BASE .EQ. '@int' ) THEN
 
         MINVAL = DBLE ( INTMIN() )
         MAXVAL = DBLE ( INTMAX() )
 
      ELSE
 
         MINVAL = DPMIN()
         MAXVAL = DPMAX()
 
      END IF
 
C
C      parse the restriction template
C
      IF ( STRING(BEGIN:BEGIN) .EQ. ':' ) THEN
 
         A = MINVAL
         CALL NPARSD ( STRING(BEGIN+1:LAST), B, ERROR, J )
 
         IF ( ERROR .NE. ' ' ) THEN
            B = MAXVAL
         END IF
 
      ELSE IF ( STRING(LAST:LAST) .EQ. ':' ) THEN
 
         CALL NPARSD ( STRING(BEGIN:LAST-1), A, ERROR, J )
 
         IF ( ERROR .NE. ' ' ) THEN
            A = MINVAL
         END IF
 
         B = MAXVAL
 
      ELSE
 
         J = INDEX ( STRING(BEGIN:LAST) , ':' ) + BEG
 
         CALL NPARSD ( STRING(BEGIN:J-1), A, ERROR, K )
 
         IF ( ERROR .NE. ' ' ) THEN
            A = MINVAL
         END IF
 
         CALL NPARSD ( STRING(J+1  :LAST), B, ERROR, K )
 
         IF ( ERROR .NE. ' ' ) THEN
            B = MAXVAL
         END IF
 
      END IF
 
      BEG = END + 1
 
      RETURN
      END
