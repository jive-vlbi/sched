C$Procedure      DPSTRE ( Double Precision Number to Character )
 
      SUBROUTINE DPSTRE ( X, SIGDIG, STRING )
 
C$ Abstract
C
C     Take a double precision number and convert it to an equivalent
C     character string representation (base 10).
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
C     PARSING
C
C$ Declarations
 
      IMPLICIT NONE

      DOUBLE PRECISION X
      INTEGER          SIGDIG
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X          I   A double precision number
C     SIGDIG     I   The number of significant digits placed in output
C     STRING     O   A character string representation of X
C
C$ Detailed_Input
C
C     X          is a double precision number.
C
C     SIGDIG     is the number of significant digits that are desired
C                for the output string.
C
C$ Detailed_Output
C
C
C     STRING     is a character representation of X to the number of
C                significant digits specified by SIGDIG.  The number of
C                spaces required to return the requested character
C                string is SIGDIG + 6.  If STRING is not declared to
C                have adequate length, the number returned will be
C                truncated on the right.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     If SIGDIG is less than one, this routine returns one significant
C     digit in the output string.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes an approximate character representation of
C     the input DP number X. The minimum number of significant digits
C     returned is 1. The maximum number of significant digits returned 
C     is 33.
C
C     For the numbers of significant digits less or equal to 14 this
C     routines calls DPSTR.
C
C     For the numbers of significant digits between 15 and 33 this
C     routines uses the following FORTRAN write statement
C
C         WRITE ( STRING, FMT=(1PEXX.YY) )
C
C     where XX = (SIGDIG + 6) and YY = (SIGDIG - 1).
C
C     For the numbers of significant digits greater than 33 this
C     routines uses the following FORTRAN write statement
C
C         WRITE ( STRING, FMT=(1PE39.32) )
C
C     as if the number of significant digits was 33.
C
C$ Examples
C
C     This example program prints PI with 3, 14, 17 and 22 significant
C     digits:
C
C           DOUBLE PRECISION      PI
C           CHARACTER*(80)     TEXT
C
C           CALL DPSTRE( PI(),  3, TEXT )
C           CALL TOSTDO( TEXT )
C           CALL DPSTRE( PI(), 14, TEXT )
C           CALL TOSTDO( TEXT )
C           CALL DPSTRE( PI(), 17, TEXT )
C           CALL TOSTDO( TEXT )
C           CALL DPSTRE( PI(), 22, TEXT )
C           CALL TOSTDO( TEXT )
C
C           END
C
C     When compiled with 32bit GFORTRAN on a Linux box it produces 
C     the following output:
C
C            3.14E+00
C            3.1415926535898E+00
C            3.14159265358979312E+00
C            3.141592653589793115998E+00
C
C$ Restrictions
C
C     The maximum number of significant digits returned is 33.
C
C     If the output string is not declared to be adequately large
C     (at least SIGDIG + 6), the numeric string will be truncated
C     on the right.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SUPPORT Version 1.0.0, 27-JAN-2012 (BVS)
C
C-&
 
C$ Index_Entries
C
C     d.p. number to character
C
C-&

C
C     Local parameters.
C
C     The maximum number of allowed significant digits is set to 33
C     (=16*2+1). This is an arbitrarily picked value because FORTRAN
C     doesn't seen to have a limit. But we do need a limit to make sure
C     that the formatted WRITE does not overflow the local buffer
C     string.
C
C
      INTEGER               MOSTDG
      PARAMETER           ( MOSTDG = 33 )

C
C     Format template.
C
      CHARACTER*(*)         FMTPIC
      PARAMETER           ( FMTPIC = '(1PE#.#)' )
 
C
C     Local variables
C
      INTEGER               IOSTAT
      INTEGER               MAXSIG
      INTEGER               MAXSAV
 
      CHARACTER*(10)        FMTSTR
      CHARACTER*(MOSTDG+7)  NUMSTR

      LOGICAL               FIRST

C
C     Saved variables.
C
      SAVE                  MAXSAV
      SAVE                  FIRST
      SAVE                  FMTSTR

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /
      DATA                  MAXSAV  / 14 /
      DATA                  FMTSTR  / '(1PE20.13)' /

C
C     Reset the input number of significant digits if it is outside of
C     the allowed range (1 to 33).
C
      MAXSIG = MIN ( MOSTDG, MAX( 1, SIGDIG ) )
 
C
C     If the number of significant digits is less then or equal to 14,
C     outsource conversion to DPSTR.
C
      IF ( MAXSIG .LE. 14 ) THEN

         CALL DPSTR( X, MAXSIG, STRING )

      ELSE

C
C        The number of significant digits is greater than 14. Make
C        output format. Do it only for the first call or if the
C        previous call had a different number of significant digits.
C        Otherwise, use the SAVEd format string from the previous 
C        call.
C
         IF ( FIRST .OR. MAXSIG .NE. MAXSAV ) THEN

            FMTSTR = FMTPIC
            CALL REPMI( FMTSTR, '#', MAXSIG+6, FMTSTR )
            CALL REPMI( FMTSTR, '#', MAXSIG-1, FMTSTR )

            MAXSAV = MAXSIG
            FIRST  = .FALSE.

         END IF

C
C        Use WRITE to create a temporary output string. This string is
C        declared to have enough room for any allowed numbers of
C        significant digits. We should not get any errors.
C
         WRITE ( NUMSTR, 
     .           FMTSTR,
     .           IOSTAT = IOSTAT ) X

C
C        This is fail safe check. Since we made the format string
C        ourselves and declared the output string with enough room we
C        should never hit it. But we do this check anyway, just in
C        case.
C
         IF ( IOSTAT .NE. 0 ) THEN

            CALL CHKIN  ( 'DPSTRE'                             )
            CALL SETMSG ( 'Bug. FORTRAN WRITE failed; number ' //
     .                    '= #; format = #; IOSTAT = #'        )
            CALL ERRDP  ( '#', X                               )
            CALL ERRCH  ( '#', FMTSTR                          )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(BUGWRITEFAILED)'              )
            CALL CHKOUT ( 'DPSTRE'                             )
            RETURN

         END IF

C
C        NOTE 1: should we also check for '*'s?
C
C        NOTE 2: should we check for 'E' in the string for cases of 
C                output FORTRAN WRITE for numbers greater than 1D100?
C                (In this case GFORTRAN leaves E out and prints 
C                pi*1D101 like this -3.14+101.)
C

C
C        Assign output string.
C
         STRING = NUMSTR

      END IF

      RETURN
 
      END
