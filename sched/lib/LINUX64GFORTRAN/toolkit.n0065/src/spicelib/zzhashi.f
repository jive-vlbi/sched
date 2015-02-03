C$Procedure ZZHASHI ( Private---integer hash function )
 
      INTEGER FUNCTION ZZHASHI ( N, M )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This function returns the hash value corresponding to an integer.
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
C     PRIVATE UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INTEGER               N
      INTEGER               M
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     N          I   An integer number.
C     M          I   Divisor used for the hash function
C
C     The function returns the hash value associated with N.
C
C$ Detailed_Input
C
C     N           is an integer number. 
C
C     M           is the divisor of the hashing function. This value
C                 defines the spread of the hash values, that spread
C                 covering the interval [1, M]. M must be a positive
C                 number. If it is not, the function signals an error
C                 and returns 0.
C
C$ Detailed_Output
C
C     The function returns the hash value of N as computed using M
C     as the hash function divisor.
C
C     This function is sign-insensitive, i.e. it computes the same
C     hash value for A as is it does for -A.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input divisor value is not in the allowed range, the
C        error 'SPICE(INVALIDDIVISOR)' will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes the hash value of an integer number using
C     the user specified hash divisor value.
C
C$ Examples
C
C     None.
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
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 02-AUG-2013 (BVS)
C
C-&

C
C     Check divisor.
C
      IF ( M .LE. 0 ) THEN

         ZZHASHI = 0

         CALL CHKIN  ( 'ZZHASHI'                                     )
         CALL SETMSG ( 'The input hash function divisor was not '    //
     .                 'a positive number. It was #.'                )
         CALL ERRINT ( '#', M                                        )
         CALL SIGERR ( 'SPICE(INVALIDDIVISOR)'                       )
         CALL CHKOUT ( 'ZZHASHI'                                     )
         RETURN

      END IF

C
C     Use simple division method -- h(k) = k mod m.
C
      ZZHASHI = MOD(ABS(N),M) + 1

      RETURN
      END


