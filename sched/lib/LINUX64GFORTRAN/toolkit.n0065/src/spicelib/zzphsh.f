C$Procedure ZZPHSH ( Private---kernel POOL hash function umbrella )
 
      INTEGER FUNCTION ZZPHSH ( WORD, M, M2 )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is an umbrella routine for the kernel POOL hash function.
C     It should never be called directly.
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
      CHARACTER*(*)         WORD
      INTEGER               M
      INTEGER               M2
 
C$ Brief_I/O
C
C     VARIABLE  I/O  Entry point
C     --------  ---  --------------------------------------------------
C     WORD       I   ZZHASH, ZZHASH2
C     M          I   ZZSHSH
C     M2         I   ZZHASH2
C
C     The function returns zero.
C
C$ Detailed_Input
C
C     See individual entry points.
C
C$ Detailed_Output
C
C     The function ZZPHSH should never be called. However, it returns
C     the value zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is an umbrella for the kernel POOL hash function
C     ZZHASH and its set up routine ZZSHSH, and for an arbitrary
C     divisor hash function ZZHASH2 that uses the same algorithm as
C     ZZHASH.
C
C     ZZSHSH and ZZHASH are intended to be used ONLY by the POOL
C     subsystem. ZZSHSH must be called once to save the POOL-specific
C     hash divisor and to initialize character-code map prior to the
C     first call to ZZHASH.
C
C     ZZHASH2 can be used with with an arbitrary divisor (recommended
C     to be a prime number) that is passed in as an input argument.
C     If ZZHASH2 is called prior to ZZSHSH, it does the same 
C     character-code map initialization.
C
C     The algorithm implemented in ZZHASH and ZZHASH2 is
C     case-insensitive and uses only the first word of the input
C     string. In order to make effective use of this hash algorithm,
C     input strings should be left-justified and space-less.
C
C$ Examples
C
C     To make use of the ZZHASH hash function the POOL subsystem first
C     calls ZZSHSH. The value returned by ZZSHSH has no meaning and can
C     bed assigned to any temporary variable. Then ZZHASH can be used
C     as needed by the POOL routine to compute hash value for the POOL
C     keywords.
C
C        I = ZZSHSH ( M )
C
C           ...any other set up code...
C
C        LOOKAT = ZZHASH ( WORD )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
C         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 31-JUL-2013 (BVS)
C
C        Added more details to the header. Added initialization of POOL
C        divisor to -1 to allow for initialization error checking on
C        the first call to ZZHASH.
C
C-    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS)
C
C        Replaced ICHAR('\\') expression with parameter
C        BSLASH, the parameter set to the ASCII value
C        of the backslash character, 92.
C
C-    SPICELIB Version 1.1.0, 14-SEP-2005 (EDW)
C
C        Added function ZZHASH2. Operation matches
C        that of ZZHASH with the exception that ZZHASH2
C        accepts the divisor value, M, as an input.
C
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&
 
C
C     Entry Points
C
      INTEGER               ZZSHSH
      INTEGER               ZZHASH
      INTEGER               ZZHASH2

C
C     SPICELIB functions
C
      INTEGER               INTMAX

C
C     Local Variables.
C
      INTEGER               I
      INTEGER               DIVISR
      INTEGER               VAL ( 0:128 )
      INTEGER               F
      INTEGER               BASE
      INTEGER               BLANK
      INTEGER               LENGTH
      INTEGER               MAXDIV

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )
 
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /

      DATA                  DIVISR / -1 /

C
C     We do not diagnose a bogus call since this is a private routine.
C
      ZZPHSH = 0

      RETURN
 

C$Procedure ZZSHSH ( Private---Set up POOL hash function )
 
      ENTRY ZZSHSH ( M )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine sets up the kernel POOL hash function. Call it once
C     per program execution prior to the first call to ZZHASH.
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
C
C     INTEGER               M
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M          I   Divisor used for the POOL hash function
C
C     The function returns 0.
C
C$ Detailed_Input
C
C     M           is the divisor of the hashing function. It is
C                 recommended that this be a prime number nominally
C                 equal to the maximum number of POOL variables. 
C                 The value of M must be in the range from 1 to
C                 INTMAX/68 - 1.
C
C$ Detailed_Output
C
C     The function returns the value zero (0).
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
C     This entry point saves the divisor used for hashing input
C     strings. It must be called once by an initialization branch of
C     the kernel POOL.
C
C     The character-code map initialized by this function is
C     case-insensitive.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This entry must NOT be called by any subsystem except the
C     kernel POOL.
C
C$ Literature_References
C
C     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
C         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 31-JUL-2013 (BVS)
C
C        Added more details to the header. Added computation of
C        the maximum allowed divisor value to the initialization
C        block. Added a check for the input divisor to be in the
C        allowed range.
C
C-    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS)
C
C        Replaced ICHAR('\\') expression with parameter
C        BSLASH, the parameter set to the ASCII value
C        of the backslash character, 92.
C
C-    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW)
C
C        Added punctuation marks to array of allowed
C        characters. The function can process any
C        character with ASCII decimal value 33 to 122.
C
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&
 
C
C     Return zero.
C
      ZZSHSH = 0

C
C     The initialization block below is identical to the initialization
C     block in the entry ZZHASH2. If this block is changed in any way,
C     the block in ZZHASH2 must be changed in the same way.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         BASE  = 68
         BLANK = ICHAR( ' ' )

         MAXDIV = INTMAX() / BASE - 1
 
         DO I = 0, 128
            VAL(I) = 0
         END DO
 
         VAL(ICHAR('0')) = 1
         VAL(ICHAR('1')) = 2
         VAL(ICHAR('2')) = 3
         VAL(ICHAR('3')) = 4
         VAL(ICHAR('4')) = 5
         VAL(ICHAR('5')) = 6
         VAL(ICHAR('6')) = 7
         VAL(ICHAR('7')) = 8
         VAL(ICHAR('8')) = 9
         VAL(ICHAR('9')) = 10
         VAL(ICHAR('A')) = 11
         VAL(ICHAR('B')) = 12
         VAL(ICHAR('C')) = 13
         VAL(ICHAR('D')) = 14
         VAL(ICHAR('E')) = 15
         VAL(ICHAR('F')) = 16
         VAL(ICHAR('G')) = 17
         VAL(ICHAR('H')) = 18
         VAL(ICHAR('I')) = 19
         VAL(ICHAR('J')) = 20
         VAL(ICHAR('K')) = 21
         VAL(ICHAR('L')) = 22
         VAL(ICHAR('M')) = 23
         VAL(ICHAR('N')) = 24
         VAL(ICHAR('O')) = 25
         VAL(ICHAR('P')) = 26
         VAL(ICHAR('Q')) = 27
         VAL(ICHAR('R')) = 28
         VAL(ICHAR('S')) = 29
         VAL(ICHAR('T')) = 30
         VAL(ICHAR('U')) = 31
         VAL(ICHAR('V')) = 32
         VAL(ICHAR('W')) = 33
         VAL(ICHAR('X')) = 34
         VAL(ICHAR('Y')) = 35
         VAL(ICHAR('Z')) = 36
         VAL(ICHAR('-')) = 37
         VAL(ICHAR('_')) = 38
         VAL(ICHAR('.')) = 39
         VAL(ICHAR('/')) = 40
         VAL(ICHAR('!')) = 41
         VAL(ICHAR('@')) = 42
         VAL(ICHAR('#')) = 43
         VAL(ICHAR('$')) = 44
         VAL(ICHAR('%')) = 45
         VAL(ICHAR('^')) = 46
         VAL(ICHAR('&')) = 47
         VAL(ICHAR('*')) = 48
         VAL(ICHAR('(')) = 49
         VAL(ICHAR(')')) = 50
         VAL(ICHAR('+')) = 51
         VAL(ICHAR('=')) = 52
         VAL(ICHAR('[')) = 53
         VAL(ICHAR('{')) = 54
         VAL(ICHAR(']')) = 55
         VAL(ICHAR('}')) = 56
         VAL(ICHAR('|')) = 57
         VAL(BSLASH    ) = 58
         VAL(ICHAR(':')) = 59
         VAL(ICHAR(';')) = 60
         VAL(ICHAR('<')) = 61
         VAL(ICHAR(',')) = 62
         VAL(ICHAR('>')) = 63
         VAL(ICHAR('?')) = 64

C
C        Note, ICHAR('''') returns the ASCII value for the single 
C        quote -> '.
C
         VAL(ICHAR(''''))= 65
         VAL(ICHAR('"')) = 66
         VAL(ICHAR('`')) = 67
         VAL(ICHAR('~')) = 68
 
         VAL(ICHAR('a')) = VAL(ICHAR('A'))
         VAL(ICHAR('b')) = VAL(ICHAR('B'))
         VAL(ICHAR('c')) = VAL(ICHAR('C'))
         VAL(ICHAR('d')) = VAL(ICHAR('D'))
         VAL(ICHAR('e')) = VAL(ICHAR('E'))
         VAL(ICHAR('f')) = VAL(ICHAR('F'))
         VAL(ICHAR('g')) = VAL(ICHAR('G'))
         VAL(ICHAR('h')) = VAL(ICHAR('H'))
         VAL(ICHAR('i')) = VAL(ICHAR('I'))
         VAL(ICHAR('j')) = VAL(ICHAR('J'))
         VAL(ICHAR('k')) = VAL(ICHAR('K'))
         VAL(ICHAR('l')) = VAL(ICHAR('L'))
         VAL(ICHAR('m')) = VAL(ICHAR('M'))
         VAL(ICHAR('n')) = VAL(ICHAR('N'))
         VAL(ICHAR('o')) = VAL(ICHAR('O'))
         VAL(ICHAR('p')) = VAL(ICHAR('P'))
         VAL(ICHAR('q')) = VAL(ICHAR('Q'))
         VAL(ICHAR('r')) = VAL(ICHAR('R'))
         VAL(ICHAR('s')) = VAL(ICHAR('S'))
         VAL(ICHAR('t')) = VAL(ICHAR('T'))
         VAL(ICHAR('u')) = VAL(ICHAR('U'))
         VAL(ICHAR('v')) = VAL(ICHAR('V'))
         VAL(ICHAR('w')) = VAL(ICHAR('W'))
         VAL(ICHAR('x')) = VAL(ICHAR('X'))
         VAL(ICHAR('y')) = VAL(ICHAR('Y'))
         VAL(ICHAR('z')) = VAL(ICHAR('Z'))
 
      END IF

C
C     Check and save divisor.
C
      IF ( M .LE. 0 .OR. M .GT. MAXDIV ) THEN

         CALL CHKIN  ( 'ZZSHSH'                                      )
         CALL SETMSG ( 'The input hash function divisor was not '    //
     .                 'in the allowed range from 1 to #. It was #.' )
         CALL ERRINT ( '#', MAXDIV                                   )
         CALL ERRINT ( '#', M                                        )
         CALL SIGERR ( 'SPICE(INVALIDDIVISOR)'                       )
         CALL CHKOUT ( 'ZZSHSH'                                      )
         RETURN

      END IF
 
      DIVISR = M

      RETURN
 

C$Procedure ZZHASH ( Private---POOL Hash function )
 
      ENTRY ZZHASH ( WORD )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine computes the hash value associated with a kernel
C     POOL variable name.
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
C
C     CHARACTER*(*)         WORD
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WORD       I   A left justified string of characters.
C
C     The function returns the hash value associated with WORD.
C
C$ Detailed_Input
C
C     WORD        is a left justified string of characters. Nominally
C                 this is the name of some kernel POOL variable.
C
C$ Detailed_Output
C
C     The function returns the hash value of WORD.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called prior to a call to ZZSPSH, the
C        error 'SPICE(CALLEDOUTOFORDER)' will be signaled. This should
C        never occur. In this case the function returns 0.
C
C     2) If this routine calculates a negative value, the error
C        SPICE(NEGATIVEHASHVALUE1) or SPICE(NEGATIVEHASHVALUE2) will be
C        signaled. This should never occur.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes the hash value of a string of characters.
C     The algorithm implemented by this function is case-insensitive and
C     uses only the first word of the input string. In order to make
C     effective use of this hash algorithm, input strings should be
C     left-justified and space-less. All non-left justified strings map
C     to the same value 0.
C
C$ Examples
C
C     See POOL.
C
C$ Restrictions
C
C     ZZSPSH must be called prior to calling this routine.
C
C$ Literature_References
C
C     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
C         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 31-JUL-2013 (BVS)
C
C        Added more details to the header. Add an exception for 
C        un-initialized divisor. Added one more exception for 
C        a negative output value.
C
C-    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW)
C
C        Added error test to catch non-positive hash values. 
C
C-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
C
C-&

C
C     Check if divisor was initialized by a prior call to ZZSHSH.
C
      IF ( DIVISR .EQ. -1 ) THEN

         ZZHASH = 0

         CALL CHKIN  ( 'ZZHASH'                                     )
         CALL SETMSG ( 'The ZZHASH function was called before the ' //
     .                 'POOL hash parameters were initialized by '  //
     .                 'a call to ZZSHSH.'                          )
         CALL SIGERR ( 'SPICE(CALLEDOUTOFORDER)'                    )
         CALL CHKOUT ( 'ZZHASH'                                     )
         RETURN

      END IF

C
C     Compute hash value for the input string.
C
      F      = 0
      LENGTH = LEN( WORD )
 
      DO I = 1, LENGTH
 
         IF ( ICHAR(WORD(I:I)) .EQ. BLANK ) THEN

            ZZHASH = MOD ( F*BASE, DIVISR ) + 1

C
C           A negative value for ZZHASH indicates a serious problem.
C
            IF( ZZHASH .LT. 0 ) THEN

               CALL CHKIN  ( 'ZZHASH'                             )
               CALL SETMSG ( 'The ZZHASH function calculated a '  //
     .                       'negative value for string $1. '     //
     .                       'Contact NAIF.'                      )
               CALL ERRCH  ( '$1', WORD                           )
               CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE1)'          )
               CALL CHKOUT ( 'ZZHASH'                             )

            END IF

            RETURN

         END IF
 
         F = VAL(  MIN( 128, ICHAR(WORD(I:I)) )  ) + F * BASE
         F = MOD( F, DIVISR )
 
      END DO
 
      ZZHASH = MOD( F*BASE, DIVISR ) + 1

C
C     A negative value for ZZHASH indicates a serious problem.
C
      IF( ZZHASH .LT. 0 ) THEN

         CALL CHKIN  ( 'ZZHASH'                              )
         CALL SETMSG ( 'The ZZHASH function calculated a '  //
     .                 'negative value for string $1. '     //
     .                 'Contact NAIF.'                       )
         CALL ERRCH  ( '$1', WORD                            )
         CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE2)'           )
         CALL CHKOUT ( 'ZZHASH'                              )

      END IF

      RETURN


C$Procedure ZZHASH2 ( Private---Arbitrary divisor hash function )

      ENTRY ZZHASH2 ( WORD, M2 )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine computes the hash value corresponding to an string
C     given a particular divisor value (M2).
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
C
C     CHARACTER*(*)         WORD
C     INTEGER               M2
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WORD       I   A left justified string of characters.
C     M2         I   Divisor used for the hash function
C
C     The function returns the hash value associated with WORD.
C
C$ Detailed_Input
C
C     WORD        is a left justified string of characters. 
C
C     M2          the divisor of the hashing function. This value
C                 defines the spread of the hash values, that spread
C                 covering the interval [1, M2]. The larger the M2
C                 value, the less the chance of a hash key collision.
C                 The user should always chose a prime for M2. The
C                 value of M2 must be in the range from 1 to INTMAX/68
C                 - 1. If it is not, the function signals an error and
C                 returns 0.
C
C$ Detailed_Output
C
C     The function returns the hash value of WORD as computed using M2
C     as the hash function divisor.
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
C     2) If this routine calculates a negative value, the error
C        SPICE(NEGATIVEHASHVALUE1) or SPICE(NEGATIVEHASHVALUE2) will be
C        signaled. This should never occur.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine computes the hash value of a string of characters
C     using the user specified hash divisor value. The algorithm
C     implemented by this function is case-insensitive and uses only
C     the first word of the input string. In order to make effective
C     use of this hash algorithm, input strings should be
C     left-justified and space-less. All non-left justified strings map
C     to the same value 0.
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
C     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
C         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 31-JUL-2013 (BVS)
C
C        Added more details to the header. Added computation of the
C        maximum allowed divisor value to the initialization block.
C        Added a check for the input divisor to be in the allowed
C        range. Added one more exception for a negative output value.
C
C-    SPICELIB Version 1.0.0, 14-SEP-2005 (EDW)
C
C-&

C
C     The initialization block below is identical to the initialization
C     block in the entry ZZSHSH. If this block is changed in any way,
C     the block in ZZSHSH must be changed in the same way.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         BASE  = 68
         BLANK = ICHAR( ' ' )
 
         MAXDIV = INTMAX() / BASE - 1
 
         DO I = 0, 128
            VAL(I) = 0
         END DO
 
         VAL(ICHAR('0')) = 1
         VAL(ICHAR('1')) = 2
         VAL(ICHAR('2')) = 3
         VAL(ICHAR('3')) = 4
         VAL(ICHAR('4')) = 5
         VAL(ICHAR('5')) = 6
         VAL(ICHAR('6')) = 7
         VAL(ICHAR('7')) = 8
         VAL(ICHAR('8')) = 9
         VAL(ICHAR('9')) = 10
         VAL(ICHAR('A')) = 11
         VAL(ICHAR('B')) = 12
         VAL(ICHAR('C')) = 13
         VAL(ICHAR('D')) = 14
         VAL(ICHAR('E')) = 15
         VAL(ICHAR('F')) = 16
         VAL(ICHAR('G')) = 17
         VAL(ICHAR('H')) = 18
         VAL(ICHAR('I')) = 19
         VAL(ICHAR('J')) = 20
         VAL(ICHAR('K')) = 21
         VAL(ICHAR('L')) = 22
         VAL(ICHAR('M')) = 23
         VAL(ICHAR('N')) = 24
         VAL(ICHAR('O')) = 25
         VAL(ICHAR('P')) = 26
         VAL(ICHAR('Q')) = 27
         VAL(ICHAR('R')) = 28
         VAL(ICHAR('S')) = 29
         VAL(ICHAR('T')) = 30
         VAL(ICHAR('U')) = 31
         VAL(ICHAR('V')) = 32
         VAL(ICHAR('W')) = 33
         VAL(ICHAR('X')) = 34
         VAL(ICHAR('Y')) = 35
         VAL(ICHAR('Z')) = 36
         VAL(ICHAR('-')) = 37
         VAL(ICHAR('_')) = 38
         VAL(ICHAR('.')) = 39
         VAL(ICHAR('/')) = 40
         VAL(ICHAR('!')) = 41
         VAL(ICHAR('@')) = 42
         VAL(ICHAR('#')) = 43
         VAL(ICHAR('$')) = 44
         VAL(ICHAR('%')) = 45
         VAL(ICHAR('^')) = 46
         VAL(ICHAR('&')) = 47
         VAL(ICHAR('*')) = 48
         VAL(ICHAR('(')) = 49
         VAL(ICHAR(')')) = 50
         VAL(ICHAR('+')) = 51
         VAL(ICHAR('=')) = 52
         VAL(ICHAR('[')) = 53
         VAL(ICHAR('{')) = 54
         VAL(ICHAR(']')) = 55
         VAL(ICHAR('}')) = 56
         VAL(ICHAR('|')) = 57
         VAL(BSLASH    ) = 58
         VAL(ICHAR(':')) = 59
         VAL(ICHAR(';')) = 60
         VAL(ICHAR('<')) = 61
         VAL(ICHAR(',')) = 62
         VAL(ICHAR('>')) = 63
         VAL(ICHAR('?')) = 64

C
C        Note, ICHAR('''') returns the ASCII value for the single 
C        quote -> '.
C
         VAL(ICHAR(''''))= 65
         VAL(ICHAR('"')) = 66
         VAL(ICHAR('`')) = 67
         VAL(ICHAR('~')) = 68
 
         VAL(ICHAR('a')) = VAL(ICHAR('A'))
         VAL(ICHAR('b')) = VAL(ICHAR('B'))
         VAL(ICHAR('c')) = VAL(ICHAR('C'))
         VAL(ICHAR('d')) = VAL(ICHAR('D'))
         VAL(ICHAR('e')) = VAL(ICHAR('E'))
         VAL(ICHAR('f')) = VAL(ICHAR('F'))
         VAL(ICHAR('g')) = VAL(ICHAR('G'))
         VAL(ICHAR('h')) = VAL(ICHAR('H'))
         VAL(ICHAR('i')) = VAL(ICHAR('I'))
         VAL(ICHAR('j')) = VAL(ICHAR('J'))
         VAL(ICHAR('k')) = VAL(ICHAR('K'))
         VAL(ICHAR('l')) = VAL(ICHAR('L'))
         VAL(ICHAR('m')) = VAL(ICHAR('M'))
         VAL(ICHAR('n')) = VAL(ICHAR('N'))
         VAL(ICHAR('o')) = VAL(ICHAR('O'))
         VAL(ICHAR('p')) = VAL(ICHAR('P'))
         VAL(ICHAR('q')) = VAL(ICHAR('Q'))
         VAL(ICHAR('r')) = VAL(ICHAR('R'))
         VAL(ICHAR('s')) = VAL(ICHAR('S'))
         VAL(ICHAR('t')) = VAL(ICHAR('T'))
         VAL(ICHAR('u')) = VAL(ICHAR('U'))
         VAL(ICHAR('v')) = VAL(ICHAR('V'))
         VAL(ICHAR('w')) = VAL(ICHAR('W'))
         VAL(ICHAR('x')) = VAL(ICHAR('X'))
         VAL(ICHAR('y')) = VAL(ICHAR('Y'))
         VAL(ICHAR('z')) = VAL(ICHAR('Z'))
 
      END IF

C
C     Check divisor.
C
      IF ( M2 .LE. 0 .OR. M2 .GT. MAXDIV ) THEN

         ZZHASH2 = 0

         CALL CHKIN  ( 'ZZHASH2'                                     )
         CALL SETMSG ( 'The input hash function divisor was not '    //
     .                 'in the allowed range from 1 to #. It was #.' )
         CALL ERRINT ( '#', MAXDIV                                   )
         CALL ERRINT ( '#', M2                                       )
         CALL SIGERR ( 'SPICE(INVALIDDIVISOR)'                       )
         CALL CHKOUT ( 'ZZHASH2'                                     )
         RETURN

      END IF
 
C
C     Compute hash value for the input string.
C
      F      = 0
      LENGTH = LEN( WORD )
 
      DO I = 1, LENGTH
 
         IF ( ICHAR(WORD(I:I)) .EQ. BLANK ) THEN

            ZZHASH2 = MOD ( F*BASE, M2 ) + 1

C
C           A negative value for ZZHASH2 indicates a serious problem.
C
            IF( ZZHASH2 .LT. 0 ) THEN

               CALL CHKIN  ( 'ZZHASH2'                            )
               CALL SETMSG ( 'The ZZHASH2 function calculated a ' //
     .                       'negative value for string $1. '     //
     .                       'Contact NAIF.'                      )
               CALL ERRCH  ( '$1', WORD                           )
               CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE1)'          )
               CALL CHKOUT ( 'ZZHASH2'                            )

            END IF

            RETURN

         END IF
 
         F = VAL(  MIN( 128, ICHAR(WORD(I:I)) )  ) + F * BASE
         F = MOD( F, M2 )
 
      END DO
 
      ZZHASH2 = MOD( F*BASE, M2 ) + 1

C
C     A negative value for ZZHASH2 indicates a serious problem.
C
      IF( ZZHASH2 .LT. 0 ) THEN

         CALL CHKIN  ( 'ZZHASH2'                             )
         CALL SETMSG ( 'The ZZHASH2 function calculated a ' //
     .                 'negative value for string $1. '     //
     .                 'Contact NAIF.'                       )
         CALL ERRCH  ( '$1', WORD                            )
         CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE2)'           )
         CALL CHKOUT ( 'ZZHASH2'                             )

      END IF

      RETURN
      END


