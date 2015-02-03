C$Procedure ZZXLATEI ( Private --- Translate Integers )
 
      SUBROUTINE ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Convert integers from one binary file format to another.
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      INTEGER               INBFF
      CHARACTER*(*)         INPUT
      INTEGER               SPACE
      INTEGER               OUTPUT ( * )
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INBFF      I   Binary file format code for integers in INPUT.
C     INPUT      I   String containing integers read as characters.
C     SPACE      I   Number of integers that can be placed in OUTPUT.
C     OUTPUT     O   Translated integer values.
C
C$ Detailed_Input
C
C     INBFF      is an integer code that indicates the binary file
C                format of INPUT.  Acceptable values are the
C                parameters:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C                as defined in the include file 'zzddhman.inc'.
C
C     INPUT      is a string containing a group of integers read
C                from a file as a character string.  The length of
C                this string must be a multiple of the number of
C                bytes used to store an integer in a file utilizing
C                INBFF.
C
C     SPACE      is the number of integers that OUTPUT has room to
C                store.
C
C$ Detailed_Output
C
C     OUTPUT     is an array of integers containing the translated
C                values from INPUT into the native binary format.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     This routine signals several SPICE(BUG) exceptions.  They are
C     signaled when improperly specified inputs are passed into the
C     routine or if the module or modules in its calling tree are
C     improperly configured to run on this platform.  Callers that
C     prevent invalid inputs from being passed into this routine
C     need not check in.  See the $Restrictions section for a
C     discussion of input argument restrictions.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine translates integers from a non-native integer format
C     read from a file as a sequence of characters to the native format.
C
C$ Examples
C
C     See ZZDAFGFR, ZZDAFGSR.
C
C$ Restrictions
C
C     1) Numeric data when read as characters from a file preserve
C        the bit patterns present in the file in memory.
C
C     2) A byte is 8 bits, and a character is some multiple of
C        bytes.
C
C     3) The intrinsic ICHAR preserves the bit pattern of the character
C        byte read from a file.  Namely if one examines the integer
C        created the 8 least significant bits will be precisely those
C        found in the character.
C
C     4) The size of integers on the target environment are a multiple
C        of some number of bytes.
C
C     5) The length of the INPUT string is a multiple of the number
C        of bytes for an integer in the INBFF format.
C
C     6) INBFF is supported for reading on this platform, and not
C        equivalent to NATBFF on this platform.
C
C     7) This routine must support all of the non-native translations
C        required by the 'READS_BFF' key in ZZPLATFM.
C
C     8) The character label corresponding to INBFF must be one of the
C        non-native entries in the value of 'READS_BFF' returned by
C        ZZPLATFM for this environment.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               INTMIN
      INTEGER               ISRCHC
 
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C
C     These parameters are used for arithmetic shifting.
C
      INTEGER               SHFT8
      PARAMETER           ( SHFT8  = 2**8  )
 
      INTEGER               SHFT16
      PARAMETER           ( SHFT16 = 2**16 )
 
      INTEGER               SHFT24
      PARAMETER           ( SHFT24 = 2**24 )
 
C
C     Local Variables
C
      CHARACTER*(1)         CARG
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR
 
      INTEGER               BIGINT
      INTEGER               I
      INTEGER               J
      INTEGER               LENIPT
      INTEGER               NATBFF
      INTEGER               NUMINT
      INTEGER               OSIGN
      INTEGER               SMLINT
      INTEGER               VALUE
 
      LOGICAL               FIRST
 
C
C     Statement Functions
C
      INTEGER               ZZICHR
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  NATBFF
      SAVE                  STRBFF
      SAVE                  SMLINT
      SAVE                  BIGINT
 
C
C     Data Statements
C
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /
 
C
C     Statement Function Definitions
C
C     This function controls the conversion of characters to integers.
C     Some versions of the g77 implement ICHAR with a signed integer.
C     This function computes the value of ICHAR that this code requires
C     on any version of g77 for x86 Linux.
C
      ZZICHR(CARG) = ICHAR(CARG) - MAX( -1, MIN(0,ICHAR(CARG)) )*256
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZXLATEI' )
      END IF
 
C
C     Perform some initialization tasks.
C
      IF ( FIRST ) THEN
 
C
C        Populate STRBFF with the appropriate binary file
C        format labels.
C
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO
 
C
C        Fetch the native binary file format.
C
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )
 
         NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )
 
         IF ( NATBFF .EQ. 0 ) THEN
 
            CALL SETMSG ( 'The binary file format, ''#'', is not '
     .      //            'supported by this version of the toolkit. '
     .      //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZXLATEI'                                   )
            RETURN
 
         END IF
 
 
C
C        Store the largest value a 32-bit integer can actually
C        hold.
C
         BIGINT = 2147483647
 
C
C        Prepare the smallest value a 32-bit integer can actually
C        store, regardless of what INTMIN returns.
C
         SMLINT = INTMIN()
 
C
C        Set SMLINT to the appropriate value if INTMIN is too large.
C
         IF ( SMLINT .EQ. -2147483647 ) THEN
            SMLINT = SMLINT - 1
         END IF
 
C
C        Do not perform initialization tasks again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check to see if INBFF is valid.  This should never occur if this
C     routine is called properly.
C
      IF ( ( INBFF .LT. 1 ) .OR. ( INBFF .GT. NUMBFF ) ) THEN
 
         CALL SETMSG ( 'The integer code used to indicate the '
     .   //            'binary file format of the input integers, '
     .   //            '#, is out of range.  This error should '
     .   //            'never occur.'                               )
         CALL ERRINT ( '#', INBFF                                   )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'ZZXLATEI'                                   )
         RETURN
 
      END IF
 
C
C     Retrieve the length of the input string.
C
      LENIPT = LEN(INPUT)
 
C
C     Now branch based on the value of NATBFF.
C
      IF ( NATBFF .EQ. BIGI3E ) THEN
 
         IF ( INBFF .EQ. LTLI3E ) THEN
 
C
C           Check to see that the length of the input string is
C           appropriate.  Since this is a string containing LTL-IEEE
C           integers and this is a BIG-IEEE machine, characters are
C           1-byte and integers are 4-bytes.  So the length of INPUT
C           must be a multiple of 4.
C
            NUMINT = LENIPT / 4
 
            IF ( LENIPT - ( NUMINT*4 ) .NE. 0 ) THEN
 
               CALL SETMSG ( 'The input string that is to be '
     .         //            'translated from the binary format '
     .         //            '# to format # has a length that is '
     .         //            'not a multiple of 4 bytes.  This '
     .         //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(INBFF)                    )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'ZZXLATEI'                            )
               RETURN
 
            END IF
 
C
C           Verify there is enough room to store the results of
C           the translation.
C
            IF ( NUMINT .GT. SPACE ) THEN
 
               CALL SETMSG ( 'The caller specified that # integers '
     .         //            'are to be translated from binary '
     .         //            'format # to #.  However there is only '
     .         //            'room to hold # integers in the output '
     .         //            'array.  This error should never occur.' )
               CALL ERRINT ( '#', NUMINT                              )
               CALL ERRCH  ( '#', STRBFF(INBFF)                       )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'ZZXLATEI'                               )
               RETURN
 
            END IF
 
C
C           Start looping over each 4 character package in INPUT and
C           converting them to integers.
C
            DO I = 1, NUMINT
 
C
C              Compute the substring index of the first character
C              in INPUT for this integer.
C
               J = 4*(I-1) + 1
 
C
C              Now arrange the bytes properly.  Since these characters
C              were read from a file utilizing LTL-IEEE, we know that
C              J is the least significant byte and that (J+3) is the
C              most significant.
C
C              INPUT:
C
C                      -------------------------------------
C                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
C                      -------------------------------------
C
C              From this we construct OUTPUT(I) using the following
C              relation:
C
C                      INPUT(J:J)
C                      INPUT(J+1:J+1) shifted 8 bits to the MSb
C                      INPUT(J+2:J+2) shifted 16 bits to the MSb
C                   +  INPUT(J+3:J+3) shifted 24 bits to the MSb
C                   -------------------------
C                      OUTPUT(I)
C
C
C              Perform the necessary computations.  What is outlined
C              above is implemented below using arithmetic operations.
C              The last "shifted 24 bits to the MSb" is handled
C              in a special way, since the sign bit can not be shifted
C              into place through simple multiplication.
C
               VALUE     = ZZICHR( INPUT(J:J) )
               OUTPUT(I) = VALUE
 
               VALUE     = ZZICHR( INPUT((J+1):(J+1)) )
               VALUE     = VALUE*SHFT8
               OUTPUT(I) = OUTPUT(I)+VALUE
 
               VALUE     = ZZICHR( INPUT((J+2):(J+2)) )
               VALUE     = VALUE*SHFT16
               OUTPUT(I) = OUTPUT(I)+VALUE
 
               VALUE     = ZZICHR( INPUT((J+3):(J+3)) )
 
C
C              In order to properly install the last byte,
C              the sign bit needs to be managed separately.
               OSIGN = VALUE / 128
 
C
C              Strip the sign bit if necessary.
C
               IF ( OSIGN .EQ. 1 ) THEN
                  VALUE = VALUE - 128
               END IF
 
C
C              Shift the non-sign bits out to their appropriate
C              positions and combine them with OUTPUT(I).
C
               VALUE     = VALUE*SHFT24
               OUTPUT(I) = OUTPUT(I)+VALUE
 
C
C              Install the sign bit.  At the moment in OUTPUT(I)
C              we have the bits precisely as they need to be
C              arranged.  Perform the following computations:
C
C                 OUTPUT(I) = (2**31-1) - OUTPUT(I) + 1
C
C              Break this up into steps since 2**31 is not
C              representable with 32 bit integers that utilize
C              2's complement.
C
C              First negate the result:
C
C                 OUTPUT(I) = -OUTPUT(I)
C
C              But this negation is effectively:
C
C                 OUTPUT(I) = 2**32 - OUTPUT(I)
C
C              Which yields:
C
C                 2**32 - (2**31) + OUTPUT(I)
C
C              or
C
C                 2**31 + OUTPUT(I)
C
C              which is the desired quantity.  Note, 0 must be
C              treated as a special case.
C
               IF ( OSIGN .EQ. 1 ) THEN
                  IF ( OUTPUT(I) .EQ. 0 ) THEN
                     OUTPUT(I) = SMLINT
                  ELSE
                     OUTPUT(I) = BIGINT-OUTPUT(I)
                     OUTPUT(I) = OUTPUT(I) + 1
                     OUTPUT(I) = -OUTPUT(I)
                  END IF
               END IF
 
            END DO
 
         ELSE
 
            CALL SETMSG ( 'Unable to translate integers from '
     .      //            'binary file format # to #.  This error '
     .      //            'should never occur and is indicative '
     .      //            'of a bug.  Contact NAIF.'                )
            CALL ERRCH  ( '#', STRBFF(INBFF)                        )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                       )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'ZZXLATEI'                                )
            RETURN
 
         END IF
 
      ELSE IF ( NATBFF .EQ. LTLI3E ) THEN
 
         IF ( INBFF .EQ. BIGI3E ) THEN
 
C
C           Check to see that the length of the input string is
C           appropriate.  Since this is a string containing BIG-IEEE
C           integers and this is a LTL-IEEE machine, characters are
C           1-byte and integers are 4-bytes.  So the length of INPUT
C           must be a multiple of 4.
C
            NUMINT = LENIPT / 4
 
            IF ( LENIPT - ( NUMINT*4 ) .NE. 0 ) THEN
 
               CALL SETMSG ( 'The input string that is to be '
     .         //            'translated from the binary format '
     .         //            '# to format # has a length that is '
     .         //            'not a multiple of 4 bytes.  This '
     .         //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(INBFF)                    )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'ZZXLATEI'                            )
               RETURN
 
            END IF
 
C
C           Verify there is enough room to store the results of
C           the translation.
C
            IF ( NUMINT .GT. SPACE ) THEN
 
               CALL SETMSG ( 'The caller specified that # integers '
     .         //            'are to be translated from binary '
     .         //            'format # to #.  However there is only '
     .         //            'room to hold # integers in the output '
     .         //            'array.  This error should never occur.' )
               CALL ERRINT ( '#', NUMINT                              )
               CALL ERRCH  ( '#', STRBFF(INBFF)                       )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'ZZXLATEI'                               )
               RETURN
 
            END IF
 
C
C           Start looping over each 4 character package in INPUT and
C           converting them to integers.
C
            DO I = 1, NUMINT
 
C
C              Compute the substring index of the first character
C              in INPUT for this integer.
C
               J = 4*(I-1) + 1
 
C
C              Now arrange the bytes properly.  Since these characters
C              were read from a file utilizing BIG-IEEE, we know that
C              J is the most significant byte and that (J+3) is the
C              least significant.
C
C              INPUT:
C
C                      -------------------------------------
C                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
C                      -------------------------------------
C
C              From this we construct OUTPUT(I) using the following
C              relation:
C
C                      INPUT(J+3:J+3)
C                      INPUT(J+2:J+2)*SHFT8
C                      INPUT(J+1:J+1)*SHFT16
C                   +  INPUT(J:J)*SHFT24
C                   -------------------------
C                      OUTPUT(I)
C
C
C              Perform the necessary computations.  What is outlined
C              above is implemented below using arithmetic operations.
C              The last "shifted 24 bits to the MSb" is handled
C              in a special way, since the sign bit can not be shifted
C              into place through simple multiplication.
C
               VALUE     = ZZICHR( INPUT((J+3):(J+3)) )
               OUTPUT(I) = VALUE
 
               VALUE     = ZZICHR( INPUT((J+2):(J+2)) )
               VALUE     = VALUE*SHFT8
               OUTPUT(I) = OUTPUT(I)+VALUE
 
               VALUE     = ZZICHR( INPUT((J+1):(J+1)) )
               VALUE     = VALUE*SHFT16
               OUTPUT(I) = OUTPUT(I)+VALUE
 
               VALUE     = ZZICHR( INPUT(J:J) )
 
C
C              In order to properly install the last byte,
C              the sign bit needs to be managed separately.
C
               OSIGN = VALUE / 128
 
C
C              Strip the sign bit if necessary.
C
               IF ( OSIGN .EQ. 1 ) THEN
                  VALUE = VALUE - 128
               END IF
 
C
C              Shift the non-sign bits out to their appropriate
C              positions and combine them with OUTPUT(I).
C
               VALUE     = VALUE*SHFT24
               OUTPUT(I) = OUTPUT(I)+VALUE
 
C
C              Install the sign bit.  At the moment in OUTPUT(I)
C              we have the bits precisely as they need to be
C              arranged.  Perform the following computations:
C
C                 OUTPUT(I) = (2**31-1) - OUTPUT(I) + 1
C
C              Break this up into steps since 2**31 is not
C              representable with 32 bit integers that utilize
C              2's complement.
C
C              First, negate the result:
C
C                 OUTPUT(I) = -OUTPUT(I)
C
C              But this negation is effectively:
C
C                 OUTPUT(I) = 2**32 - OUTPUT(I)
C
C              Which yields:
C
C                 2**32 - (2**31) + OUTPUT(I)
C
C              or
C
C                 2**31 + OUTPUT(I)
C
C              which is the desired quantity.  Note, 0 must be
C              treated as a special case.
C
               IF ( OSIGN .EQ. 1 ) THEN
                  IF ( OUTPUT(I) .EQ. 0 ) THEN
                     OUTPUT(I) = SMLINT
                  ELSE
                     OUTPUT(I) = BIGINT-OUTPUT(I)
                     OUTPUT(I) = OUTPUT(I) + 1
                     OUTPUT(I) = -OUTPUT(I)
                  END IF
               END IF
 
            END DO
 
         ELSE
 
            CALL SETMSG ( 'Unable to translate integers from '
     .      //            'binary file format # to #.  This error '
     .      //            'should never occur and is indicative '
     .      //            'of a bug.  Contact NAIF.'                )
            CALL ERRCH  ( '#', STRBFF(INBFF)                        )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                       )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'ZZXLATEI'                                )
            RETURN
 
         END IF
 
C
C     The native binary file format on this platform is not supported
C     for the conversion of integers.  This is a bug, as this branch
C     of code should never be reached in normal operation.
C
      ELSE
 
         CALL SETMSG ( 'The native binary file format of this '
     .   //            'toolkit build, #, is not currently supported '
     .   //            'for translation of integers from non-native '
     .   //            'formats.'                                      )
         CALL ERRCH  ( '#', STRBFF(NATBFF)                             )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZXLATEI'                                      )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'ZZXLATEI' )
      RETURN
 
      END
