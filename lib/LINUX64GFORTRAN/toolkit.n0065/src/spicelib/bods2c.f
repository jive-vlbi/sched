C$Procedure BODS2C ( Body string to ID code translation )
 
      SUBROUTINE BODS2C ( NAME, CODE, FOUND )
 
C$ Abstract
C
C     Translate a string containing a body name or ID code to an
C     integer code. 
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
C     NAIF_IDS
C
C$ Keywords
C
C     BODY
C     CONVERSION
C     ID
C     NAME
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         NAME
      INTEGER               CODE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   String to be translated to an ID code.
C     CODE       O   Integer ID code corresponding to NAME.
C     FOUND      O   Flag indicating whether translation succeeded.
C
C$ Detailed_Input
C
C     NAME        is a string containing the name or ID code of a
C                 body or object, such as a planet, satellite, comet,
C                 asteroid, barycenter, DSN station, spacecraft, or
C                 instrument.  
C
C                 If NAME contains the name of a body or object, that
C                 name must be "known" to the SPICE system, whether
C                 through hard-coded registration or run-time
C                 registration in the SPICE kernel pool.
C
C                 Case and leading and trailing blanks in a name are
C                 not significant.  However when a name is made up of
C                 more than one word, adjacent words must be separated
C                 by at least one blank.  That is, all of the following
C                 strings are equivalent names:
C
C                    'JUPITER BARYCENTER'
C                    'Jupiter Barycenter'
C                    'JUPITER BARYCENTER   '
C                    'JUPITER    BARYCENTER'
C                    '   JUPITER BARYCENTER'
C
C                 However, 'JUPITERBARYCENTER' is not equivalent to
C                 the names above.
C
C                 If NAME is a string representation of an integer,
C                 for example 
C
C                    '399'
C
C                 the string will be translated to the equivalent
C                 INTEGER datum.  The input integer need not be one
C                 recognized by the SPICE system:  the integer need not
C                 be a built-in NAIF ID code, nor need it be associated
C                 with a name via run-time registration.
C
C$ Detailed_Output
C
C     CODE        is, if NAME contains the name of a body or object,
C                 the corresponding NAIF or user-defined integer ID
C                 code, as determined by the SPICE name-code mapping
C                 subsystem. If NAME represents an integer, the same
C                 integer is returned in CODE.
C
C                 CODE is assigned a value only if FOUND is returned
C                 as .TRUE.; otherwise it is returned unchanged.
C
C
C     FOUND       is .TRUE. if NAME has a translation or represents an
C                 integer.  Otherwise, FOUND is .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     Body-name mappings may be defined at run time by loading text
C     kernels containing kernel variable assignments of the form
C
C        NAIF_BODY_NAME += ( <name 1>, ... )
C        NAIF_BODY_CODE += ( <code 1>, ... )
C
C     See NAIF_IDs for details.
C
C$ Particulars
C
C     BODS2C is one of five related subroutines,
C
C        BODS2C      Body string to code
C        BODC2S      Body code to string
C        BODN2C      Body name to code
C        BODC2N      Body code to name
C        BODDEF      Body name/code definition
C
C     BODS2C, BODC2S, BODN2C, and BODC2N perform translations between 
C     body names and their corresponding integer ID codes which are 
C     used in SPICE files and routines.
C
C     BODS2C is a slightly more general version of BODN2C: support
C     for strings containing ID codes in string format enables a caller
C     to identify a body using a string, even when no name is
C     associated with that body.
C
C     BODC2S is a general version of BODC2N; the routine returns either
C     the name assigned in the body ID to name mapping or a string
C     representation of the CODE value if no mapping exists.
C
C     BODDEF assigns a body name to ID mapping. The mapping has
C     priority in name-to-ID and ID-to-name translations.
C
C     Refer to NAIF_IDs for the list of name/code associations built
C     into SPICE, and for details concerning adding new name/code
C     associations at run time by loading text kernels.
C
C$ Examples
C
C     1.  In the following code fragment, BODEUL returns the Euler
C         angles representing the orientation of Jupiter relative to
C         the J2000 reference frame. BODEUL requires the NAIF integer
C         ID code for Jupiter, so we use BODS2C to convert the name to
C         its corresponding integer ID code.
C
C         We know Jupiter has a built-in name-code mapping, so we
C         needn't check the FOUND flag.
C          
C            CALL BODS2C ( 'JUPITER', JUPID, FOUND )
C
C            CALL BODEUL ( JUPID, ET, RA, DEC, W, LAMBDA )
C
C
C     2.  In this example, we assume that only the set of default 
C         name/code pairs has been defined. 
C
C         Given these names, BODS2C will return the following codes:
C
C            Name                             Code    Found?
C            ------------------------       ------    ------
C            'EARTH'                           399    Yes
C            '  Earth '                        399    Yes
C            '399'                             399    Yes
C            ' 399 '                           399    Yes
C            'EMB'                               3    Yes
C            '3'                                 3    Yes
C            '1000000000'               1000000000    Yes
C            'Solar System Barycenter'           0    Yes
C            'SolarSystemBarycenter'             -    No
C            'SSB'                               0    Yes
C            'Voyager 2'                       -32    Yes
C            'U.S.S. Enterprise'                 -    No
C            ' '                                 -    No
C            'Halley's Comet'                    -    No
C
C         Given these codes, BODC2N will return the following names:
C
C            Code        Name                        Found?
C            -------     -------------------         ------
C            399         'EARTH'                     Yes
C              0         'SOLAR SYSTEM BARYCENTER'   Yes
C              3         'EARTH BARYCENTER'          Yes
C            -77         'GALILEO ORBITER'           Yes
C             11          -                          No
C     1000000000          -                          No
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
C     C.H. Acton      (JPL)
C     N.J. Bachman    (JPL)
C     K.R. Gehringer  (JPL)
C     B.V. Semenov    (JPL)
C     F.S. Turner     (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 16-MAY-2009 (EDW) 
C
C        Edit to Particulars section to document the BODC2S routine.
C
C-    SPICELIB Version 1.0.1, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.0.0, 23-JUL-2003 (CHA) (NJB) (KRG) (FST) (EDW)
C
C        Based on SPICELIB Version 1.0.3, 29-JUL-2003 
C        (CHA) (NJB) (KEG) (FST) (EDW)
C
C-&
 
C$ Index_Entries
C
C     body string to code
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               BEINT
      LOGICAL               RETURN
      
C
C     Local parameters
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 1 )

C
C     Local variables
C
      CHARACTER*(MSGLEN)    ERRMSG
      INTEGER               PTR

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'BODS2C' )

C
C     Attempt to translate the input name to an integer code.  Call
C     the private routine ZZBODN2C to avoid additional CHKIN and
C     CHKOUT calls.
C 
      CALL ZZBODN2C ( NAME, CODE, FOUND )
      
      IF ( .NOT. FOUND ) THEN
C
C        It's possible the name is a string representation 
C        of an integer, for example, '999'.  If so, find
C        the equivalent datum of INTEGER type.
C      
         IF ( BEINT(NAME) ) THEN
C
C           The input conforms to the syntax of an integer, but it may
C           be outside of the range of the INTEGER data type.
C           Therefore we use the non-error-signaling routine NPARSI
C           rather than the cleaner PRSINT to attempt to convert the
C           string to an INTEGER.
C
            CALL NPARSI ( NAME, CODE, ERRMSG, PTR )

C
C           We have an ID code if and only if PTR is zero.
C
            FOUND = PTR .EQ. 0

         END IF
         
      END IF

C
C     FOUND is set.  CODE is set if NAME was a recognized name
C     or a string representation of an integer.
C

      CALL CHKOUT ( 'BODS2C' )
      RETURN 
      END
