C$Procedure BODN2C ( Body name to ID code translation )
 
      SUBROUTINE BODN2C ( NAME, CODE, FOUND )
 
C$ Abstract
C
C    Translate the name of a body or object to the corresponding SPICE
C    integer ID code.
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
C
C$ Declarations
 
      CHARACTER*(*)         NAME
      INTEGER               CODE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Body name to be translated into a SPICE ID code.
C     CODE       O   SPICE integer ID code for the named body.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Maximum length of NAME string.
C
C$ Detailed_Input
C
C     NAME         is the name of a body or object, such as a planet,
C                  satellite, comet, asteroid, barycenter, DSN station,
C                  spacecraft, or instrument, that is "known" to the
C                  SPICE system, whether through hard-coded
C                  registration or run-time registration in the SPICE
C                  kernel pool.
C
C                 Case and leading and trailing blanks in a name
C                 are not significant.  However when a name is made
C                 up of more than one word, they must be separated by
C                 at least one blank.  That is, all of the following
C                 strings are equivalent names:
C
C                         'JUPITER BARYCENTER'
C                         'Jupiter Barycenter'
C                         'JUPITER BARYCENTER   '
C                         'JUPITER    BARYCENTER'
C                         '   JUPITER BARYCENTER'
C
C                 However, 'JUPITERBARYCENTER' is not equivalent to
C                 the names above.
C
C$ Detailed_Output
C
C     CODE        is the SPICE or user-defined integer ID code for the
C                 named body.
C
C     FOUND       is true if NAME has a translation.  Otherwise, FOUND
C                 is false.
C
C$ Parameters
C
C     MAXL        is the maximum allowable length of a body name.
C                 The value of this parameter may be found in the
C                 include file 'zzbodtrn.inc'.
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
C     BODN2C is one of five related subroutines,
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
C     Programmers writing user interface code should consider using the
C     SPICELIB routine BODS2C. BODS2C provides more flexibility in
C     handling input strings, since it accepts both body names and
C     strings representing integer ID codes, for example '399'.
C
C     Refer to NAIF_IDs for the list of name/code associations built
C     into SPICE, and for details concerning adding new name/code
C     associations at run time by loading text kernels.
C
C$ Examples
C
C     1.  In the following code fragment, BODVCD returns the radii
C         of Jupiter.  BODVCD requires the SPICE integer ID code for
C         Jupiter, so we use BODN2C to convert the name to
C         its corresponding integer ID code.
C
C            CALL BODN2C ( 'JUPITER', JUPID,  FOUND )
C
C            CALL BODVCD ( JUPID, 'RADII', 3, N, RADII )
C
C
C     2.  In this example, we assume that only the set of default 
C         name/code pairs has been defined. 
C
C         Given these names, BODN2C will return the following codes:
C
C            Name                         Code    Found?
C            ------------------------   ------    ------
C            'EARTH'                       399    Yes
C            '  Earth '                    399    Yes
C            'EMB'                           3    Yes
C            'Solar System Barycenter'       0    Yes
C            'SolarSystemBarycenter'         -    No
C            'SSB'                           0    Yes
C            'Voyager 2'                   -32    Yes
C            'U.S.S. Enterprise'             -    No
C            ' '                             -    No
C            'Halley's Comet'                -    No
C
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
C-    SPICELIB Version 1.0.8, 16-MAY-2009 (EDW) 
C
C        Edit to Particulars section to document the BODC2S routine.
C
C-    SPICELIB Version 1.0.7, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.0.6, 31-JAN-2008 (NJB)
C
C        References to the routine BODS2C were added to the header.
C
C-    SPICELIB Version 1.0.5, 24-OCT-2005 (NJB)
C
C        Header update:  changed references to BODVAR to references
C        to BODVCD.
C
C-    SPICELIB Version 1.0.4, 20-JUL-2004 (EDW)
C
C        Removed unneeded assignment of FOUND = .FALSE.
C
C-    SPICELIB Version 1.0.3, 29-JUL-2003 (NJB) (CHA)
C
C        Various header changes were made to improve clarity. Some
C        minor header corrections were made.
C
C-    SPICELIB Version 1.0.2, 26-AUG-2002 (FST)
C
C        Added discussion of MAXL to the parameters section.
C
C-    SPICELIB Version 1.0.1, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF.
C
C-    SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)
C
C        This was the BODN2C entry point from the original BODTRN
C        subroutine that was in the NAIF toolkit SUPPORT library.
C        When the private subroutine ZZBODTRN was added to SPICELIB,
C        superceding the BODTRN from SUPPORT, the body ID code/name
C        translation interface from the original BODTRN was moved to
C        SPICELIB so that ID codes did not have to be hard coded by
C        users of the Toolkit.
C
C        This subroutine simply calls the private subroutine ZZBODN2C
C        to perform its job.
C
C-&
 
C$ Index_Entries
C
C     body name to code
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'BODN2C' )
      END IF
 
      CALL ZZBODN2C ( NAME, CODE, FOUND )
C
C     No need for any error checking, since all we do is check out
C     and return anyway. We leave the error checking to the caller.
C
      CALL CHKOUT ( 'BODN2C' )
      RETURN
 
      END
