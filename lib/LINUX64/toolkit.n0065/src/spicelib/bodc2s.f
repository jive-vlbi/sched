C$Procedure BODC2S ( Body ID code to string translation )

      SUBROUTINE BODC2S( CODE, NAME ) 

C$ Abstract
C
C     Translate a body ID code to either the corresponding name
C     or if no name to ID code mapping exists, the string 
C     representation of the body ID value.
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

      INTEGER               CODE
      CHARACTER*(*)         NAME
  
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CODE       I   Integer ID code to translate to a string.
C     NAME       O   String corresponding to CODE.
C
C$ Detailed_Input
C
C     CODE       the integer code for a body: planet, satellite, 
C                barycenter, spacecraft, asteroid, comet, or 
C                other ephemeris object.
C
C$ Detailed_Output
C
C     NAME       the string name of the body identified by CODE
C                if a mapping between CODE and a body name exists
C                within SPICE.
C
C                If CODE has more than one translation, then the
C                most recently defined NAME corresponding to CODE
C                is returned.  NAME will have the exact format (case
C                and blanks) as when the name/code pair was defined.
C
C                If the input value of CODE does not map to a body
C                name, NAME returns the string representation
C                of CODE.
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
C     See naif_ids.req for details.
C
C$ Particulars
C
C     BODS2N is one of five related subroutines,
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
C     Refer to naif_ids.req for the list of name/code associations built
C     into SPICE, and for details concerning adding new name/code
C     associations at run time by loading text kernels.
C
C$ Examples
C
C     Apply the BODC2S call to several IDs representing codes
C     included in the default SPICE ID-name lists and codes not
C     included in the list.
C
C           PROGRAM BODC2S_T
C     
C           INTEGER                CODE (7)
C           CHARACTER*(32)         NAME
C     
C     C
C     C     Assign an array of body IDs. Not all the listed IDS
C     C     map to a body name.
C     C
C           CODE(1) = 399
C           CODE(2) = 0
C           CODE(3) = 3
C           CODE(4) = -77
C           CODE(5) = 11
C           CODE(6) = -1
C           CODE(7) = 6000001
C     
C     C
C     C     Loop over the CODE array, call BODC2S for each
C     C     element of CODE.
C     C
C           DO I= 1, 7
C     
C              CALL BODC2S( CODE(I), NAME )
C     
C              WRITE(*, '(I8,3x,A)' ) CODE(I),  NAME
C     
C           END DO
C     
C           END 
C     
C     Given these codes, BODC2S returns the following NAME strings:
C
C            Code        Name
C            -------     -------------------  
C                399     'EARTH'                           
C                  0     'SOLAR SYSTEM BARYCENTER'         
C                  3     'EARTH BARYCENTER'
C                -77     'GALILEO ORBITER'                 
C                 11     '11'
C                 -1     'GEOTAIL'
C            6000001     '6000001'
C
C     The codes 11 and 6000001 did not map to a name so the call
C     returns as NAME the string expression of the codes.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 18-APR-2014 (BVS)
C
C        Minor header edits.
C
C-    SPICELIB Version 1.0.0, 10-APR-2010 (EDW)
C
C-&

C$ Index_Entries
C
C     body ID code to string
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
      LOGICAL               FOUND

C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'BODC2S' )

C
C     Fortran. No type check available for CODE. Bother.
C

C
C     Attempt to translate the input CODE to a name. Use
C     the private routine ZZBODC2N.
C 
      CALL ZZBODC2N ( CODE, NAME, FOUND )

      IF ( FOUND ) THEN

C
C        Success. CODE maps to NAME. Return.
C
         CALL CHKOUT ( 'BODC2S' )
         RETURN

      END IF

C
C     If execution reaches this level, the SPICE body ID
C     to name mapping lacks an assignment for CODE. Convert
C     CODE to a string representation of the integer value.
C
      CALL INTSTR ( CODE, NAME )

      CALL CHKOUT ( 'BODC2S' )
      RETURN

      END




