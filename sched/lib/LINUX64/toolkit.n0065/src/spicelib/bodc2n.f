C$Procedure      BODC2N ( Body ID code to name translation )
 
      SUBROUTINE BODC2N ( CODE, NAME, FOUND )
 
C$ Abstract
C
C     Translate the SPICE integer code of a body into a common name
C     for that body.
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
C
C$ Declarations
 
      INTEGER               CODE
      CHARACTER*(*)         NAME
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CODE       I   Integer ID code to be translated into a name.
C     NAME       O   A common name for the body identified by CODE.
C     FOUND      O   True if translated, otherwise false.
C     MAXL       P   Maximum length of NAME string.
C
C$ Detailed_Input
C
C     CODE        is an integer code for a body ---
C                 a planet, satellite, barycenter, spacecraft,
C                 asteroid, comet, or other ephemeris object.
C
C$ Detailed_Output
C
C     NAME        is a common name of the body identified by CODE.
C                 If CODE has more than one translation, then the
C                 most recently defined NAME corresponding to CODE
C                 is returned.  NAME will have the exact format (case
C                 and blanks) as when the name/code pair was defined.
C                 If the input value of CODE is not recognized, NAME
C                 will remain unchanged from its input value.
C
C     FOUND       is true if CODE has a translation.  Otherwise, FOUND
C                 is false.
C
C$ Parameters
C
C     MAXL        is the maximum allowable length of a body name.
C                 This amount of storage space should be declared
C                 to receive NAME, otherwise truncation may occur.
C                 The value of this parameter may be found in the
C                 include file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
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
C     Refer to NAIF_IDs for the list of name/code associations built
C     into SPICE, and for details concerning adding new name/code
C     associations at run time by loading text kernels.
C
C$ Examples
C
C     1.  Suppose you ran the utility program SPACIT to summarize
C         an SPK ephemeris file and the following data was output
C         to the terminal screen.
C
C            ----------------------------------------------------------
C            Segment identifier: JPL archive 21354
C            Body        : -77                         Center     : 399
C            From        : 1990 DEC 08 18:00:00.000
C            To          : 1990 DEC 10 21:10:00.000
C            Reference   : DE-200                      SPK Type    :1
C            ----------------------------------------------------------
C
C        You could write a program to translate the body codes
C        shown in the SPACIT output:
C
C           CALL BODC2N ( -77, BODY,   FOUND )
C           CALL BODC2N ( 399, CENTER, FOUND )
C
C           IF ( FOUND ) THEN
C
C              WRITE ( *,* ) 'BODY:    -77 = ', BODY
C              WRITE ( *,* ) 'CENTER:  399 = ', CENTER
C
C           END IF
C
C        You could also read the body and center codes directly from
C        the SPK files, using the appropriate DAF routines, and then
C        translate them, as above.
C
C
C     2.  In this example, we assume that BODDEF has not been called,
C         so only the set of default name/code pairs has
C         been defined.
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
C             -1          -                          No
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
C     K.R. Gehringer  (JPL)
C     B.V. Semenov    (JPL)
C     F.S. Turner     (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.4, 16-MAY-2009 (EDW) 
C
C        Edit to Particulars section to document the BODC2S routine.
C
C-    SPICELIB Version 1.0.3, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.0.2, 26-AUG-2002 (FST)
C
C        Added documentation discussing the parameter MAXL.
C
C-    SPICELIB Version 1.0.1, 01-DEC-1998 (WLT)
C
C        Added documentation that describes the output NAME if CODE
C        is not a recognized body ID.
C
C-    SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)
C
C        This was the BODC2N entry point from the original BODTRN
C        subroutine that was in the NAIF toolkit SUPPORT library.
C        When the private subroutine ZZBODTRN was added to SPICELIB,
C        superceding the BODTRN from SUPPORT, the body ID code/name
C        translation interface from the original BODTRN was moved to
C        SPICELIB so that ID codes did not have to be hard coded by
C        users of the toolkit.
C
C        This subroutine simply calls the private subroutine ZZBODC2N
C        to perform its job.
C
C-&
 
C$ Index_Entries
C
C     body id code to name
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
         CALL CHKIN ( 'BODC2N' )
      END IF
 
      CALL ZZBODC2N ( CODE, NAME, FOUND )
C
C     No need for any error checking, since all we do is check out
C     and return anyway. We leave the error checking to the caller.
C
      CALL CHKOUT ( 'BODC2N' )
      RETURN
 
      END
