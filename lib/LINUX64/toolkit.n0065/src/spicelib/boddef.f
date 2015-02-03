C$Procedure      BODDEF ( Body name/ID code definition )
 
      SUBROUTINE BODDEF ( NAME, CODE )
 
C$ Abstract
C
C     Define a body name/ID code pair for later translation via
C     BODN2C or BODC2N.
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
 
      CHARACTER*(*)         NAME
      INTEGER               CODE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAME       I   Common name of some body.
C     CODE       I   Integer code for that body.
C     MAXL       P   Maximum length of NAME string.
C
C$ Detailed_Input
C
C     NAME        is an arbitrary name of a body which could be
C                 a planet, satellite, barycenter, spacecraft,
C                 asteroid, comet, or other ephemeris object.
C
C                 The case and positions of blanks in a name are
C                 significant. BODC2N returns the same string
C                 (case and space) most recently mapped to a code.
C                 When NAME consists of more than one word, the
C                 words require separation by at least one blank.
C
C                 The kernel sub-system stores NAME as described in
C                 the BODDEF call, but creates an equivalence class
C                 based on NAME for comparisons in BODN2C. This class
C                 ignores leading/trailing whitespace, compresses
C                 interior whitespace to a single space, and ignores 
C                 character case.
C
C                 The following strings belong to the same equivalence 
C                 class:
C
C                         'JUPITER BARYCENTER'
C                         'Jupiter Barycenter'
C                         'JUPITER BARYCENTER   '
C                         'JUPITER    BARYCENTER'
C                         '   JUPITER BARYCENTER'
C
C                 However, 'JUPITERBARYCENTER' is distinct from
C                 the names above.
C
C                 When ignoring trailing blanks, NAME must be short
C                 enough to fit into the space defined by parameter
C                 MAXL.
C
C     CODE        is the integer ID code for assignment to body NAME.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     MAXL        is the maximum allowed length of a body NAME.
C                 Names exceeding this length will be truncated
C                 on assignment to a code with BODDEF.  The value
C                 of this parameter may be found in the include
C                 file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     1) Routines in the call tree of this routine may signal errors
C        if improper inputs are supplied, or if there is insufficient
C        room to store the requested addition.
C
C     2) If a name-code definition inserted into this routine seems to
C        have no effect, it is possible that the contents of the
C        definition are masked by the higher precedence kernel pool
C        assignments. See the "Particulars" section of this document
C        for more information.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     BODDEF is one of five related subroutines,
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
C     Modifying the SPICE name-ID mapping set
C     =======================================
C
C     Each body has a unique integer CODE, but may have several
C     names. Thus you may associate more than one name with
C     a particular integer code.
C
C     CODE may already have a name as defined by a previous
C     call to BODDEF or as part of the set of default
C     definitions.  That previous definition will remain,
C     and a translation of that name will still give the
C     same CODE.  However, future translations of CODE will
C     give the new NAME instead of the previous one.  This
C     feature is useful for assigning a more familiar or
C     abbreviated name to a body. For example, in addition
C     to the default name for body 5, 'JUPITER BARYCENTER',
C     you could define the abbreviation 'JB' to mean 5.
C
C     Note: In the case where BODDEF performs a name-to-ID mapping
C     assignment for an unused body name and unused ID value,
C     any subsequent assignment to NAME destroys the previous
C     mapping.
C
C        BODDEF( 'spud', 22)
C
C     then
C
C        BODDEF( 'spud', 23)
C
C     results in the state 'spud' maps to 23, 23 maps to 'spud',
C     and 22 maps to nothing (FOUND in BODC2N returns FALSE).
C
C$ Examples
C
C     You may associate a new name for a previously defined code:
C
C            CALL BODDEF ( 'JB', 5 )
C
C     You may also define the name and integer code for a new body:
C
C            CALL BODDEF ( 'Asteroid Frank', 20103456 )
C
C     After these calls to BODDEF, BODN2C would return the following
C     translations:
C
C        Name                         Code    Found?
C        ------------------------   ------    ------
C        'JB'                            5    Yes
C        'Jupiter Barycenter'            5    Yes
C        'ASTEROID FRANK'         20103456    Yes
C        'ASTEROIDFRANK'                 -    No
C        'Frank'                         -    No
C
C     and BODC2N will return these translations:
C
C        Code        Name                     Found?
C        -------     -------------------      ------
C               5    'JB'                     Yes
C        20103456    'Asteroid Frank'         Yes
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 16-MAY-2009 (EDW) 
C
C        Edit to Particulars section to document the BODC2S routine.
C
C-    SPICELIB Version 1.1.1, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.1.0, 23-JAN-2004 (EDW)
C
C        Rewrote header for clarity with regards to the
C        current capabilities of the kernel subsystem.
C
C-    SPICELIB Version 1.0.2, 26-AUG-2002 (FST)
C
C        Updated header to describe the parameter MAXL and
C        its effect on this module.  The exceptions section
C        was updated to include a more general discussion
C        of errors that routines in the call tree of this
C        routine may signal.
C
C-    SPICELIB Version 1.0.1, 12-AUG-2001 (EDW)
C
C        Updated header with information on new functionality.
C        The code-to-name retrieval routines now return the exact
C        string as defined in the last code/name mapping (case
C        and space).
C
C-    SPICELIB Version 1.0.0, 23-JAN-1996 (KRG)
C
C        This was the BODDEF entry point from the original BODTRN
C        subroutine that was in the NAIF toolkit SUPPORT library.
C        When the private subroutine ZZBODTRN was added to SPICELIB,
C        superceding the BODTRN from SUPPORT, the body ID code/name
C        translation interface from the original BODTRN was moved to
C        SPICELIB so that ID codes did not have to be hard coded by
C        users of the toolkit.
C
C        This subroutine simply calls the private subroutine ZZBODDEF
C        to perform its job.
C
C-&

C$ Index_Entries
C
C     body name/id code definition
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
         CALL CHKIN ( 'BODDEF' )
      END IF
 
      CALL ZZBODDEF ( NAME, CODE )

C
C     No need for any error checking, since all we do is check out
C     and return anyway. We leave the error checking to the caller.
C
      CALL CHKOUT ( 'BODDEF' )
      RETURN
 
      END
