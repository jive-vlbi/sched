C$Procedure ZZBODKER ( Private --- Process Body-Name Kernel Pool Maps )
 
      SUBROUTINE ZZBODKER ( NAMES,  NORNAM, CODES,  NVALS,  EXTKER,
     .                      BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                      BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This routine processes the kernel pool vectors NAIF_BODY_NAME
C     and NAIF_BODY_CODE into the lists and hashes required by ZZBODTRN
C     to successfully compute code-name mappings.
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
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE               'zzbodtrn.inc'
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      CHARACTER*(MAXL)      NAMES  (          NROOM )
      CHARACTER*(MAXL)      NORNAM (          NROOM )
      INTEGER               CODES  (          NROOM )
      INTEGER               NVALS
      LOGICAL               EXTKER
      INTEGER               BNMLST (          NROOM )
      INTEGER               BNMPOL ( LBPOOL : NROOM )
      CHARACTER*(MAXL)      BNMNMS (          NROOM )
      INTEGER               BNMIDX (          NROOM )
      INTEGER               BIDLST (          NROOM )
      INTEGER               BIDPOL ( LBPOOL : NROOM )
      INTEGER               BIDIDS (          NROOM )
      INTEGER               BIDIDX (          NROOM )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAMES      O   Array of kernel pool assigned names.
C     NORNAM     O   Array of normalized kernel pool assigned names.
C     CODES      O   Array of ID codes for NAMES/NORNAM.
C     NVALS      O   Length of NAMES, NORNAM, and CODES arrays.
C     EXTKER     O   Logical indicating presence of kernel pool names.
C     BNMLST     O   Body name-based hash head node pointer list
C     BNMPOL     O   Body name-based hash node collision list
C     BNMNMS     O   Body name-based hash item list
C     BNMIDX     O   Body name-based hash index storage array 
C     BIDLST     O   Body ID-based hash head node pointer list
C     BIDPOL     O   Body ID-based hash node collision list
C     BIDIDS     O   Body ID-based hash item list
C     BIDIDX     O   Body ID-based hash index storage array
C     LBPOOL     P   Lower bound of hash pool arrays
C     MAXL       P   Maximum length of body name strings.
C     NROOM      P   Maximum length of kernel pool data vectors.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     NAMES     is the array of names extracted from the kernel pool
C               vector NAIF_BODY_NAME. This array is parallel to
C               NORNAM and CODES.
C
C     NORNAM    the array of names extracted from the kernel pool
C               vector NAIF_BODY_NAME.  After extraction, each entry is
C               converted to uppercase, and groups of spaces are
C               compressed to a single space. This represents the
C               canonical member of the equivalence class each parallel
C               entry in NAMES belongs.
C
C     CODES     the array of codes extracted from the kernel pool
C               vector NAIF_BODY_CODE.  This array is parallel to NAMES
C               and NORNAM.
C
C     NVALS     the number of items contained in NAMES, NORNAM, and
C               CODES.
C
C     EXTKER    is a logical that indicates to the caller whether any
C               kernel pool name-code maps have been defined. If EXTKER
C               is .FALSE., then the kernel pool variables
C               NAIF_BODY_CODE and NAIF_BODY_NAME are empty and only
C               the built-in and ZZBODDEF code-name mappings need
C               consideration. If .TRUE., then the values returned by
C               this module need consideration.
C
C     BNMLST
C     BNMPOL
C     BNMNMS    are the body name-based hash head node pointer, node
C               collision, and item lists. Together they return the
C               index of the element in the BNMIDX index storage array
C               that stores the index of the body items in the NAMES,
C               NORNAM, and CODES arrays.
C
C     BNMIDX    is the body name-based hash index storage array
C               containing at the index determined by the hash for a
C               given normalized name the index corresponding to this
C               name in the NAMES, NORNAM, and CODES arrays.
C
C     BIDLST
C     BIDPOL
C     BIDIDS    are the body ID-based hash head node pointer, node
C               collision, and item lists. Together they return the
C               index of the element in the BNMIDX index storage array
C               that stores the index of the body items in the
C               NAMES, NORNAM, and CODES arrays.
C
C     BIDIDX    is the body ID-based hash index storage array
C               containing at the index determined by the hash for a
C               given ID the index corresponding to this ID in the
C               NAMES, NORNAM, and CODES arrays.
C
C$ Parameters
C
C     LBPOOL    is the lower bound of the hashes' collision list array.
C
C     MAXL      is the maximum length of a body name.  Defined in the
C               include file 'zzbodtrn.inc'.
C
C     NROOM     is the maximum number of kernel pool data items that
C               can be processed from the NAIF_BODY_CODE and
C               NAIF_BODY_NAME lists.
C
C$ Exceptions
C
C     1) The error SPICE(MISSINGKPV) is signaled when one of the
C        NAIF_BODY_CODE and NAIF_BODY_NAME keywords is present in the
C        kernel pool and the other is not.
C
C     2) The error SPICE(KERVARTOOBIG) is signaled if one or both of
C        the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors
C        have a cardinality that exceeds NROOM.
C
C     3) The error SPICE(BADDIMENSIONS) is signaled if the cardinality
C        of the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors do
C        not match.
C
C     4) The error SPICE(BLANKNAMEASSIGNED) is signaled if an entry
C        in the NAIF_BODY_NAME kernel pool vector is a blank string.
C        ID codes may not be assigned to a blank string.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine examines the contents of the kernel pool, ingests
C     the contents of the NAIF_BODY_CODE and NAIF_BODY_NAME keywords,
C     and produces name/code lists and hashes that ZZBODTRN requires to
C     resolve code to name and name to code mappings.
C
C     The NAMES and CODES arrays stored all values provided in the
C     corresponding POOL variables. No attempt to remove duplicates,
C     change order, or do any other alterations to these arrays is made
C     by this routine.
C
C     The order of mapping in the NAMES, NORNAM, and CODES arrays
C     determines the priority, with the mapping with the lowest
C     priority being first and the mapping with the highest priority
C     being last.
C
C     If more than one entry with a particular normalized name is
C     present in the NORNAM array, only the latest entry is registered
C     in the name-based hash.
C
C     If more than one entry with a particular ID is present in the
C     CODES array, only the latest entry that maps to a not-yet
C     registered normalized name is registered in the ID-based hash.
C     Registering IDs only for not-yet registered names achieves masking
C     all IDs with the lower priority in cases when a single normalized
C     name maps to more than one ID.
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
C     B.V. Semenov   (JPL)
C     F.S. Turner    (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-SEP-2013 (BVS)
C
C        Changed routine's calling sequence by dropping name and ID
C        order vectors and adding name- and ID-based hashes and
C        modified it to initialize hashes instead of the order arrays.
C
C-    SPICELIB Version 1.0.0, 23-AUG-2002 (EDW) (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Local Parameters
C
      INTEGER               KEYLEN
      PARAMETER           ( KEYLEN = 32 )
 
C
C     Local Variables
C
      CHARACTER*(KEYLEN)    NBC
      CHARACTER*(KEYLEN)    NBN
      CHARACTER*(1)         TYPE   ( 2 )
 
      INTEGER               I
      INTEGER               NSIZ   ( 2 )
      INTEGER               NUM    ( 2 )
 
      LOGICAL               FOUND
      LOGICAL               PLFIND ( 2 )
 
C
C     Saved Variables
C
      SAVE                  NBC
      SAVE                  NBN
 
C
C     Data Statements
C
      DATA                  NBC    / 'NAIF_BODY_CODE' /
      DATA                  NBN    / 'NAIF_BODY_NAME' /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZBODKER' )
      END IF
 
C
C     Until the code below proves otherwise, we shall assume
C     we lack kernel pool name/code mappings.
C
      EXTKER = .FALSE.
 
C
C     Check for the external body ID variables in the kernel pool.
C
      CALL GCPOOL ( NBN, 1, NROOM, NUM(1), NAMES, PLFIND(1) )
      CALL GIPOOL ( NBC, 1, NROOM, NUM(2), CODES, PLFIND(2) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
      END IF
 
C
C     Examine PLFIND(1) and PLFIND(2) for problems.
C
      IF ( PLFIND(1) .NEQV. PLFIND(2) ) THEN
 
C
C        If they are not both present or absent, signal an error.
C
         CALL SETMSG ( 'The kernel pool vector, #, used in '
     .   //            'mapping between names and ID-codes '
     .   //            'is absent, while # is not.  This is '
     .   //            'often due to an improperly constructed '
     .   //            'text kernel.  Check loaded kernels for '
     .   //            'these keywords.'                         )
 
         IF ( PLFIND(1) ) THEN
            CALL ERRCH ( '#', NBC )
            CALL ERRCH ( '#', NBN )
         ELSE
            CALL ERRCH ( '#', NBN )
            CALL ERRCH ( '#', NBC )
         END IF
 
         CALL SIGERR ( 'SPICE(MISSINGKPV)' )
         CALL CHKOUT ( 'ZZBODKER'          )
         RETURN
 
      ELSE IF ( .NOT. PLFIND(1) ) THEN
 
C
C        Return if both keywords are absent.
C
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
 
      END IF
 
C
C     If we reach here, then both kernel pool variables are present.
C     Perform some simple sanity checks on their lengths.
C
      CALL DTPOOL ( NBN, FOUND, NSIZ(1), TYPE(1) )
      CALL DTPOOL ( NBC, FOUND, NSIZ(2), TYPE(2) )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
      END IF
 
      IF ( ( NSIZ(1) .GT. NROOM ) .OR. ( NSIZ(2) .GT. NROOM ) ) THEN
 
         CALL SETMSG ( 'The kernel pool vectors used to '
     .   //            'define the names/ID-codes mapping'
     .   //            'exceeds the max size. The size of '
     .   //            'the NAME vector is #1. The size of '
     .   //            'the CODE vector is #2. The max '
     .   //            'number allowed of elements is #3.'   )
         CALL ERRINT ( '#1', NSIZ(1)                         )
         CALL ERRINT ( '#2', NSIZ(2)                         )
         CALL ERRINT ( '#3', NROOM                           )
         CALL SIGERR ( 'SPICE(KERVARTOOBIG)'                 )
         CALL CHKOUT ( 'ZZBODKER'                            )
         RETURN
 
      ELSE IF ( NSIZ(1) .NE. NSIZ(2) ) THEN
 
         CALL SETMSG ( 'The kernel pool vectors used for '
     .   //            'mapping between names and ID-codes '
     .   //            'are not the same size.  The size '
     .   //            'of the name vector, NAIF_BODY_NAME '
     .   //            'is #. The size of the ID-code '
     .   //            'vector, NAIF_BODY_CODE is #. You '
     .   //            'need to examine the ID-code kernel '
     .   //            'you loaded and correct the mismatch.' )
         CALL ERRINT ( '#', NSIZ(1)                           )
         CALL ERRINT ( '#', NSIZ(2)                           )
         CALL SIGERR ( 'SPICE(BADDIMENSIONS)'                 )
         CALL CHKOUT ( 'ZZBODKER'                             )
         RETURN
 
      END IF
 
C
C     Compute the canonical member of the equivalence class of NAMES,
C     NORNAM. This normalization compresses groups of spaces into a
C     single space, left justifies the string, and upper-cases the
C     contents.  While passing through the NAMES array, look for any
C     blank strings and signal an appropriate error.
C
      NVALS = NUM(1)
 
      DO I = 1, NVALS
 
C
C        Check for blank strings.
C
         IF ( NAMES(I) .EQ. ' ' ) THEN
 
            CALL SETMSG ( 'An attempt to assign the code, #, to '
     .      //            'a blank string was made.  Check loaded '
     .      //            'text kernels for a blank string in '
     .      //            'the NAIF_BODY_NAME array.'               )
            CALL ERRINT ( '#', I                                    )
            CALL SIGERR ( 'SPICE(BLANKNAMEASSIGNED)'                )
            CALL CHKOUT ( 'ZZBODKER'                                )
            RETURN
 
         END IF
 
C
C        Compute the canonical member of the equivalence class.
C
         CALL LJUCRS ( 1, NAMES(I), NORNAM(I) )
 
      END DO
 
C
C     Populate hashes required by ZZBODTRN.
C
      CALL ZZBODINI ( NAMES,  NORNAM, CODES,  NVALS,  NROOM,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZBODKER' )
         RETURN
      END IF


C
C     We're on the home stretch if we make it to this point. Set EXTKER
C     to .TRUE., check out and return.
C
      EXTKER = .TRUE.
 
      CALL CHKOUT ( 'ZZBODKER' )
      RETURN
 
      END
