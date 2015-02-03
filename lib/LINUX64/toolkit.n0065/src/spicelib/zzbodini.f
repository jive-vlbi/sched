C$Procedure ZZBODINI ( Private --- Body-Code Hash Initialization )
 
      SUBROUTINE ZZBODINI (  NAMES,  NORNAM, CODES,  NVALS, MAXVAL,
     .                       BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                       BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize the name-based and ID-based hashes used for efficient
C     access to body-name mapping arrays. This routine should be called
C     by ZZBODTRN and ZZBODKER only.
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
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzbodtrn.inc'
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      CHARACTER*(MAXL)      NAMES  (          * )
      CHARACTER*(MAXL)      NORNAM (          * )
      INTEGER               CODES  (          * )
      INTEGER               NVALS
      INTEGER               MAXVAL
      INTEGER               BNMLST (          * )
      INTEGER               BNMPOL ( LBPOOL : * )
      CHARACTER*(MAXL)      BNMNMS (          * )
      INTEGER               BNMIDX (          * )
      INTEGER               BIDLST (          * )
      INTEGER               BIDPOL ( LBPOOL : * )
      INTEGER               BIDIDS (          * )
      INTEGER               BIDIDX (          * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAMES      I   Array of names
C     NORNAM     I   Array of normalized names
C     CODES      I   Array of ID codes for NAMES/NORNAM
C     NVALS      I   Length of NAMES, NORNAM, and CODES arrays
C     MAXVAL     I   Size of the hash arrays
C     BNMLST     O   Body name-based hash head node pointer list
C     BNMPOL     O   Body name-based hash node collision list
C     BNMNMS     O   Body name-based hash item list
C     BNMIDX     O   Body name-based hash index storage array 
C     BIDLST     O   Body ID-based hash head node pointer list
C     BIDPOL     O   Body ID-based hash node collision list
C     BIDIDS     O   Body ID-based hash item list
C     BIDIDX     O   Body ID-based hash index storage array
C     LBPOOL     P   Lower bound of hash pool arrays
C     MAXL       P   Maximum length of body name strings
C
C$ Detailed_Input
C
C     NAMES     is the array of body names. This array is parallel to
C               NORNAM and CODES.
C
C     NORNAM    is the array of normalized body names, made from
C               elements of NAMES by upper-casing, left-justifying, and
C               compressing groups of spaces to a single space. This
C               represents the canonical member of the equivalence
C               class each parallel entry in NAMES belongs.
C
C     CODES     is the array of body codes extracted. This array is
C               parallel to NAMES and NORNAM.
C
C     NVALS     is the number of items contained in NAMES, NORNAM,
C               CODES.
C
C     MAXVAL    is the output hash size.
C
C$ Detailed_Output
C
C     All output arrays must be declared with the dimension MAXVAL.
C     MAXVAL must be greater than or equal to NVALS.
C
C     BNMLST
C     BNMPOL
C     BNMNMS    are the body name-based hash head node pointer, node
C               collision, and item lists. Together they return the
C               index of the element in the BNMIDX index storage array
C               that stores the index of the body items in the input
C               storage arrays.
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
C               that stores the index of the body items in the input
C               storage arrays.
C
C     BIDIDX    is the body ID-based hash index storage array
C               containing at the index determined by the hash for a
C               given ID the index corresponding to the same ID in the
C               NAMES, NORNAM, and CODES arrays.
C
C$ Parameters
C
C     LBPOOL    is the lower bound of the hashes' collision list array.
C
C     MAXL      is the maximum length of a body name. Defined in the
C               include file 'zzbodtrn.inc'.
C
C$ Exceptions
C
C     1) If the input number of bodies NVALS is not less than or equal
C        to the size of the output hash, the error 'SPICE(BUG1)' will be
C        signaled.
C     
C     2) If registering an ID in the output ID-based hash fails, the
C        error 'SPICE(BUG2)' will be signaled.
C
C     3) If registering an name in the output name-based hash fails,
C        the error 'SPICE(BUG3)' will be signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine used for initializing the hashes
C     facilitating efficient body name-ID translation in ZZBODTRN.
C
C     The order of mapping in the input arrays determines the priority,
C     with the mapping with the lowest priority being first and the
C     mapping with the highest priority being last.
C
C     If more than one entry with a particular normalized name is
C     present in the input arrays, only the latest entry is registered
C     in the name-based hash.
C
C     If more than one entry with a particular ID is present in the
C     input arrays, only the latest entry that maps to a not-yet
C     registered normalized name is registered in the ID-based hash.
C     Registering IDs only for not-yet registered names achieves masking
C     all IDs with the lower priority in cases when a single normalized
C     name maps to more than one ID.
C     
C$ Examples
C
C     See the routine ZZBODTRN.
C
C$ Restrictions
C
C     1) This routine is intended only for use by ZZBODTRN and
C        ZZBODKER.
C
C     2) All output hash arrays must be declared with the same dimension
C        which is greater than or equal to MAXVAL.
C
C     3) The order of mappings in the input arrays determines the
C        priority, with the mapping with the lowest priority being
C        the first and the mapping with the highest priority being 
C        the last.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov       (JPL)
C     M.J. Spencer       (JPL)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    SPICELIB Version 4.0.0, 16-SEP-2013 (BVS)
C
C        Changed routine's calling sequence by dropping name and ID
C        order vectors and adding name- and ID-based hashes and
C        modified it to initialize hashes instead of the order arrays.
C
C-    SPICELIB Version 3.0.0, 23-AUG-2002 (FST)
C
C        Implemented changes to support the new precedence
C        system.
C
C        Altered the calling sequence of ZZBODINI to remove
C        unused arguments.  This routine also no longer computes
C        NORNAM from NAMES, since it is used in a more general
C        capacity.
C
C        Updated module header and comments to document additional
C        assumptions this module now makes about its inputs.
C
C        This routine is now error free.
C
C-    SPICELIB Version 2.1.1, 07-MAR-2002 (EDW)
C
C        Modified error logic to allow duplicate
C        NAME -> CODE mappings without signaling an error.
C        The mapping operation is a no-op, but might
C        cause a user problems if an error signals.
C
C-    SPICELIB Version 2.1.0, 12-AUG-2001 (EDW)
C
C        Modified logic for all ZZBOD routines to function with
C        equivalence class concept. A body name now exists
C        as a member of an equivalence class named by the
C        normalized form of the body name. To facilitate this
C        concept, an addition name vector, NORNAM, and
C        order vector, ORDNOM, now exist.
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT)
C
C        Renamed to ZZBODINI and filled out the comments on what this
C        routine does and how it works.
C
C-&
 
C
C     Local Variables
C
      INTEGER               I
      INTEGER               ITEM

      LOGICAL               NEW
 
C
C     Consistency check.
C
      IF ( MAXVAL .LT. NVALS ) THEN
         CALL CHKIN  ( 'ZZBODINI' )
         CALL SETMSG ( 'There is an inconsistency between the '  //
     .                 'number of input bodies and the size of ' //
     .                 'the output hashes. The number of input ' //
     .                 'bodies was #. The size of the output '   //
     .                 'hashes was #.' )
         CALL ERRINT ( '#', NVALS  )
         CALL ERRINT ( '#', MAXVAL )
         CALL SIGERR ( 'SPICE(BUG1)' )
         CALL CHKOUT ( 'ZZBODINI' )
         RETURN
      END IF

C
C     Initialize output hashes.
C
      CALL ZZHSIINI ( MAXVAL, BIDLST, BIDPOL )
      CALL ZZHSCINI ( MAXVAL, BNMLST, BNMPOL )

C
C     Loop through the input arrays to populate hashes. We do it
C     backwards to pick and register only the highest priority (latest)
C     values for each normalized name.
C
      DO I = NVALS, 1, -1 

C
C        Register this normalized name, but only if it is not already 
C        in the hash.
C
         CALL ZZHSCADD ( BNMLST, BNMPOL, BNMNMS, NORNAM(I), ITEM, NEW )
         IF ( NEW ) THEN
            IF ( ITEM .NE. 0 ) THEN
               BNMIDX( ITEM ) = I
            ELSE
               CALL CHKIN  ( 'ZZBODINI' )
               CALL SETMSG ( 'Could not add name # to the hash.' )
               CALL ERRCH  ( '#', NORNAM(I) )
               CALL SIGERR ( 'SPICE(BUG3)' )
               CALL CHKOUT ( 'ZZBODINI' )
            END IF
         END IF

C
C        We may have a situation when a single normalized name maps to
C        more than one ID. In such cases we want to completely mask all
C        IDs with the lower priority. This is easy to do by simply not
C        attempting to register any more IDs if the name is already
C        registered.
C
         IF ( NEW ) THEN

C
C           Register this ID, but only if it is not already in the hash.
C
            CALL ZZHSIADD ( BIDLST, BIDPOL, BIDIDS, CODES(I), ITEM, NEW)
            IF ( NEW ) THEN
               IF ( ITEM .NE. 0 ) THEN
                  BIDIDX( ITEM ) = I
               ELSE
                  CALL CHKIN  ( 'ZZBODINI' )
                  CALL SETMSG ( 'Could not add ID # to the hash.' )
                  CALL ERRINT ( '#', CODES(I) )
                  CALL SIGERR ( 'SPICE(BUG2)' )
                  CALL CHKOUT ( 'ZZBODINI' )
                  RETURN
               END IF
            END IF
         
         END IF
         
      END DO
 
      RETURN
      END
