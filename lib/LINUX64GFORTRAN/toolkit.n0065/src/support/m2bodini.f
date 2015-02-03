C$Procedure      M2BODINI ()
 
      SUBROUTINE M2BODINI (  NAMES,
     .                       NNAM,
     .                       CODES,
     .                       NCOD,
     .                       ORDNAM,
     .                       ORDCOD )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Initialize the two order vectors. This routine should be called
C     by M2BODTRN only.
C
C     This routine can not graduate as it is without modifying the
C     specification of BSCHOI and BSCHOC. (WLT)
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
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)        NAMES  (*)
      INTEGER              CODES  (*)
      INTEGER              NNAM
      INTEGER              NCOD
      INTEGER              ORDNAM (*)
      INTEGER              ORDCOD (*)
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NAMES     I/O  Array of recognized names
C     CODES     I/O  Id-codes to associated with NAMES
C     NNAM      I/O  Number of names
C     NCOD      I/O  Number if id-codes
C     ORDNAM     O   An order vector for NAMES
C     ORDCOD     O   An ordered vector for CODES
C
C$ Detailed_Input
C
C     NAMES     is an array of names for whick there is an
C               id-code.
C
C     CODES     is an array of id-codes for the items in NAMES.  The
C               correspondence is: CODES(I) is the id-code of the body
C               named in NAMES(I)
C
C     NNAM      Number of names
C
C$ Detailed_Output
C
C     NCOD      is the number pointers in the ordered pointer array
C               ORDCOD
C
C     ORDNAM    is an order vector of integers for NAMES.  The set of
C               values NAMES(ORDNAM(1)), NAMES(ORDNAM(2),  ... forms
C               an increasing list of names.
C
C     ORDCOD    is an ordering array of integers (as opposed to an
C               order vector).  The list CODES(ORDNAM(1)),
C               CODES(ORDNAM(2)), ... CODES(ORDNAM(NCOD)) forms an
C               increasing non-repeating list of integers.  Moreover,
C               every value in CODES is listed exactly once in this
C               sequence.
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
C     This is a utitility routine used for initializing the ordering
C     vectors that point to the recognized names and codes usde by
C     the private routine M2BODTRN
C
C$ Examples
C
C     See the routine M2BODTRN.
C
C$ Restrictions
C
C     This routine is intended only for use by M2BODTRN.
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT)
C
C        Renamed to M2BODINI and filled out the comments on what this
C        routine does and how it works.
C
C-&
 
C$ Index_Entries
C
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER              I
      INTEGER              N
 
C
C     Create order vectors ORDNAM and ORDCOD
C
      CALL ORDERC ( NAMES, NNAM, ORDNAM )
      CALL ORDERI ( CODES, NNAM, ORDCOD )
 
C
C     Remove duplicate entries in the code order table. The entry that
C     points to the highest entry in CODES should remain.
C
      N = 1
      I = 2
 
C
C     Now for some very funky manuevering.  We are going to take our
C     order vector for the id-codes and modify it!
C
C     Here's what is true now.
C
C     CODES(ORDCOD(1)) <= CODES(ORDCOD(2)) <=...<= CODES(ORDCOD(NNAM)
C
C     For each element such that CODES(ORDCOD(I)) = CODES(ORDCOD(I+1))
C     we are going to "shift" the items ORDCOD(I+1), ORDCOD(I+2), ...
C     left by one.  We will then repeat the test and shift as needed.
C     When we get done we will have a possibly shorter array ORDCOD
C     and the array will satisfy
C
C        CODES(ORDCOD(1)) < CODES(ORDCOD(2)) < ... < CODES(ORDCOD(NNAM)
C
C     We can still use the resulting "ordered vector" (as opposed to
C     order vector) in the BSCHOI routine because it only relies
C     upon the indexes to ORDCOD and not to CODES itself.  This is
C     making very heavy use of the implementation of BSCHOI but we
C     are going to let it go for the momemt because this is a private
C     routine.
C
      DO WHILE ( I .LE. NNAM )
 
         IF ( CODES(ORDCOD(I)) .EQ. CODES(ORDCOD(N)) ) THEN
 
            IF ( ORDCOD(I) .GT. ORDCOD(N) ) THEN
 
               ORDCOD(N) = ORDCOD(I)
 
            END IF
 
         ELSE
 
            N         = N + 1
            ORDCOD(N) = ORDCOD(I)
 
         END IF
 
         I = I + 1
 
      END DO
 
      NCOD = N
 
      RETURN
      END
