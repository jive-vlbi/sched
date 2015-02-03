C$Procedure      PACKAC ( Pack a character array )
 
      SUBROUTINE PACKAC ( IN,
     .                    PACK,   NPACK,
     .                    MAXOUT, NOUT,   OUT )
 
C$ Abstract
C
C     Pack the contents of a CHARACTER array. That is, take
C     a set of arbitrarily spaced elements from an input array,
C     and make them adjacent elements in an output array.
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
C     ARRAY
C     ASSIGNMENT
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)    IN      ( * )
      INTEGER          PACK    ( * )
      INTEGER          NPACK
      INTEGER          MAXOUT
 
      INTEGER          NOUT
      CHARACTER*(*)    OUT     ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IN         I   Input array.
C     PACK       I   Indices of elements to be packed.
C     NPACK      I   Number of indices.
C     MAXOUT     I   Maximum number of elements in the output array.
C     NOUT       O   Number of elements in the output array.
C     OUT        O   Output array.
C
C$ Detailed_Input
C
C     IN          is the input array.
C
C     PACK        is the set of elements to be packed into the output
C                 array. PACK(i) is the index of the element in the
C                 input array that is to become the i'th element of
C                 the output array.
C
C     NPACK       is the number of elements to be packed into the
C                 output array.
C
C     MAXOUT      is the maximum number of elements to be packed
C                 into the output array. If NPACK is larger than
C                 MAXOUT, the extra items are ignored.
C
C$ Detailed_Output
C
C     NOUT        is the number of elements in the output array.
C
C     OUT         is the output array. This array contains up to
C                 MAXOUT elements from the input array, located
C                 in the first NOUT elements of the array.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If an element in the PACK array is less than 1, the error
C     SPICE(INVALIDINDEX) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The indicated elements are moved from their current locations
C     in the input array to consecutive positions in the output array.
C
C        OUT(   1) = IN(PACK(   1))
C        OUT(   2) = IN(PACK(   2))
C             .
C             .
C        OUT(NOUT) = IN(PACK(NOUT))
C
C     NOUT is either NPACK or MAXOUT, whichever is smaller.
C
C$ Examples
C
C     The most common use for this routine is to remove unwanted items
C     from an array or set of arrays. For example, suppose that the
C     arrays NAME, CODE, RADIUS and MASS contain the names, NAIF
C     integer ID codes, radii, and masses of a set of NSAT satellites.
C     Suppose further that the user selects a subset of the original
C     set of satellites from a menu of some sort. Let the indices of
C     these satellites be the NSEL elements of the array SEL. The
C     following sequence would remove the names, codes, etc., of the
C     unselected satellites from the arrays.
C
C        CALL PACKAC ( NAME,   SEL, NSEL, NSAT, NOUT, NAME2   )
C        CALL PACKAI ( CODE,   SEL, NSEL, NSAT, NOUT, CODE2   )
C        CALL PACKAD ( RADIUS, SEL, NSEL, NSAT, NOUT, RADIUS2 )
C        CALL PACKAD ( MASS,   SEL, NSEL, NSAT, NOUT, MASS2   )
C
C     In the example above, suppose that NAME and PACK contain
C     the following:
C
C        NAME = 'MIMAS'          PACK = 2, 4, 6, 7
C               'ENCELADUS'
C               'TETHYS'
C               'DIONE'
C               'RHEA'
C               'TITAN'
C               'HYPERION'
C               'IAPETUS'
C               'PHOEBE'
C
C     Then, following the call to PACKAC, NOUT and NAME2 contain
C     the following:
C
C        NOUT = 4                 NAME2 = 'ENCELADUS'
C                                         'DIONE'
C                                         'TITAN'
C                                         'HYPERION'
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
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     pack a character array
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 2.0.0, 4-JAN-1989 (HAN)
C
C         Error handling was added to detect array indices that are
C         out of bound. If any element contained in the PACK array is
C         less than one, an error is signalled, and the output array is
C         not packed.
C
C-&
 
 
 
 
C
C     Spicelib functions
C
      LOGICAL               RETURN
 
C
C
C     Local variables
C
      INTEGER          I
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PACKAC' )
      END IF
 
 
C
C     First, determine how many items to transfer.
C
      NOUT = MIN ( NPACK, MAXOUT )
 
 
C
C     Check to see if PACK contains valid array indices.
C
      DO I = 1, NOUT
 
         IF ( PACK (I) .LT. 1 ) THEN
            CALL SETMSG ( 'Element number * contains index *.' )
            CALL ERRINT ( '*', I                )
            CALL ERRINT ( '*', PACK(I)          )
            CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
            CALL CHKOUT ( 'PACKAC' )
            RETURN
         END IF
 
      END DO
 
 
C
C     Transfer them. Just like it says in the header.
C
      DO I = 1, NOUT
         OUT(I) = IN(PACK(I))
      END DO
 
 
      CALL CHKOUT ( 'PACKAC' )
      RETURN
      END
