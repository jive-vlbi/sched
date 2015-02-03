C$Procedure      VPROJG ( Vector projection, general dimension )
 
      SUBROUTINE VPROJG ( A, B, NDIM, P )
 
C$ Abstract
C
C     VPROJG finds the projection of the one vector onto another
C     vector.  All vectors are of arbitrary dimension.
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
C     VECTOR
C
C$ Declarations
 
      INTEGER            NDIM
      DOUBLE PRECISION   A ( NDIM )
      DOUBLE PRECISION   B ( NDIM )
      DOUBLE PRECISION   P ( NDIM )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     A          I   The vector to be projected.
C     B          I   The vector onto which A is to be projected.
C     NDIM       I   Dimension of A, B, and P.
C     P          O   The projection of A onto B.
C
C$ Detailed_Input
C
C     A     is a double precision vector of arbitrary dimension.  This
C           vector is to be projected onto the vector B.
C
C     B     is a double precision vector of arbitrary dimension.  This
C           vector is the vector which receives the projection.
C
C     NDIM  is the dimension of A, B and P.
C
C$ Detailed_Output
C
C     P     is a double precision vector of arbitrary dimension
C           containing the projection of A onto B. (P is necessarily
C           parallel to B.)
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
C     The projection of a vector A onto a vector B is, by definition,
C     that component of A which is parallel to B.  To find this
C     component it is enough to find the scalar ratio of the length of
C     B to the projection of A onto B, and then use this number to
C     scale the length of B.  This ratio is given by
C
C         RATIO = (A DOT B) / (B DOT B)
C
C     where DOT denotes the general vector dot product. This routine
C     does not attempt to divide by zero in the event that B is the
C     zero vector.
C
C$ Examples
C
C     The following table gives sample inputs and results from calling
C     VPROJG.
C
C        A                  B                NDIM       P
C        -----------------------------------------------------------
C        (6, 6, 6, 6)      ( 2, 0, 0, 0)     4          (6, 0, 0, 0)
C        (6, 6, 6, 0)      (-3, 0, 0, 0)     4          (6, 0, 0, 0)
C        (6, 6, 0, 0)      ( 0, 7, 0, 0)     4          (0, 6, 0, 0)
C        (6, 0, 0, 0)      ( 0, 0, 9, 0)     4          (0, 0, 0, 0)
C
C$ Restrictions
C
C     No error detection or recovery schemes are incorporated into this
C     subroutine except to insure that no attempt is made to divide by
C     zero.  Thus, the user is required to make sure that the vectors
C     A and B are such that no floating point overflow will occur when
C     the dot products are calculated.
C
C$ Literature_References
C
C     Any reasonable calculus text (for example Thomas)
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     H.A. Neilan     (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW)
C
C        Corrected ENDIF to END IF.
C     
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     n-dimensional vector projection
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB)
C
C         Contents of the Exceptions section was changed
C         to "error free" to reflect the decision that the
C         module will never participate in error handling.
C
C         The declaration of the unused variable I was removed.
C-&
 
      DOUBLE PRECISION VDOTG
C
      DOUBLE PRECISION ADOTB
      DOUBLE PRECISION BDOTB
      DOUBLE PRECISION SCALE
C
      ADOTB  = VDOTG (A,B,NDIM)
      BDOTB  = VDOTG (B,B,NDIM)
C
      IF ( BDOTB .EQ. 0.D0 ) THEN
         SCALE = 0.D0
      ELSE
         SCALE = ADOTB/BDOTB
      END IF
C
      CALL VSCLG (SCALE, B, NDIM, P)
C
      RETURN
      END
