C$Procedure ZZCK4D2I ( Unpack a set of integers from DP number )

      SUBROUTINE ZZCK4D2I ( DPCOEF, NSETS, PARCOD, I )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the routine that unpacks a set integer numbers stored in 
C     a single double precision number. 
C
C     Its current specific use is to "uncompress" seven integer numbers
C     representing numbers of polynomial coefficients in a logical 
C     type 4 CK record from a single DP number stored in a physical 
C     type 4 CK record in a file.
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
C     None.
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION        DPCOEF
      INTEGER                 NSETS
      DOUBLE PRECISION        PARCOD
      INTEGER                 I      ( * )


C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     DPCOEF     I   DP number containing packed integer numbers.
C     NSETS      I   Number of integer components packed in DPCOEF.
C     PARCOD     I   Packing base.
C     I          O   Array of NSETS integer components.
C
C$ Detailed_Input
C
C     DPCOEF     is a DP number containing NSETS integers packed 
C                together.
C
C     NSETS      is the number of integers packed in the DPCOEF.
C
C     PARCOD     is the packing base.
C
C$ Detailed_Output
C
C     I          is an array containing unpacked integers.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine unpacks NSETS integers packed in a single double 
C     precision number using base specified by PARCOD and stored them
C     in the array I. The integers are packed in the DP using the 
C     following algorithm:
C
C        [DPCOEF]= PARCOD ** ( NSETS - 1 ) * I( 1 )        +
C                  PARCOD ** ( NSETS - 2 ) * I( 2 )        +
C                  ...
C                  PARCOD ** 1             * I( NSETS - 1 )+
C                  PARCOD ** 0             * I( NSETS )
C     where:
C  
C        I(1:NSETS) - is an array of integer numbers with values 
C                     in the range [0:PARCOD-1].
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     Output integer array I must have enough space to hold NSETS
C     numbers.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     Y.K. Zaiko     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS)
C
C-&

C
C     Local variables.
C
      DOUBLE PRECISION        X
      INTEGER                 K
      
C
C     Let's unpack it!
C
      X = PARCOD ** ( NSETS - 1 )

      DO K = 0, NSETS - 1

         I( NSETS - K ) = DPCOEF / X
         DPCOEF = DPCOEF - I( NSETS - K ) * X
         X = X / PARCOD

      END DO
      
C
C     All done.
C
      RETURN
      
      END
