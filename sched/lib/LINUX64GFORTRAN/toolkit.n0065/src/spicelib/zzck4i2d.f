C$Procedure      ZZCK4I2D ( Pack set of integers into a single DP )

      SUBROUTINE ZZCK4I2D ( I, NSETS, PARCOD, DPCOEF )

C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     This is the routine that packs a set integer numbers into a 
C     single double precision number.
C
C     Its current specific use is to "compress" seven integer numbers
C     representing numbers of polynomial coefficients in a logical 
C     type 4 CK record into a single DP number stored in a physical 
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

      INTEGER                 I      ( * )
      INTEGER                 NSETS
      DOUBLE PRECISION        PARCOD
      DOUBLE PRECISION        DPCOEF

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     I          I   Array of NSETS integer components.
C     NSETS      I   Number of integer components in input array I.
C     PARCOD     I   Packing base.
C     DPCOEF     O   DP number containing NSETS packed integer numbers.
C
C$ Detailed_Input
C
C     I          is an array containing integers to be packed.
C
C     NSETS      is the number of elements in the array I.
C
C     PARCOD     is the packing base.
C
C$ Detailed_Output
C
C     DPCOEF     is a DP number containing elements of the input 
C                array packed together.
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
C     No checking is done to determine whether elements of the array
C     I are within range [0:PARCOD-1] and whether PARCOD**NSETS will 
C     cause DPCOEF mantissa overflow.
C
C$ Particulars
C
C     This routine packs NSETS elements of the array I into a single
C     double precision variable using base specified by PARCOD. When 
C     packed the double precision number DPCOEF represents NSETS of 
C     integer elements of the array I as follows:
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
C     1) No checking is done to determine whether elements of the 
C        array I are within range [0:PARCOD-1] to prevent "overflow"
C        of particular 
C
C     2) No checking is done to determine whether PARCOD**NSETS
C        will cause DPCOEF mantissa overflow.
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
C     Local variables
C
      DOUBLE PRECISION        X
      INTEGER                 K

C
C     Let's pack it!
C
      DPCOEF = 0.D0

      X = 1

      DO K = 1, NSETS, 1

         DPCOEF = DPCOEF + I( K ) * X
         X = X * PARCOD
         
      END DO

C
C     All done.
C
      RETURN
      END
