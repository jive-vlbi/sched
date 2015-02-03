C$Procedure      VEQUG ( Vector equality, general dimension )
 
      SUBROUTINE VEQUG ( VIN, NDIM, VOUT )
 
C$ Abstract
C
C      Make one double precision vector of arbitrary dimension equal
C      to another.
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
C      ASSIGNMENT,  VECTOR
C
C$ Declarations
 
      INTEGER            NDIM
      DOUBLE PRECISION   VIN   ( NDIM )
      DOUBLE PRECISION   VOUT  ( NDIM )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C       VIN       I   NDIM-dimensional double precision vector.
C       NDIM      I   Dimension of VIN (and also VOUT).
C       VOUT      O   NDIM-dimensional double precision vector set
C                     equal to VIN.
C
C$ Detailed_Input
C
C      VIN      is a double precision vector of arbitrary dimension.
C
C      NDIM     is the number of components of VIN.
C
C$ Detailed_Output
C
C      VOUT    is a double precision vector set equal to VIN.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      The code simply sets each component of VOUT equal to the
C      corresponding component of VIN.
C
C$ Examples
C
C      Let STATE be a state vector. Set ABSTAT equal to STATE, and
C      correct ABSTAT for stellar aberration.
C
C      CALL VEQUG  ( STATE,           6, ABSTAT )
C      CALL STELAB ( STATE(1), STATE(4), ABSPOS )
C      CALL VEQU   ( ABSPOS,   ABSTAT(1)        )
C
C
C      Note that this routine may be used in place of MOVED, which
C      sets each output array element equal to the corresponding
C      input array element.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     assign an n-dimensional vector to another
C
C-&
 
      INTEGER I
 
      DO I=1,NDIM
         VOUT(I) = VIN(I)
      END DO
 
      RETURN
      END
