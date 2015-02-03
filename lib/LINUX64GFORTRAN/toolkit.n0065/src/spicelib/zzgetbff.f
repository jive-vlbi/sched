C$Procedure ZZGETBFF ( Private --- Get Binary File Format )
 
      SUBROUTINE ZZGETBFF ( BFFID )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Fetch binary file format.
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      INTEGER               BFFID
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BFFID      O   Binary file format code for this system.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     BFFID      is an integer code that indicates the binary file
C                format that is determined to be native to this
C                platform.  Possible values are:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C                as defined in the include file 'zzddhman.inc'.
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
C     1) If the platform on which this code is compiled does not
C        produce results that match any of the known binary file
C        formats, this routine sets BFFID to 0.
C
C$ Particulars
C
C     This simple program:
C
C        PROGRAM DPTEST
C
C        DOUBLE PRECISION      DPNUM
C        INTEGER               INNUM ( 2 )
C
C        EQUIVALENCE         ( DPNUM, INNUM )
C
C        DPNUM = 7.0D0
C
C        WRITE (*,*) DPNUM
C        WRITE (*,*) INNUM ( 1 )
C        WRITE (*,*) INNUM ( 2 )
C
C        END
C
C     produces the following results on these representative platforms:
C
C        Sun-Solaris (BIGI3E):
C
C           7.000000000
C           1075576832
C           0
C
C        PC-Linux (LTLI3E):
C
C           7.000000000
C           0
C           1075576832
C
C        Alpha-Gfloat (VAXGFL):
C
C           7.000000000
C           16444
C           0
C
C        Alpha-Dfloat (VAXDFL):
C
C           7.000000000
C           16864
C           0
C
C     This routine performs exactly the same decomposition of the
C     double precision number 7.0D0 into two integers.  The results
C     are checked against those displayed here, and if a match is
C     found, returned.
C
C$ Examples
C
C     See ZZDDHOPN for sample usage.
C
C$ Restrictions
C
C     This routine derives the binary file format ID for a particular
C     platform, but for verification purposes only.  ZZPLATFM should
C     be used to obtain the binary file format for the current platform.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-AUG-2002 (FST)
C
C
C-&
 
C
C     Local Parameters
C
      DOUBLE PRECISION      DPVALU
      PARAMETER           ( DPVALU = 7.0D0 )
 
C
C     Local Variables
C
      INTEGER               I
 
      DOUBLE PRECISION      DEQUIV
      INTEGER               IEQUIV ( 2 )
 
      EQUIVALENCE         ( DEQUIV, IEQUIV )
 
      INTEGER               INT1ST ( NUMBFF )
      INTEGER               INT2ND ( NUMBFF )
 
      DATA                  INT1ST(BIGI3E)  / 1075576832 /
      DATA                  INT2ND(BIGI3E)  / 0          /
 
      DATA                  INT1ST(LTLI3E)  / 0          /
      DATA                  INT2ND(LTLI3E)  / 1075576832 /
 
      DATA                  INT1ST(VAXGFL)  / 16444      /
      DATA                  INT2ND(VAXGFL)  / 0          /
 
      DATA                  INT1ST(VAXDFL)  / 16864      /
      DATA                  INT2ND(VAXDFL)  / 0          /
 
      SAVE                  INT1ST
      SAVE                  INT2ND
 
C
C     Copy DPVALU into the equivalenced DP, DEQUIV.
C
      DEQUIV = DPVALU
 
C
C     Examine the integer pairs, to identify the binary
C     file format.
C
      BFFID = 0
 
      DO I = 1, NUMBFF
 
         IF ( ( IEQUIV(1) .EQ. INT1ST(I) ) .AND.
     .        ( IEQUIV(2) .EQ. INT2ND(I) )       ) THEN
 
            BFFID = I
 
         END IF
 
      END DO
 
      RETURN
 
      END
