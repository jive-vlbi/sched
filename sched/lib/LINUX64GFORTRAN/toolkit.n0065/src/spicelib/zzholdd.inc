C$ Abstract
C
C     SPICE private routine intended solely for the support of SPICE
C     routines. Users should not call this routine directly due to the
C     volatile nature of this routine.
C
C     This file contains parameter declarations for the ZZHOLDD
C     routine.
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
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     GEN       general value, primarily for testing.
C
C     GF_REF    user defined GF reference value.
C
C     GF_TOL    user defined GF convergence tolerance.
C
C     GF_DT     user defined GF step for numeric differentiation.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0  03-DEC-2013 (EDW)
C
C-&


C
C     OP codes. The values exist in the integer domain
C     [ -ZZNOP, -1],
C
      INTEGER               ZZGET
      PARAMETER           ( ZZGET     = -1 )

      INTEGER               ZZPUT
      PARAMETER           ( ZZPUT     = -2 )

      INTEGER               ZZRESET
      PARAMETER           ( ZZRESET   = -3 )

C
C     Current number of OP codes.
C
      INTEGER               ZZNOP
      PARAMETER           ( ZZNOP     =  3 )


C
C     ID codes. The values exist in the integer domain
C     [ 1, NID],
C

C
C     General use, primarily testing.
C
      INTEGER               GEN
      PARAMETER           ( GEN       = 1 )

C
C     The user defined GF reference value.
C
      INTEGER               GF_REF
      PARAMETER           ( GF_REF    = 2 )

C
C     The user defined GF convergence tolerance.
C
      INTEGER               GF_TOL
      PARAMETER           ( GF_TOL    = 3 )

C
C     The user defined GF step for numeric differentiation.
C
      INTEGER               GF_DT
      PARAMETER           ( GF_DT     = 4 )

C
C     Current number of ID codes, dimension of array
C     in ZZHOLDD. Bad things can happen if this parameter
C     does not have the proper value.
C
      INTEGER               NID
      PARAMETER           ( NID       = 4 )


C
C     End of file zzholdd.inc.
C 
