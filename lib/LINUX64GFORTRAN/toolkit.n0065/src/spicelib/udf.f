C$Procedure UDF ( Dummy function for UDFUNS )

      SUBROUTINE UDF ( X, VALUE )

C$ Abstract
C
C     No-op routine for with an argument signature matching UDFUNS.
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
C    None.
C
C$ Keywords
C
C    None.
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      X
      DOUBLE PRECISION      VALUE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     X         I/O  Double precision value, unused.
C     VALUE     I/O  Double precision value, unused.
C
C$ Detailed_Input
C
C     X         Double precision value, unused.
C
C     VALUE     Double precision value, unused.
C
C$ Detailed_Output
C
C    None.
C
C$ Parameters
C
C    None.
C
C$ Exceptions
C
C    None.
C
C$ Files
C
C    None.
C
C$ Particulars
C
C    The routine performs no evaluations. It exists for routines
C    expecting an UDFUNS argument. In the cases where UDFUNC is
C    unneeded or unavailable, this routine provides a null operation
C    alternative.
C
C$ Examples
C
C    None.
C
C$ Restrictions
C
C    None.
C
C$ Literature_References
C
C    None.
C
C$ Author_and_Institution
C
C    E.D. Wright    (JPL)
C
C$ Version
C
C-   SPICELIB Version 1.0.0  21-OCT-2013 (EDW)
C
C-&

C$ Index_Entries
C
C   dummy function for UDFUNS signature arguments
C
C-&

      X     = X     + 0.D0
      VALUE = VALUE + 0.D0

      RETURN

      END

