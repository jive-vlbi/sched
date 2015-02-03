C$Procedure      DPSTRP ( DP Number to Character, With Plus )

      SUBROUTINE DPSTRP ( X, SIGDIG, STRING )

C$ Abstract
C
C     This routine is a wrapper around DPSTRE. It passes all inputs
C     directly to DPSTRE and does only one thing to its output --
C     replaces the first character of the output string with '+'
C     for positive numbers.
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
C     CHARACTER
C     CONVERSION
C     PARSING
C
C$ Declarations

      IMPLICIT              NONE

      DOUBLE PRECISION X
      INTEGER          SIGDIG
      CHARACTER*(*)    STRING

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X          I   A double precision number
C     SIGDIG     I   The number of significant digits placed in output
C     STRING     O   A character string representation of X
C
C$ Detailed_Input
C
C     See DPSTRE.
C
C$ Detailed_Output
C
C     See DPSTRE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See DPSTRE.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See DPSTRE.
C
C$ Examples
C
C     See DPSTRE.
C
C$ Restrictions
C
C     See DPSTRE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    FRMDIFF Version 2.0.0, 27-FEB-2012 (BVS)
C
C        Replaced the call to SPICELIB's DPSTR with the call to
C        SUPPORT's DPSTRE which does not limit the number of
C        significant digits to 14.
C
C-    FRMDIFF Version 1.0.0, 10-SEP-2008 (BVS)
C
C-&

C$ Index_Entries
C
C     d.p. number to character
C
C-&

C
C     Call DPSTRE.
C
      CALL DPSTRE( X, SIGDIG, STRING )

C
C     If the first character is blank, replace it with '+'.
C
      IF ( STRING(1:1) .EQ. ' ' ) THEN
         STRING(1:1) = '+'
      END IF

C
C     That's all folks.
C
      RETURN

      END
