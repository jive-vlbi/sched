C$Procedure      LDKLST ( Loads Kernels Listed In a String)

      SUBROUTINE LDKLST ( KLIST )


C$ Abstract
C
C     Load kernels listed in a string.
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
C     SPKDIFF User's Guide.
C
C$ Keywords
C
C     TBD.
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE              'spkdiff.inc'

      CHARACTER*(*)         KLIST

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     KLIST      I   Kernel list.
C
C$ Detailed_Input
C
C     KLIST       is a string containing space-delimited list of
C                 kernels names.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See include file.
C
C$ Exceptions
C
C     1) If FURNSH cannot load a kernel, it or routines in its calling
C        tree signal an error.
C
C$ Files
C
C     Each of the files listed in the input string is loaded using
C     FURNSH.
C
C$ Particulars
C
C     This routine extracts individual kernels names from the input
C     space-delimited list and load each kernel using FURNSH.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     Each word in the input string must be a name of an existing file.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    Version 1.0.0, 30-JUN-2008 (BVS).
C
C-&

C
C     Local variables.
C
C     HLINE is declared 5*LINSIZ to accommodate a string produced
C     by concatenation of up to 5 strings with LINSIZ length.
C
C     HNAME is declared LINSIZ because a individual file name cannot be
C     longer than LINSIZ.
C
      CHARACTER*(5*LINSIZ)  HLINE
      CHARACTER*(LINSIZ)    HNAME

C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     Check in.
C
      CALL CHKIN ( 'LDKLST' )

C
C     Load kernel one by one in a loop.
C
      HLINE = KLIST
      DO WHILE ( HLINE .NE. ' ' )
         CALL NEXTWD( HLINE, HNAME, HLINE )
         CALL FURNSH( HNAME )
      END DO

C
C     Check out.
C
      CALL CHKOUT( 'LDKLST' )

      END
