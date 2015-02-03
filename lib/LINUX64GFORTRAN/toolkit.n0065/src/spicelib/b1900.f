C$Procedure                     B1900 ( Besselian Date 1900.0 )
 
      DOUBLE PRECISION FUNCTION B1900 ( )
 
C$ Abstract
C
C     Return the Julian Date corresponding to Besselian Date 1900.0.
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
C     CONSTANTS
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     The function returns the Julian Date corresponding to Besselian
C     date 1900.0.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     The function returns 2415020.31352, the Julian Date corresponding
C     to Besselian Date 1900.0.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The function always returns the constant value shown above.
C
C$ Examples
C
C     The following code fragment illustrates the use of B1900.
C
C        C
C        C     Convert Julian Date to UTC seconds past the reference
C        C     epoch (B1900).
C        C
C              SPREF = ( JD - B1900() ) * SPD()
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
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     besselian date 1900.0
C
C-&
 
 
      B1900 = 2415020.31352D0
 
      RETURN
      END
