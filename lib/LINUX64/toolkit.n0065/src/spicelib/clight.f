C$Procedure CLIGHT ( C, Speed of light in a vacuum )
 
      DOUBLE PRECISION FUNCTION CLIGHT ( )
 
C$ Abstract
C
C     Return the speed of light in a vacuum (IAU official
C     value, in km/sec).
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
C     The function returns the speed of light in vacuo (km/sec).
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     The function returns the IAU official value for the speed of light
C     in vacuo: 299792.458 km/sec.
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
C     The function always returns the constant value shown above.
C
C$ Examples
C
C     Find the light time corresponding to the length of a given
C     3-dimensional position vector. Length units are km.
C
C     To use CLIGHT, declare it as having double precision type:
C
C        DOUBLE PRECISION      CLIGHT
C
C     Let POS be a 3-vector of interest; let TAU be the light time.
C     VNORM is the SPICELIB function that returns the norm of a 
C     3-vector.
C
C        DOUBLE PRECISION      VNORM
C        DOUBLE PRECISION      TAU
C        DOUBLE PRECISION      POS (3 )
C        
C     Find the light time:
C
C        TAU = VNORM ( POS ) / CLIGHT ()
C
C     Note that the SPK readers 
C
C        SPKEZR
C        SPKEZ
C        SPKPOS
C        SPKEZP
C 
C     return the one-way light time between target and observer
C     as an output.
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.2, 08-JAN-2008 (NJB)
C
C         Example section was updated to remove references to SPKAPP
C         and BODMAT.
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
C     c speed of light in a vacuum
C
C-&
 
 
C
C     Just like it says.
C
      CLIGHT = 299792.458D0
 
      RETURN
      END
