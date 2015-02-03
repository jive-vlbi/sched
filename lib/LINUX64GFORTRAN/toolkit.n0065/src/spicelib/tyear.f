C$Procedure      TYEAR ( Seconds per tropical year )
 
      DOUBLE PRECISION FUNCTION TYEAR ()
 
C$ Abstract
C
C     Return the number of seconds in a tropical year.
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
C  None.
C
C$ Keywords
C
C      CONSTANTS
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C      VARIABLE  I/O              DESCRIPTION
C      --------  ---  --------------------------------------------------
C      TYEAR       O   The number of seconds/tropical year
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     The function returns the number of seconds per tropical
C     year.  This value is taken from the 1992 Explanatory Supplement
C     to the Astronomical Almanac.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     The tropical year is often used as a fundamental unit
C     of time when dealing with older ephemeris data.  For this
C     reason its value in terms of ephemeris seconds is
C     recorded in this function.
C
C$ Examples
C
C     Suppose you wish to compute the number of tropical centuries
C     that have elapsed since the ephemeris epoch B1950 (beginning
C     of the Besselian year 1950) at a particular ET epoch.  The
C     following line of code will do the trick.
C
C
C        CENTRY = ( ET - UNITIM ( B1950(), 'JED', 'ET' ) )
C       .       / ( 100.0D0 * TYEAR() )
C
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
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      Explanatory Supplement to the Astronomical Almanac.
C      Page 80. University Science Books, 20 Edgehill Road,
C      Mill Valley, CA 94941
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Number of seconds per tropical year
C
C-&
 
      TYEAR = 31556925.9747D0
      RETURN
      END
 
 
