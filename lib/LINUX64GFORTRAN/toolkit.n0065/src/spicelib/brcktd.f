C$Procedure BRCKTD (Bracket a double precision value within an interval)
 
      DOUBLE PRECISION FUNCTION BRCKTD ( NUMBER, END1, END2 )
 
C$ Abstract
C
C      Bracket a number. That is, given a number and an acceptable
C      interval, make sure that the number is contained in the
C      interval. (If the number is already in the interval, leave it
C      alone. If not, set it to the nearest endpoint of the interval.)
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
C      INTERVALS,  NUMBERS,  UTILITY
C
C$ Declarations
 
      DOUBLE PRECISION   NUMBER
      DOUBLE PRECISION   END1
      DOUBLE PRECISION   END2
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NUMBER     I   Number to be bracketed.
C      END1       I   One of the bracketing endpoints for NUMBER.
C      END2       I   The other bracketing endpoint for NUMBER.
C      BRCKTD     O   Bracketed number.
C
C$ Detailed_Input
C
C      NUMBER      is the number to be bracketed. That is, the
C                  value of NUMBER is constrained to lie in the
C                  interval bounded by END1 and END2.
C
C      END1,
C      END2        are the lower and upper bounds for NUMBER. The
C                  order is not important.
C
C$ Detailed_Output
C
C      BRCKTD      is NUMBER, if it was already in the interval
C                  provided. Otherwise it is the value of the nearest
C                  bound of the interval.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine provides a shorthand notation for code fragments
C      like the following
C
C            IF      ( NUMBER .LT. END1 ) THEN
C                                              NUMBER = END1
C            ELSE IF ( NUMBER .GT. END2 ) THEN
C                                              NUMBER = END2
C            END IF
C
C      which occur frequently during the processing of program inputs.
C
C$ Examples
C
C      The following illustrate the operation of BRCKTD.
C
C            BRCKTD (  -1.D0,   1.D0,  10.D0 )  =  1.D0
C            BRCKTD (  29.D0,   1.D0,  10.D0 )  = 10.D0
C            BRCKTD (   3.D0, -10.D0,  10.D0 )  =  3.D0
C            BRCKTD (   3.D0, -10.D0,  -1.D0 )  = -1.D0
C
C      The following code fragment illustrates a typical use for BRCKTD.
C
C            C
C            C     Star magnitude limit must be in the range 0-10.
C            C
C                  READ (5,*) MAGLIM
C                  MAGLIM = BRCKTD ( MAGLIM, 0.D0, 10.D0 )
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
C      I.M. Underwood  (JPL)
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     bracket a d.p. value within an interval
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 30-DEC-1988 (WLT)
C
C      The routine was modified so that the order of the endpoints
C      of the bracketing interval is not needed.  The routine now
C      determines which is the left endpoint and which is the
C      right and acts appropriately.
C
C-&
 
 
C
C     What else is there to say?
C
      IF ( END1 .LT. END2 ) THEN
 
         BRCKTD = MAX (  END1, MIN(END2,NUMBER)  )
 
      ELSE
 
         BRCKTD = MAX (  END2, MIN(END1,NUMBER)  )
 
      END IF
 
      RETURN
      END
