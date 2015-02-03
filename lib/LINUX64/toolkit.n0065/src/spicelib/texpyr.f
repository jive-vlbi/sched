C$Procedure      TEXPYR ( Time --- Expand year )
 
      SUBROUTINE TEXPYR ( YEAR )
 
C$ Abstract
C
C    Expand an abbreviated year to a full year specification.
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
C     TIME
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               YEAR
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     YEAR      I/O  The year of some epoch abbreviated/expanded.
C
C$ Detailed_Input
C
C     YEAR      is an "abbreviated year."  In other words the 98 of
C               1998,  05 of 2005, etc.
C
C$ Detailed_Output
C
C     YEAR      is the expansion of the abbreviated year according
C               to the lower bound established in the entry point
C               TSETYR.  By default if YEAR is 69 to 99, the output
C               is 1900 + the input value of YEAR.  If YEAR is 0 to 68
C               the output value of YEAR is 2000 + the input value of
C               YEAR.
C
C               See the entry point TSETRY to modify this behavior.
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
C     1) If on input YEAR is not in the inclusive interval from
C        0 to 99, YEAR is returned unchanged.
C
C$ Particulars
C
C     This routine allows all of the SPICE time subsystem to handle
C     uniformly the expansion of "abbreviated" years.  (i.e. the
C     remainder after dividing the actual year by 100).
C
C     By using this routine together with the routine TSETYR you
C     can recover the actual year to associate with an abbreviation.
C
C     The default behavior is as follows
C
C     YEAR input      YEAR Output
C     ----------      -----------
C     00              2000
C     01              2001
C      .                .
C      .                .
C      .                .
C     66              2066
C     67              2067
C     68              2068
C     69              1969
C      .                .
C      .                .
C      .                .
C     99              1999
C
C
C$ Examples
C
C     Suppose that you use TPARTV to parse time strings and that
C     you want to treat years components in the range from 0 to 99
C     as being abbreviations for years in the range from
C     1980 to 2079 (provided that the years are not modified by
C     an ERA substring).  The code fragment below shows how you
C     could go about this.
C
C        Early in your application set up the lower bound for the
C        expansion of abbreviated years.
C
C        CALL TSETYR ( 1980 )
C
C
C        After calling TPARTV or some other suitable parsing routine
C        get the integer value of the year.
C
C        YEAR = NINT( TVEC(1) )
C
C        Having satisfied yourself that the year does not represent
C        a year in the range from 99 to 1 B.C. or in the range
C        from 1 to 99 A.D.  Expand Year to the appropriate value.
C
C        IF ( YEAR .LT. 100 ) THEN
C
C           CALL TEXPYR ( YEAR )
C
C        END IF
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT)
C
C        The default century was change from 1950-2049 to 1969-2068
C
C-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Expand an abbreviated year to a fully specified year.
C
C-&
 
      INTEGER               CENTRY
      INTEGER               LBOUND
      SAVE
 
      DATA                  CENTRY  / 1900 /
      DATA                  LBOUND  / 1969 /
 
      IF ( YEAR .GE. 100 .OR. YEAR .LT. 0 ) THEN
         RETURN
      END IF
 
      YEAR = YEAR + CENTRY
 
      IF ( YEAR .LT. LBOUND ) THEN
         YEAR = YEAR + 100
      END IF
 
      RETURN
 
 
 
 
C$Procedure      TSETYR ( Time --- set year expansion boundaries )
 
      ENTRY TSETYR ( YEAR )
 
C$ Abstract
C
C    Set the lower bound on the 100 year range
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
C     TIME
C
C$ Declarations
C
C     INTEGER               YEAR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     YEAR       I   Lower bound on the 100 year interval of expansion
C
C$ Detailed_Input
C
C     YEAR       is the year associated with the lower bound on all
C                year expansions computed by TEXPYR.  For example
C                if YEAR is 1980, then the range of years that
C                can be abbreviated is from 1980 to 2079.
C
C$ Detailed_Output
C
C     None.
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
C     1) If YEAR is less than 1 no action is taken
C
C$ Particulars
C
C     This entry point allows you to set the range to which years
C     abbreviated to the last two digits will be expanded. The input
C     supplied to this routine represents the lower bound of the
C     expansion interval.  The upper bound of the expansion interval
C     is YEAR + 99.
C
C     The default expansion interval is from 1969 to 2068.
C
C$ Examples
C
C     See the main routine TEXPYR.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT)
C
C        The default century was change from 1950-2049 to 1969-2068
C
C-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Set the interval of expansion for abbreviated years
C
C-&
 
      CENTRY = ( YEAR/100 ) * 100
      LBOUND =   YEAR
 
      RETURN
      END
