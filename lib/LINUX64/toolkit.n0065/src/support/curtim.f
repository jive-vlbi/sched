C$Procedure CURTIM (Current Time)
 
      SUBROUTINE CURTIM ( TIME )
 
C$ Abstract
C
C     Return a string giving the current date and time
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
C      None.
C
C$ Keywords
C
C       TIME
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         TIME
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      TIME       O   A string containing the current date and time.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     TIME        is a string that contains the current date and
C                 time in the format YEAR-MON-DY HR:MN:SC
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is a utility for creating time-stamps for inserting
C     into data products.  It is not intended to provide accurate
C     measurment of local time since local time is not necessarily
C     in step with the processor clock.  If you need the numeric
C     components, see the routine ZZCPUTIM.
C
C
C$ Examples
C
C     Suppose that you wish to insert into a data product the
C     system time at the time of creation of the product.  You
C     could call this routine to get the current time (in a string)
C     and then simply write that string into the data product.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SUPPORT Version 1.1.0, 11-SEP-2007 (EDW)
C
C        Replaced CPUTIM call with ZZCPUTIM call. Edited previous
C        Version entries to clarify CURTIM pedigree.
C
C-    SUPPORT Version 1.0.1, 03-MAY-1994 (WLT)
C
C        This is the configured version of the Command Loop
C        software as of May 4, 1994
C
C-    SUPPORT Version 1.0.0, 20-APR-1994 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Get a string giving the current system time
C
C-&
 
 
 
      CHARACTER*(4)         YEAR
      CHARACTER*(3)         MONTH ( 12 )
      CHARACTER*(2)         DAY
      CHARACTER*(2)         HOUR
      CHARACTER*(2)         MIN
      CHARACTER*(2)         SEC
 
      DOUBLE PRECISION      TVEC ( 6 )
      INTEGER               IVEC ( 6 )
      INTEGER               I
 
      SAVE
 
      DATA                  MONTH / 'JAN', 'FEB', 'MAR', 'APR',
     .                              'MAY', 'JUN', 'JUL', 'AUG',
     .                              'SEP', 'OCT', 'NOV', 'DEC'  /
 
      CALL ZZCPUTIM( TVEC )
 
      DO I = 1,6
         IVEC(I) = INT(TVEC(I))
      END DO
 
      CALL INTSTR ( IVEC(1), YEAR  )
      CALL INTSTR ( IVEC(3), DAY   )
      CALL INTSTR ( IVEC(4), HOUR  )
      CALL INTSTR ( IVEC(5), MIN   )
      CALL INTSTR ( IVEC(6), SEC   )
 
      CALL RJUST  ( DAY,  DAY  )
      CALL RJUST  ( HOUR, HOUR )
      CALL RJUST  ( MIN,  MIN  )
      CALL RJUST  ( SEC,  SEC  )
 
      CALL REPLCH ( DAY,  ' ', '0', DAY  )
      CALL REPLCH ( HOUR, ' ', '0', HOUR )
      CALL REPLCH ( MIN,  ' ', '0', MIN  )
      CALL REPLCH ( SEC,  ' ', '0', SEC  )
 
      TIME = YEAR // '-' // MONTH(IVEC(2)) // '-' // DAY // ' '
     .//     HOUR // ':' // MIN            // ':' // SEC
 
      RETURN
      END
