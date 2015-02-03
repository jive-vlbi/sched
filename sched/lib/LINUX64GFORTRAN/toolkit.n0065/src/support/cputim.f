C$Procedure CPUTIM ( CPU Time )
 
      SUBROUTINE CPUTIM ( TVEC )
 
C$ Abstract
C
C     Fetch the current CPU date and time and store the result
C     as a double precision 6-vector.
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
C     UTILITY
C
C$ Declarations
 
      DOUBLE PRECISION      TVEC ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TVEC       O   contains year, month, day, hours, minutes, seconds
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     TVEC       is a 6-vector containing the current system time.
C                The various components have the following meaning
C
C                   TVEC(1)  --- current calendar year
C                   TVEC(2)  --- current month
C                   TVEC(3)  --- current day of month
C                   TVEC(4)  --- current hour. Hours have a range from
C                                0 to 23.  0 corresponds to system
C                                midnight.
C                   TVEC(5)  --- current minutes
C                   TVEC(6)  --- current seconds and fraction of a
C                                second (provided the system clock
C                                has sufficiently fine granularity
C                                to provide greater precision).
C
C                The first 5 components will be double precision
C                integers.  (They truncate without change.)
C
C$ Parameters
C
C     None.
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
C     This routine returns the components of the current date and
C     time as determined by the system clock.
C
C
C$ Examples
C
C     Example 1.
C
C     The following routine illustrates how you might use CPUTIM
C     to generate a "time stamp" that might be used to tag data
C     you plan to write to a file.
C
C           SUBROUTINE TSTAMP ( STAMP )
C
C           CHARACTER*(15)        STAMP
C
C           DOUBLE PRECISION      TVEC ( 6 )
C
C     C
C     C     First fetch the current system time.
C     C
C           CALL CPUTIM ( TVEC )
C
C     C
C     C     now form a time stamp of the form YYYYYMMDDhhmmss
C     C
C           CALL DPFMT ( TVEC(1), '0YYYY', STAMP(1:5)   )
C           CALL DPFMT ( TVEC(2), '0M',    STAMP(6:7)   )
C           CALL DPFMT ( TVEC(3), '0D',    STAMP(8:9)   )
C           CALL DPFMT ( TVEC(4), '0h',    STAMP(10:11) )
C           CALL DPFMT ( TVEC(5), '0m',    STAMP(12:13) )
C           CALL DPFMT ( TVEC(6), '0s',    STAMP(14:15) )
C
C           RETURN
C
C     Example 2.
C
C     The following code illustrates how you might use this routine
C     to perform a crude estimate of the running time of the
C     SPICELIB routine VDOT. (This assumes that the program runs
C     during a single calendar day and that there is no competition
C     between users for system resources.)
C
C           DOUBLE PRECISION      VDOT
C
C           DOUBLE PRECISION      AVE
C           DOUBLE PRECISION      SINCE0( 2    )
C           DOUBLE PRECISION      TVEC  ( 6, 3 )
C           DOUBLE PRECISION      V1    ( 3    )
C           DOUBLE PRECISION      V2    ( 3    )
C           DOUBLE PRECISION      X
C
C           INTEGER               I
C           INTEGER               TRIALS
C           PARAMETER           ( TRIALS = 100000 )
C
C     C
C     C     Give the vectors some values (these seem as good as
C     C     anything else that comes to mind).
C     C
C           V1(1) = 1.0D0
C           V1(2) = 2.0D0
C           V1(3) = 3.0D0
C
C           V2(1) = 10.0D0
C           V2(2) = 20.0D0
C           V3(3) = 30.0D0
C
C     C
C     C     Perform the loop twice, once with one call to VDOT, the
C     C     second with two calls to VDOT.
C     C     The first will require
C     C
C     C        LOOP_OVERHEAD + TRIALS*TIME_FOR_VDOT
C     C
C     C     The second will require
C     C
C     C        LOOP_OVERHEAD + 2*TRIALS*TIME_FOR_VDOT
C     C
C     C     The difference of the two, will give us
C     C
C     C        TRIALS*TIME_FOR_VDOT
C     C
C
C     C
C     C     get the current system time.
C     C
C           CALL CPUTIM ( TVEC(1,1) )
C
C           DO I = 1, TRIALS
C              X = VDOT( V1, V2 )
C           END DO
C
C     C
C     C     Get the time after the first pass.
C     C
C           CALL CPUTIM ( TVEC(1,2)
C
C           DO I = 1, TRIALS
C              X = VDOT( V1, V2 )
C              X = VDOT( V1, V2 )
C           END DO
C
C      C
C      C    Get the time after the second pass.
C      C
C           CALL CPUTIM ( TVEC(1,3)
C
C
C      C
C      C    Now compute seconds past midnight at each clock reading.
C      C
C           DO I = 1, 3
C
C              SINCE0(I) = TVEC(4,I) * 3600.0D0
C          .             + TVEC(5,I) *   60.0D0
C          .             + TVEC(6,I)
C           END DO
C
C      C
C      C    The time for the first  pass is SINCE0(2) - SINCE0(1)
C      C    The time for the second pass is SINCE0(3) - SINCE0(2)
C      C
C      C    The difference between these divided by the number of
C      C    trials is the average running time.
C      C
C           AVE = (SINCE0(3) - 2*SINCE0(2) - SINCE0(1)) / DBLE(TRIALS)
C
C           WRITE (*,*) 'The average running time for VDOT is ', AVE
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C   None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     K.R. Gehringer (JPL)
C     H.A. Neilan    (JPL)
C     M.J. Spencer   (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version  1.0.0, 13-FEB-2008 (EDW)
C
C        This routine calls the ZZCPUTIM routine in SPICELIB, 
C        performing no other operation.
C
C-&
 
C$ Index_Entries
C
C     get system date and time
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CPUTIM' )
      END IF

C
C     Get the date and time.
C
      CALL ZZCPUTIM( TVEC )

C
C     That's it.
C
      CALL CHKOUT ( 'CPUTIM' )
      RETURN
 
      END
