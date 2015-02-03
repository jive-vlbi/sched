C$Procedure ZZGFWSTS ( Private --- GF, sift first window thru second )
 
      SUBROUTINE ZZGFWSTS ( WNDW1, WNDW2, INCLSN, WNDW3 )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Determine those intervals of the first window that are
C     properly contained in an interval of the second.
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
C     INTERVALS,  WINDOWS
C
C$ Declarations
 
      IMPLICIT NONE
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      WNDW1 ( LBCELL : * )
      DOUBLE PRECISION      WNDW2 ( LBCELL : * )
      CHARACTER*(*)         INCLSN
      DOUBLE PRECISION      WNDW3 ( LBCELL : * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ---------------------------------------------------
C     WNDW1      I   Input window 1.
C     WNDW2      I   Input window 2.
C     INCLSN     I   Flag indicating inclusion desired.
C     WNDW3     I/O  Result of sifting WNDW1 through WNDW2.
C
C$ Detailed_Input
C
C     WNDW1      is an initialized SPICELIB window
C
C     WNDW2      is an initialized SPICELIB window
C
C     INCLSN     is a string indicating how intervals of WNDW1 must
C                be contained in WNDW2. Allowed values are: '[]', '(]',
C                '[)', and '()', where a square bracket represents a
C                closed interval and a curved bracket an open interval.
C                Suppose that [a,b] is an interval of WNDW1 and that
C                [c,d] is an interval of WNDW2.  Then the table below
C                shows the tests used to determine the inclusion of
C                [a,b] in the interval from c to d.
C
C                []     ---  [a,b]  is contained in [c,d]
C                (]     ---  [a,b]  is contained in (c,d]
C                [)     ---  [a,b]  is contained in [c,d)
C                ()     ---  [a,b]  is contained in (c,d)
C
C                if INCLSN is not one of these four values, the
C                error SPICE(UNKNOWNINCLUSION) is signaled.
C
C
C
C     WNDW3      is an initialized SPICELIB window, used on input
C                only for the purpose of determining the amount
C                of space declared for use in WNDW3.
C
C$ Detailed_Output
C
C     WNDW3    is a window consisting those of intervals in WNDW1
C              that are wholly contained in some interval of WNDW2.
C
C$ Parameters
C
C     LBCELL     is the SPICELIB cell lower bound.
C
C$ Exceptions
C
C     1) If the window WNDW3 does not have sufficient space to
C        contain the sifting of WNDW1 through WNDW2 the error
C        'SPICE(OUTOFROOM)' is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows the user to specify two closed subsets of the
C     real line and to find the intervals of one that are contained
C     within the intervals of another. The subsets of the real line
C     are assumed to be made up of disjoint unions of closed intervals.
C
C$ Examples
C
C     Suppose that WNDW1 and WNDW2 are described by the tables below.
C
C                    WNDW1                         WNDW2
C                12.3    12.8                  11.7    13.5
C                17.8    20.4                  17.2    18.3
C                21.4    21.7                  18.5    22.6
C                38.2    39.8                  40.1    45.6
C                44.0    59.9
C
C     Then WNDW3 will be given by:
C
C                    WNDW3
C                12.3    12.8
C                21.4    21.7
C
C$ Restrictions
C
C     The set WNDW3 must not overwrite WNDW1 or WNDW2.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     L.S. Elson      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 08-DEC-2010 (EDW)
C
C        Edit to replaced term "schedule" with "window." 
C
C-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT)
C
C-&
 
C$ Index_Entries
C
C find window intervals contained in an interval of another
C
C-&
 
C
C     SPICELIB functions
C
 
      INTEGER               CARDD
      INTEGER               SIZED
 
      LOGICAL               RETURN

C
C     Local parameters
C   
      INTEGER               INCLEN
      PARAMETER           ( INCLEN = 2 )

C
C     Local Variables
C
      CHARACTER*(INCLEN)    LOCINC

      INTEGER               BEGP1
      INTEGER               BEGP2
      INTEGER               BEGP3
      INTEGER               ENDP1
      INTEGER               ENDP2
      INTEGER               ENDP3
      INTEGER               MAXPTS
      INTEGER               OVFLOW
      INTEGER               SIZE1
      INTEGER               SIZE2
 
      LOGICAL               CLOSED
      LOGICAL               KEEP
      LOGICAL               LEFT
      LOGICAL               OPEN
      LOGICAL               RIGHT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'ZZGFWSTS')
 
C
C     Store the maximum number of endpoints that can be loaded into
C     WNDW3
C
      MAXPTS      = SIZED ( WNDW3 )
      CALL SSIZED ( MAXPTS, WNDW3 )
 
C
C     Find the number of endpoints in each of the input windows.
C
      SIZE1     = CARDD( WNDW1 )
      SIZE2     = CARDD( WNDW2 )
 
C
C     Initialize the place holders for each of the input windows.
C
      BEGP1     =  1
      BEGP2     =  1
 
      ENDP1     =  2
      ENDP2     =  2
 
      BEGP3     = -1
      ENDP3     =  0
 

      CALL CMPRSS ( ' ', 0, INCLSN, LOCINC )

      OPEN   = LOCINC .EQ. '()'
      LEFT   = LOCINC .EQ. '[)'
      RIGHT  = LOCINC .EQ. '(]'
      CLOSED = LOCINC .EQ. '[]'
 
      IF ( .NOT. (     OPEN
     .            .OR. LEFT
     .            .OR. RIGHT
     .            .OR. CLOSED ) ) THEN
 
         CALL SETMSG ( 'The value of the inclusion flag must be '
     .   //            'one of the following: ''[]'', ''[)'', '
     .   //            '''(]'', or ''()''.  However the value '
     .   //            'supplied was ''#''. ' )
         CALL ERRCH  ( '#', INCLSN )
         CALL SIGERR ( 'SPICE(UNKNOWNINCLUSION)'  )
         CALL CHKOUT ( 'ZZGFWSTS' )
         RETURN
 
      END IF

C
C     We haven't had a chance to overflow yet.
C
      OVFLOW    = 0
 
      DO WHILE (       ( BEGP1 .LT. SIZE1 )
     .           .AND. ( BEGP2 .LT. SIZE2 ) )
C
C        Using the current interval endpoints determine the overlap of
C        the two intervals.
C
         IF      ( WNDW1(ENDP1) .LT. WNDW2(BEGP2)  ) THEN
C
C           the end of the first interval precedes the beginning of the
C           second
C
            BEGP1    = BEGP1    + 2
            ENDP1    = ENDP1    + 2
 
         ELSE IF ( WNDW2(ENDP2) .LT. WNDW1(BEGP1) ) THEN
C
C           the end of the second interval precedes the beginning of the
C           first
C
            BEGP2    = BEGP2    + 2
            ENDP2    = ENDP2    + 2
 
         ELSE
C
C           the intervals intersect.  Is the first contained in the
C           second?
C
            IF      ( CLOSED ) THEN
 
               KEEP =       ( WNDW1(BEGP1) .GE. WNDW2(BEGP2) )
     .                .AND. ( WNDW1(ENDP1) .LE. WNDW2(ENDP2) )
 
            ELSE IF ( OPEN   ) THEN
 
               KEEP =       ( WNDW1(BEGP1) .GT. WNDW2(BEGP2) )
     .                .AND. ( WNDW1(ENDP1) .LT. WNDW2(ENDP2) )
 
            ELSE IF ( LEFT   ) THEN
 
               KEEP =       ( WNDW1(BEGP1) .GE. WNDW2(BEGP2) )
     .                .AND. ( WNDW1(ENDP1) .LT. WNDW2(ENDP2) )
 
            ELSE IF ( RIGHT ) THEN
 
               KEEP =       ( WNDW1(BEGP1) .GT. WNDW2(BEGP2) )
     .                .AND. ( WNDW1(ENDP1) .LE. WNDW2(ENDP2) )
 
            END IF
 
            IF ( KEEP ) THEN
 
               BEGP3 = BEGP3    + 2
               ENDP3 = ENDP3    + 2
 
               IF ( BEGP3 .LT. MAXPTS ) THEN
 
C
C                 Adequate room is left in WNDW3 to include this
C                 interval
C
                  WNDW3 ( BEGP3 ) = WNDW1(BEGP1)
                  WNDW3 ( ENDP3 ) = WNDW1(ENDP1)
 
               ELSE
 
                  OVFLOW = OVFLOW + 2
 
               END IF
 
            END IF
 
C
C           Determine which window pointers to increment
C
            IF      (  WNDW1(ENDP1) .LT. WNDW2(ENDP2)  ) THEN
C
C              The first interval lies before the end of the second
C
               BEGP1    = BEGP1    + 2
               ENDP1    = ENDP1    + 2
 
            ELSE IF ( WNDW2(ENDP2) .LT. WNDW1(ENDP1) ) THEN
C
C              The second interval lies before the end of the first
C
               BEGP2    = BEGP2    + 2
               ENDP2    = ENDP2    + 2
 
            ELSE
C
C              The first and second intervals end at the same place
C
               BEGP1    = BEGP1    + 2
               ENDP1    = ENDP1    + 2
 
               BEGP2    = BEGP2    + 2
               ENDP2    = ENDP2    + 2
 
            END IF
 
         END IF
 
      END DO
 
      IF ( OVFLOW .GT. 0 ) THEN
 
         CALL SETMSG ( 'The output window does not have '
     .   //            'sufficient memory to contain the result '
     .   //            'of sifting the two given windows. The '
     .   //            'output window requires space for # '
     .   //            'more values than what has been provided. ')
         CALL ERRINT ( '#', OVFLOW        )
         CALL SIGERR ( 'SPICE(OUTOFROOM)' )
 
      ELSE
 
         CALL SCARDD ( ENDP3, WNDW3 )
 
      END IF
 
      CALL CHKOUT ( 'ZZGFWSTS' )
      RETURN
      END
