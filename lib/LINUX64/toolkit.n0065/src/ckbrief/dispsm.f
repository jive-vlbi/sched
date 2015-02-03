C$Procedure  DISSM ( Write a summary to standard output )

      SUBROUTINE DISPSM ( NOBJ, IDS, TSTRTS, TENDS, AVFS, FRAMES,
     .                    TOUT, FDSP, TDSP, GDSP, NDSP )

C$ Abstract
C
C     Format and display CK-file data summary on standard output.
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
C     CKBRIEF.UG
C
C$ Keywords
C
C     SUMMARY
C     CK
C
C$ Declarations

      IMPLICIT            NONE
      
      INCLUDE             'ckbrief.inc'

      INTEGER             NOBJ
      INTEGER             IDS    ( NOBJ + 1 )
      INTEGER             FRAMES ( NOBJ )
      INTEGER             AVFS   ( NOBJ )
      DOUBLE PRECISION    TSTRTS ( NOBJ + 1 )
      DOUBLE PRECISION    TENDS  ( NOBJ + 1 )
      CHARACTER*(*)       TOUT
      LOGICAL             FDSP
      LOGICAL             TDSP
      LOGICAL             GDSP
      LOGICAL             NDSP

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NOBJ       I   Number of intervals
C     IDS        I   NAIF ID codes of objects
C     TSTRTS     I   Begin DP SCLK times of intervals
C     TENDS      I   End DP SCLK times of intervals
C     AVFS       I   Angular velocity flags
C     FRAMES     I   NAIF ID codes of reference frames
C     TOUT       I   Key specifying times representation on output 
C     FDSP       I   Flag defining whether frame's name/id is printed
C     TDSP       I   Flag defining tabular/non-tabular summary format
C     GDSP       I   Flag requesting object grouping by coverage
C     NDSP       I   Flag to display frame assosiated with CK ID
C
C$ Detailed_Input
C
C     NOBJ           Number of different coverage intervals in a 
C                    CK-file.
C
C     IDS            Integer array of NAIF ID codes corresponding to 
C                    the coverage intervals.
C
C     TSTRTS         Double precision array of begin DP SCLK times for
C                    each interval for a given CK-file.
C
C     TENDS          Double precision array of end DP SCLK times for
C                    each interval for a given CK-file.
C
C     AVFS           Integer array of angular velocities flags
C                    corresponding to the coverage intervals.
C
C     FRAMES         Integer array of reference frame ID codes 
C                    corresponding to the coverage intervals.
C
C     TOUT           Key specifying time representation on output:
C                    SCLK string, encoded SCLK, ET, UTC or DOY
C
C     FDSP           Flag defining whether name or ID code of the 
C                    FRAME should appear on output.
C
C     TDSP           Flag defining whether summaries have to be written
C                    in tabular or non-tabular format.
C
C     GDSP           Flag defining whether objects with the same
C                    coverage must be grouped together.
C
C     NDSP           Flag requesting display of the name of the frame
C                    associated with CK ID.
C
C$ Detailed_Output
C
C     No output parameters in this subroutine. It prints summary for 
C     a given CK-file.
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
C     None.
C
C$ Examples
C
C     None.
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
C     Y.K. Zaiko      (BERC)
C     B.V. Semenov    (NAIF)
C
C$ Version
C
C-    CKBRIEF Beta Version 2.0.0, 13-OCT-2008 (BVS)
C
C        Added NDSP argument. Changed tabular display heading for
C        frame name display.
C
C-    CKBRIEF Beta Version 1.1.0, 28-DEC-2001 (NJB)
C
C        Removed extraneous white space at end of file so that
C        the final character is a newline.  This was done
C        to suppress compiler warnings.
C
C-    CKBRIEF Beta Version 1.0.0, 17-FEB-1999 (YKZ)(BVS)
C
C-&

C
C     SPICELIB functions.
C
      INTEGER               RTRIM

C
C     Local variables
C
      CHARACTER*(LINESZ)    TDSPH1
      CHARACTER*(LINESZ)    TDSPH2
      
      INTEGER               I
      INTEGER               K
      
C
C     If table output was requested, substitute correct time type in
C     the table header and print it (header) out.
C
      IF ( TDSP ) THEN
      
C
C        Set header template for tabular format of summary display.
C
         IF ( NDSP ) THEN
            IF ( FDSP ) THEN
               TDSPH1 = 'Frames                     ' //
     .                  'Interval Begin #######   ' //
     .                  'Interval End #######     ' //
     .                  'AV  ' //
     .                  'Relative to FRAME'
               TDSPH2 = '-------------------------- ' //
     .                  '------------------------ ' //
     .                  '------------------------ ' //
     .                  '--- ' //
     .                  '-----------------'
            ELSE
               TDSPH1 = 'Frames                     ' //
     .                  'Interval Begin #######   ' //
     .                  'Interval End #######     ' //
     .                  'AV  '
               TDSPH2 = '-------------------------- ' //
     .                  '------------------------ ' //
     .                  '------------------------ ' //
     .                  '--- '
            END IF
         ELSE
            IF ( FDSP ) THEN
               TDSPH1 = 'Objects  ' //
     .                  'Interval Begin #######   ' //
     .                  'Interval End #######     ' //
     .                  'AV  ' //
     .                  'Relative to FRAME'
               TDSPH2 = '-------- ' //
     .                  '------------------------ ' //
     .                  '------------------------ ' //
     .                  '--- ' //
     .                  '-----------------'
            ELSE
               TDSPH1 = 'Objects  ' //
     .                  'Interval Begin #######   ' //
     .                  'Interval End #######     ' //
     .                  'AV  '
               TDSPH2 = '-------- ' //
     .                  '------------------------ ' //
     .                  '------------------------ ' //
     .                  '--- '
            END IF
         END IF

         CALL REPMCW(TDSPH1, '#######', TOUT, RTRIM('#######'), TDSPH1)
         CALL REPMCW(TDSPH1, '#######', TOUT, RTRIM('#######'), TDSPH1)
         
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( TDSPH1 )
         CALL TOSTDO ( TDSPH2 )
         
      END IF

C
C     If option "group together objects with the same coverage" was not
C     specified then objects will be displayed one by one from index
C     1 to index NOBJ.
C
      IF ( .NOT. GDSP ) THEN
      
         DO I = 1, NOBJ
            CALL PRINST (IDS (I), TSTRTS(I), TENDS(I), AVFS(I),
     .                   FRAMES(I), TOUT, FDSP, TDSP, GDSP, NDSP )
         END DO
 
      ELSE
      
C
C        Grouping option was specified. But, do we have anything to 
C        group together (or in other words do we have more that one
C        record?)
C
         IF ( NOBJ .EQ. 1 ) THEN

C
C           No, we don't. Then we display this one (and only :) record.
C
               
            CALL PRINST (IDS(1), TSTRTS(1), TENDS(1), AVFS(1),
     .                   FRAMES(1), TOUT, FDSP, TDSP, GDSP, NDSP )
            
         ELSE
      
C
C           We need to group together objects this the same coverage
C           in summary display. To provide this, there are two
C           loops. Loop for variable I is to find first record
C           in source buffer, which was not displayed yet. Loop for
C           variable K is to find an index of object with the coverage
C           equal to the coverage of previous displayed object (if
C           such exists).
C
            I = 1
         
            DO WHILE ( I .LT. NOBJ )
         
C
C              Look for the next ID that wasn't displayed yet.
C
               DO WHILE ( IDS(I) .EQ. 0 .AND. I .LT. NOBJ )
                  I = I + 1
               END DO
            
C
C              Did we reach the end of the buffer?
C
               IF ( I .EQ. NOBJ ) THEN
            
C
C                 We did. Was the last record in the buffer processed 
C                 already? If not, print in out.
C
                  IF ( IDS(I) .NE. 0 ) THEN
               
                     CALL PRINST (IDS(I), TSTRTS(I), TENDS(I), AVFS(I),
     .                        FRAMES(I), TOUT, FDSP, TDSP, GDSP, NDSP )
     
                  END IF
               
               ELSE

C
C                 Our record is somewhere in the middle of the buffer.
C                 Print it first and after that loop over the rest of
C                 the buffer to see whether we have more records
C                 with the same coverage.
C
                  CALL PRINST (IDS (I), TSTRTS(I), TENDS(I), AVFS(I),
     .                       FRAMES(I), TOUT, FDSP, TDSP, GDSP, NDSP )
                  IDS(I) = 0
     
                  K = I
               
                  DO WHILE( K .LT. NOBJ )
               
                     K = K + 1
                  
                     IF ( TSTRTS(I) .EQ. TSTRTS(K) .AND. 
     .                    TENDS(I)  .EQ. TENDS(K)        ) THEN

C
C                       Print this records and set IDS(K) to 0.
C
                        CALL PRINST(IDS(K),TSTRTS(K),TENDS(K),AVFS(K),
     .                       FRAMES(K), TOUT, FDSP, TDSP, GDSP, NDSP )
                        IDS(K) = 0
                     
                     END IF
                  
                  END DO
                           
               END IF

            END DO
         
         END IF
         
      END IF

C
C     Reset variables saved in PRINST to make sure that summary for 
C     the next CK file will be displayed correctly.
C
      CALL PRINSR
      
      RETURN
      END
