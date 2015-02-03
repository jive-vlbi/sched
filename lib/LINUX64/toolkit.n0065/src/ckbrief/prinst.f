C$Procedure PRINST (Display string of CK-file summary)

      SUBROUTINE PRINST ( ID, TBEGIN, TEND, AVFLAG, FRAME, 
     .                    TOUT, FDSP, TDSP, GDSP, NDSP )
     
C$ Abstract
C
C     Write a single CK-file summary record string to standard 
C     output in requested format.
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

      IMPLICIT NONE
      
      INCLUDE           'ckbrief.inc'

      INTEGER           ID
      DOUBLE PRECISION  TBEGIN
      DOUBLE PRECISION  TEND
      INTEGER           AVFLAG
      INTEGER           FRAME
      CHARACTER*(*)     TOUT
      LOGICAL           FDSP
      LOGICAL           TDSP
      LOGICAL           GDSP
      LOGICAL           NDSP
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ID         I   NAIF ID code of object 
C     TBEGIN     I   Start time of object coverage interval, SCLK ticks
C     TEND       I   End time of object coverage interval, SCLK ticks
C     AVFLAG     I   Angular velocity flag
C     FRAME      I   NAIF ID code of reference frame
C     TOUT       I   Key specifying times representation on output 
C     FDSP       I   Flag defining whether frames name/id is printed
C     TDSP       I   Flag defining tabular/non-tabular summary format
C     GDSP       I   Flag requesting object grouping by coverage
C     NDSP       I   Flag to display frame assosiated with CK ID
C
C$ Detailed_Input
C
C     ID             Integer NAIF ID code found in summaries
C                    of CK-file and to be written to standard output.
C
C     TBEGIN         Begin time for object coverage given as DP 
C                    SCLK ticks.
C
C     TEND           End time for object coverage given as DP 
C                    SCLK ticks.
C
C     AVFLAG         Angular velocities presence flag: 0 - not present,
C                    1 - present, 2 - mixed. 
C
C     FRAME          Integer NAIF ID code of reference frame relative 
C                    to which orientation of the ID was given.
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
C     None. This subroutine displays summary line for a CK-file/segment
C     for subroutine DISPSM.
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
C        Added NDSP argument. Changed to display frame names associated
C        with CK IDs when NDSP is .TRUE..
C
C-    CKBRIEF Beta Version 1.0.0, 17-FEB-1999 (YKZ)(BVS)
C
C-&

C
C     SPICELIB functions
C
      INTEGER               RTRIM      

C
C     Local parameters.
C

C
C     Output fields widths.
C      
      INTEGER               BTIMEW
      PARAMETER           ( BTIMEW  = 24 )
      
      INTEGER               ETIMEW
      PARAMETER           ( ETIMEW  = 24 )
      
      INTEGER               FNAMEW
      PARAMETER           ( FNAMEW  = 32 )
      
      INTEGER               AVFLW
      PARAMETER           ( AVFLW   = 1 )

C
C     Preset output values.
C
      CHARACTER*(*)         YESVAL
      PARAMETER           ( YESVAL  = 'Y' )
      
      CHARACTER*(*)         NOVAL
      PARAMETER           ( NOVAL   = 'N' )
      
      CHARACTER*(*)         AVMVAL
      PARAMETER           ( AVMVAL  = '*' )
      
      CHARACTER*(*)         MXDVAL
      PARAMETER           ( MXDVAL  = 'MIXED' )
      
      CHARACTER*(*)         SAMEFL
      PARAMETER           ( SAMEFL  = '   -- same --' )
      
      CHARACTER*(*)         UNKFL
      PARAMETER           ( UNKFL   = 'NO FRAME FOR #' )
      
C
C     Local variables
C
      CHARACTER*(LINESZ)    OUTLIN
      CHARACTER*(LINESZ)    IDLINE
      CHARACTER*(LINESZ)    TBLINE
      CHARACTER*(LINESZ)    TELINE
      CHARACTER*(LINESZ)    FNLINE
      CHARACTER*(LINESZ)    AVLINE

      DOUBLE PRECISION      TBPREV
      DOUBLE PRECISION      TEPREV
      INTEGER               IDPREV
      INTEGER               SCIDW
      INTEGER               FRCODE
      INTEGER               HINT          

      LOGICAL               FOUND

C
C     Save previous time boundaries and ID code.
C     
      SAVE                  TBPREV
      SAVE                  TEPREV
      SAVE                  IDPREV

C
C     Set initial value to zeros.
C
      DATA                  TBPREV / 0.D0 /            
      DATA                  TEPREV / 0.D0 /            
      DATA                  IDPREV / 0    /            

C
C     Convert all inputs to strings that will appear on output.
C
      IF ( NDSP ) THEN
         SCIDW = 26
         CALL CCIFRM ( 3, ID, FRCODE, IDLINE, HINT, FOUND )
         IF ( .NOT. FOUND ) THEN
            IDLINE = UNKFL
            CALL REPMI ( IDLINE, '#', ID, IDLINE )
         END IF
      ELSE
         SCIDW = 8
         CALL INTSTR( ID, IDLINE )
      END IF

      CALL TIMECN( TBEGIN, ID, TOUT, TBLINE )
      CALL TIMECN( TEND,   ID, TOUT, TELINE )
      
      IF      ( AVFLAG .EQ. 2 ) THEN 
         AVLINE = AVMVAL
      ELSE IF ( AVFLAG .EQ. 1 ) THEN 
         AVLINE = YESVAL
      ELSE
         AVLINE = NOVAL
      END IF
      
      CALL FRMNAM( FRAME, FNLINE )
      IF ( FNLINE .EQ. ' ' ) THEN
         IF ( FRAME .EQ. 0 ) THEN
            FNLINE = MXDVAL
         ELSE           
            CALL INTSTR ( FRAME, FNLINE )
         END IF
      END IF
      
C
C     Make up output string and print them depending on what kind of 
C     output format was requested.
C      
      IF ( TDSP ) THEN

C
C        For table output, set output line template depending on 
C        whether FRAME display was requested.
C
         IF ( FDSP ) THEN
            OUTLIN = '# # # #   #'
         ELSE
            OUTLIN = '# # # #'
         END IF
C
C        Check whether coverage is the same as previous one and 
C        reassign begin and end time to 'same' flag if so.
C
         IF ( TBEGIN .EQ. TBPREV .AND. TEND   .EQ. TEPREV .AND. 
     .        TBLINE .NE. NOTIME .AND. TELINE .NE. NOTIME       ) THEN
            TBLINE = SAMEFL
            TELINE = SAMEFL   
         END IF

C
C        Substitute string and print out the line.
C
         CALL REPMCW( OUTLIN, '#', IDLINE, SCIDW,  OUTLIN )
         CALL REPMCW( OUTLIN, '#', TBLINE, BTIMEW, OUTLIN )
         CALL REPMCW( OUTLIN, '#', TELINE, ETIMEW, OUTLIN )
         CALL REPMCW( OUTLIN, '#', AVLINE, AVFLW,  OUTLIN )
         CALL REPMCW( OUTLIN, '#', FNLINE, FNAMEW, OUTLIN )
         
C
C        Display the line.
C
         CALL TOSTDO ( OUTLIN(:RTRIM(OUTLIN)) )
         
      ELSE
      
C
C        If grouping flag is set, we display single coverage line for 
C        multiple objects. If it's not set, we display multiple 
C        coverage lines for a single object. Also when GDSP set we do
C        NOT display angular velocity flags or FRAME names/ids.
C
         IF ( GDSP ) THEN
         
            IF ( TBEGIN .EQ. TBPREV .AND. TEND .EQ. TEPREV ) THEN
            
C
C              This is another object in a group with the same 
C              coverage. Display just the object ID.
C
               OUTLIN = '         #'
               
            ELSE

C
C              This is the first object in a group with a different
C              coverage. Display blank line, coverage and ID of the 
C              first object.
C
               CALL TOSTDO ( ' ' )
               
               OUTLIN = 'Begin #: #  End #: # '
               CALL REPMC ( OUTLIN, '#', TOUT, OUTLIN)
               CALL REPMCW( OUTLIN, '#', TBLINE, BTIMEW, OUTLIN )
               CALL REPMC ( OUTLIN, '#', TOUT, OUTLIN)
               CALL REPMCW( OUTLIN, '#', TELINE, ETIMEW, OUTLIN )
               CALL TOSTDO ( OUTLIN(:RTRIM(OUTLIN)) )

               IF ( NDSP ) THEN
                  OUTLIN = 'Frames:  #'
               ELSE
                  OUTLIN = 'Objects: #'
               END IF
               
            END IF
            
            CALL REPMCW( OUTLIN, '#', IDLINE, SCIDW,  OUTLIN )
            CALL TOSTDO ( OUTLIN(:RTRIM(OUTLIN)) )
               
         ELSE
         
C
C           No grouping by time was requested. So, display contains 
C           sets of coverage intervals for a particular object.
C
            IF ( ID .EQ. IDPREV ) THEN

C
C              It's the same object. Print out only interval.
C
               IF ( FDSP ) THEN
                  OUTLIN = '  # # #   #'
               ELSE
                  OUTLIN = '  # # #'
               END IF

            ELSE
            
C
C              It's another object. Print object ID, header and 
C              the first interval.
C
               CALL TOSTDO ( ' ' )
               
               IF ( NDSP ) THEN
                  OUTLIN = 'Frame:   #'
               ELSE
                  OUTLIN = 'Object:  #'
               END IF
               CALL REPMCW( OUTLIN, '#', IDLINE, SCIDW,  OUTLIN )
               CALL TOSTDO ( OUTLIN(:RTRIM(OUTLIN)) )
               
               IF ( FDSP ) THEN
               
                  OUTLIN = '  Interval Begin #######   ' //
     .               'Interval End #######     AV  Relative to FRAME'
                  CALL REPMCW( OUTLIN, '#######', TOUT, 
     .                         RTRIM('#######'), OUTLIN)
                  CALL REPMCW( OUTLIN, '#######', TOUT, 
     .                         RTRIM('#######'), OUTLIN)
                  CALL TOSTDO( OUTLIN(:RTRIM(OUTLIN)) )

                  OUTLIN = '  ------------------------ ' //
     .               '------------------------ --- ----------------- '
                  CALL TOSTDO ( OUTLIN(:RTRIM(OUTLIN)) )

                  OUTLIN = '  # # #   #'
                  
               ELSE
               
                  OUTLIN = '  Interval Begin #######   ' //
     .               'Interval End #######     AV  '
                  CALL REPMCW( OUTLIN, '#######', TOUT, 
     .                         RTRIM('#######'), OUTLIN)
                  CALL REPMCW( OUTLIN, '#######', TOUT, 
     .                         RTRIM('#######'), OUTLIN)
                  CALL TOSTDO ( OUTLIN(:RTRIM(OUTLIN)) )

                  OUTLIN = '  ------------------------ ' //
     .               '------------------------ --- '
                  CALL TOSTDO ( OUTLIN(:RTRIM(OUTLIN)) )

                  OUTLIN = '  # # #'
                  
               END IF
               
            END IF
                           
            CALL REPMCW( OUTLIN, '#', TBLINE, BTIMEW, OUTLIN )
            CALL REPMCW( OUTLIN, '#', TELINE, ETIMEW, OUTLIN )
            CALL REPMCW( OUTLIN, '#', AVLINE, AVFLW,  OUTLIN )
            CALL REPMCW( OUTLIN, '#', FNLINE, FNAMEW, OUTLIN )
            CALL TOSTDO( OUTLIN(:RTRIM(OUTLIN)) )
            
         END IF
         
      END IF
      
C
C     Reassign saved variables.
C
      TBPREV = TBEGIN
      TEPREV = TEND
      IDPREV = ID
                 
      RETURN
      
      
C$Procedure PRINSR (Reset saved variables)

      ENTRY PRINSR
     
C$ Abstract
C
C     This entry point resets saved ID and start and stop time)
C     to make sure that CKBRIEF generates table headers correctly.
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
C
C     None.
C      
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
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
C-&
      TBPREV = 0.D0
      TEPREV = 0.D0
      IDPREV = 0
      
      RETURN

      END
