C$Procedure      ZZMCKDMP ( Dump CK to test MSOPCK )
 
      SUBROUTINE ZZMCKDMP ( CKFILE )
 
C$ Abstract
C
C     Private module. Dumps contents of a CK file to the screen to
C     support testing of MSOPCK.
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
C     CK.REQ
C     MSOPCK.UG
C
C$ Keywords
C
C     CK
C
C$ Declarations

      IMPLICIT NONE
 
      CHARACTER*(*)   CKFILE
 
C$ Brief_I/O
C
C     VARIABLE  I/O              DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CKFILE     I   Name of the CK file to be dumped   
C
C$ Detailed_Input
C
C     TBD
C
C$ Detailed_Output
C
C     TBD
C
C$ Parameters
C
C     TBD
C
C$ Particulars
C
C     TBD
C
C$ Examples
C
C     TBD
C
C$ Restrictions
C
C     TBD
C
C$ Exceptions
C
C     TBD
C
C$ Files
C
C     TBD      
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    Version 1.0.0, 01-SEP-2006 (BVS)
C
C-&

 
C
C     Local Parameters
C

      CHARACTER*(*)         TIMPIC
      PARAMETER           ( TIMPIC = 'xxxxxxxxxxxxxx.xxx' )

      CHARACTER*(*)         QPIC
      PARAMETER           ( QPIC = '+0.xxxxxxxx' )

      CHARACTER*(*)         RPIC
      PARAMETER           ( RPIC = '+0.xxxxxxxx' )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
 
C
C     Local Variables
C
      CHARACTER*(LINLEN)    NUMBER
      CHARACTER*(LINLEN)    OUTLIN
 
      DOUBLE PRECISION      DC      ( 2 )
      DOUBLE PRECISION      RECORD  ( 10 )
      DOUBLE PRECISION      SUM     ( 5 )
      DOUBLE PRECISION      INTBEG
 
      INTEGER               ADDR
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IC      ( 6 )
      INTEGER               J
      INTEGER               NIDIR
      INTEGER               NUMINT
      INTEGER               NUMREC
      INTEGER               SEGMNT
 
      LOGICAL               FOUND
 
C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     Check in.
C
      CALL CHKIN( 'ZZMCKDMP' )

C
C     Load CK file.
C 
      CALL CKLPF ( CKFILE, HANDLE )

C
C     Start scanning through the segments from the front of the file.
C
      CALL DAFBFS ( HANDLE )
      CALL DAFFNA ( FOUND )
 
      SEGMNT = 1
 
      DO WHILE ( FOUND )
 
C
C        Get and unpack the segment summary.
C
         CALL DAFGS ( SUM )
         CALL DAFUS ( SUM, 2, 6, DC, IC )

C
C        Dump the segment summary.
C
         CALL TOSTDO( ' ' )

         OUTLIN = 'SEGMENT #'
         CALL REPMI ( OUTLIN, '#', SEGMNT, OUTLIN )
         CALL TOSTDO( OUTLIN )

         OUTLIN = '   CKID #'
         CALL REPMI ( OUTLIN, '#', IC(1), OUTLIN )
         CALL TOSTDO( OUTLIN )

         OUTLIN = '  REFID #'
         CALL REPMI ( OUTLIN, '#', IC(2), OUTLIN )
         CALL TOSTDO( OUTLIN )

         OUTLIN = '   TYPE #'
         CALL REPMI ( OUTLIN, '#', IC(3), OUTLIN )
         CALL TOSTDO( OUTLIN )

         OUTLIN = ' AVFLAG #'
         CALL REPMI ( OUTLIN, '#', IC(4), OUTLIN )
         CALL TOSTDO( OUTLIN )

         CALL DPFMT ( DC(1), TIMPIC, NUMBER)
         OUTLIN = '  BTIME ' // NUMBER
         CALL TOSTDO( OUTLIN )

         CALL DPFMT ( DC(2), TIMPIC, NUMBER)
         OUTLIN = '  ETIME ' // NUMBER
         CALL TOSTDO( OUTLIN )
 
C
C        Branch depending on segment type.
C
         IF      ( IC(3) .EQ. 1 ) THEN
 
C
C           Obtain the number of pointing records.
C
            CALL CKNR01 ( HANDLE, SUM, NUMREC )

C
C           Dump data records one by one.
C
            DO J = 1, NUMREC
 
               CALL CKGR01 ( HANDLE, SUM, J, RECORD )

               OUTLIN = 'T(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               CALL DPFMT ( RECORD(1), TIMPIC, NUMBER )
               CALL SUFFIX( NUMBER, 1, OUTLIN )
               CALL TOSTDO( OUTLIN )

               OUTLIN = 'Q(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               DO I = 1, 4
                  CALL DPFMT ( RECORD(I+1), QPIC, NUMBER )
                  IF ( NUMBER(1:11) .EQ. '+0.00000000' .OR.
     .                 NUMBER(1:11) .EQ. '-0.00000000'      ) THEN
                     NUMBER(1:11) = ' 0.00000000'
                  END IF
                  CALL SUFFIX( NUMBER, 1, OUTLIN )
               END DO
               CALL TOSTDO( OUTLIN )
 
               IF ( IC(4) .NE. 0 ) THEN                     
                  OUTLIN = 'R(#)'
                  CALL REPMI ( OUTLIN, '#', J, OUTLIN )
                  DO I = 1, 3
                     CALL DPFMT ( RECORD(I+5), RPIC, NUMBER)
                     IF ( NUMBER(1:11) .EQ. '+0.00000000' .OR.
     .                    NUMBER(1:11) .EQ. '-0.00000000' ) THEN
                        NUMBER(1:11) = ' 0.00000000'
                     END IF
                     CALL SUFFIX( NUMBER, 1, OUTLIN )
                  END DO
                  CALL TOSTDO( OUTLIN )
               END IF

            END DO

         ELSE IF ( IC(3) .EQ. 2 ) THEN
 
C
C           Obtain the number of pointing records.
C
            CALL CKNR02 ( HANDLE, SUM, NUMREC )

C
C           Dump data records one by one.
C
            DO J = 1, NUMREC
 
               CALL CKGR02 ( HANDLE, SUM, J, RECORD )

               OUTLIN = 'B(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               CALL DPFMT ( RECORD(1), TIMPIC, NUMBER )
               CALL SUFFIX( NUMBER, 1, OUTLIN )
               CALL TOSTDO( OUTLIN )

               OUTLIN = 'E(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               CALL DPFMT ( RECORD(2), TIMPIC, NUMBER )
               CALL SUFFIX( NUMBER, 1, OUTLIN )
               CALL TOSTDO( OUTLIN )

               OUTLIN = 'S(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               CALL DPFMT ( RECORD(3), TIMPIC, NUMBER )
               CALL SUFFIX( NUMBER, 1, OUTLIN )
               CALL TOSTDO( OUTLIN )

               OUTLIN = 'Q(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               DO I = 1, 4
                  CALL DPFMT ( RECORD(I+3), QPIC, NUMBER )
                  IF ( NUMBER(1:11) .EQ. '+0.00000000' .OR.
     .                 NUMBER(1:11) .EQ. '-0.00000000' ) THEN
                     NUMBER(1:11) = ' 0.00000000'
                  END IF
                  CALL SUFFIX( NUMBER, 1, OUTLIN )
               END DO
               CALL TOSTDO( OUTLIN )
 
               OUTLIN = 'R(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               DO I = 1, 3
                  CALL DPFMT ( RECORD(I+7), RPIC, NUMBER)
                  IF ( NUMBER(1:11) .EQ. '+0.00000000' .OR.
     .                 NUMBER(1:11) .EQ. '-0.00000000' ) THEN
                     NUMBER(1:11) = ' 0.00000000'
                  END IF
                  CALL SUFFIX( NUMBER, 1, OUTLIN )
               END DO
               CALL TOSTDO( OUTLIN )

            END DO

         ELSE IF ( IC(3) .EQ. 3 ) THEN

C
C           Obtain the number of pointing records.
C
            CALL CKNR03 ( HANDLE, SUM, NUMREC )

C
C           Dump data records one by one.
C
            DO J = 1, NUMREC
 
               CALL CKGR03 ( HANDLE, SUM, J, RECORD )

               OUTLIN = 'T(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               CALL DPFMT ( RECORD(1), TIMPIC, NUMBER )
               CALL SUFFIX( NUMBER, 1, OUTLIN )
               CALL TOSTDO( OUTLIN )

               OUTLIN = 'Q(#)'
               CALL REPMI ( OUTLIN, '#', J, OUTLIN )
               DO I = 1, 4
                  CALL DPFMT ( RECORD(I+1), QPIC, NUMBER )
                  IF ( NUMBER(1:11) .EQ. '+0.00000000' .OR.
     .                 NUMBER(1:11) .EQ. '-0.00000000' ) THEN
                     NUMBER(1:11) = ' 0.00000000'
                  END IF
                  CALL SUFFIX( NUMBER, 1, OUTLIN )
               END DO
               CALL TOSTDO( OUTLIN )
 
               IF ( IC(4) .NE. 0 ) THEN                     
                  OUTLIN = 'R(#)'
                  CALL REPMI ( OUTLIN, '#', J, OUTLIN )
                  DO I = 1, 3
                     CALL DPFMT ( RECORD(I+5), RPIC, NUMBER)
                     IF ( NUMBER(1:11) .EQ. '+0.00000000' .OR.
     .                    NUMBER(1:11) .EQ. '-0.00000000' ) THEN
                        NUMBER(1:11) = ' 0.00000000'
                     END IF
                     CALL SUFFIX( NUMBER, 1, OUTLIN )
                  END DO
                  CALL TOSTDO( OUTLIN )
               END IF

            END DO

C
C           Dump interval start times.
C
            CALL DAFGDA ( HANDLE, IC(6)-1, IC(6)-1, INTBEG )
            NUMINT = INTBEG
            NIDIR = ( NUMINT - 1 ) / 100

            DO I = 1, NUMINT

               ADDR = IC(6) - 2 - NIDIR - NUMINT + I
               CALL DAFGDA ( HANDLE, ADDR, ADDR, INTBEG )

               OUTLIN = 'I(#)'
               CALL REPMI ( OUTLIN, '#', I, OUTLIN )
               CALL DPFMT ( INTBEG, TIMPIC, NUMBER )
               CALL SUFFIX( NUMBER, 1, OUTLIN )
               CALL TOSTDO( OUTLIN )

            END DO

         ELSE 
 
            OUTLIN = 'Dump for this segment type is not supported.'
            CALL TOSTDO ( OUTLIN )

         END IF

         CALL TOSTDO( ' ' )
 
C
C        Fetch the next segment.
C
         SEGMNT = SEGMNT + 1
         CALL DAFFNA ( FOUND )
 
      END DO


C
C     Check in.
C
      CALL CHKOUT( 'ZZMCKDMP' )

      RETURN
      END
