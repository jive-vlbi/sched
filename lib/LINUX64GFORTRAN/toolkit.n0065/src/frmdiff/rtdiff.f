C$Procedure      RTDIFF ( Rotation differences )

      SUBROUTINE RTDIFF( Q1, AV1, Q2, AV2, CMPWIN, NITR, EPOCH, DIFTYP,
     .                   AVFLG, AVFFLG, TIMFMT, SCLKID, AXES, AUNITS,
     .                   SAMPLE, SIGDIG )

C$ Abstract
C
C     Computes the differences between the rotations and AVs provided
C     on the input and writes summary of requested type to the screen.
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
C     FRMDIFF.UG
C
C$ Keywords
C
C     FRAMES
C     ROTATIONS
C     CK
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'frmdiff.inc'

      DOUBLE PRECISION      Q1    ( 4, * )
      DOUBLE PRECISION      AV1   ( 3, * )
      DOUBLE PRECISION      Q2    ( 4, * )
      DOUBLE PRECISION      AV2   ( 3, * )
      DOUBLE PRECISION      CMPWIN ( LBCELL : * )
      INTEGER               NITR
      DOUBLE PRECISION      EPOCH ( * )
      CHARACTER*(*)         DIFTYP
      LOGICAL               AVFLG
      LOGICAL               AVFFLG
      CHARACTER*(*)         TIMFMT
      INTEGER               SCLKID
      INTEGER               AXES   ( 3 )
      CHARACTER*(*)         AUNITS
      LOGICAL               SAMPLE
      INTEGER               SIGDIG

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     Q1         I   First quaternion buffer
C     AV1        I   First AV buffer
C     Q2         I   Second quaternion buffer
C     AV2        I   Second AV buffer
C     CMPWIN     I   Comparison window
C     NITR       I   Number of point in Q, AV and EPOCH buffers
C     EPOCH      I   Epoch buffer
C     DIFTYP     I   Type of report to produce
C     AVFLG      I   Angular velocity flag
C     AVFFLG     I   Angular velocity frame flag
C     TIMFMT     I   Output time format
C     SCLKID     I   ID to use in SCLK conversions
C     AXES       I   Rotation axes for output Euler angles.
C     AUNITS     I   Units for output Euler angles.
C     SAMPLE     I   Flag indicating a sampling run
C     SIGDIG     I   Number of significant digits
C
C$ Detailed_Input
C
C     TBD.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     See include file.
C
C$ Exceptions
C
C     TBD.
C
C$ Files
C
C     All kernels needed for time conversions (ET->UTC, ET->SCLK) must
C     be loaded prior to calling this routine.
C
C$ Particulars
C
C     TBD.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     See files.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    Version 2.0.0, 27-FEB-2012 (BVS)
C
C        Changed the calling sequence to include additional input
C        SIGDIG.
C
C        Updated to use SIGDIG to specify the number of significant
C        digits in numeric times and numbers in all dump reports.
C
C-    Version 1.0.0, 30-AUG-2008 (BVS)
C
C        Initial version.
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      DPMIN

      INTEGER               RTRIM
      INTEGER               WNCARD

      LOGICAL               EQSTR

C
C     Local parameters.
C
      CHARACTER*(*)         DURPIC
      PARAMETER           ( DURPIC = 'mmmmmmmmmmmm.mmmmmm' )

C
C     Local variables
C
      INTEGER               I
      INTEGER               J
      INTEGER               K

      CHARACTER*(DSPSIZ)    HLPSTR
      CHARACTER*(LINSIZ)    DMPSTR
      CHARACTER*(LINSIZ)    DATSTR
      CHARACTER*(LINSIZ)    HDRSTR
      CHARACTER*(1)         AXISNM   ( 3 )

      DOUBLE PRECISION      M1     ( 3, 3 )
      DOUBLE PRECISION      M2     ( 3, 3 )
      DOUBLE PRECISION      MDIFF  ( 3, 3 )
      DOUBLE PRECISION      QDIFFS ( 4 )
      DOUBLE PRECISION      QDIFFO ( 4 )
      DOUBLE PRECISION      AVDIFF ( 3 )
      DOUBLE PRECISION      AVDTMP ( 3 )
      DOUBLE PRECISION      ANGLE  ( 3 )
      DOUBLE PRECISION      RAXIS  ( 3 )
      DOUBLE PRECISION      RANGLE
      DOUBLE PRECISION      HDP
      DOUBLE PRECISION      AVNORM

      DOUBLE PRECISION      RANMAX
      DOUBLE PRECISION      AVDMAX

      DOUBLE PRECISION      RANMET
      DOUBLE PRECISION      AVDMET

      DOUBLE PRECISION      RANAV
      DOUBLE PRECISION      AVDAV

      DOUBLE PRECISION      RANRMS
      DOUBLE PRECISION      AVDRMS

      INTEGER               INFCNT
      CHARACTER*(LINSIZ)    INFMSG ( INFMAX )
      LOGICAL               INFPRT ( INFMAX )


C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     Axis names.
C
      DATA    AXISNM        / 'X', 'Y', 'Z' /

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RTDIFF' )
      END IF

C
C     Generate reports based on requested report type.
C
      IF ( EQSTR( DIFTYP, DCVAL  ) ) THEN

C
C        Do coverage interval dump.
C
         HDRSTR = '# interval_start, interval_stop, ' //
     .            'interval_duration_sec, interval_duration_string'
         DATSTR = '# #'

         CALL TOSTDO( HDRSTR )

         DO I = 1, WNCARD(CMPWIN)*2, 2

C
C           Reset output line template.
C
            DMPSTR = DATSTR

C
C           Insert interval start and end.
C
            CALL ET2STR( CMPWIN(I),   TIMFMT, SCLKID, SIGDIG, HLPSTR )
            CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )

            CALL ET2STR( CMPWIN(I+1), TIMFMT, SCLKID, SIGDIG, HLPSTR )
            CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )

C
C           Append interval duration as seconds and D:H:M:S string.
C
            CALL DPFMT ( CMPWIN(I+1)-CMPWIN(I),
     .                   DURPIC,
     .                   DMPSTR(RTRIM(DMPSTR)+1:) )

            CALL DR2STR( CMPWIN(I+1)-CMPWIN(I),
     .                   DMPSTR(RTRIM(DMPSTR)+1:) )

C
C           Print output string.
C
            CALL TOSTDO( DMPSTR )

         END DO

C
C        End of coverage interval dump.
C

      ELSE IF ( EQSTR( DIFTYP, DGVAL  ) ) THEN

C
C        Do coverage gap dump.
C

         HDRSTR = '# gap_start, gap_stop, ' //
     .            'gap_duration_sec, gap_duration_string'
         DATSTR = '# #'

C
C        Display table header is there is at least one gap.
C
         IF ( .NOT. ( WNCARD(CMPWIN) .EQ. 1 ) ) THEN            
            CALL TOSTDO( HDRSTR )
         END IF


         DO I = 2, (WNCARD(CMPWIN)-1)*2, 2

C
C           Reset output line template.
C
            DMPSTR = DATSTR

C
C           Insert gap start and end.
C
            CALL ET2STR( CMPWIN(I),   TIMFMT, SCLKID, SIGDIG, HLPSTR )
            CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )

            CALL ET2STR( CMPWIN(I+1), TIMFMT, SCLKID, SIGDIG, HLPSTR )
            CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )

C
C           Append interval duration as seconds and D:H:M:S string.
C
            CALL DPFMT ( CMPWIN(I+1)-CMPWIN(I),
     .                   DURPIC,
     .                   DMPSTR(RTRIM(DMPSTR)+1:) )

            CALL DR2STR( CMPWIN(I+1)-CMPWIN(I),
     .                   DMPSTR(RTRIM(DMPSTR)+1:) )

C
C           Print output string.
C
            CALL TOSTDO( DMPSTR )

         END DO

C
C        If there no gaps, report it.
C
         IF ( WNCARD(CMPWIN) .EQ. 1 ) THEN

            CALL TOSTDO( ' ' )
            CALL TOSTDO( 'There are no gaps in coverage.' )
            CALL TOSTDO( ' ' )

         END IF

C
C        End of coverage gap dump.
C

      ELSE IF ( EQSTR( DIFTYP, DMVAL  ) .OR.
     .          EQSTR( DIFTYP, DQSVAL ) .OR.
     .          EQSTR( DIFTYP, DQOVAL ) .OR.
     .          EQSTR( DIFTYP, DAAVAL ) .OR.
     .          EQSTR( DIFTYP, DEAVAL )      ) THEN

C
C        Do difference dumps.
C

C
C        Set header and data line strings depending on requested dump
C        type.
C
         IF      ( EQSTR( DIFTYP, DMVAL  ) ) THEN

            HDRSTR = '# time, m11, m12, m13, m21, m22, ' //
     .               'm23, m31, m32, m33'
            DATSTR = '# # # # # # # # # #'

         ELSE IF ( EQSTR( DIFTYP, DQSVAL ) ) THEN

            HDRSTR = '# time, q_cos, q_sin1, q_sin2, q_sin3'
            DATSTR = '# # # # #'

         ELSE IF ( EQSTR( DIFTYP, DQOVAL ) ) THEN

            HDRSTR = '# time, q_sin1, q_sin2, q_sin3, q_cos'
            DATSTR = '# # # # #'

         ELSE IF ( EQSTR( DIFTYP, DEAVAL ) ) THEN

            HDRSTR = '# angles are shown in $.'
            CALL REPMC ( HDRSTR, '$', AUNITS, HDRSTR )
            CALL TOSTDO( HDRSTR )

            HDRSTR = '#'
            CALL TOSTDO( HDRSTR )

            HDRSTR = '# time, ang3_about_$, ang2_about_$, ' //
     .               'ang1_about_$'
            DO I = 1, 3
               CALL REPMC ( HDRSTR, '$', AXISNM(AXES(I)), HDRSTR )
            END DO
            DATSTR = '# # # #'

         ELSE IF ( EQSTR( DIFTYP, DAAVAL ) ) THEN

            HDRSTR = '# angle is shown in $.'
            CALL REPMC ( HDRSTR, '$', AUNITS, HDRSTR )
            CALL TOSTDO( HDRSTR )

            HDRSTR = '#'
            CALL TOSTDO( HDRSTR )

            HDRSTR = '# time, angle, axis_x, axis_y, axis_z'
            DATSTR = '# # # # #'

         END IF

C
C        Modify header and data line strings for AVs if needed.
C
         IF ( AVFLG ) THEN

            CALL SUFFIX( ', av_x, av_y, av_z', 0, HDRSTR )
            CALL SUFFIX( ' # # #',             0, DATSTR )

            IF ( EQSTR( DIFTYP, DAAVAL ) ) THEN
               CALL SUFFIX( ', av_magnitude', 0, HDRSTR )
               CALL SUFFIX( ' #',             0, DATSTR )
            END IF

         END IF

C
C        Print header string.
C
         CALL TOSTDO( HDRSTR )

C
C        Compute and print difference for each point.
C
         DO I = 1, NITR

C
C           Reset output line template.
C
            DMPSTR = DATSTR

C
C           Put time in the output string.
C
            CALL ET2STR( EPOCH(I), TIMFMT, SCLKID, SIGDIG, HLPSTR )
            CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )

C
C           Compute rotation difference.
C
            CALL Q2M  ( Q1(1,I), M1 )
            CALL Q2M  ( Q2(1,I), M2 )
            CALL MXMT ( M1, M2, MDIFF )

C
C           Put rotation difference in the output string in requested
C           format.
C
            IF      ( EQSTR( DIFTYP, DMVAL  ) ) THEN

C
C              Package matrix.
C
               DO J = 1, 3
                  DO K = 1, 3
                     CALL DPSTRP( MDIFF(J,K), SIGDIG, HLPSTR )
                     CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
                  END DO
               END DO

            ELSE IF ( EQSTR( DIFTYP, DQSVAL ) ) THEN

C
C              Compute and package SPICE quaternion.
C
               CALL M2Q( MDIFF, QDIFFS )

               DO J = 1, 4
                  CALL DPSTRP( QDIFFS(J), SIGDIG, HLPSTR )
                  CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               END DO

            ELSE IF ( EQSTR( DIFTYP, DQOVAL ) ) THEN

C
C              Compute and package ``other'' quaternion.
C
               CALL M2Q( MDIFF, QDIFFS )

               QDIFFO(1) = -QDIFFS(2)
               QDIFFO(2) = -QDIFFS(3)
               QDIFFO(3) = -QDIFFS(4)
               QDIFFO(4) =  QDIFFS(1)

               DO J = 1, 4
                  CALL DPSTRP( QDIFFO(J), SIGDIG, HLPSTR )
                  CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               END DO

            ELSE IF ( EQSTR( DIFTYP, DEAVAL  ) ) THEN

C
C              Compute and package Euler angles.
C
               CALL M2EUL( MDIFF, AXES(1),  AXES(2),  AXES(3),
     .                            ANGLE(1), ANGLE(2), ANGLE(3) )

               DO J = 1, 3
                  CALL CONVRT( ANGLE(J), 'RADIANS', AUNITS, HDP )
                  CALL DPSTRP( HDP, SIGDIG, HLPSTR )
                  CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               END DO

            ELSE IF ( EQSTR( DIFTYP, DAAVAL ) ) THEN

C
C              Compute and package angle/axis.
C
               CALL RAXISA( MDIFF, RAXIS, RANGLE )

               CALL CONVRT( RANGLE, 'RADIANS', AUNITS, HDP )
               CALL DPSTRP( HDP, SIGDIG, HLPSTR )
               CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )

               DO J = 1, 3
                  CALL DPSTRP( RAXIS(J), SIGDIG, HLPSTR )
                  CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               END DO

            ELSE

               CALL SETMSG ( 'There is a bug in the program. '       //
     .                       'Please, contact NAIF.'                 )
               CALL SIGERR ( 'SPICE(FRMDIFFBUG7)'                    )

            END IF

C
C           Add AV difference if needed.
C
            IF ( AVFLG ) THEN

               CALL VSUB( AV1(1,I), AV2(1,I), AVDIFF )

C
C              If requested, rotate AV difference to the first ``to''
C              frame.
C
               IF ( AVFFLG ) THEN

                  CALL Q2M  ( Q1(1,I), M1 )
                  CALL MXV  ( M1, AVDIFF, AVDTMP )
                  CALL MOVED( AVDTMP, 3, AVDIFF )

               END IF

               DO J = 1, 3
                  CALL DPSTRP( AVDIFF(J), SIGDIG, HLPSTR )
                  CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               END DO

C
C              If we dump angle/axis, add AV difference magnitude to
C              output.
C
               IF ( EQSTR( DIFTYP, DAAVAL ) ) THEN
                  CALL DPSTRP( VNORM( AVDIFF ), SIGDIG, HLPSTR )
                  CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               END IF

            END IF

C
C           Print output string.
C
            CALL TOSTDO( DMPSTR )

         END DO

C
C        End of difference dumps.
C

      ELSE IF ( EQSTR( DIFTYP, BASVAL ) .OR.
     .          EQSTR( DIFTYP, STSVAL )      ) THEN

C
C        Do basic and stats dumps.
C

C
C        Set initial values.
C
         RANMAX = DPMIN()
         AVDMAX = DPMIN()

         RANAV  = 0.D0
         AVDAV  = 0.D0

         RANRMS = 0.D0
         AVDRMS = 0.D0

C
C        Compute difference for each point and accumulate statistics
C        as we go.
C
         DO I = 1, NITR

C
C           Compute rotation difference and angle/axis. Reset maximum
C           value. Add current differences and squares to cumulative
C           values.
C
            CALL Q2M  ( Q1(1,I), M1 )
            CALL Q2M  ( Q2(1,I), M2 )
            CALL MXMT ( M1, M2, MDIFF )
            CALL RAXISA( MDIFF, RAXIS, RANGLE )

            IF ( RANGLE .GT. RANMAX ) THEN
               RANMAX = RANGLE
               RANMET = EPOCH(I)
            END IF

            RANAV  = RANAV  + RANGLE
            RANRMS = RANRMS + RANGLE * RANGLE

C
C           Same for AVs, if requested.
C
            IF ( AVFLG ) THEN

               CALL VSUB( AV1(1,I), AV2(1,I), AVDIFF )
               AVNORM = VNORM( AVDIFF )

               IF ( AVNORM .GT. AVDMAX ) THEN
                  AVDMAX = AVNORM
                  AVDMET = EPOCH(I)
               END IF

               AVDAV  = AVDAV  + AVNORM
               AVDRMS = AVDRMS + AVNORM * AVNORM

            END IF

         END DO

C
C        Compute average and RMS.
C
         RANAV  = RANAV / DBLE( NITR )
         RANRMS = DSQRT( RANRMS / DBLE( NITR ) )

         IF ( AVFLG ) THEN
            AVDAV  = AVDAV / DBLE( NITR )
            AVDRMS = DSQRT( AVDRMS / DBLE( NITR ) )
         END IF

C
C        Tag all lines as printable.
C
         INFCNT = INFMAX
         DO I = 1, INFCNT
            INFPRT(I) = .TRUE.
         END DO

C
C        Populate report text based report type.
C
         IF      ( EQSTR( DIFTYP, BASVAL ) ) THEN

C
C           Set template for basic report.
C
            INFMSG(  1 ) = ' '
            INFMSG(  2 ) = 'Absolute difference magnitudes:'
            INFMSG(  3 ) = ' '
            INFMSG(  4 ) = '                                     '
     .           //        'maximum                 average'
            INFMSG(  5 ) = ' '
            INFMSG(  6 ) = '  Rotation (rad):             #      #'
            INFMSG(  7 ) = '  Angular Velocity (rad/s):   #      #'
            INFMSG(  8 ) = ' '

            INFCNT = 8

C
C           Tweak template for sampling case.
C
            IF ( SAMPLE ) THEN
               INFMSG(  2 ) = 'Absolute magnitudes:'
            END IF

C
C           Fill in maximum and average rotation value.
C
            CALL DPSTRP( RANMAX, DEFSDG, HLPSTR )
            CALL REPMC ( INFMSG( 6 ), '#', HLPSTR, INFMSG( 6 ) )

            CALL DPSTRP( RANAV,  DEFSDG, HLPSTR )
            CALL REPMC ( INFMSG( 6 ), '#', HLPSTR, INFMSG( 6 ) )

C
C           If requested, fill in maximum and average AV values.
C
            IF ( AVFLG ) THEN

               CALL DPSTRP( AVDMAX, DEFSDG, HLPSTR )
               CALL REPMC ( INFMSG(  7 ), '#', HLPSTR, INFMSG(  7 ) )

               CALL DPSTRP( AVDAV,  DEFSDG, HLPSTR )
               CALL REPMC ( INFMSG(  7 ), '#', HLPSTR, INFMSG(  7 ) )

            ELSE

C
C              Remove AV line from the report.
C
               INFPRT(7) = .FALSE.

            END IF

         ELSE IF ( EQSTR( DIFTYP, STSVAL ) ) THEN

C
C           Set template for stats report.
C
            INFMSG(  1 ) = ' '
            INFMSG(  2 ) = '1) Average difference'
            INFMSG(  3 ) = ' '
            INFMSG(  4 ) = '   1a) Rotation (rad):                 '
     .           //             '              #'
            INFMSG(  5 ) = ' '
            INFMSG(  6 ) = '   1b) Angular velocity (rad/s):       '
     .           //             '              #'
            INFMSG(  7 ) = ' '
            INFMSG(  8 ) = ' '
            INFMSG(  9 ) = '2) RMS of difference'
            INFMSG( 10 ) = ' '
            INFMSG( 11 ) = '   2a) Rotation (rad):                 '
     .           //             '              #'
            INFMSG( 12 ) = ' '
            INFMSG( 13 ) = '   2b) Angular velocity (rad/s):       '
     .           //             '              #'
            INFMSG( 14 ) = ' '
            INFMSG( 15 ) = ' '
            INFMSG( 16 ) = '3) Maximum rotation difference'
            INFMSG( 17 ) = ' '
            INFMSG( 18 ) = '   3a) Rotation (rad):                 '
     .           //             '              #'
            INFMSG( 19 ) = ' '
            INFMSG( 20 ) = '   3b) Epoch (TDB, seconds past J2000):'
     .           //             '              #'
            INFMSG( 21 ) = ' '
            INFMSG( 22 ) = '   3c) Epoch (TDB, calendar format):   '
     .           //             '              #'
            INFMSG( 23 ) = ' '
            INFMSG( 24 ) = ' '
            INFMSG( 25 ) = '4) Maximum angular velocity difference'
            INFMSG( 26 ) = ' '
            INFMSG( 27 ) = '   4a) Angular velocity (rad/s):       '
     .           //             '              #'
            INFMSG( 28 ) = ' '
            INFMSG( 29 ) = '   4b) Epoch (TDB, seconds past J2000):'
     .           //             '              #'
            INFMSG( 30 ) = ' '
            INFMSG( 31 ) = '   4c) Epoch (TDB, calendar format):   '
     .           //             '              #'
            INFMSG( 32 ) = ' '

            INFCNT = 32

C
C           Tweak template for sampling case.
C
            IF ( SAMPLE ) THEN
               INFMSG(  2 ) = '1) Average'
               INFMSG(  9 ) = '2) RMS of'
               INFMSG( 16 ) = '3) Maximum rotation'
               INFMSG( 25 ) = '4) Maximum angular velocity'
            END IF

C
C           Fill in rotation values and accompanying times.
C
            CALL DPSTRF( RANAV,  DEFSDG, 'F', HLPSTR )
            CALL REPMC ( INFMSG(  4 ), '#', HLPSTR, INFMSG(  4 ) )

            CALL DPSTRF( RANRMS, DEFSDG, 'F', HLPSTR )
            CALL REPMC ( INFMSG( 11 ), '#', HLPSTR, INFMSG( 11 ) )

            CALL DPSTRF( RANMAX, DEFSDG, 'F', HLPSTR )
            CALL REPMC ( INFMSG( 18 ), '#', HLPSTR, INFMSG( 18 ) )

            CALL DPSTRF( RANMET, DEFSDG, 'F', HLPSTR )
            CALL REPMC ( INFMSG( 20 ), '#', HLPSTR, INFMSG( 20 ) )

            CALL ETCAL ( RANMET, HLPSTR )
            HLPSTR(5:5)   = '-'
            HLPSTR(9:9)   = '-'
            HLPSTR(12:12) = '-'
            CALL REPMC ( INFMSG( 22 ), '#', HLPSTR, INFMSG( 22 ) )

C
C           If requested, fill in AV values and accompanying times.
C
            IF ( AVFLG ) THEN

               CALL DPSTRF( AVDAV,  DEFSDG, 'F', HLPSTR )
               CALL REPMC ( INFMSG(  6 ), '#', HLPSTR, INFMSG(  6 ) )

               CALL DPSTRF( AVDRMS, DEFSDG, 'F', HLPSTR )
               CALL REPMC ( INFMSG( 13 ), '#', HLPSTR, INFMSG( 13 ) )

               CALL DPSTRF( AVDMAX, DEFSDG, 'F', HLPSTR )
               CALL REPMC ( INFMSG( 27 ), '#', HLPSTR, INFMSG( 27 ) )

               CALL DPSTRF( AVDMET, DEFSDG, 'F', HLPSTR )
               CALL REPMC ( INFMSG( 29 ), '#', HLPSTR, INFMSG( 29 ) )

               CALL ETCAL ( AVDMET, HLPSTR )
               HLPSTR(5:5)   = '-'
               HLPSTR(9:9)   = '-'
               HLPSTR(12:12) = '-'
               CALL REPMC ( INFMSG( 31 ), '#', HLPSTR, INFMSG( 31 ) )

            ELSE

C
C              Remove AV lines from the report.
C
               INFPRT(6) = .FALSE.
               INFPRT(7) = .FALSE.

               INFPRT(13) = .FALSE.
               INFPRT(14) = .FALSE.

               DO I = 24, 32
                  INFPRT(I) = .FALSE.
               END DO

            END IF

         ELSE

            CALL SETMSG ( 'There is a bug in the program. '          //
     .                    'Please, contact NAIF.'                    )
            CALL SIGERR ( 'SPICE(FRMDIFFBUG8)'                       )

         END IF

C
C        Display report.
C
         DO I = 1, INFCNT
            IF ( INFPRT(I) ) THEN
               CALL TOSTDO ( INFMSG(I) )
            END IF
         END DO

C
C        End of basic and stats dumps.
C
      ELSE

         CALL SETMSG ( 'There is a bug in the program. '             //
     .                 'Please, contact NAIF.'                       )
         CALL SIGERR ( 'SPICE(FRMDIFFBUG9)'                          )

      END IF

C
C     All done.
C
      CALL CHKOUT ( 'RTDIFF' )

      RETURN

      END
