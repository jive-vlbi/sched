C$Procedure      STDIFF ( State differences )
 
      SUBROUTINE STDIFF ( STA, STB, CMPWIN, NITR, TIMES, DIFTYP, TIMFMT,
     .                    SAMPLE, SIGDIG )
 
C$ Abstract
C
C     Computes the differences between the state vectors in STA and
C     those in STB and writes summary of requested type to the screen.
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
C     EPHEMERIS
C
C$ Declarations

      IMPLICIT NONE
 
      INCLUDE 'spkdiff.inc'
 
      DOUBLE PRECISION      STA    ( 6, * )
      DOUBLE PRECISION      STB    ( 6, * )
      DOUBLE PRECISION      CMPWIN ( LBCELL : * )
      INTEGER               NITR
      DOUBLE PRECISION      TIMES  (    * )
      CHARACTER*(*)         DIFTYP
      CHARACTER*(*)         TIMFMT
      LOGICAL               SAMPLE
      INTEGER               SIGDIG

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STA        I   Array of state vectors.
C     CMPWIN     I   Comparison window
C     STB        I   Another array of state vectors..
C     NITR       I   The number of states in STA or STB.
C     TIMES      I   Epochs of the states.
C     DIFTYP     I   Type of report to produce.
C     TIMFMT     I   Output time format.
C     SAMPLE     I   Flag indicating a sampling run.
C     SIGDIG     I   Number of significant digits.
C
C$ Detailed_Input
C
C     STA,
C     STB         are arrays of state vectors.  A state vector is a six
C                 element array that describes the position and
C                 velocity of one body with respect to another at a
C                 particular time.  The first three elements are the X,
C                 Y, and Z coordinates of the position, and the last
C                 three elements are the coordinates of the velocity.
C
C                 The states in STA and STB should be for the same
C                 body, with respect to the same center, in the same
C                 reference frame, and associated with the same epochs.
C                 They differ only in that they come from different
C                 sources.
C
C     CMPWIN      is the comparison window covering all TIMES.
C
C     NITR        is the number of states in STA or STB.
C
C     TIMES       is an array of epochs for which the states in STA and
C                 STB are valid.
C
C     DIFTYP      is the string indicating what type of report is to be
C                 displayed. It can have value 'basic', 'stats',
C                 'dump', 'dumpvf', 'dumpc', or 'dumpg'.
C
C                 If DIFTYP is 'basic' then only relative differences
C                 in state vectors and magnitude of state difference
C                 vectors will be displayed.
C
C                 If DIFTYP is 'stats' then average components of
C                 position difference vectors in view frame
C                 coordinates, average |components| of position
C                 difference vectors in view frame coordinates,
C                 Components of the position difference vector (in view
C                 frame coordinates) for the states with the MAXIMUM
C                 RELATIVE difference in position, and RMS of position
C                 difference vectors in view frame coordinates will be
C                 displayed.
C
C                 If DIFTYP is 'dump' then a simple table of 
C                 differences between input state vectors will be 
C                 displayed.
C
C                 If DIFTYP is 'dumpvf' then a table of differences
C                 between input state vectors rotated into into the
C                 view frame based on states from STA will be
C                 displayed.
C
C                 If DIFTYP is 'dumpc' then a table of start and stop
C                 times of CMPWIN intervals will be displayed.
C
C                 If DIFTYP is 'dumpg' then a table of start and stop
C                 times of gaps between CMPWIN intervals will be
C                 displayed.
C
C     TIMFMT      is the output format for the time tags in the 
C                 difference table generated for DIFTYP = 'dump'. 
C                 If it is blank, then times are printed as ET seconds
C                 past J2000. If it is non blank, it is passed directly 
C                 into the TIMOUT.
C
C     SAMPLE      is a logical flag indicating whether it is a sampling
C                 run.
C
C     SIGDIG      is the number of significant digits in the output
C                 numbers in scientific format included in the dump
C                 reports.
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
C     None.
C
C$ Files
C
C     LSK needed for time conversions (ET->UTC) must be loaded prior to
C     calling this routine.
C
C$ Particulars
C
C     TBD
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
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    Version 2.0.0, 25-OCT-2011 (BVS)
C
C        Updated for majors functionality additions (sampling, window
C        with gaps, coverage and gaps display, etc).
C
C-    Version 1.0.0, 10-NOV-2006 (BVS)
C
C        Initial version, heavily based on CMPSPK's STDIFF.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VREL
      DOUBLE PRECISION      VDOT
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
      INTEGER               J
      INTEGER               I

      CHARACTER*(SWDSIZ)    SWDSTR
      CHARACTER*(WRDSIZ)    OUTCH    ( 4 )
      CHARACTER*(DSPSIZ)    POSSTR
      CHARACTER*(DSPSIZ)    VELSTR
      CHARACTER*(DSPSIZ)    HLPSTR
      CHARACTER*(LINSIZ)    DMPSTR
      CHARACTER*(LINSIZ)    DATSTR
      CHARACTER*(LINSIZ)    HDRSTR
 
      DOUBLE PRECISION      DIFF     ( 6 )
      DOUBLE PRECISION      POSDIF   ( 3 )
      DOUBLE PRECISION      VELDIF   ( 3 )
      DOUBLE PRECISION      POSCMP   ( 3 )
      DOUBLE PRECISION      VELCMP   ( 3 )
      DOUBLE PRECISION      SUMP     ( 3 )
      DOUBLE PRECISION      SUMV     ( 3 )
      DOUBLE PRECISION      SUMPA    ( 3 )
      DOUBLE PRECISION      SUMVA    ( 3 )
      DOUBLE PRECISION      SUMPS    ( 3 )
      DOUBLE PRECISION      SUMVS    ( 3 )
      DOUBLE PRECISION      MXAPOS   ( 3 )
      DOUBLE PRECISION      MXAVEL   ( 3 )
      DOUBLE PRECISION      MXRPOS   ( 3 )
      DOUBLE PRECISION      MXRVEL   ( 3 )
      DOUBLE PRECISION      AVPO     ( 3 )
      DOUBLE PRECISION      AVVE     ( 3 )
      DOUBLE PRECISION      AVPOA    ( 3 )
      DOUBLE PRECISION      AVVEA    ( 3 )
      DOUBLE PRECISION      AVPOSQ   ( 3 )
      DOUBLE PRECISION      AVVESQ   ( 3 )
      DOUBLE PRECISION      AXIS     ( 3, 3 )
      DOUBLE PRECISION      VCROSS   ( 3 )
      DOUBLE PRECISION      TMPVEC   ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      POSMAG
      DOUBLE PRECISION      VELMAG
      DOUBLE PRECISION      POSSUM
      DOUBLE PRECISION      VELSUM
      DOUBLE PRECISION      SUMDT
      DOUBLE PRECISION      SUMADT
      DOUBLE PRECISION      SUMSDT
      DOUBLE PRECISION      AVPOS
      DOUBLE PRECISION      AVVEL
      DOUBLE PRECISION      AVADT
      DOUBLE PRECISION      AVSDT
      DOUBLE PRECISION      AVDT
      DOUBLE PRECISION      POSREL
      DOUBLE PRECISION      VELREL
      DOUBLE PRECISION      RELPSM
      DOUBLE PRECISION      RELVSM
      DOUBLE PRECISION      MAXPMG
      DOUBLE PRECISION      MAXVMG
      DOUBLE PRECISION      MAXPRL
      DOUBLE PRECISION      MAXVRL
      DOUBLE PRECISION      MXAADT
      DOUBLE PRECISION      MXRADT
      DOUBLE PRECISION      AVRELP
      DOUBLE PRECISION      AVRELV
      DOUBLE PRECISION      MXATIM
      DOUBLE PRECISION      MXRTIM 

      LOGICAL               MXATRU
      LOGICAL               MXRTRU
      LOGICAL               NONZER
 
C
C     Save everything to prevent potential memory problems in f2c'ed
C     version.
C
      SAVE

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STDIFF' )
      END IF


C
C     If requested, dump sampled states or differences and return.
C
      IF ( EQSTR( DIFTYP, DMPVAL ) ) THEN
         
         IF ( SAMPLE ) THEN
            DMPSTR = '# time, x, y, z, vx, vy, vz' 
         ELSE
            DMPSTR = '# time, (x1-x2), (y1-y2), (z1-z2), ' //
     .               '(vx1-vx2), (vy1-vy2), (vz1-vz2)' 
         END IF
         CALL TOSTDO( DMPSTR )

         DO J = 1, NITR

            DMPSTR = '# # # # # # #'

            IF ( TIMFMT .EQ. ' ' ) THEN
               CALL DPSTRE ( TIMES(J), SIGDIG, HLPSTR )
            ELSE
               CALL TIMOUT ( TIMES(J), TIMFMT, HLPSTR )
            END IF
            
            CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )

            DO I = 1, 6

               IF ( SAMPLE ) THEN
                  CALL DPSTRE ( STA(I,J),          SIGDIG, HLPSTR )
               ELSE
                  CALL DPSTRE ( STA(I,J)-STB(I,J), SIGDIG, HLPSTR )
               END IF

               IF ( HLPSTR(1:1) .EQ. ' ' ) THEN
                  HLPSTR(1:1) = '+'
               END IF

               CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               
            END DO

            CALL TOSTDO( DMPSTR )

         END DO

         CALL CHKOUT ( 'STDIFF' )
         RETURN

      END IF


C
C     If requested, dump interval table and return.
C
      IF ( EQSTR( DIFTYP, DCVAL ) ) THEN

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
            IF ( TIMFMT .EQ. ' ' ) THEN

               CALL DPSTRE ( CMPWIN(I),   SIGDIG, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )
 
               CALL DPSTRE ( CMPWIN(I+1), SIGDIG, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )

            ELSE

               CALL TIMOUT ( CMPWIN(I),   TIMFMT, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )

               CALL TIMOUT ( CMPWIN(I+1), TIMFMT, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )

            END IF

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
         CALL CHKOUT ( 'STDIFF' )
         RETURN

      END IF


C
C     If requested, dump gap table and return.
C
      IF ( EQSTR( DIFTYP, DGVAL ) ) THEN

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
            IF ( TIMFMT .EQ. ' ' ) THEN

               CALL DPSTRE ( CMPWIN(I),   SIGDIG, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )
 
               CALL DPSTRE ( CMPWIN(I+1), SIGDIG, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )

            ELSE

               CALL TIMOUT ( CMPWIN(I),   TIMFMT, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )

               CALL TIMOUT ( CMPWIN(I+1), TIMFMT, HLPSTR )
               CALL REPMC  ( DMPSTR, '#', HLPSTR, DMPSTR )

            END IF

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
C        If there are no gaps, report it.
C
         IF ( WNCARD(CMPWIN) .EQ. 1 ) THEN

            CALL TOSTDO( ' ' )
            CALL TOSTDO( 'There are no gaps in coverage.' )
            CALL TOSTDO( ' ' )

         END IF

C
C        End of coverage gap dump.
C
         CALL CHKOUT ( 'STDIFF' )
         RETURN

      END IF

C
C     By this time we should have handled all possible output types
C     applicable to sampling. If we did get here, it's a bug. Report it
C     and exit.
C
      IF ( SAMPLE ) THEN
         CALL SETMSG ( 'There is a bug in the program. ' //
     .                 'Please, contact NAIF.'           )
         CALL SIGERR ( 'SPICE(SPKDIFFBUG2)'              )
      END IF


C
C     If requested, dump differences in view frame and return.
C
      IF ( EQSTR( DIFTYP, DVFVAL ) ) THEN

C
C        Before doing this dump we need to verify that we can construct
C        view frame for every state is STA.
C
         NONZER = .TRUE.
         DO J = 1, NITR
            CALL UCRSS ( STA(4,J), STA(1,J), VCROSS )
            IF ( VNORM( VCROSS ) .EQ. 0.D0 ) THEN
               NONZER = .FALSE.
            END IF
         END DO

         IF ( .NOT. NONZER ) THEN
            CALL TOSTDO ( ' ' )
            CALL TOSTDO ( 'No view frame difference table can be '
     .//                  'generated because in one or more cases  ' )
            CALL TOSTDO ( 'the state computed from the first SPK has '
     .//                  'linearly dependent position and' )
            CALL TOSTDO ( 'velocity, which makes constructing the view '
     .//                  'frame impossible.' )
            CALL TOSTDO ( ' ' )
            CALL CHKOUT ( 'STDIFF' )
            RETURN
         END IF

C
C        There are no states with linearly dependent position and
C        velocity in STA. We can proceed with dumping differences in
C        the view frame.
C         
         DMPSTR = '# time, down_track_p_diff, ' //
     .            'normal_to_plane_p_diff, in_plane_p_diff, ' //
     .            'down_track_v_diff, normal_to_plane_v_diff, ' //
     .            'in_plane_v_diff'
         CALL TOSTDO( DMPSTR )

         DO J = 1, NITR

C
C           Find the difference vector between the two states (STA-STB).
C
            CALL VSUBG ( STA(1,J), STB(1,J), 6, DIFF )

C
C           Construct view frame based on the current STA:
C
C             AXIS(1): is parallel to the direction of motion given by
C                      STA
C
C             AXIS(2): is normal to the orbit plane defined by STA
C  
C             AXIS(3): is AXIS(1) X AXIS(2)            
C
            CALL TWOVEC( STA(4,J), 1, STA(1,J), 3, AXIS )

C
C           Rotate difference in position and velocity into view frame.
C
            CALL MXV  ( AXIS, DIFF(1), TMPVEC )
            CALL VEQU ( TMPVEC, DIFF(1) )
            CALL MXV  ( AXIS, DIFF(4), TMPVEC )
            CALL VEQU ( TMPVEC, DIFF(4) )

C
C           Format everything for output.
C            
            DMPSTR = '# # # # # # #'

            IF ( TIMFMT .EQ. ' ' ) THEN
               CALL DPSTRE ( TIMES(J), SIGDIG, HLPSTR )
            ELSE
               CALL TIMOUT ( TIMES(J), TIMFMT, HLPSTR )
            END IF
            
            CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
 
            DO I = 1, 6

               CALL DPSTRE ( DIFF(I), SIGDIG, HLPSTR )

               IF ( HLPSTR(1:1) .EQ. ' ' ) THEN
                  HLPSTR(1:1) = '+'
               END IF

               CALL REPMC ( DMPSTR, '#', HLPSTR, DMPSTR )
               
            END DO

            CALL TOSTDO( DMPSTR )

         END DO

         CALL CHKOUT ( 'STDIFF' )
         RETURN

      END IF

C
C     Do all other reports -- basic and stats.
C

C
C     Initial values.
C
      DO I = 1, 3
         SUMP   (I)   =   0.D0
         SUMV   (I)   =   0.D0
         SUMPA  (I)   =   0.D0
         SUMVA  (I)   =   0.D0
         SUMPS  (I)   =   0.D0
         SUMVS  (I)   =   0.D0
         MXAPOS (I)   =   0.D0
         MXAVEL (I)   =   0.D0
         MXRPOS (I)   =   0.D0
         MXRVEL (I)   =   0.D0
      END DO
  
      SUMDT   =   0.D0
      SUMADT  =   0.D0
      SUMSDT  =   0.D0
      POSSUM  =   0.D0
      VELSUM  =   0.D0
      RELPSM  =   0.D0
      RELVSM  =   0.D0
      MAXPMG  = DPMIN()
      MAXVMG  = DPMIN()
      MAXPRL  = DPMIN()
      MAXVRL  = DPMIN()
      MXAADT  =   0.D0
      MXRADT  =   0.D0
  
      NONZER  =  .TRUE.
 
C
C     After subtracting the state vectors for the Jth time, first
C     compare the individual components of the difference vector and
C     then perform the tests on the vector itself.
C
      DO J = 1, NITR
 
C
C        Find the difference vector between the two states (STA-STB).
C
         CALL VSUBG ( STA(1,J), STB(1,J), 6, DIFF )
 
C
C        Now perform tests on the actual difference vectors.
C
         CALL VEQU ( DIFF(1), POSDIF )
         CALL VEQU ( DIFF(4), VELDIF )
 
C
C        Find the magnitudes and relative error of the position and
C        velocity difference vectors.
C
         POSMAG = VNORM ( POSDIF )
         VELMAG = VNORM ( VELDIF )
 
         POSREL = VREL ( STA(1,J), STB(1,J) )
 
         VELREL = VREL ( STA(4,J), STB(4,J) )
 
         POSSUM = POSSUM + POSMAG
         VELSUM = VELSUM + VELMAG
 
         RELPSM = RELPSM + POSREL
         RELVSM = RELVSM + VELREL
 
         MXATRU = .FALSE.
         MXRTRU = .FALSE.
 
         IF ( POSMAG .GT. MAXPMG ) THEN

C
C           We are going to return information on the case with the
C           largest absolute difference in position.
C
            MXATRU    = .TRUE.
            MAXPMG    = POSMAG

         END IF
 
         IF ( VELMAG .GT. MAXVMG ) THEN
            MAXVMG = VELMAG
         END IF
 
         IF ( POSREL .GT. MAXPRL ) THEN
 
C
C           We are going to return information on the case with the
C           largest relative difference in position.
C
            MXRTRU    = .TRUE.
            MAXPRL    = POSREL
 
         END IF
 
         IF ( VELREL .GT. MAXVRL ) THEN
            MAXVRL = VELREL
         END IF
 
C
C        Compute the components of the position and velocity difference
C        vectors in view frame coordinates.
C
C           AXIS(1): is parallel to the direction of motion given by
C                    STA
C
C           AXIS(2): is normal to the orbit plane defined by STA
C
C           AXIS(3): is AXIS(1) X AXIS(2)
C
C        We could have called TWOVEC like this:
C
C           CALL TWOVEC( STA(4,J), 1, STA(1,J), 3, AXIS )
C
C        to build this frame but it signals an error if input vectors
C        are linearly dependent and we don't want this to happen. So we
C        build this matrix by hand.
C
         CALL VEQU ( STA(4,J), AXIS(1,1) )
 
         CALL VCRSS ( STA (1,J), AXIS(1,1), AXIS(1,2) )
 
         CALL VCRSS ( AXIS(1,1), AXIS(1,2), AXIS(1,3) )

C
C        Can't do the tests if any of the axis are zero.
C
         IF ( ( VNORM ( AXIS(1,1) ) .EQ. 0.D0 )   .OR.
     .        ( VNORM ( AXIS(1,2) ) .EQ. 0.D0 )   .OR.
     .        ( VNORM ( AXIS(1,3) ) .EQ. 0.D0 ) ) THEN
 
            NONZER = .FALSE.

         END IF
 
         IF ( NONZER ) THEN
 
C
C           Find the components of the difference vector w/r to view
C           frame axes. If our AXIS matrix was a rotation matrix, we
C           could have called MXV to do this, but since it is not we
C           will multiply vector by hand.
C
            DO I = 1, 3
 
               POSCMP(I) = VDOT ( POSDIF,AXIS(1,I) ) / VNORM(AXIS(1,I))
 
               VELCMP(I) = VDOT ( VELDIF,AXIS(1,I) ) / VNORM(AXIS(1,I))
 
            END DO

C
C           Divide the downtrack difference in the position by the
C           speed to give the error in time along the flight path.
C
            DELTA  = POSCMP(1) / VNORM ( AXIS(1,1) )
 
C
C           Keep track of the differences.
C
            SUMDT  = SUMDT     + DELTA
 
            SUMADT = SUMADT    + ABS ( DELTA )
 
            SUMSDT = SUMSDT    + DELTA * DELTA

            DO I = 1, 3
 
               SUMP  (I) = SUMP(I) + POSCMP(I)
 
               SUMV  (I) = SUMV(I) + VELCMP(I)
 
               SUMPA (I) = SUMPA(I) + ABS ( POSCMP(I) )
 
               SUMVA (I) = SUMVA(I) + ABS ( VELCMP(I) )
 
               SUMPS (I) = SUMPS(I) + POSCMP(I) * POSCMP(I) 
 
               SUMVS (I) = SUMVS(I) + VELCMP(I) * VELCMP(I)
 
            END DO
 
C
C           For the worst absolute difference in the position, record
C           the components in view frame coordinates.
C
            IF ( MXATRU ) THEN
 
               DO I = 1, 3
                  MXAPOS(I)   = POSCMP(I)
                  MXAVEL(I)   = VELCMP(I)
               END DO
 
               MXAADT    = DELTA
               MXATIM    = TIMES(J)
  
            END IF

C
C           For the worst relative difference in the position, record
C           the components in view frame coordinates.
C
            IF ( MXRTRU ) THEN
 
               DO I = 1, 3
                  MXRPOS(I)   = POSCMP(I)
                  MXRVEL(I)   = VELCMP(I)
               END DO
 
               MXRADT    = DELTA
               MXRTIM    = TIMES(J)

            END IF
 
         END IF
 
      END DO

C
C     Find the average values of all the statistics computed.
C
      AVPOS = POSSUM / DBLE ( NITR )
 
      AVVEL = VELSUM / DBLE ( NITR )
 
      AVRELP = RELPSM / DBLE ( NITR )
 
      AVRELV = RELVSM / DBLE ( NITR )
 
      IF ( NONZER ) THEN
 
         DO I = 1, 3
            AVPO  (I) = SUMP  (I) / DBLE ( NITR )
            AVVE  (I) = SUMV  (I) / DBLE ( NITR )
            AVPOA (I) = SUMPA (I) / DBLE ( NITR )
            AVVEA (I) = SUMVA (I) / DBLE ( NITR )
            AVPOSQ(I) = DSQRT( SUMPS (I) / DBLE ( NITR ) )
            AVVESQ(I) = DSQRT( SUMVS (I) / DBLE ( NITR ) )
         END DO
 
         AVDT   = SUMDT  / DBLE ( NITR )
         AVADT  = SUMADT / DBLE ( NITR )
         AVSDT  = DSQRT( SUMSDT / DBLE ( NITR ) )
 
      END IF
 
C
C     If requested, write basic report.
C
      IF ( EQSTR( DIFTYP, BSCVAL ) ) THEN


C
C        Construct and print maximum and average relative differences
C        block.
C         
         POSSTR = '  Position:             #      # '
         VELSTR = '  Velocity:             #      # '
 
         CALL DPSTR ( MAXPRL, DEFSDG, OUTCH(1) )
         CALL DPSTR ( AVRELP, DEFSDG, OUTCH(2) )
         CALL DPSTR ( MAXVRL, DEFSDG, OUTCH(3) )
         CALL DPSTR ( AVRELV, DEFSDG, OUTCH(4) )

         CALL REPMC ( POSSTR, '#', OUTCH(1), POSSTR )
         CALL REPMC ( POSSTR, '#', OUTCH(2), POSSTR )
         CALL REPMC ( VELSTR, '#', OUTCH(3), VELSTR )
         CALL REPMC ( VELSTR, '#', OUTCH(4), VELSTR )
         
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( 'Relative differences in state vectors: ' )
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( '                              maximum       '
     .//               '          average' )
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( POSSTR )
         CALL TOSTDO ( VELSTR )
         CALL TOSTDO ( ' ' )
 
C
C        Construct and print maximum and average relative differences
C        block.
C         
         POSSTR = '  Position (km):        #      # '
         VELSTR = '  Velocity (km/s):      #      # '
 
         CALL DPSTR ( MAXPMG, DEFSDG, OUTCH(1) )
         CALL DPSTR ( AVPOS,  DEFSDG, OUTCH(2) )
         CALL DPSTR ( MAXVMG, DEFSDG, OUTCH(3) )
         CALL DPSTR ( AVVEL,  DEFSDG, OUTCH(4) )

         CALL REPMC ( POSSTR, '#', OUTCH(1), POSSTR )
         CALL REPMC ( POSSTR, '#', OUTCH(2), POSSTR )
         CALL REPMC ( VELSTR, '#', OUTCH(3), VELSTR )
         CALL REPMC ( VELSTR, '#', OUTCH(4), VELSTR )
 
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( 'Absolute differences in state vectors:' )
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( '                              maximum       ' //
     .                 '          average' )
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( POSSTR )
         CALL TOSTDO ( VELSTR )
 
         CALL TOSTDO ( ' ' )

      END IF
 
C
C     If requested and if states are different, write stats report.
C
      IF ( NONZER .AND. EQSTR( DIFTYP, STSVAL ) ) THEN
 
C
C        Print the average of difference values stats block.
C
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( '1) Average components of position difference '
     .//               'vectors in view ' )
         CALL TOSTDO ( '   frame coordinates:' )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPO(1), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   1a) Down track (km):                       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPO(3), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   1b) In orbit plane (km):                   '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPO(2), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   1c) Normal to orbit plane (km):            '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVDT, DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   1d) Average delta time down track (sec):   '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )

C
C        Print the average of absolute difference values stats block.
C
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( '2) Average |components| of position difference'
     .//               ' vectors in ' )
         CALL TOSTDO ( '   view frame coordinates:' )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPOA(1), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   2a) Down track (km):                       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPOA(3), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   2b) In orbit plane (km):                   '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPOA(2), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   2c) Normal to orbit plane (km):            '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVADT, DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   2d) Average |delta time| down track (sec): '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )

C
C        Print the RMS stats block.
C
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( '3) RMS of position difference'
     .//                 ' vectors in view frame coordinates:' )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPOSQ(1), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   3a) Down track (km):                       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPOSQ(3), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   3b) In orbit plane (km):                   '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVPOSQ(2), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   3c) Normal to orbit plane (km):            '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( AVSDT, DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   3d) RMS delta time down track (sec):       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )

C
C        Print the maximum relative difference block.
C
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( '4) Components of the position difference '
     .//               'vector in view frame' )
         CALL TOSTDO ( '   coordinates for the states with the '
     .//               'MAXIMUM RELATIVE ' )
         CALL TOSTDO ( '   difference in position: ' )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXRPOS(1), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   4a) Down track (km):                       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXRPOS(3), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   4b) In orbit plane (km):                   '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXRPOS(2), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   4c) Normal to orbit plane (km):            '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXRADT, DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   4d) Delta time down track (sec):           '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXRTIM, DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   4e) Epoch (TDB, seconds past J2000):       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL ETCAL  ( MXRTIM, HLPSTR )
         DO I = 1, RTRIM(HLPSTR)
            IF ( HLPSTR(I:I) .EQ. ' ' ) THEN
               HLPSTR(I:I) = '-'
            END IF
         END DO
         CALL TOSTDO ( '   4f) Epoch (TDB, calendar format):           '
     .//               HLPSTR )
         CALL TOSTDO ( ' ' )

C
C        Print the maximum absolute difference block.
C
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( '5) Components of the position difference '
     .//                'vector in view frame' )
         CALL TOSTDO ( '   coordinates for the states with the '
     .//               'MAXIMUM ABSOLUTE ' )
         CALL TOSTDO ( '   difference in position: ' )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXAPOS(1), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   5a) Down track (km):                       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXAPOS(3), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   5b) In orbit plane (km):                   '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXAPOS(2), DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   5c) Normal to orbit plane (km):            '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXAADT, DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   5d) Delta time down track (sec):           '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL DPSTRF ( MXATIM, DEFSDG, 'F', SWDSTR )
         CALL TOSTDO ( '   5e) Epoch (TDB, seconds past J2000):       '
     .//               SWDSTR )
         CALL TOSTDO ( ' ' )
         CALL ETCAL  ( MXATIM, HLPSTR )
         DO I = 1, RTRIM(HLPSTR)
            IF ( HLPSTR(I:I) .EQ. ' ' ) THEN
               HLPSTR(I:I) = '-'
            END IF
         END DO
         CALL TOSTDO ( '   5f) Epoch (TDB, calendar format):           '
     .//               HLPSTR )
         CALL TOSTDO ( ' ' )

      ELSE IF ( EQSTR( DIFTYP, STSVAL ) ) THEN
 
C
C        View frame could not be constructed for one or more states
C        from STA. Report this instead of printing stats.
C
         CALL TOSTDO ( ' ' )
         CALL TOSTDO ( 'No view frame statistical data can be '
     .//               'generated because in one or more cases  ' )
         CALL TOSTDO ( 'the state computed from the first SPK has '
     .//               'linearly dependent position and' )
         CALL TOSTDO ( 'velocity, which makes constructing the view '
     .//               'frame impossible.' )
         CALL TOSTDO ( ' ' )

      END IF
 
      CALL CHKOUT ( 'STDIFF' )
      RETURN
      END
