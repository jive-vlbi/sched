C$Procedure      SCENCD ( Encode spacecraft clock )
 
      SUBROUTINE SCENCD ( SC, SCLKCH, SCLKDP )
 
C$ Abstract
C
C     Encode character representation of spacecraft clock time into a
C     double precision number.
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
C     SCLK
C
C$ Keywords
C
C     CONVERSION
C     TIME
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'sclk.inc'

      INTEGER               SC
      CHARACTER*(*)         SCLKCH
      DOUBLE PRECISION      SCLKDP
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     SCLKCH     I   Character representation of a spacecraft clock.
C     SCLKDP     O   Encoded representation of the clock count.
C     MXPART     P   Maximum number of spacecraft clock partitions.
C
C$ Detailed_Input
C
C     SC         is the standard NAIF ID of the spacecraft whose clock's
C                time is being encoded.
C
C     SCLKCH     is the character representation of some spacecraft's
C                clock count.
C
C                SCLKCH will have the following general format:
C
C                             'pp/sclk_string', or just
C                                'sclk_string'
C
C                'pp' is an integer greater than or equal to one
C                and is called the partition number.
C
C                Each mission is divided into some number of partitions.
C                A new partition starts when the spacecraft clock
C                resets, either to zero, or to some other
C                value. Thus, the first partition for any mission
C                starts with launch, and ends with the first clock
C                reset. The second partition starts immediately when
C                the first stopped, and so on.
C
C                In order to be completely unambiguous about a
C                particular time, you need to specify a partition number
C                along with the standard clock string.
C
C                Information about when partitions occur for different
C                missions is contained in a spacecraft clock kernel
C                file, which needs to be loaded into the kernel pool,
C                using the routines CLPOOL and FURNSH.
C
C                The routine SCPART is used to read the partition
C                start and stop times, in encoded units of SCLK (called
C                "ticks" -- see SCLKDP below) from the kernel file.
C
C                If the partition number is included, it must be
C                separated from the rest of the string by a '/'.
C                Any number of spaces may separate the partition number,
C                the '/', and the rest of the clock string.
C
C
C                If the partition number is omitted, a default partition
C                will be assumed. The default partition is the lowest-
C                numbered partition that contains the given clock time.
C                If the clock time does not fall in any of the
C                partition boundaries then an error is signaled.
C
C
C                'sclk_string' is a spacecraft specific clock string.
C                Using Galileo as an example, the full format is
C
C                               wwwwwwww:xx:y:z
C
C                where z is a mod-8 counter (values 0-7) which
C                increments approximately once every 8 1/3 ms., y is a
C                mod-10 counter (values 0-9) which increments once
C                every time z turns over, i.e., approximately once every
C                66 2/3 ms., xx is a mod-91 (values 0-90) counter
C                which increments once every time y turns over, i.e.,
C                once every 2/3 seconds. wwwwwwww is the Real-Time Image
C                Count (RIM), which increments once every time xx turns
C                over, i.e., once every 60 2/3 seconds. The roll-over
C                expression for the RIM is 16777215, which corresponds
C                to approximately 32 years.
C
C                wwwwwwww, xx, y, and z are referred to interchangeably
C                as the fields or components of the spacecraft clock.
C                SCLK components may be separated by any of these
C                five characters: ' '  ':'  ','  '-'  '.'
C                Any number of spaces can separate the components and
C                the delimiters. The presence of the RIM component
C                is required. Successive components may be omitted, and
C                in such cases are assumed to represent zero values.
C
C                Values for the individual components may exceed the
C                maximum expected values. For instance, '0:0:0:9' is
C                an acceptable Galileo clock string, and will convert
C                to the same number of ticks as '0:0:1:1'.
C
C                Consecutive delimiters containing no intervening digits
C                are treated as if they delimit zero components.
C
C                Trailing zeros should always be included to match the
C                length of the counter.  For example, a Galileo clock
C                count of '25684.90' should not be represented as
C                '25684.9'.
C
C                Some spacecraft clock components have offset, or
C                starting, values different from zero.  For example,
C                with an offset value of 1, a mod 20 counter would
C                cycle from 1 to 20 instead of from 0 to 19.
C
C                See the SCLK required reading for a detailed
C                description of the Voyager and Mars Observer clock
C                formats.
C
C
C$ Detailed_Output
C
C     SCLKDP     is the double precision encoding of SCLKCH.
C
C                The encoding is such that order and proximity will be
C                preserved. That is, if t1, t2, and t3 are spacecraft
C                clock times, and t1*, t2*, and t3* are their encodings,
C                then if
C
C                              t1 < t2 < t3, and
C
C                t2 is closer to t1 than to t3, you will have the result
C                that
C
C                             t1* < t2* < t3*, and
C
C                t2* is closer to t1* than to t3*.
C
C                The units of encoded SCLK are "ticks since the start of
C                the mission", where a "tick" is defined to be the
C                shortest time increment expressible by a particular
C                spacecraft's clock.
C
C                Each clock string without partition number represents
C                a certain number of ticks, but you need to include
C                partition information to determine the relative
C                position of that time in relation to the start of the
C                mission.
C
C                Since the end time of one partition is coincident
C                with the begin time of the next, there are two
C                different representations for this instant, and they
C                will both yield the same encoding.
C
C                For example, if partition 1 has an end time of t1, and
C                partition 2 has a begin time of t2, then if we did
C
C                   CALL SCENCD ( '1/t1', SC, X ) and
C                   CALL SCENCD ( '2/t2', SC, Y ), then
C
C                                  X = Y.
C
C                The individual routines TIKSnn, where nn is the
C                clock type code, contain more detailed information
C                on the conversion process.
C
C$ Parameters
C
C     MXPART     is the maximum number of spacecraft clock partitions
C                expected in the kernel file for any one spacecraft.
C                See the INCLUDE file sclk.inc for this parameter's
C                value.
C
C$ Exceptions
C
C     1) If the number of partitions in the kernel file for spacecraft
C        SC exceeds the parameter MXPART, the error
C        'SPICE(TOOMANYPARTS)' is signaled.
C
C
C     If a partition number is included in the SCLK string, the
C     following exceptions may occur:
C
C     2) If the partition number cannot be parsed as an integer, the
C        error 'SPICE(BADPARTNUMBER)' is signaled.
C
C     3) If the partition number is not in the range of the number of
C        partitions found in the kernel pool, the error
C        'SPICE(BADPARTNUMBER)' is signaled.
C
C     4) If the clock count does not fall in the boundaries of the
C        specified partition, the error 'SPICE(NOTINPART)' is
C        signaled.
C
C
C     If a partition number is not included in the SCLK string, the
C     following exception may occur.
C
C     5) If the clock count does not fall in the boundaries of any
C        partition found in the kernel pool, the error
C        'SPICE(NOPARTITION)' is signaled.
C
C     The following error is signaled by a routine called by SCENCD
C
C     6)  If any of the extracted clock components cannot be parsed as
C         integers, or the string has too many components, or the value
C         of one of the components is less than the offset value, then
C         the error SPICE(INVALIDSCLKSTRING) is signaled.
C
C$ Files
C
C     A kernel file containing spacecraft clock partition information
C     for the desired spacecraft must be loaded, using the routines
C     CLPOOL and FURNSH, before calling this routine.
C
C$ Particulars
C
C     In general, it is difficult to compare spacecraft clock counts
C     numerically since there are too many clock components for a
C     single comparison.  This routine provides a method of assigning a
C     single double precision number to a spacecraft's clock count,
C     given one of its character representations.
C
C     The routine SCDECD performs the inverse operation to SCENCD,
C     converting an encoded double precision number to character format.
C
C     To convert the string to ticks since the start of the mission,
C     SCENCD
C
C        1) Converts the non-partition portion of the string to
C           ticks, using the routine SCTIKS.
C
C        2) Determines the partition number for the clock time,
C           either by getting it directly from the input string, or
C           determining the default partition if none was specified.
C
C        3) Includes partition start and stop times, which are also
C           measured in ticks, to compute the number of ticks
C           since the beginning of the mission of the clock time.
C
C$ Examples
C
C      Double precision encodings of spacecraft clock counts are used to
C      tag pointing data in the C-kernel.
C
C      In the following example, pointing for a sequence of images from
C      the Voyager 2 narrow angle camera is requested from the C-kernel
C      using an array of character spacecraft clock counts as input.
C      The clock counts attached to the output are then decoded to
C      character and compared with the input strings.
C
C            CHARACTER*(25)     SCLKIN   ( 4 )
C            CHARACTER*(25)     SCLKOUT
C            CHARACTER*(25)     CLKTOL
C
C            DOUBLE PRECISION   TIMEIN
C            DOUBLE PRECISION   TIMOUT
C            DOUBLE PRECISION   CMAT     ( 3, 3 )
C
C            INTEGER            NPICS
C            INTEGER            SC
C
C            DATA  NPICS     /  4                   /
C
C            DATA  SCLKIN    / '2 / 20538:39:768',
C           .                  '2 / 20543:21:768',
C           .                  '2 / 20550:37',
C           .                  '2 / 20561:59'       /
C
C            DATA  CLKTOL   /  '      0:01:000'     /
C
C      C
C      C     The instrument we want pointing for is the Voyager 2
C      C     narrow angle camera.  The reference frame we want is
C      C     J2000. The spacecraft is Voyager 2.
C      C
C            INST = -32001
C            REF  = 'J2000'
C            SC   = -32
C
C      C
C      C     Load the appropriate files. We need
C      C
C      C     1) CK file containing pointing data.
C      C     2) Spacecraft clock kernel file, for SCENCD and SCDECD.
C      C
C            CALL CKLPF  ( 'VGR2NA.CK' )
C            CALL CLPOOL
C            CALL FURNSH ( 'SCLK.KER'  )
C
C      C
C      C     Convert the tolerance string to ticks.
C      C
C            CALL SCTIKS ( SC, CLKTOL, TOL )
C
C            DO I = 1, NPICS
C
C               CALL SCENCD ( SC, SCLKIN( I ), TIMEIN )
C
C               CALL CKGP   ( INST, TIMEIN, TOL, REF, CMAT, TIMOUT,
C           .                 FOUND )
C
C               CALL SCDECD ( SC, TIMOUT, SCLKOUT )
C
C               WRITE (*,*)
C               WRITE (*,*) 'Input  s/c clock count: ', SCLKIN( I )
C               WRITE (*,*) 'Output s/c clock count: ', SCLKOUT
C               WRITE (*,*) 'Output C-Matrix:        ', CMAT
C               WRITE (*,*)
C
C            END DO
C
C     The output from such a program might look like:
C
C
C            Input  s/c clock count:  2 / 20538:39:768
C            Output s/c clock count:  2/20538:39:768
C            Output C-Matrix:  'first C-matrix'
C
C            Input  s/c clock count:  2 / 20543:21:768
C            Output s/c clock count:  2/20543:22:768
C            Output C-Matrix:  'second C-matrix'
C
C            Input  s/c clock count:  2 / 20550:37
C            Output s/c clock count:  2/20550:36:768
C            Output C-Matrix:  'third C-matrix'
C
C            Input  s/c clock count:  2 / 20561:59
C            Output s/c clock count:  2/20561:58:768
C            Output C-Matrix:  'fourth C-matrix'
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
C     N.J. Bachman (JPL)
C     J.M. Lynch   (JPL)
C     R.E. Thurman (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 28-FEB-2014 (BVS)
C
C        Added FAILED checks to prevent passing uninitialized values to
C        ANINT, which can causing numeric exceptions on some
C        environments.
C
C-    SPICELIB Version 1.1.0, 05-FEB-2008 (NJB)
C
C        The values of the parameter MXPART is now
C        provided by the INCLUDE file sclk.inc.
C
C-    SPICELIB Version 1.0.2, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1990 (JML) (RET)
C
C-&
 
C$ Index_Entries
C
C     encode spacecraft_clock
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
      INTEGER               CPOS
 
C
C     Local variables
C
      INTEGER               POS
      INTEGER               NPARTS
      INTEGER               PNTER
      INTEGER               PART
      INTEGER               I
 
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      PSTART   ( MXPART )
      DOUBLE PRECISION      PSTOP    ( MXPART )
      DOUBLE PRECISION      PTOTLS   ( MXPART )
 
      CHARACTER*(25)        ERROR
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCENCD' )
      END IF
 
C
C     Convert the non-partition portion of the clock string to ticks.
C
      POS = CPOS ( SCLKCH, '/', 1 )
 
      CALL SCTIKS (  SC, SCLKCH( POS+1 : ), TICKS )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SCENCD' )
         RETURN   
      END IF

      TICKS = ANINT ( TICKS )
 
C
C     Read the partition start and stop times (in ticks) for this
C     mission. Error if there are too many of them.
C
      CALL SCPART ( SC, NPARTS, PSTART, PSTOP )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SCENCD' )
         RETURN   
      END IF

      IF ( NPARTS .GT. MXPART ) THEN
 
         CALL SETMSG ( 'The number of partitions, #, for spacecraft ' //
     .                 '# exceeds the value for parameter MXPART, #.' )
         CALL ERRINT ( '#', NPARTS           )
         CALL ERRINT ( '#', SC               )
         CALL ERRINT ( '#', MXPART           )
         CALL SIGERR ( 'SPICE(TOOMANYPARTS)' )
         CALL CHKOUT ( 'SCENCD'              )
         RETURN
 
      END IF
 
C
C     PSTART and PSTOP represent integers but are read from the
C     kernel pool as double precision numbers. Make them whole
C     numbers so that logical tests may be performed with them.
C
      DO I = 1, NPARTS
         PSTOP (I) = ANINT ( PSTOP(I)  )
         PSTART(I) = ANINT ( PSTART(I) )
      END DO
 
 
 
C     For each partition, compute the total number of ticks in that
C     partition plus all preceding partitions.
C
      PTOTLS( 1 ) = ANINT ( PSTOP( 1 ) - PSTART( 1 ) )
 
      DO I = 2, NPARTS
         PTOTLS( I ) = ANINT ( PTOTLS( I-1 ) + PSTOP( I ) - PSTART( I ))
      END DO
 
C
C     Determine the partition number for the input clock string:
C
C        If it was included in the string make sure it's valid for
C        this mission.
C
C           Error if
C
C           1) The partition number can't be parsed.
C           2) The partition number is not in the range 1 to the number
C              of partitions.
C           3) The clock count does not fall in the boundaries of the
C              specified partition.
C
C        If it wasn't included, determine the default partition for
C        this clock count.
C
C           Error if
C
C           1) The clock count does not fall in the boundaries of any
C              of the partitions.
C
C
      IF ( POS .EQ. 1 ) THEN
 
         CALL SETMSG ( 'Unable to parse the partition number '   //
     .                 'from SCLK string #.'  )
         CALL ERRCH  ( '#', SCLKCH            )
         CALL SIGERR ( 'SPICE(BADPARTNUMBER)' )
         CALL CHKOUT ( 'SCENCD'               )
          RETURN
      END IF
 
 
      IF ( POS .GT. 1 ) THEN
 
         PART = 0
 
         CALL NPARSI ( SCLKCH( :POS-1 ), PART, ERROR, PNTER )
 
         IF ( ERROR .NE. ' ' ) THEN
 
            CALL SETMSG ( 'Unable to parse the partition number '   //
     .                    'from SCLK string #.'  )
            CALL ERRCH  ( '#', SCLKCH            )
            CALL SIGERR ( 'SPICE(BADPARTNUMBER)' )
            CALL CHKOUT ( 'SCENCD'               )
            RETURN
 
         ELSE IF ( PART .LE. 0 .OR. PART .GT. NPARTS ) THEN
 
            CALL SETMSG ( 'Partition number # taken from SCLK string '//
     .                    '# is not in acceptable range 1 to #.'     )
            CALL ERRINT ( '#', PART              )
            CALL ERRCH  ( '#', SCLKCH            )
            CALL ERRINT ( '#', NPARTS            )
            CALL SIGERR ( 'SPICE(BADPARTNUMBER)' )
            CALL CHKOUT ( 'SCENCD'               )
            RETURN
 
         ELSE IF ( TICKS .LT. PSTART( PART )   .OR.
     .             TICKS .GT. PSTOP ( PART ) )  THEN
 
            CALL SETMSG ( 'SCLK count # does not fall in the '        //
     .                    'boundaries of partition number #.'  )
            CALL ERRCH  ( '#', SCLKCH        )
            CALL ERRINT ( '#', PART          )
            CALL SIGERR ( 'SPICE(NOTINPART)' )
            CALL CHKOUT ( 'SCENCD'           )
            RETURN
 
         END IF
 
      ELSE
 
         PART = 1
 
         DO WHILE (   PART  .LE. NPARTS             .AND.
     .              ( TICKS .LT. PSTART( PART )     .OR.
     .                TICKS .GT. PSTOP ( PART ) )        )
 
            PART = PART + 1
 
         END DO
 
         IF ( PART .GT. NPARTS ) THEN
 
            CALL SETMSG ( 'SCLK count # does not fall in the '        //
     .                    'boundaries of any of the partitions for '  //
     .                    'spacecraft #.'      )
            CALL ERRCH  ( '#', SCLKCH          )
            CALL ERRINT ( '#', SC              )
            CALL SIGERR ( 'SPICE(NOPARTITION)' )
            CALL CHKOUT ( 'SCENCD'             )
            RETURN
 
         END IF
 
      END IF
 
C
C     Now we have a valid partition number, and the number of ticks for
C     the clock string. To convert to ticks since the start of the
C     mission, add in the total number of ticks in preceding partitions
C     and subtract off the starting ticks value for this partition.
C
      IF ( PART .GT. 1 ) THEN
         SCLKDP = TICKS - PSTART( PART ) + PTOTLS( PART - 1 )
      ELSE
         SCLKDP = TICKS - PSTART( PART )
      END IF
 
      CALL CHKOUT ( 'SCENCD' )
      RETURN
 
      END
