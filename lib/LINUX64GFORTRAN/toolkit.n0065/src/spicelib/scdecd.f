C$Procedure      SCDECD ( Decode spacecraft clock )
 
      SUBROUTINE SCDECD ( SC, SCLKDP, SCLKCH )
 
C$ Abstract
C
C     Convert double precision encoding of spacecraft clock time into
C     a character representation.
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
      DOUBLE PRECISION      SCLKDP
      CHARACTER*(*)         SCLKCH
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     SCLKDP     I   Encoded representation of a spacecraft clock count.
C     SCLKCH     O   Character representation of a clock count.
C     MXPART     P   Maximum number of spacecraft clock partitions.
C
C$ Detailed_Input
C
C     SC         is the NAIF integer code of the spacecraft whose
C                clock's time is being decoded.
C
C     SCLKDP     is the double precision encoding of a clock time in
C                units of ticks since the spacecraft clock start time.
C                This value does reflect partition information.
C
C                An analogy may be drawn between a spacecraft clock
C                and a standard wall clock. The number of ticks
C                corresponding to the wall clock string
C
C                                hh:mm:ss
C
C                would be the number of seconds represented by that
C                time.
C
C                For example:
C
C                      Clock string      Number of ticks
C                      ------------      ---------------
C                        00:00:10              10
C                        00:01:00              60
C                        00:10:00             600
C                        01:00:00            3600
C
C                If SCLKDP contains a fractional part the result
C                is the same as if SCLKDP had been rounded to the
C                nearest whole number.
C
C$ Detailed_Output
C
C     SCLKCH     is the character representation of the clock count.
C                The exact form that SCLKCH takes depends on the
C                spacecraft.
C
C                Nevertheless, SCLKCH will have the following general
C                format:
C
C                             'pp/sclk_string'
C
C                'pp' is an integer greater than or equal to one and
C                represents a "partition number".
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
C                file which needs to be loaded into the kernel pool
C                before calling SCDECD.
C
C                The routine SCPART may be used to read the partition
C                start and stop times, in encoded units of ticks, from
C                the kernel file.
C
C                Since the end time of one partition is coincident with
C                the begin time of the next, two different time strings
C                with different partition numbers can encode into the
C                same value.
C
C                For example, if partition 1 ends at time t1, and
C                partition 2 starts at time t2, then
C
C                               '1/t1' and '2/t2'
C
C                will be encoded into the same value, say X. SCDECD
C                always decodes such values into the latter of the
C                two partitions. In this example,
C
C                          CALL SCDECD ( X, SC, CLKSTR )
C
C                will result in
C
C                          CLKSTR = '2/t2'.
C
C
C
C                'sclk_string' is a spacecraft specific clock string,
C                typically consisting of a number of components
C                separated by delimiters.
C
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
C                SCLK components may be separated by any of these five
C                characters: ' '  ':'  ','  '-'  '.'
C                The delimiter used is determined by a kernel pool
C                variable and can be adjusted by the user.
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
C$ Parameters
C
C     MXPART     is the maximum number of spacecraft clock partitions
C                expected in the kernel file for any one spacecraft.
C                See the INCLUDE file sclk.inc for this parameter's
C                value.
C
C$ Exceptions
C
C     1) If kernel variables required by this routine are unavailable,
C        the error will be diagnosed by routines called by this routine.
C        SCLKCH will be returned as a blank string in this case.
C
C     2) If the number of partitions in the kernel file for spacecraft
C        SC exceeds the parameter MXPART, the error
C        'SPICE(TOOMANYPARTS)' is signaled.  SCLKCH will be returned
C        as a blank string in this case.
C
C     3) If the encoded value does not fall in the boundaries of the
C        mission, the error 'SPICE(VALUEOUTOFRANGE)' is signaled.
C        SCLKCH will be returned as a blank string in this case.
C
C     4) If the declared length of SCLKCH is not large enough to
C        contain the output clock string the error
C        'SPICE(SCLKTRUNCATED)' is signaled either by this routine
C        or by a routine called by this routine.  On output SCLKCH
C        will contain a portion of the truncated clock string.
C
C$ Files
C
C     A kernel file containing spacecraft clock partition information
C     for the desired spacecraft must be loaded, using the routine
C     FURNSH, before calling this routine.
C
C$ Particulars
C
C     In general, it is difficult to compare spacecraft clock counts
C     numerically since there are too many clock components for a
C     single comparison.  The routine SCENCD provides a method of
C     assigning a single double precision number to a spacecraft's
C     clock count, given one of its character representations.
C
C     This routine performs the inverse operation to SCENCD, converting
C     an encoded double precision number to character format.
C
C     To convert the number of ticks since the start of the mission to
C     a clock format character string, SCDECD:
C
C        1) Determines the spacecraft clock partition that TICKS falls
C           in.
C
C        2) Subtracts off the number of ticks occurring in previous
C           partitions, to get the number of ticks since the beginning
C           of the current partition.
C
C        3) Converts the resulting ticks to clock format and forms the
C           string
C
C                      'partition_number/clock_string'
C
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
C            CHARACTER*(25)     CLKIN   ( 4 )
C            CHARACTER*(25)     CLKOUT
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
C            DATA  CLKIN     / '2/20538:39:768',
C           .                  '2/20543:21:768',
C           .                  '2/20550:37',
C           .                  '2/20561:59'         /
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
C            CALL FURNSH ( 'SCLK.KER'  )
C
C      C
C      C     Convert the tolerance string to ticks.
C      C
C            CALL SCTIKS ( SC, CLKTOL, TOL )
C
C            DO I = 1, NPICS
C
C               CALL SCENCD ( SC, CLKIN( I ), TIMEIN )
C
C               CALL CKGP   ( INST, TIMEIN, TOL, REF, CMAT, TIMOUT,
C           .                 FOUND )
C
C               CALL SCDECD ( SC, TIMOUT, CLKOUT )
C
C               WRITE (*,*)
C               WRITE (*,*) 'Input  s/c clock count: ', CLKIN( I )
C               WRITE (*,*) 'Output s/c clock count: ', CLKOUT
C               WRITE (*,*) 'Output C-Matrix:        ', CMAT
C
C            END DO
C
C
C     The output from such a program might look like:
C
C
C            Input  s/c clock count:  2/20538:39:768
C            Output s/c clock count:  2/20538:39:768
C            Output C-Matrix:  'first C-matrix'
C
C            Input  s/c clock count:  2/20543:21:768
C            Output s/c clock count:  2/20543:22:768
C            Output C-Matrix:  'second C-matrix'
C
C            Input  s/c clock count:  2/20550:37
C            Output s/c clock count:  2/20550:36:768
C            Output C-Matrix:  'third C-matrix'
C
C            Input  s/c clock count:  2/20561:59
C            Output s/c clock count:  2/20561:58:768
C            Output C-Matrix:  'fourth C-matrix'
C
C
C$ Restrictions
C
C     1) Assumes that an SCLK kernel file appropriate for the clock
C        designated by SC is loaded in the kernel pool at the time
C        this routine is called.
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
C-    SPICELIB Version 2.1.0, 05-FEB-2008 (NJB)
C
C        Values of parameter MXPART and PARTLN are now
C        provided by the INCLUDE file sclk.inc.
C
C-    SPICELIB Version 2.0.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (JML) (WLT)
C
C        The routine was changed to signal an error when SCLKCH is
C        not long enough to contain the output spacecraft clock
C        string.
C
C        FAILED is now checked after calling SCPART.
C
C        References to CLPOOL were deleted.
C
C        Miscellaneous minor updates to the header were performed.
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET)
C
C-&
 
C$ Index_Entries
C
C     decode spacecraft_clock
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 10-APR-1992 (JML) (WLT)
C
C        The routine was changed to signal an error when SCLKCH is
C        not long enough to contain the output spacecraft clock
C        string.  Previously, the SCLK routines simply truncated
C        the clock string on the right.  It was determined that
C        since this truncation could easily go undetected by the
C        user ( only the leftmost field of a clock string is
C        required when clock string is used as an input to a
C        SCLK routine ), it would be better to signal an error
C        when this happens.
C
C        FAILED is checked after calling SCPART in case an
C        error has occurred reading the kernel file and the
C        error action is not set to 'abort'.
C
C        References to CLPOOL were deleted.
C
C        Miscellaneous minor updates to the header were performed.
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-&
 

C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
      INTEGER               LSTLED
      INTEGER               LASTNB
 
C
C     Local variables
C 
      CHARACTER*(PARTLN)    PRTSTR

      INTEGER               NPARTS
      INTEGER               PART
      INTEGER               PRELEN
      INTEGER               SUFLEN
      INTEGER               I
 
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      PSTART   ( MXPART )
      DOUBLE PRECISION      PSTOP    ( MXPART )
      DOUBLE PRECISION      PTOTLS   ( MXPART )
  
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCDECD' )
      END IF
 
C
C     Use a working copy of the input.
C
      TICKS = ANINT ( SCLKDP )
 
      SCLKCH = ' '
 
C
C     Read the partition start and stop times (in ticks) for this
C     mission. Error if there are too many of them.  Also need to
C     check FAILED in case error handling is not in ABORT or
C     DEFAULT mode.
C
      CALL SCPART ( SC, NPARTS, PSTART, PSTOP )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SCDECD' )
         RETURN
      END IF
 
      IF ( NPARTS .GT. MXPART ) THEN
 
         CALL SETMSG ( 'The number of partitions, #, for spacecraft ' //
     .                 '# exceeds the value for parameter MXPART, #.' )
         CALL ERRINT ( '#', NPARTS           )
         CALL ERRINT ( '#', SC               )
         CALL ERRINT ( '#', MXPART           )
         CALL SIGERR ( 'SPICE(TOOMANYPARTS)' )
         CALL CHKOUT ( 'SCDECD'              )
         RETURN
 
      END IF
 
C
C     For each partition, compute the total number of ticks in that
C     partition plus all preceding partitions.
C
      PTOTLS( 1 ) = ANINT ( PSTOP( 1 ) - PSTART( 1 ) )
 
      DO I = 2, NPARTS
         PTOTLS( I ) = ANINT ( PTOTLS( I-1 ) + PSTOP( I ) - PSTART( I ))
      END DO
 
C
C     The partition corresponding to the input ticks is the first one
C     whose tick total is greater than the input value.  The one
C     exception is when the input ticks is equal to the total number
C     of ticks represented by all the partitions.  In this case the
C     partition number is the last one, i.e. NPARTS.
C
C     Error if TICKS comes before the first partition (that is, if it's
C     negative), or after the last one.
C
 
 
      IF ( TICKS .EQ. PTOTLS (NPARTS) )  THEN
         PART = NPARTS
      ELSE
         PART = LSTLED ( TICKS, NPARTS, PTOTLS ) + 1
      END IF
 
 
      IF ( ( TICKS .LT. 0.D0 ).OR.( PART .GT. NPARTS ) ) THEN
 
         CALL SETMSG ( 'Value for ticks, #, does not fall in any ' //
     .                 'partition for spacecraft #.'  )
         CALL ERRDP  ( '#', TICKS                     )
         CALL ERRINT ( '#', SC                        )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'       )
         CALL CHKOUT ( 'SCDECD'                       )
         RETURN
 
      END IF
 
C
C     To get the count in this partition, subtract off the total of
C     the preceding partition counts and add the beginning count for
C     this partition.
C
      IF ( PART .EQ. 1 ) THEN
         TICKS = TICKS + PSTART( PART )
      ELSE
         TICKS = TICKS + PSTART( PART ) - PTOTLS( PART - 1 )
      END IF
 
C
C     Now create the output SCLK clock string.
C
C     First convert from ticks to clock string format.
C
      CALL SCFMT (  SC, TICKS, SCLKCH )
 
C
C     Now convert the partition number to a character string and prefix
C     it to the output string.
C
      CALL INTSTR ( PART, PRTSTR )
 
      CALL SUFFIX ( '/', 0, PRTSTR )
 
      PRELEN = LASTNB ( PRTSTR )
      SUFLEN = LASTNB ( SCLKCH )
 
      IF ( LEN ( SCLKCH ) - SUFLEN .LT. PRELEN ) THEN
 
         CALL SETMSG ( 'Output string too short to contain clock '    //
     .                 'string. Input tick value: #, requires string '//
     .                 'of length #, but declared length is #.'       )
         CALL ERRDP  ( '#', SCLKDP )
         CALL ERRINT ( '#', PRELEN + SUFLEN     )
         CALL ERRINT ( '#', LEN ( SCLKCH )      )
         CALL SIGERR ( 'SPICE(SCLKTRUNCATED)'   )
         CALL CHKOUT ( 'SCDECD'                 )
         RETURN
 
      END IF
 
      CALL PREFIX ( PRTSTR, 0, SCLKCH )
 
      CALL CHKOUT ( 'SCDECD' )
      RETURN
      END
