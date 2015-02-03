C$Procedure       SCPS01 ( Convert type 1 SCLK string to ticks )
 
      SUBROUTINE SCPS01 ( SC, CLKSTR, ERROR, MSG, TICKS )
 
C$ Abstract
C
C     Convert a character representation of a type 1 spacecraft clock
C     count to ticks.
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
      CHARACTER*(*)         CLKSTR
      LOGICAL               ERROR
      CHARACTER*(*)         MSG
      DOUBLE PRECISION      TICKS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C     CLKSTR     I   Character representation of a clock count.
C     ERROR      O   Parsing error flag.
C     MSG        O   Output message for parsing error.
C     TICKS      O   Number of ticks represented by the clock count.
C     MXNFLD     P   Maximum number of allowed fields in an SCLK string.
C     DELIMS     P   The accepted delimiters of an SCLK string.
C     DPLEN      P   Maximum width of a clock field.
C
C$ Detailed_Input
C
C     SC         is a NAIF spacecraft identification code.  See the
C                `Examples' section below, and also the NAIF_IDS
C                required reading file for a complete list of body ID
C                codes.
C
C
C     CLKSTR     on input is the character representation of a
C                spacecraft clock count (SCLK), without a partition
C                number.
C
C                Using Galileo as an example, a SCLK string without
C                a partition number has the form
C
C                               wwwwwwww:xx:y:z
C
C                where z is a mod-8 counter (values 0-7) which
C                increments approximately once every 8 1/3 ms., y is a
C                mod-10 counter (values 0-9) which increments once
C                every time z turns over, i.e., approximately once every
C                66 2/3 ms., xx is a mod-91 (values 0-90) counter
C                which increments once every time y turns over, i.e.,
C                once every 2/3 seconds. wwwwwwww is the Real-Time
C                Image Count (RIM), which increments once every time
C                xx turns over, i.e., once every 60 2/3 seconds. The
C                roll-over expression for the RIM is 16777215, which
C                corresponds to approximately 32 years.
C
C                wwwwwwww, xx, y, and z are referred to interchangeably
C                as the fields or components of the spacecraft count.
C                SCLK components may be separated by any of the
C                single character delimiters in the string DELIMS, with
C                any number of spaces separating the components and
C                the delimiters. The presence of the RIM component
C                is required. Successive components may be omitted, and
C                in such cases are assumed to represent zero values.
C
C                Values for the individual components may exceed the
C                maximum expected values. For instance, '0:0:0:9' is
C                an acceptable Galileo clock string, and indicates the
C                same time interval as '0:0:1:1'.
C
C                Consecutive delimiters containing no intervening digits
C                are treated as if they delimit zero components, except
C                in the case of blanks.  Consecutive blanks are treated
C                as a single blank.
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
C                description of the Galileo, Mars Observer, and Voyager
C                clock formats.
C
C                See the `Examples' section in SCPS01, below.
C
C$ Detailed_Output
C
C     ERROR      is true if an error occurred parsing the input clock
C                string and converting it to ticks.
C
C     MSG        is the message generated if an error occurred parsing
C                the input clock string.
C
C     TICKS      is the number of "ticks" corresponding to the input
C                spacecraft clock string CLKSTR.  "Ticks" are the units
C                in which encoded SCLK strings are represented.
C
C                A typical Galileo SCLK string looks like
C
C                             'wwwwwwww xx y z',
C
C                as described above. Since z is the mod-8 (one tick)
C                counter, the number of ticks represented by y is 8*y.
C                And since y is the mod-10 counter, the number of ticks
C                represented by xx is 10*8*xx. The total number of
C                ticks represented by the above string is
C
C                              wwwwwwww( 7280 ) +
C                                    xx(   80 ) +
C                                     y(    8 ) +
C                                     z
C
C                Clock strings for other spacecraft are converted in
C                a similar manner.
C
C                See Examples below.
C
C$ Parameters
C
C     See the INCLUDE file sclk.inc.
C
C$ Exceptions
C
C     1)  This routine assumes that that an SCLK kernel appropriate
C         to the spacecraft clock identified by the input argument SC
C         has been loaded.  If an SCLK kernel has not been loaded,
C         does not contain all of the required data, or contains
C         invalid data, error diagnoses will be performed by routines
C         called by this routine.  The output argument TICKS will not
C         be modified.
C
C         The variables that must be set by the SCLK kernel are:
C
C            -  The number of fields in an (unabridged) SCLK string
C            -  The output delimiter code
C            -  The parallel time system code
C            -  The moduli of the fields of an SCLK string
C            -  The offsets for each clock field.
C            -  The SCLK coefficients array
C            -  The partition start times
C            -  The partition end times
C
C         When using SCLK kernels that map SCLK to a time system other
C         than ET (also called barycentric dynamical time---`TDB'), it
C         is necessary to have a leapseconds kernel loaded at the time
C         this routine is called.  If a leapseconds kernel is required
C         for conversion between SCLK and ET but is not loaded, the
C         error will be diagnosed by routines called by this routine.
C         The output argument TICKS will not be modified.
C
C         The time system that an SCLK kernel maps SCLK to is indicated
C         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
C         is the negative of the NAIF integer code for the spacecraft.
C         The time system used in a kernel is TDB if and only if the
C         variable is assigned the value 1.
C
C
C     2)  If any of the following kernel variables have invalid values,
C         the error will be diagnosed by routines called by this
C         routine:
C
C            -  The time system code
C            -  The number of SCLK coefficients
C            -  The number of partition start times
C            -  The number of partition end times
C            -  The number of fields of a SCLK string
C            -  The number of moduli for a SCLK string
C
C         If the number of values for any item read from the kernel
C         pool exceeds the maximum allowed value, it is may not be
C         possible to diagnose the error correctly, since overwriting
C         of memory may occur.  This particular type of error is not
C         diagnosed by this routine.
C
C
C     3)  The input argument CLKSTR may be invalid for a variety of
C         reasons:
C
C            -- One of the extracted clock components cannot be parsed
C               as an integer
C
C            -- CLKSTR contains too many components
C
C            -- the value  of one of the components is less than the
C               offset value
C
C         If any of these conditions is detected, the error
C         SPICE(INVALIDSCLKSTRING) is signaled.  The output argument
C         TICKS will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine converts a character string representation of a
C     spacecraft clock count into the number of ticks represented
C     by the clock count.  An important distinction between this type
C     of conversion and that carried out by SCENCD is that this routine
C     treats spacecraft clock times as representations of time
C     intervals, not absolute times.
C
C     This routine does not make use of any partition information.
C     See SCENCD for details on how to make use of partition numbers.
C
C$ Examples
C
C     1)  Below are some examples illustrating various inputs and the
C         resulting outputs for the Galileo spacecraft.
C
C         CLKSTR                TICKS
C         ----------------      --------------------
C         '0:0:0:1'             1
C         '0:0:1'               8
C         '0:1'                 80
C         '1'                   7280
C         '1 0 0 0'             7280
C         '1,0,0,0'             7280
C         '1:90'                14480
C         '1:9'                 8000
C         '1:09'                8000
C         '0-0-10'              80   |--  Third component is supposed
C         '0-1-0'               80   |    to be a mod-10 count.
C         '0/1/0'               Error: '/' is not an accepted delimiter.
C         '1: 00 : 0 : 1'       7281
C         '1:::1'               7281
C         '1.1.1.1.1'           Error: Too many components
C         '1.1.1.1.'            Error: The last delimiter signals that
C                                      a fifth component will follow.
C
C
C         The following examples are for the Voyager 2 spacecraft. Note
C         that the last component of the Voyager clock has an offset
C         value of 1.
C
C         CLKSTR                TICKS
C         ----------------      --------------------
C         '0.0.001'             0
C         '0:0:002'             1
C         '0:01'                800
C         '1'                   48000
C         '1.0'                 48000
C         '1.0.0'               Error: The 3rd component is never 0.
C         '0.0:100'             99
C         '0-60-1'              48000
C         '1-1-1'               48800
C         '1-1-2'               48801
C
C
C$ Restrictions
C
C     1)  An SCLK kernel appropriate to the spacecraft clock identified
C         by SC must be loaded at the time this routine is called.
C
C     2)  If the SCLK kernel used with this routine does not map SCLK
C         directly to barycentric dynamical time, a leapseconds kernel
C         must be loaded at the time this routine is called.
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
C-    SPICELIB Version 1.1.0, 11-FEB-2008 (NJB)
C
C        Global parameters are now declared in the Fortran
C        INCLUDE file sclk.inc.
C
C-    SPICELIB Version 1.0.0, 25-FEB-1993 (JML)
C
C-&
 
C$ Index_Entries
C
C     convert type_1 spacecraft_clock string to ticks
C
C-&
 
C
C     SPICELIB functions
C
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN =    240 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =     60 )
 
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN =    320 )
 
      INTEGER               INITSC
      PARAMETER           ( INITSC =      0 )
 
      DOUBLE PRECISION      INITID
      PARAMETER           ( INITID = -1.D20 )
 
C
C     Following are parameters for the indices within the
C     array NAMLST of the kernel variable names.
C
 
      INTEGER               NFLIDX
      PARAMETER           ( NFLIDX = 1 )
 
      INTEGER               OFFIDX
      PARAMETER           ( OFFIDX = NFLIDX + 1 )
 
      INTEGER               MODIDX
      PARAMETER           ( MODIDX = OFFIDX + 1 )
 
      INTEGER               NKITEM
      PARAMETER           ( NKITEM = MODIDX )
 
C
C     Local variables
C
      CHARACTER*(DPLEN)     CMP    ( MXNFLD )
      CHARACTER*(ERRLEN)    STRERR
      CHARACTER*(NAMLEN)    NAMLST ( NKITEM )
 
      DOUBLE PRECISION      CMPTKS ( MXNFLD )
      DOUBLE PRECISION      CMPVAL ( MXNFLD )
      DOUBLE PRECISION      MODULI ( MXNFLD )
      DOUBLE PRECISION      OFFSET ( MXNFLD )
 
      INTEGER               I
      INTEGER               N
      INTEGER               NFIELD
      INTEGER               PNTR
 
 
C
C     Save everything
C
      SAVE
 
 
 
      DATA NAMLST / 'SCLK01_N_FIELDS',
     .              'SCLK01_OFFSETS',
     .              'SCLK01_MODULI'        /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCPS01' )
      END IF
 
C
C     Start off with the error flag and message set for a regular
C     SPICE error.
C
      ERROR = .TRUE.
      MSG   = 'SPICELIB error detected.'
 
C
C     Our first piece of business is to look up all of the data
C     we require from the kernel pool.  We must form the names
C     of the items we want using the input S/C ID code.  The items
C     we need are:
C
C        -  The number of fields in an (unabridged) SCLK string
C        -  The moduli of the fields of an SCLK string
C        -  The offsets for each clock field.
C
 
      CALL SCLI01 ( NAMLST(NFLIDX), SC, MXNFLD, N, NFIELD )
      CALL SCLD01 ( NAMLST(MODIDX), SC, MXNFLD, N, MODULI )
      CALL SCLD01 ( NAMLST(OFFIDX), SC, MXNFLD, N, OFFSET )
 
C
C     Don't try to continue if we had a lookup error.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SCPS01' )
         RETURN
      END IF
 
C
C     If our clock string is blank, we can stop now.
C
      IF ( CLKSTR .EQ. ' ' ) THEN
 
         MSG =  'Non partition part of the input clock string is ' //
     .          'blank.'
 
         ERROR = .TRUE.
 
         CALL CHKOUT ( 'SCPS01' )
         RETURN
 
      END IF
 
C
C     Determine how many ticks is each field is worth.
C
      CMPTKS( NFIELD ) = 1.D0
 
      DO  I = NFIELD-1, 1, -1
         CMPTKS(I) = CMPTKS(I+1) * MODULI(I+1)
      END DO
 
C
C     Parse the clock components from the input string. There should
C     be at most NFIELD of them, but, in order to check for too long
C     a clock string, we'll let LPARSM take up to MXNFLD components and
C     then test for an error.
C
      CALL LPARSM ( CLKSTR, DELIMS, MXNFLD, N, CMP )
 
C
C     If the string has too many fields for the specified spacecraft
C     then signal an error.
C
      IF  ( N .GT. NFIELD )  THEN
 
         ERROR = .TRUE.
 
         MSG = 'Input clock string # has # fields; maximum for this ' //
     .         'spacecraft clock is #.'
 
         CALL REPMC ( MSG, '#', CLKSTR, MSG )
         CALL REPMI ( MSG, '#', N,      MSG )
         CALL REPMI ( MSG, '#', NFIELD, MSG )
 
         CALL CHKOUT ( 'SCPS01' )
         RETURN
 
      END IF
 
C
C     Convert each of the components into numbers.  Error if any
C     of the conversions screw up.
C
      DO I = 1, N
 
         IF ( CMP(I) .EQ. ' ' )  THEN
            CMPVAL(I) = OFFSET(I)
         ELSE
            CALL NPARSD ( CMP(I), CMPVAL(I), STRERR, PNTR )
         END IF
 
         IF ( STRERR .NE. ' ' ) THEN
 
            ERROR = .TRUE.
 
            MSG = 'Could not parse SCLK component # from ' //
     .            '# as a number.'
 
            CALL REPMC ( MSG, '#', CMP(I), MSG )
            CALL REPMC ( MSG, '#', CLKSTR, MSG )
 
            CALL CHKOUT ( 'SCPS01' )
            RETURN
 
         END IF
 
C
C        Subtract off the offset value so that we can do base ten
C        arithmetic.  Also, if any of the components become negative
C        as a result of the subtraction, then that component must
C        have been invalid.
C
         CMPVAL(I) = CMPVAL(I) - OFFSET(I)
 
         IF  ( ANINT( CMPVAL(I) ) .LT. 0.D0  )   THEN
 
            ERROR = .TRUE.
 
            MSG = 'Component number #, # in the SCLK string ' //
     .            ' # is invalid.'
 
            CALL REPMI ( MSG, '#', I,      MSG )
            CALL REPMC ( MSG, '#', CMP(I), MSG )
            CALL REPMC ( MSG, '#', CLKSTR, MSG )
 
            CALL CHKOUT ( 'SCPS01' )
            RETURN
 
         END IF
 
      END DO
 
C
C     Convert to ticks by multiplying the value of each component by
C     the number of ticks each component count represents, and then
C     add up the results.
C
      TICKS = 0.D0
 
      DO I = 1, N
         TICKS = TICKS + CMPVAL(I)*CMPTKS(I)
      END DO
 
      ERROR = .FALSE.
 
      MSG   = ' '
 
      CALL CHKOUT ( 'SCPS01' )
      RETURN
      END
 
