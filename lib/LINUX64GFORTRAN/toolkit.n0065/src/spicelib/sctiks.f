C$Procedure      SCTIKS ( Convert spacecraft clock string to ticks. )
 
      SUBROUTINE SCTIKS ( SC, CLKSTR, TICKS )
 
C$ Abstract
C
C     Convert a spacecraft clock format string to number of "ticks".
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
C
      INTEGER               SC
      CHARACTER*(*)         CLKSTR
      DOUBLE PRECISION      TICKS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     CLKSTR     I   Character representation of a spacecraft clock.
C     TICKS      O   Number of ticks represented by the clock string.
C
C$ Detailed_Input
C
C     SC         is the NAIF ID number for the spacecraft whose clock
C                string is being converted.
C
C     CLKSTR     is a character string representing a spacecraft clock
C                time, WITHOUT PARTITION NUMBER.
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
C                SCLK components may be separated by any of the
C                following characters: ' '  '.'  ':'  ','  '-'
C                Any number of spaces may separate the components and
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
C     TICKS      is the number of ticks represented by the spacecraft
C                clock string. A tick is defined to be the smallest
C                time increment expressible by the spacecraft clock.
C
C                An analogy may be drawn between a spacecraft clock
C                and a standard wall clock, measuring hours, minutes
C                and seconds. The number of ticks represented by the
C                wall clock string
C                                     hh:mm:ss
C
C                would be the number of seconds represented by that
C                time.
C
C                For example:
C
C                         00:00:10  would convert to 10
C                         00:01:00  would convert to 60
C                         00:10:00  would convert to 600
C                         01:00:00  would convert to 3600
C                         01:01:00  would convert to 3660
C
C                See the Examples section below for examples for
C                actual spacecraft clocks.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the spacecraft clock type is not supported then the
C         error SPICE(NOTSUPPORTED) is signalled.
C
C     2)  If any of the extracted clock components cannot be parsed as
C         integers, or the string has too many components, or the value
C         of one of the components is less than the offset value, then
C         the error is diagnosed by routines called by this routine.
C
C     3)  Invalid spacecraft ID's are not diagnosed.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Each spacecraft is assigned a clock type code in the kernel file.
C     SCTIKS calls the function SCTYPE to determine this value. If the
C     clock type is supported by SPICELIB, then the routine TIKSnn is
C     called to handle the actual conversion from clock format to number
C     of ticks. The nn in TIKSnn refers to the spacecraft clock type
C     code. Different spacecraft have distict clock formats but can
C     still be of the same clock type.
C
C     The TIKSnn routines are entry points to the routines SCLKnn, which
C     also contain the ticks-to-clock format conversion routines FMTnn.
C     FMTnn is called by the subroutine SCFMT, which performs the
C     inverse operation to SCTIKS.
C
C     Note the important difference between SCENCD and SCTIKS. SCENCD
C     converts a clock string to the number of ticks it represents
C     since the beginning of the mission, and so uses partition
C     information. SCTIKS just converts to absolute ticks.
C
C$ Examples
C
C     SCTIKS is used as part of the process of encoding spacecraft clock
C     by SCENCD, though SCTIKS does not process any partition informa-
C     tion.
C
C     Another use of SCTIKS, however, is to convert a clock measurement
C     to ticks for use as a tolerance for the CK reader CKGP.
C
C
C     C
C     C      Get the pointing from a CK file of the VGR 1 narrow angle
C     C      image corresponding to a particular SCLK count.
C     C
C     C      Load the CK file and the kernel file containing SCLK
C     C      partition information for SCENCD.
C     C
C            CALL CKLPF  ( 'VGR1NA.CK', HANDLE )
C            CALL FURNSH ( 'SCLK.KER' )
C
C     C
C     C      Get the right ID numbers.
C     C
C            SC    = -31
C            INSTR = -31001
C
C     C
C     C      The SCLK string includes a partition number. Pictures are
C     C      never shuttered at intervals smaller than 1 MOD60 count
C     C      from each other. So use 1 MOD60 count as the time
C     C      tolerance.
C     C
C            CLKSTR = '1/20556:14:768'
C            TOLSTR = '      0:01:000'
C
C     C
C     C      Encode the clock string and the tolerance.
C     C
C            CALL SCENCD ( SC, CLKSTR, SCLK )
C            CALL SCTIKS ( SC, TOLSTR, TOL  )
C
C     C
C     C      Get the pointing from the C-kernel.
C     C
C            CALL CKGP ( INSTR, SCLK, TOL, REF, CMAT, CLKOUT, FOUND )
C
C
C
C      Below are some examples illustrating various clock string inputs
C      and the resulting outputs for the Galileo spacecraft. See the
C      SCLK required reading for a detailed description of the Galileo
C      clock format.
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
C          '0.0.001'              0
C          '0:0:002'              1
C          '0:01'                 800
C          '1'                    48000
C          '1.0'                  48000
C          '1.0.0'                Error: The 3rd component is never 0.
C          '0.0:100'              99
C          '0-60-1'               48000
C          '1-1-1'                48800
C          '1-1-2'                48801
C
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
C     J.M. Lynch     (JPL)
C     R.E. Thurman   (JPL)
C
C$ Version
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
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (JML) (RET)
C
C-&
 
C$ Index_Entries
C
C     convert spacecraft_clock string to ticks
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               SCTYPE
      LOGICAL               RETURN
 
 
C
C     Local variables
C
      INTEGER               TYPE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCTIKS' )
      END IF
 
C
C     If the spacecraft clock type is supported by NAIF then
C     call TIKSnn to perform the conversion.
C
 
      TYPE = SCTYPE ( SC )
 
      IF ( TYPE .EQ. 1 )  THEN
         CALL SCTK01 ( SC, CLKSTR, TICKS )
      ELSE
         CALL SETMSG ( 'Clock type # is not supported.' )
         CALL ERRINT ( '#', TYPE                        )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'            )
         CALL CHKOUT ( 'SCTIKS'                         )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'SCTIKS' )
 
      RETURN
      END
