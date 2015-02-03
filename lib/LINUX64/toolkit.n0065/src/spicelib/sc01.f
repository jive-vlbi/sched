C$Procedure      SC01 ( Spacecraft clock, type 1 )
 
      SUBROUTINE SC01 ( SC, CLKSTR, TICKS, SCLKDP, ET )
 
C$ Abstract
C
C     Perform time conversions between different representations of
C     type 1 spacecraft clock.
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
C     TIME
C
C$ Keywords
C
C     CONVERSION
C     TIME
C
C$ Declarations

      IMPLICIT NONE
      
      INCLUDE               'sclk.inc'
      INCLUDE               'zzctr.inc'
 
      INTEGER               SC
      CHARACTER*(*)         CLKSTR
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      ET
  
C$ Brief_I/O
C
C     Variable  I/O  Entry point
C     --------  ---  --------------------------------------------------
C     SC         I   (All)
C     CLKSTR    I,O  SCTK01, SCFM01
C     TIKS      I,O  SCTK01, SCFM01
C     SCLKDP    I,O  SCTE01, SCET01, SCEC01
C     ET        I,O  SCTE01, SCET01, SCEC01
C     MXCOEF     P   SCTE01, SCET01
C     MXPART     P   (All)
C     DELIMS     P   SCTK01, SCFM01
C     MXNFLD     P   SCTK01, SCFM01
C     DPLEN      P   SCTK01, SCFM01
C
C$ Detailed_Input
C
C     See the entry points SCTK01, SCFM01, SCET01, SCTE01, SCEC01.
C
C$ Detailed_Output
C
C     See the entry points SCTK01, SCFM01, SCET01, SCTE01, SCEC01.
C
C$ Parameters
C
C     MXCOEF         is the maximum number of coefficient sets in the
C                    array COEFFS that defines the mapping between
C                    encoded type 1 SCLK and a parallel time system,
C                    such as TDB or TDT.  This array has dimension
C                    3 x MXCOEF.  The value of MXCOEF may be increased
C                    as required.
C
C     MXPART         is the maximum number of partitions for any type 1
C                    spacecraft clock.  Type 1 SCLK kernels contain
C                    start and stop times for each partition.  The value
C                    of MXPART may be increased as required.
C
C     MXNFLD         is an upper bound on the number of components in
C                    the clock string.
C
C     DPLEN          is an upper bound on the width of the individual
C                    components of the clock string.
C
C     DELIMS         are the characters that are accepted delimiters of
C                    the clock components in the input SCLK string.
C
C$ Exceptions
C
C     1)  If SC01 is called directly, the error SPICE(BOGUSENTRY)
C         is signalled.
C
C     2)  See the entry points SCTK01, SCFM01, SCET01, SCTE01 for a
C         description of the exceptions specific to those routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     SC01 serves as an umbrella routine under which the shared
C     variables of its entry points are declared.  SC01 should
C     never be called directly.
C
C     The entry points of SC01 are
C
C        SCTK01 ( SCLK to ticks,          type 1 )
C        SCFM01 ( Format,                 type 1 )
C        SCET01 ( ET to ticks,            type 1 )
C        SCEC01 ( ET to continuous ticks, type 1 )
C        SCTE01 ( Ticks to ET,            type 1 )
C
C$ Examples
C
C     See the entry points SCTK01, SCFM01, SCET01, SCEC01, SCTE01.
C
C$ Restrictions
C
C     1)  An SCLK kernel appropriate to the spacecraft clock identified
C         by SC must be loaded at the time any entry point of this
C         routine is called.
C
C     2)  If the SCLK kernel used with this routine does not map SCLK
C         directly to barycentric dynamical time, a leapseconds kernel
C         must be loaded at the time any entry point of this routine is
C         called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     J.M. Lynch     (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.4.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 3.3.0, 05-MAR-2009 (NJB)
C
C        Bug fix: the entry points of this routine now keep track of
C        whether their kernel pool look-ups succeeded. If not, a kernel
C        pool lookup is attempted on the next call to any entry point
C        of this routine.
C
C-    SPICELIB Version 3.2.0, 17-FEB-2008 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C        Bug fix: spaces between fields are now inserted
C        correctly when the output field delimiter is blank.
C
C-    SPICELIB Version 3.1.1, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references
C        to FURNSH.
C
C-    SPICELIB Version 3.1.0, 24-JAN-2003 (BVS)
C
C        Increased MXCOEF to 10000.
C
C-    SPICELIB Version 3.0.0, 09-MAR-1999 (NJB)
C
C        Added new entry point SCEC01.  Removed some extraneous
C        C's from column 1; these had been added by a wayward
C        preprocessor.
C
C        Removed local variable RNDCLK; entry point SCTE01 no longer
C        creates a rounded version of its input argument.
C
C        Updated/fixed various comments here and in entry SCET01.
C
C-    SPICELIB Version 2.1.0, 07-JUL-1996 (NJB)
C
C        Removed declaration, DATA and SAVE statements for unused
C        variables NFDMSG and OLDID.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        All entry points were updated to handle SCLK kernels that
C        map between SCLK and a variety of time systems; formerly
C        only TDB was supported.  All entry points have had corrections
C        and additions made to their headers.  Comment section for
C        permuted index source lines was added following the header.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) (JML)
C
C-&
 
C$ Index_Entries
C
C     type_1 spacecraft_clock
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.3.0, 05-MAR-2009 (NJB)
C
C        Bug fix: the entry points of this routine now keep track of
C        whether their kernel pool look-ups succeeded. If not, a kernel
C        pool lookup is attempted on the next call to any entry point
C        of this routine.
C
C        All entry points of this routine look up the same kernel
C        variables, and use the saved variable UPDATE to indicate that
C        a kernel pool look-up is needed. A look-up failure occurring
C        in any entry point will now prevent all entry points from
C        relying on stored kernel data.
C
C
C-    SPICELIB Version 3.2.0, 17-FEB-2008 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C        Bug fix: spaces between fields are now inserted
C        correctly when the output field delimiter is blank.
C
C        Unused parameter INITID was removed.
C
C-    SPICELIB Version 3.1.0, 24-JAN-2003 (BVS)
C
C        Increased MXCOEF to 10000.
C
C-    SPICELIB Version 3.0.0, 06-JAN-1999 (NJB)
C
C        Added new entry point SCEC01.  Removed some extraneous
C        C's from column 1; these had been added by a wayward
C        preprocessor.
C
C        Removed local variable RNDCLK; entry point SCTE01 no longer
C        creates a rounded version of its input argument.
C
C        Updated/fixed various comments here and in entry SCET01.
C
C-    SPICELIB Version 2.1.0, 07-JUL-1996 (NJB)
C
C        Removed declaration, DATA and SAVE statements for unused
C        variables NFDMSG and OLDID.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        Entry points SCET01 and SCTE01 were updated to handle a time
C        system specification for the `parallel' time system
C        in the SCLK kernel.  Formerly, the only time system that
C        an SCLK kernel could map SCLK to was TDB.  Now TDT is
C        supported, and the mechanism for allowing other parallel
C        time systems is in place.
C
C        To support a new parallel time system, it is necessary
C        to
C
C           -- Update SCTE01 so that after the routine converts an input
C              tick value to a value in the parallel system, the
C              resulting value is converted to TDB.  See the current
C              treatment of TDT in that routine for an example of how
C              this is done.
C
C           -- Update SCET01 so that the input TDB value can be
C              converted to a value in the new parallel system when
C              required.  This converted value is then used as an input
C              to the interpolation algorithm performed in SCET01.  See
C              the current treatment of TDT in that routine for an
C              example of how this is done.
C
C           -- Update the parameter MXTSYS in SCLU01 to indicate the
C              new number of supported parallel time systems.
C
C           -- Update the SCLK Required Reading to document the
C              description of the currently supported parallel time
C              systems.
C
C        See the named entry points for further details.
C
C        The kernel pool routines SWPOOL and CVPOOL are now used
C        to determine when it is necessary to look up kernel pool
C        constants.  The variable UPDATE is now used to indicate
C        when it is necessary to look up the kernel variables used by
C        this suite of routines.  All of the entry points SCFM01,
C        SCTK01, SCET01, and SCTE01 were affected by this update.
C
C        All of the entry points have had their headers updated to
C        discuss the fact that a leapseconds kernel will now need to be
C        loaded in order to use SCLK kernels that map between SCLK and
C        a parallel time system other than TDB.
C
C        In this routine, a comment section for permuted index
C        source lines was added following the header.
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      UNITIM
 
      INTEGER               SUMAI
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
 
C
C     Local parameters
C
      INTEGER               TDB
      PARAMETER           ( TDB    =      1 )
 
      INTEGER               TDT
      PARAMETER           ( TDT    =      2 )
 
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN =    240 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =     60 )
 
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN =    320 )
 
      INTEGER               INITSC
      PARAMETER           ( INITSC =      0 )
 
C
C     Following are parameters for the indices within the
C     array NAMLST of the kernel variable names used by the
C     SC01 entry points.
C
      INTEGER               KIDIDX
      PARAMETER           ( KIDIDX =          1 )
 
      INTEGER               COFIDX
      PARAMETER           ( COFIDX = KIDIDX + 1 )
 
      INTEGER               PRSIDX
      PARAMETER           ( PRSIDX = COFIDX + 1 )
 
      INTEGER               PREIDX
      PARAMETER           ( PREIDX = PRSIDX + 1 )
 
      INTEGER               NFLIDX
      PARAMETER           ( NFLIDX = PREIDX + 1 )
 
      INTEGER               OFFIDX
      PARAMETER           ( OFFIDX = NFLIDX + 1 )
 
      INTEGER               MODIDX
      PARAMETER           ( MODIDX = OFFIDX + 1 )
 
      INTEGER               DELIDX
      PARAMETER           ( DELIDX = MODIDX + 1 )
 
      INTEGER               SYSIDX
      PARAMETER           ( SYSIDX = DELIDX + 1 )
 
      INTEGER               NKITEM
      PARAMETER           ( NKITEM = SYSIDX     )
 
 
C
C     Local variables
C
      CHARACTER*(MSGLEN)    BVLMSG
      CHARACTER*(DPLEN)     CMP    ( MXNFLD )
      CHARACTER*(1)         DEL    ( NDELIM )
      CHARACTER*(DPLEN)     DPCHAR
      CHARACTER*(ERRLEN)    ERROR
      CHARACTER*(NAMLEN)    NAMLST ( NKITEM )
      CHARACTER*(NAMLEN)    KVNAME ( NKITEM )
 
      DOUBLE PRECISION      CMPTKS ( MXNFLD )
      DOUBLE PRECISION      CMPVAL ( MXNFLD )
      DOUBLE PRECISION      COEFFS ( 3, MXCOEF )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      MODULI ( MXNFLD )
      DOUBLE PRECISION      MAXWID
      DOUBLE PRECISION      MXTICK
      DOUBLE PRECISION      OFFSET ( MXNFLD )
      DOUBLE PRECISION      PARTIM
      DOUBLE PRECISION      PREND  ( MXPART )
      DOUBLE PRECISION      PRSTRT ( MXPART )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      TIKDIF
      DOUBLE PRECISION      TIKMSC
      DOUBLE PRECISION      TIMDIF
      DOUBLE PRECISION      REM
 
      INTEGER               CMPWID ( MXNFLD )
      INTEGER               DELCDE
      INTEGER               END
      INTEGER               I
      INTEGER               J
      INTEGER               LENGTH ( MXNFLD )
      INTEGER               LOWER
      INTEGER               MIDDLE
      INTEGER               N
      INTEGER               NCOEFF
      INTEGER               NEEDED
      INTEGER               NFIELD
      INTEGER               NPART
      INTEGER               NTSYS
      INTEGER               OLDSC
      INTEGER               PAD
      INTEGER               PNTR
      INTEGER               TIMSYS
      INTEGER               UPPER
      INTEGER               USRCTR ( CTRSIZ ) 
 
      LOGICAL               FIRST
      LOGICAL               NODATA
      LOGICAL               UPDATE
 
 
C
C     Saved variables
C
 
C
C     There are at least a half dozen distinct items to save.  We're
C     safer just saving everything.
C
C     Maintenance programming note: the coefficient buffer
C     should be saved in any event to prevent memory problems
C     on some platforms.
C
      SAVE
 
 
 
C
C     Initial values
C
      DATA BVLMSG / 'Invalid value of #. Value was #.'    /
 
      DATA DEL    / '.'    ,
     .              ':'    ,
     .              '-'    ,
     .              ','    ,
     .              ' '                                   /
 
      DATA FIRST  / .TRUE.                                /
 
      DATA NAMLST /
     .
     .      'SCLK_KERNEL_ID',
     .      'SCLK01_COEFFICIENTS',
     .      'SCLK_PARTITION_START',
     .      'SCLK_PARTITION_END',
     .      'SCLK01_N_FIELDS',
     .      'SCLK01_OFFSETS',
     .      'SCLK01_MODULI',
     .      'SCLK01_OUTPUT_DELIM',
     .      'SCLK01_TIME_SYSTEM'                          /
 
 
      DATA NODATA / .TRUE.                                /
 
      DATA OLDSC  / INITSC                                /
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SC01' )
      END IF
 
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'SC01' )
      RETURN
 
 
 
 
C$Procedure       SCTK01 ( Convert type 1 SCLK string to ticks )
 
      ENTRY SCTK01 ( SC, CLKSTR, TICKS )
 
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
C
C     CHARACTER*(*)         CLKSTR
C     DOUBLE PRECISION      TICKS
C     INTEGER               SC
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C     CLKSTR     I   Character representation of a clock count.
C     TICKS      O   Number of ticks represented by the clock count.
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
C                See the `Examples' section in SCTK01, below.
C
C$ Detailed_Output
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
C     None.
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
C         SPICE(INVALIDSCLKSTRING) is signalled.  The output argument
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
C     B.V. Semenov (JPL)
C     R.E. Thurman (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        Header was updated, particularly $Exceptions and $Restrictions
C        sections.  Kernel pool watch is now set on required kernel
C        variables.  Comment section for permuted index source lines
C        was added following the header.
C
C-    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB) (JML) (RET)
C
C-&
 
C$ Index_Entries
C
C     convert type_1 spacecraft_clock string to ticks
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        This routine now uses the new kernel pool watch capability
C        to determine when it is necessary to look up SCLK variables.
C        This method of checking for kernel pool updates replaces the
C        previously used once-per-call lookup of the SCLK_KERNEL_ID
C        kernel variable.
C
C        The header was updated to discuss the fact that a leapseconds
C        kernel will now need to be loaded in order to use SCLK kernels
C        that map between SCLK and a parallel time system other than
C        TDB.  The $Exceptions and $Restrictions sections were affected.
C
C        A comment section for permuted index source lines was added
C        following the header.
C
C-&
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCTK01' )
      END IF
 
C
C     On the first pass through the subroutine, or if the spacecraft
C     clock ID changes, we will set watches on the SCLK kernel
C     variables for the current clock.
C
      IF (  FIRST  .OR.  ( SC .NE. OLDSC )  ) THEN
 
         FIRST = .FALSE.
 
C
C        Make up a list of names of kernel variables that we'll use.
C        The first name in the list is SCLK_KERNEL_ID, which does not
C        require the addition of a spacecraft code suffix.  For the
C        rest of the names, we'll have to add the suffix.
C
         KVNAME(KIDIDX) = NAMLST(KIDIDX)
 
         CALL MOVEC  ( NAMLST,  NKITEM,  KVNAME )
 
         DO I = COFIDX, SYSIDX
            CALL SUFFIX ( '_#',        0,         KVNAME(I) )
            CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
         END DO
 
C
C        Set a watch on all of the kernel variables we use.
C
         CALL SWPOOL ( 'SC01',  NKITEM,  KVNAME )
 
C
C        Keep track of the last spacecraft clock ID encountered.
C
         OLDSC =  SC
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

      END IF
 
C
C     Find out whether we need to look up new format descriptors from
C     the kernel pool.  If any relevant kernel variables were updated,
C     we have to do a look-up.  Note that changing the s/c clock ID
C     causes a new watch to be set, so a look-up is required.  When we
C     do a look-up, we grab everything that any of the SC01 entry
C     points might need.
C
      CALL ZZCVPOOL ( 'SC01', USRCTR, UPDATE )
 
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Our first piece of business is to look up all of the data
C        we require from the kernel pool.  We must form the names
C        of the items we want using the input S/C ID code.  The items
C        we need are:
C
C           -  The number of fields in an (unabridged) SCLK string
C           -  The output delimiter code
C           -  The parallel time system code
C           -  The moduli of the fields of an SCLK string
C           -  The offsets for each clock field.
C           -  The SCLK coefficients array
C           -  The partition start times
C           -  The partition end times
C
 
         CALL SCLI01 ( NAMLST(NFLIDX), SC, 1,        N,      NFIELD )
         CALL SCLI01 ( NAMLST(DELIDX), SC, 1,        N,      DELCDE )
         CALL SCLI01 ( NAMLST(SYSIDX), SC, 1,        NTSYS,  TIMSYS )
 
         CALL SCLD01 ( NAMLST(COFIDX), SC, 3*MXCOEF, NCOEFF, COEFFS )
         CALL SCLD01 ( NAMLST(PRSIDX), SC, MXPART,   N,      PRSTRT )
         CALL SCLD01 ( NAMLST(PREIDX), SC, MXPART,   NPART,  PREND  )
         CALL SCLD01 ( NAMLST(MODIDX), SC, MXNFLD,   N,      MODULI )
         CALL SCLD01 ( NAMLST(OFFIDX), SC, MXNFLD,   N,      OFFSET )
 
C
C        Don't try to continue if we had a lookup error.
C
         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT( 'SCTK01' )
            RETURN

         END IF

C
C        The kernel pool look-up succeeded.
C
         NODATA = .FALSE.
 
C
C        Use the default time system (TDB) if none was specified in the
C        SCLK kernel.
C
         IF ( NTSYS .EQ. 0 ) THEN
            TIMSYS = TDB
         END IF
 
 
      END IF
 
C
C     If our clock string is blank, we can stop now.
C
      IF ( CLKSTR .EQ. ' ' ) THEN
 
         CALL SETMSG ( 'CLKSTR is blank.'         )
         CALL SIGERR ( 'SPICE(INVALIDSCLKSTRING)' )
         CALL CHKOUT ( 'SCTK01'                   )
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
 
         CALL SETMSG ( 'CLKSTR has # fields, which is too many.' )
         CALL ERRINT ( '#' , N                                   )
         CALL SIGERR ( 'SPICE(INVALIDSCLKSTRING)'                )
         CALL CHKOUT ( 'SCTK01'                                  )
         RETURN
 
      END IF
 
C
C     Convert each of the components into numbers.  Error if any
C     of the conversions screw up.  NPARSD doesn't assign a value
C     to ' ', so assign the numeric value of the blank components
C     to be equal to the offset value.
C
      DO I = 1, N
 
         IF ( CMP(I) .EQ. ' ' )  THEN
            CMPVAL(I) = OFFSET(I)
         ELSE
            CALL NPARSD ( CMP(I), CMPVAL(I), ERROR, PNTR )
         END IF
 
 
         IF ( ERROR .NE. ' ' ) THEN
 
            CALL SETMSG ( 'Could not parse SCLK component # from '    //
     .                    '# as a number.'                           )
            CALL ERRCH  ( '#', CMP(I)                                )
            CALL ERRCH  ( '#', CLKSTR                                )
            CALL SIGERR ( 'SPICE(INVALIDSCLKSTRING)'                 )
            CALL CHKOUT ( 'SCTK01'                                   )
            RETURN
 
          END IF
 
C
C         Subtract off the offset value so that we can do base ten
C         arithmetic.  Also, if any of the components become negative
C         as a result of the subtraction, then that component must
C         have been invalid.
C
          CMPVAL(I) = CMPVAL(I) - OFFSET(I)
 
 
          IF  ( ANINT( CMPVAL(I) ) .LT. 0.D0  )   THEN
 
            CALL SETMSG ( ' Component number # in the SCLK string '//
     .                    ' is invalid                            '  )
            CALL ERRINT ( '#', I                                     )
            CALL SIGERR ( 'SPICE(INVALIDSCLKSTRING)'                 )
            CALL CHKOUT ( 'SCTK01'                                   )
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
 
      CALL CHKOUT ( 'SCTK01' )
      RETURN
 
 
 
 
C$Procedure      SCFM01 ( Convert ticks to a type 1 SCLK string. )
 
      ENTRY SCFM01 ( SC, TICKS, CLKSTR )
 
C$ Abstract
C
C     Convert a number of ticks to an equivalent type 1 spacecraft clock
C     string.
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
C     INTEGER               SC
C     DOUBLE PRECISION      TICKS
C     CHARACTER*(*)         CLKSTR
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft identification code.
C     TICKS      I   Number of ticks represented by a clock count.
C     CLKSTR     O   Character string representation of the clock count.
C
C$ Detailed_Input
C
C     SC         is a NAIF spacecraft identification code.  See the
C                `Examples' section below, and also the KERNEL required
C                reading file for a complete list of body ID codes.
C
C
C     TICKS      is the number of ticks to be converted to a spacecraft
C                clock string, where a tick is defined to be
C                the smallest time increment expressible by the
C                spacecraft clock.
C
C                If TICKS contains a fractional part, the string that
C                results is the same as if TICKS had been rounded to
C                the nearest whole number.
C
C                See Examples below.
C
C$ Detailed_Output
C
C
C     CLKSTR     on output is the character string representation of
C                the spacecraft clock count. The returned string has
C                the form
C
C                                 'wwwwwwww:xx:y:z',
C
C                where the number of components and the width of each
C                one are different for each spacecraft.  The delimiter
C                used is determined by a kernel pool variable and is
C                one of the five specified by the parameter DELIMS.
C                See Examples below.
C
C                If CLKSTR is not long enough to accommodate the
C                formatted tick value, the result will be truncated on
C                the right.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine assumes that that an SCLK kernel appropriate
C         to the spacecraft clock identified by the input argument SC
C         has been loaded.  If an SCLK kernel has not been loaded,
C         does not contain all of the required data, or contains
C         invalid data, error diagnoses will be performed by routines
C         called by this routine.  The output argument CLKSTR will not
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
C         The output argument CLKSTR will not be modified.
C
C         The time system that an SCLK kernel maps SCLK to is indicated
C         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
C         is the negative of the NAIF integer code for the spacecraft.
C         The time system used in a kernel is TDB if and only if the
C         variable is assigned the value 1.
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
C     3)  If the input value for TICKS is negative, the error
C         SPICE(VALUEOUTOFRANGE) is signalled.  The output argument
C         CLKSTR will not be modified.
C
C     4)  If the output argument CLKSTR is too short to accommodate
C         the output string produced by this routine, the error
C         SPICE(SCLKTRUNCATED) is signalled.  The output string
C         CLKSTR will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The routine determines the values of the components of the
C     spacecraft clock count that is equivalent to the number TICKS.
C     The information needed to perform this operation, such as the
C     number of clock components and their moduli, is provided by
C     an SCLK kernel file.  Normally, your program should load this
C     file during initialization.
C
C     This routine does not make use of any partition information.
C     See SCDECD for details on how to make use of partition numbers.
C
C$ Examples
C
C      Below are some examples illustrating various inputs and the
C      resulting outputs for the Galileo spacecraft.
C
C         TICKS                 CLKSTR
C         ----------------      --------------------
C         -1                    Error: Ticks must be a positive number
C         0                     '0:00:0:0'
C         1                     '0:00:0:1'
C         1.3                   '0:00:0:1'
C         1.5                   '0:00:0:2'
C         2                     '0:00:0:2'
C         7                     '0:00:0:7'
C         8                     '0:00:1:0'
C         80                    '0:01:0:0'
C         88                    '0:01:1:0'
C         7279                  '0:90:9:7'
C         7280                  '1:00:0:0'
C         1234567890            '169583:45:6:2'
C
C
C     The following examples are for the Voyager 2 spacecraft.
C     Note that the third component of the Voyager clock has an
C     offset value of one.
C
C         TICKS                 CLKSTR
C         ----------------      --------------------
C         -1                    Error: Ticks must be a positive number
C         0                     '00000 00 001'
C         1                     '00000 00 002'
C         1.3                   '00000:00:002'
C         1.5                   '00000.00.003'
C         2                     '00000-00-003'
C         799                   '00000,00,800'
C         800                   '00000 01 001'
C         47999                 '00000 59 800'
C         48000                 '00001 00 001'
C         3145727999            '65535 59 800'
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
C     B.V. Semenov (JPL)
C     R.E. Thurman (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 2.1.0, 17-FEB-2008 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C        Bug fix: spaces between fields are now inserted
C        correctly when the output field delimiter is blank.
C
C-    SPICELIB Version 2.0.1, 18-JUL-1996 (NJB)
C
C        Misspelling in header fixed.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        Error is now signalled if truncation of output string occurs.
C        Header was updated, particularly $Exceptions and $Restrictions
C        sections.  Kernel pool watch is now set on required kernel
C        variables.  Comment section for permuted index source lines
C        was added following the header.
C
C-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB) (JML) (RET)
C
C-&
 
C$ Index_Entries
C
C     convert ticks to a type_1 spacecraft_clock string
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 17-FEB-2008 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C        Bug fix: spaces between fields are now inserted
C        correctly when the output field delimiter is blank.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        An error is now signalled if truncation of output string
C        occurs.
C
C        The header was updated to discuss exception handling when
C        the output string is truncated.  The header was also expanded
C        to discuss the fact that a leapseconds kernel will now need to
C        be loaded in order to use SCLK kernels that map between SCLK
C        and a parallel time system other than TDB.  The $Exceptions
C        and $Restrictions sections were affected.
C
C        This routine now uses the new kernel pool watch capability
C        to determine when it is necessary to look up SCLK variables.
C        This method of checking for kernel pool updates replaces the
C        previously used once-per-call lookup of the SCLK_KERNEL_ID
C        kernel variable.
C
C        A comment section for permuted index source lines was added
C        following the header.
C
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCFM01' )
      END IF
 
C
C     On the first pass through the subroutine, or if the spacecraft
C     clock ID changes, we will set watches on the SCLK kernel
C     variables for the current clock.
C
      IF (  FIRST  .OR.  ( SC .NE. OLDSC )  ) THEN
 
         FIRST = .FALSE.
 
C
C        Make up a list of names of kernel variables that we'll use.
C        The first name in the list is SCLK_KERNEL_ID, which does not
C        require the addition of a spacecraft code suffix.  For the
C        rest of the names, we'll have to add the suffix.
C
         KVNAME(KIDIDX) = NAMLST(KIDIDX)
 
         CALL MOVEC  ( NAMLST,  NKITEM,  KVNAME )
 
         DO I = COFIDX, SYSIDX
            CALL SUFFIX ( '_#',        0,         KVNAME(I) )
            CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
         END DO
 
C
C        Set a watch on all of the kernel variables we use.
C
         CALL SWPOOL ( 'SC01',  NKITEM,  KVNAME )
 
C
C        Keep track of the last spacecraft clock ID encountered.
C
         OLDSC =  SC
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

      END IF
 
C
C     Find out whether we need to look up new format descriptors from
C     the kernel pool.  If any relevant kernel variables were updated,
C     we have to do a look-up.  Note that changing the s/c clock ID
C     causes a new watch to be set, so a look-up is required.  When we
C     do a look-up, we grab everything that any of the SC01 entry
C     points might need.
C
      CALL ZZCVPOOL ( 'SC01', USRCTR, UPDATE )
 
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Our first piece of business is to look up all of the data
C        we require from the kernel pool.  We must form the names
C        of the items we want using the input S/C ID code.  The items
C        we need are:
C
C           -  The number of fields in an (unabridged) SCLK string
C           -  The output delimiter code
C           -  The parallel time system code
C           -  The moduli of the fields of an SCLK string
C           -  The offsets for each clock field.
C           -  The SCLK coefficients array
C           -  The partition start times
C           -  The partition end times
C
 
         CALL SCLI01 ( NAMLST(NFLIDX), SC, 1,        N,      NFIELD )
         CALL SCLI01 ( NAMLST(DELIDX), SC, 1,        N,      DELCDE )
         CALL SCLI01 ( NAMLST(SYSIDX), SC, 1,        NTSYS,  TIMSYS )
 
         CALL SCLD01 ( NAMLST(COFIDX), SC, 3*MXCOEF, NCOEFF, COEFFS )
         CALL SCLD01 ( NAMLST(PRSIDX), SC, MXPART,   N,      PRSTRT )
         CALL SCLD01 ( NAMLST(PREIDX), SC, MXPART,   NPART,  PREND  )
         CALL SCLD01 ( NAMLST(MODIDX), SC, MXNFLD,   N,      MODULI )
         CALL SCLD01 ( NAMLST(OFFIDX), SC, MXNFLD,   N,      OFFSET )
 
C
C        Don't try to continue if we had a lookup error.
C
         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT( 'SCFM01' )
            RETURN
         END IF

C
C        The kernel pool look-up succeeded.
C
         NODATA = .FALSE.
 
C
C        Use the default time system (TDB) if none was specified in the
C        SCLK kernel.
C
         IF ( NTSYS .EQ. 0 ) THEN
            TIMSYS = TDB
         END IF
 
      END IF
 
C
C     Determine how many ticks each field is worth.
C
      CMPTKS( NFIELD ) = 1.D0
 
      DO  I = NFIELD-1, 1, -1
         CMPTKS(I) = CMPTKS(I+1) * MODULI(I+1)
      END DO
 
C
C     Determine the width of each field.
C
      DO I  = 1, NFIELD
 
         MAXWID     =  MODULI(I) + OFFSET(I) - 1.D0
 
         CMPWID(I)  =  1 + INT ( LOG10( MAXWID + 0.5D0 ) )
 
      END DO
 
C
C     Check whether the output string is long enough to contain the
C     string we're about to assemble.  We need room for (NFIELD - 1)
C     delimiters as well as for the numeric fields.
C
      NEEDED  =  NFIELD  -  1  +  SUMAI ( CMPWID, NFIELD )
 
      IF (  LEN( CLKSTR )  .LT.  NEEDED  ) THEN
 
         CALL SETMSG ( 'Output argument has declared length #; '      //
     .                 'required length is #. Input tick value was #.' )
         CALL ERRINT ( '#',  LEN(CLKSTR)                               )
         CALL ERRINT ( '#',  NEEDED                                    )
         CALL ERRDP  ( '#',  TICKS                                     )
         CALL SIGERR ( 'SPICE(SCLKTRUNCATED)'                          )
         CALL CHKOUT ( 'SCFM01'                                        )
         RETURN
 
      END IF
 
 
C
C     Need to check that TICKS is a positive number.
C
      IF (  ANINT( TICKS ) .LT. 0  ) THEN
 
         CALL SETMSG ( 'Negative value for SCLK ticks: #' )
         CALL ERRDP  ( '#', TICKS                         )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'           )
         CALL CHKOUT ( 'SCFM01'                           )
         RETURN
 
      END IF
 
C
C     Determine the value of each of the components. This is done by
C     successively dividing by the number of ticks each component value
C     is worth.
C
      REM = ANINT ( TICKS )
 
      DO I = 1, NFIELD - 1
 
         CMPVAL(I)   =  AINT ( REM / CMPTKS(I) ) + OFFSET(I)
         REM         =  MOD  ( REM,  CMPTKS(I) )
 
      END DO
 
      CMPVAL(NFIELD) =  REM + OFFSET(NFIELD)
 
C
C     Convert the values of each component from double precision
C     numbers to character strings.
C
      DO  I = 1, NFIELD
 
         CALL DPSTRF ( CMPVAL(I), DPLEN, 'F', DPCHAR )
 
         END        =  INDEX ( DPCHAR, '.' ) - 1
         LENGTH(I)  =  END - 1
         CMP(I)     =  DPCHAR(2:END)
 
      END DO
 
C
C     Pad on the left with zeros if necessary.
C
      DO  I = 1, NFIELD
 
         PAD = CMPWID(I) - LENGTH(I)
 
         IF ( PAD .GT. 0 )  THEN
 
            DO  J = 1, PAD
               CALL PREFIX ( '0', 0, CMP(I) )
            END DO
 
         END IF
 
      END DO
 
C
C     Construct the clock string with a delimiter separating
C     each field.
C
      CLKSTR = CMP(1)
 
      DO  I = 2, NFIELD
 
         IF ( DEL(DELCDE) .NE. ' ' ) THEN

            CALL PREFIX ( DEL(DELCDE), 0, CMP(I) )
            CALL SUFFIX ( CMP(I),      0, CLKSTR )
         ELSE
            CALL SUFFIX ( CMP(I),      1, CLKSTR )
         END IF
 
      END DO
 
      CALL CHKOUT ( 'SCFM01' )
      RETURN
 
 
 
 
 
C$Procedure      SCTE01 ( Ticks to ET, type 01 )
 
      ENTRY SCTE01 ( SC, SCLKDP, ET )
 
C$ Abstract
C
C     Convert encoded type 1 spacecraft clock (`ticks') to ephemeris
C     seconds past J2000 (ET).
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
C     TIME
C
C$ Keywords
C
C     CONVERSION
C     TIME
C
C$ Declarations
C
C     INTEGER               SC
C     DOUBLE PRECISION      SCLKDP
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C     SCLKDP     I   Type 1 SCLK, encoded as ticks since clock start.
C     ET         I   Ephemeris time, seconds past J2000.
C
C$ Detailed_Input
C
C     SC             is a NAIF ID code for a spacecraft, one of whose
C                    clock values is represented by SCLKDP.
C
C     SCLKDP         is an encoded type 1 spacecraft clock value
C                    produced by the routine SCENCD.  SCLKDP is a
C                    count of ticks since spacecraft clock start:
C                    partition information IS included in the encoded
C                    value.
C
C$ Detailed_Output
C
C     ET             is the ephemeris time, seconds past J2000, that
C                    corresponds to SCLKDP.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine assumes that that an SCLK kernel appropriate
C         to the spacecraft clock identified by the input argument SC
C         has been loaded.  If an SCLK kernel has not been loaded,
C         does not contain all of the required data, or contains
C         invalid data, error diagnoses will be performed by routines
C         called by this routine.  The output argument ET will not
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
C         The output argument ET will not be modified.
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
C     3)  If the input SCLK value SCLKDP is out of range, this routine
C         will signal the error SPICE(VALUEOUTOFRANGE).  The output
C         argument ET will not be modified.
C
C
C     4)  If the partition times or SCLK coefficients themselves
C         are invalid, this routine will almost certainly give
C         incorrect results.  This routine cannot diagnose errors
C         in the partition times or SCLK coefficients, except possibly
C         by crashing.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     SCTE01 is not usually called by routines external to SPICELIB.
C     The conversion routine SCT2E converts any type of encoded
C     spacecraft clock value produced by SCENCD to ephemeris seconds
C     past J2000.  SCT2E is the preferred user interface routine
C     because its interface specification does not refer to spacecraft
C     clock types.  However, direct use of SCTE01 by user routines is
C     not prohibited.
C
C$ Examples
C
C     1)  Convert an encoded type 1 SCLK value to ET:
C
C         During program initialization, load the leapseconds and SCLK
C         kernels.  We will assume that these files are named
C         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the
C         actual names of these files in your code.
C
C            CALL CLPOOL
C            CALL FURNSH ( 'LEAPSECONDS.KER' )
C            CALL FURNSH ( 'SCLK.KER'        )
C
C         If SCLKDP is an encoded spacecraft clock value, if SC
C         is the NAIF integer code for the spacecraft whose
C         SCLK <--> ET mapping is defined by the data in SCLK.KER,
C         then the call
C
C            CALL SCTE01 ( SC, SCLKDP, ET )
C
C         will return the ET value corresponding to SCLKDP.
C
C         For example, if SC is -77, indicating the Galileo spacecraft,
C         and if a Galileo SCLK kernel is loaded, then if SCLKDP
C         is set to
C
C            7.2800000000000E+05
C
C         the call
C
C            CALL SCTE01 ( SC, SCLKDP, ET )
C
C         returns ET as
C
C            -3.2286984854565E+08
C
C         on a VAX 11/780 running VMS 5.3, Fortran 5.5.
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
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.3.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 3.2.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 3.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-    SPICELIB Version 3.0.0, 06-JAN-1998 (NJB)
C
C        Removed local variable RNDCLK; this entry point no longer
C        creates a rounded version of its input argument.  Use of 
C        ANINT to round coefficients has been discontinued.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        This routine was updated to handle SCLK kernels that use
C        TDT as their `parallel' time system.  Header was updated,
C        particularly $Exceptions and $Restrictions.  Watch is now
C        set on required kernel variables.  Comment section for
C        permuted index source lines was added following the header.
C
C-    SPICELIB Version 1.0.0, 21-AUG-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     type_1 ticks to ephemeris time
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-    SPICELIB Version 3.0.0, 06-JAN-1998 (NJB)
C
C        Removed local variable RNDCLK; this entry point no longer
C        creates a rounded version of its input argument.  Use of 
C        ANINT to round coefficients has been discontinued.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
C
C        This routine was updated to handle a time system specification
C        for the `parallel' time system used in the SCLK kernel.
C
C        Specific changes include:
C
C           -- The time system code is looked up along with the
C              other SCLK specification parameters.
C
C           -- The time value arrived at by interpolation of the
C              SCLK-to-parallel time mapping is converted to TDB
C              if the parallel time system is TDT.
C
C        The header was expanded to discuss the fact that a leapseconds
C        kernel will now need to be loaded in order to use SCLK kernels
C        that map between SCLK and a parallel time system other than
C        TDB.  The $Exceptions and $Restrictions sections were affected.
C
C        This routine now uses the new kernel pool watch capability
C        to determine when it is necessary to look up SCLK variables.
C        This method of checking for kernel pool updates replaces the
C        previously used once-per-call lookup of the SCLK_KERNEL_ID
C        kernel variable.
C
C        A comment section for permuted index source lines was added
C        following the header.
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCTE01' )
      END IF
 
C
C     On the first pass through the subroutine, or if the spacecraft
C     clock ID changes, we will set watches on the SCLK kernel
C     variables for the current clock.
C
      IF (  FIRST  .OR.  ( SC .NE. OLDSC )  ) THEN
 
         FIRST = .FALSE.
 
C
C        Make up a list of names of kernel variables that we'll use.
C        The first name in the list is SCLK_KERNEL_ID, which does not
C        require the addition of a spacecraft code suffix.  For the
C        rest of the names, we'll have to add the suffix.
C
         CALL MOVEC  ( NAMLST,  NKITEM,  KVNAME )
 
         DO I = COFIDX, SYSIDX
            CALL SUFFIX ( '_#',        0,         KVNAME(I) )
            CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
         END DO
 
C
C        Set a watch on all of the kernel variables we use.
C
         CALL SWPOOL ( 'SC01',  NKITEM,  KVNAME )
 
C
C        Keep track of the last spacecraft clock ID encountered.
C
         OLDSC =  SC
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

      END IF
 
C
C     Find out whether we need to look up new format descriptors from
C     the kernel pool.  If any relevant kernel variables were updated,
C     we have to do a look-up.  Note that changing the s/c clock ID
C     causes a new watch to be set, so a look-up is required.  When we
C     do a look-up, we grab everything that any of the SC01 entry
C     points might need.
C
      CALL ZZCVPOOL ( 'SC01', USRCTR, UPDATE )
 
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Our first piece of business is to look up all of the data
C        we require from the kernel pool.  We must form the names
C        of the items we want using the input S/C ID code.  The items
C        we need are:
C
C           -  The number of fields in an (unabridged) SCLK string
C           -  The output delimiter code
C           -  The parallel time system code
C           -  The moduli of the fields of an SCLK string
C           -  The offsets for each clock field.
C           -  The SCLK coefficients array
C           -  The partition start times
C           -  The partition end times
C
 
         CALL SCLI01 ( NAMLST(NFLIDX), SC, 1,        N,      NFIELD )
         CALL SCLI01 ( NAMLST(DELIDX), SC, 1,        N,      DELCDE )
         CALL SCLI01 ( NAMLST(SYSIDX), SC, 1,        NTSYS,  TIMSYS )
 
         CALL SCLD01 ( NAMLST(COFIDX), SC, 3*MXCOEF, NCOEFF, COEFFS )
         CALL SCLD01 ( NAMLST(PRSIDX), SC, MXPART,   N,      PRSTRT )
         CALL SCLD01 ( NAMLST(PREIDX), SC, MXPART,   NPART,  PREND  )
         CALL SCLD01 ( NAMLST(MODIDX), SC, MXNFLD,   N,      MODULI )
         CALL SCLD01 ( NAMLST(OFFIDX), SC, MXNFLD,   N,      OFFSET )
 
C
C        Don't try to continue if we had a lookup error.
C
         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT( 'SCTE01' )
            RETURN
         END IF

C
C        The kernel pool look-up succeeded.
C
         NODATA = .FALSE.
 
C
C        Use the default time system (TDB) if none was specified in the
C        SCLK kernel.
C
         IF ( NTSYS .EQ. 0 ) THEN
            TIMSYS = TDB
         END IF
 
      END IF
 
C
C     To check whether SCLKDP is in range, we must find the end time
C     of the last partition, in total ticks since spacecraft clock
C     start.
C
      MXTICK = 0.D0
 
      DO I = 1, NPART
         MXTICK  =  ANINT (  PREND(I) - PRSTRT(I) + MXTICK  )
      END DO
 
 
C
C     We now check that SCLKDP is in range.  COEFFS(1,1) and
C     MXTICK are, respectively, the first and last absolute
C     tick values of the clock.
C
      IF (      ( SCLKDP .LT. COEFFS(1,1)    )
     .     .OR. ( SCLKDP .GT. MXTICK         )   ) THEN
 
            CALL SETMSG (  BVLMSG                  )
            CALL ERRCH  ( '#', 'SCLKDP'            )
            CALL ERRDP  ( '#',  SCLKDP             )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
            CALL CHKOUT ( 'SCTE01'                 )
            RETURN
 
      END IF
 
C
C     Ok, if we made it this far, we can actually interpret the tick
C     value.  But by this time, we're not in very good mood.
C
 
C
C     Find the tick value in COEFFS closest to the rounded input tick
C     value.  The tick values in COEFFS are monotone increasing, so we
C     can do a binary search to find index of the greatest tick value
C     in the coefficient array that is less than or equal to SCLKDP.
C
C     There are two cases:
C
C        1) SCLKDP is bounded by the least and greatest SCLK
C           coefficients in the array.  In this case, we must search
C           the array for a consecutive pair of records whose SCLK
C           values bound SCLKDP.
C
C        2) SCLKDP is greater than or equal to all of the SCLK
C           coefficients.  In that case, we don't need to search:  the
C           last SCLK value in the array is the one we want.
C
 
      IF (  SCLKDP .LT.  COEFFS(1, NCOEFF/3) ) THEN
 
         LOWER  =  1
         UPPER  =  NCOEFF / 3
 
C
C        In the following loop, we maintain an invariant:
C
C           COEFFS( 1, LOWER )  <  SCLKDP  <  COEFFS( 1, UPPER )
C                               -
C
C        At each step, we decrease the distance between LOWER and
C        UPPER, while keeping the above statement true.  The loop
C        terminates when LOWER = UPPER - 1.
C
C        Note that we start out with if LOWER < UPPER, since we've
C        already made sure that the invariant expression above is true.
C
         DO WHILE ( LOWER .LT. UPPER - 1 )
 
            MIDDLE  =  ( LOWER + UPPER ) / 2
 
            IF (  SCLKDP  .LT. COEFFS(1,MIDDLE) ) THEN
               UPPER = MIDDLE
            ELSE
               LOWER = MIDDLE
            END IF
 
         END DO
 
C
C        We've got SCLKDP trapped between two tick values that are
C        `adjacent' in the list:
C
C           COEFFS ( 1, LOWER )  and
C           COEFFS ( 1, UPPER )
C
C        since the second value must be greater than the first.  So
C
C           COEFFS( 1, LOWER )
C
C        is the last tick value in the coefficients array less than or
C        equal to SCLKDP.
C
 
      ELSE
C
C        SCLKDP is greater than or equal to all of the SCLK
C        coefficients in the coefficients array.
C
         LOWER = NCOEFF / 3
 
      END IF
 
C
C     Now we evaluate a linear polynomial to find the time value that
C     corresponds to SCLKDP.  The coefficients of the polynomial are
C     the time and rate (in units of seconds per tick) that correspond
C     to the tick value
C
C        COEFFS( 1, LOWER )
C
C     We call these coefficients CONST and RATE.  The rates in the
C     coefficients array are in units of seconds per most significant
C     SCLK count, so we use the conversion factor TIKMSC to change the
C     rate to seconds per tick.
C
      TIKMSC = 1.0D0
 
      DO I = NFIELD, 2, -1
         TIKMSC  =  TIKMSC * MODULI(I)
      END DO
 
      TIKDIF =  SCLKDP  -  COEFFS( 1, LOWER )
      CONST  =             COEFFS( 2, LOWER )
      RATE   =             COEFFS( 3, LOWER )  /  TIKMSC
 
      PARTIM =  CONST  +  ( RATE * TIKDIF )
 
C
C     Convert the parallel time to TDB, if the system is not TDB.
C     We don't need to check the validity of TIMSYS, because SCLI01
C     already made this check.
C
      IF ( TIMSYS .EQ. TDB ) THEN
 
         ET = PARTIM
 
      ELSE IF ( TIMSYS .EQ. TDT ) THEN
 
         ET = UNITIM ( PARTIM, 'TDT', 'TDB' )
 
      END IF
 
 
      CALL CHKOUT ( 'SCTE01' )
      RETURN
 
 
 
 
 
C$Procedure      SCET01 ( ET to discrete ticks, type 1 )
 
      ENTRY SCET01 ( SC, ET, SCLKDP )
 
C$ Abstract
C
C     Convert ephemeris seconds past J2000 (ET) to discrete encoded 
C     type 1 spacecraft clock (`ticks').
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
C     TIME
C
C$ Keywords
C
C     CONVERSION
C     TIME
C
C$ Declarations
C
C     INTEGER               SC
C     DOUBLE PRECISION      ET
C     DOUBLE PRECISION      SCLKDP
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C     ET         I   Ephemeris time, seconds past J2000.
C     SCLKDP     O   Type 1 SCLK, encoded as ticks since clock start.
C
C$ Detailed_Input
C
C     SC             is a NAIF ID code for a spacecraft, one of whose
C                    clock values is represented by SCLKDP.
C
C     ET             is an ephemeris time, specified in seconds past
C                    J2000, whose equivalent encoded SCLK value is
C                    desired.
C
C$ Detailed_Output
C
C     SCLKDP         is the encoded type 1 spacecraft clock value
C                    that corresponds to ET.  The value is obtained
C                    by mapping ET, using the piecewise linear mapping
C                    defined by the SCLK kernel, to a value that may
C                    have a non-zero fractional part, and then
C                    rounding this value to the nearest double precision
C                    whole number.
C
C                    SCLKDP represents total time since spacecraft
C                    clock start and hence does reflect partition
C                    information.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine assumes that that an SCLK kernel appropriate
C         to the spacecraft clock identified by the input argument SC
C         has been loaded.  If an SCLK kernel has not been loaded,
C         does not contain all of the required data, or contains
C         invalid data, error diagnoses will be performed by routines
C         called by this routine.  The output argument SCLKDP will not
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
C         The output argument SCLKDP will not be modified.
C
C         The time system that an SCLK kernel maps SCLK to is indicated
C         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
C         is the negative of the NAIF integer code for the spacecraft.
C         The time system used in a kernel is TDB if and only if the
C         variable is assigned the value 1.
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
C     3)  If the input ephemeris time value ET is out of range, this
C         routine will signal the error SPICE(VALUEOUTOFRANGE).
C         The output argument SCLKDP will not be modified.
C
C     4)  If the SCLK rate used to interpolate SCLK values is zero, the
C         error SPICE(VALUEOUTOFRANGE) is signalled.  The output
C         argument SCLKDP will not be modified.
C
C     5)  If the partition times or SCLK coefficients themselves
C         are invalid, this routine will almost certainly give
C         incorrect results.  This routine cannot diagnose errors
C         in the partition times or SCLK coefficients, except possibly
C         by crashing.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Normally, the newer entry point SCEC01 (ET to continuous ticks,
C     type 1) should be used in place of this routine.
C
C     SCET01 is not usually called by routines external to SPICELIB.
C     The conversion routine SCE2T converts ephemeris seconds past J2000
C     to any type of discrete, encoded type 1 spacecraft clock value. 
C     For conversion to continuous, encoded SCLK, SCE2C is the preferred
C     user interface routine because its interface specification does
C     not refer to spacecraft clock types. For conversion to discrete, 
C     encoded SCLK, SCE2T is the preferred interface routine.
C
C     However, direct use of SCET01 by user routines is not prohibited.
C
C$ Examples
C
C     1)  Converting ET to encoded type 1 SCLK:
C
C         During program initialization, load the leapseconds and SCLK
C         kernels.  We will assume that these files are named
C         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the
C         actual names of these files in your code.
C
C            CALL CLPOOL
C            CALL FURNSH ( 'LEAPSECONDS.KER' )
C            CALL FURNSH ( 'SCLK.KER'        )
C
C         If SC is -77, indicating the Galileo spacecraft, and
C         ET is set to
C
C            -3.2286984854565E+08
C
C         then the call
C
C            CALL SCET01 ( SC, ET, SCLKDP )
C
C         returns SCLKDP as
C
C            7.2800000000000E+05
C
C         on a VAX 11/780 running VMS 5.3, Fortran 5.5.  Note that
C         the result should be the same (except for the output format)
C         on most computers, since the result is a double precision
C         whole number.
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
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-    SPICELIB Version 2.0.3, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 2.0.2, 09-MAR-1999 (NJB)
C
C        Comments were updated; references to SCE2C and SCEC01 were
C        added.
C
C-    SPICELIB Version 2.0.1, 18-JUL-1996 (NJB)
C
C        Typo in comment fixed.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        This routine was updated to handle SCLK kernels that use
C        TDT as their `parallel' time system.  Header was updated,
C        particularly $Exceptions and $Restrictions.  Watch is now
C        set on required kernel variables.  Comment section for
C        permuted index source lines was added following the header.
C
C-    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     ephemeris time to type_1 ticks
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
C
C        This routine was updated to handle a time system specification
C        for the `parallel' time system used in the SCLK kernel.
C
C        Specific changes include:
C
C           -- The time system code is looked up along with the
C              other SCLK specification parameters.
C
C           -- The input TDB value is converted, if necessary, to the
C              time system used in the parallel-time-to-SCLK mapping
C              defined by the current SCLK coefficients for the
C              specified spacecraft clock.  This conversion is performed
C              prior to determination by interpolation of the
C              corresponding encoded SCLK value.
C
C        The header was expanded to discuss the fact that a leapseconds
C        kernel will now need to be loaded in order to use SCLK kernels
C        that map between SCLK and a parallel time system other than
C        TDB.  The $Exceptions and $Restrictions sections were affected.
C
C        This routine now uses the new kernel pool watch capability
C        to determine when it is necessary to look up SCLK variables.
C        This method of checking for kernel pool updates replaces the
C        previously used once-per-call lookup of the SCLK_KERNEL_ID
C        kernel variable.
C
C        A comment section for permuted index source lines was added
C        following the header.
C-&
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCET01' )
      END IF
 
C
C     On the first pass through the subroutine, or if the spacecraft
C     clock ID changes, we will set watches on the SCLK kernel
C     variables for the current clock.
C
      IF (  FIRST  .OR.  ( SC .NE. OLDSC )  ) THEN
 
         FIRST = .FALSE.
 
C
C        Make up a list of names of kernel variables that we'll use.
C        The first name in the list is SCLK_KERNEL_ID, which does not
C        require the addition of a spacecraft code suffix.  For the
C        rest of the names, we'll have to add the suffix.
C
         CALL MOVEC  ( NAMLST,  NKITEM,  KVNAME )
 
         DO I = COFIDX, SYSIDX
            CALL SUFFIX ( '_#',        0,         KVNAME(I) )
            CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
         END DO
 
C
C        Set a watch on all of the kernel variables we use.
C
         CALL SWPOOL ( 'SC01',  NKITEM,  KVNAME )
 
C
C        Keep track of the last spacecraft clock ID encountered.
C
         OLDSC =  SC
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

      END IF
 
C
C     Find out whether we need to look up new format descriptors from
C     the kernel pool.  If any relevant kernel variables were updated,
C     we have to do a look-up.  Note that changing the s/c clock ID
C     causes a new watch to be set, so a look-up is required.  When we
C     do a look-up, we grab everything that any of the SC01 entry
C     points might need.
C
      CALL ZZCVPOOL ( 'SC01', USRCTR, UPDATE )
 
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Our first piece of business is to look up all of the data
C        we require from the kernel pool.  We must form the names
C        of the items we want using the input S/C ID code.  The items
C        we need are:
C
C           -  The number of fields in an (unabridged) SCLK string
C           -  The output delimiter code
C           -  The parallel time system code
C           -  The moduli of the fields of an SCLK string
C           -  The offsets for each clock field.
C           -  The SCLK coefficients array
C           -  The partition start times
C           -  The partition end times
C
 
         CALL SCLI01 ( NAMLST(NFLIDX), SC, 1,        N,      NFIELD )
         CALL SCLI01 ( NAMLST(DELIDX), SC, 1,        N,      DELCDE )
         CALL SCLI01 ( NAMLST(SYSIDX), SC, 1,        NTSYS,  TIMSYS )
 
         CALL SCLD01 ( NAMLST(COFIDX), SC, 3*MXCOEF, NCOEFF, COEFFS )
         CALL SCLD01 ( NAMLST(PRSIDX), SC, MXPART,   N,      PRSTRT )
         CALL SCLD01 ( NAMLST(PREIDX), SC, MXPART,   NPART,  PREND  )
         CALL SCLD01 ( NAMLST(MODIDX), SC, MXNFLD,   N,      MODULI )
         CALL SCLD01 ( NAMLST(OFFIDX), SC, MXNFLD,   N,      OFFSET )
 
C
C        Don't try to continue if we had a lookup error.
C
         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT( 'SCET01' )
            RETURN
         END IF

C
C        The kernel pool look-up succeeded.
C
         NODATA = .FALSE.
 
C
C        Use the default time system (TDB) if none was specified in the
C        SCLK kernel.
C
         IF ( NTSYS .EQ. 0 ) THEN
            TIMSYS = TDB
         END IF
 
      END IF
 
C
C     Convert the input TDB time to the parallel time system, if the
C     parallel system is not TDB.
C
C     We don't need to check the validity of TIMSYS, because SCLI01
C     already made this check.
C
      IF ( TIMSYS .EQ. TDB ) THEN
 
         PARTIM = ET
 
      ELSE IF ( TIMSYS .EQ. TDT ) THEN
 
         PARTIM = UNITIM ( ET, 'TDB', 'TDT' )
 
      END IF
 
 
C
C     We'd like to ascertain whether PARTIM is between the minimum
C     time value in the coefficients array and the end time
C     corresponding to the number of ticks since spacecraft clock
C     start at the end of the last partition.
C
C     Checking the time value is a special case; we'll convert the time
C     value to ticks, and then check whether the resulting value is
C     less than the total number of ticks since spacecraft clock start
C     at the end of the last partition.  So, this check is performed
C     at the end of the routine.
C
C     Find the time value in COEFFS closest to the input time value.
C     The time values are ordered, so we can do a binary search for the
C     closest one.  When the search is done, we will have found the
C     index of the greatest time value in the coefficient array that
C     is less than or equal to PARTIM.
C
C
C     There are three cases:
C
C        1) PARTIM is less than the least time coefficient in the array.
C           In this case, we'll use the first coefficient set in the
C           kernel to extrapolate from.  We don't automatically treat
C           this case as an error because PARTIM could round up to the
C           minimum tick value when converted to ticks.
C
C        2) PARTIM is bounded by the least and greatest time
C           coefficients in the array.  In this case, we must search
C           the array for a consecutive pair of records whose time
C           values bound PARTIM.
C
C        3) PARTIM is greater than or equal to all of the time
C           coefficients.  In that case, we don't need to search:  the
C           last time value in the array is the one we want.
C
C
      IF (  PARTIM  .LT.  COEFFS( 2, 1 )  ) THEN
C
C        The coefficient set to use for extrapolation is the first.
C
         LOWER = 1
 
 
      ELSE IF (  PARTIM .LT. COEFFS( 2, NCOEFF/3 )  ) THEN
C
C        In the following loop, we maintain an invariant:
C
C           COEFFS( 2, LOWER )  <   PARTIM   <   COEFFS( 2, UPPER )
C                               -
C
C        At each step, we decrease the distance between LOWER and
C        UPPER, while keeping the above statement true.  The loop
C        terminates when LOWER = UPPER - 1.
C
C        Note that we start out with if LOWER < UPPER, since we've
C        already made sure that the invariant expression above is true.
C
 
         LOWER  =  1
         UPPER  =  NCOEFF / 3
 
         DO WHILE ( LOWER .LT. UPPER - 1 )
 
            MIDDLE  =  ( LOWER + UPPER ) / 2
 
            IF (  PARTIM  .LT.  COEFFS( 2, MIDDLE )  ) THEN
               UPPER = MIDDLE
            ELSE
               LOWER = MIDDLE
            END IF
 
         END DO
 
C
C        We've got PARTIM trapped between two time values that are
C        `adjacent' in the list:
C
C           COEFFS ( 2, LOWER )  and
C           COEFFS ( 2, UPPER )
C
C        since the second value must be greater than the first.  So
C
C           COEFFS( 2, LOWER )
C
C        is the last time value in the coefficients array less than or
C        equal to PARTIM.
C
 
      ELSE
C
C        PARTIM is greater than or equal to all of the time values in
C        the coefficients array.
C
         LOWER = NCOEFF / 3
 
      END IF
 
C
C     Now we evaluate a linear polynomial to find the tick value that
C     corresponds to PARTIM.  The coefficients of the polynomial are
C     the tick value and rate (in units of ticks per second) that
C     correspond to the time value
C
C        COEFFS( 2, LOWER )
C
C     We call these coefficients CONST and RATE.  The rates in the
C     coefficients array are in units of seconds per most significant
C     clock count, so we use the conversion factor TIKMSC (`ticks per
C     most significant count') to change the rate to seconds per tick.
C
C     One other thing:  SCLKDP should be an integral number of ticks.
C     We use the generic `nearest whole number' function ANINT to
C     ensure this.
C
      TIMDIF =  PARTIM  -  COEFFS( 2, LOWER )
      CONST  =             COEFFS( 1, LOWER )
 
      IF (  COEFFS( 3, LOWER ) .LE. 0.D0  ) THEN
 
         CALL SETMSG ( 'Invalid SCLK rate.'     )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'SCET01'                 )
         RETURN
 
      END IF
 
 
      TIKMSC = 1.0D0
 
      DO I = NFIELD, 2, -1
         TIKMSC  =  TIKMSC * MODULI(I)
      END DO
 
      RATE   =  1.D0   /   (  COEFFS(3, LOWER) / TIKMSC  )
 
      SCLKDP =  ANINT (  CONST   +   ( RATE * TIMDIF )  )
 
C
C     Now, we'll see whether the SCLK value we've found is meaningful.
C     If it's too large, that's because the input PARTIM was beyond the
C     maximum value we can handle.  To check whether PARTIM is in
C     range, we must find the end time of the last partition, in total
C     ticks since spacecraft clock start.
C
      MXTICK  =  ANINT ( PREND(1) - PRSTRT(1) )
 
      DO I = 2, NPART
         MXTICK  =  ANINT ( PREND(I) -  PRSTRT(I)  +  MXTICK )
      END DO
 
C
C     Make sure that ET does not precede the ET corresponding to
C     the clock's minimum tick value or exceed the ET corresponding to
C     the clock's maximum tick value.  We'll do the comparison
C     using the tick value that ET mapped to and the minimum and
C     maximum tick values of the spacecraft clock.
C
C     Convert SCLKDP and COEFFS(1,1) to whole numbers, so that
C     direct comparisons without tolerances are possible.
C
      SCLKDP      = ANINT ( SCLKDP      )
      COEFFS(1,1) = ANINT ( COEFFS(1,1) )
 
      IF (      ( SCLKDP .LT. COEFFS(1,1)  )
     .     .OR. ( SCLKDP .GT. MXTICK       )   ) THEN
 
         CALL SETMSG (  BVLMSG                  )
         CALL ERRCH  ( '#', 'ET'                )
         CALL ERRDP  ( '#',  ET                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'SCET01'                 )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'SCET01' )
      RETURN
 
 
 
 
C$Procedure      SCEC01 ( ET to continuous ticks, type 1 )
 
      ENTRY SCEC01 ( SC, ET, SCLKDP )
 
C$ Abstract
C
C     Convert ephemeris seconds past J2000 (ET) to continuous encoded
C     type 1 spacecraft clock (`ticks').  The output value need not be
C     integral.
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
C     TIME
C
C$ Keywords
C
C     CONVERSION
C     TIME
C
C$ Declarations
C
C     INTEGER               SC
C     DOUBLE PRECISION      ET
C     DOUBLE PRECISION      SCLKDP
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C     ET         I   Ephemeris time, seconds past J2000.
C     SCLKDP     O   Type 1 SCLK, encoded as continuous ticks since
C                    clock start.
C
C$ Detailed_Input
C
C     SC             is a NAIF ID code for a spacecraft, one of whose
C                    clock values is represented by SCLKDP.
C
C     ET             is an ephemeris time, specified in seconds past
C                    J2000, whose equivalent encoded SCLK value is
C                    desired.
C
C$ Detailed_Output
C
C     SCLKDP         is the continuous encoded type 1 spacecraft clock
C                    value corresponding to ET.  The value is obtained
C                    by mapping ET, using the piecewise linear mapping
C                    defined by the SCLK kernel, to a value that may
C                    have a non-zero fractional part.  Unlike the output
C                    of SCET01, SCLKDP is not rounded by this routine.
C
C                    SCLKDP represents total time since spacecraft
C                    clock start and hence does reflect partition
C                    information.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine assumes that that an SCLK kernel appropriate
C         to the spacecraft clock identified by the input argument SC
C         has been loaded.  If an SCLK kernel has not been loaded,
C         does not contain all of the required data, or contains
C         invalid data, error diagnoses will be performed by routines
C         called by this routine.  The output argument SCLKDP will not
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
C         The output argument SCLKDP will not be modified.
C
C         The time system that an SCLK kernel maps SCLK to is indicated
C         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
C         is the negative of the NAIF integer code for the spacecraft.
C         The time system used in a kernel is TDB if and only if the
C         variable is assigned the value 1.
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
C     3)  If the input ephemeris time value ET is out of range, this
C         routine will signal the error SPICE(VALUEOUTOFRANGE).
C         The output argument SCLKDP will not be modified.
C
C     4)  If the SCLK rate used to interpolate SCLK values is zero, the
C         error SPICE(VALUEOUTOFRANGE) is signalled.  The output
C         argument SCLKDP will not be modified.
C
C     5)  If the partition times or SCLK coefficients themselves
C         are invalid, this routine will almost certainly give
C         incorrect results.  This routine cannot diagnose errors
C         in the partition times or SCLK coefficients, except possibly
C         by crashing.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     SCEC01 is not usually called by routines external to SPICELIB.
C     The conversion routine SCE2C converts ephemeris seconds
C     past J2000 to any type of encoded spacecraft clock value.
C     SCE2C is the preferred user interface routine because its
C     interface specification does not refer to spacecraft clock types.
C     However, direct use of SCEC01 by user routines is not prohibited.
C
C$ Examples
C
C     1)  Converting ET to encoded type 1 SCLK:
C
C         During program initialization, load the leapseconds and SCLK
C         kernels.  We will assume that these files are named
C         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the
C         actual names of these files in your code.
C
C            CALL CLPOOL
C            CALL FURNSH ( 'LEAPSECONDS.KER' )
C            CALL FURNSH ( 'SCLK.KER'        )
C
C         If SC is -77, indicating the Galileo spacecraft, and
C         ET is set to
C
C            -27848635.8149248
C
C         then the call
C
C            CALL SCEC01 ( SC, ET, SCLKDP )
C
C         returns SCLKDP as
C
C            35425287435.8554
C
C         on a NeXT workstation running NEXTSTEP 3.3.
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
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.4.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 1.3.0, 05-MAR-2009 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 1.2.0, 09-NOV-2007 (NJB)
C
C        Bug fix: this routine now keeps track of whether its
C        kernel pool look-up succeeded. If not, a kernel pool
C        lookup is attempted on the next call to this routine.
C
C-    SPICELIB Version 1.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-    SPICELIB Version 1.0.0, 13-FEB-1999 (NJB)
C
C-&
 
C$ Index_Entries
C
C     ephemeris time to continuous type_1 ticks
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 09-NOV-2007 (NJB)
C
C        Bug fix: changed maximum value arguments to 1 in 
C        calls to SCLI01 to fetch NFIELD and DELCDE values.
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCEC01' )
      END IF
 
C
C     On the first pass through the subroutine, or if the spacecraft
C     clock ID changes, we will set watches on the SCLK kernel
C     variables for the current clock.
C
      IF (  FIRST  .OR.  ( SC .NE. OLDSC )  ) THEN
 
         FIRST = .FALSE.
 
C
C        Make up a list of names of kernel variables that we'll use.
C        The first name in the list is SCLK_KERNEL_ID, which does not
C        require the addition of a spacecraft code suffix.  For the
C        rest of the names, we'll have to add the suffix.
C
         CALL MOVEC  ( NAMLST,  NKITEM,  KVNAME )
 
         DO I = COFIDX, SYSIDX
            CALL SUFFIX ( '_#',        0,         KVNAME(I) )
            CALL REPMI  ( KVNAME(I),  '#',  -SC,  KVNAME(I) )
         END DO
 
C
C        Set a watch on all of the kernel variables we use.
C
         CALL SWPOOL ( 'SC01',  NKITEM,  KVNAME )
 
C
C        Keep track of the last spacecraft clock ID encountered.
C
         OLDSC =  SC
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

      END IF
 
C
C     Find out whether we need to look up new format descriptors from
C     the kernel pool.  If any relevant kernel variables were updated,
C     we have to do a look-up.  Note that changing the s/c clock ID
C     causes a new watch to be set, so a look-up is required.  When we
C     do a look-up, we grab everything that any of the SC01 entry
C     points might need.
C
      CALL ZZCVPOOL ( 'SC01', USRCTR, UPDATE )
 
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Our first piece of business is to look up all of the data
C        we require from the kernel pool.  We must form the names
C        of the items we want using the input S/C ID code.  The items
C        we need are:
C
C           -  The number of fields in an (unabridged) SCLK string
C           -  The output delimiter code
C           -  The parallel time system code
C           -  The moduli of the fields of an SCLK string
C           -  The offsets for each clock field.
C           -  The SCLK coefficients array
C           -  The partition start times
C           -  The partition end times
C
 
         CALL SCLI01 ( NAMLST(NFLIDX), SC, 1,        N,      NFIELD )
         CALL SCLI01 ( NAMLST(DELIDX), SC, 1,        N,      DELCDE )
         CALL SCLI01 ( NAMLST(SYSIDX), SC, 1,        NTSYS,  TIMSYS )
 
         CALL SCLD01 ( NAMLST(COFIDX), SC, 3*MXCOEF, NCOEFF, COEFFS )
         CALL SCLD01 ( NAMLST(PRSIDX), SC, MXPART,   N,      PRSTRT )
         CALL SCLD01 ( NAMLST(PREIDX), SC, MXPART,   NPART,  PREND  )
         CALL SCLD01 ( NAMLST(MODIDX), SC, MXNFLD,   N,      MODULI )
         CALL SCLD01 ( NAMLST(OFFIDX), SC, MXNFLD,   N,      OFFSET )
 
C
C        Don't try to continue if we had a lookup error.
C
         IF ( FAILED() ) THEN

            NODATA = .TRUE.

            CALL CHKOUT( 'SCEC01' )
            RETURN
         END IF
 
C
C        The kernel pool look-up succeeded.
C
         NODATA = .FALSE.

C
C        Use the default time system (TDB) if none was specified in the
C        SCLK kernel.
C
         IF ( NTSYS .EQ. 0 ) THEN
            TIMSYS = TDB
         END IF
 
      END IF
 
C
C     Convert the input TDB time to the parallel time system, if the
C     parallel system is not TDB.
C
C     We don't need to check the validity of TIMSYS, because SCLI01
C     already made this check.
C
      IF ( TIMSYS .EQ. TDB ) THEN
 
         PARTIM = ET
 
      ELSE IF ( TIMSYS .EQ. TDT ) THEN
 
         PARTIM = UNITIM ( ET, 'TDB', 'TDT' )
 
      END IF
 
 
C
C     We'd like to ascertain whether PARTIM is between the minimum
C     time value in the coefficients array and the end time
C     corresponding to the number of ticks since spacecraft clock
C     start at the end of the last partition.
C
C     Checking the time value is a special case; we'll convert the time
C     value to ticks, and then check whether the resulting value is
C     less than the total number of ticks since spacecraft clock start
C     at the end of the last partition.  So, this check is performed
C     at the end of the routine.
C
C     Find the time value in COEFFS closest to the input time value.
C     The time values are ordered, so we can do a binary search for the
C     closest one.  When the search is done, we will have found the
C     index of the greatest time value in the coefficient array that
C     is less than or equal to PARTIM.
C
C
C     There are two cases:
C
C        1) PARTIM is bounded by the least and greatest time
C           coefficients in the array.  In this case, we must search
C           the array for a consecutive pair of records whose time
C           values bound PARTIM.
C
C        2) PARTIM is greater than or equal to all of the time
C           coefficients.  In that case, we don't need to search:  the
C           last time value in the array is the one we want.
C
C
      IF (  PARTIM  .LT.  COEFFS( 2, 1 )  ) THEN
C
C        PARTIM precedes the coverage of the kernel.
C
         CALL SETMSG (  BVLMSG                  )
         CALL ERRCH  ( '#', 'ET'                )
         CALL ERRDP  ( '#',  ET                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'SCEC01'                 )
         RETURN
  
 
      ELSE IF (  PARTIM .LT. COEFFS( 2, NCOEFF/3 )  ) THEN
C
C        In the following loop, we maintain an invariant:
C
C           COEFFS( 2, LOWER )  <   PARTIM   <   COEFFS( 2, UPPER )
C                               -
C
C        At each step, we decrease the distance between LOWER and
C        UPPER, while keeping the above statement true.  The loop
C        terminates when LOWER = UPPER - 1.
C
C        Note that we start out with if LOWER < UPPER, since we've
C        already made sure that the invariant expression above is true.
C
 
         LOWER  =  1
         UPPER  =  NCOEFF / 3
 
         DO WHILE ( LOWER .LT. UPPER - 1 )
 
            MIDDLE  =  ( LOWER + UPPER ) / 2
 
            IF (  PARTIM  .LT.  COEFFS( 2, MIDDLE )  ) THEN
               UPPER = MIDDLE
            ELSE
               LOWER = MIDDLE
            END IF
 
         END DO
 
C
C        We've got PARTIM trapped between two time values that are
C        `adjacent' in the list:
C
C           COEFFS ( 2, LOWER )  and
C           COEFFS ( 2, UPPER )
C
C        since the second value must be greater than the first.  So
C
C           COEFFS( 2, LOWER )
C
C        is the last time value in the coefficients array less than or
C        equal to PARTIM.
C
 
      ELSE
C
C        PARTIM is greater than or equal to all of the time values in
C        the coefficients array.
C
         LOWER = NCOEFF / 3
 
      END IF
 
C
C     Now we evaluate a linear polynomial to find the tick value that
C     corresponds to PARTIM.  The coefficients of the polynomial are
C     the tick value and rate (in units of ticks per second) that
C     correspond to the time value
C
C        COEFFS( 2, LOWER )
C
C     We call these coefficients CONST and RATE.  The rates in the
C     coefficients array are in units of seconds per most significant
C     clock count, so we use the conversion factor TIKMSC (`ticks per
C     most significant count') to change the rate to seconds per tick.
C
      TIMDIF =  PARTIM  -  COEFFS( 2, LOWER )
      CONST  =             COEFFS( 1, LOWER )
 
      IF (  COEFFS( 3, LOWER ) .LE. 0.D0  ) THEN
 
         CALL SETMSG ( 'Invalid SCLK rate.'     )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'SCEC01'                 )
         RETURN
 
      END IF
 
 
      TIKMSC = 1.0D0
 
      DO I = NFIELD, 2, -1
         TIKMSC  =  TIKMSC * MODULI(I)
      END DO
 
      RATE   =  1.D0   /  (  COEFFS(3, LOWER) / TIKMSC  )
 
      SCLKDP =  CONST  +  ( RATE * TIMDIF )
 
C
C     Now, we'll see whether the SCLK value we've found is meaningful.
C     If it's too large, that's because the input PARTIM was beyond the
C     maximum value we can handle.  To check whether PARTIM is in
C     range, we must find the end time of the last partition, in total
C     ticks since spacecraft clock start.
C
      MXTICK  =  ANINT ( PREND(1) - PRSTRT(1) )
 
      DO I = 2, NPART
         MXTICK  =  ANINT ( PREND(I) -  PRSTRT(I)  +  MXTICK )
      END DO
 
C
C     Make sure that ET does not exceed the ET corresponding to
C     the clock's maximum tick value.  We'll do the comparison
C     using the tick value that ET mapped to and the maximum tick 
C     value of the spacecraft clock.
C
      IF ( SCLKDP .GT. MXTICK ) THEN
 
         CALL SETMSG (  BVLMSG                  )
         CALL ERRCH  ( '#', 'ET'                )
         CALL ERRDP  ( '#',  ET                 )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'SCEC01'                 )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'SCEC01' )
      RETURN
 
      END
