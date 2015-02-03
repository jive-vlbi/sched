C$Procedure      SCE2S ( ET to SCLK string )
 
      SUBROUTINE SCE2S ( SC, ET, SCLKCH )
 
C$ Abstract
C
C   Convert an epoch specified as ephemeris seconds past J2000 (ET) to
C   a character string representation of a spacecraft clock value
C   (SCLK).
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
 
      INTEGER               SC
      DOUBLE PRECISION      ET
      CHARACTER*(*)         SCLKCH
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft clock ID code.
C     ET         I   Ephemeris time, specified as seconds past J2000.
C     SCLKCH     O   An SCLK string.
C
C$ Detailed_Input
C
C     SC             is a NAIF ID code for a spacecraft clock whose 
C                    reading at the epoch specified by ET is desired.  
C
C     ET             is an epoch, specified as ephemeris seconds past
C                    J2000.
C
C$ Detailed_Output
C
C     SCLKCH         is a character string representation of the
C                    spacecraft clock value that corresponds to ET, for
C                    the spacecraft clock specified by the input
C                    argument SC. SCLKCH is an absolute spacecraft
C                    clock value, so a partition number is included in
C                    the string. The format of SCLKCH is specified in
C                    the SCLK kernel for the clock SC.  A general
C                    discussion of spacecraft clock string formats is
C                    available in the SCLK Required Reading.
C
C                    In order to choose an appropriate length for
C                    SCLKCH, you can examine an SCLK kernel for the
C                    clock specified by SC.  The format of string
C                    representations of the clock's values is specified
C                    by kernel variables associated with the clock. See
C                    Examples below for further information.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine assumes that an SCLK kernel appropriate to the
C         spacecraft clock identified by the input argument SC has been
C         loaded.  If an SCLK kernel has not been loaded, does not
C         contain all of the required data, or contains invalid data,
C         error diagnoses will be performed by routines called by this
C         routine.  The output argument SCLKCH will not be modified.
C
C     2)  When using an SCLK kernel that maps SCLK to a time system
C         other than ET (also called barycentric dynamical
C         time---`TDB'), it is necessary to have a leapseconds kernel
C         loaded at the time this routine is called.  If a leapseconds
C         kernel is required for conversion between SCLK and ET but is
C         not loaded, the error will be diagnosed by routines called by
C         this routine. The output argument SCLKCH will not be
C         modified.
C
C         The time system to which an SCLK kernel maps SCLK epochs is
C         indicated by the variable SCLK_TIME_SYSTEM_nn in the kernel,
C         where nn is the negative of the NAIF integer code for the
C         spacecraft. The time system used in a kernel is TDB if and
C         only if the variable is assigned the value 1.
C
C     3)  If the input ET value is not representable in the spacecraft
C         clock string format for the spacecraft clock identified by
C         SC, the error will be diagnosed by routines called by this
C         routine.  The output argument SCLKCH will not be modified.
C
C     4)  If the output argument SCLKCH is too short to contain the
C         output spacecraft clock string produced by this routine,
C         the error will be diagnosed by routines called by this
C         routine.  The output argument SCLKCH may contain a portion
C         of the truncated string.
C
C$ Files
C
C     1)  An SCLK kernel appropriate to the spacecraft clock identified
C         by SC must be loaded at the time this routine is called.
C
C     2)  If the SCLK kernel used with this routine does not map SCLK
C         directly to barycentric dynamical time, a leapseconds kernel
C         must be loaded at the time this routine is called.
C
C$ Particulars
C
C     This routine is provided as a convenience; it is simply shorthand
C     for the code fragment
C
C        CALL SCE2T  ( SC,  ET,      SCLKDP )
C        CALL SCDECD ( SC,  SCLKDP,  SCLKCH )
C
C     See the SCLK Required Reading for a list of the entire set of
C     SCLK conversion routines.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms.  The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C
C     1)  Determine the length of Galileo spacecraft clock strings.
C
C         Examine a Galileo SCLK kernel.  There you'll find the
C         kernel variable assignments
C
C            SCLK01_MODULI_77          = ( 16777215 91 10 8 )
C            SCLK01_OFFSETS_77         = (        0  0  0 0 )
C
C         Each field of the clock string contains values ranging
C         from the offset value to M-1, where M is the corresponding
C         modulus.  So the Galileo clock fields have maximum values
C
C            16777214 90 9 7
C
C         representing the partition number by the symbol "pp" and
C         the field delimiter character by the symbol "D", we see
C         that the GLL SCLK format is
C
C            pp/xxxxxxxxDxxDxDx
C
C         This string has length 18 characters.  Accounting for the
C         terminating null character, the value of `lenout' should
C         be set to at least 19.
C
C         Note:  the delimiter character is determined by the integer
C         code assignment
C
C            SCLK01_OUTPUT_DELIM_77    = (                2 )
C
C         The SCLK Required Reading indicates that 2 is the SCLK kernel
C         code for the colon character.
C
C
C     2)  Find the Galileo SCLK value corresponding to the ET
C
C            -322452420.5593641.
C
C
C            C
C            C     Start out by loading the SCLK kernel.  In your own
C            C     program, you must use the name of a real SCLK kernel.
C            C     The name shown here is fictitious.
C            C
C                  CALL FURNSH ( 'GLLSCLK.KER' )
C
C            C
C            C     Load a leapseconds kernel in case it is needed for
C            C     SCLK-to-ET conversion.  Depending on the SCLK kernel
C            C     used, it may not be necessary to load this file; it's
C            C     just a simple, reliable way of making sure that the
C            C     leapseconds kernel constants are available if we need
C            C     them.  Again, a fictitious name is used.
C            C
C                  CALL FURNSH ( 'LEAPSECONDS.KER' )
C
C            C
C            C     The spacecraft ID code for Galileo is -77.
C            C
C                  SC = -77
C                  ET = -322452420.5593641
C
C                  CALL SCE2S ( SC, ET, SCLKCH )
C
C
C         The returned value of SCLKCH will be
C
C            1/00010001:44:2:0.
C
C
C     2)  Convert the UTC time
C
C            August 25 1989 4:00:00
C
C         to a Voyager 2 SCLK value.
C
C         To enable you to perform UTC to ET conversion, your
C         initialization code must load the leapseconds and SCLK
C         kernels:
C
C            C
C            C     Load leapseconds and SCLK kernels:
C            C
C                  CALL FURNSH ( 'LEAPSECONDS.KER' )
C                  CALL FURNSH ( 'VGR2SCLK.KER'    )
C
C
C         To find Voyager 2 SCLK string corresponding to the
C         specified UTC time, you can use the code fragment
C
C                  CALL UTC2ET ( 'Aug 25 1989 4:00:00',  ET     )
C                  CALL SCE2S  ( -32,        ET,         SCLKCH )
C
C         The result of the conversion is
C
C            SCLKCH  =  '4/11390:22:012'
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
C     C.H. Acton     (JPL)
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.2, 29-JUL-2003 (NJB) 
C
C        Various header changes were made to improve clarity and 
C        more fully explain the routine's functionality.
C
C-    SPICELIB Version 1.2.1, 09-MAR-1999 (NJB) 
C
C        Explicit list of SCLK conversion routines in Particulars 
C        section has been replaced by a pointer to the SCLK Required
C        Reading.
C
C-    SPICELIB Version 1.2.0, 10-APR-1992 (NJB) (WLT)
C
C        Truncation of the output string is now treated as an error.
C        Header was updated to reflect possibility of needing to load
C        a leapseconds kernel before calling this routine.  Comment
C        section for permuted index source lines was added following the
C        header.
C
C-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
C
C        Missing example added to the $ Examples section.  Restrictions
C        section no longer states that you must load the leapseconds
C        kernel prior to calling this routine.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     ephemeris time to spacecraft_clock string
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 10-APR-1992 (NJB) (WLT)
C
C        Truncation of the output string is now treated as an error.
C        The code changes made to implement the error checking were
C        in SCDECD and other lower-level routines.
C
C        The header was updated to reflect possibility of needing to
C        load a leapseconds kernel before calling this routine.
C
C        The comment section for permuted index source lines was added
C        following the header.
C
C
C-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
C
C        Missing example added to the $ Examples section.  Restrictions
C        section no longer states that you must load the leapseconds
C        kernel prior to calling this routine.
C
C        The second example no longer uses a call to CLPOOL.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      SCLKDP
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCE2S' )
      END IF
 
C
C     Convert ET to encoded SCLK, and then to an SCLK string.
C
      CALL SCE2T  ( SC, ET,     SCLKDP )
      CALL SCDECD ( SC, SCLKDP, SCLKCH )
 
      CALL CHKOUT ( 'SCE2S' )
      RETURN
      END
