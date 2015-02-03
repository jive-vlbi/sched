C$Procedure      SCT2E ( SCLK ticks to ET )
 
      SUBROUTINE SCT2E ( SC, SCLKDP, ET )
 
C$ Abstract
C
C     Convert encoded spacecraft clock (`ticks') to ephemeris
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
 
      INTEGER               SC
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      ET
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C     SCLKDP     I   SCLK, encoded as ticks since spacecraft clock
C                    start.
C     ET         O   Ephemeris time, seconds past J2000.
C
C$ Detailed_Input
C
C     SC             is a NAIF integer code for a spacecraft, one of
C                    whose encoded clock values is represented by
C                    SCLKDP.
C
C     SCLKDP         is an encoded spacecraft clock value.  SCLKDP
C                    represents time measured from spacecraft clock
C                    start:  partition information IS reflected in the
C                    encoded value.
C
C$ Detailed_Output
C
C     ET             is the epoch, specified as ephemeris seconds past
C                    J2000, that corresponds to SCLKDP.
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
C         called by this routine.  The output argument ET will not be
C         modified.
C
C     2)  When using SCLK kernels that map SCLK to a time system other
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
C     3)  If the clock type for the spacecraft clock identified by
C         SC is not supported by this routine, the error
C         SPICE(NOTSUPPORTED) is signalled.  The output argument ET
C         will not be modified.
C
C     4)  If the input argument SCLKDP is invalid, the error will be
C         diagnosed by routines called by this routine.  The output
C         argument ET will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine operates on encoded SCLK values.  These values
C     are obtained by calling the SPICELIB routine SCENCD or other
C     SCLK conversion routines.  The advantage of encoded SCLK, as
C     opposed to character string representations of SCLK is that
C     encoded SCLK values are easy to perform arithmetic operations on.
C     Additionally, working with encoded SCLK reduces the overhead of
C     repeated conversion of character strings to integers or double
C     precision numbers.
C
C     To convert a string representation of an SCLK value to ET, use
C     the SPICELIB routine SCS2E.
C
C     See the SCLK Required Reading for a list of the entire set of
C     SCLK conversion routines.
C
C$ Examples
C
C     1)  Encode a Galileo SCLK string, and convert the encoded value
C         to ET; use these time values to look up both GLL orbiter
C         scan platform's pointing and the GLL--Earth state vector
C         for an epoch specified by an SCLK string.
C
C            During program initialization, load the leapseconds and
C            SCLK kernels.  We will pretend that these files are named
C            "LEAPSECONDS.KER" and "GLLSCLK.KER".  To use this code
C            fragment, you must substitute the actual names of these
C            kernel files for the names used here.
C
C               C
C               C     Load leapseconds and SCLK kernels:
C               C
C                     CALL FURNSH ( 'LEAPSECONDS.KER' )
C                     CALL FURNSH ( 'SCLK.KER'        )
C
C            The mission is Galileo, which has spacecraft ID -77.
C            Let's assume that the SCLK string is
C
C                     1 / 1900000:00:00
C
C            The number 1, followed by a slash, indicates that the
C            epoch is in the first partition.
C
C            The next step is to encode this SCLK string, and also
C            find the corresponding ET value:
C
C                     CALL SCENCD ( -77, '1/ 1900000:00:00', SCLKDP )
C                     CALL SCT2E  ( -77,  SCLKDP,            ET     )
C
C            We'll assume that you've already loaded SPK and CK files
C            containing ephemeris data for the GLL orbiter and the
C            Earth, as well as scan platform pointing.  Now you're
C            ready to call both CKGP, which expects the input epoch to
C            be specified by an encoded SCLK string, and SPKEZ, which
C            expects the epoch to be specified as an ephemeris time.
C
C               C
C               C     Find scan platform pointing CMAT and s/c--target
C               C     vector (first 3 components of STATE) at epoch.
C               C     We assume that CK and SPK kernels have been loaded
C               C     already, via CKLPF and SPKLEF respectively.
C               C
C                     SCANPL = -77001
C                     EARTH  =    399
C
C                     CALL CKGP  ( SCANPL,
C                    .             SCLKDP,
C                    .             TOL,
C                    .             REFSYS,
C                    .             CMAT,
C                    .             CLKOUT,
C                    .             FOUND   )
C
C                     IF ( .NOT. FOUND ) THEN
C
C                        [ Indicate to user that pointing was not
C                          available ]
C
C                     END IF
C
C
C                     CALL SPKEZ ( EARTH,
C                    .             ET,
C                    .             REFSYS,
C                    .             CORR,
C                    .             -77,
C                    .             STATE,
C                    .             LT      )
C
C
C
C     2)  Convert an encoded Voyager 2 SCLK value to UTC, using calendar
C         format, with 3 digits of precision.
C
C            Again, your initialization code must load the leapseconds
C            and SCLK kernels:
C
C               C
C               C     Load leapseconds and SCLK kernels:
C               C
C                     CALL FURNSH ( 'LEAPSECONDS.KER' )
C                     CALL FURNSH ( 'VGR2SCLK.KER'    )
C
C
C            To find the UTC value corresponding to the encoded
C            Voyager 2 SCLK value SCLKDP, you can use the code fragment
C
C                     CALL SCT2E  ( -32,  SCLKDP,     ET  )
C                     CALL ET2UTC (  ET,  'C',    3,  UTC )
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
C     [1]  CK Required Reading
C
C     [2]  SPK Required Reading
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.4, 22-AUG-2006 (EDW)
C
C        Replaced references to LDPOOL with references 
C        to FURNSH.
C
C-    SPICELIB Version 1.0.3, 09-MAR-1999 (NJB) 
C
C        Explicit list of SCLK conversion routines in Particulars 
C        section has been replaced by a pointer to the SCLK Required
C        Reading.
C
C-    SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)
C
C        The $Brief_I/O section now lists ET correctly as an output
C        from this routine.  Header was updated to reflect possibility
C        of needing to load a leapseconds kernel before calling this
C        routine.  Comment section for permuted index source lines was
C        added following the header.
C
C-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
C
C        Restrictions section no longer states that you must load the
C        leapseconds kernel prior to calling this routine.
C
C        The examples have been slightly re-written.
C
C-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     spacecraft_clock ticks to ephemeris time
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.0.1, 12-OCT-1990 (NJB)
C
C        Restrictions section no longer states that you must load the
C        leapseconds kernel prior to calling this routine.
C
C        The examples have been slightly re-written.  In particular,
C        they no longer use calls to CLPOOL.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               SCTYPE
 
      LOGICAL               RETURN
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCT2E' )
      END IF
 
C
C     Just hand off the conversion to the appropriate routine.
C
      IF (  SCTYPE ( SC )  .EQ.  1  ) THEN
 
         CALL SCTE01 ( SC, SCLKDP, ET )
 
      ELSE
 
         CALL SETMSG ( 'Clock type # is not supported.' )
         CALL ERRINT ( '#', SCTYPE ( SC )               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'            )
         CALL CHKOUT ( 'SCT2E'                          )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'SCT2E' )
      RETURN
      END
