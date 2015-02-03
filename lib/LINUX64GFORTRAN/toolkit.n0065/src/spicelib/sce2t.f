C$Procedure      SCE2T ( ET to discrete SCLK ticks )
 
      SUBROUTINE SCE2T ( SC, ET, SCLKDP )
 
C$ Abstract
C
C     Convert ephemeris seconds past J2000 (ET) to integral
C     encoded spacecraft clock (`ticks'). For conversion to
C     fractional ticks, (required for C-kernel production), see
C     the routine SCE2C.
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
      DOUBLE PRECISION      SCLKDP
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF spacecraft ID code.
C     ET         I   Ephemeris time, seconds past J2000.
C     SCLKDP     O   SCLK, encoded as ticks since spacecraft clock
C                    start.
C
C$ Detailed_Input
C
C     SC             is a NAIF integer code for a spacecraft whose
C                    encoded SCLK value at the epoch specified by ET is
C                    desired.
C
C     ET             is an epoch, specified as ephemeris seconds past
C                    J2000.
C
C$ Detailed_Output
C
C     SCLKDP         is an encoded integral spacecraft clock value.
C                    SCLKDP is an encoded representation of the total
C                    count of spacecraft clock ticks measured from the
C                    time the spacecraft clock started to the epoch ET:
C                    partition information IS reflected in the encoded
C                    value.  SCLKDP is rounded to the nearest integral
C                    double precision number.
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
C         called by this routine.  The output argument SCLKDP will not
C         be modified.
C
C     2)  When using SCLK kernels that map SCLK to a time system other
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
C
C     3)  If the clock type for the spacecraft clock identified by
C         SC is not supported by this routine, the error
C         SPICE(NOTSUPPORTED) is signaled.  The output argument SCLKDP
C         will not be modified.
C
C     4)  If the input ET value is not representable as an encoded
C         spacecraft clock value for the spacecraft clock identified by
C         SC, the error will be diagnosed by routines called by this
C         routine.  The output argument SCLKDP will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine outputs discrete, encoded SCLK values.  Since 
C     continuous SCLK values are generally more useful, the newer
C     routine SCE2C (ET to continuous ticks) should normally be used
C     in place of this one.  However, the functionality of this 
C     routine is needed for converting ET to SCLK strings, and the
C     routine SCE2S calls this routine for that purpose.
C     
C     The advantage of encoded SCLK, as opposed to character string 
C     representations of SCLK, is that encoded SCLK values are easy to 
C     perform arithmetic operations on.  Also, working with encoded SCLK
C     reduces the overhead of repeated conversion of  character strings
C     to integers or double precision numbers.
C
C     To convert ET to a string representation of an SCLK value, use
C     the SPICELIB routine SCE2S.
C
C     See the SCLK Required Reading for a list of the entire set of
C     SCLK conversion routines.
C
C$ Examples
C
C     1)  Convert ET directly to an encoded SCLK value; use both of
C         these time values to look up both C-kernel (pointing) and
C         SPK (position and velocity) data for an epoch specified by an
C         ephemeris time.
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
C                     CALL FURNSH ( 'GLLSCLK.KER'     )
C
C            The mission is Galileo, which has spacecraft ID -77.
C            Let ET be the epoch, specified in ephemeris seconds
C            past J2000, at which both position and pointing data
C            is desired.
C
C            Find the encoded SCLK value corresponding to ET.
C
C                     CALL SCE2T  ( -77,  ET,  SCLKDP )
C
C            Now you're ready to call both CKGP, which expects the input
C            epoch to be specified by an encoded SCLK string, and
C            SPKEZ, which expects the epoch to be specified as an
C            ephemeris time.
C
C               C
C               C     Find scan platform pointing CMAT and s/c--target
C               C     vector (first 3 components of STATE) at epoch.
C               C     We assume that CK and SPK kernels have been loaded
C               C     already, via CKLPF and SPKLEF respectively.
C               C
C                     CALL CKGP  ( SCANPL,
C                    .             SCLKDP,
C                    .             TOL,
C                    .             REFSYS,
C                    .             CMAT,
C                    .             CLKOUT,
C                    .             FOUND   )
C
C                     CALL SPKEZ ( TARGET,
C                    .             ET,
C                    .             REFSYS,
C                    .             CORR,
C                    .             -77,
C                    .             STATE,
C                    .             LT      )
C
C
C     2)  Convert UTC to an encoded Voyager 2 SCLK value.
C
C            Again, your initialization code must load the leapseconds
C            and SCLK kernels.
C
C               C
C               C     Load leapseconds and SCLK kernels:
C               C
C                     CALL FURNSH ( 'LEAPSECONDS.KER' )
C                     CALL FURNSH ( 'VGR2SCLK.KER'    )
C
C
C            To find the encoded Voyager 2 SCLK value SCLKDP
C            corresponding to a UTC time, you can use the code fragment
C
C                     CALL UTC2ET ( UTC,  ET          )
C                     CALL SCE2T  ( -32,  ET,  SCLKDP )
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
C-    SPICELIB Version 1.0.4, 27-JAN-2004 (NJB)  
C
C        Header was updated to remove comments indicating this routine
C        was deprecated.  Minor changes were made to clarify both the 
C        functionality of this routine and the difference between 
C        this routine and SCE2C.  Examples were updated to use FURNSH.
C
C-    SPICELIB Version 1.0.3, 09-MAR-1999 (NJB)  
C
C        Updated to reflect the introduction of continuous ticks and
C        the routine SCE2C.
C
C-    SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)
C
C        Header was updated to reflect possibility of needing to load
C        a leapseconds kernel before calling this routine.  Comment
C        section for permuted index source lines was added following the
C        header.
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
C     ephemeris time to spacecraft_clock ticks
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
      LOGICAL               RETURN
 
      INTEGER               SCTYPE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SCE2T' )
      END IF
 
C
C     Just hand off the conversion to the appropriate routine.
C
      IF (  SCTYPE ( SC )  .EQ.  1  ) THEN
 
         CALL SCET01 ( SC, ET, SCLKDP )
 
      ELSE
 
         CALL SETMSG ( 'Clock type # is not supported.' )
         CALL ERRINT ( '#', SCTYPE ( SC )               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'            )
         CALL CHKOUT ( 'SCE2T'                          )
         RETURN
 
      END IF
 
 
      CALL CHKOUT ( 'SCE2T' )
      RETURN
      END
