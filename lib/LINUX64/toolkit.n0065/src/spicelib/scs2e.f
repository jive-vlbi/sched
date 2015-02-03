C$Procedure      SCS2E ( SCLK string to ET )
 
      SUBROUTINE SCS2E ( SC, SCLKCH, ET )
 
C$ Abstract
C
C     Convert a spacecraft clock string to ephemeris seconds past
C     J2000 (ET).
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
      CHARACTER*(*)         SCLKCH
      DOUBLE PRECISION      ET
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SC         I   NAIF integer code for a spacecraft.
C     SCLKCH     I   An SCLK string.
C     ET         O   Ephemeris time, seconds past J2000.
C
C$ Detailed_Input
C
C     SC             is a NAIF ID code for a spacecraft, one of whose
C                    clock values is represented by SCLKCH.  The set of
C                    supported spacecraft clocks is listed in the SCLK
C                    Required Reading.
C
C     SCLKCH         is a character string representation of the
C                    spacecraft clock value that corresponds to ET, for
C                    the spacecraft specified by the input argument SC.
C                    SCLKCH is an absolute spacecraft clock time, so
C                    partition information should be included in this
C                    string.  The precise format of SCLKCH is specified
C                    in the SCLK Required Reading.
C
C$ Detailed_Output
C
C     ET             is the epoch, specified as ephemeris seconds past
C                    J2000, that corresponds to SCLKCH.
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
C     3)  Invalid values of SCLKCH will be diagnosed by routines called
C         by this routine.  The output argument ET will not be modified.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine is provided as a convenience; it is simply shorthand
C     for the code fragment
C
C        CALL SCENCD ( SC, SCLKCH, SCLKDP )
C        CALL SCT2E  ( SC, SCLKDP, ET     )
C
C     See the SCLK Required Reading for a list of the entire set of
C     SCLK conversion routines.
C
C$ Examples
C
C     1)  Find the state (position and velocity) of Jupiter, as seen
C         from the Galileo spacecraft, at the epoch corresponding to
C         the SCLK value
C
C            2 / 3110578:89:09
C
C         The digit `2', followed by the forward slash, indicates that
C         the time value is in the second mission partition.
C
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
C               C
C               C     Load an SPK file (again, a fictitious file)
C               C     containing an ephemeris for Jupiter and the
C               C     GLL orbiter's trajectory.
C               C
C                     CALL SPKLEF ( 'GLLSPK.KER', HANDLE )
C
C            The Galileo spacecraft ID is -77.  Convert our SCLK
C            string to ephemeris seconds past J2000, which is the
C            time representation expected by SPKEZ.
C
C                     CALL SCS2E ( -77, '2 / 3110578:89:09', ET )
C
C
C            Find the state of Jupiter (body 599) as seen from Galileo
C            at time ET.  To use SPKEZ, you must first load an SPK
C            kernel, using the routine SPKLEF.
C
C                     CALL SPKEZ ( 599,
C                    .             ET,
C                    .             REFSYS,
C                    .             CORR,
C                    .             -77,
C                    .             STATE,
C                    .             LT      )
C
C
C
C     2)  Convert a Voyager 2 SCLK value to UTC, using calendar format,
C         with 3 digits of precision in the seconds component.
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
C            To find the UTC value corresponding to Voyager 2 SCLK
C            string
C
C                     11389.20.768
C
C            you can use the code fragment
C
C                     CALL SCS2E  ( -32,  '11389.29.768',  ET  )
C                     CALL ET2UTC (  ET,  'C',      3,     UTC )
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
C     [1]   SPK Required Reading
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
C     spacecraft_clock string to ephemeris time
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
         CALL CHKIN ( 'SCS2E' )
      END IF
 
C
C     Encode SCLKCH, and convert the result to ET.
C
      CALL SCENCD ( SC, SCLKCH, SCLKDP )
      CALL SCT2E  ( SC, SCLKDP, ET     )
 
      CALL CHKOUT ( 'SCS2E' )
      RETURN
      END
