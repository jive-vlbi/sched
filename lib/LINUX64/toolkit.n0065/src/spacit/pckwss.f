C$Procedure      PCKWSS ( PCK write segment summary )
 
      SUBROUTINE PCKWSS ( UNIT,   SEGID,  SEGBOD, SEGFRM,
     .                    SEGTYP, SEGBTM, SEGETM           )
 
C$ Abstract
C
C     Write the segment summary for a PCK segment to a Fortran logical
C     unit.
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
C      None.
C
C$ Keywords
C
C     None.
C
C$ Declarations
 
      INTEGER               UNIT
      CHARACTER*(*)         SEGID
      INTEGER               SEGBOD
      INTEGER               SEGFRM
      INTEGER               SEGTYP
      DOUBLE PRECISION      SEGBTM
      DOUBLE PRECISION      SEGETM
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I   The logical unit to use for writing the summary.
C      SEGIDS    I   Segment ID for the segment in a PCK file.
C      SEGBOD    I   Body for the segment in a PCK file.
C      SEGFRM    I   Reference frame for the segment in a PCK file.
C      SEGTYP    I   Ephemeris type for the segment in a PCK file.
C      SEGBTM    I   Begin time (ET) for the segment in a PCK file.
C      SEGETM    I   End time (ET) for the segment in a PCK file.
C
C$ Detailed_Input
C
C      UNIT     The Fortran logical unit to which the segment summary
C               is written.
C
C      SEGID    Segment ID for a segment in a PCK file.
C
C      SEGBOD   Body for a segment in a PCK file. This is the
C               NAIF integer code for the body.
C
C      SEGFRM   Inertial reference frame for a segment in a PCK file.
C               this is the NAIF integer code for the inertial reference
C               frame.
C
C      SEGTYP   Ephemeris type for a segment in a PCK file. This is an
C               integer code which represents the PCK segment data type.
C
C      SEGBTM   Begin time (ET) for a segment in a PCK file.
C
C      SEGETM   End time (ET) for a segment in a PCK file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If an error occurs while writing to the logical unit, the error
C        will be signalled by a routine called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will format and display a PCK segment summary in a
C     human compatible fashion.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) This routine performs time conversions using ET2UTC, and
C        therefore requires that a SPICE leapseconds kernel file be
C        loaded into the SPICELIB kernel pool before being called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber (JPL)
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    Beta Version 2.1.0, 17-May-2001 (WLT) (20 years in CA today!)
C
C        Added a description for type 03 PCK segments.
C
C-    Beta Version 2.0.0, 24-JAN-1996 (KRG)
C
C        There have been several undocumented revisions of this
C        subroutine to improve its display formats and fix display bugs.
C        We are starting a new trend here, with the documentation of the
C        changes to this version. Hopefully we will continue to do so.
C
C        The changes to this version are:
C
C           Calling a new subroutien to get reference frame names, to
C           support the non-inertial frames software.
C
C           Fixing some display inconsistencies when body, or frame
C           names are not found.
C
C-    Beta Version 1.0.0, 25-FEB-1993 (KRG)
C
C-&
 
C$ Index_Entries
C
C      format and write a pck segment summary
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
C
C     Local parameters
C
C     Set the value for the maximum output display width.
C
      INTEGER               OUTLEN
      PARAMETER           ( OUTLEN = 80 )
C
C     Set the maximum length for the inertial reference frame name.
C
      INTEGER               FRMLEN
      PARAMETER           ( FRMLEN = 32 )
C
C     Set the maximum length for a body name.
C
      INTEGER               BNMLEN
      PARAMETER           ( BNMLEN = 32 )
C
C     Set the precision for fractions of seconds used for UTC times
C     when converted from ET times.
C
      INTEGER               UTPREC
      PARAMETER           ( UTPREC = 3 )
C
C     Set the length of a UTC time string.
C
      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 32 )
C
C     Set the maximum length of an PCK data type description.
C
      INTEGER               PCKLEN
      PARAMETER           ( PCKLEN = 80 )
C
C     Set the maximum number of PCK data types.
C
      INTEGER               MAXTYP
      PARAMETER           ( MAXTYP = 3 )
C
C     Set up some mnemonics for accessing the correct labels.
C
      INTEGER               IDXSID
      PARAMETER           ( IDXSID  = 1          )
 
      INTEGER               IDXBOD
      PARAMETER           ( IDXBOD  = IDXSID + 1 )
 
      INTEGER               IDXFRM
      PARAMETER           ( IDXFRM  = IDXBOD + 1 )
 
      INTEGER               IDXTYP
      PARAMETER           ( IDXTYP  = IDXFRM + 1 )
 
      INTEGER               IDXTPD
      PARAMETER           ( IDXTPD  = IDXTYP + 1 )
 
      INTEGER               IDXUB
      PARAMETER           ( IDXUB   = IDXTPD + 1 )
 
      INTEGER               IDXUE
      PARAMETER           ( IDXUE   = IDXUB + 1  )
 
      INTEGER               IDXETB
      PARAMETER           ( IDXETB  = IDXUE + 1  )
 
      INTEGER               IDXETE
      PARAMETER           ( IDXETE  = IDXETB + 1 )
C
C     Set the number of output lines.
C
      INTEGER               NUMLIN
      PARAMETER           ( NUMLIN = IDXETE )
C
C     Local variables
C
      CHARACTER*(BNMLEN)    BODY
      CHARACTER*(FRMLEN)    FRAME
      CHARACTER*(OUTLEN)    LINES(NUMLIN)
      CHARACTER*(TIMLEN)    BEGTIM
      CHARACTER*(TIMLEN)    ENDTIM
      CHARACTER*(PCKLEN)    PCKTYP(MAXTYP)
      CHARACTER*(PCKLEN)    TYPDSC
 
      LOGICAL               FOUND
 
C
C     Save everything to keep configuration control happy.
C
      SAVE
 
C
C     Initial Values
C
      DATA PCKTYP /
     . '***Not Used***',
     . 'Fixed Width, Fixed Order Chebyshev Polynomials: Angles',
     . 'Variable Width Chebyshev Polynomials Angles (in degrees!!!)'
     .            /
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKWSS' )
      END IF
C
C     Set up the line labels.
C
      LINES(IDXSID) = '   Segment ID     : #'
      LINES(IDXUB)  = '   UTC Start time : #'
      LINES(IDXUE)  = '   UTC Stop time  : #'
      LINES(IDXETB) = '   ET Start time  : #'
      LINES(IDXETE) = '   ET Stop time   : #'
      LINES(IDXBOD) = '   Body           : Body #'
      LINES(IDXFRM) = '   Reference frame: Frame #'
      LINES(IDXTYP) = '   PCK Data Type  : #'
      LINES(IDXTPD) = '      Description : #'
C
C     Format the segment ID.
C
      CALL REPMC  ( LINES(IDXSID), '#', SEGID, LINES(IDXSID) )
C
C     Convert the segment start and stop times from ET to UTC for
C     human readability.
C
      CALL ET2UTC ( SEGBTM, 'C', UTPREC, BEGTIM )
      CALL ET2UTC ( SEGETM, 'C', UTPREC, ENDTIM )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKWSS' )
         RETURN
      END IF
C
C     Format the UTC times.
C
      CALL REPMC ( LINES(IDXUB), '#', BEGTIM, LINES(IDXUB) )
      CALL REPMC ( LINES(IDXUE), '#', ENDTIM, LINES(IDXUE) )
C
C     Convert the ET times into Calendar format.
C
      CALL ETCAL ( SEGBTM, BEGTIM )
      CALL ETCAL ( SEGETM, ENDTIM )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'PCKWSS' )
         RETURN
      END IF
C
C     Format the ET times.
C
      CALL REPMC ( LINES(IDXETB), '#', BEGTIM, LINES(IDXETB) )
      CALL REPMC ( LINES(IDXETE), '#', ENDTIM, LINES(IDXETE) )
C
C     Format the body and its name if we found it.
C
      CALL BODC2N ( SEGBOD, BODY, FOUND )
 
      IF ( FOUND ) THEN
         CALL REPMC  ( LINES(IDXBOD), '#', '#, #', LINES(IDXBOD) )
         CALL REPMI  ( LINES(IDXBOD), '#', SEGBOD, LINES(IDXBOD) )
         CALL REPMC  ( LINES(IDXBOD), '#', BODY,   LINES(IDXBOD) )
      ELSE
         CALL REPMI  ( LINES(IDXBOD), '#', SEGBOD, LINES(IDXBOD) )
      END IF
C
C     Format the inertial reference frame and its name if we found it.
C
      CALL FRMNAM ( SEGFRM, FRAME )
 
      IF ( FRAME .NE. ' ' ) THEN
         CALL REPMC  ( LINES(IDXFRM), '#', '#, #', LINES(IDXFRM) )
         CALL REPMI  ( LINES(IDXFRM), '#', SEGFRM, LINES(IDXFRM) )
         CALL REPMC  ( LINES(IDXFRM), '#', FRAME,  LINES(IDXFRM) )
      ELSE
         CALL REPMI  ( LINES(IDXFRM), '#', SEGFRM, LINES(IDXFRM) )
      END IF
 
C
C     Format the PCK segment type and a description if we have one.
C     The reason SEGTYP >= 2 is that this routine works on binary
C     PCK files, and their segment types begin with type 2. Type 1 is
C     considered to be the text PCK files.
C
      IF ( ( SEGTYP .GT. MAXTYP )  .OR. ( SEGTYP .LT. 2 ) ) THEN
         TYPDSC = 'No description for this type.'
     .          //  ' Do you need a new toolkit?'
      ELSE
         TYPDSC = PCKTYP(SEGTYP)
      END IF
      CALL REPMI  ( LINES(IDXTYP), '#', SEGTYP, LINES(IDXTYP) )
      CALL REPMC  ( LINES(IDXTPD), '#', TYPDSC, LINES(IDXTPD) )
C
C     Display the summary.
C
      CALL WRITLA ( NUMLIN, LINES, UNIT )
C
C     We were either successful or not on the previous write. In either
C     event, we want to check out and return to the caller, so there is
C     no need to check FAILED() here.
C
      CALL CHKOUT ( 'PCKWSS' )
      RETURN
      END
