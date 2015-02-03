
C$Procedure      CKWSS ( CK write segment summary )

      SUBROUTINE CKWSS ( UNIT,   SEGID,  SEGINS, SEGFRM, 
     .                   SEGTYP, SEGRTS, SEGBTM, SEGETM  )

C$ Abstract
C
C     Write a segment summary for a CK segment to a Fortran logical
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
      INTEGER               SEGINS
      INTEGER               SEGFRM
      INTEGER               SEGTYP
      INTEGER               SEGRTS
      DOUBLE PRECISION      SEGBTM
      DOUBLE PRECISION      SEGETM
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      UNIT      I   The logical unit to use for writing the summary.
C      SEGID     I   Segment ID for a segment in a CK file.
C      SEGINS    I   ID for the instrument having data in a CK segment.
C      SEGFRM    I   Reference frame for a segment in a CK file.
C      SEGTYP    I   Data type for a segment in a CK file.
C      SEGRTS    I   Flag for velocity info in a CK segment.
C      SEGBTM    I   Begin time (SCLK) for a segment in a CK file.
C      SEGETM    I   End time (SCLK) for a segment in a CK file.
C
C$ Detailed_Input
C
C      UNIT     The Fortran logical unit on which the segment summary
C               is to be written.
C
C      SEGID    Segment ID for the current segment in a CK file.
C      
C      SEGINS   NAIF integer ID code for the instrument having data
C               in the current segment in a CK file.
C               
C      SEGFRM   Inertial reference frame for the current segment in a
C               CK file. This is the NAIF integer code for the inertial
C               reference frame.
C               
C      SEGTYP   Data type for the current segment in a CK file. This
C               is an integer code which specifies the type of the data
C               in the current segment.
C               
C      SEGRTS   Integer flag which indicates whether the segment
C               contains angular velocity data in addition to pointing
C               data, SEGRTS .EQ. 1, or just pointing data, SEGRTS .EQ.
C               0.
C               
C      SEGBTM   The beginning encoded SCLK time for the data in the
C               current segment in a CK file.
C               
C      SEGETM   The ending encoded SCLK time for the data in the
C               current segment in a CK file.
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
C        SPICE(FILEWRITEFAILED) will be signalled.
C
C     2) If an error occurs in a routine called by this routine, this
C        routine will check out and return. Presumably an appropriate
C        error message will already have been set.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will format and display a CK segment summary in a
C     human compatible fashion.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) This routine performs time conversions using SCDECD, and
C        therefore requires that a SPICE SCLK kernel file be
C        loaded into the SPICELIB kernel pool before it is called.
C     
C     2) This routine performs time conversions using ET2UTC, and
C        therefore requires that a SPICE leapseconds kernel file be
C        loaded into the SPICELIB kernel pool before it is called.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    SPACIT Version 4.0.0,  08-MAR-2014 (NJB)
C
C        The routine was updated to handle CK type 6.
C
C-    SPACIT Version 3.0.0,  28-AUG-2002 (NJB)
C
C        The routine was updated to handle CK types 4 and 5.
C
C-    Beta Version 2.1.0,  7-FEB-1997 (WLT)
C
C        The routine was modified to use CKMETA to obtain the
C        spacecraft and spacecraft clock associated with a
C        a segment.  This replaces the old method of just dividing
C        by 1000.
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
C      format and write a ck segment summary
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
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )
C
C     Set the precision for fractions of seconds used for UTC times
C     when converted from ET times.
C     
      INTEGER               UTPREC
      PARAMETER           ( UTPREC = 3 )
C
C     Set the length of a time string, UTC or SCLK.
C     
      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 32 )
C
C     Set the maximum length of a CK data type description.
C     
      INTEGER               CKLEN
      PARAMETER           ( CKLEN = 80 )
C
C     Set a value for the length of the pointing only/pointing and
C     angular velocity messages.
C     
      INTEGER               PVLEN
      PARAMETER           ( PVLEN = 40 )
C
C     Set the maximum number of CK data types.
C     
      INTEGER               MAXTYP
      PARAMETER           ( MAXTYP = 6 )
C
C     Set up some mnemonics for accessing the correct labels.
C
      INTEGER               IDXSID
      PARAMETER           ( IDXSID  = 1          )

      INTEGER               IDXINS
      PARAMETER           ( IDXINS  = IDXSID + 1  )

      INTEGER               IDXSC
      PARAMETER           ( IDXSC   = IDXINS + 1 )

      INTEGER               IDXFRM
      PARAMETER           ( IDXFRM  = IDXSC + 1 )

      INTEGER               IDXTYP
      PARAMETER           ( IDXTYP  = IDXFRM + 1 )

      INTEGER               IDXTPD
      PARAMETER           ( IDXTPD  = IDXTYP + 1 )

      INTEGER               IDXAVD
      PARAMETER           ( IDXAVD  = IDXTPD + 1 )

      INTEGER               IDXUB
      PARAMETER           ( IDXUB   = IDXAVD + 1  )

      INTEGER               IDXUE
      PARAMETER           ( IDXUE   = IDXUB + 1   )

      INTEGER               IDXSCB
      PARAMETER           ( IDXSCB  = IDXUE + 1  )

      INTEGER               IDXSCE
      PARAMETER           ( IDXSCE  = IDXSCB + 1 )
C
C     Set the number of output lines.
C     
      INTEGER               NUMLIN
      PARAMETER           ( NUMLIN = IDXSCE )
C
C     Local variables
C
      CHARACTER*(NAMLEN)    SPNAME
      CHARACTER*(CKLEN)     CKTYP(MAXTYP)
      CHARACTER*(FRMLEN)    FRAME
      CHARACTER*(OUTLEN)    LINES(NUMLIN)
      CHARACTER*(TIMLEN)    BEGTIM
      CHARACTER*(TIMLEN)    ENDTIM
      CHARACTER*(PVLEN)     PVSTAT(2)
      CHARACTER*(CKLEN)     TYPDSC
      
      DOUBLE PRECISION      BEGET
      DOUBLE PRECISION      ENDET
      
      INTEGER               SPCRFT
      INTEGER               SCLK
      
      LOGICAL               FOUND
      SAVE
C
C     Initial Values
C     
      DATA CKTYP  / 
     .     'Discrete Pointing',
     .     'Continuous Pointing: Constant Angular Velocity',
     .     'Continuous Pointing: Linear Interpolation',
     .     'Continuous Pointing: Chebyshev, Variable Interval Length',
     .     'Continuous Pointing: MEX/Rosetta Polynomial Interpolation',
     .     'Continuous Pointing: ESOC/DDID Piecewise Interpolation'
     .            /
     
      DATA PVSTAT  / 
     .               'Pointing Only',
     .               'Pointing and Angular Velocity'
     .             /
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKWSS' )
      END IF
C
C     Set up the line labels.
C
      LINES(IDXSID) = '   Segment ID     : #'
      LINES(IDXSC ) = '   Spacecraft     : Body #'
      LINES(IDXINS) = '   Instrument Code: #'
      LINES(IDXUB ) = '   UTC Start Time : #'
      LINES(IDXUE ) = '   UTC Stop Time  : #'
      LINES(IDXSCB) = '   SCLK Start Time: #'
      LINES(IDXSCE) = '   SCLK Stop Time : #'
      LINES(IDXFRM) = '   Reference Frame: Frame #'
      LINES(IDXTYP) = '   CK Data Type   : Type #'
      LINES(IDXTPD) = '      Description : #'
      LINES(IDXAVD) = '   Available Data : #'
C
C     Format the segment ID.
C        
      CALL REPMC  ( LINES(IDXSID), '#', SEGID, LINES(IDXSID) )
C
C     Get the spacecraft ID code from the instrument ID code by dividing
C     by 1000.
C     
      CALL CKMETA ( SEGINS, 'SPK',  SPCRFT )
      CALL CKMETA ( SEGINS, 'SCLK', SCLK   )

C
C     Format the spacecraft name and its name if we found it.
C     
      CALL BODC2N ( SPCRFT, SPNAME, FOUND )

      IF ( FOUND ) THEN
         CALL REPMC  ( LINES(IDXSC), '#', '#, #', LINES(IDXSC) )
         CALL REPMI  ( LINES(IDXSC), '#', SPCRFT, LINES(IDXSC) )
         CALL REPMC  ( LINES(IDXSC), '#', SPNAME, LINES(IDXSC) )
      ELSE
         CALL REPMI  ( LINES(IDXSC), '#', SPCRFT, LINES(IDXSC) )
      END IF
C
C     Format the instrument name if we found it.
C     
      CALL REPMI  ( LINES(IDXINS), '#', SEGINS, LINES(IDXINS) )
C
C     Convert the segment start and stop times from encoded SCLK
C     to SCLK time strings that are human readable.
C     
      CALL SCDECD ( SCLK, SEGBTM, BEGTIM )
      CALL SCDECD ( SCLK, SEGETM, ENDTIM )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKWSS' )
         RETURN
      END IF
C
C     Format the UTC AND SCLK times.
C 
      CALL REPMC  ( LINES(IDXSCB), '#', BEGTIM, LINES(IDXSCB) )
      CALL REPMC  ( LINES(IDXSCE), '#', ENDTIM, LINES(IDXSCE) )
C
C     Convert the segment start and stop times from encoded SCLK to ET
C     so that we can convert them to UTC.
C     
      CALL SCT2E ( SCLK, SEGBTM, BEGET )
      CALL SCT2E ( SCLK, SEGETM, ENDET )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKWSS' )
         RETURN
      END IF
C
C     Convert the segment start and stop times from ET to UTC for
C     human readability.
C        
      CALL ET2UTC ( BEGET, 'C', UTPREC, BEGTIM )
      CALL ET2UTC ( ENDET, 'C', UTPREC, ENDTIM )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKWSS' )
         RETURN
      END IF
C
C     Format the UTC times.
C        
      CALL REPMC  ( LINES(IDXUB),  '#', BEGTIM, LINES(IDXUB) )
      CALL REPMC  ( LINES(IDXUE),  '#', ENDTIM, LINES(IDXUE) )
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
C     Format the CK segment type and a description if we have one.
C     
      IF ( ( SEGTYP .GT. MAXTYP )  .OR. ( SEGTYP .LT. 1 ) ) THEN
         TYPDSC = 'No description for this type.'
     .          //  ' Do you need a new toolkit?'
      ELSE
         TYPDSC = CKTYP(SEGTYP)
      END IF

      CALL REPMI  ( LINES(IDXTYP), '#', SEGTYP, LINES(IDXTYP) )
      CALL REPMC  ( LINES(IDXTPD), '#', TYPDSC, LINES(IDXTPD) )
C
C     Format the pointing / pointing and angular velocity status
C     
      CALL REPMC ( LINES(IDXAVD), '#', PVSTAT(SEGRTS+1), LINES(IDXAVD) )
C
C     Display the summary.
C
      CALL WRITLA ( NUMLIN, LINES, UNIT )
C
C     We were either successful or not on the previous write. In either 
C     event, we want to check out and return to the caller, so there is 
C     no need to check FAILED() here.
C
      CALL CHKOUT ( 'CKWSS' )
      RETURN
      END
      
