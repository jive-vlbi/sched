C$Procedure PCKPDS ( PCK, pack descriptor )

      SUBROUTINE PCKPDS ( BODY, FRAME, TYPE, FIRST, LAST, DESCR )

C$ Abstract
C
C     Perform routine error checks and if all checks pass, pack the
C     descriptor for a PCK segment
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
C     PCK.
C
C$ Keywords
C
C     PCK
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               BODY
      CHARACTER*(*)         FRAME
      INTEGER               TYPE
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      DESCR ( * )

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BODY       I   The NAIF ID code for the body of the segment.
C     FRAME      I   The inertial frame for this segment.
C     TYPE       I   The type of PCK segment to create.
C     FIRST      I   The first epoch for which the segment is valid.
C     LAST       I   The last  epoch for which the segment is valid.
C     DESCR      O   A PCK segment descriptor.
C
C$ Detailed_Input
C
C     BODY       is the NAIF ID code for the body of the segment.
C
C     FRAME      is a string that names the inertial frame to which
C                states for the body shall be referenced.
C
C     TYPE       is the type of PCK segment to create.
C
C     FIRST      is the first epoch for which the segment will have
C                ephemeris data.
C
C     LAST       is the last epoch for which the segment will have
C                ephemeris data.
C
C$ Detailed_Output
C
C     DESCR       is a valid PCK segment descriptor to use
C                 when creating a DAF segment for this body.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) The error 'SPICE(BARYCENTERIDCODE)' is signalled if the
C        value of BODY is the ID code of a barycenter, codes
C        0, 1, ..., 9.
C
C     3) The error 'SPICE(INVALIDREFFRAME)' is signalled if FRAME
C        is not one of the known SPICE inertial reference frames.
C
C     4) The error 'SPICE(BADDESCRTIMES)' is signalled if FIRST
C        is greater than or equal to LAST
C
C     5) The error 'SPICE(UNKNOWNPCKTYPE)' is signalled if the
C        value of TYPE is outside the range 2 to 1000 (inclusive).
C        This does not ensure that the TYPE is a legitimate PCK
C        segment type, but it is a simple check that helps avoid
C        problems that arise from unitialized values or improperly
C        ordered calling arguments.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine for validating and creating
C     the descriptor for a PCK segment.  It is intended for
C     use only by routines that create PCK segments.
C
C$ Examples
C
C     Suppose that you wish to create a PCK segment of type X
C     and that you are writing a routine to handle the details
C     of the segment creation.  This routine can be used to
C     ensure that the descriptor needed for the segment is
C     properly formed and that the information in that descriptor
C     is reasonable.
C
C     Having collected the needed information you can create the
C     descriptor and then begin a new segment as shown below.
C
C     CALL PCKPDS ( BODY, FRAME, TYPE, FIRST, LAST, DESCR )
C     CALL DAFBNA ( HANDLE, DESCR,  SEGID )
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
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Corrected order of header sections to conform to NAIF 
C        standard.
C
C-    SPICELIB Version 1.0.0, 04-JAN-1995 (WLT)
C
C-&

C$ Index_Entries
C
C     Validate and pack a PCK segment descriptor
C
C-&

C
C     Spicelib Functions
C
      LOGICAL               RETURN
C
C     Local Parameters
C
C     ND and NI values for a PCK file.
C
      INTEGER               ND
      PARAMETER           ( ND = 2 )

      INTEGER               NI
      PARAMETER           ( NI = 5 )
C
C     Length of a calender string.
C
      INTEGER               CALSIZ
      PARAMETER           ( CALSIZ = 40 )
C
C     Local Variables
C
      CHARACTER*(CALSIZ)    CALFST
      CHARACTER*(CALSIZ)    CALLST

      DOUBLE PRECISION      DPPART ( ND )

      INTEGER               IPART  ( NI )
      INTEGER               REFCOD

C
C     Standard SPICLEIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKPDS' )
      END IF
C
C     We do not support orientation models for barycenters.
C
      IF ( ( BODY .GE. 0 ) .AND. ( BODY .LE. 9 ) ) THEN

         CALL SETMSG ( 'You have attempted to create a segment '
     .   //            ' for for a barycenter, and the PCK system'
     .   //            ' does not support this.'                   )
         CALL SIGERR ( 'SPICE(BARYCENTERIDCODE)'                   )
         CALL CHKOUT ( 'PCKPDS'                                    )

         RETURN

      END IF
C
C     Get the NAIF integer code for the reference frame.
C
      CALL IRFNUM ( FRAME, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', FRAME                                  )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'PCKPDS'                                    )
         RETURN

      END IF
C
C     The segment stop time should be greater then the begin time.
C
      IF ( FIRST .GE. LAST ) THEN
C
C        We've got an error. Get the calendar string for the first
C        and last epochs.
C
         CALL ETCAL  ( FIRST, CALFST )
         CALL ETCAL  ( LAST,  CALLST )

         CALL SETMSG ( 'The segment start time: # (#) is at or'
     .   //            'after the segment stop time # (#). '       )

         CALL ERRDP  ( '#', FIRST                                  )
         CALL ERRCH  ( '#', CALFST                                 )
         CALL ERRDP  ( '#', LAST                                   )
         CALL ERRCH  ( '#', CALLST                                 )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'PCKPDS'                                    )
         RETURN

      END IF
C
C     The type must be something reasonable.  The interval from
C     2 to 1000 is what we are calling reasonable these days.
C
      IF ( ( TYPE .LE. 1)  .OR. ( TYPE .GT. 1000 ) ) THEN

         CALL SETMSG ( 'The type specified, #, is not supported '
     .   //            'within the PCK system. '                  )
         CALL ERRINT ( '#', TYPE                                  )
         CALL SIGERR ( 'SPICE(UNKNOWNPCKTYPE)'                    )
         CALL CHKOUT ( 'PCKPDS'                                   )
         RETURN

      END IF
C
C     Well, that's it.  As far as we can determine these seem to be
C     reasonble values to put into a descriptor.   Do it.
C
      IPART(1)  = BODY
      IPART(2)  = REFCOD
      IPART(3)  = TYPE
      IPART(4)  = 0
      IPART(5)  = 0

      DPPART(1) = FIRST
      DPPART(2) = LAST

      CALL DAFPS  ( ND, NI, DPPART, IPART, DESCR )

      CALL CHKOUT ( 'PCKPDS' )
      RETURN

      END
