C$Procedure DISTIM ( Format Time for Displaying by BRIEF )

      SUBROUTINE DISTIM ( TIMTYP, ET, TIMLBL, TIMSTR )

C$ Abstract
C
C     Format time for displaying by BRIEF.
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
C     None.
C
C$ Keywords
C
C     KERNEL
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         TIMTYP
      DOUBLE PRECISION      ET
      CHARACTER*(*)         TIMLBL
      CHARACTER*(*)         TIMSTR

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TIMTYP     I   Desired output format
C     ET         I   ET to be formatted
C     TIMLBL     O   Label for BRIEF table heading.
C     TIMSTR     O   Output time string.
C
C$ Detailed_Input
C
C     TIMTYP     is the desired output format type: ETCAL, UTCCAL,
C                UTCDOY, or ETSEC.
C
C     ET         is the input ET seconds past J2000 to be formatted.
C
C$ Detailed_Output
C
C     TIMLBL     is the label for BRIEF table heading.
C
C     TIMSTR     is the output time string.
C
C$ Parameters
C
C     The output format pictures for TIMOUT and DPFMT are provided
C     using parameters UCLPIC, UDYPIC, and ESCPIC.
C
C$ Exceptions
C
C     1) If the desired output time type is not recognized, then the
C        error SPICE(BADTIMEFORMAT) is signaled.
C
C     2) If required LSK data are not loaded an error will be signaled
C        by routines in the calling tree of this routine.
C
C$ Files
C
C     An LSK file must be loaded prior to calling this routine.
C
C$ Particulars
C
C     The following label and time string will be returned for each
C     of the allowed time formats:
C
C        ETCAL:
C
C           TIMLBL = 'ET'
C           TIMSTR returned by ETCAL
C
C        UTCCAL:
C
C           TIMLBL = 'UTC'
C           TIMSTR returned by TIMOUT in 
C           'YYYY-MON-DD HR:MN:SC.###' format
C
C        UTCDOY:
C
C           TIMLBL = 'UTC'
C           TIMSTR returned by TIMOUT in 
C           'YYYY-DOY // HR:MN:SC.###' format
C
C        ETSEC:
C
C           TIMLBL = 'ET'
C           TIMSTR returned by DPFMT in  
C           'xxxxxxxxxxxxxxxxx.xxxxxx' format
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     This routine must not be called by any routines except BRIEF's
C     DISPLY routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    BRIEF Version 1.0.0, 22-OCT-2007 (BVS)
C
C-&


C$ Index_Entries
C
C     format time for display by BRIEF
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters.
C

C
C     Output format pictures.
C
      CHARACTER*(*)         UCLPIC
      PARAMETER           ( UCLPIC = 'YYYY-MON-DD HR:MN:SC.###' )

      CHARACTER*(*)         UDYPIC
      PARAMETER           ( UDYPIC = 'YYYY-DOY // HR:MN:SC.###' )

      CHARACTER*(*)         ESCPIC
      PARAMETER           ( ESCPIC = 'xxxxxxxxxxxxxxxxx.xxxxxx' )

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN  ( 'DISTIM' )
      END IF

C
C     Set outputs.
C
      IF      ( TIMTYP .EQ. 'ETCAL' ) THEN

         TIMLBL = 'ET'
         CALL ETCAL ( ET, TIMSTR )

      ELSE IF ( TIMTYP .EQ. 'UTCCAL' ) THEN

         TIMLBL = 'UTC'
         CALL TIMOUT( ET, UCLPIC, TIMSTR )

      ELSE IF ( TIMTYP .EQ. 'UTCDOY' ) THEN

         TIMLBL = 'UTC'
         CALL TIMOUT( ET, UDYPIC, TIMSTR )

      ELSE IF ( TIMTYP .EQ. 'ETSEC' ) THEN

         TIMLBL = 'ET'
         CALL DPFMT ( ET, ESCPIC, TIMSTR )

      ELSE

         CALL SETMSG ( 'Time type ''#'' is not recognized.' )
         CALL ERRCH  ( '#', TIMTYP                          )
         CALL SIGERR ( 'SPICE(BADTIMEFORMAT)'               )
         CALL CHKOUT ( 'DISTIM'                             )
         RETURN

      END IF

C
C     All done.
C
      CALL CHKOUT ( 'DISTIM' )
      RETURN

      END
