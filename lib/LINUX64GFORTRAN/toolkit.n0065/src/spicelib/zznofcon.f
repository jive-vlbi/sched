C$Procedure  ZZNOFCON ( Create frame connection long error message )
 
      SUBROUTINE ZZNOFCON ( ET, FRAME1, ENDP1, FRAME2, ENDP2, ERRMSG )

      IMPLICIT NONE
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Create an informative long error message for cases where the
C     frame system signals a SPICE(NOFRAMECONNECT) error.
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
C     FRAMES
C     TIME
C
C$ Keywords
C
C     FRAMES
C     PRIVATE
C     UTILITY
C
C$ Declarations

      INCLUDE 'frmtyp.inc'

      DOUBLE PRECISION      ET
      INTEGER               FRAME1
      INTEGER               ENDP1
      INTEGER               FRAME2
      INTEGER               ENDP2
      CHARACTER*(*)         ERRMSG

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C     FRAME1     I   "From" frame ID code.
C     ENDP1      I   "From" path endpoint frame ID code.
C     FRAME2     I   "To" frame ID code.
C     ENDP2      I   "To" path endpoint frame ID code.
C     ERRMSG     O   Long error message.
C
C$ Detailed_Input
C
C     ET             Epoch of frame transformation, expressed as 
C                    seconds past J2000 TDB.
C
C     FRAME1         Frame ID code of frame at start of first path.
C
C     ENDP1          Frame ID code of frame at end of first path;
C                    this frame is the last node that could be
C                    reached from the frame designated by FRAME1.
C
C     FRAME2         Frame ID code of frame at start of second path.
C
C     ENDP2          Frame ID code of frame at end of second path;
C                    this frame is the last node that could be
C                    reached from the frame designated by FRAME2.
C                    
C$ Detailed_Output
C
C     ERRMSG         Long error message specifying computable
C                    frame paths, indications of missing SCLK
C                    or CK data, and optionally, debugging hints.
C                    
C                    The rules for formation of this message are:
C
C                       1) State the epoch.
C
C                       2) State the names of the frames for which
C                          a connection was attempted, if these
C                          names are available.
C
C                       3) State the names of the frames at the
C                          endpoints of both paths, if these
C                          names are available.
C
C                          Omit this portion of the message for any
C                          path of length one: in other words, if a
C                          frame and path endpoint coincide, omit the
C                          clause stating the frame can be connected to
C                          itself.
C
C                       4) For any path endpoint frame, if that
C                          frame is of CK type, indicate that
C                          CK and SCLK data must be loaded for
C                          that frame.
C
C                       5) For any path endpoint frame, if that
C                          frame is of CK type and SCLK data for the
C                          SCLK associated with that frame are not
C                          available, indicate this problem, along with
C                          the CK and SCLK ID codes associated with
C                          this frame.
C                         
C                       6) If at least one path endpoint frame
C                          is of CK type, and all required SCLK data
C                          are present, include a closing message
C                          explaining how CK coverage may be inadequate
C                          and recommending use of CKBRIEF.
C
C                       7) If both path endpoint frames are of CK type,
C                          and required SCLK data are present for only
C                          one of these frames, include a closing
C                          message explaining how CK coverage may be
C                          inadequate for a frame for which SCLK data
C                          are available, and recommending use of
C                          CKBRIEF.
C
C                       8) Omit the closing message if no path 
C                          endpoint CK frame has associated SCLK
C                          data.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C 
C     1) If a call to FRINFO or NAMFRM signals an error, this routine
C        will not be able to create a long error message. The
C        caller will not be able to diagnose the frame connection
C        failure, since an error condition will already exist.
C
C$ Files
C
C     1) Each input frame ID argument will be mapped, if possible,
C        to a frame name. Any input frame ID that's not built in
C        must 
C
C$ Particulars
C
C     This routine centralizes creation of a long error message for
C     frame connection failures. This routine should be called 
C     from:
C
C        FRMCHG
C        REFCHG
C        ZZFRMCH0
C        ZZFRMCH1
C        ZZREFCH0
C        ZZREFCH1
C
C$ Examples
C
C     Below are some examples of messages created by this routine.
C
C
C    1)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
C         TDB), there is insufficient information available to
C         transform from reference frame -82000 (CASSINI_SC_COORD) to
C         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
C         a CK frame; a CK file containing data for instrument or
C         structure -82000 at the epoch shown above, as well as a
C         corresponding SCLK kernel, must be loaded in order to use
C         this frame. Frame MEX_SPACECRAFT could be transformed to
C         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK
C         file containing data for instrument or structure -41001 at
C         the epoch shown above, as well as a corresponding SCLK
C         kernel, must be loaded in order to use this frame. Failure to
C         find required CK data could be due to one or more CK files
C         not having been loaded, or to the epoch shown above lying
C         within a coverage gap or beyond the coverage bounds of the
C         loaded CK files. It is also possible that no loaded CK file
C         has required angular velocity data for the input epoch, even
C         if a loaded CK does have attitude data for that epoch. You
C         can use CKBRIEF with the -dump option to display coverage
C         intervals of a CK file.
C
C
C    2)   At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
C         TDB), there is insufficient information available to
C         transform from reference frame -82360 (CASSINI_ISS_NAC) to
C         reference frame 1 (J2000). Frame CASSINI_ISS_NAC could be
C         transformed to frame -82000 (CASSINI_SC_COORD). The latter is
C         a CK frame; a CK file containing data for instrument or
C         structure -82000 at the epoch shown above, as well as a
C         corresponding SCLK kernel, must be loaded in order to use
C         this frame. Failure to find required CK data could be due to
C         one or more CK files not having been loaded, or to the epoch
C         shown above lying within a coverage gap or beyond the
C         coverage bounds of the loaded CK files. It is also possible
C         that no loaded CK file has required angular velocity data for
C         the input epoch, even if a loaded CK does have attitude data
C         for that epoch. You can use CKBRIEF with the -dump option to
C
C
C     3)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
C         TDB), there is insufficient information available to
C         transform from reference frame -82000 (CASSINI_SC_COORD) to
C         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
C         a CK frame; a CK file containing data for instrument or
C         structure -82000 at the epoch shown above, as well as a
C         corresponding SCLK kernel, must be loaded in order to use
C         this frame. No SCLK kernel for instrument or structure
C         -82000, with corresponding SCLK ID -82, is currently loaded.
C         Frame MEX_SPACECRAFT could be transformed to frame -41001
C         (MEX_SC_REF). The latter is a CK frame; a CK file containing
C         data for instrument or structure -41001 at the epoch shown
C         above, as well as a corresponding SCLK kernel, must be loaded
C         in order to use this frame. No SCLK kernel for instrument or
C         structure -41001, with corresponding SCLK ID -41, is
C         currently loaded.
C
C
C     4)  At epoch 2.8149297000000E+08 TDB (2008 DEC 02 12:29:30.000
C         TDB), there is insufficient information available to
C         transform from reference frame -82000 (CASSINI_SC_COORD) to
C         reference frame -41000 (MEX_SPACECRAFT). CASSINI_SC_COORD is
C         a CK frame; a CK file containing data for instrument or
C         structure -82000 at the epoch shown above, as well as a
C         corresponding SCLK kernel, must be loaded in order to use
C         this frame. Frame MEX_SPACECRAFT could be transformed to
C         frame -41001 (MEX_SC_REF). The latter is a CK frame; a CK
C         file containing data for instrument or structure -41001 at
C         the epoch shown above, as well as a corresponding SCLK
C         kernel, must be loaded in order to use this frame. No SCLK
C         kernel for instrument or structure -41001, with corresponding
C         SCLK ID -41, is currently loaded. For a CK frame for which
C         the corresponding SCLK kernel has been loaded, failure to
C         find required CK data could be due to one or more CK files
C         not having been loaded, or to the epoch shown above lying
C         within a coverage gap or beyond the coverage bounds of the
C         loaded CK files. It is also possible that no loaded CK file
C         has required angular velocity data for the input epoch, even
C         if a loaded CK does have attitude data for that epoch. You
C         can use CKBRIEF with the -dump option to display coverage
C         intervals of a CK file.
C
C$ Restrictions
C
C     1) This is a private routine. SPICE user applications should not
C        call this routine.
C
C     2) See exception (1) above.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 09-SEP-2013 (NJB)  
C
C        Long error message for missing CK data now
C        mentions the possibility of missing angular
C        velocity data.
C
C-    SPICELIB Version 1.0.0, 14-DEC-2008 (NJB)  
C
C-&
 
C$ Index_Entries
C
C     create error message for frame connection failure
C
C-&
 

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               ZZSCLK

C
C     Local parameters
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )

      INTEGER               PHRSLN
      PARAMETER           ( PHRSLN = 600 )

C
C     Local variables
C
      CHARACTER*(FRNMLN)    BNAME ( 2 )
      CHARACTER*(FRNMLN)    NAME  ( 2 )
      CHARACTER*(PHRSLN)    PHRASE
      CHARACTER*(TIMLEN)    TIMSTR

      INTEGER               ENDPS  ( 2 )
      INTEGER               CENTER
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               FRAMES ( 2 )
      INTEGER               I
      INTEGER               SCLKID

      LOGICAL               CKMISS
      LOGICAL               FOUND
      LOGICAL               HAVNAM ( 2 )
      LOGICAL               SCMISS

C
C     Because this routine might cause a SPICE error to be
C     signaled, we have to check in.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZNOFCON' )

C
C     Capture input IDs in arrays.
C
      FRAMES(1) = FRAME1
      FRAMES(2) = FRAME2
      ENDPS(1)  = ENDP1
      ENDPS(2)  = ENDP2

C
C     The flags CKMISS and SCMISS are used, respectively, to
C     record whether any CK lookup failed due to missing CK
C     data or missing SCLK data. Each of these flags is turned
C     on if at least one lookup failed due to the indicated
C     cause.
C
      CKMISS = .FALSE.
      SCMISS = .FALSE.

C
C     Get a string representation of the transformation epoch.
C
      CALL ETCAL ( ET, TIMSTR )

C
C     Get the names of the participating frames, if available.
C
      CALL FRMNAM ( FRAMES(1),   NAME(1) )
      CALL FRMNAM ( FRAMES(2),   NAME(2) )
      CALL FRMNAM ( ENDPS(1),    BNAME(1) )
      CALL FRMNAM ( ENDPS(2),    BNAME(2) )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZNOFCON' )
         RETURN
      END IF

      DO I = 1, 2

         IF ( NAME(I) .EQ. ' ' ) THEN

            NAME(I)   = 'Name not available'
            HAVNAM(I) = .FALSE.
         ELSE
            HAVNAM(I) = .TRUE.
         END IF

         IF ( BNAME(I) .EQ. ' ' ) THEN
            BNAME(I) = 'Name not available'
         END IF

      END DO 
 
      ERRMSG = 'At epoch # TDB (# TDB), there is '
     .//       'insufficient information available to '
     .//       'transform from reference frame # (@) to '
     .//       'reference frame # (@).'


      CALL REPMF ( ERRMSG, '#', ET,     14, 'E', ERRMSG )
      CALL REPMC ( ERRMSG, '#', TIMSTR,          ERRMSG )

      DO I = 1, 2

         CALL REPMI ( ERRMSG, '#', FRAMES(I), ERRMSG )
         CALL REPMC ( ERRMSG, '@', NAME(I),   ERRMSG )

      END DO

C
C     For any frame graph longer than a single point, tell the user
C     the endpoint of the frame connection graph originating
C     at that frame.
C
      DO I = 1, 2

         IF ( FRAMES(I) .NE. ENDPS(I)  ) THEN

            PHRASE = 'Frame # could be transformed to frame # (@).'

            IF ( HAVNAM(I) ) THEN
               CALL REPMC ( PHRASE, '#', NAME(I),   PHRASE )
            ELSE
               CALL REPMI ( PHRASE, '#', FRAMES(I), PHRASE )
            END IF

            CALL REPMI ( PHRASE, '#', ENDPS(I),  PHRASE )
            CALL REPMC ( PHRASE, '@', BNAME(I),  PHRASE )

            CALL SUFFIX ( PHRASE, 1, ERRMSG )

C
C           The error messages below are appended only if they're not
C           redundant.
C
            IF (  ( I .EQ. 1 )  .OR.  ( ENDPS(2) .NE. ENDPS(1) )  ) THEN
C
C              For each endpoint frame, if that frame is of CK type,
C              indicate the instrument ID for which CK data are needed.
C
               CALL FRINFO ( ENDPS(I), CENTER, CLASS, CLSSID, FOUND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZNOFCON' )
                  RETURN
               END IF



               IF ( FOUND ) THEN

                  IF ( CLASS .EQ. CK ) THEN
 
                     PHRASE = 'The latter is a CK frame; a CK file '
     .               //       'containing data for instrument or '
     .               //       'structure # at the epoch shown above, '
     .               //       'as well as a corresponding SCLK kernel, '
     .               //       'must be loaded in order to use this '
     .               //       'frame.'

                     CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )

                     CALL SUFFIX ( PHRASE, 1, ERRMSG )            

C
C                    Find out whether we have SCLK data for this
C                    CK ID.
C
                     CALL CKMETA ( CLSSID, 'SCLK', SCLKID )

                     IF (  .NOT. ZZSCLK ( CLSSID, SCLKID )  ) THEN

                        SCMISS = .TRUE.

                        PHRASE = 'No SCLK kernel for instrument or '
     .                  //       'structure #, with corresponding '
     .                  //       'SCLK ID #, is currently loaded.'

                        CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )
                        CALL REPMI ( PHRASE, '#', SCLKID, PHRASE )

                        CALL SUFFIX ( PHRASE, 1, ERRMSG )            


                     ELSE
C
C                       If we got here and have the SCLK data, then
C                       we don't have CK data.
C
                        CKMISS = .TRUE.

                     END IF

                  END IF
C
C                 End of CK frame case.
C
               END IF
C
C              End of "info found" case.
C
            END IF
C
C           End of distinct frame case.
C

         ELSE IF (        ( I        .EQ. 1        ) 
     .              .OR.  ( ENDPS(2) .NE. ENDPS(1) )  ) THEN
C
C           The error messages below are appended only if they're not
C           redundant.           
C
C           This graph has length one. If the frame comprising
C           this graph is a CK frame, generate a phrase 
C           indicating the needed CK data.
C
            CALL FRINFO ( FRAMES(I), CENTER, CLASS, CLSSID, FOUND )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZNOFCON' )
               RETURN
            END IF


            IF ( FOUND ) THEN

               IF ( CLASS .EQ. CK ) THEN

                  PHRASE = '# is a CK frame; a CK file '
     .            //       'containing data for instrument or '
     .            //       'structure # at the epoch shown above, '
     .            //       'as well as a corresponding SCLK kernel, '
     .            //       'must be loaded in order to use this '
     .            //       'frame.'

                  IF ( HAVNAM(I) ) THEN
                     CALL REPMC ( PHRASE, '#', NAME(I),   PHRASE )
                  ELSE
                     CALL REPMI ( PHRASE, '#', FRAMES(I), PHRASE )
                  END IF

                  CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )

                  CALL SUFFIX ( PHRASE, 1, ERRMSG )  

C
C                 Find out whether we have SCLK data for this
C                 CK ID.
C
                  CALL CKMETA ( CLSSID, 'SCLK', SCLKID )

                  IF (  .NOT. ZZSCLK ( CLSSID, SCLKID )  ) THEN

                     SCMISS = .TRUE.

                     PHRASE = 'No SCLK kernel for instrument or '
     .               //       'structure #, with corresponding '
     .               //       'SCLK ID #, is currently loaded.'

                     CALL REPMI ( PHRASE, '#', CLSSID, PHRASE )
                     CALL REPMI ( PHRASE, '#', SCLKID, PHRASE )

                     CALL SUFFIX ( PHRASE, 1, ERRMSG )            

                  ELSE
C
C                    If we got here and have the SCLK data, then
C                    we don't have CK data.
C
                     CKMISS = .TRUE.

                  END IF          

               END IF
C
C              End of CK frame case.
C
            END IF
C
C           End of "info found" case.
C
         END IF
C
C        End of path length case.
C
      END DO
C
C     End of path loop.
C

      IF ( CKMISS ) THEN
C
C        At least one lookup failed due to missing CK data.
C
C        The informational message we include depends on whether we
C        also lack SCLK data.
C
         IF ( SCMISS ) THEN
C
C           We lack SCLK data for one frame and CK data for another.
C
            PHRASE = 'For a CK frame for which the corresponding SCLK '
     .      //       'kernel has been loaded, '
     .      //       'failure to find required CK data '
     .      //       'could be due to one or more CK files '
     .      //       'not having been loaded, or to the epoch '
     .      //       'shown above lying within a coverage gap or '
     .      //       'beyond the coverage bounds of the loaded CK '
     .      //       'files. It is also possible that no loaded CK '
     .      //       'file has required angular velocity data for '
     .      //       'the input epoch, even if a loaded CK '
     .      //       'does have attitude data for that epoch. '
     .      //       'You can use CKBRIEF with the -dump option '
     .      //       'to display coverage intervals of a CK file.'

         ELSE
C
C           We have SCLK data but lack CK data.
C
            PHRASE = 'Failure to find required CK data '
     .      //       'could be due to one or more CK files '
     .      //       'not having been loaded, or to the epoch '
     .      //       'shown above lying within a coverage gap or '
     .      //       'beyond the coverage bounds of the loaded CK '
     .      //       'files. It is also possible that no loaded CK '
     .      //       'file has required angular velocity data for '
     .      //       'the input epoch, even if a loaded CK '
     .      //       'does have attitude data for that epoch. '
     .      //       'You can use CKBRIEF with the -dump option '
     .      //       'to display coverage intervals of a CK file.'

         END IF

         CALL SUFFIX ( PHRASE, 1, ERRMSG )

      END IF
 
      CALL CHKOUT ( 'ZZNOFCON' )
      RETURN
      END
