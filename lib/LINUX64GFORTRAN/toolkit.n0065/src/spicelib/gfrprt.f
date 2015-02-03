C$Procedure GFRPRT ( GF, progress reporting package )
 
      SUBROUTINE GFRPRT ( WINDOW, BEGMSS, ENDMSS, IVBEG, IVEND, TIME )
 
C$ Abstract
C
C     The entry points contained under this routine provide users
C     information regarding the status of a GF search in progress.
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
C     GF
C
C$ Keywords
C
C     SEARCH
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzgf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      WINDOW ( LBCELL : * )
      CHARACTER*(*)         BEGMSS
      CHARACTER*(*)         ENDMSS
      DOUBLE PRECISION      IVBEG
      DOUBLE PRECISION      IVEND
      DOUBLE PRECISION      TIME
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   The SPICELIB cell lower bound.
C     MXBEGM     P   Maximum progress report message prefix length.
C     MXENDM     P   Maximum progress report message suffix length.
C     WINDOW     I   A window over which a job is to be performed.
C     BEGMSS     I   Beginning of the text portion of the output message
C     ENDMSS     I   End of the text portion of the output message
C     IVBEG      I   Current confinement window interval start time.
C     IVEND      I   Current confinement window interval stop time.
C     TIME       I   Input to the reporting routine.
C
C$ Detailed_Input
C
C     See the individual entry points.
C
C$ Detailed_Output
C
C     See the individual entry points.
C
C$ Parameters
C
C     LBCELL    is the SPICELIB cell lower bound.
C
C     MXBEGM,
C     MXENDM    are, respectively, the maximum lengths of the progress
C               report message prefix and suffix.
C
C$ Exceptions
C
C     See the individual entry points.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This umbrella routine contains default progress reporting entry
C     points that display a report via console I/O. These routines may
C     be used by SPICE-based applications as inputs to mid-level GF
C     search routines. These routines may be useful even when progress
C     reporting is not desired, since the mid-level search routines
C     provide some capabilities that aren't supported by the top-level
C     GF routines.
C
C     Developers wishing to use their own GF progress reporting
C     routines must design them with the same interfaces and should
C     assign them the same progress reporting roles as the entry points
C     of these routines.
C
C     The entry points contained in this routine are written to
C     make reporting of work (such as searching for a geometric event)
C     over a particular window easy. This is an important feature for
C     interactive programs that may "go away" from the user's control
C     for a considerable length of time. It allows the user to see that
C     something is still going on (although maybe not too quickly).
C
C     The three entry points contained under this module are:
C
C        GFREPI  used to set up the reporting mechanism. It lets GFRPRT
C                know that some task is about to begin that involves
C                interaction with some window of times.  It is used
C                only to set up and store the constants associated with
C                the reporting of the job in progress.
C
C        GFREPU  is used to notify the reporter that work has
C                progressed to a given point with respect to the start
C                of the confinement window.
C
C        GFREPF  is used to "finish" the reporting of work (set the
C                completion value to 100%.
C
C     The progress reporting utilities are called by GF search routines
C     as follows:
C
C        1) Given a window over which some work is to be performed,
C           CALL GFREPI with the appropriate inputs, to let the routine
C           know the intervals over which some work is to be done.
C
C        2) Each time some "good" amount of work has been done, call
C           GFREPU so that the total amount of work done can be updated
C           and can be reported.
C
C        3) When work is complete call GFREPF to "clean up" the end of
C           the progress report.
C
C$ Examples
C
C     1)  This example shows how to call a mid-level GF search API that
C         requires as input progress reporting routines.
C
C         If custom progress reporting routines are available, they
C         can replace GFREPI, GFREPU, and GFREPF in any GF API calls.
C
C         The code fragment below is from the first code example in the
C         header of
C
C            gfocce.for
C
C         Only the portions of that program relevant to use of the
C         progress reporting routines are copied here. Deleted portions
C         of code are indicated by ellipses.
C
C
C              PROGRAM EX1
C
C              IMPLICIT NONE
C               
C              ...
C
C              EXTERNAL              GFREPI
C              EXTERNAL              GFREPU
C              EXTERNAL              GFREPF
C
C              ...
C
C        C
C        C     Turn on progress reporting; turn off interrupt
C        C     handling.
C        C
C              RPT  = .TRUE.
C              ...
C
C        C
C        C     Perform the search.
C        C
C              CALL GFOCCE ( 'ANY',
C             .              'MOON',   'ellipsoid',  'IAU_MOON',
C             .              'SUN',    'ellipsoid',  'IAU_SUN',
C             .              'LT',     'EARTH',      CNVTOL,
C             .              GFSTEP,   GFREFN,       RPT,
C             .              GFREPI,   GFREPU,       GFREPF,
C             .              BAIL,     GFBAIL,       CNFINE,  RESULT )
C
C
C             ...
C
C
C
C     2)  The following piece of code provides a more concrete example
C         of how these routines might be used.  It is part of code that
C         performs a search for the time of an occultation of one body
C         by another. It is intended only for illustration and is not
C         recommended for use in code that has to do real work.
C
C  C
C  C     Prepare the progress reporter if appropriate.
C  C
C        IF ( RPT ) THEN
C           CALL UDREPI ( CNFINE, 'Occultation/transit search ',
C       .                         'done.'                        )
C        END IF
C
C  C
C  C     Cycle over the intervals in the confining window.
C  C
C        COUNT = WNCARD(CNFINE)
C
C        DO I = 1, COUNT
C  C
C  C        Retrieve the bounds for the Ith interval of the confinement
C  C        window. Search this interval for occultation events.
C  C        Union the result with the contents of the RESULT window.
C  C
C           CALL WNFETD ( CNFINE, I, START, FINISH  )
C
C           CALL ZZGFSOLV ( ZZGFOCST,   UDSTEP,   UDREFN,   BAIL,
C    .                      UDBAIL,     CSTEP,    STEP,     START,
C       .                   FINISH,     TOL,      RPT,      UDREPU,
C       .                   RESULT   )
C
C
C           IF (  FAILED()  ) THEN
C              CALL CHKOUT ( 'GFOCCE'  )
C              RETURN
C           END IF
C
C           IF ( BAIL ) THEN
C  C
C  C           Interrupt handling is enabled.
C  C
C              IF ( UDBAIL () ) THEN
C  C
C  C              An interrupt has been issued. Return now regardless of
C  C              whether the search has been completed.
C  C
C                 CALL CHKOUT ( 'GFOCCE' )
C                 RETURN
C
C              END IF
C
C           END IF
C
C        END DO
C
C  C
C  C     End the progress report.
C  C
C        IF ( RPT ) THEN
C           CALL UDREPF
C        END IF
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
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1 10-FEB-2014 (BVS)
C
C        Added declarations of IVBEG and IVEND to the Declarations
C        section of the GFREPU header.
C
C        Corrected declaration of WINDOW in the Declarations
C        section and added descriptions of LBCELL to the GFREPI 
C        header.
C
C-    SPICELIB Version 1.0.0 06-MAR-2009 (NJB) (LSE) (WLT) (IMU)
C
C
C-&
 
C$ Index_Entries
C
C     GF progress report umbrella
C
C-& 
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               RETURN 

C
C     Local parameters
C
      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )
 
      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )

C
C     Local variables
C
 
      CHARACTER*(MXBEGM)    BEGIN
      CHARACTER*(MXBEGM)    COPYB
      CHARACTER*(MXENDM)    COPYE
      CHARACTER*(MXENDM)    END
 
      DOUBLE PRECISION      AVE
      DOUBLE PRECISION      FREQ
      DOUBLE PRECISION      INCR
      DOUBLE PRECISION      MEASUR
      DOUBLE PRECISION      REMAIN
      DOUBLE PRECISION      STDDEV
      DOUBLE PRECISION      T0
      DOUBLE PRECISION      TOTAL
 
      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               LONG
      INTEGER               SHORT
      INTEGER               STDOUT
      INTEGER               TCHECK
      INTEGER               UNIT
     
      SAVE                  COPYB
      SAVE                  COPYE
      SAVE                  T0
      SAVE                  REMAIN

 
      CALL CHKIN  ( 'GFRPRT'            )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'GFRPRT'            )
      RETURN
 

 
C$Procedure GFREPI ( GF, progress report initialization )
 
      ENTRY GFREPI ( WINDOW, BEGMSS, ENDMSS )
 
C$ Abstract
C
C     This entry point initializes a search progress report.
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
C     GF
C
C$ Keywords
C
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      WINDOW ( LBCELL : * )
C     CHARACTER*(*)         BEGMSS
C     CHARACTER*(*)         ENDMSS
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LBCELL     P   The SPICELIB cell lower bound.
C     MXBEGM     P   Maximum progress report message prefix length.
C     MXENDM     P   Maximum progress report message suffix length.
C     WINDOW     I   A window over which a job is to be performed.
C     BEGMSS     I   Beginning of the text portion of the output message
C     ENDMSS     I   End of the text portion of the output message
C
C$ Detailed_Input
C
C     WINDOW   is the name of a constraint window. This is the window
C              associated with some root finding activity. It is
C              used to determine how much total time is being searched
C              in order to find the events of interest.
C
C     BEGMSS   is the beginning of the output message reported by the
C              routine GFRPWK.  This output message has the form
C
C                 BEGMSS(1:LASTNB(BEGMSS)) // ' xx.xx% ' // ENDMSS
C
C              BEGMSS must have length not greater than MXBEGM
C              characters. All characters of BEGMSS must be printable.
C
C     ENDMSS   is the last portion of the output message reported by
C              the routine GFRPWK.
C
C              ENDMSS must have length not greater than MXBENM
C              characters. All characters of ENDMSS must be printable.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     LBCELL     is the SPICELIB cell lower bound.
C
C     MXBEGM,
C     MXENDM    are, respectively, the maximum lengths of the progress
C               report message prefix and suffix. See the INCLUDE file
C               zzgf.inc for details.
C
C$ Exceptions
C
C     1) If BEGMSS has length greater than MXBEGM characters, or if
C        ENDMSS has length greater than MXENDM characters, the error
C        SPICE(MESSAGETOOLONG) is signaled.
C
C     2) If either BEGMSS or ENDMSS contains non-printing characters,
C        the error SPICE(NOTPRINTABLECHARS) is signaled.
C     
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point initializes the GF progress reporting system. It
C     is called by the GF root finding utilities once at the start of
C     each search pass. See the Particulars section of the main
C     subroutine header for further details of its function.
C
C$ Examples
C
C     See the header of the umbrella routine GFRPRT.
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
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1 10-FEB-2014 (BVS)
C
C        Corrected declaration of WINDOW in the Declarations
C        section. Added description of LBCELL to the Declarations,
C        Brief_I/O, and Parameters sections.
C
C-    SPICELIB Version 1.0.0 21-FEB-2009 (NJB) (LSE) (WLT) (IMU)
C
C-&

C$ Index_Entries
C
C     GF initialize a progress report
C
C-& 
C
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF  

      CALL CHKIN  ( 'GFREPI' ) 
 
C
C     Check to see if either the message prefix or suffix
C     is too long.
C
      IF ( LASTNB(BEGMSS) .GT. MXBEGM ) THEN
 
         CALL SETMSG ( 'Progress report prefix message contains '
     .   //            '# characters; limit is #.'                 )
         CALL ERRINT ( '#', LASTNB(BEGMSS)                         )
         CALL ERRINT ( '#', MXBEGM                                 )
         CALL SIGERR ( 'SPICE(MESSAGETOOLONG)'                     )
         CALL CHKOUT ( 'GFREPI'                                    )
         RETURN
 
      END IF

      IF ( LASTNB(ENDMSS) .GT. MXENDM ) THEN
 
         CALL SETMSG ( 'Progress report suffix message contains '
     .   //            '# characters; limit is #.'                 )
         CALL ERRINT ( '#', LASTNB(ENDMSS)                         )
         CALL ERRINT ( '#', MXENDM                                 )
         CALL SIGERR ( 'SPICE(MESSAGETOOLONG)'                     )
         CALL CHKOUT ( 'GFREPI'                                    )
         RETURN
 
      END IF

C
C     Now check that all the characters in the message prefix and
C     suffix can be printed.
C
      DO I = 1, LASTNB(BEGMSS)
 
         CHRCOD = ICHAR( BEGMSS(I:I) )
 
         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The progress report message prefix '  
     .      //            'contains a nonprintable character; '     
     .      //            'ASCII code is #.'                    )
            CALL ERRINT ( '#', CHRCOD                           )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'            )
            CALL CHKOUT ( 'GFREPI'                              )
            RETURN
 
         END IF
 
      END DO

      DO I = 1, LASTNB(ENDMSS)
 
         CHRCOD = ICHAR( ENDMSS(I:I) )
 
         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN
 
            CALL SETMSG ( 'The progress report message suffix '  
     .      //            'contains a nonprintable character; '     
     .      //            'ASCII code is #.'                    )
            CALL ERRINT ( '#', CHRCOD                           )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'            )
            CALL CHKOUT ( 'GFREPI'                              )
            RETURN
 
         END IF
 
      END DO


      COPYB = BEGMSS
      COPYE = ENDMSS

C
C     Find the length of the window. Use that to initialize the work
C     reporter.
C
      CALL WNSUMD   ( WINDOW, MEASUR, AVE, STDDEV, SHORT, LONG )
      CALL ZZGFTSWK ( MEASUR, 1.0D0,  4,   BEGMSS, ENDMSS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'GFREPI' )
         RETURN
      END IF

  
C
C     Initialize the time to the start of the confinement window.
C     The remaining amount of work in the current interval is
C     the measure of the interval.
C
      IF ( CARDD(WINDOW) .GE. 2 ) THEN

         T0     = WINDOW(1)
         REMAIN = WINDOW(2) - T0

      ELSE

         REMAIN = 0.D0

      END IF



      CALL CHKOUT  ( 'GFREPI' )
      RETURN
 
 
 

 
C$Procedure   GFREPU ( GF, progress report update )
 
      ENTRY GFREPU ( IVBEG, IVEND, TIME )
 
C$ Abstract
C
C     This entry point tells the progress reporting system 
C     how far a search has progressed.
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
C     GF
C
C$ Keywords
C
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      IVBEG
C     DOUBLE PRECISION      IVEND
C     DOUBLE PRECISION      TIME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IVBEG      I   Start time of work interval.
C     IVEND      I   End time of work interval.
C     TIME       I   Current time being examined in the search process
C
C$ Detailed_Input
C
C     IVBEG,
C     IVEND    are the bounds of an interval that is contained in some
C              interval belonging to the confinement window. The
C              confinement window is associated with some root finding
C              activity. It is used to determine how much total time is
C              being searched in order to find the events of interest.
C
C              In order for a meaningful progress report to be
C              displayed, IVBEG and IVEND must satisfy the following
C              constraints:
C
C                 - IVBEG must be less than or equal to IVEND.
C
C                 - The interval [ IVBEG, IVEND ] must be contained in
C                   some interval of the confinement window. It can be
C                   a proper subset of the containing interval; that
C                   is, it can be smaller than the interval of the
C                   confinement window that contains it.
C
C                 - Over a search pass, the sum of the differences
C
C                      IVEND - IVBEG
C
C                   for all calls to this routine made during the pass
C                   must equal the measure of the confinement window.
C
C
C     TIME     is the current time reached in the search for an event.
C              TIME must lie in the interval 
C
C                 IVBEG : IVEND
C
C              inclusive. The input values of TIME for a given interval
C              need not form an increasing sequence.
C
C$ Detailed_Output
C
C     None. This routine does perform console I/O when progress
C     reporting is enabled.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If IVBEG and IVEND are in decreasing order, the error
C        SPICE(BADENDPOINTS) is signaled.
C
C     2) If TIME is not in the closed interval [IVBEG, IVEND], the
C        error SPICE(VALUEOUTOFRANGE) is signaled.
C
C     3) Any I/O errors resulting from writing to standard output
C        will be diagnosed by routines in the call tree of this
C        routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used to indicate the current progress of a
C     search. Using information recorded through the initialization
C     entry point of this routine, the progress reporting system
C     determines how much work has been completed and whether or not to
C     report it on the users screen.
C
C$ Examples
C
C     See the header of the umbrella routine GFRPRT.
C
C$ Restrictions
C
C     This routine has no way of enforcing that the input values of
C     IVBEG and IVEND are compatible with the input window passed to
C     GFREPI. Callers of this routine are responsible for ensuring
C     that this requirement is obeyed.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1 10-FEB-2014 (BVS)
C
C        Added declarations of IVBEG and IVEND to the Declarations
C        section.
C
C-    SPICELIB Version 1.0.0 21-FEB-2009 (NJB) (LSE) (WLT) (IMU)
C
C-&

C$ Index_Entries
C
C     GF update a progress report
C
C-& 

      IF ( RETURN () ) THEN
         RETURN
      END IF  

      CALL CHKIN  ( 'GFREPU' ) 

C
C     Do a few error checks before getting started.
C
C     We expect the endpoints of the current window to be in order.
C
      IF ( IVEND .LT. IVBEG ) THEN

         CALL SETMSG ( 'Interval endpoints are #:#; endpoints '   
     .   //            'must be in increasing order.'             )
         CALL ERRDP  ( '#',  IVBEG                                )
         CALL ERRDP  ( '#',  IVEND                                )
         CALL SIGERR ( 'SPICE(BADENDPOINTS)'                      )
         CALL CHKOUT ( 'GFREPU'                                   )
         RETURN
      END IF

C
C     We expect TIME to be in the current interval of the confinement
C     window.
C
      IF (  ( TIME .LT. IVBEG ) .OR. ( TIME .GT. IVEND )  ) THEN

         CALL SETMSG ( 'TIME should be in interval #:# but ' 
     .   //            'is #.'                               )
         CALL ERRDP  ( '#',  TIME                            )
         CALL ERRDP  ( '#',  IVBEG                           )
         CALL ERRDP  ( '#',  IVEND                           )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'              )
         CALL CHKOUT ( 'GFREPU'                              )
         RETURN
      END IF

C
C     The amount of work done is the difference between the current
C     time and the previous time T0, presuming both times are in 
C     the current interval.  Note this work amount may be negative.  
C
      IF (  ( T0 .GE. IVBEG ) .AND. ( T0 .LE. IVEND )  ) THEN

         INCR = TIME - T0

      ELSE
C
C        T0 is in the previous interval.  The amount of work 
C        done to complete processing of that interval is REMAIN.
C        The amount of work done in the current interval is
C        the difference of TIME and the left endpoint of the
C        interval.
C
         INCR = REMAIN + TIME - IVBEG

      END IF

C
C     The remaining work is the distance from TIME to the right
C     endpoint of the current interval.
C
      REMAIN = IVEND - TIME

C
C     Record the current time as T0.
C
      T0  =  TIME

C
C     Report the work increment.
C
      CALL ZZGFWKIN ( INCR )
 
      CALL CHKOUT  ( 'GFREPU' ) 
      RETURN

 
C$Procedure      GFREPF ( GF, progress report finalization )
 
      ENTRY GFREPF
 
C$ Abstract
C
C     Finish a progress report.
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
C     GF
C
C$ Keywords
C
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None. This routine does perform console I/O when progress
C     reporting is enabled.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any I/O errors resulting from writing to standard output
C        will be diagnosed by routines in the call tree of this
C        routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point "finishes" a progress report, i.e. updates the
C     report to indicate the underlying task is 100% complete.
C
C$ Examples
C
C     See the header of the umbrella routine GFRPRT.
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
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 21-FEB-2009 (NJB) (LSE) (WLT) (IMU)
C
C-&

C$ Index_Entries
C
C     GF finish a progress report
C
C-&  

      IF ( RETURN () ) THEN
         RETURN 
      END IF  

      CALL CHKIN  ( 'GFREPF' ) 

      CALL ZZGFWKAD ( 0.0D0, 1, COPYB, COPYE )
      CALL ZZGFWKIN ( 0.0D0 )

C
C     Determine whether progress report output is currently
C     being sent to standard output. Fetch the output unit.
C
      CALL ZZGFWKMO ( UNIT, TOTAL, FREQ, TCHECK, BEGIN, END, INCR )

      CALL STDIO ( 'STDOUT', STDOUT )
 
      IF ( UNIT .NE. STDOUT ) THEN
C
C        We're not currently writing to standard output, so we're
C        done.
C
         CALL CHKOUT ( 'GFREPF' )
         RETURN

      END IF

C
C     Emit a final blank line by moving the cursor down two
C     spaces.
C
C     The set of actual arguments passed here is rather funky
C     and deserves some explanation:
C
C        The first argument, calling for a leading blank line, moves
C        the cursor down so that the next blank line written won't
C        overwrite the final status message. That blank line is
C        followed with a cursor repositioning command that moves the
C        cursor to the beginning of the line that was just written. The
C        last argument, calling for another blank line, moves the
C        cursor down again. The total cursor movement is down 2 lines.
C        This results in one skipped line.
C
C     We could accomplish the same results more simply if were
C     were to use I/O statements in this routine; however, in the
C     interest of minimizing the number of places where I/O is
C     performed, we rely on ZZGFDSPS to do that job.
C
      CALL ZZGFDSPS  ( 1, ' ', 'A', 1 )


      CALL CHKOUT  ( 'GFREPF' ) 
      RETURN
      END
