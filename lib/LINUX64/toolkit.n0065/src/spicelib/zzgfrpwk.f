C$Procedure    ZZGFRPWK ( Geometry finder report work done on a task )
 
      SUBROUTINE ZZGFRPWK ( UNIT,   TOTAL, FREQ, 
     .                      TCHECK, BEGIN, END, INCR )
 
C$ Abstract
C
C     The entry points under this routine allows one to easily monitor
C     the status of job in progress.
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
C     UTILITY
C     REPORT
C     WORK
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzgf.inc'

      INTEGER               UNIT
      DOUBLE PRECISION      TOTAL
      DOUBLE PRECISION      FREQ
      INTEGER               TCHECK
      CHARACTER*(*)         BEGIN
      CHARACTER*(*)         END
      DOUBLE PRECISION      INCR
 
C$ Brief_I/O
C
C     VARIABLE  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     UNIT      I-O  ZZGFWKUN, ZZGFWKMO
C     TOTAL     I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
C     FREQ      I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
C     TCHECK    I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
C     BEGIN     I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
C     END       I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO
C     INCR      I-O  ZZGFWKIN, ZZGFWKMO
C
C$ Detailed_Input
C
C     See the headers of the entry points.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     MXBEGM,
C     MXENDM,
C     MXMSG     are, respectively, the maximum lengths of the progress
C               message prefix, progress message suffix, and the
C               complete message.
C
C$ Exceptions
C
C     If this routine is called directly, the error SPICE(BOGUSENTRY)
C     is signaled.
C
C     See the entry points for descriptions of exceptions they detect.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The entry points under this routine are designed to allow one to
C     easily build into his/her application a monitoring facility
C     that reports how work on a particular task is proceeding.
C
C     There are five entry points: ZZGFTSWK, ZZGFWKIN, ZZGFWKAD,
C     ZZGFWKUN, and ZZGFWKMO.
C
C     The first entry point ZZGFTSWK is used to initialize the reporter.
C     It is used to tell the reporter "I have some work to do.  This is
C     how much, and this is how often I want you to report on the
C     progress of the task."
C
C     The second entry point ZZGFWKIN is used to tell the reporter "I've
C     just finished some of the task I told you about with ZZGFTSWK.
C     This is how much I've just done."  (As in real life, the amount
C     of work you've just done can be negative.)  The reporter uses
C     this information together with the information input in ZZGFTSWK
C     to decide whether and how much work to report as finished.  The
C     reports will be sent to the current output device.
C
C     The third entry point, ZZGFWKAD, adjusts the frequency with which
C     work progress is reported.
C
C     The fourth entry point ZZGFWKUN also is used for testing. It is
C     used to send the output to the file connected to a specified
C     logical unit.
C
C     The fifth entry point ZZGFWKMO is used for testing. It returns
C     the saved search parameters.
C
C     A more detailed description of each entry point is provided in its
C     associated header.
C
C$ Examples
C
C     A typical use of ZZGFRPWK might be as follows.
C
C
C     C
C     C     Compute how much work is to be done and put it in TOTAL
C     C
C
C           code
C           computing
C           how
C           much
C           work
C           to
C           do
C            .
C            .
C            .
C           TOTAL     = <the amount of work to do>
C
C     C
C     C     Tell the work reporter to report work completed every
C     C     3 seconds. (The third argument in ZZGFTSWK is explained 
C     C     in the header for ZZGFTSWK.)
C     C
C           FREQUENCY = 3.0D0
C           BEGIN     = 'Current work status: '
C           END       = 'completed. '
C
C           CALL ZZGFTSWK ( TOTAL, FREQUENCY, 1, BEGIN, END )
C
C           DO WHILE ( THERE_IS_MORE_WORK_TO_DO )
C
C              code that
C              performs
C              the work to
C              be done
C
C              AMOUNT = amount of work done in this loop pass
C
C              CALL ZZGFWKIN ( AMOUNT )
C
C           END DO
C
C
C$ Restrictions
C
C      You can use this routine to report progress on only one task at
C      a time.  The work reporter must be initialized using ZZGFTSWK
C      before calling ZZGFWKIN.  Failure to do this may lead to 
C      unexpected results.
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
C-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
C
C-&

C$ Index_Entries
C
C     GF low-level progress report umbrella
C
C-&

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      BRCKTD
      INTEGER               RTRIM
      LOGICAL               RETURN

C
C     Local variables
C  
      CHARACTER*10          PRCENT
      CHARACTER*(MXENDM)    FINISH
      CHARACTER*(MXBEGM)    START
      CHARACTER*(MXMSG)     MESSGE

      DOUBLE PRECISION      CURSEC
      DOUBLE PRECISION      DONE
      DOUBLE PRECISION      ENTIRE
      DOUBLE PRECISION      FRACTN
      DOUBLE PRECISION      SVINCR
      DOUBLE PRECISION      LSTSEC
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TVEC ( 6 )

      INTEGER               CALLS
      INTEGER               CHECK
      INTEGER               LS
      INTEGER               STDOUT
      INTEGER               SVUNIT

      LOGICAL               FIRST

C
C     Saved variables
C
      SAVE                  CALLS
      SAVE                  CHECK
      SAVE                  DONE
      SAVE                  ENTIRE
      SAVE                  FINISH
      SAVE                  FIRST
      SAVE                  LS
      SAVE                  LSTSEC
      SAVE                  START
      SAVE                  STDOUT
      SAVE                  STEP
      SAVE                  SVINCR
      SAVE                  SVUNIT

C
C     Initial values
C
      DATA                  CALLS  / 0       /
      DATA                  CHECK  / 1       /
      DATA                  DONE   / 0.D0    /
      DATA                  ENTIRE / 0.D0    /
      DATA                  FINISH / ' '     /
      DATA                  FIRST  / .TRUE.  /
      DATA                  LS     / 1       /
      DATA                  LSTSEC / 0.D0    /
      DATA                  START  / ' '     /
      DATA                  STDOUT / 6       /
      DATA                  STEP   / 0.D0    /
      DATA                  SVINCR / 0.D0    /
      DATA                  SVUNIT / 6       /


      CALL CHKIN  ( 'ZZGFRPWK'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFRPWK'          )
      RETURN




 
C$Procedure ZZGFTSWK ( Geometry finder total sum of work to be done. )
 
      ENTRY ZZGFTSWK ( TOTAL, FREQ, TCHECK, BEGIN, END )
 
C$ Abstract
C
C     Initialize the work progress utility. This is required prior to
C     use of the routine that performs the actual reporting.
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
C     UTILITY
C     REPORT
C     WORK
C
C$ Declarations
C
C     DOUBLE PRECISION      TOTAL
C     DOUBLE PRECISION      FREQ
C     INTEGER               TCHECK
C     CHARACTER*(*)         BEGIN
C     CHARACTER*(*)         END
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TOTAL      I   A measure of the total amount of work to be done.
C     FREQ       I   How often the work progress should be reported.
C     TCHECK     I   How often to sample the system clock.
C     BEGIN      I   First part of the output message.
C     END        I   Last part of the output message.
C
C$ Detailed_Input
C
C     UNIT       is a logical unit connected to the output stream
C                to which the progress report should be sent.
C                Normally UNIT is set to the standard output unit,
C                which can be obtained by calling the SPICELIB
C                routine STDIO. Unit can be a logical unit connected
C                to a file; this feature supports testing.
C
C     TOTAL      is a measure of the total amount of work to be done
C                by the routine(s) that will be using this facility.
C                It is expected (but not required) that TOTAL is a
C                positive number.
C
C     FREQ       is the how often the work progress should be reported
C                in seconds.  If FREQ = 5 then a work progress report
C                will be sent to the output device approximately every
C                5 seconds.  Since writing to the output device takes
C                time, the smaller FREQ is set, the greater the overhead
C                taken up by the work reporter will be. ( A value of 2
C                or greater should not burden your application
C                appreciably )
C
C     TCHECK     is an integer used to the tell the reporter how often
C                to sample the system clock.  If TCHECK = 7, then on
C                every seventh call to ZZGFWKIN, the system clock will
C                be sampled to determine if FREQ seconds have elapsed
C                since the last report time.  Sampling the system clock
C                takes time. Not a lot of time, but it does take time.
C                If ZZGFWKIN is being called from a loop that does not
C                take a lot of time for each pass, the sampling of
C                the system clock can become a significant overhead
C                cost in itself.  On the VAX the sampling of the
C                system clock used here takes about 37 double precision
C                multiplies.  If thousands of multiplies take place
C                between calls to ZZGFWKIN, the sampling time is
C                insignificant.  On the other hand, if only a hundred or
C                so multiplies occur between calls to ZZGFWKIN, the
C                sampling of the system clock can become a significant
C                fraction of your overhead.  TCHECK allows you to
C                tailor the work reporter to your application.
C
C                If a non-positive value for TCHECK is entered, a value
C                of 1 will be used instead of the input value.
C
C      BEGIN     Is the first part of the output message that will be
C                constructed for shipment to the output device. This
C                message will have the form:
C
C                BEGIN // xx.x% // END
C
C                where xx.x  is the percentage of the job completed when
C                the output message is sent to the output device.
C
C      END       is the second part of the output message that will be
C                constructed and sent to the output device (see above).
C
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Standard SPICE error handling.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used to initialize parameters that will
C     be used by ZZGFWKIN.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     See the header for this module
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
C-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
C
C-&     

C$ Index_Entries
C
C     GF low-level initialize progress report
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN  ( 'ZZGFTSWK' )

C
C     On the first pass, obtain the logical unit for
C     standard output.
C
      IF ( FIRST ) THEN

         CALL STDIO ( 'STDOUT', STDOUT )

C
C        The output unit is STDOUT unless the caller
C        sets it to something else.
C
         SVUNIT = STDOUT

         FIRST  = .FALSE.

      END IF

C
C     Save the inputs and set the amount of work done to 0
C
      ENTIRE = TOTAL
      STEP   = MIN ( 3600.0D0, MAX( 0.0D0, FREQ ) )
      CHECK  = MAX ( 1, TCHECK )
               
      START  = BEGIN
      FINISH = END
 
      DONE   = 0.0D0
 
C
C     Set the timer.
C
      CALL ZZCPUTIM ( TVEC )

      LSTSEC = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)
 
C
C     Set the increment counter
C
      CALLS  = 0
       
C
C     Compose the output message.
C 
      LS            = RTRIM ( START  )
      MESSGE        = START ( 1 : LS )  //
     .                ' '               //
     .                '  0.00%'         //
     .                ' '               //
     .                FINISH
 
C
C     Display a blank line, make sure we don't overwrite anything 
C     at the bottom of the screen. The display the message.
C 
      IF ( SVUNIT .EQ. STDOUT ) THEN

         CALL ZZGFDSPS ( 1, MESSGE, 'A', 0 )

      ELSE
C
C        Write the message without special carriage control.
C
         CALL WRITLN ( ' ',    SVUNIT )
         CALL WRITLN ( ' ',    SVUNIT )
         CALL WRITLN ( MESSGE, SVUNIT )

      END IF     

      CALL CHKOUT  ( 'ZZGFTSWK' )
      RETURN


 
C$Procedure ZZGFWKIN ( Geometry finder work finished increment )
 
      ENTRY ZZGFWKIN ( INCR )
 
C$ Abstract
C
C     Let the work reporter know that an increment of work has just
C     been completed.
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
C     UTILITY
C     REPORT
C     WORK
C
C$ Declarations
C
C     DOUBLE PRECISION      INCR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INCR       I   An amount of work just completed.
C
C$ Detailed_Input
C
C     INCR       is some amount of work that has been completed since
C                the last call to ZZGFWKIN.
C
C$ Detailed_Output
C
C      None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Standard SPICE error handling.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point is used to report work that has been done since
C     initialization was performed using ZZGFTSWK or since the last
C     call to ZZGFWKIN.  The work reporter uses this information
C     together with samples of the system clock to report how much of
C     the total job has been completed.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     See the header for this module
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
C-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
C
C-&

C$ Index_Entries
C
C     ZZGF low-level progress report increment
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN  ( 'ZZGFWKIN' ) 

      SVINCR = INCR
      DONE   = DONE  + INCR               
      CALLS  = CALLS + 1
 
      IF ( ENTIRE .EQ. 0 ) THEN
         CALL CHKOUT  ( 'ZZGFWKIN' )
         RETURN
      END IF
          
      IF ( CALLS .GE. CHECK ) THEN
 
         CALLS  = 0

         CALL ZZCPUTIM ( TVEC )

         CURSEC = TVEC(4)*3600.0D0 + TVEC(5)*60.0D0 + TVEC(6)
            
 
         IF ( DABS( CURSEC - LSTSEC ) .GE. STEP ) THEN
           
            LSTSEC = CURSEC 
C
C           Report how much work has been done.
C
            FRACTN = BRCKTD ( ( DONE/ENTIRE )*100.0D0,  0.D0,  100.D0 )
 
            CALL DPFMT ( FRACTN, 'xxx.xx', PRCENT )
 
            PRCENT(7:7) = '%'
 
            MESSGE        = START  (1:LS)  //
     .                      ' '            //
     .                      PRCENT (1:7)   //
     .                      ' '            //
     .                      FINISH(1:RTRIM(FINISH))
 

            IF ( SVUNIT .EQ. STDOUT ) THEN

               CALL ZZGFDSPS ( 0, MESSGE, 'A', 0 )

            ELSE
C
C              Write the message without special carriage control.
C
               CALL WRITLN ( MESSGE, SVUNIT )

            END IF     
 
         END IF
 
      END IF

      CALL CHKOUT  ( 'ZZGFWKIN' ) 
      RETURN 


 
C$Procedure ZZGFWKAD ( Geometry finder work reporting adjustment )
 
      ENTRY ZZGFWKAD ( FREQ, TCHECK, BEGIN, END )
 
C$ Abstract
C
C     Adjust the frequency with which work progress is reported.
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
C     UTILITY
C     REPORT
C     WORK
C
C$ Declarations
C
C     DOUBLE PRECISION      FREQ
C     INTEGER               TCHECK
C     CHARACTER*(*)         BEGIN
C     CHARACTER*(*)         END
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TOTAL      I   A measure of the total amount of work to be done.
C     FREQ       I   How often the work progress should be reported.
C     BEGIN      I   First part of the output message.
C     END        I   Last part of the output message.
C
C$ Detailed_Input
C
C     FREQ       is the how often the work progress should be reported
C                in seconds.  If FREQ = 5 then a work progress report
C                will be sent to the output device approximately every
C                5 seconds.  Since writing to the output device takes
C                time, the smaller FREQ is set, the greater the overhead
C                taken up by the work reporter will be. ( A value of 2
C                or greater should not burden your application
C                appreciably )
C
C     TCHECK     is an integer used to the tell the reporter how often
C                to sample the system clock.  If TCHECK = 7, then on
C                every seventh call to ZZGFWKIN, the system clock will
C                be sampled to determine if FREQ seconds have elapsed
C                since the last report time.  Sampling the system clock
C                takes time. Not a lot of time, but it does take time.
C                If ZZGFWKIN is being called from a loop that does not
C                take a lot of time for each pass, the sampling of
C                the system clock can become a significant overhead
C                cost in itself.  On the VAX the sampling of the
C                system clock used here takes about 37 double precision
C                multiplies.  If thousands of multiplies take place
C                between calls to ZZGFWKIN, the sampling time is
C                insignificant.  On the other hand, if only a hundred or
C                so multiplies occur between calls to ZZGFWKIN, the
C                sampling of the system clock can become a significant
C                fraction of your overhead.  TCHECK allows you to
C                tailor the work reporter to your application.
C
C                If a non-positive value for TCHECK is entered, a value
C                of 1 will be used instead of the input value.
C
C
C     BEGIN      Is the first part of the output message that will be
C                constructed for shipment to the output device. This
C                message will have the form:
C
C                BEGIN // xx.x% // END
C
C                where xx.x  is the percentage of the job completed when
C                the output message is sent to the output device.
C
C     END        is the second part of the output message that will be
C                constructed and sent to the output device (see above).
C
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
C     Error free. 
C
C     1) If TCHECK is less than 1, the value 1 is stored.
C
C     2) If FREQ is less than 0.1, the value 0.1 is stored.
C        If FREQ is greater than 3600, the value 3600 is stored.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point exists to modify the reporting frequency set
C     up by an initial call to ZZGFTSWK.  In this way one can override
C     how often reporting of work increments is performed, without
C     causing the screen to be modified (which happens if a new
C     call to  ZZGFTSWK is made.)
C
C     It exists primarily as a back door to existing code
C     that calls ZZGFTSWK in a rigid way.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     See the header for this module.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     L.S. Elson     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU)
C
C-&


C$ Index_Entries
C
C     GF low-level progress report adjust frequency
C
C-&

      STEP   = MIN  ( 3600.0D0,  MAX( 0.D0, FREQ ) )
      CHECK  = MAX  ( 1, TCHECK )
      START  = BEGIN
      FINISH = END
 
      RETURN





 
C$Procedure ZZGFWUN ( Geometry finder set work report output unit )
 
      ENTRY ZZGFWKUN ( UNIT )
 
C$ Abstract
C
C     Set the output unit for the progress report.
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
C     UTILITY
C     REPORT
C     WORK
C
C$ Declarations
C
C     INTEGER               UNIT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Output logical unit.
C
C$ Detailed_Input
C
C     UNIT           Logical unit of a text file open for write access.
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
C     Error free. 
C
C$ Files
C
C     The file designated by UNIT should be a text file opened by the
C     calling application.
C
C$ Particulars
C
C     This routine can be called before ZZGFTSWK to set the output
C     logical unit to that of a text file.
C
C     This entry point exists to support testing of the higher-level
C     GF progress reporting routines
C
C        GFREPI
C        GFREPU
C        GFREPF
C
C     This routine enables TSPICE to send the output report to
C     a specified file.
C
C$ Examples
C
C     None.
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB)
C
C-&


C$ Index_Entries
C
C     GF low-level progress report output select unit
C
C-&


C
C     On the first pass, obtain the logical unit for
C     standard output.
C
      IF ( FIRST ) THEN

         CALL STDIO ( 'STDOUT', STDOUT )

         FIRST  = .FALSE.

      END IF

      SVUNIT = UNIT

      RETURN




 
C$Procedure ZZGFWKMO ( Geometry finder work reporting monitor )
 
      ENTRY ZZGFWKMO ( UNIT, TOTAL, FREQ, TCHECK, BEGIN, END, INCR )
 
C$ Abstract
C
C     Return saved progress report parameters.
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
C     UTILITY
C     REPORT
C     WORK
C
C$ Declarations
C
C     INTEGER               UNIT
C     DOUBLE PRECISION      TOTAL
C     DOUBLE PRECISION      FREQ
C     INTEGER               TCHECK
C     CHARACTER*(*)         BEGIN
C     CHARACTER*(*)         END
C     DOUBLE PRECISION      INCR
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       O   Output logical unit.
C     TOTAL      O   A measure of the total amount of work to be done.
C     FREQ       O   How often the work progress should be reported.
C     TCHECK     O   Number of calls between system time check.
C     BEGIN      O   First part of the output message.
C     END        O   Last part of the output message.
C     INCR       O   Last progress increment.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     UNIT,
C     TOTAL,
C     FREQ,     
C     TCHECK,      
C     BEGIN,     
C     END,
C     INCR           are the most recent values of these
C                    variables passed in via calls to ZZGFTSWK,
C                    ZZGFWKIN, or ZZGFWKAD.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free. 
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This entry point exists to support testing of the higher-level
C     GF progress reporting routines
C
C        GFREPI
C        GFREPU
C        GFREPF
C
C     This routine enables TSPICE to determine the values passed
C     in to entry points of this package by those routines.
C
C$ Examples
C
C     None.
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
C
C$ Version
C
C-    SPICELIB Version 1.0.0 17-FEB-2009 (NJB)
C
C-&


C$ Index_Entries
C
C     GF low-level progress report monitor
C
C-&

      UNIT   = SVUNIT
      TOTAL  = ENTIRE
      FREQ   = STEP
      TCHECK = CHECK
      BEGIN  = START
      END    = FINISH
      INCR   = SVINCR

      RETURN
      END


 
 
