C$Procedure SPKS19 ( S/P Kernel, subset, type 19 )
 
      SUBROUTINE SPKS19 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in an SPK segment of type 19
C     into a new segment.
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
C     SPK
C     DAF
C
C$ Keywords
C
C     EPHEMERIS
C     SPK
C
C$ Declarations

      INCLUDE 'spk19.inc'
 
      INTEGER               HANDLE
      INTEGER               BADDR
      INTEGER               EADDR
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of file containing source segment.
C     BADDR      I   Beginning address in file of source segment.
C     EADDR      I   Ending address in file of source segment.
C     BEGIN      I   Beginning (initial epoch) of subset.
C     END        I   End (final epoch) of subset.
C
C$ Detailed_Input
C
C     HANDLE,
C     BADDR,
C     EADDR       are the file handle assigned to an SPK file, and the
C                 beginning and ending addresses of a segment within
C                 that file. Together these identify an SPK segment
C                 from which a subset is to be extracted. 
C
C                 The subset is written to a second SPK file which is
C                 open for writing, and in which a new segment has been
C                 started. See the Particulars section below for
C                 details.
C
C     BEGIN,
C     END         are the initial and final epochs (ephemeris time)
C                 of the subset.
C
C                 The first epoch for which there will be ephemeris
C                 data in the new segment will be the greatest time
C                 in the source segment that is less than or equal
C                 to BEGIN.
C
C                 The last epoch for which there will be ephemeris
C                 data in the new segment will be the smallest time
C                 in the source segment that is greater than or equal
C                 to END.
C
C$ Detailed_Output
C
C     None. See $Files section.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine relies on the caller to ensure that the
C         interval [BEGIN, END] is contained in the coverage
C         interval of the source segment.
C
C     2)  If BEGIN > END, no data are written to the target file.
C
C     3)  If a unexpected SPK type 19 subtype is found in the input
C         segment, the error SPICE(INVALIDVALUE) is signaled.
C
C     4)  The input segment must have valid structure; this 
C         routine may fail in unpredictable ways if not.
C
C$ Files
C
C     Data are extracted from the file connected to the input
C     handle, and written to the current DAF open for writing.
C
C     The segment descriptor and summary must already have been written
C     prior to calling this routine.  The segment must be ended
C     external to this routine.
C
C$ Particulars
C
C     This routine is intended solely for use as a utility by the
C     routine SPKSUB.
C
C     It transfers a subset of a type 19 SPK data segment to
C     a properly initialized segment of a second SPK file.
C
C     The exact structure of a segment of data type 19 is described
C     in the section on type 19 in the SPK Required Reading.
C
C$ Examples
C
C     This routine is intended only for use as a utility by SPKSUB.
C     To use this routine successfully, you must:
C
C        Open the SPK file from which to extract data.
C        Locate the segment from which data should be extracted.
C
C        Open the SPK file to which this data should be written.
C        Begin a new segment (array).
C        Write the summary information for the array.
C
C        Call this routine to extract the appropriate data from the
C        SPK open for read.
C
C        End the array to which this routine writes data.
C
C     Much of this procedure is carried out by the routine SPKSUB.  The
C     examples of that routine illustrate more fully the process
C     described above.
C
C$ Restrictions
C
C     1) This routine relies on the input segment being correct;
C        very limited error checking is performed on the input
C        data.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 17-OCT-2011 (NJB) (BVS) (WLT) (IMU) (EDW)
C
C-&
 
 
C$ Index_Entries
C
C     subset type_19 spk_segment
C
C-&
 
C$ Revisions
C
C     None.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               LSTLED
      INTEGER               LSTLTD

      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

C
C     Mini-segment control area size:
C
      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 3 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

C
C     Local variables
C
      CHARACTER*(FILSIZ)    SPK

      DOUBLE PRECISION      CONTRL   ( CTRLSZ )
      DOUBLE PRECISION      DATA     ( BUFSIZ )
      DOUBLE PRECISION      IV1BEG
      DOUBLE PRECISION      IV1END
      DOUBLE PRECISION      IVFBEG
      DOUBLE PRECISION      IVFEND
      DOUBLE PRECISION      IVLBEG
      DOUBLE PRECISION      IVLEND
      
      INTEGER               BEGIDX
      INTEGER               BEPIDX
      INTEGER               BUFBAS
      INTEGER               CURIVL
      INTEGER               EEPIDX
      INTEGER               ENDIDX
      INTEGER               I
      INTEGER               ISEL
      INTEGER               IVLBAS
      INTEGER               L
      INTEGER               MIN1SZ
      INTEGER               MINBEP
      INTEGER               MINFSZ
      INTEGER               MINIB
      INTEGER               MINIE
      INTEGER               MINNDR
      INTEGER               MINNPK
      INTEGER               NDIR
      INTEGER               NINTVL
      INTEGER               NOIVL
      INTEGER               NPAD
      INTEGER               NPKT
      INTEGER               NREAD
      INTEGER               NSDIR
      INTEGER               PKTSIZ
      INTEGER               PTRBAS
      INTEGER               REMAIN
      INTEGER               SHIFT
      INTEGER               START
      INTEGER               SUBTYP
      INTEGER               UB
      INTEGER               WNDSIZ

      LOGICAL               FINAL
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKS19' )

C
C     Terminology
C     ===========
C
C       - A point P is in the "interior" of a set S if P is 
C         an element of S and P is not contained in the boundary
C         of S. If S is a discrete set of distinct times, then
C         the interior points of S are greater than the earliest
C         time in S and earlier than the latest time in S. If 
C         S is the closed interval [A, B], that is, if S is the set of
C         points P such that 
C
C            A  <  P  <  B
C               -     -
C
C         then the interior of S is the set of points P such that
C
C            A  <  P  <  B
C
C       - A subset S2 of a set S1 is in the "interior" of S1 if 
C         every point of S2 is contained in the interior of S1.
C
C       - SPK type 19 interpolation intervals are often simply
C         called "intervals." 
C
C       - The data set corresponding to a type 19 interpolation 
C         interval is called a "mini-segment."
C        
C       - "Padding" consists of a sequence of contiguous data packets
C         and a corresponding sequence of epochs provided to enable
C         correct interpolation near interval boundaries, where the
C         epochs lie outside of the interval's coverage time range.
C         Padding data and epochs are always drawn only from the same
C         input mini-segment that provides data for the output
C         mini-segment under construction.
C
C       - A "base address" of a structure is the DAF address preceding
C         the first address occupied by the structure.
C
C
C     Algorithm
C     =========
C
C     The algorithm below transfers to the output segment sufficient
C     data to cover the time range BEGIN : END, such that the output
C     segment yields interpolation behavior identical to that of the
C     selected portion of the input segment.
C
C     No use is made of the selection order attribute other than to
C     transfer it to the output segment. This simplifies the algorithm,
C     at the expense of making the output segment larger than necessary
C     by at most a small, bounded amount. Specifically, when either
C     BEGIN or END coincides with an interior interval boundary, a
C     small additional output interval is created so as to make that
C     boundary an interior point of the output segment's coverage
C     interval. This guarantees that the correct interval can be
C     selected when a request time coincides with the boundary of
C     interest.
C
C     The overall approach is:
C
C        1)  Obtain attribute information from the input segment.
C
C        2)  Create a first output mini-segment. This mini-segment is
C            created using data from the first input mini-segment
C            having an end time greater than or equal to BEGIN.
C
C            The first output mini-segment contains padding, if needed,
C            on both the left and right sides. On the left side, given
C            a nominal interpolation window width W (W must be even),
C            the nominal pad size NPAD is (W/2) - 1. If I is the index
C            of the last time tag (in the selected input mini-segment)
C            less than or equal to BEGIN, the pad starts at I-NPAD or
C            1, whichever is greater.
C
C            On the right side, if END is greater than or equal to the
C            last epoch of the input mini-segment, all epochs and
C            packets of the input mini-segment following the first ones
C            selected are transferred to the output mini-segment.
C
C            The first mini-segment requires padding on the right only
C            if END precedes the end time of the input mini-segment. In
C            this case the pad size is chosen so that the output
C            mini-segment contains W/2 epochs greater than or equal to
C            END, if possible. If I is the index of the first time tag
C            in the mini-segment greater than or equal to END, then the
C            pad ends at I + (W/2) - 1 or NPKT, whichever is smaller.
C
C            Note that due to the asymmetry of the search techniques
C            used (there are no SPICELIB right-to-left search routines
C            analogous to LSTLTD and LSTLED), the implementation of the
C            pad computation for the right side is not as similar to
C            that for the left side as one might expect.
C
C            The first output mini-segment and all subsequent output
C            mini-segments have the structure of an SPK type 18
C            segment. They consist of
C
C               a) A sequence of data packets
C  
C               b) A sequence of epochs
C
C               c) An epoch directory, if needed
C
C               d) A control area, consisting of
C
C                    - A subtype code
C                    - An interpolation window size
C                    - A packet count
C
C        3)  All input mini-segments whose interpolation intervals
C            follow that of the first used mini-segment and whose stop
C            times are are less than or equal to END are copied whole
C            to the output segment. We refer to this sequence of
C            mini-segments as the "middle group." The middle group may
C            be empty.
C           
C        4)  If necessary, a final output mini-segment is written. This
C            mini-segment will be required unless either
C
C               - The interval of the first mini-segment contains in
C                 its interior the interval BEG : END.
C
C               - The middle group ends at the end of the input segment.
C
C            Note that if the last interval of the middle group ends at
C            END, but END is less than the final input interval's stop
C            time, a final mini-segment is still needed to ensure
C            correct interval selection. If there is no middle group
C            and the first used interpolation interval ends at END, and
C            if END is less than the final input interval's stop time,
C            a final mini-segment is required as well.
C
C            The interpolation interval of the final output
C            mini-segment always starts at an input interval boundary.
C            This interval has padding on the left only if the
C            corresponding input interval has padding on the left; any
C            existing left side padding from the input mini-segment is
C            simply copied to the output mini-segment. On the right
C            side, padding is created if it is necessary and possible
C            to do so. When right side padding is used, the pad size
C            and placement follow the same rules used for the right
C            side padding of the first output mini-segment.
C
C        5)  After all output mini-segments have been written, the
C            following segment-level structures are written to the
C            output segment:
C
C               a) The output segment interpolation interval
C                  boundaries. This list includes the start time of
C                  each output interval and the stop time of the final
C                  output interval.
C
C               b) The output segment interpolation interval boundary
C                  directory, if needed.
C 
C               c) The output segment's mini-segment begin and "end"
C                  pointers. This list consists of the segment
C                  base-relative first address of each mini-segment,
C                  plus the relative address following the final output
C                  mini-segment.
C
C               d) The output segment control area. This consists of:
C 
C                    - The interval selection order flag. This is copied
C                      from the input segment.
C         
C                    - The output segment interval count
C
C     
C     
C
C     See whether there's any work to do; return immediately if not.
C
      IF (  BEGIN  .GT.  END  ) THEN
         CALL CHKOUT ( 'SPKS19' )
         RETURN
      END IF
C
C     We don't check BEGIN and END against the time bounds of the
C     descriptor of the input file because, according to the 
C     SPK subsetting subsystem design, the calling routine SPKSUB
C     has done this already. Note that the descriptor of the source
C     segment is not even an input to this routine. If we wanted to,
C     we could search the input DAF for a descriptor that mapped to
C     the address range BADDR : EADDR. but we're not going to do
C     that.
C
C     Initialize the flag indicating the existence of the "final"
C     output mini-segment.
C
      FINAL  = .FALSE.


C***********************************************************************
C
C     Part 1: Obtain attributes of the input segment
C
C***********************************************************************

C
C     Read the input segment structure control area.
C
      CALL DAFGDA ( HANDLE, EADDR-1, EADDR, DATA )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF
 
C
C     Fetch the interval selection order flag and the 
C     number of interpolation intervals.
C
      ISEL   = NINT ( DATA(1) )
      NINTVL = NINT ( DATA(2) )

C
C     Compute the number of interval boundary directories. Recall that
C     the final interval stop time must be accounted for, so the 
C     directory count is
C
C        ( ( NINTVL + 1) - 1 ) / DIRSIZ
C

      NDIR = NINTVL / DIRSIZ

C
C     Find the base address IVLBAS of the interval start times. First
C     set PTRBAS, which is the address preceding the interval pointers.
C
C     The interval base precedes the interval bounds, the interval
C     directories, the interval pointers, and the control area.
C
      PTRBAS = EADDR  -  ( 2    +  NINTVL + 1 )
      IVLBAS = PTRBAS -  ( NDIR +  NINTVL + 1 )  


C***********************************************************************
C
C     Part 2: Create the first output mini-segment
C
C***********************************************************************

C
C     Search for the first interval that will contribute data to the
C     output segment. We first find the last interval boundary that is
C     strictly less than the epoch BEGIN. The final interval stop time
C     need not be considered, since the segment covers the interval
C     [BEGIN : END]. Note however there is a "corner case" in which
C     
C        BEGIN == END == <final interval stop time>
C
C     Since we're only examining interval start times, the last one
C     we may need to read is at index NINTVL.
C
      NREAD  = MIN ( BUFSIZ, NINTVL )

      BUFBAS = IVLBAS

C
C     NREAD is at least 1 here.
C
      CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )
      
      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF
      

      REMAIN = NINTVL - NREAD

C
C     The variable NREAD is the array index of the last
C     item read into the buffer on the previous read
C     operation. On the first pass NREAD is at least 1.
C
      DO WHILE (       ( REMAIN      .GT. 0     ) 
     .           .AND. ( DATA(NREAD) .LT. BEGIN )  )
      
         BUFBAS = BUFBAS + NREAD

         NREAD  = MIN ( BUFSIZ, REMAIN )         
C
C        NREAD is at least 1 here.
C
         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

         REMAIN = REMAIN - NREAD

      END DO 
C
C     Let I be the index of the last interval boundary time that
C     precedes BEGIN. If there are no such boundary times, I will be
C     zero. This latter case can happen only when the first interval
C     start time is equal to BEGIN.
C 
C     At this point BUFBAS - IVLBAS is the number of boundaries we
C     examined before the final call above to DAFGDA. All of those
C     boundary times were strictly less than BEGIN.
C
      I  =  ( BUFBAS - IVLBAS )  +  LSTLTD ( BEGIN, NREAD, DATA )

C
C     Let BEGIDX be the index of the last interval start time that
C     precedes BEGIN, unless BEGIN coincides with the first interval
C     start time; in this case, BEGIDX must be 1.
C
      BEGIDX = MAX ( 1, I )

C
C     In order to extract data from the mini-segment, we'll need its
C     address range.
C
      CALL DAFGDA ( HANDLE, PTRBAS+BEGIDX, PTRBAS+BEGIDX+1, DATA )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF

C
C     Convert the segment-base-relative mini-segment begin and end
C     pointers to absolute DAF addresses.
C
      MINIB = ( BADDR - 1 ) + NINT( DATA(1) )
      MINIE = ( BADDR - 1 ) + NINT( DATA(2) ) - 1 

C
C     Read the control area of the mini-segment.
C      
      BUFBAS = MINIE - CTRLSZ

      CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+CTRLSZ, CONTRL )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF

C
C     Fetch the control area parameters for the mini-segment.
C
      SUBTYP  =  NINT ( CONTRL(1) )
      WNDSIZ  =  NINT ( CONTRL(2) )
      NPKT    =  NINT ( CONTRL(3) )

C
C     Set the packet size, which is a function of the subtype.
C
      IF ( SUBTYP .EQ. S19TP0 ) THEN

         PKTSIZ = S19PS0

      ELSE IF ( SUBTYP .EQ. S19TP1 ) THEN

         PKTSIZ = S19PS1

      ELSE

         CALL SETMSG ( 'Unexpected SPK type 19 subtype # found in ' 
     .   //            'type 19 segment within mini-segment #.'     )
         CALL ERRINT ( '#',  SUBTYP                                 )
         CALL ERRINT ( '#',  BEGIDX                                 )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                        )
         CALL CHKOUT ( 'SPKS19'                                     )
         RETURN

      END IF

C
C     Determine how much of the mini-segment we need to transfer. The
C     first step is to find the last epoch less than or equal to BEGIN
C     in the mini-segment's epoch list. Let MINBEP be the base address
C     of the epoch list (that is, the start address minus 1).
C    
      MINBEP = ( MINIB - 1 )  +  ( NPKT * PKTSIZ )  

C
C     Read epochs until we find one greater than or equal to BEGIN.
C
C     It's possible that only the last epoch of the input mini-segment
C     satisfies this criterion, but at least one epoch must satisfy it.
C
      NREAD  = MIN ( BUFSIZ, NPKT )

      BUFBAS = MINBEP

      CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )
      
      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF

      REMAIN = NPKT - NREAD

C
C     The variable NREAD is the array index of the last
C     item read into the buffer on the previous read
C     operation.
C
      DO WHILE (       ( REMAIN      .GT. 0     ) 
     .           .AND. ( DATA(NREAD) .LT. BEGIN )  )
C
C        Advance the buffer base to account for the NREAD
C        epochs fetched on the previous DAFGDA call.
C
         BUFBAS = BUFBAS + NREAD
      
         NREAD  = MIN ( BUFSIZ, REMAIN )
C
C        Since REMAIN was positive at the beginning of this
C        loop iteration, NREAD is positive here.
C         
         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

         REMAIN = REMAIN - NREAD

      END DO 
C 
C     At this point BUFBAS - MINBEP is the number of epochs in the
C     input mini-segment we've examined before the final call above to
C     DAFGDA. All of those epochs were strictly less than BEGIN.
C
C     Let BEPIDX be the index of the last epoch that precedes or is
C     equal to BEGIN. That epoch is contained in the last buffer we
C     read.
C
      BEPIDX =  ( BUFBAS - MINBEP )  +  LSTLED ( BEGIN, NREAD, DATA )
      
C
C     BEPIDX is at least 1 and may be as large as NPKT.
C
C     Compute the number of pad epochs we need to maintain proper
C     interpolation behavior in the neighborhood of the epoch at
C     index BEPIDX.
C
      NPAD   =  ( WNDSIZ / 2 ) - 1

C
C     Shift BEPIDX by the pad amount, if possible. The minimum value
C     of BEPIDX is 1.
C
      BEPIDX =  MAX ( 1,  BEPIDX - NPAD )

C
C     The output mini-segment can never have fewer than two epochs.
C     When the window size is 2 and BEPIDX is equal to NPKT, we
C     must extend the window on the left.
C
      BEPIDX = MIN ( BEPIDX,  NPKT-1 )

C
C     If the input interval end time is less than or equal to END, we
C     need to use the rest of the data from this interval. Otherwise
C     find out how much data from this interval we need to transfer.
C
      BUFBAS = IVLBAS + BEGIDX 
      
      CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, IVLEND )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF

C
C     Let EEPIDX be the index of the last epoch we select from
C     the current input mini-segment. We'll set EEPIDX below.
C      
      IF ( IVLEND .LE. END ) THEN
C
C        The requested subset coverage is either equal to or extends
C        beyond the right boundary of this interval. We'll use all data
C        from this interval.
C
         EEPIDX = NPKT

      ELSE
C
C        IVLEND is strictly greater than END. This interval covers
C
C           [BEGIN, END].
C
C        Read epochs from this mini-segment until we find one greater
C        than or equal to END. We have an error if we run out of
C        epochs.
C
C        The input mini-segment contains ( NPKT - BEPIDX + 1 ) epochs
C        following and including the one at BEPIDX.
C
         REMAIN = NPKT - BEPIDX + 1
C
C        REMAIN is at least 2 at this point, since in this case,
C        some epoch exceeds END, and that epoch must have index
C        greater than BEPIDX.
C
         NREAD  = MIN ( BUFSIZ, REMAIN )
C
C        NREAD is at least 2.
C
         IF ( NREAD .LT. 2 ) THEN
C
C           This code should not be reached.
C
            CALL DAFHFN ( HANDLE, SPK )

            CALL SETMSG ( 'Input file: #. Segment address range: '
     .      //            '#:#. Structural error found: NREAD '
     .      //            'is #; end time of interval # is #.'      )
            CALL ERRCH  ( '#', SPK                                  )
            CALL ERRINT ( '#', BADDR                                )
            CALL ERRINT ( '#', EADDR                                )
            CALL ERRINT  ( '#', NREAD                                )
            CALL ERRINT ( '#', BEGIDX                               )
            CALL ERRDP  ( '#', IVLEND                               )
            CALL SIGERR ( 'SPICE(SPKSTRUCTUREERROR)'                )
            CALL CHKOUT ( 'SPKS19'                                  )
            RETURN
            
         END IF
C
C        Set the buffer base address so that we start reading at 
C        address MINBEP + BEPIDX.
C        
         BUFBAS = MINBEP + BEPIDX - 1

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

         REMAIN = REMAIN - NREAD

C
C        NREAD is (still) at least 2.
C
         DO WHILE (  ( REMAIN .GT. 0 ) .AND. ( DATA(NREAD) .LE. END )  )

            BUFBAS = BUFBAS + NREAD

            NREAD  = MIN ( REMAIN, BUFSIZ )
C
C           NREAD is at least 1 since REMAIN was positive 
C           at the top of the loop.
C
            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

            REMAIN = REMAIN - NREAD

         END DO
C 
C        At this point BUFBAS - MINBEP is the number of epochs in the
C        input mini-segment we've examined before the final call above
C        to DAFGDA. If this set of epochs is non-empty, all of these
C        epochs are less than or equal to END. Note that it's possible
C        for END and BEGIN to be equal to the first epoch.
C
C        Let EEPIDX be the index of the first epoch that is strictly
C        greater than END. As asserted above, in this branch of the
C        code, such an epoch must exist. That epoch is contained in the
C        last buffer we read.
C
C        EEPIDX exceeds by 1 the index of the last epoch less than or
C        equal to END.
C
         L      = LSTLED ( END, NREAD, DATA )

         EEPIDX = ( BUFBAS - MINBEP ) + L + 1

C
C        EEPIDX is at least 2 and is less than or equal to NPKT.
C
         IF (  ( EEPIDX .LT. 2 ) .OR. ( EEPIDX .GT. NPKT ) ) THEN
C
C           This code should not be reached.
C
            CALL DAFHFN ( HANDLE, SPK )

            CALL SETMSG ( 'Input file: #. Segment address range: '
     .      //            '#:#. Structural error found: last epoch '
     .      //            'is #; end time of interval # is #.'      )
            CALL ERRCH  ( '#', SPK                                  )
            CALL ERRINT ( '#', BADDR                                )
            CALL ERRINT ( '#', EADDR                                )
            CALL ERRDP  ( '#', DATA(NREAD)                          )
            CALL ERRINT ( '#', BEGIDX                               )
            CALL ERRDP  ( '#', IVLEND                               )
            CALL SIGERR ( 'SPICE(SPKSTRUCTUREERROR)'                )
            CALL CHKOUT ( 'SPKS19'                                  )
            RETURN
            
         END IF

C
C        Compute the number of pad epochs we need to maintain proper
C        interpolation behavior in the neighborhood of the epoch at
C        index EEPIDX.
C
         IF ( DATA(L) .EQ. END ) THEN
C
C           The epochs at indices EEPIDX-1 and EEPIDX comprise the
C           first two epochs of the right half of an interpolation
C           window of size WNDSIZ. We need two fewer pad epochs to
C           complete the right half of the window.
C           
            NPAD = ( WNDSIZ / 2 ) - 2

         ELSE
C
C           The epoch at EEPIDX is the first of the pad.
C
            NPAD = ( WNDSIZ / 2 ) - 1

         END IF

C
C        The maximum allowed value of EEPIDX is NPKT.
C
         EEPIDX =  MIN ( NPKT,  EEPIDX + NPAD )

      END IF

C
C     At this point BEPIDX and EEPIDX are both set.
C
C     Look up the input interval's start time at index BEGIDX.
C     We'll use this below when we compute the interval start
C     time of the first output mini-segment.
C
      CALL DAFGDA ( HANDLE, IVLBAS+BEGIDX, IVLBAS+BEGIDX, IVLBEG )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF


C     We're ready to start transferring data to the output segment. The
C     first mini-segment of the output segment will contain packets
C     BEPIDX through EEPIDX of the input mini-segment at index BEGIDX.
C
      DO I = BEPIDX, EEPIDX

         BUFBAS = MINIB - 1 + ( (I-1) * PKTSIZ )

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+PKTSIZ, DATA )

         CALL DAFADA ( DATA, PKTSIZ )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

      END DO

C
C     Now transfer the epochs at indices BEPIDX : EEPIDX.
C
C     Inside this loop, determine the bounds of the first output
C     interpolation interval. Each bound is either the corresponding
C     bound of the input interval, or a boundary epoch (first or last)
C     of the output epoch list, whichever is most restrictive.
C
      DO I = BEPIDX, EEPIDX

         BUFBAS  =  ( MINIB - 1 ) + ( NPKT * PKTSIZ ) + ( I - 1 )

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, DATA )

         CALL DAFADA ( DATA, 1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

C
C        Let IV1BEG be the start time of the first output
C        interpolation interval. Determine IV1BEG on the first loop
C        pass. IVLBEG has already been set to the start time of the
C        input interval at index BEGIDX.
C
         IF ( I .EQ. BEPIDX ) THEN
C
C           The first output interval cannot start earlier than
C           the interval from which its data are taken. It may
C           start later.
C
            IV1BEG = MAX ( IVLBEG, DATA(1) )
             
         END IF
            
C
C        Determine IV1END on the final loop pass.
C
         IF ( I .EQ. EEPIDX ) THEN
C
C           The first output interval cannot end later than
C           the interval from which its data are taken. It may
C           end earlier.
C
            IV1END = MIN ( IVLEND, DATA(1) )

         END IF

      END DO

C
C     Create the epoch directory for the first output mini-segment.
C
      MINNPK = EEPIDX - BEPIDX + 1

      MINNDR = ( MINNPK - 1 ) / DIRSIZ 

      DO I = 1, MINNDR
C
C        Set BUFBAS to the address that immediately precedes the
C        element we're about to read. We must skip over the data
C        packets and the first (BEPIDX-1) epochs before starting our
C        count.
C
         BUFBAS  =   ( MINIB  - 1 ) + ( NPKT * PKTSIZ ) + 
     .               ( BEPIDX - 1 ) + ( I    * DIRSIZ ) - 1

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, DATA )

         CALL DAFADA ( DATA, 1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

      END DO
            
C
C     Finally, write out the control area for the first mini-segment.
C
      CALL DAFADA (  DBLE( SUBTYP ),  1  )
      CALL DAFADA (  DBLE( WNDSIZ ),  1  )
      CALL DAFADA (  DBLE( MINNPK ),  1  )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF

C
C     Compute the size of the first output mini-segment; we'll need
C     this later to compute the second mini-segment start pointer.
C     The size is the sum of the sizes of the packet set, the
C     epochs, the epoch directories, and the control area.
C
      MIN1SZ = (  MINNPK * ( PKTSIZ + 1 )  ) + MINNDR + CTRLSZ


C***********************************************************************
C
C     Part 3: Transfer the middle group of mini-segments to the
C             output segment, if this group is non-empty
C
C***********************************************************************


C
C     At this point, we might already be done with copying
C     mini-segments. If the coverage interval of the mini-segment we
C     just processed contains [BEGIN, END] in its interior, we're done.
C     If there are no more input mini-segments, we're also done.
C     Otherwise, we'll continue to transfer data from subsequent
C     input mini-segments.
C
C     At this point IVLEND is the end time of the first input 
C     interval. Note that this time may differ from IV1END, which
C     is the end time of the first output interval.
C
      IF (  ( IVLEND .GT. END ) .OR. ( BEGIDX .EQ. NINTVL )  ) THEN
C
C        We've transferred all the data we need. We don't need
C        to obtain data from other mini-segments.
C
         ENDIDX = BEGIDX

C
C        FINAL is already set to .FALSE.
C        
      ELSE
C
C        We need more data, and there are more data to be had.
C
C        Things get a bit easier here: all mini-segments that follow
C        the one we just wrote, and that have end times less than or
C        equal to END, get copied without modification to the output
C        file. Note that this sequence of mini-segments could be empty.

         CURIVL = BEGIDX + 1

C
C        Initialize the start time of the final output mini-segment.
C        We'll update this if we produce more output mini-segments.
C
         IVFBEG = IVLEND

C
C        Get the end time of the interval at index CURIVL.
C
         BUFBAS = IVLBAS + CURIVL

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, IVLEND )
      
         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

C
C        CURIVL is the index of the interval we're about to process,
C        and if CURIVL is in range, IVLEND is its end time.
C
         DO WHILE (  ( IVLEND .LE. END ) .AND. ( CURIVL .LE. NINTVL )  )
C
C           Entering this loop means the "middle" component of the 
C           output segment is non-empty.
C
C           Get the begin and end pointers for the current mini-segment.
C        
            BUFBAS = PTRBAS + CURIVL - 1

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+2, DATA )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

            MINIB = ( BADDR - 1 ) + NINT ( DATA(1) )
            MINIE = ( BADDR - 1 ) + NINT ( DATA(2) ) - 1

C
C           Transfer all data from DAF address MINIB through DAF
C           address MINIE to the target SPK segment.
C
            REMAIN = MINIE - MINIB + 1

            BUFBAS = MINIB - 1

            NREAD  = MIN ( BUFSIZ, REMAIN )            

            DO WHILE ( REMAIN .GT. 0 )

               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )

               CALL DAFADA ( DATA, NREAD )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKS19' )
                  RETURN
               END IF

               REMAIN = REMAIN - NREAD

               BUFBAS = BUFBAS + NREAD

               NREAD  = MIN ( BUFSIZ, REMAIN )            

            END DO
C
C           We've copied the mini-segment at index CURIVL.
C
C           Save the end time of this mini-segment in case
C           this one turns out NOT to be the last; in that
C           case this is the final interval's start time.
C
            IVFBEG = IVLEND

C
C           Get the end time of the next interval, if there
C           is one.
C
            CURIVL = CURIVL + 1

            IF ( CURIVL .LE. NINTVL ) THEN

               BUFBAS = IVLBAS + CURIVL

               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, IVLEND )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKS19' )
                  RETURN
               END IF

            END IF

         END DO
C
C        We've transferred the middle group, if it exists, to the
C        output segment.
C
C        If the last mini-segment we transferred isn't the last of the
C        input segment, we're going to copy at least a portion of the
C        next mini-segment to the output file.
C
C        At this point CURIVL is the index of the next interval to
C        process, if any. If CURIVL is valid, IVLEND is the interval's
C        end time.
C

C***********************************************************************
C
C     Part 4: Create the final output mini-segment, if necessary
C
C***********************************************************************

         IF ( CURIVL .GT. NINTVL ) THEN
C
C           The coverage of the middle group extends to the end of
C           the coverage of the input segment. There's no more data to
C           transfer.
C
C           FINAL is already set to .FALSE.
C
            ENDIDX = NINTVL

         ELSE
C
C           We're going to create one last output mini-segment.
C
            FINAL  = .TRUE.
C
C           The input segment contains at least one more interpolation
C           interval, and the end time of this interval is greater than
C           END. Note that if this interval's end time were equal to
C           END, the interval would have been processed in the loop
C           above.
C
            ENDIDX = CURIVL

C
C           In order to extract data from the mini-segment, we'll need
C           its address range.
C
            CALL DAFGDA ( HANDLE, PTRBAS+ENDIDX, PTRBAS+ENDIDX+1, DATA )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

            MINIB = ( BADDR - 1 ) + NINT( DATA(1) )
            MINIE = ( BADDR - 1 ) + NINT( DATA(2) ) - 1
   
C
C           Read the control area of the mini-segment.
C      
            BUFBAS = MINIE - CTRLSZ

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+CTRLSZ, CONTRL )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

C
C           Fetch the control area parameters for the mini-segment.
C
            SUBTYP  =  NINT ( CONTRL(1) )
            WNDSIZ  =  NINT ( CONTRL(2) )
            NPKT    =  NINT ( CONTRL(3) )

C
C           Set the packet size, which is a function of the subtype.
C
            IF ( SUBTYP .EQ. S19TP0 ) THEN

               PKTSIZ = S19PS0

            ELSE IF ( SUBTYP .EQ. S19TP1 ) THEN

               PKTSIZ = S19PS1

            ELSE

               CALL SETMSG ( 'Unexpected SPK type 19 subtype # found ' 
     .         //            'in type 19 segment within mini-segment '
     .         //            '#.'                                      )
               CALL ERRINT ( '#',  SUBTYP                              )
               CALL ERRINT ( '#',  CURIVL                              )
               CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
               CALL CHKOUT ( 'SPKS19'                                  )
               RETURN

            END IF

C
C           Determine how much of the mini-segment we need to transfer.
C           The first step is to find the last epoch less than or equal
C           to END in the mini-segment's epoch list. Let MINBEP be the
C           base address of the epoch list (that is, the start address
C           minus 1).
C
            MINBEP = MINIB  -  1  +  ( NPKT * PKTSIZ )

C 
C           Read epochs until we find one strictly greater than END.
C           The previous interval was the last one with an end time
C           less than or equal to END, so the epoch we seek should
C           exist. We have an error condition if it doesn't.
C
            NREAD  = MIN ( BUFSIZ, NPKT )

            BUFBAS = MINBEP

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )
      
            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

            REMAIN = NPKT - NREAD

C
C           The variable NREAD is the array index of the last item read
C           into the buffer on the previous read operation.
C
            DO WHILE (       ( REMAIN      .GT. 0   ) 
     .                 .AND. ( DATA(NREAD) .LE. END )  )
      
               BUFBAS = BUFBAS + NREAD

               NREAD  = MIN ( BUFSIZ, REMAIN )

               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+NREAD, DATA )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKS19' )
                  RETURN
               END IF

               REMAIN = REMAIN - NREAD

            END DO 
C
C           At this point BUFBAS - MINBEP is the number of epochs in
C           the input mini-segment we've examined before the final call
C           above to DAFGDA. If this set of epochs is non-empty, all of
C           these epochs are less than or equal to END. Note that it's
C           possible for END and BEGIN to be equal to the first epoch.
C
C           Let EEPIDX be the index of the first epoch that is strictly
C           greater than END. As asserted above, in this branch of the
C           code, such an epoch must exist. That epoch is contained in
C           the last buffer we read.
C
C           EEPIDX exceeds by 1 the index of the last epoch less than
C           or equal to END.
C
            L      =  LSTLED ( END, NREAD, DATA )

            EEPIDX =  ( BUFBAS - MINBEP ) + L + 1

C
C           EEPIDX is at least 2 and is less than or equal to NPKT.
C
            IF ( EEPIDX .LT. 2 ) THEN
C
C              This code should not be reached, since getting here
C              implies the first epoch of the interval is greater than
C              END.
C
               CALL DAFHFN ( HANDLE, SPK )

               CALL SETMSG ( 'Input file: #. Segment address range: '
     .         //            '#:#. Structural error found: no epochs '
     .         //            'in final input interval exceed END. '
     .         //            'Interval index is #; END is #.'          )
               CALL ERRCH  ( '#', SPK                                  )
               CALL ERRINT ( '#', BADDR                                )
               CALL ERRINT ( '#', EADDR                                )
               CALL ERRINT ( '#', ENDIDX                               )
               CALL ERRDP  ( '#', END                                  )
               CALL SIGERR ( 'SPICE(SPKSTRUCTUREERROR)'                )
               CALL CHKOUT ( 'SPKS19'                                  )
               RETURN

            END IF

C
C           Compute the number of pad epochs we need to maintain proper
C           interpolation behavior in the neighborhood of the epoch at
C           index EEPIDX.
C
            IF ( DATA(L) .EQ. END ) THEN
C
C              The epochs at indices EEPIDX-1 and EEPIDX comprise
C              the first two epochs of the right half of an 
C              interpolation window of size WNDSIZ. We need two
C              fewer pad epochs to complete the right half of the
C              window.
C           
               NPAD = ( WNDSIZ / 2 ) - 2

            ELSE
C
C              The epoch at EEPIDX is the first of the pad.
C
               NPAD = ( WNDSIZ / 2 ) - 1

            END IF

C
C           Update the final epoch index to include the pad. The index
C           cannot exceed the mini-segment's packet count.
C
            EEPIDX = MIN ( NPKT,  EEPIDX + NPAD )

C
C           EEPIDX must always exceed BEPIDX; no interpolation
C           interval may have zero length.
C
C           When BEGIN is equal to END, and both are equal to the
C           first epoch, and the window size is 2, NPAD will be
C           -1, and EEPIDX will be 1. We don't want to allow 
C           EEPIDX to be less than 2.
C
            EEPIDX = MAX ( EEPIDX, 2 )

C
C           EEPIDX should always be in range at this point.
C            
            IF ( ( EEPIDX .LT. 2 ) .OR. ( EEPIDX .GT. NPKT ) ) THEN
C
C              This code should not be reached, since getting here
C              implies the first epoch of the interval is greater than
C              END.
C
               CALL DAFHFN ( HANDLE, SPK )

               CALL SETMSG ( 'Input file: #. Segment address range: '
     .         //            '#:#. BEPIDX = #; EEPIDX = #; NPKT = #.'
     .         //            'Interval index is #; END is #.'          )
               CALL ERRCH  ( '#', SPK                                  )
               CALL ERRINT ( '#', BADDR                                )
               CALL ERRINT ( '#', EADDR                                )
               CALL ERRINT ( '#', BEPIDX                               )
               CALL ERRINT ( '#', EEPIDX                               )
               CALL ERRINT ( '#', NPKT                                 )
               CALL ERRINT ( '#', ENDIDX                               )
               CALL ERRDP  ( '#', END                                  )
               CALL SIGERR ( 'SPICE(SPKSTRUCTUREERROR)'                )
               CALL CHKOUT ( 'SPKS19'                                  )
               RETURN

            END IF

C
C           Write the packets of the last mini-segment.
C
            DO I = 1, EEPIDX

               BUFBAS  =  ( MINIB - 1 ) + ( (I-1) * PKTSIZ )

               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+PKTSIZ, DATA )

               CALL DAFADA ( DATA, PKTSIZ )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKS19' )
                  RETURN
               END IF

            END DO

C
C           Write the epochs of the last mini-segment. Save the
C           final epoch; we'll need it later.
C
            DO I = 1, EEPIDX

               BUFBAS  =  ( MINIB - 1 ) + ( NPKT * PKTSIZ ) + ( I - 1 )

               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, DATA )

               CALL DAFADA ( DATA, 1 )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKS19' )
                  RETURN
               END IF

               IF ( I .EQ. EEPIDX ) THEN
C
C                 The current interval is the last of the output
C                 segment. The interval end must be greater than or
C                 equal to END. It's safe to simply choose the final
C                 epoch as the interval end.
C
                  IVFEND = DATA(1)            

               END IF

            END DO

C
C           Create epoch directories for the last mini-segment.
C
            MINNPK = EEPIDX

            MINNDR = ( MINNPK - 1 ) / DIRSIZ 


            DO I = 1, MINNDR

               BUFBAS  =     ( MINIB - 1  ) + ( NPKT * PKTSIZ ) 
     .                    +  ( I * DIRSIZ ) -   1

               CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, DATA )

               CALL DAFADA ( DATA, 1 )

               IF ( FAILED() ) THEN
                  CALL CHKOUT( 'SPKS19' )
                  RETURN
               END IF

            END DO
      
C
C           Finally, write out the control area for the last
C           mini-segment.
C
            CALL DAFADA (  DBLE( SUBTYP ),  1  )
            CALL DAFADA (  DBLE( WNDSIZ ),  1  )
            CALL DAFADA (  DBLE( MINNPK ),  1  )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

C
C           Compute the size in DAF addresses of the last mini-segment.
C           This is the sum of the sizes of the packet space, the
C           epochs, the directories, and the control area.
C
            MINFSZ = ( MINNPK * ( PKTSIZ + 1 ) )  +  MINNDR  +  CTRLSZ

         END IF
C
C        We're done with the final mini-segment.
C 
      END IF
C
C     We've transferred all of the data we need from mini-segments at
C     indices BEGIDX : ENDIDX.


C***********************************************************************
C
C     Part 5: Create segment-level data structures in the output segment
C
C***********************************************************************


C
C     Write out the interval bounds for the new segment.
C
C     Let NOIVL be the number of intervals in the output subset
C     segment.
C
      NOIVL = ENDIDX - BEGIDX + 1

C
C     The first interval start time is IV1BEG.
C
      CALL DAFADA ( IV1BEG, 1 )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'SPKS19' )
         RETURN
      END IF

C
C     Write the remaining interval boundaries.
C
      IF ( NOIVL .EQ. 1 ) THEN
C
C        The final interval boundary is the stop time of 
C        the first interval.
C
         CALL DAFADA ( IV1END, 1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

      ELSE
C
C        There are multiple output mini-segments. There is either
C        a non-empty middle group, a final mini-segment, or both.
C
C        Set the upper bound of the interval boundary transfer loop.
C        
         IF ( FINAL ) THEN
C
C           We'll transfer all interval start times up to,
C           but not including, the final one.
C
            UB = NOIVL - 1

         ELSE
C
C           There's no mini-segment following the middle group.
C
C           Transfer all start times of the middle group, plus
C           the end time of the last interval of the middle
C           group.
C            
            UB = NOIVL + 1

         END IF

C
C        Transfer interval boundaries from the middle group.
C
         DO I = 2, UB

            BUFBAS = IVLBAS + ( BEGIDX - 1 ) + ( I - 1 )

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, DATA )

            CALL DAFADA ( DATA(1), 1 )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

         END DO

C
C        If the "final" mini-segment exists, we haven't 
C        transferred its interval boundaries. Do it now.
C
         IF ( FINAL ) THEN
C
C           The start and end times of the last output interpolation 
C           interval are stored in IVFBEG and IVFEND.
C
C           Note that IVFBEG was initialized after the first output
C           mini-segment was written, and it was updated if necessary
C           in the block of code that transferred the middle group.
C
            CALL DAFADA ( IVFBEG, 1 )
            CALL DAFADA ( IVFEND, 1 )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

         END IF

      END IF
C
C     The interval boundaries have been written.
C
C     Create an interval boundary directory for the new segment. Every
C     boundary whose index relative to BEGIDX-1 is multiple of DIRSIZ
C     becomes a directory entry, unless that entry has no successors.
C     This implies that the interval bounds to be read belong to the
C     range
C
C        BEGIDX + 1  :  ENDIDX - 1
C
C     This implies that we can read all of the directory entries 
C     from the input segment; we won't use as directory entries 
C     the initial or final interval bounds of the output segment.
C     
C     Since the number of epoch boundaries is NOIVL + 1, the directory
C     count is
C
C        ( ( NOIVL + 1 ) - 1 ) / DIRSIZ
C
C
      NSDIR = NOIVL / DIRSIZ

      DO I = 1, NSDIR
C
C        Look up the interval boundary at offset I*DIRSIZ from
C        the boundary index BEGIDX-1.
C
         BUFBAS = IVLBAS + ( BEGIDX - 1 ) + ( I * DIRSIZ ) - 1

         CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, DATA )

C
C        Write this directory entry to the output segment.
C
         CALL DAFADA ( DATA, 1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT( 'SPKS19' )
            RETURN
         END IF

      END DO

C
C     Write out mini-segment pointers for the new segment.
C
C     The first output mini-segment ranges from relative
C     addresses 1 : MIN1SZ. 
C
      CALL DAFADA ( DBLE( 1 ),  1 )

      IF ( NOIVL .EQ. 1 ) THEN
C
C        The next pointer indicates the first address after the
C        mini-segment, whether or not there is another mini-segment.
C
C        Note that MIN1SZ was initialized after the first output
C        mini-segment was written.
C
         CALL DAFADA ( DBLE( MIN1SZ + 1),  1 )

      ELSE
C
C        There are multiple output mini-segments. There is either
C        a non-empty middle group, a final mini-segment, or both.
C
C        We can obtain from the input segment the sizes of the
C        mini-segments that were copied whole.
C
         START = MIN1SZ + 1

C
C        Set the upper bound of the mini-segment pointer transfer loop.
C        
         IF ( FINAL ) THEN
C
C           We'll transfer all mini-segment start pointers up to and
C           including the start pointer of the final output
C           
            UB = NOIVL 

         ELSE
C
C           The middle group is non-empty, and there's no mini-segment
C           following the middle group.
C
C           Write all start pointers of the middle group, plus the end
C           pointer of the last mini-segment of the middle group. The
C           end pointer is the successor of the last DAF address
C           occupied by the mini-segment.
C            
            UB = NOIVL + 1

         END IF

C
C        Write mini-segment pointers from the middle group.
C
C        All of the middle group pointers of the output segment will be
C        shifted relative to the corresponding pointers of the input
C        segment. The shift reflects the sum of the sizes of the input
C        mini-segments preceding the first one from which data were
C        transferred, as well as the amount by which the first output
C        mini-segment "shrank" relative to the mini-segment from which
C        it was created. The shift equals the difference between the
C        final address of the first output mini-segment and the final
C        address of the input mini-segment at index BEGIDX.
C         
         DO I = 2, UB
C
C           Look up the Ith start pointer.
C
            BUFBAS = PTRBAS + ( BEGIDX - 1 ) + ( I - 1 )

            CALL DAFGDA ( HANDLE, BUFBAS+1, BUFBAS+1, DATA )
C
C           On the first pass, compute the pointer shift amount.
C
            IF ( I .EQ. 2 ) THEN

               SHIFT = ( MIN1SZ + 1 ) - NINT( DATA(1) )

            END IF

            START = NINT( DATA(1) )  +  SHIFT

            CALL DAFADA ( DBLE( START ), 1 )

            IF ( FAILED() ) THEN
               CALL CHKOUT( 'SPKS19' )
               RETURN
            END IF

         END DO

C
C        If the "final" mini-segment exists, we haven't 
C        transferred its end pointer. Do it now.
C
         IF ( FINAL ) THEN
C
C           MINFSZ is the size of the final output mini-segment.
C
C           The end pointer of the last output mini-segment is
C           START+MINFSZ. The end pointer is the successor of the last
C           DAF address of the mini-segment.
C
C           Write the pointer.
C
            CALL DAFADA ( DBLE( START + MINFSZ ),  1 )

         END IF

      END IF

C
C     Write the interval count and selection flag to the
C     new segment.
C     
      CALL DAFADA ( DBLE( ISEL  ), 1 )
      CALL DAFADA ( DBLE( NOIVL ), 1 )      

      CALL CHKOUT ( 'SPKS19' )
      RETURN
      END
