C$Procedure      SGFRVI ( Generic Segments: Fetch ref. value and index )
 
      SUBROUTINE SGFRVI ( HANDLE, DESCR, X, VALUE, INDX, FOUND )
      IMPLICIT NONE
 
C$ Abstract
C
C     Given the handle of a DAF and the descriptor associated with
C     a generic DAF segment in the file, find the reference value
C     associated with the value X and it's index.
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
C     DAF Required Reading.
C
C$ Keywords
C
C     GENERIC SEGMENTS
C
C$ Declarations
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR ( * )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      VALUE
      INTEGER               INDX
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of a DAF open for reading.
C     DESCR      I   The descriptor for a DAF generic segment.
C     X          I   The key value used to find a reference and index.
C     VALUE      O   The reference value associated with X.
C     INDX       O   The index of VALUE within the reference values.
C     FOUND      O   A flag indicating whether values for X were found.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of a DAF open for reading
C
C     DESCR      is the descriptor of the generic segment that we are
C                going to search for a reference value to associate with
C                X.
C
C     X          a value for which the associated reference value
C                and reference index is requested.
C
C$ Detailed_Output
C
C     VALUE      is the reference value associated with the input value
C                X.
C
C     INDX       is the index of VALUE within the set of reference
C                values for the generic segment. This value may be used
C                to obtain a particular packet of data from the generic
C                segment.
C
C     FOUND      is a logical flag indicating whether a reference value
C                associated with X was found. If a reference value was
C                found, FOUND will have a value of TRUE; otherwise it
C                will have a value of FALSE.
C
C$ Parameters
C
C     This subroutine makes use of parameters defined in the file
C     'sgparam.inc'.
C
C$ Files
C
C      See the description of HANDLE above.
C
C$ Exceptions
C
C     1) The error SPICE(UNKNOWNREFDIR) will be signalled if
C        the reference directory structure is unrecognized.  The most
C        likely cause of this error is that an upgrade to your
C        version of the SPICE toolkit is needed.
C
C     2) If a value computed for the index of an implicitly indexed
C        generic segment is too large to be represented as an integer,
C        the error SPICE(INDEXTOOLARGE) will be signalled.
C
C$ Particulars
C
C     This routine allows you to easily find the index and value
C     of the reference item that should be associated with a
C     value X.  Given this information you can then easily retrieve
C     the packet that should be associated with X.
C
C$ Examples
C
C     Suppose that you have a generic segment that contains the
C     following items.
C
C         1)  Packets that model the motion of a body as a function
C             of time over some interval of time.
C
C         2)  Reference values that are the epochs corresponding
C             to the beginning of the intervals for the packets.
C
C     To retrieve the correct packet to use to compute the position
C     and velocity of the body at a particular epoch,  ET, you could
C     use the following code. (Note this block of code assumes that
C     you aren't going to run into any exceptional cases such as ET
C     falling outside the range of times for which the packets can
C     provide ephemeris data.)
C
C        Find out the index of the time that should be associated
C        with the ET we've been given
C
C        CALL SGFRVI ( HANDLE, DESCR, ET,  ETFND, INDX, FOUND )
C
C        Fetch the INDX'th ephemeris packet from the segment.
C
C        CALL SGFPKT ( HANDLE, DESCR, INDX, EPHEM )
C
C
C$ Restrictions
C
C     The segment described by DESCR MUST be a generic segment,
C     otherwise the results of this routine are not predictable.
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C
C-    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT)
C
C        A bug was found in the EXPCLS index case when the
C        trying to retrieve the last value in a generic segment.
C        This bug was discovered by the HP compiler complaining
C        that an index used was not initialized.
C
C        The offending line was
C
C                 MYVALU = BUFFER(I)
C
C        The corrected line is:
C
C                 MYVALU = BUFFER(BFINDX)
C
C-    SPICELIB Version 1.0.0, 28-Mar-1994 (KRG) (WLT)
C
C-&
 
C$ Index_Entries
C
C     find the index of a reference value in a generic segment
C
C-&
 
C
C     Spicelib Functions
C
      INTEGER               INTMAX
      INTEGER               LSTLED
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
C     Include the mnemonic values for the generic segment declarations.
C
      INCLUDE 'sgparam.inc'
 
      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )
 
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = DIRSIZ + 1 )
C
C     Local Variables
C
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      DPIMAX
      DOUBLE PRECISION      DPTEMP
      DOUBLE PRECISION      ENDREF
      DOUBLE PRECISION      MYVALU
 
      INTEGER               BEGIN
      INTEGER               BFINDX
      INTEGER               FULLRD
      INTEGER               END
      INTEGER               I
      INTEGER               MYINDX
      INTEGER               NFETCH
      INTEGER               MYNPKT
      INTEGER               MYNRDR
      INTEGER               MYNREF
      INTEGER               MYRDRB
      INTEGER               MYRDRT
      INTEGER               MYREFB
      INTEGER               RDRIDX
      INTEGER               REMAIN
 
      LOGICAL               DONE
      LOGICAL               FIRST
      LOGICAL               ISDIRV
      LOGICAL               MYFND
C
C     Saved Variables
C
      SAVE                  FIRST
C
C     Initial Values
C
      DATA                  FIRST / .TRUE. /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( 'SGFRVI' )
C
C     Set the value for the maximum index as a double precision number,
C     but only do it the first time into the subroutine.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         DPIMAX = DBLE( INTMAX() )
 
      END IF
 
C
C     Collect the necessary meta data values common to all cases.
C
      CALL SGMETA ( HANDLE, DESCR, NPKT,   MYNPKT )
      CALL SGMETA ( HANDLE, DESCR, NREF,   MYNREF )
      CALL SGMETA ( HANDLE, DESCR, RDRTYP, MYRDRT )
      CALL SGMETA ( HANDLE, DESCR, REFBAS, MYREFB )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SGFRVI' )
         RETURN
      END IF
C
C     Check to be sure that we know how to deal with the type of index
C     in the segment. The index type should be between the minimum
C     allowed index type, MNIDXT, and the maximum allowed index type,
C     MXIDXT, as specified in the file 'sgparam.inc'.
C
      IF ( ( MYRDRT .LT. MNIDXT) .OR. ( MYRDRT .GT. MXIDXT ) ) THEN
 
         CALL SETMSG ( 'The generic DAF segment you attempted to'
     .   //            ' read has an unsupported reference directory'
     .   //            ' structure. The integer code given for this'
     .   //            ' structure is #, and allowed codes are within'
     .   //            ' the range # to #. The likely cause of this'
     .   //            ' anamoly is your version of SPICELIB needs'
     .   //            ' updating. Contact your system administrator'
     .   //            ' or NAIF for a toolkit update.'               )
         CALL ERRINT ( '#', MYRDRT                                    )
         CALL ERRINT ( '#', MNIDXT                                    )
         CALL ERRINT ( '#', MXIDXT                                    )
         CALL SIGERR ( 'SPICE(UNKNOWNREFDIR)'                         )
         CALL CHKOUT ( 'SGFRVI'                                       )
         RETURN
 
      END IF
C
C     We don't have an index yet and we initialize things to zero.
C
      MYFND  = .FALSE.
      MYINDX = 0
      MYVALU = 0.0D0
C
C     We pass the idiot checks, so lets proceed. We have a IF block for
C     each allowed reference directory type code.
C
C        For implicitly indexed data packets, the interval
C
C           [ BUFFER(1), BUFFER(1) + (N - 1) * BUFFER(2) )
C
C        is divided into subintervals as follows:
C
C           (-infinity, r1), [r_1,r_2) [r_2, r_3), ..., [r_i, r_(i+1)),
C            ..., [r_N, +infinity),
C
C        where N = the number of packets in the segment, MYNPKT, and
C        r_i = BUFFER(1) + (i-1) * BUFFER(2).
C
C        If X is in [r_i, r_(i+1)), i = 1, N-1, then we found a value
C        and the index returned will be i with the reference value
C        returned will be r_i.
C
C        If X is in [r_N, +infinity), then we found a value and the
C        index returned will be N and the reference value returned will
C        be r_N.
C
C        If X is in (-infinity, r1), we have two possibilities:
C
C           1) If the index type is implicit closest, we found a value,
C              the index returned will be 1 and the reference value
C              returned will be r_1.
C
C           2) If the index type is implicit less than or equal, we do
C              not find a value.
C
C        For explicitly indexed packets we simply search the reference
C        directory for an appropriate reference value.
C
      IF ( ( MYRDRT .NE. IMPLE ) .AND. ( MYRDRT .NE. IMPCLS ) ) THEN
C
C        In addition to the meta data items we already have, we also
C        need these.
C
         CALL SGMETA ( HANDLE, DESCR, NRDR,   MYNRDR )
         CALL SGMETA ( HANDLE, DESCR, RDRBAS, MYRDRB )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFRVI' )
            RETURN
         END IF
C
C        We need to scan the reference directory (if there is one) to
C        determine the appropriate block of reference values to read
C        from the generic segment. Then we compute the number of
C        reference values to fetch and examine. Finally, based on the
C        index type we figure out whether we have found a reference
C        value or not. It will take a little while to get there, so
C        let's get going.
C
C        We have not started yet, so we're not done and we cannot have a
C        reference directory value yet.
C
         DONE   = .FALSE.
         ISDIRV = .FALSE.
C
C        We have not read any full buffers of reference directory values
C        yet, all of the reference directory values remain to be read,
C        and we have no index for a reference directory value.
C
         FULLRD = 0
         REMAIN = MYNRDR
         RDRIDX = 0
C
C        Search the reference directory values to select the appropriate
C        block of reference values to read.
C
         DO WHILE ( ( .NOT. DONE ) .AND. ( REMAIN .GT. 0 ) )
C
C           Read a buffer of reference directory items.
C
            NFETCH = MIN ( DIRSIZ, REMAIN )
            BEGIN  = MYRDRB + FULLRD * DIRSIZ + 1
            END    = BEGIN  + NFETCH - 1
 
            CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SGFRVI' )
               RETURN
            END IF
C
C           See if X is in the current buffer.
C
            RDRIDX = LSTLED ( X, NFETCH, BUFFER )
 
            IF ( RDRIDX .EQ. 0 ) THEN
C
C              If not, then X < BUFFER(1) and we're done. This indicates
C              that the desired reference value is before, or in, the
C              previous block of reference values.
C
               DONE = .TRUE.
 
            ELSE IF ( RDRIDX .EQ. NFETCH ) THEN
C
C              If we get the last value of the buffer, then either we
C              are done, X = BUFFER(NFETCH), or X > BUFFER(NFETCH).
C
               IF ( X .EQ. BUFFER(NFETCH) ) THEN
C
C                 If X = BUFFER(NFETCH) we are done, we have a directory
C                 value, and it might be a value we want to return.
C
                  DONE   = .TRUE.
                  ISDIRV = .TRUE.
 
               ELSE
C
C                 Otherwise, we might have more stuff to read, so update
C                 the remainder and the current number of full buffer
C                 reads and try the loop again.
C
                  REMAIN = REMAIN - NFETCH
 
                  IF ( REMAIN .GT. 0 ) THEN
C
C                    We don't want to increment FULLRD for a partial
C                    buffer read. The arithmetic for the index
C                    calculations below will use RDRIDX to deal with
C                    this.
C
                     FULLRD = FULLRD + 1
 
                  END IF
 
               END IF
 
            ELSE
C
C              BUFFER(1) <= X < BUFFER(NFETCH), i.e., we have something
C              in the buffer. Check to see if X = BUFFER(RDRIDX). If so,
C              we are done, we have a directory value, and it might be a
C              value we want to return. Otherwise, we are just done.
C
               DONE = .TRUE.
 
               IF ( X .EQ. BUFFER(RDRIDX) ) THEN
 
                  ISDIRV = .TRUE.
 
               END IF
 
            END IF
 
         END DO
 
         RDRIDX = FULLRD * DIRSIZ + RDRIDX
C
C        There are three cases that we need to consider when X is not a
C        reference directory value:
C
C           Case 1: 0 < RDRIDX < MYNRDR (most common first)
C           Case 2: RDRIDX = 0
C           Case 3: RDRIDX = MYNRDR
C
         IF ( .NOT. ISDIRV ) THEN
 
            IF ( ( RDRIDX .GT. 0 ) .AND. ( RDRIDX .LT. MYNRDR ) ) THEN
C
C              If we were able to bracket X before reaching the end of
C              the reference directory, then we KNOW that we have a
C              candidate for a reference value in the reference data.
C              All we need to do is read the reference data and find it
C              in the buffer. We also read the reference directory
C              values that bracket the desired reference value into
C              BUFFER, so that they are there if we need them.
C
               NFETCH = MIN( BUFSIZ, MYNREF - RDRIDX*DIRSIZ + 1 )
 
               BEGIN  = MYREFB + DIRSIZ * RDRIDX
               END    = BEGIN  + NFETCH - 1
 
               CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF
 
               BFINDX = LSTLED ( X, NFETCH, BUFFER )
               MYINDX = DIRSIZ * RDRIDX + BFINDX - 1
 
            ELSE IF ( RDRIDX .EQ. 0 ) THEN
C
C              The reference value may be one of the reference values
C              less than the first reference directory item. So we
C              compute the beginning and ending addresses for the data,
C              read it in, and try to find a reference value.
C
               NFETCH = MIN ( BUFSIZ, MYNREF )
 
               BEGIN  = MYREFB + 1
               END    = BEGIN  + NFETCH - 1
 
               CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF
 
               BFINDX = LSTLED ( X, NFETCH, BUFFER )
               MYINDX = BFINDX
 
            ELSE IF ( RDRIDX .EQ. MYNRDR ) THEN
C
C              If we were not able to bracket X before reaching the end
C              of the reference directory, then we might have a
C              candidate for a reference value in the reference data
C              after the last reference directory value. All we need to
C              do is read the reference data and look.
C
C              NOTE: NFETCH can never be zero or negative, so we can
C              glibly use it. The reason for this is the NFETCH can only
C              be zero if the desired reference value is a reference
C              directory value, and we already know that the reference
C              value we want is not a reference directory value, because
C              we are here. For similar reasons, NFETCH can never be
C              negative.
C
               BEGIN  = MYREFB + DIRSIZ * RDRIDX
               END    = MYREFB + MYNREF
               NFETCH = END - BEGIN + 1
 
               CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF
 
               BFINDX = LSTLED ( X, NFETCH, BUFFER )
               MYINDX = DIRSIZ * RDRIDX + BFINDX - 1
 
            END IF
 
         ELSE
C
C           We have a reference directory value, whose index is easy to
C           compute.
C
            MYINDX = DIRSIZ * RDRIDX
 
         END IF
C
C        Now, if we have a candidate for a reference value, lets make
C        sure, based onthe type of index we have.
C
         IF ( MYRDRT .EQ. EXPLT ) THEN
C
C           We have a reference value only if X > some reference
C           value.
C
            IF ( .NOT. ISDIRV ) THEN
C
C              If the value is not a reference directory value, then
C              we have two cases:
C
C                 Case 1: 0 < MYINDX <= MYNREF
C                 Case 2: MYINDX = 0
C
               IF (       ( MYINDX .GT. 0      )
     .              .AND. ( MYINDX .LE. MYNREF ) ) THEN
C
C                 We found a reference value. The reference value we
C                 want is either the value indicated by MYINDX or
C                 the reference value immediately preceding MYINDX,
C                 if there is such a value. To deal with this we
C                 split the test up into two cases.
C
                  IF ( MYINDX .GT. 1 ) THEN
C
C                    If X > BUFFER(BFINDX) then we are done, so set the
C                    value. If not, then we want the reference value
C                    that is immediately before the current one.
C
                     IF ( X .GT. BUFFER(BFINDX) ) THEN
 
                        MYFND  = .TRUE.
                        MYVALU = BUFFER(BFINDX)
 
                     ELSE
 
                        MYFND = .TRUE.
                        MYVALU = BUFFER(BFINDX-1)
                        MYINDX = MYINDX - 1
 
                     END IF
 
                  ELSE
C
C                    Remember, MYINDX is 1 here. If we are greater
C                    than the first reference value in the segment,
C                    we are done. Otherwise there is no reference
C                    value to be associated with X.
C
                     IF ( X .GT. BUFFER(MYINDX) ) THEN
 
                        MYFND  = .TRUE.
                        MYVALU = BUFFER(MYINDX)
 
                     ELSE
C
C                       We did not find a reference value. X was
C                       equal to the first reference value of the
C                       generic segment.
C
                        MYFND = .FALSE.
 
                     END IF
 
                  END IF
 
               ELSE IF ( MYINDX .EQ. 0 ) THEN
C
C                 We did not find a reference value. X was < the
C                 first reference value for the generic segment.
C
                  MYFND = .FALSE.
 
               END IF
 
            ELSE
C
C              We have a reference directory value, and we are done.
C              Either the reference directory value is the one we
C              want or the reference value immediately preceeding it
C              is the one we want.
C
               MYFND  = .TRUE.
 
               MYINDX = MYINDX - 1
 
               BEGIN  = MYREFB + MYINDX
               END    = BEGIN
 
               CALL DAFGDA ( HANDLE, BEGIN, END, MYVALU )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SGFRVI' )
                  RETURN
               END IF
 
            END IF
 
         ELSE IF ( MYRDRT .EQ. EXPLE ) THEN
C
C           We have a reference value only if X >= some reference
C           value. At this point, either we have the value and index
C           we want or X is before the first reference value of the
C           generic segment. We consider two cases, the first when X
C           is not a referecne directory value, and the second when
C           it is.
C
            IF ( .NOT. ISDIRV ) THEN
C
C              If X is not a directory value, then MYINDX is either
C              equal to zero, implying that X is before the first
C              reference value in the generic segment, or MYINDX > 0,
C              implying that we have found a reference value.
C
               IF (       ( MYINDX .GT. 0      )
     .              .AND. ( MYINDX .LE. MYNREF ) ) THEN
 
                  MYFND  = .TRUE.
                  MYVALU = BUFFER(BFINDX)
 
               ELSE IF ( MYINDX .EQ. 0 ) THEN
C
C                 We did not find a reference value. X was < the
C                 first reference value for the generic segment.
C
                  MYFND = .FALSE.
 
               END IF
 
            ELSE
C
C              We have a reference directory value, and it is the one
C              we want.
C
               MYFND  = .TRUE.
               MYVALU = X
 
            END IF
 
         ELSE IF ( MYRDRT .EQ. EXPCLS ) THEN
C
C           We have a reference value for every value of X. If X <
C           the first reference value of the generic segment, the
C           closest value is the first reference value. If X > the
C           last reference value of the generic segment, the closest
C           value is the last reference value. For X between the
C           first and last reference values we simple take the
C           closest reference value to X, resolving a tie by
C           accepting the larger reference value.
C
            IF ( .NOT. ISDIRV ) THEN
C
C              If X is not a directory value, then MYINDX is either
C              equal to zero, implying that X is before the first
C              reference value in the generic segment,
C              0 < MYINDX < MYNPKT, implying X is between the first
C              and last reference values in the generic segment, or
C              MYINDX = MYNPKT implying that X is greater than or
C              equal to the last reference value.
C
               IF (       ( MYINDX .GT. 0      )
     .              .AND. ( MYINDX .LT. MYNREF ) ) THEN
 
                  I = BFINDX
C
C                 Find the closest value to X, choosing the larger in
C                 the event of a tie.
C
                  IF ( (BUFFER(I+1) - X) .LE. (X - BUFFER(I)) ) THEN
 
                     I      = I + 1
                     MYINDX = MYINDX + 1
 
                  END IF
 
                  MYFND  = .TRUE.
                  MYVALU = BUFFER(I)
 
               ELSE IF ( MYINDX .EQ. 0 ) THEN
C
C                 X is before the first reference value for the
C                 generic segment, so the closest reference value is
C                 the first one.
C
                  MYFND  = .TRUE.
                  MYINDX = 1
                  MYVALU = BUFFER(1)
 
               ELSE IF ( MYINDX .EQ. MYNREF ) THEN
C
C                 X is at of after the last reference value for the
C                 generic segment, so the closest reference value is
C                 the last reference value, which will be in BUFFER.
C
 
                  MYFND  = .TRUE.
                  MYVALU = BUFFER(BFINDX)
 
               END IF
 
            ELSE
C
C              We have a reference directory value, and it is the one
C              we want.
C
               MYFND  = .TRUE.
               MYVALU = X
 
            END IF
 
         END IF
 
      ELSE IF ( MYRDRT .EQ. IMPLE ) THEN
C
C        Get the begin and end addresses from which to read the
C        reference values and get the reference values.
C
         BEGIN = MYREFB + 1
         END   = MYREFB + 2
 
         CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFRVI' )
            RETURN
         END IF
 
         ENDREF = BUFFER(1) + DBLE(MYNPKT-1) * BUFFER(2)
C
C        Compute the index if we can.
C
         IF ( X .LT. BUFFER(1) ) THEN
C
C           If X is less than BUFFER(1), we do not have a reference
C           value.
C
            MYFND = .FALSE.
 
         ELSE IF ( X .GT. ENDREF ) THEN
C
C           If X is greater than ENDREF, then we have a reference
C           value, ENDREF.
C
            MYFND  = .TRUE.
            MYINDX = MYNPKT
            MYVALU = ENDREF
 
         ELSE
C
C           r_1 < X < r_N, i.e., we found a value. Compute the index
C           and the reference value.
C
            IF ( MYNPKT .GT. 1 ) THEN
 
               MYFND = .TRUE.
C
C              Compute the index.
C
               DPTEMP = 1.0D0 + (X - BUFFER(1)) / BUFFER(2)
C
C              Test to see if we can safely convert the index to an
C              integer.
C
               IF ( DPTEMP .GT. DPIMAX ) THEN
 
                  CALL SETMSG ( 'The computed index is too large'
     .            //            ' to be represented as an integer.'
     .            //            ' The most likely problem is that'
     .            //            ' an incorrect value was stored'
     .            //            ' for the step size. The value'
     .            //            ' found for the step was: #'        )
                  CALL ERRDP ( '#', BUFFER(2)                       )
                  CALL SIGERR ( 'SPICE(INDEXTOOLARGE)'              )
                  CALL CHKOUT ( 'SGFRVI'                            )
                  RETURN
 
               END IF
 
               MYINDX = IDINT( DPTEMP         )
               MYINDX = MIN  ( MYINDX, MYNPKT )
 
            ELSE
C
C              There is only one packet.
C
               MYINDX = 1
 
            END IF
C
C           Compute the reference value.
C
            MYVALU = BUFFER(1) + DBLE(MYINDX-1) * BUFFER(2)
 
         END IF
 
      ELSE IF ( MYRDRT .EQ. IMPCLS ) THEN
C
C        Get the begin and end addresses from which to read the
C        reference values and get the reference values.
C
         BEGIN = MYREFB + 1
         END   = MYREFB + 2
 
         CALL DAFGDA ( HANDLE, BEGIN, END, BUFFER )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SGFRVI' )
            RETURN
         END IF
 
         ENDREF = BUFFER(1) + DBLE(MYNPKT-1) * BUFFER(2)
C
C        Compute the index if we can.
C
         IF ( X .LT. BUFFER(1) ) THEN
C
C           If X < BUFFER(1), then we found a value, the index
C           returned will be 1 and the reference value returned will
C           be BUFFER(1).
C
            MYFND  = .TRUE.
            MYINDX = 1
            MYVALU = BUFFER(1)
 
         ELSE IF (  X .GT. ENDREF ) THEN
C
C           If X > ENDREF, then we found a value, the index returned
C           will be MYNPKT and the reference value returned will be
C           ENDREF.
C
            MYFND  = .TRUE.
            MYINDX = MYNPKT
            MYVALU = ENDREF
 
         ELSE
C
C           r_1 < X < r_N, i.e., we found a value. Compute the index
C           and the reference value. If X is closer to r_I, the index
C           returned will be I with a reference value of r_I. If X is
C           closer to r_(I+1), the index returned will be I+1 with a
C           reference value of r_(I+1).
C
            IF ( MYNPKT .GT. 1 ) THEN
 
               MYFND = .TRUE.
C
C              Compute the index.
C
               DPTEMP = 1.5D0 + (X - BUFFER(1)) / BUFFER(2)
 
               IF ( DPTEMP .GT. DPIMAX + 0.5D0 ) THEN
 
                  CALL SETMSG ( 'The computed index is too large'
     .            //            ' to be represented as an integer.'
     .            //            ' The most likely problem is that'
     .            //            ' an incorrect value was stored'
     .            //            ' for the step size. The value'
     .            //            ' found for the step was: #'        )
                  CALL ERRDP ( '#', BUFFER(2)                       )
                  CALL SIGERR ( 'SPICE(INDEXTOOLARGE)'              )
                  CALL CHKOUT ( 'SGFRVI'                            )
                  RETURN
 
               END IF
 
               MYINDX = IDINT( DPTEMP )
 
            ELSE
C
C              There is only one packet.
C
               MYINDX = 1
 
            END IF
C
C           Compute the reference value.
C
            MYVALU = BUFFER(1) + DBLE(MYINDX-1) * BUFFER(2)
 
         END IF
 
      END IF
 
C
C     At this point, we have either found a value or not. If so, then we
C     need to set the index, value, and found flag for output.
C     Otherwise, we simply set the found flag.
C
      IF ( MYFND ) THEN
 
         INDX  = MYINDX
         VALUE = MYVALU
 
      END IF
 
      FOUND = MYFND
 
      CALL CHKOUT ( 'SGFRVI' )
      RETURN
 
      END
