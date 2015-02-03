C$Procedure      ZZEKSCA ( EK, scratch area )
 
      SUBROUTINE ZZEKSCA ( N, BEG, END, IDATA, TOP )
 
C$ Abstract
C
C     Manage the EK scratch area.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
 
      INTEGER               N
      INTEGER               BEG
      INTEGER               END
      INTEGER               IDATA ( * )
      INTEGER               TOP
 
C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     N          I   ZZEKSPSH, ZZEKSPOP, ZZEKSDEC
C     BEG        I   ZZEKSUPD, ZZEKSRD
C     END        I   ZZEKSUPD, ZZEKSRD
C     IDATA     I-O  ZZEKSPSH, ZZEKSPOP, ZZEKSUPD, ZZEKSRD
C     TOP        O   ZZEKSTOP
C
C$ Detailed_Input
C
C     See the entry points for descriptions of their inputs.
C
C$ Detailed_Output
C
C     See the entry points for descriptions of their outputs.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called directly, the error SPICE(BOGUSENTRY)
C        is signalled.
C
C     See the entry points for discussions of exceptions specific to
C     those routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The specific implementation of the EK scratch area is NOT
C     considered part of the specification of this suite of routines:
C     the implementation may be changed without notice.  However,
C     some aspects of the current implementation, such as scratch
C     file usage, are visible to users and therefore are discussed
C     in this subroutine header.
C
C     The EK system, in searching for events that satisfy a query,
C     produces intermediate results that require a potentially very
C     large amount of storage, more than can be expected to be
C     available in the form of memory.  On the other hand, in order
C     to achieve reasonable query response time, these intermediate
C     results must be capable of being accessed quickly.  The EK
C     scratch area provides a storage location that uses a combination
C     of memory and disk storage to give the EK system a large storage
C     area, part of which can be rapidly accessed.
C
C     The logical structure of the EK scratch area is that of a large
C     one-dimensional integer stack.  The indices of the  elements of
C     this stack are referred to as scratch area `addresses'.  Scratch
C     area addresses start at 1 and increase.  The maximum address is
C     the maximum integer representable on the host computer, but the
C     maximum usable address depends on the disk storage available
C     to the calling program at the time the program is run.
C
C     The EK scratch area has access routines that allow a calling
C     program to write to and read from it. Calling routines must
C     coordinate their use of the scratch area:  the scratch area is
C     effectively a global data structure.  Routines outside of the EK
C     system should not use the scratch area.
C
C     The EK scratch area routines are:
C
C        ZZEKSCA  ( EK scratch area umbrella routine )
C        ZZEKSTOP ( EK scratch area, return stack pointer )
C        ZZEKSPSH ( EK scratch area, push data onto stack )
C        ZZEKSDEC ( EK scratch area, decrement stack pointer )
C        ZZEKSPOP ( EK scratch area, pop data from stack )
C        ZZEKSUPD ( EK scratch area, update data )
C        ZZEKSRD  ( EK scratch area, read data )
C        ZZEKSCLN ( EK scratch area, clean up )
C
C$ Examples
C
C     1)  Push data on the scratch area stack.
C
C            C
C            C     Push N items onto the stack.
C            C
C                  CALL ZZEKSPSH ( N, DATA )
C
C
C     2)  Update a range of addresses that may span the stack top.
C
C            C
C            C     Since we can't leave a gap between the stack top
C            C     and the start of the range of addresses we write to,
C            C     we'll need to know where the top is.  The address
C            C     range to update is BEG:END.
C            C
C                  CALL ZZEKSTOP ( TOP )
C
C                  IF ( BEG .GT. TOP ) THEN
C
C                     [ Handle error case ]
C
C                  ELSE
C
C                     CALL ZZEKSUPD ( BEG, END, DATA )
C
C                  END IF
C
C
C
C     3)  Read from the scratch area.
C
C            C
C            C     Read the contents of the scratch area address
C            C     range BEG:END into the integer array DATA:
C            C
C                  CALL ZZEKSTOP ( TOP )
C
C                  IF ( BEG .GT. TOP ) THEN
C
C                     [ Handle error case ]
C
C                  ELSE
C
C                     CALL ZZEKSRD ( BEG, END, DATA )
C
C                  END IF
C
C$ Restrictions
C
C     1)  The current implementation of this suite of routines opens
C         a scratch file.  The logical unit connected to the scratch
C         file counts against the total that may be used by the calling
C         program.  Also, the scratch file, if written to, will occupy
C         additional disk storage.
C
C     2)  This suite of routines should not be used by routines outside
C         of the EK system.
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
C-    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB)
C
C        Increased buffer size from 500K to 2M integers.
C
C-    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB)
C
C        Added DASWBR call to entry point ZZEKCLN.  This call frees
C        the buffer records used by the scratch file.
C
C-    SPICELIB Version 3.0.0, 13-DEC-2001 (NJB)
C
C        Added entry point ZZEKCLN.
C
C-    Beta Version 2.0.0, 02-NOV-1995 (NJB)
C
C        Updated for EK architecture 3.
C
C-    Beta Version 1.1.0, 01-AUG-1994 (NJB)
C
C        Scratch area buffer size increased to 500K integers.
C        On 32-bit systems, this amounts to 2Mb of storage.
C
C-    Beta Version 1.0.1, 25-FEB-1993 (NJB)
C
C        Documented.
C
C-    Beta Version 1.0.0, 16-DEC-1992 (NJB)
C
C-&
 
C$ Index_Entries
C
C     manage the EK scratch area
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB)
C
C        Increased buffer size from 500K to 2M integers.
C
C-    Beta Version 2.0.0, 08-SEP-1994 (NJB)
C
C        Updated for EK architecture 3.
C
C-    Beta Version 1.1.0, 01-AUG-1994 (NJB)
C
C        Scratch area buffer size increased to 500K integers.
C        On 32-bit systems, this amounts to 2Mb of storage.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
 
C
C     The parameter MEMSIZ is the size of an integer array used as
C     part of the scratch area.  The first MEMSIZ scratch area addresses
C     refer to elements of this array.  Additional storage is supplied
C     by the integer logical array of a scratch DAS file; the first
C     word of the scratch DAS file corresponds to scratch area address
C     MEMSIZ + 1.
C
      INTEGER               MEMSIZ
      PARAMETER           ( MEMSIZ = 2500000 )
 
 
 
C
C     Local variables
C
      INTEGER               B
      INTEGER               BASE
      INTEGER               E
      INTEGER               I
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               NUMADD
      INTEGER               NUMRD
      INTEGER               RB
      INTEGER               REMAIN
      INTEGER               RT
      INTEGER               SCRHAN
      INTEGER               SCRTCH ( MEMSIZ )
      INTEGER               START
      INTEGER               T
 
      LOGICAL               FIRST
 
 
C
C     Saved variables
C
      SAVE
 
C
C     Initial values
C
      DATA                  FIRST  / .TRUE.  /
      DATA                  T      /  0      /
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKSCA' )
      END IF
 
C
C     This routine should never be called directly.
C
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
 
      CALL CHKOUT ( 'ZZEKSCA' )
      RETURN
 
 
 
 
C$Procedure    ZZEKSTOP  ( EK scratch area, stack top )
 
      ENTRY ZZEKSTOP ( TOP )
 
C$ Abstract
C
C     Obtain last address in use in EK scratch area.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               TOP
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TOP        O   EK scratch area stack top.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     TOP            is the last address of the EK scratch area stack
C                    top.  This is the highest EK scratch area address
C                    currently in use.
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
C     The EK scratch area stack top ranges from zero to, theoretically,
C     the largest integer representable on the host system.
C     and never decreases during a program run.  Data pushed on the
C     EK stack is inserted at address TOP+1 and occupies a contiguous
C     range of addresses that extends upwards from this address.
C
C$ Examples
C
C     See the header of the umbrella routine ZZEKSCA for an example
C     of use of this routine.
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
C-    Beta Version 2.0.0, 08-SEP-1994 (NJB)
C
C        Updated for EK architecture 3.
C
C-    Beta Version 1.0.0, 25-FEB-1993 (NJB)
C
C-&
 
C$ Index_Entries
C
C     read from EK scratch area
C
C-&
 
      TOP  =  T
      RETURN
 
 
 
 
 
 
C$Procedure    ZZEKSPSH  ( EK scratch area, push data )
 
      ENTRY ZZEKSPSH ( N, IDATA )
 
C$ Abstract
C
C     Push the contents of an integer array onto the EK scratch area
C     stack.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               N
C     INTEGER               IDATA ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of integers to push.
C     IDATA      I   Integer data.
C
C$ Detailed_Input
C
C     N              is the number of integers in the array IDATA to
C                    append to the EK scratch area.  The data is
C                    stored in scratch area addresses T+1:T+N,
C                    where T is the EK scratch area stack top prior to
C                    the call to ZZEKSPSH.
C
C     IDATA          is an integer array containing data to append to
C                    the EK scratch area.  The first N elements of
C                    IDATA are appended to the EK scratch area, in
C                    order.
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
C     1)  If N is non-positive, this routine simply returns.  No error
C         is signalled.
C
C     2)  If an I/O error occurs during the data addition, the error
C         will be diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let TOP be the EK scratch area stack top prior to a call to this
C     routine.  Data that is appended to the EK scratch area by this
C     routine is inserted at address TOP+1 and occupies a contiguous
C     range of addresses that extends upwards from this address.
C
C     As a side effect of calling this routine, TOP is set to TOP + N.
C
C$ Examples
C
C     See the header of the umbrella routine ZZEKSCA for an example
C     of use of this routine.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
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
C-    Beta Version 1.0.0, 15-JAN-1995 (NJB)
C
C-&
 
 
 
C$ Index_Entries
C
C     push integer data onto EK scratch area stack
C
C-&
 
C
C     No checking in here.
C
 
C
C     First time through, open a scratch DAS file.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL DASOPS ( SCRHAN )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
      END IF
 
C
C     Go back if there's no data to write.
C
      IF ( N .LT. 1 ) THEN
         RETURN
      END IF
 
C
C     Add as much data as possible to our big array.
C
      IF ( T .LT. MEMSIZ ) THEN
 
         NUMADD = MIN ( N, MEMSIZ-T )
 
         DO I = 1, NUMADD
            SCRTCH(T+I) = IDATA(I)
         END DO
 
         T      = T + NUMADD
 
         IF ( NUMADD .EQ. N ) THEN
            RETURN
         END IF
 
         REMAIN = N - NUMADD
         START  = 1 + NUMADD
 
         IF ( REMAIN .EQ. 0 ) THEN
            RETURN
         END IF
 
      ELSE
 
         REMAIN = N
         START  = 1
 
      END IF
C
C     At this point, REMAIN and START are set, and T reflects the
C     amount of data we've pushed so far.  If we got this far,
C     we'll need to put the rest of the data in the scratch DAS.
C
C     The DAS system requires separate operations for updating
C     an existing range of addresses and for appending data.
C     We need to know the last integer address in use in the DAS
C     file in order to determine which part of the data will
C     be written to addresses previously written to, and which
C     part will be appended.
C
      CALL DASLLA ( SCRHAN, LASTC, LASTD, LASTI )
 
C
C     To simplify our arithmetic, we'll work with a variable RT
C     that represents the stack top measured relative to the base
C     of the DAS integer array.  At this point, RT is greater than
C     or equal to zero.
C
      RT  =  T - MEMSIZ
 
      IF ( RT .LT. LASTI ) THEN
C
C        Some data can be added by updating DAS addresses.  The
C        available range for updating is B:E, where B and E are
C        calculated below.  This case can occur only when LASTI > 0.
C
         B       =  RT + 1
         E       =  MIN ( LASTI,  RT + REMAIN )
 
         CALL DASUDI ( SCRHAN, B, E, IDATA(START) )
 
         NUMADD  =  E      - B + 1
         START   =  START  + NUMADD
         REMAIN  =  REMAIN - NUMADD
         T       =  T      + NUMADD
 
         IF ( REMAIN .EQ. 0 ) THEN
            RETURN
         END IF
 
      END IF
 
C
C     At this point, START and REMAIN are set, and T reflects the
C     amount of data we've pushed so far..  The remaining data
C     must be appended to the scratch DAS file.
C
      CALL DASADI ( SCRHAN, REMAIN, IDATA(START) )
 
      T = T + REMAIN
 
      RETURN
 
 
 
 
C$Procedure    ZZEKSPOP  ( EK scratch area, pop data )
 
      ENTRY ZZEKSPOP ( N, IDATA )
 
C$ Abstract
C
C     Pop a specified number of elements from the top of the EK scratch
C     area stack, transferring this data to an integer array.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               N
C     INTEGER               IDATA ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of integers to pop.
C     IDATA      O   Integer data.
C
C$ Detailed_Input
C
C     N              is the number of integers to pop from the
C                    EK scratch area stack.  The data is
C                    read from the scratch area addresses T-N+1:T,
C                    where T is the stack top prior to the call to
C                    ZZEKSPOP.
C
C$ Detailed_Output
C
C     IDATA          is an integer array containing data read from
C                    the EK scratch area.  The first N elements of
C                    IDATA assigned the values occupying the top N
C                    elements of the EK stack.
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If N is non-positive or if N is greater than the number of
C         items on the stack, the error SPICE(INVALIDCOUNT) is
C         signalled.
C
C     2)  If an I/O error occurs during the data read, the error
C         will be diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let TOP be the EK scratch area stack top prior to a call to this
C     routine.  Data that is read from the EK scratch area by this
C     routine is transferred from addresses TOP-N+1 to TOP and occupies
C     to the range of addresses 1 to N in the array IDATA.
C
C     As a side effect of calling this routine, TOP is set to TOP - N.
C
C$ Examples
C
C     See the header of the umbrella routine ZZEKSCA for an example
C     of use of this routine.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
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
C-    Beta Version 1.0.0, 10-SEP-1994 (NJB)
C
C-&
 
 
 
C$ Index_Entries
C
C     pop integer data from EK scratch area stack
C
C-&
 
C
C     No checking in here.
C
 
C
C     First time through, open a scratch DAS file.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL DASOPS ( SCRHAN )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
      END IF
 
C
C     You can't pop a negative number of elements.
C
      IF ( N .LT. 0 ) THEN
 
         CALL CHKIN  ( 'ZZEKSPOP'                             )
         CALL SETMSG ( 'Pop count must be non-negative; '    //
     .                 'call requests popping # elements.'    )
         CALL ERRINT ( '#', N                                 )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                  )
         CALL CHKOUT ( 'ZZEKSPOP'                             )
         RETURN
 
C
C     It's an error to try to pop more data than we have on the
C     stack.
C
      ELSE IF ( N .GT. T ) THEN
 
         CALL CHKIN  ( 'ZZEKSPOP'                                      )
         CALL SETMSG ( 'EK stack pointer = #; call requests popping ' //
     .                 '# items.'                                      )
         CALL ERRINT ( '#', T                                          )
         CALL ERRINT ( '#', N                                          )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                           )
         CALL CHKOUT ( 'ZZEKSPOP'                                      )
         RETURN
 
      END IF
 
 
C
C     Read as much data as possible from our big array.
C
      BASE  = T - N
 
      IF ( BASE .LT. MEMSIZ ) THEN
 
         NUMRD = MIN ( N, MEMSIZ-BASE )
 
         DO I = 1, NUMRD
            IDATA(I) = SCRTCH(BASE+I)
         END DO
 
         IF ( NUMRD .EQ. N ) THEN
 
            T  =  T - NUMRD
            RETURN
 
         END IF
 
         REMAIN = N      -  NUMRD
         BASE   = MEMSIZ
         START  = NUMRD  +  1
 
      ELSE
 
         REMAIN = N
         START  = 1
 
      END IF
 
C
C     At this point, REMAIN, START and BASE are set.  If we got this
C     far, we'll need to read the rest of the data from the scratch DAS.
C     Compute the base address to read from relative to the start of
C     the DAS array.
C
      RB     =  BASE - MEMSIZ
      B      =  RB   + 1
      E      =  RB   + REMAIN
 
      CALL DASRDI ( SCRHAN, B, E, IDATA(START) )
 
      T      =  T - N
 
      RETURN
 
 
 
 
 
 
C$Procedure    ZZEKSDEC  ( EK scratch area, decrement stack pointer )
 
      ENTRY ZZEKSDEC ( N )
 
C$ Abstract
C
C     Decrement the EK scratch area stack pointer by a specified count.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               N
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Decrement count.
C
C$ Detailed_Input
C
C     N              is the number to subtract from the EK scratch
C                    area stack pointer.
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
C     1)  If N is non-positive or if N is greater than the number of
C         items on the stack, the error SPICE(INVALIDCOUNT) is
C         signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let TOP be the EK scratch area stack top prior to a call to this
C     routine.  The  effect of calling this routine is that TOP is set
C     to TOP - N.
C
C$ Examples
C
C     See the header of the umbrella routine ZZEKSCA for an example
C     of use of this routine.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
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
C-    Beta Version 1.0.0, 10-SEP-1994 (NJB)
C
C-&
 
 
 
C$ Index_Entries
C
C     decrement EK scratch area stack pointer
C
C-&
 
C
C     No checking in here.
C
 
C
C     First time through, open a scratch DAS file.
C
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         CALL DASOPS ( SCRHAN )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
      END IF
 
 
C
C     Catch non-positive decrement requests.
C
      IF ( N .LT. 0 ) THEN
 
         CALL CHKIN  ( 'ZZEKSDEC'                               )
         CALL SETMSG ( 'Decrement value must be non-negative; ' //
     .                 'call requests decrement by #.'          )
         CALL ERRINT ( '#', N                                   )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'ZZEKSDEC'                               )
         RETURN
 
C
C     It's an error to try to decrement the pointer by more than
C     the current stack depth.
C
      ELSE IF ( N .GT. T ) THEN
 
         CALL CHKIN  ( 'ZZEKSDEC'                               )
         CALL SETMSG ( 'EK stack pointer = #; call requests  ' //
     .                 'decrement by #.'                        )
         CALL ERRINT ( '#', T                                   )
         CALL ERRINT ( '#', N                                   )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'ZZEKSDEC'                               )
         RETURN
 
      END IF
 
 
      T  =  T  -  N
 
      RETURN
 
 
 
 
C$Procedure    ZZEKSUPD  ( EK scratch area, update )
 
      ENTRY ZZEKSUPD ( BEG, END, IDATA )
 
C$ Abstract
C
C     Update the contents of a range of addresses already in use in the
C     EK scratch area.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BEG
C     INTEGER               END
C     INTEGER               IDATA ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BEG,
C     END        I   Begin and end addresses of range to update.
C     IDATA      I   Integer data.
C
C$ Detailed_Input
C
C     BEG,
C     END            are the first and last of a range of EK scratch
C                    area addresses to write to.  BEG and END must
C                    satisfy the relations
C
C                       1  <  BEG  <  END  <  TOP
C                          -       -       -
C
C                    where TOP is the last EK scratch area stack top
C                    at the time this routine is called.
C
C     IDATA          is an integer array containing data to write to
C                    the specified range of addresses in the EK scratch
C                    area.  The first END-BEG+1 elements of IDATA are
C                    written to the specified range in the EK scratch
C                    area, in order.
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
C     1)  If either of BEG or END are outside of the range 1:TOP,
C         where TOP is the EK scratch area stack top, the error
C         SPICE(INVALIDADDRESS) is signalled.
C
C     2)  If END < BEG, this routine simply returns.  No error
C         is signalled.
C
C     3)  If an I/O error occurs during the data addition, the error
C         will be diagnosed by routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let TOP be the EK scratch area stack top prior to a call to this
C     routine.  This routine is used to modify values in the scratch
C     area that lie in the address range 1:TOP.
C
C$ Examples
C
C     See the header of the umbrella routine ZZEKSCA for an example
C     of use of this routine.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
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
C-    Beta Version 2.0.0, 23-FEB-1995 (NJB)
C
C        Updated for EK architecture 3.
C
C-    Beta Version 1.0.0, 25-FEB-1993 (NJB)
C
C-&
 
C$ Index_Entries
C
C     update data in EK scratch area
C
C-&
 
 
 
C
C     No checking in here.
C
 
C
C     Validate the addresses.
C
      IF (  ( BEG .LT. 1 ) .OR. ( BEG .GT. T )  ) THEN
 
         CALL CHKIN  ( 'ZZEKSUPD'                                    )
         CALL SETMSG ( 'Start address BEG was #; valid range is 1:#' )
         CALL ERRINT ( '#',  BEG                                     )
         CALL ERRINT ( '#',  T                                       )
         CALL SIGERR ( 'SPICE(INVALIDADDRESS)'                       )
         CALL CHKOUT ( 'ZZEKSUPD'                                    )
         RETURN
 
      ELSE IF (  ( END .LT. 1 ) .OR. ( END .GT. T  )  ) THEN
 
         CALL CHKIN  ( 'ZZEKSUPD'                                    )
         CALL SETMSG ( 'End address END was #; valid range is 1:#'   )
         CALL ERRINT ( '#',  END                                     )
         CALL ERRINT ( '#',  T                                       )
         CALL SIGERR ( 'SPICE(INVALIDADDRESS)'                       )
         CALL CHKOUT ( 'ZZEKSUPD'                                    )
         RETURN
 
      ELSE IF ( BEG .GT. END ) THEN
         RETURN
      END IF
 
 
      IF ( END .LE. MEMSIZ ) THEN
C
C        If the entire range is in memory, fine.  Update the range
C        now.
C
         DO I = BEG, END
            SCRTCH(I)  =  IDATA(I-BEG+1)
         END DO
 
 
      ELSE IF ( BEG .LE. MEMSIZ ) THEN
C
C        Update the portion of the address range that's in memory.
C
         DO I = BEG, MEMSIZ
            SCRTCH(I)  =  IDATA(I-BEG+1)
         END DO
 
C
C        Now update the rest of the range, which is in the scratch
C        DAS file.
C
         CALL DASUDI  ( SCRHAN,
     .                  1,
     .                  END-MEMSIZ,
     .                  IDATA( MEMSIZ-BEG+2 )  )
 
      ELSE
C
C        The whole range is in the DAS file.
C
         CALL DASUDI ( SCRHAN,  BEG-MEMSIZ,  END-MEMSIZ,  IDATA )
 
      END IF
 
      RETURN
 
 
 
 
 
 
C$Procedure    ZZEKSRD  ( EK scratch area, read )
 
      ENTRY ZZEKSRD ( BEG, END, IDATA )
 
C$ Abstract
C
C     Read from a range of addresses in the EK scratch area.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     INTEGER               BEG
C     INTEGER               END
C     INTEGER               IDATA ( * )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BEG,
C     END        I   Begin and end addresses of range to read from.
C     IDATA      O   Integer data.
C
C$ Detailed_Input
C
C     BEG,
C     END            are the first and last of a range of EK scratch
C                    area addresses to read from. BEG and END must
C                    satisfy the relations
C
C                       1  <  BEG  <  END  <  LAST
C                          -       -       -
C
C                    where LAST is the last EK scratch area address
C                    in use at the time this routine is called.
C
C$ Detailed_Output
C
C     IDATA          is an integer array containing data read from the
C                    range of addresses BEG:END in the EK scratch area.
C                    The first END-BEG+1 elements of IDATA are assigned
C                    in order using the contents of this address range.
C                    IDATA must have dimension at least END-BEG+1.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either of BEG or END are outside of the range 1:LAST,
C         where LAST is the last address already in use in the EK
C         scratch area, the error SPICE(INVALIDADDRESS) is signalled.
C
C     2)  If END < BEG, this routine simply returns.  No error
C         is signalled.
C
C     3)  If an I/O error occurs during the read, the error will be
C         diagnosed by routines called by this routine.
C
C     4)  If IDATA has dimension less than END-BEG+1, the results of
C         a call to this routine will be unpredictable, except that
C         you can safely predict they'll be wrong.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Let LAST be the last address in use in the EK scratch area prior
C     to a call to this routine.  This routine is used to read values
C     in the scratch area that lie in the address range 1:LAST.
C
C$ Examples
C
C     See the header of the umbrella routine ZZEKSCA for an example
C     of use of this routine.
C
C$ Restrictions
C
C     1)  This routine must execute quickly.  Therefore, it checks in
C         only if it detects an error.  If an error is signalled by a
C         routine called by this routine, this routine will not appear
C         in the SPICELIB traceback display.  Also, in the interest
C         of speed, this routine does not test the value of the SPICELIB
C         function RETURN upon entry.
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
C-    Beta Version 1.0.0, 23-FEB-1995 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     read from EK scratch area
C
C-&
 
C
C     No checking in here.
C
 
C
C     Validate the addresses.
C
      IF (  ( BEG .LT. 1 ) .OR. ( BEG .GT. T  )  ) THEN
 
         CALL CHKIN  ( 'ZZEKSRD'                                     )
         CALL SETMSG ( 'Start address BEG was #; valid range is 1:#' )
         CALL ERRINT ( '#',  BEG                                     )
         CALL ERRINT ( '#',  T                                       )
         CALL SIGERR ( 'SPICE(INVALIDADDRESS)'                       )
         CALL CHKOUT ( 'ZZEKSRD'                                     )
         RETURN
 
      ELSE IF (  ( END .LT. 1 ) .OR. ( END .GT. T  )  ) THEN
 
         CALL CHKIN  ( 'ZZEKSRD'                                     )
         CALL SETMSG ( 'End address END was #; valid range is 1:#'   )
         CALL ERRINT ( '#',  END                                     )
         CALL ERRINT ( '#',  T                                       )
         CALL SIGERR ( 'SPICE(INVALIDADDRESS)'                       )
         CALL CHKOUT ( 'ZZEKSRD'                                     )
         RETURN
 
      ELSE IF ( BEG .GT. END ) THEN
         RETURN
      END IF
 
 
      IF ( END .LE. MEMSIZ ) THEN
C
C        If the entire range is in memory, fine.  Read from the range
C        now.
C
         DO I = BEG, END
            IDATA(I-BEG+1)  =  SCRTCH(I)
         END DO
 
 
      ELSE IF ( BEG .LE. MEMSIZ ) THEN
C
C        Read from the portion of the address range that's in memory.
C
         DO I = BEG, MEMSIZ
            IDATA(I-BEG+1)  =  SCRTCH(I)
         END DO
 
C
C        Now read the rest of the range, which is in the scratch
C        DAS file.
C
         CALL DASRDI  ( SCRHAN,
     .                  1,
     .                  END-MEMSIZ,
     .                  IDATA( MEMSIZ-BEG+2 )  )
 
      ELSE
C
C        The whole range is in the DAS file.
C
         CALL DASRDI ( SCRHAN,  BEG-MEMSIZ,  END-MEMSIZ,  IDATA )
 
      END IF
 
      RETURN



C$Procedure    ZZEKSCLN  ( EK scratch area, clean up )
 
      ENTRY ZZEKSCLN
 
C$ Abstract
C
C     Clean up:  re-initialize the EK scratch area; unload the
C     scratch file.
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
C     EK
C
C$ Keywords
C
C     EK
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Files
C
C     This routine unloads the scratch DAS used by this system.
C
C$ Particulars
C
C     This routine is intended to enable test software to unload
C     the scratch DAS file used by the EK scratch area routines.     
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) Many EK routines operate by side effects on the EK scratch
C        area, so this routine must be used with caution.
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
C-    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB)
C
C        Added DASWBR call.  This call frees the buffer records used by
C        the scratch file.
C
C-    SPICELIB Version 3.0.0, 27-DEC-2001 (NJB)
C
C-&
 
 
C$ Index_Entries
C
C     clean up EK scratch area
C
C-&
 
C
C     No checking in here.
C
 
C
C     Clean out the stack buffer.
C     
      CALL CLEARI ( MEMSIZ, SCRTCH )
      T  = 0

C
C     If FIRST has been set to .FALSE., we've an open scratch DAS
C     to dispose of.
C      
      IF ( .NOT. FIRST ) THEN
C
C        Write out the buffered records belonging to the scratch file; 
C        this will cause them to be returned to the free list.
C
         CALL DASWBR ( SCRHAN )

C
C        Dump the scratch DAS.
C     
         CALL DASLLC ( SCRHAN )
 
      END IF

C
C     Tell the system to re-initialize on the next pass.
C
      FIRST = .TRUE.

      RETURN
      END
