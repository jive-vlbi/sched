C$Procedure CKMP06 ( C-kernel, get mini-segment parameters, type 06 )
 
      SUBROUTINE CKMP06 ( HANDLE, DESCR,  MSNO, RATE,
     .                    SUBTYP, WINSIZ, NREC, IVLBDS, LSTEPC )
      IMPLICIT NONE
 
C$ Abstract
C
C     Return the mini-segment control parameters, mini-segment interval
C     bounds, and last epoch for a specified mini-segment in a type 6
C     CK segment.
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
C     CK
C     DAF
C
C$ Keywords
C
C     POINTING
C
C$ Declarations
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR  ( * )
      INTEGER               MSNO
      DOUBLE PRECISION      RATE
      INTEGER               SUBTYP
      INTEGER               WINSIZ
      INTEGER               NREC
      DOUBLE PRECISION      IVLBDS ( 2 )
      DOUBLE PRECISION      LSTEPC
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   The handle of the file containing the segment.
C     DESCR      I   The descriptor of the type 6 segment.
C     MSNO       I   Mini-segment index.
C     RATE       O   SCLK rate in seconds/tick.
C     SUBTYP     O   Subtype code.
C     WINSIZ     O   Interpolation window size.
C     NREC       O   Number of records in mini-segment.
C     IVLBDS     O   Mini-segment interval bounds of mini-segment.
C     LSTEPC     O   Last epoch of mini-segment.
C
C$ Detailed_Input
C
C     HANDLE     is the handle of the binary CK file containing the
C                segment. Normally the CK file should be open for
C                read access. See the Files section below for details.
C
C     DESCR      is the DAF descriptor of a CK data type 6 segment.
C
C     MSNO       is the index of a mini-segment within the segment
C                identified by HANDLE and DESCR. 
C
C$ Detailed_Output
C
C     RATE       is the spacecraft clock rate for the specified
C                mini-segment. RATE has units of seconds/tick.
C
C     SUBTYP     is the subtype code of the specified mini-segment.
C     
C     WINSIZ     is the interpolation window size for the specified
C                mini-segment. The window size is 
C
C                   ( DEGREE + 1 ) / 2     for subtypes 0 and 2
C                     DEGREE + 1           for subtypes 1 and 3
C
C                where DEGREE is the interpolation degree of the
C                mini-segment.
C
C     NREC       is the number of data records in the CK mini-segment
C                identified by HANDLE, DESCR, and MSNO
C
C     IVLBDS     is a two-element array containing, in order, the
C                encoded SCLK start and stop times of the coverage
C                interval of the specified mini-segment. The
C                mini-segment provides data for times within this
C                interval.               
C
C     LSTEPC     is the last epoch in the sequence of time tags
C                belonging to the specified mini-segment. LSTEPC is
C                an encoded SCLK time. 
C
C                If LSTEPC precedes IVLBDS(2), the mini-segment has
C                a coverage gap between those two epochs.
C
C$ Parameters
C
C     See the include file ck06.inc for a description of CK type 6
C     subtypes.
C
C$ Exceptions
C
C     1)  If the segment indicated by DESCR is not a type 6 segment,
C         the error 'SPICE(CKWRONGDATATYPE)' is signaled.
C
C     2)  If the specified handle does not belong to any DAF file that
C         is currently known to be open, an error is diagnosed by a
C         routine that this routine calls.
C
C     3)  If DESCR is not a valid descriptor of a valid segment in the
C         CK file specified by HANDLE, the results of this routine are
C         unpredictable.
C 
C     4)  If N is less than 1 or greater than the number of
C         mini-segments in the specified segment, the error
C         SPICE(INDEXOUTOFRANGE) is signaled.
C
C$ Files
C
C     The CK file specified by HANDLE may be open for read or write
C     access. Normally, the file should have been opened for read
C     access. If the file is open for write access, the calling
C     application must ensure integrity of the CK segment being read.
C     If the structure of the segment is invalid---for example, if the
C     segment has been partially written---this routine will either
C     return invalid results, or it will cause a system-level runtime
C     error.
C
C$ Particulars
C
C     For a complete description of the internal structure of a type 6
C     segment, see the CK Required Reading.
C
C     This routine is normally used in conjunction with CKNM06 and
C     CKGR06 to obtain time tags and packet data from a specified type
C     6 CK segment.
C
C$ Examples
C
C     The numerical results shown for this example may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine 
C     specific arithmetic implementation. 
C
C
C     1) The following program dumps records from a CK file that
C        contains only type 6 segments.
C
C
C        Example code begins here.
C
C
C             PROGRAM GREX1
C             IMPLICIT NONE
C       C
C       C     Dump all records from a CK that 
C       C     contains only segments of type 6.
C       C
C
C             INCLUDE 'ck06.inc'
C       C
C       C     Local parameters
C       C
C             INTEGER               ND
C             PARAMETER           ( ND     = 2 )
C
C             INTEGER               NI
C             PARAMETER           ( NI     = 6 )
C
C             INTEGER               DSCSIZ
C             PARAMETER           ( DSCSIZ = 5 )
C
C             INTEGER               FILSIZ
C             PARAMETER           ( FILSIZ = 255 )
C
C       C
C       C     RECSIZ is the size of the largest pointing
C       C     record, which corresponds to subtype 2.
C       C
C             INTEGER               RECSIZ
C             PARAMETER           ( RECSIZ = C06PS2 + 3 )
C
C       C
C       C     Local variables
C       C
C             CHARACTER*(FILSIZ)    CK
C
C             DOUBLE PRECISION      DC     ( ND )
C             DOUBLE PRECISION      DESCR  ( DSCSIZ )
C             DOUBLE PRECISION      IVLBDS ( 2 )
C             DOUBLE PRECISION      LSTEPC
C             DOUBLE PRECISION      RATE
C             DOUBLE PRECISION      RECORD ( RECSIZ )
C
C             INTEGER               DTYPE
C             INTEGER               HANDLE
C             INTEGER               IC     ( NI )
C             INTEGER               RECNO
C             INTEGER               MSNO
C             INTEGER               NMINI
C             INTEGER               NREC
C             INTEGER               SEGNO
C             INTEGER               SUBTYP
C             INTEGER               WINSIZ
C
C             LOGICAL               FOUND
C
C
C             CALL PROMPT ( 'Enter name of CK to dump > ', CK )
C
C             CALL DAFOPR ( CK, HANDLE )
C       C
C       C     Dump data from each CK segment.
C       C
C             SEGNO = 0
C
C             CALL DAFBFS ( HANDLE )
C             CALL DAFFNA ( FOUND  )
C
C             DO WHILE ( FOUND )
C
C                SEGNO = SEGNO + 1
C
C                WRITE (*,*) ' '
C                WRITE (*,*) ' '
C                WRITE (*,*) 'Segment number: ', SEGNO
C
C       C
C       C        Fetch and unpack the descriptor of the
C       C        current segment; check the data type.
C       C
C                CALL DAFGS ( DESCR )
C                CALL DAFUS ( DESCR, ND, NI, DC, IC )
C
C                DTYPE = IC(3)
C
C                IF ( DTYPE .NE. 6 ) THEN
C
C                   CALL SETMSG ( 'Data type must be 6 but was #.' )
C                   CALL ERRINT ( '#',  DTYPE                      )
C                   CALL SIGERR ( 'SPICE(NOTSUPPORTED)'            )
C
C                END IF
C       C
C       C        Get the mini-segment count for this
C       C        segment.
C       C
C                CALL CKNM06 ( HANDLE, DESCR, NMINI )
C       C
C       C        Dump data from each mini-segment.
C       C
C                DO MSNO = 1, NMINI
C       C
C       C           Get the mini-segment's record count
C       C           and time bounds.
C       C
C                   CALL CKMP06 ( HANDLE, DESCR, MSNO,   RATE,  SUBTYP,
C            .                    WINSIZ, NREC,  IVLBDS, LSTEPC        )
C
C                   WRITE (*,*) ' '
C                   WRITE (*,*) '   Mini-segment number: ', MSNO
C                   WRITE (*,*) '      Rate:           ',   RATE
C                   WRITE (*,*) '      Subtype:        ',   SUBTYP
C                   WRITE (*,*) '      Window size:    ',   WINSIZ
C                   WRITE (*,*) '      Interval start: ',   IVLBDS(1)
C                   WRITE (*,*) '      Interval stop:  ',   IVLBDS(2)
C                   WRITE (*,*) '      Last epoch:     ',   LSTEPC
C                   WRITE (*,*) ' '
C
C                   DO RECNO = 1, NREC
C
C                      CALL CKGR06 ( HANDLE, DESCR,
C            .                       MSNO,   RECNO,  RECORD )
C
C                      WRITE (*,*) '      Record number: ', RECNO
C                      WRITE (*,*) '         SCLKDP:     ', RECORD(1)
C                      WRITE (*,*) '         Clock rate: ', RECORD(3)
C
C                      IF ( SUBTYP .EQ. C06TP0 ) THEN
C
C                         WRITE (*,*) '         Q(0): ', RECORD(4)
C                         WRITE (*,*) '         Q(1): ', RECORD(5)
C                         WRITE (*,*) '         Q(2): ', RECORD(6)
C                         WRITE (*,*) '         Q(3): ', RECORD(7)
C                         WRITE (*,*) '    d Q(0)/dt: ', RECORD(8)
C                         WRITE (*,*) '    d Q(1)/dt: ', RECORD(9)
C                         WRITE (*,*) '    d Q(2)/dt: ', RECORD(10)
C                         WRITE (*,*) '    d Q(3)/dt: ', RECORD(11)
C
C                      ELSE IF ( SUBTYP .EQ. C06TP1 ) THEN
C
C                         WRITE (*,*) '         Q(0): ', RECORD(4)
C                         WRITE (*,*) '         Q(1): ', RECORD(5)
C                         WRITE (*,*) '         Q(2): ', RECORD(6)
C                         WRITE (*,*) '         Q(3): ', RECORD(7)
C
C                      ELSE IF ( SUBTYP .EQ. C06TP2 ) THEN
C
C                         WRITE (*,*) '         Q(0): ', RECORD(4)
C                         WRITE (*,*) '         Q(1): ', RECORD(5)
C                         WRITE (*,*) '         Q(2): ', RECORD(6)
C                         WRITE (*,*) '         Q(3): ', RECORD(7)
C                         WRITE (*,*) '    d Q(0)/dt: ', RECORD(8)
C                         WRITE (*,*) '    d Q(1)/dt: ', RECORD(9)
C                         WRITE (*,*) '    d Q(2)/dt: ', RECORD(10)
C                         WRITE (*,*) '    d Q(3)/dt: ', RECORD(11)
C                         WRITE (*,*) '        AV(1): ', RECORD(12)
C                         WRITE (*,*) '        AV(2): ', RECORD(13)
C                         WRITE (*,*) '        AV(3): ', RECORD(14)
C                         WRITE (*,*) '   d AV(1)/dt: ', RECORD(15)
C                         WRITE (*,*) '   d AV(2)/dt: ', RECORD(16)
C                         WRITE (*,*) '   d AV(3)/dt: ', RECORD(17)
C
C                      ELSE IF ( SUBTYP .EQ. C06TP3 ) THEN
C
C                         WRITE (*,*) '         Q(0): ', RECORD(4)
C                         WRITE (*,*) '         Q(1): ', RECORD(5)
C                         WRITE (*,*) '         Q(2): ', RECORD(6)
C                         WRITE (*,*) '         Q(3): ', RECORD(7)
C                         WRITE (*,*) '        AV(1): ', RECORD(8)
C                         WRITE (*,*) '        AV(2): ', RECORD(9)
C                         WRITE (*,*) '        AV(3): ', RECORD(10)
C
C                      ELSE
C                         CALL SETMSG ( 'Subtype # is not '
C            .            //            'recognized.'         )
C                         CALL ERRINT ( '#', SUBTYP           )
C                         CALL SIGERR ( 'SPICE(NOTSUPPORTED)' )
C                      END IF
C
C                      WRITE (*,*) ' '
C
C                  END DO
C
C                END DO
C
C                CALL DAFFNA ( FOUND )
C
C             END DO
C
C             END
C
C
C     An initial portion of the output created by this program, when
C     the program was executed on a PC/Linux/gfortran platform, for
C     a sample CK containing type 6 segments, is shown below:
C
C
C        Segment number:            1
C
C           Mini-segment number:            1
C              Rate:             1.52587890625000000E-005
C              Subtype:                   1
C              Window size:              10
C              Interval start:    11288914762710.869
C              Interval stop:     11290384616127.203
C              Last epoch:        11290384616127.203
C
C              Record number:            1
C                 SCLKDP:        11288914762710.869
C                 Clock rate:   1.52587890625000000E-005
C                 Q(0):   0.46164827229286126
C                 Q(1):  -0.70575355403199758
C                 Q(2):  -0.29319084125475281
C                 Q(3):  -0.45036865373250068
C
C              Record number:            2
C                 SCLKDP:        11288998883607.230
C                 Clock rate:   1.52587890625000000E-005
C                 Q(0):   0.46162099825988423
C                 Q(1):  -0.70570932104748119
C                 Q(2):  -0.29323821047130305
C                 Q(3):  -0.45043507864268195
C
C              Record number:            3
C                 SCLKDP:        11289077342579.063
C                 Clock rate:   1.52587890625000000E-005
C                 Q(0):   0.46159435700379842
C                 Q(1):  -0.70566850733850173
C                 Q(2):  -0.29328062251407311
C                 Q(3):  -0.45049870564815003
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
C     N.J. Bachman     (JPL)
C     J.M. Lynch       (JPL)
C     B.V. Semenov     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (JML) (BVS)
C
C-&
 
C$ Index_Entries
C
C     get mini-segment parameters from ck type_6 segment
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
C        ND         is the number of double precision components in an
C                   unpacked C-kernel descriptor.
C
C        NI         is the number of integer components in an unpacked
C                   C-kernel descriptor.
C
C        DTYPE      is the data type of the segment that this routine
C                   operates on.
C
 
      INTEGER               ND 
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI 
      PARAMETER           ( NI     = 6 )
 
      INTEGER               DTYPE
      PARAMETER           ( DTYPE  = 6 )

      INTEGER               CTRLSZ
      PARAMETER           ( CTRLSZ = 2 )

C
C     Mini-segment control area size:
C
      INTEGER               MCTLSZ
      PARAMETER           ( MCTLSZ = 4 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = MCTLSZ )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

C
C     Local variables
C 
      DOUBLE PRECISION      BUFFER ( BUFSIZ )
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DPDATA ( 1  )
 
      INTEGER               BADDR
      INTEGER               EADDR
      INTEGER               EPADDR
      INTEGER               IC     ( NI )
      INTEGER               MINIE
      INTEGER               NDIR
      INTEGER               NEPDIR
      INTEGER               NINTVL
      INTEGER               IVLBAS
      INTEGER               PTRBAS

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKMP06' )
 
C
C     The number of discrete pointing instances contained in a data
C     type 6 segment is stored in the last double precision word of the
C     segment. Since the address of the last word is stored in the
C     sixth integer component of the segment descriptor, it is a
C     trivial matter to extract the count.
C
C     The unpacked descriptor contains the following information about
C     the segment:
C
C        DC(1)  Initial encoded SCLK
C        DC(2)  Final encoded SCLK
C
C        IC(1)  CK frame class ID (aka "instrument")
C        IC(2)  Inertial reference frame
C        IC(3)  Data type
C        IC(4)  Angular velocity flag
C        IC(5)  Initial address of segment data
C        IC(6)  Final address of segment data
C
C
      CALL DAFUS ( DESCR, ND, NI, DC, IC )
 
C
C     If this segment is not of data type 6, then signal an error.
C 
      IF ( IC( 3 ) .NE. DTYPE ) THEN

         CALL SETMSG ( 'Data type of the segment should be 6: Passed '
     .   //            'descriptor shows type = #.'                   )
         CALL ERRINT ( '#', IC( 3 )                                   )
         CALL SIGERR ( 'SPICE(CKWRONGDATATYPE)'                       )
         CALL CHKOUT ( 'CKMP06'                                       )
         RETURN

      END IF
 
C
C     Check the mini-segment index.
C
C     The number of mini-segments is the final word in the segment.
C
      BADDR = IC(5)
      EADDR = IC(6)

      CALL DAFGDA ( HANDLE, EADDR, EADDR, DPDATA )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKMP06' )
         RETURN
      END IF
 
      NINTVL = NINT ( DPDATA(1) )

      IF (  ( MSNO .LT. 1 )  .OR.  ( MSNO .GT. NINTVL )  ) THEN

         CALL SETMSG ( 'Mini-segment index must be in range 1:# but '
     .   //            'was #.'                                       )
         CALL ERRINT ( '#', NINTVL                                    )
         CALL ERRINT ( '#', MSNO                                      )
         CALL SIGERR ( 'SPICE(INDEXOUTOFRANGE)'                       )
         CALL CHKOUT ( 'CKMP06'                                       )
         RETURN

      END IF

C
C     Set the base address of the mini-segment pointers. There are
C     NINTVL+1 pointers, and these precede the control area.
C
      PTRBAS = EADDR - CTRLSZ - ( NINTVL+1 )

C
C     Set the base address of the interval bounds. There are N+1
C     bounds, and these precede the interval bound directories and the
C     mini-segment pointers.
C
C     The directory count is
C
C        ( ( NINTVL + 1 ) - 1 ) / DIRSIZ
C
C
      NDIR   = NINTVL / DIRSIZ

      IVLBAS = PTRBAS - NDIR - ( NINTVL + 1 )

C
C     Fetch the interval bounds for the mini-segment of interest.
C 
      CALL DAFGDA ( HANDLE, IVLBAS+MSNO, IVLBAS+MSNO+1, IVLBDS ) 

C
C     Compute the mini-segment end pointer as an absolute DAF address.
C     The stored value is a relative address. Begin by looking up the
C     start pointer of the next mini-segment.
C
      CALL DAFGDA ( HANDLE, PTRBAS+MSNO+1, PTRBAS+MSNO+1, BUFFER )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'CKMP06' )
         RETURN
      END IF

      MINIE = ( BADDR - 1 )  +  NINT( BUFFER(1) )  -  1

C
C     Fetch the parameters for the mini-segment.
C
      CALL DAFGDA ( HANDLE, MINIE-MCTLSZ+1, MINIE, BUFFER )

      IF ( FAILED() ) THEN
         CALL CHKOUT( 'CKMP06' )
         RETURN
      END IF

      RATE   =       BUFFER(1)
      SUBTYP = NINT( BUFFER(2) )
      WINSIZ = NINT( BUFFER(3) )
      NREC   = NINT( BUFFER(4) )

C
C     The last epoch of the mini-segment precedes the mini-segment's
C     control area and the epoch directories.
C
      NEPDIR = ( NREC - 1 ) / DIRSIZ

      EPADDR = MINIE - MCTLSZ - NEPDIR

      CALL DAFGDA ( HANDLE, EPADDR, EPADDR, LSTEPC )

      CALL CHKOUT ( 'CKMP06' )
      RETURN
      END
