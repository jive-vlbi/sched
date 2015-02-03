C$Procedure      SPKSUB ( S/P Kernel, subset )
 
      SUBROUTINE SPKSUB ( HANDLE, DESCR, IDENT, BEGIN, END, NEWH )
 
C$ Abstract
C
C     Extract a subset of the data in an SPK segment into a
C     separate segment.
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
C
C$ Declarations
 
      INTEGER               HANDLE
      DOUBLE PRECISION      DESCR    ( 5 )
      CHARACTER*(*)         IDENT
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END
      INTEGER               NEWH
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of source segment.
C     DESCR      I   Descriptor of source segment.
C     IDENT      I   Identifier of source segment.
C     BEGIN      I   Beginning (initial epoch) of subset.
C     END        I   End (final epoch) of subset.
C     NEWH       I   Handle of new segment.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR,
C     IDENT       are the file handle assigned to a SPK file, the
C                 descriptor for a segment within the file, and the
C                 identifier for that segment. Together they determine
C                 a complete set of ephemeris data, from which a
C                 subset is to be extracted.
C
C     BEGIN,
C     END         are the initial and final epochs (ephemeris time)
C                 of the subset.
C
C     NEWH        is the file handle assigned to the file in which
C                 the new segment is to be written. The file must
C                 be open for write access. NEWH and HANDLE may refer
C                 to the same file.
C
C$ Detailed_Output
C
C     See $Files section.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the condition
C
C           ALPHA  <  BEGIN  <  END  <  OMEGA
C                  -         -       -
C
C        is not satisfied (where ALPHA and OMEGA are the initial
C        and final epochs of the segment respectively), the error
C        'SPICE(SPKNOTASUBSET)' is signaled.
C
C     2) If the segment type is not supported by the current
C        version of SPKSUB, the error 'SPICE(SPKTYPENOTSUPP)'
C        is signaled.
C
C$ Files
C
C     A new segment, which contains a subset of the data in the
C     segment specified by DESCR and HANDLE, is written to the SPK
C     file attached to NEWH.
C
C$ Particulars
C
C     Sometimes, the segments in official source files---planetary
C     Developmental Ephemeris (DE) files, archival spacecraft
C     ephemeris files, and so on---contain more data than is needed
C     by a particular user. SPKSUB allows a user to extract from a
C     segment the smallest amount of ephemeris data sufficient to
C     cover a specific interval.
C
C     The new segment is written with the same identifier as the
C     original segment, and with the same descriptor, with the
C     following components changed:
C
C     1)  ALPHA and OMEGA (DCD(1) and DCD(2)) are assigned the values
C         specified by BEGIN and END.
C
C     2)  The beginning and ending segment addresses (ICD(5) and ICD(6))
C         are changed to reflect the location of the new segment.
C
C$ Examples
C
C     In the following code fragment, the descriptor for each segment
C     in a source SPK file is examined. For each segment that covers a
C     specified time interval, the smallest possible subset of data
C     from that segment, sufficient to cover the interval, is extracted
C     into a custom SPK file.
C
C     Assume that the source and custom files have been opened, for
C     read and write access, with handles SRC and CUST respectively.
C
C        CALL DAFBFS ( SRC    )
C        CALL DAFFNA ( FOUND  )
C
C        DO WHILE ( FOUND )
C           CALL DAFGS ( DESCR )
C           CALL DAFUS ( DESCR, 2, 6, DC, IC )
C
C           IF ( DC(1) .LE. BEGIN  .AND.  END .LE. DC(2) ) THEN
C              CALL DAFGN  ( IDENT )
C              CALL SPKSUB ( SRC, DESCR, IDENT, BEGIN, END, CUST )
C           END IF
C
C           CALL DAFFNA ( FOUND )
C        END DO
C
C
C$ Restrictions
C
C     1) There is no way for SPKSUB to verify that the descriptor and
C        identifier are the original ones for the segment. Changing
C        the descriptor can cause the data in the new segment to be
C        evaluated incorrectly; changing the identifier can destroy
C        the path from the data back to its original source.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     K.R. Gehringer  (JPL)
C     W.L. Taber      (JPL)
C     N.J. Bachman    (JPL)
C     J.M. Lynch      (JPL)
C     R.E. Thurman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 9.0.0, 23-DEC-2013 (NJB)
C
C        The routine was updated to handle types 19, 20 and 21. Some
C        minor changes were made to comments.
C
C-    SPICELIB Version 8.0.0, 12-AUG-2002 (NJB)
C
C        The routine was updated to handle type 18.
C
C-    SPICELIB Version 7.0.0, 06-NOV-1999 (NJB)
C
C        The routine was updated to handle types 12 and 13.
C
C-    SPICELIB Version 6.0.0, 30-JUN-1997 (WLT)
C
C        The routine was updated to handle types 10 and 17.
C
C-    SPICELIB Version 5.0.0, 10-MAR-1995 (KRG)
C
C        The routine was updated to handle type 14.
C
C-    SPICELIB Version 4.0.0, 07-NOV-1994 (WLT)
C
C        The routine was updated to handle type 15.
C
C-    SPICELIB Version 3.0.0, 05-AUG-1993 (NJB)
C
C        The routine was updated to handle types 08 and 09.
C
C-    SPICELIB Version 2.0.0, 01-APR-1992 (JML)
C
C        1) The routine was updated to handle type 05.
C
C        2) DESCR was being used as both an input and output
C           variable when it was only supposed to be used for
C           input. A new local variable, NDSCR, was added where DESCR
C           was being altered.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (RET)
C
C-&
 
 
C$ Index_Entries
C
C     subset of spk file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 9.0.0, 23-DEC-2013 (NJB)
C
C        The routine was updated to handle types 19, 20 and 21. Some
C        minor changes were made to comments.
C
C-    SPICELIB Version 8.0.0, 12-AUG-2002 (NJB)
C
C        The routine was updated to handle type 18.
C
C-    SPICELIB Version 6.0.0, 30-JUN-1997 (WLT)
C
C        The routine was updated to handle types 10 and 17.
C
C-    SPICELIB Version 5.0.0, 10-MAR-1995 (KRG)
C
C        The routine was updated to handle type 14.
C
C-    SPICELIB Version 4.0.0, 07-NOV-1994 (WLT)
C
C        The routine was updated to handle type 15.
C
C-    SPICELIB Version 3.0.0, 05-AUG-1993 (NJB)
C
C        The routine was updated to handle types 08 and 09.
C
C-    SPICELIB Version 2.0.0, 01-APR-1992 (JML)
C
C        1) The routine was updated to handle type 05.
C
C        2) DESCR was being used as both an input and output
C           variable when it was only supposed to be used for
C           input. A new local variable, NDSCR, was added where DESCR
C           was being altered.
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      DC       ( 2 )
      DOUBLE PRECISION      NDSCR    ( 5 )
      DOUBLE PRECISION      OMEGA
 
      INTEGER               BADDR
      INTEGER               EADDR
      INTEGER               IC       ( 6 )
      INTEGER               TYPE
 
      LOGICAL               OKAY
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPKSUB' )
      END IF
 
C
C     Unpack the descriptor.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      ALPHA = DC(1)
      OMEGA = DC(2)
      TYPE  = IC(4)
      BADDR = IC(5)
      EADDR = IC(6)
 
C
C     Make sure the epochs check out.
C
      OKAY = (        ALPHA .LE. BEGIN
     .         .AND.  BEGIN .LE. END
     .         .AND.  END   .LE. OMEGA )
 
      IF ( .NOT. OKAY ) THEN
         CALL SETMSG ( 'Specified interval [#, #] is not a subset ' //
     .                 'of segment interval [#, #].' )
         CALL ERRDP  ( '#', BEGIN                    )
         CALL ERRDP  ( '#', END                      )
         CALL ERRDP  ( '#', ALPHA                    )
         CALL ERRDP  ( '#', OMEGA                    )
         CALL SIGERR ( 'SPICE(SPKNOTASUBSET)'        )
 
         CALL CHKOUT ( 'SPKSUB' )
         RETURN
      END IF
 
C
C     Begin the new segment, with a descriptor containing the subset
C     epochs.
C
      DC(1) = BEGIN
      DC(2) = END
      CALL DAFPS  ( 2, 6, DC, IC, NDSCR )
 
C
C     Let the type-specific (SPKSnn) routines decide what to move.
C
      IF ( TYPE .EQ. 01 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS01 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 02 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS02 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 03 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS03 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
C
C      Type 04 has not been yet been added to SPICELIB.
C
C      ELSE IF ( TYPE .EQ. 04 ) THEN
C         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
C         CALL SPKS04 ( HANDLE, BADDR, EADDR, BEGIN, END )
C         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 05 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS05 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 08 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS08 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 09 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS09 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 10 ) THEN
         CALL SPKS10 ( HANDLE, DESCR, NEWH, NDSCR, IDENT )
 
      ELSE IF ( TYPE .EQ. 12 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS12 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 13 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS13 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 14 ) THEN
         CALL SPKS14 ( HANDLE, DESCR, NEWH, NDSCR, IDENT )
 
      ELSE IF ( TYPE .EQ. 15 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS15 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 17 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS17 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE IF ( TYPE .EQ. 18 ) THEN
         CALL DAFBNA ( NEWH, NDSCR,  IDENT )
         CALL SPKS18 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 19 ) THEN
         CALL DAFBNA ( NEWH,   NDSCR, IDENT )
         CALL SPKS19 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 20 ) THEN
         CALL DAFBNA ( NEWH,   NDSCR, IDENT )
         CALL SPKS20 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA

      ELSE IF ( TYPE .EQ. 21 ) THEN
         CALL DAFBNA ( NEWH,   NDSCR, IDENT )
         CALL SPKS21 ( HANDLE, BADDR, EADDR, BEGIN, END )
         CALL DAFENA
 
      ELSE
         CALL SETMSG ( 'SPK data type # is not supported.' )
         CALL ERRINT ( '#', TYPE                           )
         CALL SIGERR ( 'SPICE(SPKTYPENOTSUPP)'             )
 
         CALL CHKOUT ( 'SPKSUB' )
         RETURN
      END IF
 
      CALL CHKOUT ( 'SPKSUB' )
      RETURN
      END
