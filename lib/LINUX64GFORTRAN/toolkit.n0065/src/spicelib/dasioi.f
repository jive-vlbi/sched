C$Procedure      DASIOI ( DAS, Fortran I/O, integer )
 
      SUBROUTINE DASIOI ( ACTION, UNIT, RECNO, RECORD )
 
C$ Abstract
C
C     Perform Fortran reads and writes of integer records.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     FILES
C     UTILITY
C
C$ Declarations
 
      INTEGER               NWI
      PARAMETER           ( NWI = 256 )
 
      CHARACTER*(*)         ACTION
      INTEGER               UNIT
      INTEGER               RECNO
      INTEGER               RECORD ( NWI )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action to take (read or write).
C     UNIT       I   Fortran unit connected to DAS file.
C     RECNO      I   Number of record to read or write.
C     RECORD    I-O  DAS integer record.
C
C$ Detailed_Input
C
C     ACTION         is a character string specifying whether to read
C                    from or write to the specified DAS file.  Possible
C                    values are:
C
C                       'READ'
C                       'WRITE'
C
C                    Case and leading or trailing blanks are not
C                    significant.
C
C
C     UNIT           is the Fortran unit number connected to the DAS
C                    file that is to be read or written.  Given the
C                    handle of the DAS file, the unit number can be
C                    obtained using DASHLU.
C
C     RECNO          is the Fortran record number of the record to be
C                    read or written.
C
C     RECORD         is an integer array whose contents are to be
C                    written to record RECNO, if ACTION is WRITE.
C
C$ Detailed_Output
C
C     RECORD         is an integer array whose contents are to be
C                    set equal to those of record RECNO, if ACTION
C                    is READ.
C
C$ Parameters
C
C     NWI            is the number of elements in a DAS integer record.
C
C$ Exceptions
C
C     1)  If the value of ACTION is not recognized, the error
C         SPICE(UNRECOGNIZEDACTION) is signalled.
C
C     2)  If a Fortran read error occurs, the error
C         SPICE(DASFILEREADFAILED) is signalled.
C
C     3)  If a Fortran write error occurs, the error
C         SPICE(DASFILEWRITEFAILED) is signalled.
C
C$ Files
C
C     See the description of the argument UNIT in $Detailed_Input.
C
C$ Particulars
C
C     Normally, routines outside of SPICELIB will not need to call this
C     routine directly.  Writes to DAS files should be performed using
C     the DASADx and DASUDx routines; reads should be performed using
C     the DASRDx routines.
C
C     This routines centralizes I/O and the concommitant error handling
C     for DAS character records.
C
C     Although most DAS routines use file handles to indentify DAS
C     files, this routine uses Fortran logical units for this purpose.
C     Using unit numbers allows the DASIOx routines to be called from
C     any DAS routine, including entry points of DASFM.  (DASFM
C     contains as entry points the routines DASHLU and DASLUH, which
C     map between handles and unit numbers.)
C
C$ Examples
C
C     1)  Read and print to the screen integer records number 10
C         through 20 from the DAS file designated by HANDLE.
C
C            INTEGER               RECORD ( NWI )
C                           .
C                           .
C                           .
C
C            CALL DASHLU ( HANDLE, UNIT )
C            CALL DASHFN ( HANDLE, NAME )
C
C            DO I = 1, 20
C
C               CALL DASIOI ( 'READ', UNIT, 10, RECORD )
C
C               LABEL = 'Contents of the # record in DAS file #: '
C
C               CALL REPMOT ( LABEL,  '#',  I,  'L',   LABEL )
C               CALL REPMC  ( LABEL,  '#',      NAME,  LABEL )
C
C               WRITE (*,*) LABEL
C               WRITE (*,*) ' '
C               WRITE (*,*) RECORD
C
C            END DO
C
C
C
C     2)  Write the contents of the array RECORD to record number
C         10 in the DAS file designated by HANDLE.
C
C
C            INTEGER               RECORD ( NWI )
C
C                           .
C                           .
C                           .
C
C            CALL DASHLU (  HANDLE,  UNIT               )
C            CALL DASIOI ( 'WRITE',  UNIT,  10,  RECORD )
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
C
C-&
 
C$ Index_Entries
C
C     perform Fortran reads of integer records
C     perform Fortran writes of integer records
C     perform low-level I/O for DAS routines
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               IOSTAT
 
 
C
C     Use discovery check-in.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
 
      IF (   EQSTR ( ACTION, 'READ' )   ) THEN
C
C        We're supposed to read the file.
C
         READ (  UNIT    =    UNIT,
     .           REC     =    RECNO,
     .           IOSTAT  =    IOSTAT  )    RECORD
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL CHKIN  ( 'DASIOI'                                     )
            CALL SETMSG ( 'Could not read DAS integer record. '       //
     .                    'File = # Record number = #. IOSTAT = #.'    )
            CALL ERRFNM ( '#', UNIT                                    )
            CALL ERRINT ( '#', RECNO                                   )
            CALL ERRINT ( '#', IOSTAT                                  )
            CALL SIGERR ( 'SPICE(DASFILEREADFAILED)'                   )
            CALL CHKOUT ( 'DASIOI'                                     )
            RETURN
 
         END IF
 
 
      ELSE IF (  EQSTR ( ACTION, 'WRITE' )  ) THEN
 
C
C        We're supposed to write to the file.
C
         WRITE (  UNIT    =    UNIT,
     .            REC     =    RECNO,
     .            IOSTAT  =    IOSTAT  )    RECORD
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL CHKIN  ( 'DASIOI'                                     )
            CALL SETMSG ( 'Could not write DAS integer record. '      //
     .                    'File = # Record number = #. IOSTAT = #.'    )
            CALL ERRFNM ( '#', UNIT                                    )
            CALL ERRINT ( '#', RECNO                                   )
            CALL ERRINT ( '#', IOSTAT                                  )
            CALL SIGERR ( 'SPICE(DASFILEWRITEFAILED)'                  )
            CALL CHKOUT ( 'DASIOI'                                     )
            RETURN
 
         END IF
 
 
      ELSE
C
C        The requested action is a little too weird.
C
         CALL CHKIN  ( 'DASIOI'                                )
         CALL SETMSG ( 'Action was #; should be READ or WRITE' )
         CALL ERRCH  ( '#', ACTION                             )
         CALL SIGERR ( 'SPICE(UNRECOGNIZEDACTION)'             )
         CALL CHKOUT ( 'DASIOI'                                )
         RETURN
 
      END IF
 
 
      RETURN
      END
