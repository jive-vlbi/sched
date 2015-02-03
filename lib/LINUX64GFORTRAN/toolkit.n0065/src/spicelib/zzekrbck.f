C$Procedure      ZZEKRBCK ( EK, record backup <STUB> )
 
      SUBROUTINE ZZEKRBCK ( ACTION, HANDLE, SEGDSC, COLDSC, RECNO )
 
C$ Abstract
C
C     Back up a modified EK record belonging to a shadowed EK.
C     This is a stub routine.
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
C     PRIVATE
C     UTILITY
C
C$ Declarations
 
 
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekdatpag.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekrecptr.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
 
      CHARACTER*(*)         ACTION
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               RECNO
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ACTION     I   Action necessitating backup.
C     HANDLE     I   File handle.
C     SEGDSC     I   Segment descriptor.
C     COLDSC     I   Column descriptor.
C     RECPTR     I   Record number.
C
C$ Detailed_Input
C
C     ACTION         is a short string indicating the action taken
C                    that necessitated backing up a record from the
C                    specified EK file.  Values and meanings of
C                    ACTION are:
C
C                       'ADD'         The indicated record is being
C                                     added to the input EK.  No data
C                                     is backed up in this case, since
C                                     a rollback will remove the
C                                     indicated record from the input
C                                     EK file.
C
C                       'UPDATE'      The indicated record is being
C                                     updated.  When ACTION indicates a
C                                     record update, the argument COLDSC
C                                     (see below) indicates the column
C                                     that was affected.  If the
C                                     specified column entry has not yet
C                                     been backed up, it will be.
C                                     Otherwise, no action is taken.
C
C                       'DELETE'      The indicated record is being
C                                     deleted from the input EK.  If the
C                                     record has been added since the
C                                     last commit, no data from the
C                                     record is backed up, but a
C                                     placeholder record is created.
C                                     If the record to be deleted
C                                     existed at the time of the last
C                                     commit, the entire original
C                                     record is backed up.
C
C
C     HANDLE         is a file handle of an EK open for write access.
C
C     SEGDSC         is the descriptor of the segment from which to
C                    delete the specified column entry.
C
C     COLDSC         is the descriptor of the column from which to
C                    delete the specified column entry.  COLDSC is
C                    ignored unless ACTION is set to 'UPDATE'.
C
C     RECNO          is the number of the record containing the column
C                    entry to back up.
C
C$ Detailed_Output
C
C     None.  See the $Particulars section for a description of the
C     effect of this routine.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If HANDLE is invalid, the error will be diagnosed by routines
C         called by this routine.  The file will not be modified.
C
C     2)  If an I/O error occurs while reading or writing the indicated
C         file, the error will be diagnosed by routines called by this
C         routine.  The file may be corrupted.
C
C$ Files
C
C     See the EK Required Reading for a discussion of the EK file
C     format.
C
C$ Particulars
C
C     This routine operates by side effects:  it performs record backup
C     functions to support EK shadowing.  If the input file is not
C     shadowed, this routine returns without taking any action.
C
C     This routine uses a shadow EK file to store sufficient information
C     to restore the data in an EK file to its state at the time of
C     the last commit.  The segments in the backup file are in
C     one-to-one correspondence with the modified segments of loaded
C     EK files.  If the backup file doesn't contain a backup
C     segment corresponding to the specified segment in the input file,
C     a new backup segment is started.
C
C     Whenever this routine is called, it modifies the backup segment
C     as necessary to reflect changes made to the source segment.
C     The actions taken are as follows:
C
C        Updates
C        -------
C
C        The first time an OLD record is updated, a backup record is
C        created for that record.  The old value of the updated column
C        entry is saved in the backup record.  The status of the
C        source record becomes UPDATE.  The status of the backup
C        record is OLD.
C
C        Updates to unmodified entries in an UPDATEd record cause the
C        original values of those entries to be stored in the backup
C        record.  Unmodified entries are not backed up.  Once an entry
C        has been updated, further updates to that entry do not cause
C        any backup action to be taken.
C
C        Updates to NEW records do not result in any action.
C
C
C        Additions
C        ---------
C
C        When a new record is added to the source segment, an empty
C        record is appended to the backup segment.  The backup record
C        has status NEW and points back to the new source record.  Note
C        that this backward pointer is guaranteed to be valid only when
C        the source record occupies its current ordinal position in the
C        source segment.
C
C
C        Deletions
C        ---------
C
C        When any record is deleted from the source segment, a backup
C        record is appended to the backup segment.  The backup record
C        has a pointer to the corresponding source record.  The pointer
C        is the record number of the deleted record at the time of
C        deletion. If the deleted record had NEW status, the backup
C        record is empty and has DELNEW status.  If the deleted record
C        had OLD or UPDATE status, the backup record is a copy of the
C        original state of the deleted record and has DELOLD status.
C
C     When a rollback is performed, the set of backup records that
C     denote additions and deletions is processed in LIFO order.  Each
C     record with DELETE status is copied to the source segment and
C     inserted at the ordinal position indicated by its backward
C     pointer.  Records with NEW status signal that the corresponding
C     source records are to be deleted.  Backup records having OLD
C     status are ignored during this step.  The inversion of additions
C     and deletions performed on the source segment ensures that
C     the backup records' pointers to source records are valid at the
C     time they are referenced.  After all insertions and deletions
C     are processed, all records having UPDATE status in the source
C     segment are returned to their original status by copying values
C     from their backup records into the corresponding column entries.
C     Forward pointers in the source records are used to identify the
C     corresponding backup records.
C
C     The changes made by this routine to the target EK file become
C     permanent when the file is closed.  Failure to close the file
C     properly will leave it in an indeterminate state.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) This is a stub version of the routine.
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
C-    Beta Version 1.0.0, 25-OCT-1995 (NJB)
C
C-&
 
      CHARACTER*(1)         TMPCHR
      INTEGER               I
 
      TMPCHR = ACTION(1:1)
      I      = HANDLE
      I      = SEGDSC(1)
      I      = COLDSC(1)
      I      = RECNO
 
      RETURN
      END
