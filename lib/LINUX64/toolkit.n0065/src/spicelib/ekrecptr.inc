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
C
C     Include Section:  EK Record Pointer Parameters
C
C        ekrecptr.inc Version 2  18-JUL-1995 (NJB)
C
C
C     This file declares parameters used in EK record pointers.
C     Each segment references data in a given record via two levels
C     of indirection:  a record number points to a record pointer,
C     which is a structured array of metadata and data pointers.
C
C     Record pointers always occupy contiguous ranges of integer
C     addresses.
C
C     The parameter declarations in this file depend on the assumption
C     that integer pages contain 256 DAS integer words and that the
C     maximum number of columns in a segment is 100.  Record pointers
C     are stored in integer data pages, so they must fit within the
C     usable data area afforded by these pages.  The size of the usable
C     data area is given by the parameter IPSIZE which is declared in
C     ekdatpag.inc.  The assumed value of IPSIZE is 254.
C
C
C     The first element of each record pointer is a status indicator.
C     The meanings of status indicators depend on whether the parent EK
C     is shadowed or not.  For shadowed EKs, allowed status values and
C     their meanings are:
C
C        OLD       The record has not been modified since
C                  the EK containing the record was opened.
C
C        UPDATE    The record is an update of a previously existing
C                  record.  The original record is now on the
C                  modified record list.
C
C        NEW       The record has been added since the EK containing the
C                  record was opened.  The record is not an update
C                  of a previously existing record.
C
C        DELOLD    This status applies only to a backup record.
C                  DELOLD status indicates that the record corresponds
C                  to a deleted OLD record in the source segment.
C
C        DELNEW    This status applies only to a backup record.
C                  DELNEW status indicates that the record corresponds
C                  to a deleted NEW record in the source segment.
C
C        DELUPD    This status applies only to a backup record.
C                  DELUPD status indicates that the record corresponds
C                  to a deleted UPDATEd record in the source segment.
C
C     In EKs that are not shadowed, all records have status OLD.
C
C
      INTEGER               OLD
      PARAMETER           ( OLD    =          1 )
 
      INTEGER               UPDATE
      PARAMETER           ( UPDATE = OLD    + 1 )
 
      INTEGER               NEW
      PARAMETER           ( NEW    = UPDATE + 1 )
 
      INTEGER               DELOLD
      PARAMETER           ( DELOLD = NEW    + 1 )
 
      INTEGER               DELNEW
      PARAMETER           ( DELNEW = DELOLD + 1 )
 
      INTEGER               DELUPD
      PARAMETER           ( DELUPD = DELNEW + 1 )
 
C
C     The following parameters refer to indices within the record
C     pointer structure:
C
C     Index of status indicator:
C
      INTEGER               STAIDX
      PARAMETER           ( STAIDX = 1 )
 
C
C     Each record pointer contains a pointer to its companion:  for a
C     record belonging to a shadowed EK, this is the backup counterpart,
C     or if the parent EK is itself a backup EK, a pointer to the
C     record's source record.  The pointer is UNINIT (see below) if the
C     record is unmodified.
C
C     Record companion pointers contain record numbers, not record
C     base addresses.
C
C     Index of record's companion pointer:
C
      INTEGER               RCPIDX
      PARAMETER           ( RCPIDX = STAIDX + 1 )
 
C
C     Each data item is referenced by an integer.  The meaning of
C     this integer depends on the representation of data in the
C     column to which the data item belongs.  Actual lookup of a
C     data item must be done by subroutines appropriate to the class of
C     the column to which the item belongs.  Note that data items don't
C     necessarily occupy contiguous ranges of DAS addresses.
C
C     Base address of data pointers:
C
      INTEGER               DPTBAS
      PARAMETER           ( DPTBAS = 2 )
 
C
C     Maximum record pointer size:
C
      INTEGER               MXRPSZ
      PARAMETER           ( MXRPSZ = 254 )
 
C
C     Data pointers are given the value UNINIT to start with; this
C     indicates that the data item is uninitialized.  UNINIT is
C     distinct from the value NULL.  NOBACK indicates an uninitialized
C     backup column entry.
C
      INTEGER               UNINIT
      PARAMETER           ( UNINIT = -1 )
 
      INTEGER               NULL
      PARAMETER           ( NULL   = UNINIT - 1 )
 
      INTEGER               NOBACK
      PARAMETER           ( NOBACK = NULL   - 1 )
 
C
C     End Include Section:  EK Record Pointer Parameters
C
