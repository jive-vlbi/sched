C$Procedure      SPKOBJ ( SPK objects )
 
      SUBROUTINE SPKOBJ ( SPK, IDS )
 
C$ Abstract
C
C     Find the set of ID codes of all objects in a specified SPK file.
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
C     CELLS
C     DAF
C     SETS
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         SPK
      INTEGER               IDS ( LBCELL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     SPK        I   Name of SPK file.
C     IDS       I/O  Set of ID codes of objects in SPK file.
C
C$ Detailed_Input
C
C     SPK            is the name of an SPK file.
C     
C     IDS            is an initialized SPICELIB set data structure.
C                    IDS optionally may contain a set of ID codes on
C                    input; on output, the data already present in
C                    IDS will be combined with ID code set found for the
C                    file SPK.
C
C                    If IDS contains no data on input, its size and
C                    cardinality still must be initialized.
C
C$ Detailed_Output
C
C     IDS            is a SPICELIB set data structure which contains
C                    the union of its contents upon input with the set
C                    of ID codes of each object for which ephemeris
C                    data are present in the indicated SPK file. The
C                    elements of SPICELIB sets are unique; hence each
C                    ID code in IDS appears only once, even if the SPK
C                    file contains multiple segments for that ID code.
C
C                    See the Examples section below for a complete
C                    example program showing how to retrieve the ID
C                    codes from IDS.
C                                      
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input file has transfer format, the error 
C         SPICE(INVALIDFORMAT) is signaled.
C
C     2)  If the input file is not a transfer file but has architecture
C         other than DAF, the error SPICE(BADARCHTYPE) is signaled.
C
C     3)  If the input file is a binary DAF file of type other than
C         SPK, the error SPICE(BADFILETYPE) is signaled.
C
C     4)  If the SPK file cannot be opened or read, the error will
C         be diagnosed by routines called by this routine.
C
C     5)  If the size of the output set argument IDS is insufficient to
C         contain the actual number of ID codes of objects covered by
C         the indicated SPK file, the error will be diagnosed by
C         routines called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine provides an API via which applications can determine
C     the set of objects for which there are ephemeris data in a
C     specified SPK file.
C
C$ Examples
C
C     1)  Display the coverage for each object in a specified SPK file.
C         Find the set of objects in the file.  Loop over the contents
C         of the ID code set:  find the coverage for each item in the
C         set and display the coverage.
C
C              PROGRAM IDCOV
C              IMPLICIT NONE
C
C        C
C        C     SPICELIB functions
C        C
C              INTEGER               CARDI
C              INTEGER               WNCARD
C        C
C        C     Local parameters
C        C
C        C
C        C     Declare the coverage window.  Make enough room
C        C     for MAXIV intervals.
C        C
C              INTEGER               FILSIZ
C              PARAMETER           ( FILSIZ = 255 )
C
C              INTEGER               LBCELL
C              PARAMETER           ( LBCELL = -5 )
C
C              INTEGER               MAXIV
C              PARAMETER           ( MAXIV  = 1000 )
C
C              INTEGER               WINSIZ
C              PARAMETER           ( WINSIZ = 2 * MAXIV )
C
C              INTEGER               TIMLEN
C              PARAMETER           ( TIMLEN = 50 )
C
C              INTEGER               MAXOBJ
C              PARAMETER           ( MAXOBJ = 1000 )
C
C        C
C        C     Local variables
C        C
C              CHARACTER*(FILSIZ)    LSK
C              CHARACTER*(FILSIZ)    SPK
C              CHARACTER*(TIMLEN)    TIMSTR
C
C              DOUBLE PRECISION      B
C              DOUBLE PRECISION      COVER ( LBCELL : WINSIZ )
C              DOUBLE PRECISION      E
C
C              INTEGER               I
C              INTEGER               IDS   ( LBCELL : MAXOBJ )
C              INTEGER               J
C              INTEGER               NIV
C
C
C        C
C        C     Load a leapseconds kernel for output time conversion.
C        C     SPKCOV itself does not require a leapseconds kernel.
C        C
C              CALL PROMPT ( 'Name of leapseconds kernel > ', LSK )
C              CALL FURNSH ( LSK )
C
C        C
C        C     Get name of SPK file.
C        C
C              CALL PROMPT ( 'Name of SPK file           > ', SPK )
C
C        C
C        C     Initialize the set IDS.
C        C
C              CALL SSIZEI ( MAXOBJ, IDS )
C
C        C
C        C     Initialize the window COVER.
C        C
C              CALL SSIZED ( WINSIZ, COVER )
C
C        C
C        C     Find the set of objects in the SPK file.
C        C
C              CALL SPKOBJ ( SPK, IDS )
C
C        C
C        C     We want to display the coverage for each object.  Loop
C        C     over the contents of the ID code set, find the coverage
C        C     for each item in the set, and display the coverage.
C        C
C              DO I = 1, CARDI( IDS )
C        C
C        C        Find the coverage window for the current 
C        C        object. Empty the coverage window each time 
C        C        so we don't include data for the previous object.
C        C
C                 CALL SCARDD ( 0,   COVER )
C                 CALL SPKCOV ( SPK, IDS(I), COVER )
C
C        C
C        C        Get the number of intervals in the coverage
C        C        window.
C        C
C                 NIV = WNCARD ( COVER ) 
C
C        C
C        C        Display a simple banner.
C        C
C                 WRITE (*,*) '========================================'
C                 WRITE (*,*) 'Coverage for object ', IDS(I)
C
C        C
C        C        Convert the coverage interval start and stop
C        C        times to TDB calendar strings.
C        C
C                 DO J = 1, NIV
C        C
C        C           Get the endpoints of the Jth interval.
C        C
C                    CALL WNFETD ( COVER, J, B, E )
C        C
C        C           Convert the endpoints to TDB calendar
C        C           format time strings and display them.
C        C
C                    CALL TIMOUT ( B,
C             .                    'YYYY MON DD HR:MN:SC.### ' //
C             .                    '(TDB) ::TDB',
C             .                    TIMSTR                        )
C                    WRITE (*,*) ' '
C                    WRITE (*,*) 'Interval: ', J
C                    WRITE (*,*) 'Start:    ', TIMSTR
C
C                    CALL TIMOUT ( E,
C             .                    'YYYY MON DD HR:MN:SC.### ' //
C             .                    '(TDB) ::TDB',
C             .                    TIMSTR                        )
C                    WRITE (*,*) 'Stop:     ', TIMSTR
C                    WRITE (*,*) ' '
C
C                 END DO
C
C                 WRITE (*,*) '========================================'
C
C              END DO
C
C              END
C
C
C$ Restrictions
C
C     1) If an error occurs while this routine is updating the set
C        IDS, the set may be corrupted.
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
C-    SPICELIB Version 1.0.2, 01-JUL-2014 (NJB)
C
C        Updated index entries.
C
C-    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB)
C
C        Corrected bug in program in header Examples section:
C        program now empties the coverage window prior to collecting
C        data for the current object. Deleted declaration of unused
C        parameter NAMLEN in example program. Updated example to
C        use WNCARD rather than CARDD.
C
C-    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB)
C
C-&
 
C$ Index_Entries
C
C     find id codes of ephemeris objects in spk file
C     find id codes of bodies in spk file
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
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               ND
      PARAMETER           ( ND     = 2 )
 
      INTEGER               NI
      PARAMETER           ( NI     = 6 )
 
C
C     Local variables
C
      CHARACTER*(LNSIZE)    ARCH
      CHARACTER*(LNSIZE)    KERTYP

      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( ND + NI/2 )
 
      INTEGER               HANDLE
      INTEGER               IC     ( NI )

      LOGICAL               FOUND
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF 

      CALL CHKIN ( 'SPKOBJ' )

C
C     See whether GETFAT thinks we've got an SPK file.
C
      CALL GETFAT ( SPK, ARCH, KERTYP )

      IF ( ARCH .EQ. 'XFR' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file ' //
     .                 'must be a binary SPK file to be readable '  //
     .                 'by this routine.  If the input file is an ' //
     .                 'SPK file in transfer format, run TOBIN on ' //
     .                 'the file to convert it to binary format.'  )
         CALL ERRCH  ( '#',  SPK                                   )
         CALL ERRCH  ( '#',  ARCH                                  )
         CALL SIGERR ( 'SPICE(INVALIDFORMAT)'                      )
         CALL CHKOUT ( 'SPKOBJ'                                    )
         RETURN

      ELSE IF ( ARCH .NE. 'DAF' ) THEN

         CALL SETMSG ( 'Input file # has architecture #. The file ' //
     .                 'must be a binary SPK file to be readable '  //
     .                 'by this routine.  Binary SPK files have '   //
     .                 'DAF architecture.  If you expected the '    //
     .                 'file to be a binary SPK file, the problem ' //
     .                 'may be due to the file being an old '       //
     .                 'non-native file lacking binary file format '//
     .                 'information. It''s also possible the file ' //
     .                 'has been corrupted.'                       )
         CALL ERRCH  ( '#',  SPK                                   )
         CALL ERRCH  ( '#',  ARCH                                  )
         CALL SIGERR ( 'SPICE(INVALIDARCHTYPE)'                    )
         CALL CHKOUT ( 'SPKOBJ'                                    )
         RETURN

      ELSE IF ( KERTYP .NE. 'SPK' ) THEN

         CALL SETMSG ( 'Input file # has file type #. The file ' //
     .                 'must be a binary SPK file to be readable '  //
     .                 'by this routine. If you expected the '      //
     .                 'file to be a binary SPK file, the problem ' //
     .                 'may be due to the file being an old '       //
     .                 'non-native file lacking binary file format '//
     .                 'information. It''s also possible the file ' //
     .                 'has been corrupted.'                       )
         CALL ERRCH  ( '#',  SPK                                   )
         CALL ERRCH  ( '#',  KERTYP                                )
         CALL SIGERR ( 'SPICE(INVALIDFILETYPE)'                    )
         CALL CHKOUT ( 'SPKOBJ'                                    )
         RETURN

      END IF

C
C     Open the file for reading.
C
      CALL DAFOPR ( SPK, HANDLE )
 
      IF ( FAILED() ) THEN
         
         CALL CHKOUT ( 'SPKOBJ' )
         RETURN

      END IF

C
C     We will examine each segment descriptor in the file, and
C     we'll update our ID code set according to the data found
C     in these descriptors.
C
C     Start a forward search.
C
      CALL DAFBFS ( HANDLE )
 
C
C     Find the next DAF array.
C
      CALL DAFFNA ( FOUND )
 
      DO WHILE (  FOUND  .AND.  ( .NOT. FAILED() )  )
C
C        Fetch and unpack the segment descriptor.
C
         CALL DAFGS ( DESCR )
         CALL DAFUS ( DESCR, ND, NI, DC, IC )

C
C        Insert the current ID code into the output set.
C        The insertion algorithm will handle duplicates; no special
C        action is required here.
C
         CALL INSRTI ( IC(1), IDS )

         CALL DAFFNA ( FOUND )
 
      END DO
 
C
C     Release the file.
C
      CALL DAFCLS ( HANDLE )
 
      CALL CHKOUT ( 'SPKOBJ' )
      RETURN
      END
