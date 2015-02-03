C$Procedure      SEPOOL ( String from pool )
 
      SUBROUTINE SEPOOL ( ITEM, FIDX, CONTIN, STRING, SIZE, LIDX, FOUND)
 
C$ Abstract
C
C     Retrieve the string starting at the FIDX element of the kernel
C     pool variable, where the string may be continued across several
C     components of the kernel pool variable.
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
C     None.
C
C$ Keywords
C
C     POOL
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         ITEM
      INTEGER               FIDX
      CHARACTER*(*)         CONTIN
      CHARACTER*(*)         STRING
      INTEGER               SIZE
      INTEGER               LIDX
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ITEM       I   name of the kernel pool variable
C     FIDX       I   index of the first component of the string
C     CONTIN     I   character sequence used to indicate continuation
C     STRING     O   a full string concatenated across continuations
C     SIZE       O   the number of character in the full string value
C     LIDX       O   index of the last component of the string
C     FOUND      O   flag indicating success or failure of request
C
C$ Detailed_Input
C
C     ITEM       is the name of a kernel pool variable for which
C                the caller wants to retrieve a full (potentially
C                continued) string.
C
C     FIDX       is the index of the first component (the start) of 
C                the string in ITEM.
C
C     CONTIN     is a sequence of characters which (if they appear as
C                the last non-blank sequence of characters in a
C                component of a value of a kernel pool variable)
C                indicate that the string associated with the
C                component is continued into the next literal
C                component of the kernel pool variable.
C
C                If CONTIN is blank, all of the components of ITEM
C                will be retrieved as a single string.
C
C$ Detailed_Output
C
C     STRING     is the full string starting at the FIDX element of the
C                kernel pool variable specified by ITEM.
C
C                Note that if STRING is not sufficiently long to hold
C                the fully continued string, the value will be
C                truncated.  You can determine if STRING has been
C                truncated by examining the variable SIZE.
C
C     SIZE       is the index of last non-blank character of
C                continued string as it is represented in the
C                kernel pool. This is the actual number of characters
C                needed to hold the requested string.  If STRING
C                contains a truncated portion of the full string,
C                RTRIM(STRING) will be less than SIZE.
C
C                If the value of STRING should be a blank, then
C                SIZE will be set to 1.
C
C     LIDX       is the index of the last component (the end) of 
C                the retrieved string in ITEM.
C
C     FOUND      is a logical variable indicating success of the
C                request to retrieve the string.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the variable specified by ITEM is not present in the
C        kernel pool or is present but is not character valued, STRING
C        will be returned as a blank, SIZE will be returned with the
C        value 0 and FOUND will be set to .FALSE. In particular if NTH
C        is less than 1, STRING will be returned as a blank, SIZE will
C        be zero and FOUND will be FALSE.
C
C     2) If the variable specified has a blank string associated
C        with its full string starting at FIDX, STRING will be blank,
C        SIZE will be 1 and FOUND will be set to .TRUE.
C
C     3) If STRING is not long enough to hold all of the characters
C        associated with the NTH string, it will be truncated on the
C        right.
C
C     4) If the continuation character is a blank, every component
C        of the variable specified by ITEM will be inserted into
C        the output string.
C
C     5) If the continuation character is blank, then a blank component
C        of a variable is treated as a component with no letters.
C        For example:
C
C           STRINGS = ( 'This is a variable'
C                       'with a blank'
C                       ' '
C                       'component.' )
C
C        Is equivalent to
C
C
C           STRINGS = ( 'This is a variable'
C                       'with a blank'
C                       'component.' )
C
C        from the point of view of SEPOOL if CONTIN is set to the
C        blank character.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The SPICE Kernel Pool provides a very convenient interface
C     for supplying both numeric and textual data to user application
C     programs.  However, any particular component of a character
C     valued component of a kernel pool variable is limited to 80
C     or fewer characters in length.
C
C     This routine allows you to overcome this limitation by
C     "continuing" a character component of a kernel pool variable.
C     To do this you need to select a continuation sequence
C     of characters and then insert this sequence as the last non-blank
C     set of characters that make up the portion of the component
C     that should be continued.
C
C     For example, you may decide to use the sequence '//' to indicate
C     that a string should be continued to the next component of
C     a kernel pool variable.   Then set up the
C     kernel pool variable as shown below
C
C     LONG_STRINGS = ( 'This is part of the first component //'
C                      'that needs more than one line when //'
C                      'inserting it into the kernel pool.'
C                      'This is the second string that is split //'
C                      'up as several components of a kernel pool //'
C                      'variable.' )
C
C     When loaded into the kernel pool, the variable LONG_STRINGS
C     will have six literal components:
C
C        COMPONENT (1) = 'This is part of the first component //'
C        COMPONENT (2) = 'that needs more than one line when //'
C        COMPONENT (3) = 'inserting it into the kernel pool.'
C        COMPONENT (4) = 'This is the second string that is split //'
C        COMPONENT (5) = 'up as several components of a kernel pool //'
C        COMPONENT (6) = 'variable.'
C
C     These are the components that would be retrieved by the call
C
C        CALL GCPOOL ( 'LONG_STRINGS', 1, 6, N, COMPONENT, FOUND )
C
C     However, using the routine SEPOOL you can view the variable
C     LONG_STRINGS as having two long components.
C
C        STRING (1) = 'This is part of the first component that '
C    .   //           'needs more than one line when inserting '
C    .   //           'it into the kernel pool. '
C
C        STRING (2) = 'This is the second string that is split '
C    .   //           'up as several components of a kernel pool '
C    .   //           'variable. '
C
C
C     These string components would be retrieved by the following two
C     calls.
C
C        FIDX = 1
C        CALL SEPOOL ( 'LONG_STRINGS', FIDX, '//', 
C       .                              STRING(1), SIZE, LIDX, FOUND )
C        FIDX = LIDX+1
C        CALL SEPOOL ( 'LONG_STRINGS', FIDX, '//', 
C       .                              STRING(2), SIZE, LIDX, FOUND )
C
C$ Examples
C
C     Example 1.  Retrieving file names.
C
C     Suppose a you have used the kernel pool as a mechanism for
C     specifying SPK files to load at startup but that the full
C     names of the files are too long to be contained in a single
C     text line of a kernel pool assignment.
C
C     By selecting an appropriate continuation character ('*' for
C     example)  you can insert the full names of the SPK files
C     into the kernel pool and then retrieve them using this
C     routine.
C
C     First set up the kernel pool specification of the strings
C     as shown here:
C
C           SPK_FILES = ( 'this_is_the_full_path_specification_*'
C                         'of_a_file_with_a_long_name'
C                         'this_is_the_full_path_specification_*'
C                         'of_a_second_file_with_a_very_long_*'
C                         'name' )
C
C     Now to retrieve and load the SPK_FILES one at a time,
C     exercise the following loop.
C
C     INTEGER               FILSIZ
C     PARAMETER           ( FILSIZ = 255 )
C
C     CHARACTER*(FILSIZ)    FILE
C     INTEGER               I
C     INTEGER               LIDX
C
C     I = 1
C
C     CALL SEPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, LIDX, FOUND )
C
C     DO WHILE ( FOUND .AND. RTRIM(FILE) .EQ. SIZE )
C
C        CALL SPKLEF ( FILE, HANDLE )
C        I = LIDX + 1 
C        CALL SEPOOL ( 'SPK_FILES', I, '*', FILE, SIZE, LIDX, FOUND )
C     END DO
C
C     IF ( FOUND .AND. RTRIM(FILE) .NE. SIZE ) THEN
C        WRITE (*,*) 'The ', I, '''th file name was too long.'
C     END IF
C
C
C     Example 2. Retrieving all components as a string.
C
C
C     Occasionally, it may be useful to retrieve the entire
C     contents of a kernel pool variable as a single string.  To
C     do this you can use the blank character as the
C     continuation character.  For example if you place the
C     following assignment in a text kernel
C
C         COMMENT = (  'This is a long note '
C                      ' about the intended '
C                      ' use of this text kernel that '
C                      ' can be retrieved at run time.' )
C
C     you can retrieve COMMENT as single string via the call below.
C
C        CALL SEPOOL ( 'COMMENT', 1, ' ', COMMNT, SIZE, LIDX, FOUND )
C
C     The result will be that COMMNT will have the following value.
C
C        COMMNT = 'This is a long note about the intended use of '
C    .   //       'this text kernel that can be retrieved at run '
C    .   //       'time. '
C
C     Note that the leading blanks of each component of COMMENT are
C     significant, trailing blanks are not significant.
C
C     If COMMENT had been set as
C
C         COMMENT = (  'This is a long note '
C                      'about the intended '
C                      'use of this text kernel that '
C                      'can be retrieved at run time.' )
C
C     Then the call to SEPOOL above would have resulted in several
C     words being run together as shown below.
C
C
C        COMMNT = 'This is a long noteabout the intendeduse of '
C    .   //       'this text kernel thatcan be retrieved at run '
C    .   //       'time. '
C
C
C     resulted in several words being run together as shown below.
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-APR-2012 (WLT)(BVS)
C
C
C-&
 
C$ Index_Entries
C
C     Retrieve a continued string value from the kernel pool
C
C-&
C     SPICELIB Variables
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    PART
 
 
      INTEGER               CFIRST
      INTEGER               CLAST
      INTEGER               COMP
      INTEGER               CSIZE
      INTEGER               N
      INTEGER               PUTAT
      INTEGER               ROOM
 
      LOGICAL               GOTIT
      LOGICAL               MORE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     Return empty output if the input index is bad. 
C 
      IF ( FIDX .LT. 1 ) THEN
         FOUND  = .FALSE.
         STRING = ' '
         SIZE   = 0
         LIDX   = 0
         RETURN
      END IF

C
C     Check in.
C
      CALL CHKIN ( 'SEPOOL')

C
C     Check if the first component exists. Return empty output if not. 
C
      CALL GCPOOL ( ITEM, FIDX, 1, N, PART, GOTIT )
 
      GOTIT = GOTIT .AND. N .GT. 0
 
      IF ( .NOT. GOTIT ) THEN
         FOUND  = .FALSE.
         STRING = ' '
         SIZE   = 0
         LIDX   = 0
         CALL CHKOUT ( 'SEPOOL' )
         RETURN
      END IF

C
C     Fetch the string using Bill's algorithm from STPOOL 'as is'.
C
      ROOM   = LEN(STRING)
      CSIZE  = RTRIM(CONTIN)
      PUTAT  = 1
 
      COMP   = FIDX 
      MORE   = .TRUE.
      STRING = ' '
      N      =  0
 
      DO WHILE ( MORE )
 
         CALL GCPOOL ( ITEM, COMP, 1, N, PART, MORE )
                  
         MORE = MORE .AND. N .GT. 0
 
         IF ( MORE ) THEN
 
            FOUND  = .TRUE.
 
            CLAST  = RTRIM(PART)
            CFIRST = CLAST - CSIZE + 1
 
            IF ( CFIRST .LT. 0 ) THEN
 
               IF ( PUTAT .LE. ROOM ) THEN
                  STRING(PUTAT:) = PART(1:CLAST)
               END IF
 
               PUTAT          = PUTAT + CLAST
               MORE           = .FALSE.
 
            ELSE IF ( PART(CFIRST:CLAST) .NE. CONTIN ) THEN
 
               IF ( PUTAT .LE. ROOM ) THEN
                  STRING(PUTAT:) = PART(1:CLAST)
               END IF
               PUTAT          = PUTAT + CLAST
               MORE           = .FALSE.
 
            ELSE IF ( CFIRST .GT. 1 ) THEN
 
               IF ( PUTAT .LE. ROOM ) THEN
                  STRING(PUTAT:) = PART(1:CFIRST-1)
               END IF
               PUTAT          = PUTAT + CFIRST - 1
 
            END IF
 
         END IF
 
         COMP = COMP + 1
 
      END DO
 
C
C     We are done. Get the size of the full string and the index of its
C     last component and checkout.
C
      SIZE = PUTAT - 1
      LIDX = COMP  - 1
 
      CALL CHKOUT ( 'SEPOOL' )
 
      RETURN
      END
