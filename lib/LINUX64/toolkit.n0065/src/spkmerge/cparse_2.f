C$Procedure      CPARSE (Command Parse)

      SUBROUTINE CPARSE (KWDSYM,
     .                   KWDPTR,
     .                   KWDVAL,
     .                   HEAD,
     .                   LINE,
     .                   EOC,
     .                   CMDSYM,
     .                   CMDPTR,
     .                   CMDVAL,
     .                   ERR,
     .                   REASON,
     .                   LINNUM)

C$ Abstract
C
C     Command parser.
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
C     PARSING
C     STRING
C
C$ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
      
      CHARACTER*(*)         KWDSYM (LBCELL: *)
      INTEGER               KWDPTR (LBCELL: *)
      CHARACTER*(*)         KWDVAL (LBCELL: *)
      CHARACTER*(*)         HEAD
      CHARACTER*(*)         LINE
      LOGICAL               EOC
      CHARACTER*(*)         CMDSYM (LBCELL: *)
      INTEGER               CMDPTR (LBCELL: *)
      CHARACTER*(*)         CMDVAL (LBCELL: *)
      LOGICAL               ERR
      CHARACTER*(*)         REASON
      INTEGER               LINNUM

C$ Brief_I/O
C
C     Variable  I/O  Entry
C     --------  ---  --------------------------------------------------
C     KWDSYM,
C     KWDPTR,
C     KWDVAL     I   INITCP
C     HEAD       I   INITCP
C     LINE       I   EVALCP
C     EOC        I   EVALCP
C     CMDSYM,
C     CMDPTR,
C     CMDVAL     O   EVALCP
C     REASON     O   CPERR
C     LINNUM     O   CPERR
C      
C$ Detailed_Input
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Detailed_Output
C
C     See the ENTRY points for a discussion of their arguments.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If CPARSE is called directly, the error SPICE(BOGUSENTRY) is
C        signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     CPARSE was built to parse NIOSPK command files. However, it
C     can be used to parse similar types of files. Below is an example
C     NIOSPK command file. The indentation is not necessary.
C     
C        leapseconds_file   = leapseconds.ker
C        
C        spk_file           = gll.bsp
C           note            = this is an example
C           source_nio_file = gll.nio
C              bodies       = -77
C           source_nio_file = de200.nio
C              bodies       = 10 399 3
C     
C     Values may span any number of lines; no explicit continuation
C     characters are necessary. If a value does span multiple lines,
C     it must be broken at word boundaries. For example, the
C     following two commands are equivalent:
C     
C        note = this is my note
C        
C        note = this
C                    is
C                       my 
C                          note
C        
C     CPARSE must be told the relationships between keywords; this
C     is done in the initialization procedure, INITCP; it must be 
C     told which keywords are parent keywords, and which keywords
C     are child keywords. These two terms are defined below.
C     
C        parent keyword    is a keyword that has one or more
C                          child keywords. In the example above, 
C                          spk_file and source_nio_file are parent
C                          keywords. The child keywords of spk_file
C                          are source_nio_file and note; the child
C                          keyword of source_nio_file is bodies. 
C        
C        child keyword     is a keyword that follows a parent keyword.
C                          In the example above, source_nio_file is a
C                          child keyword of spk_file. Because 
C                          source_nio_file is a child keyword of 
C                          spk_file, it must follow spk_file, as is 
C                          shown. A child keyword may also be a 
C                          parent keyword. A keyword may be 
C                          a child keyword of two different parent
C                          keywords. For example, NIOSPK defines the
C                          bodies keyword to be a child keyword of 
C                          spk_file as well as source_nio_file. If a 
C                          keyword is a child keyword of multiple 
C                          parent keywords, CPARSE assumes the 
C                          closest parent keyword above it is its 
C                          parent keyword (that's why source_nio_file,
C                          not spk_file, is the parent keyword of bodies
C                          in example).
C                          
C     In addition to telling INITCP which keywords are parent keywords
C     and which keywords are child keywords, it must be told how many
C     times the child keywords may appear under each parent 
C     keyword---a minimum and a maximum number of times. For NIOSPK,
C     the source_nio_file keyword must appear at least once, and may 
C     appear up to 100 (or so) number of times. The bodies keyword may
C     appear 0, or up to a 100 (or so) number of times. source_nio_file,
C     then, is a required keyword, while bodies is not.
C     
C     The associations between the parent and child keywords and the 
C     number of times the child keywords may appear per parent keyword
C     are passed to INITCP in the form of a symbol table. Each symbol
C     in the symbol table is a parent keyword; the values associated 
C     with a symbol are the child keywords of the symbol, or parent
C     keyword. Two numbers must follow each child keyword; these numbers
C     are the minimum and maximum number of times the associated keyword
C     may appear under its parent keyword. For example, the bodies 
C     keyword with the parent keyword source_nio_file would be given
C     as:
C     
C        source_nio_file   -->   bodies 0 100
C     
C     and the source_nio_file keyword with the parent keyword spk_file
C     would be given as
C     
C        spk_file          -->   source_nio_file 1 100
C        
C     bodies is also a child keyword of spk_file, so it can be given
C     in the same association:
C     
C        spk_file          -->   source_nio_file 1 100
C                                bodies          0 100
C                          
C     
C     In order to specify the minimum and maximum occurrence of keywords
C     that don't have a parent keyword, such as spk_file and 
C     leapseconds_file in the example, a fictional keyword is used to
C     act as the parent keyword of those keywords. This keyword may 
C     be named anything (the name is passed to INITCP); here, it is 
C     named head. So, if spk_file and leapseconds_file were the only 
C     two keywords without a parent, NIOSPK would create the following 
C     association: 
C     
C        head              -->   spk_file         1 100
C                                leapseconds_file 1 1
C                                
C     Here, spk_file must appear at least once and no more than
C     100 times, and leapseconds_file must appear exactly
C     once. 
C     
C     Once CPARSE is initialized, it can process lines from a 
C     command file, or from some other device, with the procedure 
C     EVALCP. EVALCP does not read from a unit; rather, it is passed 
C     a line at a time. EVALCP stores the data it processes in a 
C     symbol table called the command symbol table; this data 
C     structure is passed to EVALCP. When the last line is passed to 
C     EVALCP, it must be called one last time with one of its arguments 
C     (EOC--end of commands) set to true. 
C     
C     The calling procedure may access the data stored in the command
C     symbol table by first forming the appropriate symbol, then calling
C     SYGETC. In most cases, the procedure CRTPTR (create pointer) must 
C     be called to form the appropriate symbol. The calling sequence 
C     of CRTPTR is
C        
C        CRTPTR (PARENT, INDEX, CHILD)
C        
C     For example, if NIOSPK wanted the nio files associated with
C     the first spk file, it would need to perform the following:
C     
C        SYMBOL = CRTPTR ('SPK_FILE', 1, 'SOURCE_NIO_FILE')
C        CALL SYGETC (SYMBOL, CMDSYM, CMDPTR, CMDVAL, N, VALUES, FND)
C        
C     Where CMDxxx is the command symbol table. Or, if NIOSPK wanted
C     the bodies associated with the second source nio file in the first
C     spk file, it would need to call CRTPTR twice:
C     
C        SYMBOL = CRTPTR ('SPK_FILE', 1, 'SOURCE_NIO_FILE')
C        SYMBOL = CRTPTR (SYMBOL,     2, 'BODIES'         )
C        CALL SYGETC (SYMBOL, CMDSYM, CMDPTR, CMDVAL, N, VALUES, FND)
C        
C     If FND is returned as false, then no bodies were given for the
C     source nio file. To retrieve values associated with keywords that
C     do not have parent keywords (if you ignore head), no calls to
C     CRTPTR are necessary. For example, if NIOSPK wished to get all the
C     spk files, it would just call SYGETC:
C     
C        SYMBOL = 'SPK_FILE'
C        CALL SYGETC (SYMBOL, CMDSYM, CMDPTR, CMDVAL, N, VALUES, FND)
C     
C$ Examples
C
C     None.
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    Beta Version 2.2.0, 17-JAN-2014 (BVS)
C
C       Increased LINLEN from 127 to 350.
C
C-    Beta Version 2.1.0, 14-JAN-1994 (MJS)
C
C       Removed VALSTK, as it no longer used. Increased VALLEN from
C       255 to 300.
C
C-    Beta Version 2.0.0, 12-DEC-1993 (MJS)
C
C       Parse errors no longer signal SPICE errors. Rather, applications
C       get error messages via the new entry point CPERR --- EVALCP
C       has a new argument, ERR, which indicates when an error has
C       occurred.
C
C       Modified internal data structure. This version uses one less
C       array (NUMCHL removed). CHLBEG changed to CHLPTR. CHLPTR
C       contains pointers to entries in CHLNUM, NUMMIN, and NUMMAX.
C       CHLPTR(I)-CHLPTR(I-1) is the number of child keywords of
C       parent keyword I. Child keywords of parent keyword I start at
C       CHLPTR(I-1)+1 (CHLPTR starts at 0).
C     
C-    Beta Version 1.0.0, 8-OCT-1992 (MJS) 
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               ISRCHC
      INTEGER               ISRCHI
      LOGICAL               FAILED
      INTEGER               FRSTNB
      INTEGER               LASTNB
      INTEGER               CARDC

C     
C     Statement functions
C     
      INTEGER               NUMCHL
      INTEGER               KWLOOK
      INTEGER               CHLOOK
      
C     
C     Other functions
C     
      INTEGER               SYMLEN
      PARAMETER           ( SYMLEN = 100 )
      
      CHARACTER*(SYMLEN)    CRTPTR

C
C     Local parameters
C
C
C     MAXKWD    is the maximum number of keywords
C     
C     MAXSTK    is the maximum number of keywords one must follow to
C               find any keyword.
C               
C     CHLSIZ    is the maximum number of child keywords. If a keyword
C               is a child of more than one parent keyword, it must
C               be counted as many times.
C               
C     KWDLEN    is the maximum length of a keyword.  
C
C     LINLEN    is the maximum length of a line.
C
C     VALLEN    is the maximum length of a value. A value may span
C               over multiple lines, but leading and trailing spaces
C               are ignored (except for one).
C        
C     MAXCHL    is the maximum number of child keywords a parent keyword
C               may have.
C   
C     TAB       is the ASCII value for the tab character.
C
C     ERRLEN    is the maximum length of an error message.
C
      INTEGER               MAXKWD
      PARAMETER           ( MAXKWD = 100 )
      
      INTEGER               MAXSTK
      PARAMETER           ( MAXSTK = 5 )
      
      INTEGER               CHLSIZ
      PARAMETER           ( CHLSIZ = 100 )
      
      INTEGER               KWDLEN
      PARAMETER           ( KWDLEN = 32 )
      
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 350 )
      
      INTEGER               VALLEN
      PARAMETER           ( VALLEN = 300 )
      
      INTEGER               MAXCHL
      PARAMETER           ( MAXCHL = 40 )
      
      INTEGER               TAB
      PARAMETER           ( TAB = 9 )

      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 320 )

C
C     Local variables
C
      CHARACTER*(10)        ACTION
      CHARACTER*(KWDLEN)    KWNAM  (MAXKWD)
      CHARACTER*(LINLEN)    VALUES (MAXCHL)
      CHARACTER*(LINLEN)    LN
      CHARACTER*(VALLEN)    VALUE
      CHARACTER*(ERRLEN)    MSG
      CHARACTER*(SYMLEN)    SYMBOL
      CHARACTER*(KWDLEN)    HD
      CHARACTER*(KWDLEN)    KWD
      CHARACTER*(KWDLEN)    KW
      CHARACTER*(20)        MINCH
      CHARACTER*(20)        MAXCH
      
      INTEGER               KWSTK  (0:MAXSTK)
      INTEGER               KWCNTR (MAXCHL, 0:MAXSTK)
      INTEGER               NKWD
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               P
      INTEGER               CP
      INTEGER               EP
      INTEGER               STKLEV
      INTEGER               NEWLEV
      INTEGER               NCHL
      INTEGER               KWDMIN
      INTEGER               KWDMAX
      INTEGER               KWDNUM
      INTEGER               CHLNUM (CHLSIZ)
      INTEGER               NUMMIN (CHLSIZ)
      INTEGER               NUMMAX (CHLSIZ)
      INTEGER               CHLPTR (0:MAXKWD)
      INTEGER               PNTER
      INTEGER               KWN
      INTEGER               CHN
      INTEGER               HDNUM
      INTEGER               VLEN
      INTEGER               LINES
      INTEGER               LSTCMD
      INTEGER               LINSTK (1:MAXSTK)
         
      LOGICAL               FND

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA NKWD  / 0 /

C     
C     Statement function definitions.
C     
C        NUMCHL (KWN)       returns the number of child keywords of 
C                           parent keyword index KWN in KWNAM.
C        KWLOOK (KW)        returns the index of KW in KWNAM.
C        CHLOOK (KWN,CHN)   returns the offset position of CHN in 
C                           CHLNUM using the parent index KWN.
C     
C     Where, 
C                        
C        KW   is the name of a keyword.
C        KWN  is the index of a keyword in KWNAM.
C        CHN  is the index of a keyword in KWNAM.
C     
      NUMCHL (KWN)      = CHLPTR(KWN) - CHLPTR(KWN-1)
      KWLOOK (KW)       = ISRCHC (KW, NKWD, KWNAM)
      CHLOOK (KWN, CHN) = ISRCHI (CHN, NUMCHL(KWN), 
     .                                          CHLNUM(CHLPTR(KWN-1)+1))

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ('CPARSE')
      END IF

C
C     This routine should never be called. If this routine is called,
C     an error is signaled.
C
      CALL SETMSG ('CPARSE: You have called an entry which '   //
     .             'performs no run-time function. This may '  //
     .             'indicate a bug.')
 
      CALL SIGERR ('SPICE(BOGUSENTRY)')
 
      CALL CHKOUT ('CPARSE')
      RETURN


 
C$Procedure INITCP (Initialize, command parser)
 
      ENTRY INITCP (KWDSYM, 
     .              KWDPTR, 
     .              KWDVAL,
     .              HEAD)

C$ Abstract
C
C     Initialize command parser.
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
C     PARSING
C     STRING
C
C$ Declarations
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C     
C     CHARACTER*(*)         KWDSYM (LBCELL: *)
C     INTEGER               KWDPTR (LBCELL: *)
C     CHARACTER*(*)         KWDVAL (LBCELL: *)
C     CHARACTER*(*)         HEAD
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      KWDSYM,
C      KWDPTR,
C      KWDVAL    I    Symbol table containing the parent-child 
C                     associations.
C                     
C      HEAD      I    Head symbol.
C      
C$ Detailed_Input
C
C      KWDSYM,
C      KWDPTR,
C      KWDVAL    is a symbol table containing parent-child keyword
C                associations, including the number of times the 
C                child keyword map appear under its parent. ALL 
C                associations in this symbol table are assumed to 
C                be parent-keyword associations.
C                
C                See the CPARSE particulars section for more details.
C      
C      HEAD      is the name of the HEAD node.
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
C     1) If HEAD is not a symbol in the symbol table, the error 
C        SPICE(NOHEADNODE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the CPARSE particulars section.
C     
C     ALL the symbols in CMDSYM are treated as parent keywords (that is,
C     make sure your symbol table has only parent-child associations).
C     
C     INITCP translates the parent-child associations in the symbol 
C     table to a data structure that will make parsing the commands
C     easier. The data structure is made up of five, one dimensional
C     arrays. The arrays are named KWNAM, CHLPTR, CHLNUM,
C     NUMMIN, and NUMMAX. These arrays are shown graphically below.
C        
C         KWNAM   CHLPTR      CHLNUM  NUMMIN  NUMMAX
C          +---+  +---+       +---+   +---+   +---+
C        1 |   |  |   |-------|   |   |   |   |   | 1
C          +---+  +---+       +---+   +---+   +---+
C          | . |  | . |       |   |   |   |   |   |
C            .      .         +---+   +---+   +---+ 
C        i | . |  | . |       | . |   | . |   | . |
C          +---+  +---+         .       .       . 
C          |   |  |   |\      | . |   | . |   | . |
C          +---+  +---+ \     +---+   +---+   +---+ 
C   MAXKWD |   |  |   |  \    |   |   |   |   |   |
C          +---+  +---+   \   +---+   +---+   +---+ 
C                          \  |   |   |   |   |   |
C                           \ +---+   +---+   +---+ 
C                            \|   |   |   |   |   | 
C                             +---+   +---+   +---+ 
C                             |   |   |   |   |   | MAXCHL
C                             +---+   +---+   +---+ 
C                                      
C     The first three arrays are of dimension MAXKWD, and the last
C     three are of dimension MAXCHL. These arrays are discussed 
C     separately below.
C     
C        Name    Type   Contents  
C           
C        CHLNAM  CHAR   The names of all the keywords in no particular
C                       order. 
C        
C        CHLPTR  INT    Pointer to CHLNUM, NUMMIN, and NUMMAX. 
C                       
C        CHLNUM  INT    Child keywords associated with the keywords in
C                       in KWNAM. Keywords are referenced by their
C                       position in KWNAM. 
C                       
C        NUMMIN  INT   Minimum occurrence of child keyword in CHLNUM.
C        
C        NUMMAX  INT   Maximum occurrence of child keyword in CHLNUM.
C      
C$ Examples
C
C     See the NIOSPK main module.
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 8-OCT-1992 (MJS) 
C
C-&

C     
C     Standard SPICE error handling.
C     
      IF (RETURN()) THEN
         RETURN
      ELSE
         CALL CHKIN ('INITCP')
      END IF

C     
C     We want all the keywords in KWNAM and ordered.
C     
      NKWD = 0

      DO I = 1, CARDC(KWDSYM)
         
         KWD = KWDSYM(I)
      
         CALL LJUST  (KWD, KWD)
         CALL UCASE  (KWD, KWD)
         
C     
C        Have we already come across this keyword?
C        
         P = ISRCHC (KWD, NKWD, KWNAM)
         
         IF (P .EQ. 0) THEN
            NKWD        = NKWD + 1
            KWNAM(NKWD) = KWD
         END IF

C
C        Check its children.
C
         CALL SYGETC (KWD, KWDSYM, KWDPTR, KWDVAL, NCHL, VALUES, FND)
         
         DO J = 1, NCHL

            CALL NEXTWD (VALUES(J), KWD, VALUES(J))

            CALL UCASE (KWD, KWD)

C     
C           If this is a new keyword, add it to our array.
C     
            P = ISRCHC (KWD, NKWD, KWNAM)

            IF (P .EQ. 0) THEN
               NKWD         = NKWD + 1
               KWNAM (NKWD) = KWD
            END IF
            
         END DO
         
      END DO

C
C     Here we could sort KWNAM and use a binary search, for maybe
C     a bit better performance.
C

C     
C     Now, for each keyword in KWNAM find the child keywords, if any.
C     
      CHLPTR(0) = 0

      DO I = 1, NKWD
         
         KWD = KWNAM(I)
         
         CALL SYGETC (KWD, KWDSYM, KWDPTR, KWDVAL, NCHL, VALUES, FND)
         
         P = CHLPTR(I-1)
         
         IF (FND) THEN
            DO J = 1, NCHL

               CALL NEXTWD (VALUES(J), KWD,   VALUES(J))
               CALL NEXTWD (VALUES(J), MINCH, VALUES(J))
               CALL NEXTWD (VALUES(J), MAXCH, VALUES(J))

               CALL UCASE  (KWD, KWD)
              
               CALL NPARSI (MINCH, KWDMIN, MSG, PNTER)
               CALL NPARSI (MAXCH, KWDMAX, MSG, PNTER)
            
               CHLNUM(P+J) = KWLOOK(KWD)
               NUMMIN(P+J) = KWDMIN
               NUMMAX(P+J) = KWDMAX
            
            END DO
         ELSE
            NCHL = 0
         END IF

         CHLPTR(I) = P + NCHL
         
      END DO

C     
C     HEAD must be in the data base. Its index is stored in HDNUM.
C     
      HD = HEAD
      
      CALL LJUST (HD, HD)
      CALL UCASE (HD, HD)
      
      HDNUM = KWLOOK(HD) 
      
      IF (HDNUM .EQ. 0) THEN
         CALL SIGERR ('SPICE(NOHEADNODE)')
         CALL CHKOUT ('INITCP'           )
         RETURN
      END IF
      
C     
C     Zero out the number of child nodes under HEAD. This is stack 
C     level zero. HDNUM is the first element in KWSTK. No keywords 
C     have been read, so KWD is set to ' '.
C     
      STKLEV        = 0
      KWSTK(STKLEV) = HDNUM
      KWD           = ' '
      
      CALL CLEARI (NUMCHL(HDNUM), KWCNTR(1,STKLEV))

C
C     The first line we'll read is 1.
C
      LINES = 0

C     
C     That's it. We're set up to process commands.
C     
      CALL CHKOUT ('INITCP')
      RETURN




 
C$Procedure EVALCP (Evaluate, command parser)
 
      ENTRY EVALCP (LINE, 
     .              EOC,
     .              CMDSYM,
     .              CMDPTR,
     .              CMDVAL,
     .              ERR)

C$ Abstract
C
C     Evaluate commands.
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
C     PARSING
C     STRING
C
C$ Declarations
C
C     INTEGER               LBCELL
C     PARAMETER           ( LBCELL = -5 )
C     
C     CHARACTER*(*)         LINE
C     LOGICAL               EOC
C     CHARACTER*(*)         CMDSYM (LBCELL: *)
C     INTEGER               CMDPTR (LBCELL: *)
C     CHARACTER*(*)         CMDVAL (LBCELL: *)
C     LOGICAL               ERR
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      LINE      I    Command.
C      EOC       I    End of commands. Must be set to true after last 
C                     line has been processed.
C      CMDSYM,
C      CMDPTR,
C      CMDVAL    O    Command symbol table.
C      ERR       O    True if a parse error occurs. If true, application
C                     should call CPERR for reason.
C      
C$ Detailed_Input
C
C     See the CPARSE particulars section.
C
C$ Detailed_Output
C   
C     See the CPARSE particulars section.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If the parser is not initialized an error will be signaled. Other
C     may be obtained via the entry point CPERR.
C        
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the CPARSE particulars section.
C
C$ Examples
C
C     Suppose you have a command file named INPUT.CMD, and you wish 
C     to process commands in this file after the delimiter 
C     '\begin_commands' through the end of the file. The following 
C     code fragment illustrates how one perform this task.
C     
C        CALL RDTEXT ('INPUT.CMD', LINE, EOF)
C        
C        DO WHILE (LINE .NE. '\begin_commands')
C        
C           CALL RDTEXT ('INPUT.CMD', LINE, EOF)
C           
C        END DO
C        
C        DO WHILE (.NOT. EOF)
C        
C           CALL RDTEXT ('INPUT.CMD', LINE, EOF)
C           CALL EVALCP (LINE, EOF, CMDSYM, CMDPTR, CMDVAL)
C           
C        END DO
C
C     To see how CPARSE is used in an application, see the 
C     NIOSPK main module.
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 8-OCT-1992 (MJS) 
C
C-&

C     
C     Standard SPICE error handling.
C     
      IF (RETURN()) THEN
         RETURN
      ELSE
         CALL CHKIN ('EVALCP')
      END IF

C     
C     If NKWD is zero, the parser hasn't been initialized.
C     
      IF (NKWD .EQ. 0) THEN   
         CALL SIGERR ('SPICE(PARSERNOTREADY)')
         CALL CHKOUT ('EVALCP'               )
         RETURN
      END IF

C
C     No error so far.
C
      ERR = .FALSE.
      
C     
C     If we reached the end of commands, we just need to evaluate
C     what's in the buffer.
C     
      IF (EOC) THEN   
      
         IF (KWD .EQ. ' ') THEN
            ACTION = 'DONE'
         ELSE
            ACTION = 'EVAL'
         END IF
         
      ELSE
      
         ACTION = 'MARK'
         LINES  = LINES + 1
      
      END IF

      DO WHILE (ACTION .NE. 'DONE' .AND. .NOT. FAILED())

C     
C        RETURN exits from the routine (gets the next line).
C              
         IF (ACTION .EQ. 'RETURN') THEN
            
            CALL CHKOUT ('EVALCP')
            RETURN
            
C     
C        MARK marks the position of the equal sign and comment 
C        character.
C     
         ELSE IF (ACTION .EQ. 'MARK') THEN
         
C     
C           Replace all tabs with spaces in the line, and left justify
C           it.
C     
            LN = LINE
            
            CALL REPLCH (LN, CHAR(TAB), ' ', LN)
            CALL LJUST  (LN, LN)
            
C     
C           A blank line or a comment line is of no use to EVALCP.
C     
            IF (LN .EQ. ' ' .OR. LN(1:1) .EQ. ';') THEN
               
               ACTION = 'RETURN'
               
            ELSE
            
C     
C              Mark the positions of the equal sign and comment 
C              character (EP and CP, respectively). If the comment 
C              character is non-existent, CP is set to the length of 
C              the line plus one (makes for easier parsing).
C     
               EP = INDEX (LN, '=')
               CP = INDEX (LN, ';')
            
               IF (CP .EQ. 0) THEN
                  CP = LASTNB(LN) + 1
               END IF
            
               ACTION = 'PARSE'
            
            END IF
            
C     
C        PARSE separates the keyword from the value, or if the line is
C        a continuation of the previous command, it appends the value.
C     
         ELSE IF (ACTION .EQ. 'PARSE') THEN
      
C           
C           If KWD is blank, this should be the first line of a
C           command.
C     
            IF (KWD .EQ. ' ') THEN

               LSTCMD = LINES
            
               IF (EP .GT. 1 .AND. EP .LT. CP) THEN
                  KWD   = LN(1   :EP-1)
                  VALUE = LN(EP+1:CP-1)
               
                  CALL LJUST (KWD,   KWD)
                  CALL UCASE (KWD,   KWD)
                  CALL LJUST (VALUE, VALUE)
                  
                  VLEN = LASTNB(VALUE)
               ELSE
                  MSG = 'Line #: Invalid command. Commands must ' //
     .                  'be of the form ''keyword = value.'''
                  
                  ERR = .TRUE.
                  CALL CHKOUT ('EVALCP')
                  RETURN
               END IF
                 
               ACTION = 'RETURN'
                 
            ELSE
            
C     
C              Is it a continuation of the previous command, or is it 
C              a new command? If EP is equal to 1, we still have new 
C              command, but an error will be signaled when we return 
C              to PARSE later.
C     
               IF (EP .GE. 1 .AND. EP .LT. CP) THEN
                  ACTION = 'EVAL'
               ELSE
                  I    = FRSTNB(LN)
                  VLEN = VLEN + CP - I + 1
                  
                  IF (VLEN .GT. VALLEN) THEN
                     MSG = 'Line #: Value of % command exceeds % ' //
     .                     'characters.'
                     CALL REPMC (MSG, '%', KWD,    MSG)
                     CALL REPMI (MSG, '%', VALLEN, MSG)

                     ERR = .TRUE.
                     CALL CHKOUT ('EVALCP')
                     RETURN
                  END IF

                  CALL SUFFIX (LN(1:CP-1), 1, VALUE)
                  
                  ACTION = 'RETURN'
               END IF
              
            END IF
                  
C     
C        EVAL evaluates the command, and stores it in the data 
C        structure.
C        
         ELSE IF (ACTION .EQ. 'EVAL') THEN
      
C     
C           Do we have an acceptable keyword?
C     
            KWDNUM = KWLOOK(KWD)
         
            IF (KWDNUM .EQ. 0 .OR. KWDNUM .EQ. HDNUM) THEN
               MSG = 'Line #: % is not an acceptable keyword.'
               CALL REPMC (MSG, '%', KWD, MSG)
               
               ERR = .TRUE.
               CALL CHKOUT ('EVALCP')
               RETURN
            END IF
         
C          
C           How about a value?
C          
            IF (VALUE .EQ. ' ') THEN
               MSG = 'Line #: % command has no value.'
               CALL REPMC (MSG, '%', KWD, MSG)
               
               ERR = .TRUE.
               CALL CHKOUT ('EVALCP')
               RETURN
            END IF
              
C          
C           Check if this keyword is a child node of the current level.
C           STKLEV is the current level (KWSTK(0) is HDNUM). If not, 
C           decrease STKLEV until we find a place for this keyword.
C     
            NEWLEV = STKLEV
            P      = CHLOOK(KWSTK(NEWLEV), KWDNUM)
         
            DO WHILE (NEWLEV .NE. 0 .AND. P .EQ. 0)
         
               NEWLEV = NEWLEV - 1
               P      = CHLOOK(KWSTK(NEWLEV), KWDNUM)
            
            END DO
         
C     
C           If P is still zero, KWD does not belong here.
C     
            IF (P .EQ. 0) THEN
               
               MSG = ' '
               K   = 0
               
C     
C              We'll try to be helpful by letting the user know
C              where this keyword may appear.
C     
               DO I = 1, NKWD
               
                  P = CHLOOK(I, KWDNUM)
                  
                  IF (P .NE. 0) THEN
                     K = K + 1
                     CALL SUFFIX (KWNAM(I), 1, MSG)
                  END IF
                  
               END DO
               
               IF (K .EQ. 1) THEN
                  CALL PREFIX ('Line #: Placement of the % command '  //
     .                         'is incorrect. The % command '         //
     .                         'may only appear after the', 0, MSG)
                  CALL SUFFIX ('command.', 1, MSG)
               ELSE
                  CALL PREFIX ('Line #: Placement of the % command '  //
     .                         'is incorrect. The % command '         //
     .                         'may only appear after the following ' //
     .                         'commands:', 0, MSG)
                  CALL SUFFIX ('.', 0, MSG)
               END IF
                    
               CALL REPMC (MSG, '%', KWD, MSG)
               CALL REPMC (MSG, '%', KWD, MSG)
               
               ERR = .TRUE.
               CALL CHKOUT ('EVALCP')
               RETURN
            END IF
         
C     
C           Check if we have the necessary commands between STKLEV 
C           and NEWLEV (have all the nodes been given a sufficient 
C           number of times)?
C     
            DO I = STKLEV, NEWLEV+1, -1

C              
C              J is the index in CHLNUM where the child keywords are
C              given for parent keyword index I.
C              
               J = CHLPTR(KWSTK(I)-1)

               DO K = 1, NUMCHL(KWSTK(I))
                  
                  IF (KWCNTR(K,I) .LT. NUMMIN(K+J)) THEN
                  
                     CALL INTTXT (NUMMIN(K+J), MINCH)
                     
                     IF (NUMMAX(K+J) .EQ. NUMMIN(K+J)) THEN
                        MSG = 'Line #: % %'
                        CALL LCASE (MINCH(2:), MINCH(2:))
                     ELSE
                        MSG = 'Line #: At least % %'
                        CALL LCASE (MINCH, MINCH)
                     END IF
                     
                     IF (NUMMIN(K+J) .EQ. 1) THEN
                        CALL SUFFIX ('command must appear after '  //
     .                               'the % command.', 1, MSG)
                     ELSE
                        CALL SUFFIX ('commands must appear after ' //
     .                               'the % command.', 1, MSG)
                     END IF
                     
                     CALL REPMC (MSG, '%', MINCH,              MSG)
                     CALL REPMC (MSG, '%', KWNAM(CHLNUM(K+J)), MSG)
                     CALL REPMC (MSG, '%', KWNAM(KWSTK(I)),    MSG)

                     LSTCMD = LINSTK(I) 
                     ERR    = .TRUE.
                     CALL CHKOUT ('EVALCP')
                     RETURN
                     
                  END IF
                  
               END DO
               
            END DO
            
C     
C           If we add the command to NEWLEV, will we exceed its 
C           maximum occurrence?
C     
            J = CHLPTR(KWSTK(I)-1)
            
            IF (KWCNTR(P,NEWLEV) .EQ. NUMMAX(P+J)) THEN
            
               IF (NUMMAX(P+J) .GT. 1) THEN
                  MSG = 'Line #: Only % % commands may appear'
               ELSE
                  MSG = 'Line #: Only % % command may appear'
               END IF
               
               CALL INTTXT (NUMMAX(P+J), MAXCH)
               CALL LCASE  (MAXCH,       MAXCH)
               
               CALL REPMC (MSG, '%', MAXCH, MSG)
               CALL REPMC (MSG, '%', KWD,   MSG)
               
               IF (NEWLEV .GT. 0) THEN   
                  CALL SUFFIX ('following the % command.', 1, MSG)
                  CALL REPMC (MSG, '%', KWNAM(KWSTK(NEWLEV)), MSG)
               ELSE
                  CALL SUFFIX ('.', 0, MSG)
               END IF
               
               ERR = .TRUE.
               CALL CHKOUT ('EVALCP')
               RETURN
               
            END IF
            
C     
C           Everything checks out. Update our records.
C     
            KWCNTR(P,NEWLEV) = KWCNTR(P,NEWLEV) + 1
            STKLEV           = NEWLEV + 1
            KWSTK(STKLEV)    = KWDNUM
            LINSTK(STKLEV)   = LSTCMD
            
C     
C           Zero out the number of child nodes for KWDNUM.
C                          
            CALL CLEARI (NUMCHL(KWDNUM), KWCNTR(1,STKLEV))
            
C     
C           Form the symbol.
C     
            SYMBOL = KWNAM(KWSTK(1))
            
            DO I = 2, STKLEV
               P      = CHLOOK (KWSTK(I-2), KWSTK(I-1))
               SYMBOL = CRTPTR (SYMBOL, KWCNTR(P,I-2), KWNAM(KWSTK(I)))
            END DO
            
            CALL SYENQC (SYMBOL, VALUE, CMDSYM, CMDPTR, CMDVAL)
            
C     
C           What next? If we're at the end of commands, we need to
C           check for closure. If not, we'll parse LINE (this time
C           with KWD set to ' ').
C     
            IF (EOC) THEN
               ACTION = 'DONE'
            ELSE
               ACTION = 'PARSE'
               KWD    = ' '
               VALUE  = ' '
            END IF
            
         END IF
         
      END DO

C     
C     We've evaluated the last of the commands. Here, we check for 
C     closure (we'll decrease STKLEV to zero, checking for minimum
C     child nodes).
C     
      NEWLEV = 0
      
      DO I = STKLEV, NEWLEV, -1
      
C              
C        J is the index in CHLNUM where the child keywords are
C        given for parent keyword index I.
C              
         J = CHLPTR(KWSTK(I)-1)

         DO K = 1, NUMCHL(KWSTK(I))
                  
            IF (KWCNTR(K,I) .LT. NUMMIN(K+J)) THEN
                  
               CALL INTTXT (NUMMIN(K+J), MINCH)

               IF (I .GT. 0) THEN
                  MSG = 'Line #:'
               ELSE
                  MSG = ' '
               END IF
                     
               IF (NUMMAX(K+J) .EQ. NUMMIN(K+J)) THEN
                  CALL SUFFIX ('% %', 1, MSG)
                  CALL LCASE  (MINCH(2:), MINCH(2:))
               ELSE
                  CALL SUFFIX ('At least % %', 1, MSG)
                  CALL LCASE  (MINCH, MINCH)
               END IF
                     
               CALL REPMC (MSG, '%', MINCH,              MSG)
               CALL REPMC (MSG, '%', KWNAM(CHLNUM(K+J)), MSG)
               
               IF (NUMMIN(K+J) .EQ. 1) THEN
                  CALL SUFFIX ('command must appear', 1,  MSG)
               ELSE
                  CALL SUFFIX ('commands must appear', 1, MSG)
               END IF
               
               IF (I .GT. 0) THEN   
                  CALL SUFFIX ('after the % command.', 1, MSG)
                  CALL REPMC (MSG, '%', KWNAM(KWSTK(I)),      MSG)
                  
                  LSTCMD = LINSTK(I) 
               ELSE
                  CALL SUFFIX ('.', 0, MSG)
               END IF

               CALL LJUST (MSG, MSG)
               ERR = .TRUE.

               CALL CHKOUT ('EVALCP')
               RETURN
                     
            END IF
                  
         END DO
               
      END DO
      
C     
C     That's it.
C     
      CALL CHKOUT ('EVALCP')
      RETURN



C$Procedure CPERR (Command parser error)
 
      ENTRY CPERR (REASON, LINNUM)

C$ Abstract
C
C     Return the reason and line number of a parser error.
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
C     PARSING
C     STRING
C
C$ Declarations
C     
C     CHARACTER*(*)         REASON
C     INTEGER               LINNUM
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      REASON    O    Reason for error.
C      LINNUM    O    Line number offset of the error.
C      
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     REASON     is the reason a parser error occurred. REASON may
C                contain a #, indicating the line number offset of the
C                error. The first line sent to EVALCP is line 1. The
C                marker # should be substituted the corrected line
C                number before REASON is displayed.
C
C     LINNUM     is the line offset number of the command that caused
C                the error. This number may not be needed if REASON
C                does not contain the substitution marker, #. For
C                example, REASON may be 'One LEAPSECONDS_FILE command
C                must appear.'. No line number is necessary in this
C                case.
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
C     None.
C
C$ Particulars
C
C     This routine should only be called if EVALCP indicates an error.
C     EVALCP should not be called after an error is found. Doing so
C     will produce undesired results, that is you can only find the
C     first error.
C     (Maybe in the next version this can be fixed.)
C      
C$ Examples
C
C     None.
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
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    Beta Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Balanced CHKIN/CHKOUT calls.
C 
C-    Beta Version 1.0.0, 11-DEC-1993 (MJS) 
C
C-&
      REASON = MSG 
      LINNUM = LSTCMD

      RETURN
      END
