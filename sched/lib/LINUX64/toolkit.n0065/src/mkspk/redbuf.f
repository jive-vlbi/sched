C$Procedure      REDBUF ( Read input data into the buffer line )

      SUBROUTINE REDBUF ( INPUNT, ENDLIN, NLNREC, EPOCFL, 
     .                    BUFFER, LSTBUF, BUFAUX, EOF ) 
                                 
C$ Abstract
C
C     This routine is a module of the MKSPK program. It load text 
C     from the input data file into the line buffer.
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
C     MKSPK User's Guide
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT              NONE
      INCLUDE               'mkspk.inc' 

      INTEGER               INPUNT
      CHARACTER*(*)         ENDLIN 
      INTEGER               NLNREC 
      INTEGER               EPOCFL
      CHARACTER*(*)         BUFFER
      INTEGER               LSTBUF
      CHARACTER*(*)         BUFAUX
      LOGICAL               EOF

C$ Brief_I/O        
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INPUNT     I   Input data file unit
C     ENDLIN     I   Data delimiter character
C     NLNREC     I   Number of lines per one record of input file
C     EPOCFL     I   Flag of epoch string value processing
C     BUFFER    I/O  Main line buffer
C     LSTBUF    I/O  Length of string stored in main (on input) or 
C                    auxiliary (on output) buffer
C     BUFAUX     O   Auxiliary line buffer
C     EOF        O   End of file flag
C
C$ Detailed_Input
C
C     INPUNT      is the Fortran logical unit connected to the input 
C                 data file.
C
C     ENDLIN      is a character that should be substituted in place 
C                 on the end-of-lines in the input data file. Normally
C                 this is delimiter specified by a user in the setup 
C                 file.
C
C     NLNREC      is number of lines containing in single record of
C                 the input file.
C
C     EPOCFL      is the flag that defined a way of epoch string value 
C                 processing:     
C
C                    EPOCFL = 1, if the length of epoch string 
C                                is defined.
C
C                    EPOCFL = 2, if data delimiter is not a character
C                                allowed in SPICE time strings (comma 
C                                or white space).
C
C                    EPOCFL = 3, if data delimiter is a character 
C                                allowed in SPICE time strings.
C     
C     BUFFER      is the line buffer into which the text from the 
C                 input data file is loaded for future parsing. On 
C                 the input this buffer contains value of the BUFAUX
C                 which was returned on the previous iteration.
C
C     LSTBUF      is the length of the string stored in the main
C                 buffer on the input.
C
C$ Detailed_Output
C
C     BUFFER      is the line buffer into which the text from the 
C                 input data file is loaded for future parsing. On the 
C                 output the BUFFER contains text of one or more input 
C                 data records. If the last record in the BUFFER was 
C                 not loaded completely, the remaining part of it is 
C                 stored in BUFAUX.        
C  
C     LSTBUF      is the length of the string stored in auxillary
C                 buffer on the output.
C
C     BUFAUX      is the auxillary line buffer into which contains the 
C                 remainder of the text of the last record loaded into 
C                 BUFFER if that record was not loaded completely.
C  
C     EOF         is end-of-file flag. Set to .TRUE. when the 
C                 end of input data file is reached.
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
C     This routine read data from the input data file connected to the 
C     INPUNT logical unit.
C
C$ Particulars
C
C     None.
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
C     N.G. Khavenson (IKI RAS, Russia)
C     B.V. Semenov   (NAIF, JPL)
C
C$ Version
C
C-    Version 1.0.1, 18-MAR-1999 (BVS).
C
C        Corrected EORMRK substitution logic to handle zero length 
C        empty files at the end of the input data lines. Removed NL
C        from argument list and made it SAVEd variable with initial 
C        value 0. Re-ordered argument list to place input parameters 
C        before output parameters. Corrected comments.
C
C-    Version 1.0.1, 15-NOV-1998 (NGK).
C
C
C-    Version 1.0.0, 8-SEP-1998 (NGK).
C
C-&
 
C$ Index_Entries
C
C     Load part input file text into MKSPK buffer line. 
C
C-&                       

C                              
C     SPICELIB functions
C
      INTEGER               LASTNB
      INTEGER               POSR
      LOGICAL               RETURN

C
C     Local variables
C       
C
C     Size LINLEN is defined in include file
C
      INTEGER               LINELN
      PARAMETER           ( LINELN = LINLEN + 1 )

      CHARACTER*(LINELN)    LINE 
      CHARACTER*(LINELN)    WRKCHR

      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               N1
      INTEGER               N2
      
 
      LOGICAL               GTLN 
      LOGICAL               EOB
      
C
C     Counter of the non-blank data lines in the input. We need to 
C     keep track of the non-blank lines if go with LINES PER RECORD
C     parsing method.
C
      INTEGER               NL  
      SAVE                  NL
      
      DATA                  NL / 0 /
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REDBUF' ) 
      END IF

C
C     Loop until line buffer is not full. Reset end-of-buffer flag 
C     and counters before entering the loop.
C
      EOB   = .FALSE.  
      FIRST =  LSTBUF + 1 
      GTLN  = .TRUE.

      DO WHILE ( .NOT. EOB )

C
C        Check whether get-next-line flag is set. If it is, read
C        a non-blank line from the file.
C
         IF ( GTLN ) THEN 
                     
            CALL READLN ( INPUNT, LINE, EOF )

            IF ( EOF ) THEN
                  
               LINE = ' '

            ELSE IF ( LINE .EQ. ' ' ) THEN
 
               WRKCHR = ' '

               DO WHILE ( WRKCHR .EQ. ' ' .AND. .NOT. EOF )

                  CALL READLN ( INPUNT, WRKCHR, EOF )

                  IF ( EOF ) THEN 

                     LINE = ' '

                  ELSE   
                
                     LINE = WRKCHR

                  END IF
   
               END DO

            END IF

            NL = NL + 1 
                 
         END IF 
                  
C
C        Set get-next-line flag to TRUE. This value will be
C        overwritten by FALSE if buffer is full.
C
         GTLN = .TRUE.

C
C        If we actually got a new line (but not encountered
C        end-of-file), we add this line to a buffer.
C
         IF ( .NOT. EOF ) THEN

            CALL LJUST ( LINE, LINE )
            LAST = LASTNB ( LINE ) + 1 
               
C
C           If delimiter is omitted at the end of data line,
C           we add it at the and of buffer data before get new line.
C 
            IF ( LINE ( LAST - 1 : LAST - 1 ) .EQ. ENDLIN ) THEN
      
               LAST = LAST - 1   
               
            ELSE
            
               LINE ( LAST : LAST ) = ENDLIN
               
            END IF    
              
            IF ( EPOCFL .EQ. 3 ) THEN
            
C
C              If delimiter is allowed for time string
C              we put end of record marker instead of delimiter
C              at the end of last line of current record.
C              End of record marker is declared in include file.
C
               IF ( MOD ( NL, NLNREC ) .EQ. 0 ) THEN 
                     
                  LINE ( LAST : LAST ) = EORMRK
                     
               END IF

            END IF                       

C            
C           Check if we have space in the current buffer line.  
C
            IF ( ( FIRST + LAST - 1 ) .LE. BUFLEN ) THEN
            
C              We append input line at the end of the
C              current buffer line.
C                                                      
               BUFFER ( FIRST:( FIRST + LAST - 1 ) ) = LINE ( : LAST )
               FIRST = FIRST + LAST 
               BUFAUX  = ' '
               
            ELSE

C
C              There was not space in the  buffer line.
C              Set corresponding flags and proceed.
C
               EOB = .TRUE.
               GTLN = .FALSE.  
               
            END IF

         ELSE

C
C           We reached end-of-file. Set flag to exit the loop and
C           proceed.
C
            EOB = .TRUE.
            LAST = 1
         
         END IF

C
C     End of the "filling line buffer" loop.
C
      END DO  
      
      IF ( EPOCFL .EQ. 3 ) THEN
      
         N2 = FIRST - 1
         N1 = POSR ( BUFFER, EORMRK, N2 )
         LSTBUF = LAST + N2 - N1
          
         IF ( N1 .NE. N2 ) THEN  
         
            BUFAUX ( : LSTBUF ) = BUFFER ( N1 + 1 : N2 )//
     .                                   LINE ( : LAST )
            BUFFER ( N1 + 1 : ) = ' '
            
         ELSE
         
            BUFAUX ( : LSTBUF ) = LINE ( : LAST )
            
         END IF   
               
      ELSE
      
         LSTBUF = LAST
         BUFAUX ( : LSTBUF ) = LINE ( : LAST )
       
      END IF
               
      CALL CHKOUT ( 'REDBUF' )
            
      RETURN
         
      END
