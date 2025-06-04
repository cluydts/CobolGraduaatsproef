       IDENTIFICATION DIVISION.
           PROGRAM-ID. SalaryCalculation.
           environment division.
              input-output section.
                file-control.
                    select input-file assign to DYNAMIC-INFILE
                        organization is line sequential
                        file status is WS-INPUT-STATUS.
                    select output-file assign to DYNAMIC-OUTFILE
                        organization is line sequential
                        file status is WS-OUTPUT-STATUS.

           DATA DIVISION.
          
       
            FILE SECTION.
           FD input-file.
           01 LEESREGEL pic x(100).
    
           FD output-file.
           01 output-regel pic x(100).

       WORKING-STORAGE SECTION.

       
           01 EOF-FLAG PIC X(1) VALUE "0".

           01 DYNAMIC-INFILE pic x(30).
           01 DYNAMIC-OUTFILE pic x(30).
           01 WS-INPUT-STATUS  PIC XX.            
           01 WS-OUTPUT-STATUS PIC XX.
           01 OUTPUT-PREFIX pic x(8) value "Output-".
           01 HEADER-1 PIC X(21) VALUE "Naam,Type,Bruttoloon".
           01 HEADER-2 pic X(25) value "NettoLoon,RSZ,Voorheffing".
           01 FULL-HEADER pic x(46).

           *>    ---------------------------------------------------
           01 naam PIC X(30).             01 type-werknemer PIC X(8).
           01 brutoloon-in PIC x(7).
           *>    ---------------------------------------------------
           01 brutoloon PIC 9(5)V99.
           01 brutoloon-Arbeider PIC 9(5)V99.
           01 RSZ PIC 9(5)V99.
           01 Voorheffing PIC 9(5)V99.
           01 NettoLoon PIC 9(5)V99.
           01 BRUTO-AFTER-RSZ pic 9(9)V99.
           *>    ---------------------------------------------------
           01 brutoloon-out PIC Z(5).ZZ.
           01 brutoloon-Arbeider-out PIC Z(5).ZZ.
           01 RSZ-out PIC Z(5).ZZ.
           01 Voorheffing-out PIC Z(5).ZZ.
           01 NettoLoon-out PIC Z(5).ZZ.
           *>    ---------------------------------------------------

          
           linkage section.
           01 LINK-INPUT-FILE pic x(30).

       PROCEDURE DIVISION using LINK-INPUT-FILE.

           string 
           HEADER-1 delimited by space
               "," delimited by size
               HEADER-2 delimited by space
               into FULL-HEADER
               end-string

              MOVE LINK-INPUT-FILE TO DYNAMIC-INFILE.
              string
               OUTPUT-PREFIX delimited by size
               DYNAMIC-INFILE delimited by size
               into DYNAMIC-OUTFILE
              end-string

              
              OPEN INPUT input-file
               DISPLAY "Status after OPEN input-file: " WS-INPUT-STATUS
               IF WS-INPUT-STATUS NOT = "00"
                   DISPLAY "Error opening input file: " WS-INPUT-STATUS
                   GOBACK
               END-IF
                OPEN OUTPUT output-file
                 DISPLAY "Status after OPEN output-file: " WS-OUTPUT-STATUS
           IF WS-OUTPUT-STATUS NOT = "00"
               DISPLAY "Error opening output file: " WS-OUTPUT-STATUS
               GOBACK
           END-IF
                
                move FULL-HEADER to output-regel
                write output-regel
                move spaces to output-regel

                 read input-file into LEESREGEL *> skips Header
            DISPLAY "Status after 1st READ (header skip): " WS-INPUT-STATUS
           IF WS-INPUT-STATUS = "10" *> "10" is standard for EOF
               DISPLAY "EOF reached immediately after header read. Input file might be empty or just a header."
               MOVE "1" TO EOF-FLAG *> Ensure loop doesn't run if file is truly empty after header
           ELSE IF WS-INPUT-STATUS NOT = "00"
               DISPLAY "Error on 1st READ (header skip): " WS-INPUT-STATUS
               GOBACK
           END-IF
     *>    -------------------------------------------------------------
                
                PERFORM UNTIL EOF-FLAG = "1"
                   read input-file into LEESREGEL
                        AT END
                            MOVE "1" TO EOF-FLAG
           DISPLAY "AT END encountered in loop. Final input status: " WS-INPUT-STATUS
                        NOT AT END
                     DISPLAY "Status after data READ: " WS-INPUT-STATUS
                IF WS-INPUT-STATUS NOT = "00"
                  DISPLAY "I/O Error during data read: " WS-INPUT-STATUS
                   MOVE "1" TO EOF-FLAG *> Stop processing on error
                ELSE
                        unstring function trim(LEESREGEL)
                         DELIMITED BY ","
                                    or ", "
                                    into naam
                                         type-werknemer
                                         brutoloon-in
                        end-unstring
                        display " "
                display "leesregel-naam: " naam
                display "leesregel-type-werknemer: " type-werknemer
                display "leesregel-brutoloon: " brutoloon-in
*>    -------------------------------------------------------------

           move function numval(function trim(brutoloon-in)) to brutoloon
               display "brutoloon-in : " brutoloon-in
           move zeroes to brutoloon-in
                    display "brutoloon:" brutoloon
*>    -------------------------------------------------------------
           IF type-werknemer = "Bediende"
               COMPUTE RSZ = Brutoloon * 0.1307
               compute BRUTO-AFTER-RSZ = brutoloon - RSZ
               DISPLAY "RSZ berekend voor Bediende: " RSZ
           ELSE 
               IF type-werknemer = "Arbeider"
                   COMPUTE RSZ = Brutoloon * 1.08 * 0.1307
                   compute BRUTO-AFTER-RSZ = brutoloon - RSZ
                   DISPLAY "RSZ berekend voor Arbeider (incl. 8% toeslag): " RSZ
               ELSE
                   MOVE 0 TO RSZ
                   DISPLAY "⚠️ Onbekend type-werknemer: " type-werknemer
               END-IF
           END-IF
                  

                    display " "
*>    -------------------------------------------------------------
           IF BRUTO-AFTER-RSZ <= 1318.33
               COMPUTE Voorheffing = BRUTO-AFTER-RSZ * 0.15
               ELSE
                   IF BRUTO-AFTER-RSZ <= 2326.66
                       COMPUTE Voorheffing = BRUTO-AFTER-RSZ * 0.25
                   ELSE
                       IF BRUTO-AFTER-RSZ <= 4026.66
                           COMPUTE Voorheffing = BRUTO-AFTER-RSZ * 0.45
                   ELSE
                           COMPUTE Voorheffing = BRUTO-AFTER-RSZ * 0.50
                   END-IF
               END-IF
           END-IF
*>    -------------------------------------------------------------
                   COMPUTE NettoLoon = BRUTO-AFTER-RSZ - Voorheffing
*>    -------------------------------------------------------------

                   MOVE Brutoloon TO brutoloon-out
                   MOVE RSZ TO rsz-out
                   MOVE Voorheffing TO voorheffing-out
                   MOVE NettoLoon TO NettoLoon-out

                   move zeroes to brutoloon
                   move zeroes to RSZ
                   move zeroes to Voorheffing
                   move zeroes to NettoLoon

                   display "------------------------------------"
                   display"Naam: " naam
                   display "Type-werknemer: " type-werknemer
                   display "Brutoloon-out: " brutoloon-out
                   display"Nettoloon-out: " NettoLoon-out
                   display "RSZ-out" RSZ-out
                   display "Voorheffing-out" Voorheffing-out
                   display " " 
    *>    -------------------------------------------------------------
                   string
                   function trim(naam) delimited by size
                   "," delimited by size
                   function trim(type-werknemer) delimited by size
                   "," delimited by size
                   function trim(brutoloon-out) delimited by size
                   "," delimited by size
                   function trim(NettoLoon-out) delimited by size
                   "," delimited by size
                   function trim(RSZ-out) delimited by size
                   "," delimited by size
                   function trim(Voorheffing-out) delimited by size
                   "," delimited by size
                   into output-regel
                   end-string

                   write output-regel
               
*>    -------------------------------------------------------------
           DISPLAY "Status after WRITE output data: " WS-OUTPUT-STATUS
             IF WS-OUTPUT-STATUS NOT = "00"
               DISPLAY "Error writing data to output: " WS-OUTPUT-STATUS
               MOVE "1" TO EOF-FLAG *> Stop processing
             END-IF
               display "Output-regel: " output-regel
                   move spaces to naam
                   move spaces to type-werknemer
                   move zeroes to brutoloon-out
                   move zeroes to NettoLoon-out    
                   move zeroes to RSZ-out
                   move zeroes to Voorheffing-out  
                   move spaces to output-regel
               end-if
                END-READ
              END-PERFORM.

*>    -------------------------------------------------------------
                 CLOSE input-file
           DISPLAY "Status after CLOSE input-file: " WS-INPUT-STATUS
           CLOSE output-file
           DISPLAY "Status after CLOSE output-file: " WS-OUTPUT-STATUS
                DISPLAY "Salarisberekening voltooid."

           goback.
