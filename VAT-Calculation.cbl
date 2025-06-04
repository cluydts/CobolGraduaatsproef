       identification division.
       program-id. VATCalculation.
       environment division.
       input-output section.
       file-control.
             select INPUT-FILE assign to DYNAMIC-INFILE
                 organization is line sequential
                 file status is WS-INPUT-STATUS.
             select OUTPUT-FILE assign to DYNAMIC-OUTFILE
                 organization is line sequential
                 file status is WS-OUTPUT-STATUS.

       data division.
   
       file section.
           fd INPUT-FILE.
           01 LEESREGEL pic x(100).
           
           fd OUTPUT-FILE.
           01 OUTPUT-REGEL pic x(100).

       working-storage section.

           01  EOF-FLAG PIC X(1) VALUE "0".
           01 DYNAMIC-INFILE pic x(30).
           01 DYNAMIC-OUTFILE pic x(30).
           
           01 WS-INPUT-STATUS  PIC XX.            
           01 WS-OUTPUT-STATUS PIC XX.
           01 OUTPUT-PREFIX pic x(8) value "Output-".

   01 HEADER PIC X(41) VALUE "prijs,BTW-Tarief,BTW-bedrag,Totaal-bedrag".

           01 ORIGINELE-PRIJS pic 9(6)V99.
           01 BTW-TARIEF pic 9(2).
           01 BTW-BEDRAG pic 9(5)V99.
           01 TOTAAL-BEDRAG pic 9(7)V99.

           01 ORIGINELE-PRIJS-S pic X(8).
           01 BTW-TARIEF-S pic x(2).
           
           01 DISPLAY-ORIGINELE-PRIJS pic Z(6).ZZ.
           01 DISPLAY-BTW-TARIEF pic Z(2).
           01 DISPLAY-BTW-BEDRAG pic Z(5).ZZ.
           01 DISPLAY-TOTAAL-BEDRAG pic Z(7).ZZ.

       linkage section.
           01 LINK-INPUT-FILE pic x(30).

       procedure division using LINK-INPUT-FILE.
           
           move LINK-INPUT-FILE to DYNAMIC-INFILE.
           string
           OUTPUT-PREFIX delimited by size
           DYNAMIC-INFILE delimited by size
           into DYNAMIC-OUTFILE
           end-string

           open input INPUT-FILE
           DISPLAY "Status after OPEN input-file: " WS-INPUT-STATUS
               IF WS-INPUT-STATUS NOT = "00"
                   DISPLAY "Error opening input file: " WS-INPUT-STATUS
                   GOBACK
               END-IF
           open output OUTPUT-FILE

           DISPLAY "Status after OPEN output-file: " WS-OUTPUT-STATUS
           IF WS-OUTPUT-STATUS NOT = "00"
               DISPLAY "Error opening output file: " WS-OUTPUT-STATUS
               GOBACK
           END-IF

           read INPUT-FILE into LEESREGEL *> skips Header
              
           DISPLAY "Status after 1st READ (header skip): " WS-INPUT-STATUS
           IF WS-INPUT-STATUS = "10" *> "10" is standard for EOF
               DISPLAY "EOF reached immediately after header read. Input file might be empty or just a header."
               MOVE "1" TO EOF-FLAG *> Ensure loop doesn't run if file is truly empty after header
               ELSE 
                   IF WS-INPUT-STATUS NOT = "00"
                   DISPLAY "Error on 1st READ (header skip): " WS-INPUT-STATUS
                   GOBACK
           END-IF
           *>    -------------------------------------------------------------

            perform until EOF-FLAG = "1"
               read INPUT-FILE into LEESREGEL
               at end 
               move "1" to EOF-FLAG
           DISPLAY "AT END encountered in loop. Final input status: " WS-INPUT-STATUS

               not at end
           DISPLAY "Status after data READ: " WS-INPUT-STATUS
                IF WS-INPUT-STATUS NOT = "00"
                  DISPLAY "I/O Error during data read: " WS-INPUT-STATUS
                   MOVE "1" TO EOF-FLAG *> Stop processing on error
                ELSE
                  display "LEESREGEL: " LEESREGEL
                  
                     UNSTRING function trim(LEESREGEL)
                        DELIMITED BY ","
                                    or " "
                                    or ", "
                        INTO ORIGINELE-PRIJS-S 
                             BTW-TARIEF-S
                     end-unstring
          
           display "ORIGINELE-PRIJS-S: " ORIGINELE-PRIJS-S
           display "BTW-TARIEF-S: " BTW-TARIEF-S

           MOVE function numval-c(function trim(ORIGINELE-PRIJS-S)) TO ORIGINELE-PRIJS
           MOVE function numval(function trim(BTW-TARIEF-S)) TO BTW-TARIEF

           display "ORIGINELE-PRIJS: " ORIGINELE-PRIJS
           display "BTW-TARIEF: " BTW-TARIEF
           display " "

           *>    -------------------------------------------------------------

           evaluate BTW-TARIEF
            when 6
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.06
            when 12
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.12
            when 21
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.21
            when other
               display "geen geldig tarief gebruikt."
               stop run
           end-evaluate

           compute TOTAAL-BEDRAG = ORIGINELE-PRIJS + BTW-BEDRAG

           *>    -------------------------------------------------------------

           move ORIGINELE-PRIJS to DISPLAY-ORIGINELE-PRIJS
           move BTW-TARIEF to DISPLAY-BTW-TARIEF
           move BTW-BEDRAG to DISPLAY-BTW-BEDRAG
           move TOTAAL-BEDRAG to DISPLAY-TOTAAL-BEDRAG

             display "DISPLAY-ORIGINELE-PRIJS: " DISPLAY-ORIGINELE-PRIJS
             display "DISPLAY-BTW-TARIEF: " DISPLAY-BTW-TARIEF
             display "DISPLAY-BTW-BEDRAG: " DISPLAY-BTW-BEDRAG
             display "DISPLAY-TOTAAL-BEDRAG: " DISPLAY-TOTAAL-BEDRAG
             display " "
           *>    -------------------------------------------------------------
              string
                  function trim(DISPLAY-ORIGINELE-PRIJS) delimited by size
                  "," delimited by size
                 function trim(DISPLAY-BTW-TARIEF) delimited by size
                  "," delimited by size
                 function trim(DISPLAY-BTW-BEDRAG) delimited by size
                  "," delimited by size
                function trim(DISPLAY-TOTAAL-BEDRAG) delimited by size
                  into OUTPUT-REGEL
                 
                  write OUTPUT-REGEL
           DISPLAY "Status after WRITE output data: " WS-OUTPUT-STATUS
           IF WS-OUTPUT-STATUS NOT = "00"
               DISPLAY "Error writing data to output: " WS-OUTPUT-STATUS
               MOVE "1" TO EOF-FLAG *> Stop processing
           END-IF
                  display "OUTPUT-REGEL: " OUTPUT-REGEL
           display "---------------------------------------------------"
                       
                       move zeroes to DISPLAY-ORIGINELE-PRIJS 
                       move zeroes to DISPLAY-BTW-TARIEF 
                       move zeroes to DISPLAY-BTW-BEDRAG
                       move zeroes to DISPLAY-TOTAAL-BEDRAG
           end-if
           end-read
           end-perform .
           *>    -------------------------------------------------------------
           close INPUT-FILE
               DISPLAY "Status after CLOSE input-file: " WS-INPUT-STATUS
           close OUTPUT-FILE
             DISPLAY "Status after CLOSE output-file: " WS-OUTPUT-STATUS
             DISPLAY "Salarisberekening voltooid."

           goback.
           