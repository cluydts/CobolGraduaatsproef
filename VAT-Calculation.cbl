       identification division.
       program-id. VATCalculation.
       environment division.
       input-output section.
       file-control.
             select INPUT-FILE assign to DYNAMIC-INFILE
                 organization is line sequential.
             select OUTPUT-FILE assign to DYNAMIC-OUTFILE
                 organization is line sequential.

       data division.
       linkage section.
           01 LINK-INPUT-FILE pic x(30).

       file section.
           fd INPUT-FILE.
           01 LEESREGEL pic x(30).
           
           fd OUTPUT-FILE.
           01 OUTPUT-REGEL pic x(30).

           01  EOF-FLAG PIC X(1) VALUE "0".
           01 OUTPUT-REGEL-LEN   PIC 9(2).

       working-storage section.
           01 DYNAMIC-INFILE pic x(30).
           01 DYNAMIC-OUTFILE pic x(30).
           01 OUTPUT-PREFIX pic x(8) value "Output-".

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

       procedure division using LINK-INPUT-FILE.
           
           move LINK-INPUT-FILE to DYNAMIC-INFILE.
           string
           OUTPUT-PREFIX delimited by size

           DYNAMIC-INFILE delimited by size
           into DYNAMIC-OUTFILE
           end-string

           open input INPUT-FILE
           open output OUTPUT-FILE
           read INPUT-FILE into LEESREGEL
              
               
            display "LEESREGEL: " LEESREGEL

            perform until EOF-FLAG = "1"
               read INPUT-FILE into LEESREGEL
               at end 
               move "1" to EOF-FLAG
               not at end 
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

           move ORIGINELE-PRIJS to DISPLAY-ORIGINELE-PRIJS
           move BTW-TARIEF to DISPLAY-BTW-TARIEF
           move BTW-BEDRAG to DISPLAY-BTW-BEDRAG
           move TOTAAL-BEDRAG to DISPLAY-TOTAAL-BEDRAG

             display "DISPLAY-ORIGINELE-PRIJS: " DISPLAY-ORIGINELE-PRIJS
             display "DISPLAY-BTW-TARIEF: " DISPLAY-BTW-TARIEF
             display "DISPLAY-BTW-BEDRAG: " DISPLAY-BTW-BEDRAG
             display "DISPLAY-TOTAAL-BEDRAG: " DISPLAY-TOTAAL-BEDRAG
             display " "

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
                  display "OUTPUT-REGEL: " OUTPUT-REGEL
           display "---------------------------------------------------"

           end-read
           end-perform .

           close INPUT-FILE.
           close OUTPUT-FILE.

           goback.
           