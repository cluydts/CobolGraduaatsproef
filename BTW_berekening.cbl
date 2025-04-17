           identification division.
           program-id. BTW-PROGRAM.
           environment division.
              input-output section.
                file-control.
                     select input-file assign to "BTW-Input.csv"
                         organization is line sequential.
                     select output-file assign to "BTW-Output.csv"
                         organization is line sequential.
           data division.

              file section.
           fd input-file.
           01 LEESREGEL pic x(80).

        

           fd output-file.
           01 OUTPUT-REGEL pic x(80).
           01  EOF-Flag PIC X(1) VALUE "0".

           working-storage section.
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

           procedure division.

               open input input-file
               open output output-file
               read input-file into LEESREGEL
               
               display "LEESREGEL: " LEESREGEL

               perform until EOF-Flag = "1"
                  read input-file into LEESREGEL
                  at end 
                  move "1" to EOF-Flag
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

              string
                  DISPLAY-ORIGINELE-PRIJS delimited by size
                  "," delimited by size
                  DISPLAY-BTW-TARIEF delimited by size
                  "," delimited by size
                  DISPLAY-BTW-BEDRAG delimited by size
                  "," delimited by size
                  DISPLAY-TOTAAL-BEDRAG delimited by size
                  into OUTPUT-REGEL

                  write OUTPUT-REGEL

           end-read
           end-perform .

           close input-file.
           close output-file.

           stop run.
           