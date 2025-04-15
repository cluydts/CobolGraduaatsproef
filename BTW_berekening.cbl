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

           01 ORIGINELE-PRIJS-S pic X(8).
           01 BTW-TARIEF-S pic x(2).

           working-storage section.
           01 ORIGINELE-PRIJS pic 9(6)V99.
           01 BTW-TARIEF pic 9(2).
           01 BTW-BEDRAG pic 9(5)V99.
           01 TOTAAL-BEDRAG pic 9(7)V99.
           
           01 DISPLAY-ORIGINELE-PRIJS pic Z(6).ZZ.
           01 DISPLAY-BTW-TARIEF pic Z(2).
           01 DISPLAY-BTW-BEDRAG pic Z(5).ZZ.
           01 DISPLAY-TOTAAL-BEDRAG pic Z(7).ZZ.

           procedure division.

               open inpite input-file
               open output output-file
               read input-file into 

           evaluate BTW-TARIEF
            when 6
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.06
            when 12
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.12
            when 21
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.21
            when other
               display "Niet één van de tarieven gekozen."
               stop run
           end-evaluate.

            compute TOTAAL-BEDRAG = ORIGINELE-PRIJS + BTW-BEDRAG.

           move ORIGINELE-PRIJS to DISPLAY-ORIGINELE-PRIJS.
           move BTW-TARIEF to DISPLAY-BTW-TARIEF.
           move BTW-BEDRAG to DISPLAY-BTW-BEDRAG.
           move TOTAAL-BEDRAG to DISPLAY-TOTAAL-BEDRAG.

           display "Gegeven bedrag: " DISPLAY-ORIGINELE-PRIJS.
           display "BTW-tarief: " DISPLAY-BTW-TARIEF.
           display "BTW-bedrag: " DISPLAY-BTW-BEDRAG
           display "Totaal bedrag" DISPLAY-TOTAAL-BEDRAG.

           stop run.
           