           identification division.
           program-id. BTW-PROGRAM.
           data division.
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
           display "Voer een bedrag in (zonder BTW): "
           accept ORIGINELE-PRIJS.

           display "Kies een btw-percentage:"
           display "6 - 6%"
           display "12 - 12%"
           display "21 - 21%"
           accept BTW-TARIEF.

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
           