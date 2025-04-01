           identification division.
           program-id. BTW-PROGRAM.
           data division.
           working-storage section.
           01 ORIGINELE-PRIJS pic9(6)V(2).
           01 BTW-TARIEF pic9(2).
           01 BTW-BEDRAG pic9(5)V(2).
           01 TOTAAL-BEDRAG pic9(7)V(2).

           procedure division.
           display "Voer een bedrag in (zonder BTW): "
           accept ORIGINELE-PRIJS

           display "Kies een btw-percentage:"
           display "6 - 6%"
           display "12 - 12%"
           display "21 - 21%"
           accept BTW-TARIEF

           evaluate BTW-TARIEF
            when 6
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.06
            when 12
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.12
            when 21
               compute BTW-BEDRAG = ORIGINELE-PRIJS * 0.21
            when other
               display "Geen van de gekozen tarieven gekozen."
               stop run.
           end-evaluate.

            compute TOTAAL-BEDRAG = ORIGINELE-PRIJS + BTW-BEDRAG.
           display "gegeven bedrag: " ORIGINELE-PRIJS.
           display "btw-tarief: " BTW-TARIEF.
           display "totaal bedrag" TOTAAL-BEDRAG.

           stop run.
           