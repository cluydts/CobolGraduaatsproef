           identification division.
           program-id. Salaris_berekening.

           data division.
           working-storage section.
           01 BRUTO-SALARIS pic 9(5)v99.
           01 KEUZE pic 9(1).

           


           procedure division.
           
           display "Bediende of Arbeider?".
           display "1 - Bediende".
           display "2 - Arbeider".
           accept KEUZE.

         
           

           end run.
           