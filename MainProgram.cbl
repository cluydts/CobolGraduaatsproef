       IDENTIFICATION DIVISION.
       PROGRAM-ID. RENTE-BEREKENING.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01  PRINCIPAL        PIC 9(8)V99.
           01  RATE             PIC 9(2)V99.
           01  TIME             PIC 9(2).
           01  INTEREST         PIC 9(8)V99.
           01  TOTAL-AMOUNT     PIC 9(8)V99.

       PROCEDURE DIVISION.
           DISPLAY "Voer het startkapitaal in: ".
           ACCEPT PRINCIPAL.
           DISPLAY "Voer de rentevoet in (bijv. 5.5): ".
           ACCEPT RATE.
           DISPLAY "Voer de looptijd in jaren in: ".
           ACCEPT TIME.

           COMPUTE INTEREST = (PRINCIPAL * RATE * TIME) / 100.
           COMPUTE TOTAL-AMOUNT = PRINCIPAL + INTEREST.

           DISPLAY "Totale bedrag na " TIME " jaar: " TOTAL-AMOUNT.

           STOP RUN.