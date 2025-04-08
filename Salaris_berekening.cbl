       IDENTIFICATION DIVISION.
           PROGRAM-ID. SalarisBerekening.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 Naam PIC X(30).
           01 TypeWerknemer PIC X.
           01 Brutoloon PIC 9(5)V99.
           01 RSZ PIC 9(5)V99.
           01 Voorheffing PIC 9(5)V99.
           01 NettoLoon PIC 9(5)V99.
    
       PROCEDURE DIVISION.
           DISPLAY "Geef de naam van de werknemer:".
           ACCEPT Naam.

           DISPLAY "Type werknemer (B voor bediende, A voor arbeider):".
           ACCEPT TypeWerknemer.

           DISPLAY "Geef het brutoloon:".
           ACCEPT Brutoloon.

           COMPUTE RSZ = Brutoloon * 0.1307.

           if  brutoloon <= 1318.33
            compute Voorheffing = brutoloon * 0.15
           else if brutoloon > 1318.33 and brutoloon <= 2326.66
            compute Voorheffing = brutoloon * 0.25
           else if brutoloon > 2326.66 and brutoloon <= 4026.66
            compute voorheffing = brutoloon * 0.45
            else if brutoloon > 4026.66
            compute voorheffing = brutoloon * 0.50
           end-if.

           COMPUTE NettoLoon = Brutoloon - RSZ - Voorheffing.

           DISPLAY "----------------------------------------".
           DISPLAY "Naam: " Naam.
           DISPLAY "Bruto: " Brutoloon.
           DISPLAY "RSZ: " RSZ.
           DISPLAY "Voorheffing: " Voorheffing.
           DISPLAY "Netto: " NettoLoon.
           DISPLAY "----------------------------------------".

           STOP RUN.
