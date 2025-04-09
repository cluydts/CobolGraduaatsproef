       IDENTIFICATION DIVISION.
           PROGRAM-ID. SalarisBerekening.
           environment division.
              input-output section.
                file-control.
                    select input-file assign to "input.txt"
                        organization is line sequential.
                    select output-file assign to "output.txt"
                        organization is line sequential.

       DATA DIVISION.
       
              FILE SECTION.
              FD input-file.
              01 input-record.
                05 naam PIC X(30).
                05 type-werknemer PIC X.
                05 brutoloon PIC 9(5)V99.
    
              FD output-file.
              01 output-record.
                05 naam-out PIC X(30).
                05 brutoloon-out PIC 9(5)V99.
                05 rsz-out PIC 9(5)V99.
                05 voorheffing-out PIC 9(5)V99.
                05 netto-out PIC 9(5)V99.

       WORKING-STORAGE SECTION.
   
           01 RSZ PIC 9(5)V99.
           01 Voorheffing PIC 9(5)V99.
           01 NettoLoon PIC 9(5)V99.
    
       PROCEDURE DIVISION.
              OPEN INPUT input-file
                OPEN OUTPUT output-file
                PERFORM UNTIL input-file = "EOF"
                    READ input-file INTO input-record
                        AT END
                            MOVE "EOF" TO input-file
                        NOT AT END
                            MOVE naam TO Naam
                            MOVE type-werknemer TO TypeWerknemer
                            MOVE brutoloon TO Brutoloon
                        END-READ

                    IF TypeWerknemer = "A"
                        COMPUTE RSZ = Brutoloon * 0.1307
                    ELSE IF TypeWerknemer = "B"
                        COMPUTE RSZ = Brutoloon * 0.1307
                    ELSE IF TypeWerknemer = "C"
                        COMPUTE RSZ = Brutoloon * 0.1307
                    END-IF.


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

           STOP RUN.
