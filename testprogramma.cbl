           identification division.
           program-id. testprogramma.
           environment division.
           input-output section.
           file-control.
               select Invoerbestand assign to "Testbestand.txt"
                   organization is line sequential.
               select Uitvoerbestand assign to "UitvoerTestBestand.txt"
                   organization is line sequential.

           data division.
           file section.
           fd Invoerbestand.
           01 Leesregel pic x(5).
           01 EOF-Flag PIC 9 VALUE 0.

           FD Uitvoerbestand.
           01 UitvoerRegel PIC X(36).
  
           working-storage section.
           01 Geconverteerd-Getal pic 9(5).
           01 Resultaat pic Z(5).

           procedure division.
           open input Invoerbestand.
           open output Uitvoerbestand.
           perform until EOF-Flag = 1
               read Invoerbestand into Leesregel
               at end  
                   move 1 to EOF-Flag
               not at end
                   move function numval(Leesregel) to Geconverteerd-Getal
                   compute Resultaat = Geconverteerd-Getal * 2
                   display "Origineel: " Leesregel " | verdubbeld: " Resultaat

                   string "Origineel: " Leesregel " | verdubbeld: " Resultaat
                       Resultaat delimited by size into UitvoerRegel
                   write UitvoerRegel
               end-read
           end-perform.
           
           close Invoerbestand.
           close Uitvoerbestand.
           stop run.
                        