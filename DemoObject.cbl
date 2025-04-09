           identification division.
           
           program-id. DemoObject.
              
           data division.
           working-storage section.
           01 klant.
             05 naam pic x(15) value "Jeroen Cluydts".
             05 adres pic x(15) value "Zevenhoek 20".
             05 postcode pic x(6) value "9400".
             05 plaats pic x(20) value "Voorde".


           procedure division.
            display klant.

           stop run.
