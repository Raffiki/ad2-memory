# Taak 3 Algoritmen & Datastructuren II: Lazy Buddy System

## Motivatie

* Ik koos ervoor om per index twee extra vectoren te bewaren: 
  * `d-tab` met daarin per index een `pair` met in de `car` het aantal allocaties en in de `cdr` het aantal lokaal vrije blokken. Zowel voor de `car` als `cdr` zijn er helper functies om hun waarde te incrementen of decrementen.
  * `free-tail-tab` met daarin voor elke index het adres van het laatste (lokaal of globaal) vrije blok. 

* Door de komst van de `free-tail-tab` is het beheer van de freelist complexer geworden. Er zijn vier helper functies toegevoegd:
  * `push-freelist-head!` en `push-freelist-tail!` voegen een geheugenblok toe aan de lijst.
  * `pop-freelist!` en `extract-freelist!` verwijderen een geheugenblok van de lijst.
  * Grofweg verleggen die functies enkel pointers tussen `previous` en `next` maar het is verder zaak om zowel `free-tail-tab` als `free-tab` aan te passen als de freelist leeg wordt.
  
* Het alloceren van een blok volgt volgende regels:
  * Als er een lokaal vrij blok, wordt dit gealloceerd. De D-index wordt mee geupdate (`Ai` wordt `Ai + 1`, `Li` wordt `Li - 1`).
  * Zo niet, als er een globaal vrij blok is, wordt dit gealloceerd en D-index wordt aangepast (`Ai` wordt `Ai + 1`).
  * Zo niet, zal er recursief op de volgende index een blok worden gesplitst:
    * Bij het splitsen van een vrij blok moeten we zorgen dat de invariant van de D-index (`Ai - Li >= 0`) bewaard blijft. 1 stuk van de split zal lokaal vrij worden, de buddy ofwel gealloceerd op de index, ofwel verder opgesplitst worden. In beide gevallen zullen we ook de teller van de allocaties op die index verhogen. Doen we dit niet, kan `Ai - Li` negatief worden. 
  
* Het dealloceren van een blok volgt volgende regels:
  * Als de D-index groter dan 2 is, wordt het blok lokaal vrij gemaakt.
  * Als de D-index 1 is, wordt het blok globaal vrij gemaakt.
  * Als de D-index 0 is, wordt het blok globaal vrij gemaakt. Om er alweer voor te zorgen dat `A1 - L1` niet negatief wordt, wordt het eerste lokaal vrije blok, globaal vrij gemaakt. Aangezien de D-index 0 is, en er een deallocatie aan de gang is, is er zeker een lokaal vrij blok.
  
* Een blok lokaal vrij maken (`push-free-locally`) zal enkel het blok vooraan de freelist hangen.

* Een blok globaal vrij maken (`push-free-globally`) probeert dit blok te mergen met zijn buddy indien die ook lokaal vrij is. Anders wordt dit blok achteraan de freelist gehangen.

* Er is een `(debug-memory)` functie gemaakt die het volledige geheugen doorloopt en per blok het adres, het type en de grootte teruggeeft.
