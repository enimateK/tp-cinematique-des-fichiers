IDENTIFICATION DIVISION.
PROGRAM-ID. TP1resultats.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.

       SELECT fresultats ASSIGN TO "resultats.dat"
       ORGANIZATION sequential 
       ACCESS IS sequential
       FILE STATUS IS fresult_stat.

DATA DIVISION.
FILE SECTION.
        FD fresultats.
        01 resultTampon.
          02 fres_noebur PIC A(4).
          02 fres_nb1 PIC 9(12).
          02 fres_nb2 PIC 9(12).

WORKING-STORAGE SECTION.
       77 fresult_stat PIC 9(2).
       77 Wrep PIC 9.
       77 Wtrouve PIC 9.
       77 Wfin PIC 9.
       77 Wnoebur PIC A.
*>       77 Wnb1 PIC 9.
*>       77 Wnb2 PIC 9.
       77 Wvote PIC 9.
       77 Wi PIC 9.
       77 Wn PIC 9.

PROCEDURE DIVISION.



       PERFORM AJOUT_RESULTAT
       CLOSE fresultats
*>       PERFORM AFFICHAGE_RESULTAT

       STOP RUN.


       AJOUT_RESULTAT.

*>  Recherche le nombre de votes
       PERFORM WITH TEST AFTER UNTIL Wrep = 0
       DISPLAY 'Entrez le nom du bureau de vote'
       ACCEPT Wnoebur
       OPEN INPUT fresultats
       MOVE 0 TO Wn
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 OR Wfin = 1
              READ fresultats
              ADD 1 TO Wn
              AT END MOVE 1 TO Wfin
                     MOVE 0 TO Wnb1
                     MOVE 0 TO Wnb2
              NOT AT END
                     IF Wnoebur = fres_noebur THEN
                          MOVE fres_nb1 TO Wnb1
                          MOVE fres_nb2 TO Wnb2
                          MOVE 1 TO Wtrouve 
                     END-IF
       END-PERFORM
*> ouvre le fichier ??lecteurs
       OPEN EXTEND fresultats
       IF fresult_stat =35 THEN
         OPEN OUTPUT fresultats
       CLOSE fresultats
       OPEN EXTEND fresultats
       END-IF
*> vote

       DISPLAY 'Votez pour le candidat 1 ou 2'
       ACCEPT Wvote
       IF Wvote = 1 THEN
              ADD 1 TO fres_nb1
       ELSE 
              ADD 1 TO fres_nb2
       END-IF

       CLOSE fresultats
*> Enregistrement
*> Si le bureau existe d??j??
       OPEN EXTEND fresultats
       IF Wtrouve = 1
              PERFORM WITH TEST AFTER UNTIL Wi = Wn
                      READ fresultats
                     ADD 1 TO Wi 
              END-PERFORM             
*> si le bureau n'existe pas, le cr??er       ELSE 


       WRITE resultTampon END-WRITE
       PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
            DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
            ACCEPT Wrep
       END-PERFORM
       END-PERFORM.
