IDENTIFICATION DIVISION.
PROGRAM-ID. electeurs.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT felecteurs ASSIGN TO "felecteurs.dat"
        ORGANIZATION sequential
        ACCESS IS sequential
        FILE STATUS IS felec_stat.
        SELECT fmobilite ASSIGN TO "fmobilete.dat"
        ORGANIZATION sequential
        ACCESS IS sequential
        FILE STATUS IS fmobi_stat.

DATA DIVISION.
FILE SECTION.
        FD felecteurs.
        01 elecTampon.
          02 fe_id PIC 9(15).
          02 fe_nom PIC X(30).
          02 fe_prenom PIC X(30).
          02 fe_age PIC 9(3).
          02 fe_villeHabitation PIC X(30).
        FD fmobilite.
        01 mobiTampon.
          02 fm_id PIC 9(15).
          02 fm_nom PIC X(30).
          02 fm_prenom PIC X(30).
          02 fm_age PIC 9(3).
          02 fm_villeHabitation PIC X(30).
WORKING-STORAGE SECTION.
        77 felec_stat PIC 9(2).
        77 fmobi_stat PIC 9(2). 
        77 Wrep PIC 9.
        77 Wfin PIC 9.
        77 Wprenom PIC X(30).
        77 Wnom PIC X(30).
        77 Wtrouve PIC 9.

PROCEDURE DIVISION.

        OPEN EXTEND felecteurs
        IF felec_stat =35 THEN
          OPEN OUTPUT felecteurs
        END-IF
        CLOSE felecteurs

        OPEN EXTEND fmobilite
        IF fmobi_stat =35 THEN
          OPEN OUTPUT fmobilite
        END-IF
        CLOSE fmobilite
        
        PERFORM AJOUT_ELECTEUR
        PERFORM PLAN_MOBILITE
        PERFORM RECHERCHE_ELECTEUR
        STOP RUN.

        RECHERCHE_ELECTEUR.
        OPEN INPUT felecteurs
        MOVE 0 TO Wfin
        DISPLAY 'Nom'
        ACCEPT Wnom
        DISPLAY 'Prenom'
        ACCEPT Wprenom
        PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 OR Wfin = 1
           READ felecteurs
           AT END MOVE 1 TO Wfin
              DISPLAY 'Electeur inexistant'
           NOT AT END
              IF fe_prenom = Wprenom AND fe_nom = Wnom THEN
                 MOVE 1 TO Wtrouve
                 DISPLAY 'Ville :', fe_villeHabitation
                 DISPLAY 'Num Secu :', fe_id
              END-IF
           END-READ
        END-PERFORM
        CLOSE felecteurs.

        PLAN_MOBILITE.
        OPEN INPUT felecteurs
        OPEN EXTEND fmobilite
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ felecteurs
           AT END MOVE 1 TO Wfin
              DISPLAY 'Termine'
           NOT AT END
              IF fe_age >= 80 THEN
                 MOVE fe_id TO fm_id
                 MOVE fe_nom TO fm_nom
                 MOVE fe_prenom TO fm_prenom
                 MOVE fe_age TO fm_age
                 MOVE fe_villeHabitation TO fm_villeHabitation
                 WRITE mobiTampon END-WRITE
              END-IF
           END-READ
        END-PERFORM
        CLOSE felecteurs
        CLOSE fmobilite.
 
        AJOUT_ELECTEUR.
        OPEN EXTEND felecteurs
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY 'Donnez les informations electeur'
          DISPLAY 'Numero de securite sociale'
          ACCEPT fe_id
          DISPLAY 'Nom'
          ACCEPT fe_nom
          DISPLAY 'Prenom'
          ACCEPT fe_prenom
          DISPLAY 'Age'
          ACCEPT fe_age
          DISPLAY 'Ville'
          ACCEPT fe_villeHabitation
          WRITE elecTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
             ACCEPT Wrep
          END-PERFORM
        END-PERFORM 
        CLOSE felecteurs.
