IDENTIFICATION DIVISION.
PROGRAM-ID. bureaux.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
        SELECT fbureaux ASSIGN TO "fbureaux.dat"
        ORGANIZATION indexed
        ACCESS IS dynamic
        RECORD KEY fbu_num
        ALTERNATE RECORD KEY fbu_ville WITH DUPLICATES
        FILE STATUS IS fbureaux_stat.

DATA DIVISION.
FILE SECTION.
        FD fbureaux.
        01 bureauxTampon.
          02 fbu_num PIC X(4).
          02 fbu_ville PIC X(30).
          02 fbu_heure_fermeture PIC 9(2).
          02 fbu_heure_ouverture PIC 9(2).
          02 fbu_etat PIC 9(2).

WORKING-STORAGE SECTION.
        77 fbureaux_stat PIC 9(2).
        77 Wrep PIC 9.
        77 Wnum PIC 9.


PROCEDURE DIVISION.
        
        OPEN I-O fbureaux
        IF fbureaux_stat =35 THEN
          OPEN OUTPUT fbureaux
        END-IF
        CLOSE fbureaux

        PERFORM AJOUT_BUREAU
        PERFORM MODIF_HORAIRES

        STOP RUN.

        AJOUT_BUREAU.
        OPEN I-O fbureaux
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY 'Donnez les informations du bureau'
          DISPLAY 'Numero'
          ACCEPT fbu_num
          DISPLAY 'Ville'
          ACCEPT fbu_ville
          DISPLAY 'Heure fermeture'
          ACCEPT fbu_heure_fermeture
          DISPLAY 'Heure ouverture'
          ACCEPT fbu_heure_ouverture
          DISPLAY 'etat'
          ACCEPT fbu_etat
          WRITE bureauxTampon END-WRITE
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
             ACCEPT Wrep
          END-PERFORM
        END-PERFORM 
        CLOSE fbureaux.

        MODIF_HORAIRES.

        OPEN I-O fbureaux
        DISPLAY 'Numero de bureau'
        ACCEPT Wnum
        READ fbureaux
        INTO Wnum
        DISPLAY 'Horaires ouverture :', fbu_heure_ouverture
        DISPLAY 'Horaires fermeture : ', fbu_heure_fermeture
        
        CLOSE fbureaux.

