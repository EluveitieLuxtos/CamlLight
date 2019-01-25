(*Ratios dans factorio*)

#open "graphics";;
#open "random";;
#open "printf";;

type objet =
   {nom: string;
      machine: int; (*rang dans le tableau machines*)
      durée: int * int} (*fraction a/b représentée par (a,b)*)
;;
type graphe_matrice == int vect vect;;
type graphe_liste == int list vect;;

let width = 512;;
let height = 512;;

let raff_normal = false
and raff_avance = true
and raff_liquefaction = false
and vide_petrole_brut = false
and vide_petrole_lourd = false
and vide_petrole_leger = false
and vide_lubrifiant = false
and vide_gaz = false
and vide_acide = false
and vide_eau = false
;;


let machines =
   [|1, 1; (* 0 : tuyau*)
      1, 1; (*1 : pompe*)
      1, 1; (*2 : chaudière*)
      1, 1; (*3 : turbine à vapeur*)
      1, 1; (*4 : turbine de réacteur*)
      1, 1; (*5 : réacteur nucléaire*)
      1, 1; (*6 : échangeur thermique*)
      1, 1; (*7 : foreuse thermique*)
      1, 1; (*8 : foreuse électrique*)
      1, 1; (*9 : pompe cotière*)
      1, 1; (*10: chevalet de pompage*)
      2, 1; (*11: four*)
      5, 4; (*12: machine d'assemblage*)
      1, 1; (*13: raffinerie*)
      5, 4; (*14: usine chimique*)
      3, 4; (*15: centrifugeuse*)
      1, 1; (*16: laboratoire*)
      1, 1|] (*17: silo à fusées*)
;;

let fluides =
   [|1000, 1; (*tuyau*)
      200, 1; (*pompe*)
      1, 1; (*chaudière*)
      1, 1; (*turbine à vapeur*)
      1, 1; (*turbine de réacteur*)
      1, 1; (*échangeur thermique*)
      1, 1; (*foreuse électrique*)
      1, 1; (*pompe cotière*)
      1, 1; (*chevalet de pompage*)
      1, 1; (*machine d'assemblage*)
      1, 1; (*raffinerie*)
      1, 1|] (*usine chimique*)
;;

let energie =
   [|1; (*pompe*)
      1; (*chaudière*)
      1; (*turbine à vapeur*)
      1; (*turbine de réacteur*)
      1; (*panneau solaire*)
      1; (*accumulateur*)
      1; (*réacteur nucléaire*)
      1; (*échangeur thermique*)
      1; (*conduite de chaleur*)
      1; (*foreuse thermique*)
      1; (*foreuse électrique*)
      1; (*pompe cotière*)
      1; (*chevalet de pompage*)
      1; (*four*)
      1; (*machine d'assemblage*)
      1; (*raffinerie*)
      1; (*usine chimique*)
      1; (*centrifugeuse*)
      1; (*laboratoire*)
      1; (*diffuseur de modules*)
      1; (*tourelle laser*)
      1; (*radar*)
      1|] (*silo à fusées*)
;;

let objets =
   [|{nom = "coffre en bois"; machine = 12;
         durée = 1, 2};
      {nom = "coffre en fer"; machine = 12;
         durée = 1, 2};
      {nom = "coffre en acier"; machine = 12;
         durée = 1, 2};
      {nom = "reservoir"; machine = 12;
         durée = 3, 1};
      {nom = "convoyeur"; machine = 12;
         durée = 1, 4};
      {nom = "convoyeur rapide"; machine = 12;
         durée = 1, 2};
      {nom = "convoyeur express"; machine = 12;
         durée = 1, 2};
      {nom = "convoyeur souterrain"; machine = 12;
         durée = 1, 2};
      {nom = "convoyeur souterrain rapide"; machine = 12;
         durée = 1, 4};
      {nom = "convoyeur souterrain express"; machine = 12;
         durée = 1, 4};
      {nom = "repartiteur"; machine = 12;
         durée = 1, 1};
      {nom = "repartiteur rapide"; machine = 12;
         durée = 2, 1};
      {nom = "repartiteur express"; machine = 12;
         durée = 2, 1};
      {nom = "bras robotise thermique"; machine = 12;
         durée = 1, 2};
      {nom = "bras robotise"; machine = 12;
         durée = 1, 2};
      {nom = "bras robotise long"; machine = 12;
         durée = 1, 2};
      {nom = "bras robotise rapide"; machine = 12;
         durée = 1, 2};
      {nom = "bras robotise filtrable"; machine = 12;
         durée = 1, 2};
      {nom = "bras robotise haute capacite"; machine = 12;
         durée = 1, 2};
      {nom = "bras robotise haute capacite filtrable"; machine = 12;
         durée = 1, 2};
      {nom = "petit poteau electrique"; machine = 12;
         durée = 1, 4};
      {nom = "poteau electrique"; machine = 12;
         durée = 1, 2};
      {nom = "grand poteau electrique"; machine = 12;
         durée = 1, 2};
      {nom = "poste electrique"; machine = 12;
         durée = 1, 2};
      {nom = "tuyau"; machine = 12;
         durée = 1, 2};
      {nom = "tuyau souterrain"; machine = 12;
         durée = 1, 4};
      {nom = "pompe"; machine = 12;
         durée = 2, 1};
      {nom = "rail"; machine = 12;
         durée = 1, 4};
      {nom = "arret de train"; machine = 12;
         durée = 1, 2};
      {nom = "signal ferroviaire"; machine = 12;
         durée = 1, 2};
      {nom = "signal ferroviaire chaine"; machine = 12;
         durée = 1, 2};
      {nom = "locomotive"; machine = 12;
         durée = 1, 2};
      {nom = "wagon de marchandises"; machine = 12;
         durée = 1, 2};
      {nom = "wagon-citerne"; machine = 12;
         durée = 3, 2};
      {nom = "voiture"; machine = 12;
         durée = 1, 2};
      {nom = "tank"; machine = 12;
         durée = 1, 2};
      {nom = "robot logistique"; machine = 12;
         durée = 1, 2};
      {nom = "robot de construction"; machine = 12;
         durée = 1, 2};
      {nom = "coffre d'approvisionnement logistique actif"; machine = 12;
         durée = 1, 2};
      {nom = "coffre d'approvisionnement logistique passif"; machine = 12;
         durée = 1, 2};
      {nom = "coffre de demandes logistique"; machine = 12;
         durée = 1, 2};
      {nom = "coffre de stockage logistique"; machine = 12;
         durée = 1, 2};
      {nom = "roboport"; machine = 12;
         durée = 15, 1};
      {nom = "lampe"; machine = 12;
         durée = 1, 2};
      {nom = "cable rouge"; machine = 12;
         durée = 1, 2};
      {nom = "cable vert"; machine = 12;
         durée = 1, 2};
      {nom = "calculateur"; machine = 12;
         durée = 1, 2};
      {nom = "comparateur"; machine = 12;
         durée = 1, 2};
      {nom = "emetteur de constante"; machine = 12;
         durée = 1, 2};
      {nom = "commutateur d'alimentation electrique"; machine = 12;
         durée = 2, 1};
      {nom = "haut-parleur programmable"; machine = 12;
         durée = 2, 1};
      {nom = "brique en pierre"; machine = 11;
         durée = 7, 2};
      {nom = "beton"; machine = 12;
         durée = 1, 1};
      {nom = "zone de danger"; machine = 12;
         durée = 1, 40};
      {nom = "remblai"; machine = 12;
         durée = 1, 2};

      {nom = "hache en fer"; machine = 12;
         durée = 1, 2};
      {nom = "hache en acier"; machine = 12;
         durée = 1, 2};
      {nom = "kit de reparation"; machine = 12;
         durée = 1, 2};
      {nom = "chaudiere"; machine = 12;
         durée = 1, 2};
      {nom = "turbine à vapeur"; machine = 12;
         durée = 1, 2};
      {nom = "turbine de reacteur"; machine = 12;
         durée = 1, 2};
      {nom = "panneau solaire"; machine = 12;
         durée = 10, 1};
      {nom = "accumulateur"; machine = 12;
         durée = 10, 1};
      {nom = "reacteur nucleaire"; machine = 12;
         durée = 4, 1};
      {nom = "echangeur de chaleur"; machine = 12;
         durée = 1, 2};
      {nom = "conduite de chaleur"; machine = 12;
         durée = 1, 2};
      {nom = "foreuse thermique"; machine = 12;
         durée = 2, 1};
      {nom = "foreuse électrique"; machine = 12;
         durée = 2, 1};
      {nom = "pompe cotiere"; machine = 12;
         durée = 1, 2};
      {nom = "chevalet de pompage"; machine = 12;
         durée = 10, 1};
      {nom = "four en pierre"; machine = 12;
         durée = 1, 2};
      {nom = "four en acier"; machine = 12;
         durée = 3, 1};
      {nom = "four electrique"; machine = 12;
         durée = 5, 1};
      {nom = "machine d'assemblage"; machine = 12;
         durée = 1, 2};
      {nom = "machine d'assemblage rapide"; machine = 12;
         durée = 1, 2};
      {nom = "machine d'assemblage tres rapide"; machine = 12;
         durée = 1, 2};
      {nom = "raffinerie"; machine = 12;
         durée = 20, 1};
      {nom = "usine de produits chimiques"; machine = 12;
         durée = 10, 1};
      {nom = "centrifugeuse"; machine = 12;
         durée = 4, 1};
      {nom = "laboratoire"; machine = 12;
         durée = 5, 1};
      {nom = "diffuseur de modules"; machine = 12;
         durée = 15, 1};
      {nom = "module de vitesse"; machine = 12;
         durée = 15, 1};
      {nom = "module de vitesse 2"; machine = 12;
         durée = 30, 1};
      {nom = "module de vitesse 3"; machine = 12;
         durée = 60, 1};
      {nom = "module d'efficacite"; machine = 12;
         durée = 15, 1};
      {nom = "module d'efficacite 2"; machine = 12;
         durée = 30, 1};
      {nom = "module d'efficacite 3"; machine = 12;
         durée = 60, 1};
      {nom = "module de productivite"; machine = 12;
         durée = 15, 1};
      {nom = "module de productivite 2"; machine = 12;
         durée = 30, 1};
      {nom = "module de productivite 3"; machine = 12;
         durée = 60, 1};

      {nom = "petrole brut"; machine = 0;
         durée = 1, 1};
      {nom = "petrole lourd"; machine = 0;
         durée = 1, 1};
      {nom = "petrole leger"; machine = 0;
         durée = 1, 1};
      {nom = "lubrifiant"; machine = 0;
         durée = 1, 1};
      {nom = "gaz petrolifere"; machine = 0;
         durée = 1, 1};
      {nom = "acide sulfurique"; machine = 0;
         durée = 1, 1};
      {nom = "eau"; machine = 0;
         durée = 1, 1};
      {nom = "usine d'acide"; machine = 14;
         durée = 1, 50};
      {nom = "raffinage"; machine = 13;
         durée = 5, 1};
      {nom = "raffinage avance"; machine = 13;
         durée = 5, 1};
      {nom = "liquefaction"; machine = 13;
         durée = 5, 1};
      {nom = "craquage lourd leger"; machine = 13;
         durée = 5, 1};
      {nom = "craquage leger gaz"; machine = 13;
         durée = 5, 1};
      {nom = "combustible"; machine = 0;
         durée = 1, 1};
      {nom = "combustible petrole leger"; machine = 14;
         durée = 3, 1};
      {nom = "combustible gaz petrolifere"; machine = 14;
         durée = 3, 1};
      {nom = "combustible petrole lourd"; machine = 14;
         durée = 3, 1};
      {nom = "usine de lubrifiant"; machine = 14;
         durée = 1, 10};
      {nom = "minerais pierre"; machine = 8;
         durée = 1, 1};
      {nom = "minerais fer"; machine = 8;
         durée = 1, 1};
      {nom = "minerais cuivre"; machine = 8;
         durée = 1, 1};
      {nom = "minerais uranium"; machine = 8; durée = 1, 1};
      {nom = "bois brut"; machine = 17; durée = 1, 1};
      {nom = "bois"; machine = 12;
         durée = 1, 4};
      {nom = "minerais charbon"; machine = 8;
         durée = 1, 1};
      {nom = "plaque de fer"; machine = 11;
         durée = 7, 2};
      {nom = "plaque de cuivre"; machine = 11;
         durée = 7, 2};
      {nom = "plaque d'acier"; machine = 11;
         durée = 35, 2};
      {nom = "soufre"; machine = 14;
         durée = 1, 2};
      {nom = "barre de plastique"; machine = 14;
         durée = 1, 2};
      {nom = "enrichissement de l'uranium"; machine = 15;
         durée = 10, 1};
      {nom = "uranium 235"; machine = 0;
         durée = 1, 1};
      {nom = "uranium 238"; machine = 0;
         durée = 1, 1};
      {nom = "baril de petrole brut"; machine = 12;
         durée = 1, 1};
      {nom = "baril de petrole lourd"; machine = 12;
         durée = 1, 1};
      {nom = "baril de petrole leger"; machine = 12;
         durée = 1, 1};
      {nom = "baril de lubrifiant"; machine = 12;
         durée = 1, 1};
      {nom = "baril de gaz petrolifere"; machine = 12;
         durée = 1, 1};
      {nom = "baril d'acide sulfurique"; machine = 12;
         durée = 1, 1};
      {nom = "baril d'eau"; machine = 12;
         durée = 1, 1};
      {nom = "baril vide de petrole brut"; machine = 12;
         durée = 1, 250};
      {nom = "baril vide de petrole lourd"; machine = 12;
         durée = 1, 250};
      {nom = "baril vide de petrole leger"; machine = 12;
         durée = 1, 250};
      {nom = "baril vide de lubrifiant"; machine = 12;
         durée = 1, 250};
      {nom = "baril vide de gaz petrolifere"; machine = 12;
         durée = 1, 250};
      {nom = "baril vide d'acide sulfurique"; machine = 12;
         durée = 1, 250};
      {nom = "baril vide d'eau"; machine = 12;
         durée = 1, 250};
      {nom = "cable en cuivre"; machine = 12;
         durée = 1, 4};
      {nom = "barre de fer"; machine = 12;
         durée = 1, 4};
      {nom = "engrenage"; machine = 12;
         durée = 1, 2};
      {nom = "baril vide"; machine = 12;
         durée = 1, 1};
      {nom = "circuit electronique"; machine = 12;
         durée = 1, 2};
      {nom = "circuit electronique avance"; machine = 12;
         durée = 6, 1};
      {nom = "processeur"; machine = 12;
         durée = 10, 1};
      {nom = "element de moteur"; machine = 12;
         durée = 10, 1};
      {nom = "element de moteur electrique"; machine = 12;
         durée = 10, 1};
      {nom = "batterie"; machine = 14;
         durée = 5, 1};
      {nom = "explosif"; machine = 14;
         durée = 5, 1};
      {nom = "chassis de robot volant"; machine = 12;
         durée = 20, 1};
      {nom = "structure de faible densite"; machine = 12;
         durée = 30, 1};
      {nom = "carburant pour fusee"; machine = 12;
         durée = 30, 1};
      {nom = "unite de controle de la fusee"; machine = 12;
         durée = 30, 1};
      {nom = "satellite"; machine = 12;
         durée = 3, 1};
      {nom = "barre d'uranium"; machine = 12;
         durée = 1, 50};
      {nom = "retraitement de dechets"; machine = 15;
         durée = 1, 50};
      {nom = "procede Kovarex"; machine = 15;
         durée = 1, 50};
      {nom = "fourniture de recherche 1"; machine = 12;
         durée = 5, 1};
      {nom = "fourniture de recherche 2"; machine = 12;
         durée = 6, 1};
      {nom = "fourniture de recherche 3"; machine = 12;
         durée = 12, 1};
      {nom = "pack de science militaire"; machine = 12;
         durée = 5, 1};
      {nom = "pack de science de production"; machine = 12;
         durée = 7, 1};
      {nom = "pack de science high-tech"; machine = 12;
         durée = 7, 1};
      {nom = "fusee"; machine = 17;
         durée = 1, 1};

      {nom = "pistolet"; machine = 12;
         durée = 5, 1};
      {nom = "fusil d'assaut"; machine = 12;
         durée = 10, 1};
      {nom = "fusil a pompe"; machine = 12;
         durée = 10, 1};
      {nom = "fusil a pompe de combat"; machine = 12;
         durée = 10, 1};
      {nom = "lance-roquette"; machine = 12;
         durée = 10, 1};
      {nom = "lance-flammes"; machine = 12;
         durée = 10, 1};
      {nom = "mines"; machine = 12;
         durée = 5, 4};
      {nom = "chargeur"; machine = 12;
         durée = 1, 1};
      {nom = "chargeur de munitions perforantes"; machine = 12;
         durée = 3, 1};
      {nom = "chargeur de munitions a l'uranium"; machine = 12;
         durée = 10, 1};
      {nom = "cartouches"; machine = 12;
         durée = 3, 1};
      {nom = "cartouches perforantes"; machine = 12;
         durée = 8, 1};
      {nom = "obus"; machine = 12;
         durée = 8, 1};
      {nom = "obus explosif"; machine = 12;
         durée = 8, 1};
      {nom = "obus d'uranium"; machine = 12;
         durée = 12, 1};
      {nom = "obus explosif d'uranium"; machine = 12;
         durée = 12, 1};
      {nom = "missile"; machine = 12;
         durée = 8, 1};
      {nom = "roquette explosive"; machine = 12;
         durée = 8, 1};
      {nom = "bombe atomique"; machine = 12;
         durée = 50, 1};
      {nom = "munitions pour lance-flammes"; machine = 14;
         durée = 6, 1};
      {nom = "grenade"; machine = 12;
         durée = 8, 1};
      {nom = "grenade a fragmentation"; machine = 12;
         durée = 8, 1};
      {nom = "capsule de poison"; machine = 12;
         durée = 8, 1};
      {nom = "capsule de ralentissement"; machine = 12;
         durée = 8, 1};
      {nom = "capsule de robot mitrailleur"; machine = 12;
         durée = 8, 1};
      {nom = "capsule de robot distracteur"; machine = 12;
         durée = 15, 1};
      {nom = "capsule de robot destructeur"; machine = 12;
         durée = 15, 1};
      {nom = "commande de la decharge electrique"; machine = 12;
         durée = 1, 2};
      {nom = "armure legere"; machine = 12;
         durée = 3, 1};
      {nom = "armure lourde"; machine = 12;
         durée = 8, 1};
      {nom = "armure modulaire"; machine = 12;
         durée = 15, 1};
      {nom = "armure de puissance"; machine = 12;
         durée = 20, 1};
      {nom = "armure de puissance MK2"; machine = 12;
         durée = 25, 1};
      {nom = "panneau solaire portatif"; machine = 12;
         durée = 10, 1};
      {nom = "reacteur à fusion portatif"; machine = 12;
         durée = 10, 1};
      {nom = "bouclier d'energie"; machine = 12;
         durée = 10, 1};
      {nom = "bouclier d'energie MK2"; machine = 12;
         durée = 10, 1};
      {nom = "batterie"; machine = 12;
         durée = 10, 1};
      {nom = "batterie MK2"; machine = 12;
         durée = 10, 1};
      {nom = "laser de defense personnel"; machine = 12;
         durée = 10, 1};
      {nom = "decharge electrique de defense"; machine = 12;
         durée = 10, 1};
      {nom = "exosquelette"; machine = 12;
         durée = 10, 1};
      {nom = "roboport personnel"; machine = 12;
         durée = 10, 1};
      {nom = "roboport personnel MK2"; machine = 12;
         durée = 20, 1};
      {nom = "vision de nuit"; machine = 12;
         durée = 10, 1};
      {nom = "mur de pierre"; machine = 12;
         durée = 1, 2};
      {nom = "porte"; machine = 12;
         durée = 1, 2};
      {nom = "tourelle mitrailleuse"; machine = 12;
         durée = 8, 1};
      {nom = "tourelle laser"; machine = 12;
         durée = 20, 1};
      {nom = "tourelle lance-flammes"; machine = 12;
         durée = 20, 1};
      {nom = "radar"; machine = 12;
         durée = 1, 2};
      {nom = "silo a fusees"; machine = 12;
         durée = 30, 1};

      {nom = "recherche productivite miniere"; machine = 16; durée = 60, 1};
      {nom = "recherche vitesse des robots"; machine = 16; durée = 60, 1};
      {nom = "recherche degats des roquettes"; machine = 16; durée = 60, 1};
      {nom = "recherche dommage des obus"; machine = 16; durée = 60, 1};
      {nom = "recherche degats des balles"; machine = 16; durée = 60, 1};
      {nom = "recherche degats des grenades"; machine = 16; durée = 45, 1};
      {nom = "recherche degats des cartouches"; machine = 16; durée = 60, 1};
      {nom = "recherche degats des tourelles laser"; machine = 16; durée = 60, 1};
      {nom = "recherche degats des mitrailleuses"; machine = 16; durée = 60, 1};
      {nom = "recherche degats des lance-flammes"; machine = 16; durée = 60, 1};
      {nom = "recherche nb max de robots de combat"; machine = 16; durée = 30, 1};
      {nom = "recherche degats des robots de combat"; machine = 16; durée = 30, 1}|]
;;
let n = vect_length objets;;
let matrix = make_matrix n n (0,1);;

(*coffres*)
matrix.(0).(112) <- 4, 1;
matrix.(1).(115) <- 8, 1;
matrix.(2).(117) <- 8, 1;
matrix.(3).(117) <- 20, 1;
matrix.(3).(119) <- 5, 1;
(*convoyeurs*)
matrix.(4).(115) <- 1, 2;
matrix.(4).(139) <- 1, 2;
matrix.(5).(139) <- 5, 1;
matrix.(5).(4) <- 1, 1;
matrix.(6).(139) <- 10, 1;
matrix.(6).(93) <- 20, 1;
matrix.(6).(5) <- 1, 1;
matrix.(7).(115) <- 10, 2;
matrix.(7).(4) <- 5, 2;
matrix.(8).(139) <- 20, 2;
matrix.(8).(7) <- 2, 2;
matrix.(9).(139) <- 40, 2;
matrix.(9).(93) <- 40, 2;
matrix.(9).(8) <- 2, 2;
matrix.(10).(141) <- 5, 1;
matrix.(10).(115) <- 5, 1;
matrix.(10).(4) <- 4, 1;
matrix.(11).(139) <- 10, 1;
matrix.(11).(141) <- 10, 1;
matrix.(11).(10) <- 1, 1;
matrix.(12).(139) <- 10, 1;
matrix.(12).(142) <- 10, 1;
matrix.(12).(93) <- 80, 1;
matrix.(12).(11) <- 1, 1;
(*bras robotisés*)
matrix.(13).(115) <- 1, 1;
matrix.(13).(139) <- 1, 1;
matrix.(14).(115) <- 1, 1;
matrix.(14).(139) <- 1, 1;
matrix.(14).(141) <- 1, 1;
matrix.(15).(115) <- 1, 1;
matrix.(15).(139) <- 1, 1;
matrix.(15).(14) <- 1, 1;
matrix.(16).(115) <- 2, 1;
matrix.(16).(141) <- 2, 1;
matrix.(16).(14) <- 1, 1;
matrix.(17).(141) <- 4, 1;
matrix.(17).(16) <- 1, 1;
matrix.(18).(139) <- 15, 1;
matrix.(18).(141) <- 15, 1;
matrix.(18).(142) <- 1, 1;
matrix.(18).(16) <- 1, 1;
matrix.(19).(141) <- 5, 1;
matrix.(19).(18) <- 1, 1;
(*poteaux et tuyaux*)
matrix.(20).(113) <- 2, 2;
matrix.(20).(137) <- 2, 2;
matrix.(21).(116) <- 2, 1;
matrix.(21).(117) <- 2, 1;
matrix.(22).(116) <- 5, 1;
matrix.(22).(117) <- 5, 1;
matrix.(23).(117) <- 10, 1;
matrix.(23).(116) <- 5, 1;
matrix.(23).(142) <- 5, 1;
matrix.(24).(115) <- 1, 1;
matrix.(25).(24) <- 10, 2;
matrix.(25).(115) <- 5, 2;
matrix.(26).(117) <- 1, 1;
matrix.(26).(144) <- 1, 1;
matrix.(26).(24) <- 1, 1;
(*trains*)
matrix.(27).(108) <- 1, 2;
matrix.(27).(138) <- 1, 2;
matrix.(27).(117) <- 1, 2;
matrix.(28).(115) <- 10, 1;
matrix.(28).(117) <- 3, 1;
matrix.(28).(141) <- 5, 1;
matrix.(29).(141) <- 1, 1;
matrix.(29).(115) <- 5, 1;
matrix.(30).(141) <- 1, 1;
matrix.(30).(115) <- 5, 1;
matrix.(31).(141) <- 10, 1;
matrix.(31).(117) <- 30, 1;
matrix.(31).(144) <- 20, 1;
matrix.(32).(115) <- 20, 1;
matrix.(32).(117) <- 20, 1;
matrix.(32).(139) <- 10, 1;
matrix.(33).(139) <- 10, 1;
matrix.(33).(117) <- 16, 1;
matrix.(33).(24) <- 8, 1;
matrix.(33).(3) <- 3, 1;
matrix.(34).(144) <- 8, 1;
matrix.(34).(115) <- 20, 1;
matrix.(34).(117) <- 5, 1;
matrix.(35).(144) <- 32, 1;
matrix.(35).(117) <- 50, 1;
matrix.(35).(139) <- 15, 1;
matrix.(35).(142) <- 10, 1;
(*logistique*)
matrix.(36).(148) <- 1, 1;
matrix.(36).(142) <- 2, 1;
matrix.(37).(148) <- 1, 1;
matrix.(37).(141) <- 2, 1;
matrix.(38).(2) <- 1, 1;
matrix.(38).(141) <- 3, 1;
matrix.(38).(142) <- 1, 1;
matrix.(39).(2) <- 1, 1;
matrix.(39).(141) <- 3, 1;
matrix.(39).(142) <- 1, 1;
matrix.(40).(2) <- 1, 1;
matrix.(40).(141) <- 3, 1;
matrix.(40).(142) <- 1, 1;
matrix.(41).(2) <- 1, 1;
matrix.(41).(141) <- 3, 1;
matrix.(41).(142) <- 1, 1;
matrix.(42).(117) <- 45, 1;
matrix.(42).(139) <- 45, 1;
matrix.(42).(142) <- 45, 1;
(*logique*)
matrix.(43).(141) <- 1, 1;
matrix.(43).(138) <- 3, 1;
matrix.(43).(115) <- 1, 1;
matrix.(44).(141) <- 1, 1;
matrix.(44).(137) <- 1, 1;
matrix.(45).(141) <- 1, 1;
matrix.(45).(137) <- 1, 1;
matrix.(46).(141) <- 5, 1;
matrix.(46).(137) <- 5, 1;
matrix.(47).(141) <- 5, 1;
matrix.(47).(137) <- 5, 1;
matrix.(48).(141) <- 2, 1;
matrix.(48).(137) <- 5, 1;
matrix.(49).(137) <- 5, 1;
matrix.(49).(141) <- 2, 1;
matrix.(49).(115) <- 5, 1;
matrix.(50).(115) <- 5, 1;
matrix.(50).(141) <- 4, 1;
matrix.(50).(137) <- 5, 1;
(*sols*)
matrix.(51).(108) <- 2, 1;
matrix.(52).(51) <- 5, 10;
matrix.(52).(109) <- 1, 10;
matrix.(52).(96) <- 100, 10;
matrix.(53).(52) <- 10, 1;
matrix.(54).(108) <- 20, 1;



(*haches et kits*)
matrix.(55).(115) <- 3, 1;
matrix.(55).(138) <- 2, 1;
matrix.(56).(117) <- 5, 1;
matrix.(56).(138) <- 2, 1;
matrix.(57).(139) <- 2, 1;
matrix.(57).(141) <- 2, 1;
(*énergie*)
matrix.(58).(70) <- 1, 1;
matrix.(58).(24) <- 4, 1;
matrix.(59).(139) <- 8, 1;
matrix.(59).(24) <- 5, 1;
matrix.(59).(115) <- 10, 1;
matrix.(60).(139) <- 50, 1;
matrix.(60).(116) <- 50, 1;
matrix.(60).(24) <- 20, 1;
matrix.(61).(117) <- 5, 1;
matrix.(61).(141) <- 15, 1;
matrix.(61).(116) <- 5, 1;
matrix.(62).(115) <- 2, 1;
matrix.(62).(146) <- 5, 1;
matrix.(63).(52) <- 500, 1;
matrix.(63).(117) <- 500, 1;
matrix.(63).(142) <- 500, 1;
matrix.(63).(116) <- 500, 1;
matrix.(64).(117) <- 10, 1;
matrix.(64).(116) <- 100, 1;
matrix.(64).(24) <- 10, 1;
matrix.(65).(117) <- 10, 1;
matrix.(65).(116) <- 20, 1;
(*forage*)
matrix.(66).(139) <- 3, 1;
matrix.(66).(70) <- 1, 1;
matrix.(66).(115) <- 3, 1;
matrix.(67).(141) <- 3, 1;
matrix.(67).(139) <- 5, 1;
matrix.(67).(115) <- 10, 1;
matrix.(68).(141) <- 2, 1;
matrix.(68).(24) <- 1, 1;
matrix.(68).(139) <- 1, 1;
matrix.(69).(117) <- 5, 1;
matrix.(69).(139) <- 10, 1;
matrix.(69).(141) <- 5, 1;
matrix.(69).(24) <- 10, 1;
(*fours*)
matrix.(70).(108) <- 5, 1;
matrix.(71).(117) <- 6, 1;
matrix.(71).(51) <- 10, 1;
matrix.(72).(117) <- 10, 1;
matrix.(72).(142) <- 5, 1;
matrix.(72).(51) <- 10, 1;
(*production*)
matrix.(73).(141) <- 3, 1;
matrix.(73).(139) <- 5, 1;
matrix.(73).(115) <- 9, 1;
matrix.(74).(115) <- 9, 1;
matrix.(74).(141) <- 3, 1;
matrix.(74).(139) <- 5, 1;
matrix.(74).(73) <- 1, 1;
matrix.(75).(81) <- 4, 1;
matrix.(75).(74) <- 2, 1;
matrix.(76).(117) <- 15, 1;
matrix.(76).(139) <- 10, 1;
matrix.(76).(51) <- 10, 1;
matrix.(76).(141) <- 10, 1;
matrix.(76).(24) <- 10, 1;
matrix.(77).(117) <- 5, 1;
matrix.(77).(139) <- 5, 1;
matrix.(77).(141) <- 5, 1;
matrix.(77).(24) <- 5, 1;
matrix.(78).(52) <- 100, 1;
matrix.(78).(117) <- 50, 1;
matrix.(78).(142) <- 100, 1;
matrix.(78).(139) <- 100, 1;
matrix.(79).(141) <- 10, 1;
matrix.(79).(139) <- 10, 1;
matrix.(79).(4) <- 4, 1;
(*modules*)
matrix.(80).(141) <- 20, 1;
matrix.(80).(142) <- 20, 1;
matrix.(80).(117) <- 10, 1;
matrix.(80).(137) <- 10, 1;
matrix.(81).(142) <- 5, 1;
matrix.(81).(141) <- 5, 1;
matrix.(82).(81) <- 4, 1;
matrix.(82).(142) <- 5, 1;
matrix.(82).(143) <- 5, 1;
matrix.(83).(82) <- 4, 1;
matrix.(83).(142) <- 5, 1;
matrix.(83).(143) <- 5, 1;
matrix.(84).(141) <- 5, 1;
matrix.(84).(142) <- 5, 1;
matrix.(85).(84) <- 4, 1;
matrix.(85).(142) <- 5, 1;
matrix.(85).(143) <- 5, 1;
matrix.(86).(85) <- 5, 1;
matrix.(86).(142) <- 5, 1;
matrix.(86).(143) <- 5, 1;
matrix.(87).(141) <- 5, 1;
matrix.(87).(142) <- 5, 1;
matrix.(88).(87) <- 4, 1;
matrix.(88).(142) <- 5, 1;
matrix.(88).(143) <- 5, 1;
matrix.(89).(88) <- 5, 1;
matrix.(89).(142) <- 5, 1;
matrix.(89).(143) <- 5, 1;




(*matériaux liquides*)
matrix.(97).(118) <- 5, 50;
matrix.(97).(115) <- 1, 50;
matrix.(97).(96) <- 100, 50;
matrix.(98).(90) <- 100, 1;
matrix.(99).(96) <- 50, 1;
matrix.(99).(90) <- 100, 1;
matrix.(100).(114) <- 10, 1;
matrix.(100).(91) <- 25, 1;
matrix.(100).(96) <- 50, 1;
matrix.(101).(96) <- 30, 1;
matrix.(101).(91) <- 40, 1;
matrix.(102).(96) <- 30, 1;
matrix.(102).(92) <- 30, 1;
matrix.(104).(92) <- 10, 1;
matrix.(105).(94) <- 20, 1;
matrix.(106).(91) <- 20, 1;
matrix.(107).(91) <- 10, 10;
(*matériaux solides*)
matrix.(113).(112) <- 1, 2;
matrix.(115).(109) <- 1, 1;
matrix.(116).(110) <- 1, 1;
matrix.(117).(115) <- 5, 1;
matrix.(118).(96) <- 30, 2;
matrix.(118).(94) <- 30, 2;
matrix.(119).(114) <- 1, 2;
matrix.(119).(94) <- 20, 2;
matrix.(120).(111) <- 10, 1;
matrix.(121).(120) <- 7, 1000;
matrix.(122).(120) <- 993, 1000;
(*remplir les barils*)
matrix.(123).(140) <- 1, 1;
matrix.(123).(90) <- 250, 1;
matrix.(124).(140) <- 1, 1;
matrix.(124).(91) <- 250, 1;
matrix.(125).(140) <- 1, 1;
matrix.(125).(92) <- 250, 1;
matrix.(126).(140) <- 1, 1;
matrix.(126).(93) <- 250, 1;
matrix.(127).(140) <- 1, 1;
matrix.(127).(94) <- 250, 1;
matrix.(128).(140) <- 1, 1;
matrix.(128).(95) <- 250, 1;
matrix.(129).(140) <- 1, 1;
matrix.(129).(96) <- 250, 1;
(*vider les barils*)
matrix.(130).(123) <- 1, 1;
matrix.(131).(124) <- 1, 1;
matrix.(132).(125) <- 1, 1;
matrix.(133).(126) <- 1, 1;
matrix.(134).(127) <- 1, 1;
matrix.(135).(128) <- 1, 1;
matrix.(136).(129) <- 1, 1;
(*matériaux avancés*)
matrix.(137).(116) <- 1, 2;
matrix.(138).(115) <- 1, 2;
matrix.(139).(115) <- 2, 1;
matrix.(140).(117) <- 1, 1;
matrix.(141).(115) <- 1, 1;
matrix.(141).(137) <- 3, 1;
matrix.(142).(141) <- 2, 1;
matrix.(142).(119) <- 2, 1;
matrix.(142).(137) <- 4, 1;
matrix.(143).(141) <- 20, 1;
matrix.(143).(142) <- 2, 1;
matrix.(143).(95) <- 5, 1;
matrix.(144).(117) <- 1, 1;
matrix.(144).(139) <- 1, 1;
matrix.(144).(24) <- 2, 1;
matrix.(145).(144) <- 1, 1;
matrix.(145).(141) <- 2, 1;
matrix.(145).(93) <- 15, 1;
matrix.(146).(115) <- 1, 1;
matrix.(146).(116) <- 1, 1;
matrix.(146).(95) <- 20, 1;
(*matériaux de fusée*)
matrix.(147).(118) <- 1, 1;
matrix.(147).(114) <- 1, 1;
matrix.(147).(96) <- 10, 1;
matrix.(148).(145) <- 1, 1;
matrix.(148).(146) <- 2, 1;
matrix.(148).(117) <- 1, 1;
matrix.(148).(141) <- 3, 1;
matrix.(149).(117) <- 10, 1;
matrix.(149).(116) <- 5, 1;
matrix.(149).(119) <- 5, 1;
matrix.(150).(103) <- 10, 1;
matrix.(151).(143) <- 1, 1;
matrix.(151).(81) <- 1, 1;
matrix.(152).(149) <- 100, 1;
matrix.(152).(61) <- 100, 1;
matrix.(152).(62) <- 100, 1;
matrix.(152).(213) <- 5, 1;
matrix.(152).(143) <- 100, 1;
matrix.(152).(150) <- 50, 1;
(*fournitures de recherche*)
matrix.(156).(116) <- 1, 1;
matrix.(156).(139) <- 1, 1;
matrix.(157).(4) <- 1, 1;
matrix.(157).(14) <- 1, 1;
matrix.(158).(142) <- 1, 1;
matrix.(158).(144) <- 1, 1;
matrix.(158).(73) <- 1, 1;
matrix.(159).(171) <- 1, 1;
matrix.(159).(183) <- 1, 1;
matrix.(159).(210) <- 1, 1;
matrix.(160).(69) <- 1, 1;
matrix.(160).(145) <- 1, 1;
matrix.(160).(72) <- 1, 1;
matrix.(161).(146) <- 1, 1;
matrix.(161).(143) <- 3, 1;
matrix.(161).(81) <- 1, 1;
matrix.(161).(137) <- 30, 1;
matrix.(162).(149) <- 2000, 1;
matrix.(162).(150) <- 2000, 1;
matrix.(162).(151) <- 2000, 1;
matrix.(162).(152) <- 2, 1;






(*armes*)
matrix.(163).(115) <- 5, 1;
matrix.(163).(116) <- 5, 1;
matrix.(164).(115) <- 10, 1;
matrix.(164).(116) <- 5, 1;
matrix.(164).(139) <- 10, 1;
matrix.(165).(115) <- 15, 1;
matrix.(165).(139) <- 5, 1;
matrix.(165).(116) <- 10, 1;
matrix.(165).(113) <- 5, 1;
matrix.(166).(117) <- 15, 1;
matrix.(166).(139) <- 5, 1;
matrix.(166).(116) <- 10, 1;
matrix.(166).(113) <- 10, 1;
matrix.(167).(115) <- 5, 1;
matrix.(167).(139) <- 5, 1;
matrix.(167).(141) <- 5, 1;
matrix.(168).(117) <- 5, 1;
matrix.(168).(139) <- 10, 1;
matrix.(169).(117) <- 1, 4;
matrix.(169).(147) <- 2, 4;
(*munitions*)
matrix.(170).(115) <- 4, 1;
matrix.(171).(170) <- 1, 1;
matrix.(171).(117) <- 1, 1;
matrix.(171).(116) <- 5, 1;
matrix.(172).(171) <- 1, 1;
matrix.(172).(122) <- 1, 1;
matrix.(173).(115) <- 2, 1;
matrix.(173).(116) <- 2, 1;
matrix.(174).(173) <- 2, 1;
matrix.(174).(116) <- 5, 1;
matrix.(174).(117) <- 2, 1;
matrix.(175).(117) <- 2, 1;
matrix.(175).(119) <- 2, 1;
matrix.(175).(147) <- 1, 1;
matrix.(176).(117) <- 2, 1;
matrix.(176).(119) <- 2, 1;
matrix.(176).(147) <- 2, 1;
matrix.(177).(175) <- 1, 1;
matrix.(177).(122) <- 1, 1;
matrix.(178).(176) <- 1, 1;
matrix.(178).(122) <- 1, 1;
matrix.(179).(141) <- 1, 1;
matrix.(179).(147) <- 1, 1;
matrix.(179).(115) <- 2, 1;
matrix.(180).(179) <- 1, 1;
matrix.(180).(147) <- 2, 1;
matrix.(181).(143) <- 20, 1;
matrix.(181).(147) <- 10, 1;
matrix.(181).(121) <- 30, 1;
matrix.(182).(117) <- 5, 1;
matrix.(182).(91) <- 50, 1;
matrix.(182).(92) <- 50, 1;
(*jetables*)
matrix.(183).(115) <- 5, 1;
matrix.(183).(114) <- 10, 1;
matrix.(184).(183) <- 7, 1;
matrix.(184).(147) <- 5, 1;
matrix.(184).(117) <- 5, 1;
matrix.(185).(117) <- 3, 1;
matrix.(185).(141) <- 3, 1;
matrix.(185).(114) <- 10, 1;
matrix.(186).(117) <- 2, 1;
matrix.(186).(141) <- 2, 1;
matrix.(186).(114) <- 5, 1;
matrix.(187).(171) <- 1, 1;
matrix.(187).(141) <- 2, 1;
matrix.(187).(139) <- 3, 1;
matrix.(188).(187) <- 4, 1;
matrix.(188).(142) <- 3, 1;
matrix.(189).(188) <- 4, 1;
matrix.(189).(81) <- 1, 1;
matrix.(190).(141) <- 1, 1;
(*armures*)
matrix.(191).(115) <- 40, 1;
matrix.(192).(116) <- 100, 1;
matrix.(192).(117) <- 50, 1;
matrix.(193).(142) <- 30, 1;
matrix.(193).(117) <- 50, 1;
matrix.(194).(143) <- 40, 1;
matrix.(194).(145) <- 20, 1;
matrix.(194).(117) <- 40, 1;
matrix.(195).(83) <- 5, 1;
matrix.(195).(86) <- 5, 1;
matrix.(195).(143) <- 40, 1;
matrix.(195).(117) <- 40, 1;
(*équipements*)
matrix.(196).(61) <- 5, 1;
matrix.(196).(142) <- 1, 1;
matrix.(196).(117) <- 5, 1;
matrix.(197).(143) <- 250, 1;
matrix.(198).(142) <- 5, 1;
matrix.(198).(117) <- 10, 1;
matrix.(199).(198) <- 10, 1;
matrix.(199).(143) <- 10, 1;
matrix.(200).(146) <- 5, 1;
matrix.(200).(117) <- 10, 1;
matrix.(201).(200) <- 10, 1;
matrix.(201).(143) <- 20, 1;
matrix.(202).(143) <- 1, 1;
matrix.(202).(117) <- 5, 1;
matrix.(202).(211) <- 5, 1;
matrix.(203).(143) <- 5, 1;
matrix.(203).(117) <- 20, 1;
matrix.(203).(211) <- 10, 1;
matrix.(204).(143) <- 10, 1;
matrix.(204).(145) <- 30, 1;
matrix.(204).(117) <- 20, 1;
matrix.(205).(143) <- 10, 1;
matrix.(205).(139) <- 40, 1;
matrix.(205).(117) <- 20, 1;
matrix.(205).(146) <- 45, 1;
matrix.(206).(205) <- 5, 1;
matrix.(206).(143) <- 100, 1;
matrix.(207).(142) <- 5, 1;
matrix.(207).(117) <- 10, 1;
(*bâtiments avancés*)
matrix.(208).(51) <- 5, 1;
matrix.(209).(208) <- 1, 1;
matrix.(209).(117) <- 2, 1;
matrix.(209).(141) <- 2, 1;
matrix.(210).(139) <- 10, 1;
matrix.(210).(116) <- 10, 1;
matrix.(210).(115) <- 20, 1;
matrix.(211).(117) <- 20, 1;
matrix.(211).(141) <- 20, 1;
matrix.(211).(146) <- 12, 1;
matrix.(212).(117) <- 30, 1;
matrix.(212).(139) <- 15, 1;
matrix.(212).(24) <- 10, 1;
matrix.(212).(144) <- 5, 1;
matrix.(213).(141) <- 5, 1;
matrix.(213).(139) <- 5, 1;
matrix.(213).(115) <- 10, 1;
matrix.(214).(117) <- 1000, 1;
matrix.(214).(52) <- 1000, 1;
matrix.(214).(24) <- 100, 1;
matrix.(214).(143) <- 200, 1;
matrix.(214).(145) <- 200, 1;






(*recherches*)
matrix.(215).(156) <- 1000, 1;
matrix.(215).(157) <- 1000, 1;
matrix.(215).(158) <- 1000, 1;
matrix.(215).(160) <- 1000, 1;
matrix.(215).(161) <- 1000, 1;
matrix.(215).(162) <- 1, 1;
matrix.(216).(156) <- 1000, 1;
matrix.(216).(157) <- 1000, 1;
matrix.(216).(158) <- 1000, 1;
matrix.(216).(160) <- 1000, 1;
matrix.(216).(161) <- 1000, 1;
matrix.(216).(162) <- 1, 1;
matrix.(217).(156) <- 1000, 1;
matrix.(217).(157) <- 1000, 1;
matrix.(217).(158) <- 1000, 1;
matrix.(217).(159) <- 1000, 1;
matrix.(217).(161) <- 1000, 1;
matrix.(217).(162) <- 1, 1;
matrix.(218).(156) <- 1000, 1;
matrix.(218).(157) <- 1000, 1;
matrix.(218).(158) <- 1000, 1;
matrix.(218).(159) <- 1000, 1;
matrix.(218).(161) <- 1000, 1;
matrix.(218).(162) <- 1, 1;
matrix.(219).(156) <- 1000, 1;
matrix.(219).(157) <- 1000, 1;
matrix.(219).(158) <- 1000, 1;
matrix.(219).(159) <- 1000, 1;
matrix.(219).(161) <- 1000, 1;
matrix.(219).(162) <- 1, 1;
matrix.(220).(156) <- 1000, 1;
matrix.(220).(157) <- 1000, 1;
matrix.(220).(158) <- 1000, 1;
matrix.(220).(159) <- 1000, 1;
matrix.(220).(161) <- 1000, 1;
matrix.(220).(162) <- 1, 1;
matrix.(221).(156) <- 1000, 1;
matrix.(221).(157) <- 1000, 1;
matrix.(221).(158) <- 1000, 1;
matrix.(221).(159) <- 1000, 1;
matrix.(221).(161) <- 1000, 1;
matrix.(221).(162) <- 1, 1;
matrix.(222).(156) <- 1000, 1;
matrix.(222).(157) <- 1000, 1;
matrix.(222).(158) <- 1000, 1;
matrix.(222).(159) <- 1000, 1;
matrix.(222).(161) <- 1000, 1;
matrix.(222).(162) <- 1, 1;
matrix.(223).(156) <- 1000, 1;
matrix.(223).(157) <- 1000, 1;
matrix.(223).(158) <- 1000, 1;
matrix.(223).(159) <- 1000, 1;
matrix.(223).(161) <- 1000, 1;
matrix.(223).(162) <- 1, 1;
matrix.(224).(156) <- 1000, 1;
matrix.(224).(157) <- 1000, 1;
matrix.(224).(158) <- 1000, 1;
matrix.(224).(159) <- 1000, 1;
matrix.(224).(161) <- 1000, 1;
matrix.(224).(162) <- 1, 1;
matrix.(225).(156) <- 1000, 1;
matrix.(225).(157) <- 1000, 1;
matrix.(225).(158) <- 1000, 1;
matrix.(225).(159) <- 1000, 1;
matrix.(225).(160) <- 1000, 1;
matrix.(225).(161) <- 1000, 1;
matrix.(225).(162) <- 1, 1;
matrix.(226).(156) <- 2000, 1;
matrix.(226).(157) <- 2000, 1;
matrix.(226).(158) <- 1000, 1;
matrix.(226).(159) <- 1000, 1;
matrix.(226).(161) <- 1000, 1;
matrix.(226).(162) <- 1, 1;;


let mot_of_int i =
   if 0 <= i & i < n
   then objets.(i).nom
   else "";;

let int_of_mot str =
   let j = ref 0 in
      while !j < n & objets.(!j).nom <> str do
         incr (j)
      done;
      if !j = n
      then - 1
      else !j;;

let rec pgcd a b =
   let r = a mod b in
      if r = 0
      then b
      else pgcd b r
;;

let ppcm a b = a * b / pgcd a b;;

let ppcm_frac = fun
   | (a, b) (c, d) ->
      let num = ppcm (a * d) (b * c)
      and den = b * d in
         let e = pgcd num den in
            (num / e, den / e);;

let rec ppcm_fraclist = function
	|[] -> (1,1)
	|t::q -> ppcm_frac t (ppcm_fraclist q);;

let add_frac (a, b) (c, d) =
   let num = a * d + b * c
   and den = b * d in
   if num = 0
   then (0,1)
   else let e = pgcd num den in
         (num / e, den / e);;

let mult_frac (a, b) (c, d) =
   let num = a * c
   and den = b * d in
   if num = 0
   then (0,1)
   else let e = pgcd num den in
         (num / e, den / e);;

let div_frac (a, b) (c, d) =
   let num = a * d
   and den = b * c in
      if num = 0
      then (0, 1)
      else let e = pgcd num den in
            (num / e, den / e);;

let rec except_all e = function
   | [] -> []
   | (a,b) :: q when a = e -> except_all e q
   | (a,b) :: q -> (a,b) :: except_all e q;;

let recherche_voisins init list =
   (*recherche des voisins dans le graphe acyclique*)
   if init < 0 or init >= n
   then []
   else
      let autres = ref (init :: list) in
      let rec recherche_autres i n1 =
         if !autres = []
         then []
         else
         if mem i !autres
         then (
            (*autres := except i !autres;*)
               let l1 = ref [] in
               for j = 0 to n - 1 do
                  if (not raff_normal & j = 98)
                     or (not raff_avance & j = 99)
                     or (not raff_liquefaction & j = 100)
                     or (not vide_petrole_brut & j = 130)
                     or (not vide_petrole_lourd & j = 131)
                     or (not vide_petrole_leger & j = 132)
                     or (not vide_lubrifiant & j = 133)
                     or (not vide_gaz & j = 134)
                     or (not vide_acide & j = 135)
                     or (not vide_eau & j = 136)
                  then ()
                  else
                  if matrix.(i).(j) <> (0, 1)
                  then (
                        let n2 = mult_frac (mult_frac (div_frac machines.(objets.(i).machine) objets.(i).durée) (mult_frac n1 matrix.(i).(j))) (div_frac objets.(j).durée machines.(objets.(j).machine)) in
                        let l2 = recherche_autres j n2 in
                        if l2 = []
                        then l1 := (j, n2) :: !l1 (*ajoute les voisins directs*)
                        else l1 := !l1 @ l2
                     )
               done;
               if !l1 = []
               then []
               else (i, n1) :: !l1 (*ajoute les chemin à un objet demandé*)
            )
         else
            let l1 = ref [] in
            for j = 0 to n - 1 do
               if matrix.(i).(j) <> (0, 1)
               then (
                     let n2 = mult_frac (mult_frac (div_frac machines.(objets.(i).machine) objets.(i).durée) (mult_frac n1 matrix.(i).(j))) (div_frac objets.(j).durée machines.(objets.(j).machine)) in
                     let l2 = recherche_autres j n2 in
                     if l2 = []
                     then ()
                     else l1 := !l1 @ l2
                  )
            done;
            if !l1 = []
            then []
            else (i, n1) :: !l1
      in recherche_autres init (1, 1);;

let rec mots_of_ilist = function
   | [] -> []
   | (o, (a, b)) :: q -> (mot_of_int o, (a, b)) :: mots_of_ilist q;;

let proportion_machines strinit strlist =
   let tab = make_vect n (0, 1)
   and init = int_of_mot strinit
   and list = map int_of_mot strlist in
   let rec ajoute_occurences e = function
      | [] -> 0, 1
      | (a, b) :: q ->
         let frac = ajoute_occurences e q in
         if a = e
         then add_frac frac b
         else frac
   and epurer = function
      (*épure la liste des voisins*)
      | [] -> []
      | (a, b) :: q as liste -> (a, ajoute_occurences a liste) :: epurer (except_all a q)
   and ppcm_inv_fraclist = function
      (*prend en argument les voisins et calcule le ppcm*)
      | [] -> (1, 1)
      | [_, b] -> div_frac (1, 1) b
      | (_, b) :: q -> ppcm_frac (div_frac (1, 1) b) (ppcm_inv_fraclist q)
   and nombre_machines p = function
      (*prend en argument le ppcm et les voisins et calcule le nombre de machines*)
      | [] -> []
      | (a, b) :: q -> (mot_of_int a, mult_frac p b) :: nombre_machines p q
   in
   let voisins = recherche_voisins init list in
   let p = ppcm_inv_fraclist voisins in
   epurer (nombre_machines p voisins)
;;

let majore nbmac maclist =
   let ntot, dtot =
      let rec ajoute_mac = function
         | [] -> 0, 0
         | (_, n1) :: q -> let n2 = ajoute_mac q in add_frac n1 n2
      in ajoute_mac maclist
   in
   if ntot / dtot < nbmac
   then
      let rec intlist_of_fraclist = function
         | [] -> []
         | (str, (n, d)) :: q -> (str, n / d) :: intlist_of_fraclist q
      in intlist_of_fraclist maclist
   else
      let rec prop = function
         | [] -> []
         | (str, (n, d)) :: q -> (str, (n * nbmac * dtot - d * ntot) / (d * ntot) + 1) :: prop q
      in prop maclist
;;



for i=0 to vect_length objets -1 do
	print_int i;
	print_string (" "^objets.(i).nom^"\n")
done;;

proportion_machines "rail" [];;
proportion_machines "element de moteur" ["engrenage";"plaque d'acier"; "tuyau"];;
majore 40 (proportion_machines "element de moteur" ["engrenage";"plaque d'acier"; "tuyau"]);;
proportion_machines "processeur" ["circuit electronique avance";"circuit electronique";"cable en cuivre"];;
proportion_machines "barre de plastique" [];;
















let est_acyclique nul graphe =
   (*prend en argument l'élément nul et la matrice d'adjacence d'un graphe orienté et vérifie s'il est acyclique*)
   let p = vect_length graphe
   and q = vect_length graphe.(0)
   and bool = ref true in
      let marques = make_matrix p q 0 in
         let rec recherche i =
            if not !bool
            then []
            else
               let l1 = ref [] in
                  for j = 0 to q - 1 do
                     if graphe.(i).(j) <> nul
                     then
                        if marques.(i).(j) = 0
                        then (
                              marques.(i).(j) <- 1;
                              let l2 = recherche j in
                                 marques.(i).(j) <- 0;
                                 if exists (mem i) l2
                                 then bool := false;
                                 l1 := flat_map (fun l -> [i :: l]) l2 @ (!l1)
                           )
                        else bool := false
                  done;
                  !l1
         in
            let l1 = ref [] in
               for i = 0 to p - 1 do
                  l1 := recherche i
               done;
               !bool
;;

(*
mot_of_int 129;;
int_of_mot "engrenage";;
est_acyclique (0,1) matrix;;
mots_of_ilist (recherche_voisins (int_of_mot "carburant pour fusee") (map int_of_mot []));;

exists (mem 1) [[];[2]];;
flat_map (fun l -> [1 :: l]) [[0];[1];[2];[3]];;

let recherche_voisins init olist =
   (*recherche des voisins dans un graphe possiblement cyclique*)
   if init < 0 or init >= n
   then []
   else
      let voisins_directs i =
         let res = ref [] in
            for j = 0 to n - 1 do
               if matrix.(i).(j) <> 0
               then res := j :: !res
            done;
            !res
      in
         let voisins = flat_map voisins_directs (init :: olist) in
            let autres = ref (subtract olist voisins)
            and marque = make_matrix n n 0 in
               let rec recherche_autres i =
                  if mem i !autres
                  then (autres := except i !autres; i :: voisins_directs i)
                  else
                     let l1 = ref [] in
                        for j = 0 to n - 1 do
                           if matrix.(i).(j) <> 0 & marque.(i).(j) = 0
                           then (
                                 marque.(i).(j) <- 1;
                                 let l2 = recherche_autres j in
                                    if l2 = []
                                    then ()
                                    else l1 := !l1 @ l2
                              )
                        done;
                        !l1
               in
                  init :: voisins @ recherche_autres init;;
recherche_voisins (int_of_mot "batterie") [];;

let recherche_voisins init olist =
   (*recherche des voisins dans le graphe acyclique*)
   if init < 0 or init >= n
   then []
   else
      let autres = ref (init :: olist) in
         let rec recherche_autres i =
            if !autres = []
            then []
            else
            if mem i !autres
            then (
                  (*autres := except i !autres;*)
                  let l1 = ref [] in
                     for j = 0 to n - 1 do
                        if matrix.(i).(j) <> 0
                        then (
                              let l2 = recherche_autres j in
                                 if l2 = []
                                 then l1 := j::!l1 (*ajoute les voisins directs*)
                                 else l1 := !l1 @ l2
                           )
                     done;
                     if !l1 = []
                     then []
                     else i :: !l1 (*ajoute les chemin à un objet demandé*)
               )
            else
               let l1 = ref [] in
                  for j = 0 to n - 1 do
                     if matrix.(i).(j) <> 0
                     then (
                           let l2 = recherche_autres j in
                              if l2 = []
                              then ()
                              else l1 := !l1 @ l2
                        )
                  done;
                  if !l1 = []
                  then []
                  else i :: !l1
         in recherche_autres init;;
map mot_of_int (recherche_voisins (int_of_mot "recherche") (map int_of_mot []));;
*)





