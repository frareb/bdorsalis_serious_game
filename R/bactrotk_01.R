
# -----------------------------------------------------------------------------
# ESSAI N°1 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
# agricoles
# -----------------------------------------------------------------------------

setwd("./R")

# Techniques, col 1:22 = Luttes directes, col 23:45 : Luttes indirectes
#Luttes directes Col 1:10 = TEM; 11:12 = TE; 13:14 = TPT; 15:18 = IN; 19:21 = LB ; 22 = MSQ
#Luttes indirectes Col 23:26 = VP ; 27 = VNA; 28:33 = PRO;34:41 = TRAISOL;42=TLM; 43 = RP2; 44 = TRASOL; 45 : RPF

techniques <- read.csv(file = "Luttes.csv", dec = ",", header = TRUE)
str(techniques)

#Typologie vergers
typoverger <- read.csv(file = "Typologievergers.csv", dec = ",", header = TRUE) # (1) et (2)

#Pour chaque agriculteur, on lui assigne une typologie et plusieurs modalités de luttes
#Agriculteur 1
ag01 <- list(
  typoverger = "extensif", # (7) ajouter des noms de typo dans le fichier Typologievergers.csv
  techniques = c("TEM_Mal", "TEM_DDVPetME")
)

# Fonction pour avoir le score B. dorsalis en fonction des techniques choisies
# Cette fonction prend comme argument monTK, un vecteur de type texte qui 
# contient la liste des abbréviations des techniques choisies. Elle retourne
# un nombre correspondant au score (somme des Impact.Bd).
get_technique_score <- function(monTK){
  score <- sum(techniques[techniques$LutteAbr %in% monTK,]$Impact.Bd)
  return(score)
}
get_technique_score(monTK = ag01[[2]])

# je te laisse poursuivre à partir de là.


#View(ag01)
#Impact de ag001 sur Bactrocera dorsalis
Impactag01 <- sum(techniques[c(1,16),6]) # (8) on verra cela ensuite, dis moi quand le reste (1:7) est fait
Impactag01

#Agriculteur 2
ag02 <- list (techniques[c(4,22, 33, 40),], typoverger[,(2)])
View(ag02)
Impactag02 <- sum(techniques[c(4,22, 33, 40),6])
Impactag02

#Impact d'un agriculteur sur l'autre : Si l'agriculteur 2 n'a pas atteint le score minimale des bonnes pratiques (ici 6), impact négatif sur l'agriculteur 1
if(Impactag02 < 6) {
  Impactfinal <- Impactag01 - 2
  print(Impactfinal)
  print ("Impact negatif entre agriculteurs")
  }else{
  print ("Démarche vertueuse des argriculteurs")
}
