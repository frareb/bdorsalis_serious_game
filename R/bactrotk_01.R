

#pratiques <- data.frame(
#  nom = c("a", "b", "c"),
#  description = c("this is a", "this is b", "this is c"),
#  reference = c(NA, NA, NA),
#  effet = c(-2, +2, +1)
#)
#ajouter modalités
#condition initiale : superficie, typologie (elemts fixe, variable?)

#ag00 <- list(
  
#)

#ESSAIE N°1 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes agricoles

setwd("C:/Users/etudiant/Documents/2023_AudreyNGom/github/bdorsalis_serious_game/R")
# Techniques, col 1:22 = Luttes directes, col 23:45 : Luttes indirectes
#Luttes directes Col 1:10 = TEM; 11:12 = TE; 13:14 = TPT; 15:18 = IN; 19:21 = LB ; 22 = MSQ
#Luttes indirectes Col 23:26 = VP ; 27 = VNA; 28:33 = PRO;34:41 = TRAISOL;42=TLM; 43 = RP2; 44 = TRASOL; 45 : RPF
Techniques <- read.csv("Luttes.csv", dec = ",", header = T)
str(Techniques)
View(Techniques)
summary(Techniques)

#Typologie vergers
Typoverger <- read.csv("Typologievergers.csv", dec = ",", header = T)
View(Typoverger)
#Attention, dans ce tableau, variables présentes qui peuvent changer en fonction des années

#Pour chaque agriculteur, on lui assigne une typologie et plusieurs modalités de luttes
#Agriculteur 1
ag01 <- list (Techniques[c(1,16),], Typoverger[,(4)])
View(ag01)
#Impact de ag001 sur Bactrocera dorsalis
Impactag01 <- sum(Techniques[c(1,16),6])
Impactag01

#Agriculteur 2
ag02 <- list (Techniques[c(4,22, 33, 40),], Typoverger[,(2)])
View(ag02)
Impactag02 <- sum(Techniques[c(1,16),6])
Impactag02

#Impact d'un agriculteur sur l'autre : Si l'agriculteur 2 n'a pas atteint le score minimale des bonnes pratiques (ici 5), impact négatif sur l'agriculteur 1
if(Impactag02 < 5) {
  Impactfinal <- Impactag01 - 2
  print(Impactfinal)
  print ("Impact negatif entre agriculteurs")
  }else{
  print ("Démarche vertueuse des argriculteurs")
}
