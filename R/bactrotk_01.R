
# -----------------------------------------------------------------------------
# ESSAI N°1 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
# agricoles
# -----------------------------------------------------------------------------

# setwd("C:/Users/etudiant/Documents/2023_AudreyNGom/github/bdorsalis_serious_game/R")
# (0) inutile de spécifier un chemin complet, tu peux simplement mettre un  
# chemin relatif de cette façon cela fonctionne chez tout le monde : 
setwd("./R")

# Techniques, col 1:22 = Luttes directes, col 23:45 : Luttes indirectes
#Luttes directes Col 1:10 = TEM; 11:12 = TE; 13:14 = TPT; 15:18 = IN; 19:21 = LB ; 22 = MSQ
#Luttes indirectes Col 23:26 = VP ; 27 = VNA; 28:33 = PRO;34:41 = TRAISOL;42=TLM; 43 = RP2; 44 = TRASOL; 45 : RPF

# (1) pas de majuscule pour commencer un nom de variable : Techniques devient 
# techniques
# (2) quand tu utilises une focntion il est fortement recommandé d'utiliser
# les noms des arguments et de ne pas faire de raccourcis
# read.csv("Luttes.csv", dec = ",", header = T) devient
# read.csv(file = "Luttes.csv", dec = ",", header = TRUE)
# (3) dans ton fichier Luttes.csv il faut que chaque abbréviation soit unique
# sinon on ne peut pas les utiliser sans spécifier la modalité, par exemple
# TEM devient TEM_Mal, TEM devient TEM_DDVPetME, ...

techniques <- read.csv(file = "Luttes.csv", dec = ",", header = TRUE)
str(techniques)
# View(techniques) # (4) pas de View sans le commenter, sinon cela exclue les
# personnes qui ne travaillent pas avec Rstudio : le code doit être portable.
# summary(techniques) # (5) quel intérêt ?

#Typologie vergers
typoverger <- read.csv(file = "Typologievergers.csv", dec = ",", header = TRUE) # (1) et (2)
# View(typoverger) # (4)
#Attention, dans ce tableau, variables présentes qui peuvent changer en fonction des années

#Pour chaque agriculteur, on lui assigne une typologie et plusieurs modalités de luttes
#Agriculteur 1
# (6) en utilisant uniquement l'abbréviation, d'où l'intérêt à qu'elle soit 
# unique
# ag01 <- list (techniques[c(1,16),], typoverger[,(4)])
ag01 <- list(
  typoverger = "extensif", # (7) ajouter des noms de typo dans le fichier Typologievergers.csv
  techniques = c("TEM_Mal", "TEM_DDVPetME")
)
# View(ag01)
#Impact de ag001 sur Bactrocera dorsalis
Impactag01 <- sum(techniques[c(1,16),6]) # (8) on verra cela ensuite, dis moi quand le reste (1:7) est fait
Impactag01

#Agriculteur 2
ag02 <- list (techniques[c(4,22, 33, 40),], typoverger[,(2)])
View(ag02)
Impactag02 <- sum(techniques[c(1,16),6])
Impactag02

#Impact d'un agriculteur sur l'autre : Si l'agriculteur 2 n'a pas atteint le score minimale des bonnes pratiques (ici 5), impact négatif sur l'agriculteur 1
if(Impactag02 < 5) {
  Impactfinal <- Impactag01 - 2
  print(Impactfinal)
  print ("Impact negatif entre agriculteurs")
  }else{
  print ("Démarche vertueuse des argriculteurs")
}
