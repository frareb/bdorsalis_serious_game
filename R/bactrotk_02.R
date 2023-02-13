
# -----------------------------------------------------------------------------
# ESSAI N°2 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
# agricoles
# -----------------------------------------------------------------------------

setwd("./R")

#Pratiques culturales
itk <- read.csv(file = "ITKtest2.csv", dec = ",", header = TRUE)
str(itk)
#Typologie des vergers
typoverger <- read.csv(file = "Typotest2.csv", dec = ",", header = TRUE) 
str(typoverger)
#Luttes contre la mouche
luttes <-read.csv(file = "Luttestest2.csv", dec = ",", header = TRUE)
str(luttes)

#Pour chaque agriculteur, on lui assigne une typologie, des pratiques agricoles (hors récolte) et plusieurs modalités de luttes 
# En fonction de la typologie, l'agriculteur ne part pas avec le même nombre de choix pour les pratiques et les luttes
#L'agriculteur en typologie intensif peut choisir 7 pratiques, extensif 5 et cueillette 3
#L'agriculteur en typologie intensif peut choisir 5 luttes, extensif 3 et cueillette 2

#Agriculteur 1
ag01 <- list(
  typoverger = "extensif", 
  itk = c("PRO_Enfouis", "TAILLE","PRO_Enfouis"),
  luttes = c("TEM_Mal","VP_Dieg","REPREC", "EGOUTT")
)
get_itk_score <- function(monTK1){
  score <- sum((itk[itk$ITKAbr %in% monTK1,]$Impact.Bd))
  return(score)
}
I1 <- get_itk_score(monTK1 = ag01[[2]])

get_lutte_score <- function(monTK2){
  score <- sum((luttes[luttes$LutteAbr %in% monTK2,]$Impact.Bd))
  return(score)
}
L1 <-get_lutte_score(monTK = ag01[[3]])
IL1<-sum(I1,L1)

#Agriculteur 2 (augmentorium coûte 2 et LB aussi)
ag02 <- list(
  typoverger = "intensif", 
  itk = c( "TAILLE","	PRO_Augment", "PRO_Augment"),
  luttes = c("TEM_Mal","TPT_Succ","LB_Para","REPREC", "EGOUTT")
)

I2 <- get_itk_score(monTK1 = ag02[[2]])
L2 <-get_lutte_score(monTK = ag02[[3]])
IL2<-sum(I2,L2)
#Agriculteur 3 
ag03 <- list(
  typoverger = "cueillette", 
  itk = c( "FERTI","PRO_Coin"),
  luttes = c("TEM_Mal","TRAISOL_HuileN", "RE")
)

I3 <- get_itk_score(monTK1 = ag03[[2]])
L3 <-get_lutte_score(monTK = ag03[[3]])
IL3<-sum(I3,L3)

#Si l'agriculteur intensif a un score < à 13, il fait perdre 1 point aux agriculteurs voisins, sinon il gagne 1 point
#Si l'agriculteur extensif a un score < à 9, il fait perdre 1 point aux agriculteurs voisins, sinon il gagne 1 point
#Si l'agriculteur cueillette a un score < à 7, il fait perdre 1 point aux agriculteurs voisins, sinon il gagne 1 point
# Si la plupart des scores des agriculteurs n'a pas atteint leur objectif, la mouche double, sinon enlever la moitié des mouches (condition initale : chaque verger a un jeton mouche sur sa parcelle / en fonction scnénario)

#Test si chaque agriculteur a réussi 
#Si success, on obtient TRUE : 
IL1>=9
IL2>=13
IL3>=7
#Tests (2 façons d'obtenir le même résultat) si les agriculteurs ont atteints leurs objectif en groupe
if ((IL1<9 & IL2<13) | (IL2<13 & IL3<7) | (IL1<9 & IL3<7)) {
  print ("La présence des mouches a doublé")
}else{
  print ("Bien joué, la présence des mouches a diminué par 2")
}

if (2>= sum(IL1 < 9,IL2<13,IL3<7)) {
  print ("La présence des mouches a doublé")
}else{
  print ("Bien joué, la présence des mouches a diminué par 2")
}
