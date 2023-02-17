
# -----------------------------------------------------------------------------
# ESSAI N°2 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
# agricoles
# -----------------------------------------------------------------------------

setwd("./R")

#------------------------------------------------------------------------------------------------------
#INITIALISATION DU JEU_BASSE CASAMANCE
#------------------------------------------------------------------------------------------------------
#ETAPE 1 : CONDITIONS INITIALES
#ETAPE 1.1, 1.21,1.22 AMENAGEMENT DES PARCELLES
#------------------------------------------------------------------------------------------------------

#Matrices : répartition des agriculteurs, répartition des cultures, rendement en fonction des cultures 
#Une case = 0,4 ha (Superficie plantation entre 0.2 et 10.7ha, moyenne est de 1,72ha, O.Ndiage 2020)

#Repartition des agriculteurs
repart_ag<-matrix( data = NA, nrow = 5, ncol = 5,
                   byrow = FALSE,
                   dimnames = list(c("a","b","c","d","e"), c(1,2,3,4,5)))

repart_ag[c(1,2),c(1,2)] <- "ag01"
repart_ag[c("a"), c(3,4,5)] <- "ag02"
repart_ag[c("b","c"), c(3,4,5)] <-"ag03"
repart_ag[c("c","d","e"), c(1)] <-"ag04"
repart_ag[3,2] <- "ag05"
repart_ag[c(4,5),c(2,3,4,5)] <- "ag06"
repart_ag

#Repartition des cultures
repart_cult <-repart_ag
repart_cult 

choix_cult_rdt <- list (
  variete = c("m_kent","m_keitt", "m_diourou", "m_autres", "m_sierra", "m_papay","m_peche", "anarca","pru_mon", "agru","ma_anu","ica_sene"),
  rendement = c(150,150,120,120,120,120,120,80,40,150,80,80)
)
choix_cult_rdt

#Rendement des cultures
repart_rdt <- repart_ag

#Agriculteur 1
repart_cult[which(repart_cult == "ag01")] <- c(choix_cult_rdt[[1]][c(1,2,3,8)])
repart_cult
repart_rdt[which(repart_rdt == "ag01" ) ] <- c(choix_cult_rdt[[2]][c(1,2,3,8)])
repart_rdt 

#Agriculteur 2
repart_cult[which(repart_cult == "ag02")] <- c(choix_cult_rdt[[1]][c(5,5,6)])
repart_cult
repart_rdt[which(repart_rdt == "ag02" ) ] <- c(choix_cult_rdt[[2]][c(5,5,6)])
repart_rdt 
#Agriculteur 3
repart_cult[which(repart_cult == "ag03")] <- c(choix_cult_rdt[[1]][c(1,2,3,5,5,6)])
repart_cult
repart_rdt[which(repart_rdt == "ag03" ) ] <- c(choix_cult_rdt[[2]][c(1,2,3,5,5,6)])
repart_rdt 
#Agriculteur 4
repart_cult[which(repart_cult == "ag04")] <- c(choix_cult_rdt[[1]][c(1,2,3)])
repart_cult
repart_rdt[which(repart_rdt == "ag04" ) ] <- c(choix_cult_rdt[[2]][c(1,2,3)])
repart_rdt 
#Agriculteur 5
repart_cult[which(repart_cult == "ag05")] <- c(choix_cult_rdt[[1]][c(1)])
repart_cult
repart_rdt[which(repart_rdt == "ag05" ) ] <- c(choix_cult_rdt[[2]][c(1)])
repart_rdt 
#Agriculteur 6
repart_cult[which(repart_cult == "ag06")] <- c(choix_cult_rdt[[1]][c(1,9,12,1,5,4,1,1)])
repart_cult
repart_rdt[which(repart_rdt == "ag06" ) ] <- c(choix_cult_rdt[[2]][c(1,9,12,1,5,4,1,1)])
repart_rdt 

#Visualisation des 3 matrices
resultfinal_repart <- array(data=c(repart_ag,repart_cult,repart_rdt), dim= c(5,5,3))
resultfinal_repart

#Pour l'utilisation de la matrice du rendement
repart_rdt <- matrix(as.numeric(repart_rdt), ncol=5)
repart_rdt


#------------------------------------------------------------------------------------------------------
#ETAPE 2 : DEROULEMENT DU JEU
#------------------------------------------------------------------------------------------------------
#ETAPE 2.1, 2.2,2.3 :  Gestion du verger & gestion de la mouche
#------------------------------------------------------------------------------------------------------

#Pratiques culturales
itk <- read.csv(file = "ITKtest2.csv", dec = ",", header = TRUE)
str(itk)
#Typologie des vergers
typoverger <- read.csv(file = "Typotest2.csv", dec = ",", header = TRUE) 
str(typoverger)
#Luttes contre la mouche
luttes <-read.csv(file = "Luttestest2.csv", dec = ",", header = TRUE)
str(luttes)

#Conditions initiales :
#Chaque joueur choisit une carte "Typologie"
# En fonction de la typologie, l'agriculteur ne part pas avec le même nombre de choix pour les pratiques et les luttes
#L'agriculteur en typologie intensif peut choisir 7 pratiques, extensif 5 et cueillette 3
#L'agriculteur en typologie intensif peut choisir 5 luttes, extensif 3 et cueillette 2

#PARTIE N°1
#Agriculteur 1
ag01 <- list(
  typoverger = "extensif", 
  itk = c("PRO_Enfouis", "TAILLE","PRO_Enfouis"),
  luttes = c("TEM_Mal","VP_Dieg", "TRAISOL_Cendre")
)
score_depart_ag01 = 8

get_itk_score <- function(monTK1){
  score <- sum((itk[itk$ITKAbr %in% monTK1,]$Impact.Bd))
  return(score)
}
I1 <- get_itk_score(monTK1 = ag01[[2]])

get_lutte_score <- function(monTK2){
  score <- sum((luttes[luttes$LutteAbr %in% monTK2,]$Impact.Bd))
  return(score)
}
L1 <-get_lutte_score(monTK2 = ag01[[3]])
IL1<-sum(I1,L1)

#Agriculteur 2 (augmentorium coûte 2 et LB aussi)
ag02 <- list(
  typoverger = "intensif", 
  itk = c( "TAILLE","	PRO_Augment", "PRO_Augment"),
  luttes = c("TEM_Mal","TPT_Succ","LB_Para")
)
score_depart_ag02 = 13
I2 <- get_itk_score(monTK1 = ag02[[2]])
L2 <-get_lutte_score(monTK2 = ag02[[3]])
IL2<-sum(I2,L2)
#Agriculteur 3 
ag03 <- list(
  typoverger = "cueillette", 
  itk = c( "FERTI","PRO_Coin"),
  luttes = c("TEM_Mal","TRAISOL_HuileN")
)
score_depart_ag03 = 5

I3 <- get_itk_score(monTK1 = ag03[[2]])
L3 <-get_lutte_score(monTK2 = ag03[[3]])
IL3<-sum(I3,L3)

#Si l'agriculteur intensif a un score < à 13, il fait perdre 1 point aux agriculteurs voisins, sinon il gagne 1 point
#Si l'agriculteur extensif a un score < à 9, il fait perdre 1 point aux agriculteurs voisins, sinon il gagne 1 point
#Si l'agriculteur cueillette a un score < à 7, il fait perdre 1 point aux agriculteurs voisins, sinon il gagne 1 point
# Si la plupart des scores des agriculteurs n'a pas atteint leur objectif, la mouche double, sinon enlever la moitié des mouches (condition initale : chaque verger a un jeton mouche sur sa parcelle / en fonction scnénario)

#Tests (2 façons d'obtenir le même résultat) si les agriculteurs ont atteints leurs objectif en groupe/collectivement
if ((IL1<9 & IL2<13) | (IL2<13 & IL3<7) | (IL1<9 & IL3<7)) {
  print ("La présence des mouches a doublé") ### AU LIEU DOUBLE, mettre en fonction de faux. Nb de faux * 2 ou pas de diminution ?
}else{
  print ("Bien joué, la présence des mouches a diminué par 2")
}

if (2>= sum(IL1 < 9,IL2<13,IL3<7)) {
  print ("La présence des mouches a doublé")
}else{
  print ("Bien joué, la présence des mouches a diminué par 2")
}

#Test si chaque agriculteur a réussi 
 
result_ag01 <- if(IL1>=9) {
  print ("Les pratiques et luttes réalisées favorisent votre rendement, vous avez gagné un point")
  score_final_ag01 <- score_depart_ag01 + 1
  }else{
    print ("Les pratiques réalisées favorisent la présence de la mouche, vos voisins perdent tous un point")
    score_final_ag02 <- score_depart_ag02 - 1
    score_final_ag03 <- score_depart_ag03 - 1
  } 

result_ag02 <- if(IL2>=13) {
  print ("Les pratiques et luttes réalisées favorisent votre rendement,vous avez gagné un point")
  score_final_ag02 <- score_depart_ag02 + 1
}else{
  print ("Les pratiques réalisées favorisent la présence de la mouche,vos voisins perdent tous un point")
  score_final_ag01 <- score_depart_ag01 - 1
  score_final_ag03 <- score_depart_ag03 - 1
} 

result_ag03 <- if(IL3>=7) {
  print ("Les pratiques et luttes réalisées favorisent votre rendement, vous avez gagné un point")
  score_final_ag03 <- score_depart_ag03 + 1
 }else{
  print ("Les pratiques réalisées favorisent la présence de la mouche,vos voisins perdent tous un point")
  score_final_ag01 <- score_depart_ag01 - 1
  score_final_ag02 <- score_depart_ag02 - 1
 }

#PARTIE N°2
ag01 <- list(
  typoverger = "extensif", 
  itk = c(""),
  luttes = c("")
)
score_depart2_ag01 = score_final_ag01

ag02 <- list(
  typoverger = "intensif", 
  itk = c( ""),
  luttes = c("")
)
score_depart2_ag02 = score_final_ag02

ag03 <- list(
  typoverger = "cueillette", 
  itk = c( ""),
  luttes = c("")
)
score_depart2_ag03 = score_final_ag03

