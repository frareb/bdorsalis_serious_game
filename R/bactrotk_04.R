# -----------------------------------------------------------------------------
# ESSAI N°4 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
# agricoles
#14.03.2023
# -----------------------------------------------------------------------------

setwd("./R")
itk <- read.csv(file = "pratiques23022023.csv", dec = ",", header = TRUE) 
View(itk)
#ETAPE 0 : FORMULES
get_RN_score <- function(ressncrs){ #Somme des points requis des cartes jouées
  scorei <- sapply(seq_along(ressncrs), function(i){
    score <- sum(itk[itk$modalite.abr %in% ressncrs[i],]$ressources.ncsr)
  })
  return(sum(scorei))
}
get_itk_score <- function(monTK){ #Somme de l'impact des pratiques sur BD :  si positif,  choix de la pratique efficace contre prévalence mouche
  scorei <- sapply(seq_along(monTK), function(i){
    score <- sum(itk[itk$modalite.abr %in% monTK[i],]$impactbdsurdt)
  })
  return(sum(scorei))
}
#ETAPE 1 : ENTREES
X1 = 10
BD = 0.6
X2 = 15
ag01 <- list( itk = c("pro_effic"), c(X1M_depart = X1 - X1*BD))
ag02 <- list(itk = c("dsb","dsb","piege_muscba"), c(X2M_depart = X2 - X2*BD))
n <- list(ag01, ag02)
n
#ETAPE 2 : RESULTATS INDIV
scoreitk <- for (i in 1:length(n)){
  if (get_RN_score(ressncrs = n[[i]][[1]])>n[[i]][2]) { #Vérification condition points nécessaires 
    print ("Vous n'avez pas assez de points pour jouer ces cartes, changer de cartes")
  }else{
    print(get_itk_score(monTK = n[[i]][[1]]))
  }
}
scorefinal_indiv1<- for (i in 1:length(n)){ #Resultat actions individuelles 
  if (get_itk_score(monTK = n[[i]][[1]])>= (0.1*n[[i]][[2]])) { #0.1 à modifier
    scoreindiv_positif <- n[[i]][[2]] + (n[[i]][[2]])* (BD+0.1) 
    print (scoreindiv_positif)
    print ("Pratiques vertueuses") #positif
  }else{
    scoreindiv_negatif <-n[[i]][[2]]
    print (scoreindiv_negatif) #négatif
    print ("Pratiques pas assez efficaces")
  }
}
#ETAPE 3 : RESULTATS COLLECTIFS



