# -----------------------------------------------------------------------------
# ESSAI N°4 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes agricoles
#14.03.2023
# -----------------------------------------------------------------------------
setwd("./R")
itk <- read.csv(file = "pratiques23022023.csv", dec = ",", header = TRUE) 
######### ETAPE 0 : FORMULES
get_RN_score <- function(ressncrs){ #Somme des points requis des cartes jouées
  scorei <- sapply(seq_along(ressncrs), function(i){
    score <- sum(itk[itk$modalite.abr %in% ressncrs[i],]$ressources.ncsr)
  })
  return(sum(scorei))
}
get_itk_score <- function(monTK){ #Somme de l'impact des pratiques par rapport à BD sur rendement :  si positif,  choix de la pratique efficace contre prévalence mouche
  scorei <- sapply(seq_along(monTK), function(i){
    score <- sum(itk[itk$modalite.abr %in% monTK[i],]$impactbdsurdt)
  })
  return(sum(scorei))
}
######## ETAPE 1 : ENTREES
PBD = 0.6
X=c(10,15,7,16)
ag01 <- list( itk = c("pro_effic"), X1_depart = X[1] - X[1]*PBD)
ag02 <- list(itk = c("dsb","dsb","piege_muscba"), X2_depart = X[2] - X[2]*PBD)
ag03 <- list(itk = c("appat_gf"), X3_depart = X[3] - X[3]*PBD)
ag04 <- list(itk = c("pro_effic"), X4_depart = X[4] - X[4]*PBD)
n <- list(ag01, ag02,ag03,ag04)
# -----------------------------------------------------------------------------
######### ETAPE 2 : RESULTATS INDIV #######
scoreitk <- for (i in 1:length(n)) {
  ifelse ((get_RN_score(ressncrs = n[[c(i,1)]])>n[[c(i,2)]]),  
  print ("Vous n'avez pas assez de points pour jouer ces cartes, changer de cartes"),
  print(get_itk_score(monTK = n[[i]][[1]])))
}
scoreindiv <- rep(NA, length(n))
a <- rep(NA, length(n))
scorefinal_indivtest<- for (i in 1:length(n)) {  #Resultat actions individuelles 
  if (get_itk_score(monTK = n[[c(i,1)]])>= (PBD*X[i])) { # modifier 0.6*n[[c(i,2)]] par X[i]*PBD ? 
    scoreindiv[i] <- X[i] - (X[i])* (PBD-0.1) 
    print (scoreindiv[i])
    print ("Pratiques vertueuses") #positif
    a[i] <- 1
    print(a[i])
  }else{
    scoreindiv[i] <-X[i] - (X[i])* (PBD) 
    print (scoreindiv[i]) #négatif
    print ("Pratiques pas assez efficaces")
    a[i] <- -1
    print(a[i])
  }
  as <- sum(a) #sum(rep(a[i], times = length(n)))
} 
######### ETAPE 3 : RESULTATS COLLECTIFS
for (i in 1:length(n)) {
  if (as>=0) {
    n[[c(i,2)]] <- scoreindiv[i]
  }else{
    n[[c(i,2)]] <- n[[c(i,2)]]
  }
}
n
if (as>=0) {
  print("Bravo, les pertes liés à la mouche ont baissé de 10%")  #réfléchir comment la mouche diminue d'un tour à l'autre #module de dispersion
  PBD <- PBD-0.1
  }else{
  print ("Mince, le taux de pertes n'a pas diminué cette saison ")
  }
######### ETAPE 4 : NOUVEAUX  CHOIX DE CARTE POUR PARTIE SUIVANTE
n[[1]][[1]] <- c("lb_nid", "lb_nid","lb_nid")
n[[2]][[1]] <- c("dsb","lb_nid", "lb_nid","lb_nid")
n[[3]][[1]] <-c("dsb")
n[[4]][[1]] <-c("dsb","lb_nid", "lb_nid","lb_nid")
######### ETAPE 5 : retour ETAPE 2

