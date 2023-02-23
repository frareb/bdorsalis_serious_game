# -----------------------------------------------------------------------------
# ESSAI N°3 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
# agricoles
#23.02.2023
# -----------------------------------------------------------------------------

setwd("./R")

#ETAPE 1 : rendement&superficie de départ agriculteurs
#Rendement en fonction superficie verger

rdt_ha <- read.csv(file = "rdtha23022023.csv", dec = ",", header = TRUE)
rdt_ha 
ag01_rdt_points = sum(rdt_ha[1,][c(2,3,4)])
ag01_rdt_points
ag02_rdt_points = sum(rdt_ha[2,][c(2,3,4)])
ag02_rdt_points
ag03_rdt_points = sum(rdt_ha[3,][c(2)])
ag03_rdt_points

#ETAPE 2 : DEROULEMENT DU JEU
itk <- read.csv(file = "pratiques23022023.csv", dec = ",", header = TRUE)
#Impactbdsurrdt : si positif,  choix de la pratique efficace contre prévalence mouche

#AGRICULTEUR 1
ag01 <- list(
  rdt_initial = ag01_rdt_points,
  itk = c("pro_effic", "dsb"),
  mouche = 3
)
get_itk_score <- function(monTK1){
  scorei <- sapply(seq_along(monTK1), function(i){
    score <- sum(itk[itk$modalite.abr %in% monTK1[i],]$impactbdsurdt)
  })
  return(sum(scorei))
}

#Pour jouer chaque carte, il est nécessaire d'avoir les points requis : condition "contitn_agXX"
get_ressourcenecsr_score <- function(ressncrs){
  scoret <- sapply(seq_along(ressncrs), function(t){
    score <- sum(itk[itk$modalite.abr %in% ressncrs[t],]$ressources.ncsr)
  })
  return(sum(scoret))
}
conditn_ag01 <- get_ressourcenecsr_score(ressncrs = ag01[[2]])
scorefinal_ag01 <- if (conditn_ag01>ag01_rdt_points){
  print ("Vous n'avez pas assez de points pour jouer ces cartes")
}else{
  get_itk_score(monTK1 = ag01[[2]])
}
scorefinal_ag01
if (scorefinal_ag01>= 0.2*ag01_rdt_points){ #Si l'agriculteur a un score qui a permis de compenser 20% pertes potentiels
  nouveauscore_ag01 = ag01_rdt_points + 0.5*ag01_rdt_points #bénéfice
}else{
  nouveauscore_ag01 = 0.8*ag01_rdt_points #20% de ventes en moins à cause mouche #20% de pertes
}

ag01_result <- floor(nouveauscore_ag01) #arrondir à l'entier inférieur (ceiling pour entier supérieur)
ag01_result
ag01[[1]] <- ag01_result
#AGRICULTEUR 2
ag02 <- list(
  rdt_initial = ag02_rdt_points,
  itk = c("pro_effic", "dsb","dsb","piege_muscba"),
  mouche = 3
)
conditn_ag02 <- get_ressourcenecsr_score(ressncrs = ag02[[2]])
scorefinal_ag02 <- if (conditn_ag02>ag02_rdt_points){
  print ("Vous n'avez pas assez de points pour jouer ces cartes")
}else{
  get_itk_score(monTK1 = ag02[[2]])
}
if (scorefinal_ag02>= 0.2*ag02_rdt_points){
  nouveauscore_ag02 = ag02_rdt_points + 0.5*ag02_rdt_points
}else{
  nouveauscore_ag02 = 0.8*ag02_rdt_points #20% de ventes en moins à cause mouche
}

ag02_result <- floor(nouveauscore_ag02) #arrondir à l'entier inférieur (ceiling pour entier supérieur)
ag02[[1]] <- ag02_result

#AGRICULTEUR3
ag03 <- list(
  rdt_initial = ag03_rdt_points,
  itk = c("pro_effic", "dsb","insect","piege_muscba","appat_gf","taille"),
  mouche = 3
)
conditn_ag03 <- get_ressourcenecsr_score(ressncrs = ag03[[2]])
scorefinal_ag03 <- if (conditn_ag03>ag03_rdt_points){
  print ("Vous n'avez pas assez de points pour jouer ces cartes")
}else{
  get_itk_score(monTK1 = ag03[[2]])
}
if (scorefinal_ag03>= 0.2*ag03_rdt_points){
  nouveauscore_ag03 = ag03_rdt_points + 0.5*ag03_rdt_points
}else{
  nouveauscore_ag03 = 0.8*ag03_rdt_points #20% de ventes en moins à cause mouche
}

ag03_result <- floor(nouveauscore_ag03) #arrondir à l'entier inférieur (ceiling pour entier supérieur)
ag03[[1]] <- ag03_result

#Objectif commun

if ((ag01_result<= ag01_rdt_points)|(ag02_result<= ag02_rdt_points)|(ag03_result<= ag03_rdt_points)){
  print ("La présence des mouches n'a pas diminué, elle a augmenté") 
  if (ag01_result<= ag01_rdt_points) {
    ag01[[1]]= ag01_result - 2
    ag02[[1]]= ag02_result - 1
    ag03[[1]]= ag03_result - 1
    ag01[[3]] = 1.3*ag01[[3]]
  }else{
    ag01[[1]]= ag01_result 
    ag02[[1]]= ag02_result 
    ag03[[1]]= ag03_result 
  }
  if (ag02_result<= ag02_rdt_points) {
    ag01[[1]]= ag01_result - 1
    ag02[[1]]= ag02_result - 2
    ag03[[1]]= ag03_result - 1
    ag02[[3]] = 1.3*ag02[[3]]
  }else{
    ag01[[1]]= ag01_result 
    ag02[[1]]= ag02_result 
    ag03[[1]]= ag03_result 
  }
  if (ag03_result<= ag03_rdt_points) {
    ag01[[1]]= ag01_result - 1
    ag02[[1]]= ag02_result - 1
    ag03[[1]]= ag03_result - 2
    ag03[[3]] = 1.3*ag03[[3]]
  }else{
    ag01[[1]]= ag01_result 
    ag02[[1]]= ag02_result 
    ag03[[1]]= ag03_result 
  }
}else{
  print ("Bien joué, la présence des mouches a diminué pour ceux qui ont atteint leur objectifs")
  if (ag01_result>= 0.2*ag01_rdt_points) {
    ag01[[3]] = 0.7*ag01[[3]]
  }else{
    ag01[[3]]}
  if (ag02_result>= 0.2*ag02_rdt_points){ 
    ag02[[3]] = 0.7*ag02[[3]]
  }else{
    ag02[[3]]}
  if (ag03_result>= 0.2*ag03_rdt_points) {
    ag03[[3]] = 0.7*ag03[[3]]
  }else{
    ag03[[3]]}
}

#Nouveaux points  pour partie suivante
ag01[[1]] 
ag01[[3]]
ag02[[1]] 
ag02[[3]]
ag03[[1]] 
ag03[[3]]

#PARTIE 2
#AGRICULTEUR 1
ag01[[2]] = c("taille")
get_itk_score(monTK1 = ag01[[2]])
conditn_ag01 <- get_ressourcenecsr_score(ressncrs = ag01[[2]])
scorefinal_ag01 <- if (conditn_ag01>ag01[[1]]){
  print ("Vous n'avez pas assez de points pour jouer ces cartes")
}else{
  get_itk_score(monTK1 = ag01[[2]])
}
if (scorefinal_ag01>= 0.2*ag01[[1]]){ 
  nouveauscore_ag01 = ag01[[1]] + 0.5*ag01[[1]]
}else{
  nouveauscore_ag01 = 0.8*ag01[[1]] #20% de ventes en moins à cause mouche #20% de pertes
}

ag01_result <- floor(nouveauscore_ag01) #arrondir à l'entier inférieur (ceiling pour entier supérieur)
ag01_result
#AGRICULTEUR 2
ag02[[2]] = c("taille","pro_coin","pro_coin","pro_coin")
get_itk_score(monTK1 = ag02[[2]])
conditn_ag02 <- get_ressourcenecsr_score(ressncrs = ag02[[2]])
scorefinal_ag02 <- if (conditn_ag02>ag02[[1]]){
  print ("Vous n'avez pas assez de points pour jouer ces cartes")
}else{
  get_itk_score(monTK1 = ag02[[2]])
}
if (scorefinal_ag02>= 0.2*ag02[[1]]){ #Si l'agriculteur a un score qui a permis de compenser 20% pertes potentiels
  nouveauscore_ag02 = ag02[[1]] + 0.5*ag02[[1]]
}else{
  nouveauscore_ag02 = 0.8*ag02[[1]] #20% de ventes en moins à cause mouche #20% de pertes
}

ag02_result <- floor(nouveauscore_ag02) #arrondir à l'entier inférieur (ceiling pour entier supérieur)
ag02_result

#AGRICULTEUR 3
ag03[[2]] = c("taille","pro_coin","pro_coin","pro_coin")
get_itk_score(monTK1 = ag03[[2]])
conditn_ag03 <- get_ressourcenecsr_score(ressncrs = ag03[[2]])
scorefinal_ag03 <- if (conditn_ag03>ag03[[1]]){
  print ("Vous n'avez pas assez de points pour jouer ces cartes")
}else{
  get_itk_score(monTK1 = ag03[[2]])
}
if (scorefinal_ag03>= 0.2*ag03[[1]]){ #Si l'agriculteur a un score qui a permis de compenser 20% pertes potentiels
  nouveauscore_ag03 = ag03[[1]] + 0.5*ag03[[1]]
}else{
  nouveauscore_ag03 = 0.8*ag03[[1]] #20% de ventes en moins à cause mouche #20% de pertes
}

ag03_result <- floor(nouveauscore_ag03) #arrondir à l'entier inférieur (ceiling pour entier supérieur)
ag03_result

#Objectif commun

if ((ag01_result<= ag01[[1]])|(ag02_result<= ag02[[1]])|(ag03_result<= ag03[[1]])){
  print ("La présence des mouches n'a pas diminué, elle a augmenté") 
  if (ag01_result<= ag01[[1]]) {
    ag01[[1]]= ag01_result - 2
    ag02[[1]]= ag02_result - 1
    ag03[[1]]= ag03_result - 1
    ag01[[3]] = 1.3*ag01[[3]]
  }else{
    ag01[[1]]= ag01_result 
    ag02[[1]]= ag02_result 
    ag03[[1]]= ag03_result 
    
  }
  if (ag02_result<= ag02[[1]]) {
    ag01[[1]]= ag01_result - 1
    ag02[[1]]= ag02_result - 2
    ag03[[1]]= ag03_result - 1
    ag02[[3]] = 1.3*ag02[[3]]
  }else{
    ag01[[1]]= ag01_result 
    ag02[[1]]= ag02_result 
    ag03[[1]]= ag03_result 
  }
  if (ag03_result<= ag03[[1]]) {
    ag01[[1]]= ag01_result - 1
    ag02[[1]]= ag02_result - 1
    ag03[[1]]= ag03_result - 2
    ag03[[3]] = 1.3*ag03[[3]]
    
  }else{
    ag01[[1]]= ag01_result 
    ag02[[1]]= ag02_result 
    ag03[[1]]= ag03_result 
  }
}else{
  print ("Bien joué, la présence des mouches a diminué pour ceux qui ont atteint leur objectifs")
  if (ag01_result>= 0.2*ag01_rdt_points) {
    ag01[[3]] = 0.7*ag01[[3]]
  }else{
    ag01[[3]]}
  if (ag02_result>= 0.2*ag02_rdt_points){ 
    ag02[[3]] = 0.7*ag02[[3]]
  }else{
    ag02[[3]]}
  if (ag03_result>= 0.2*ag03_rdt_points) {
    ag03[[3]] = 0.7*ag03[[3]]
  }else{
    ag03[[3]]}
}
ag01[[1]]
ag02[[1]]
ag03[[1]]
ag01[[3]]
ag02[[3]]
ag03[[3]]
