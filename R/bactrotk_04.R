# -----------------------------------------------------------------------------
# ESSAI N°4 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
#   agricoles (14.03.2023) Audrey N'Gom et François Rebaudo
# -----------------------------------------------------------------------------
setwd("./R")

# --- 1. INITIALISATION DU JEU ------------------------------------------------
tauxDePertesBD <- 0.6
rdtOptimal <- c(10, 15, 7, 16)
ag01 <- list(
  itk = c("pro_effic"), 
  X1_depart = rdtOptimal[1] - rdtOptimal[1]*tauxDePertesBD
)
ag02 <- list(
  itk = c("dsb","dsb","piege_muscba"), 
  X2_depart = rdtOptimal[2] - rdtOptimal[2]*tauxDePertesBD
)
ag03 <- list(
  itk = c("appat_gf"), 
  X3_depart = rdtOptimal[3] - rdtOptimal[3]*tauxDePertesBD
)
ag04 <- list(
  itk = c("pro_effic"), 
  X4_depart = rdtOptimal[4] - rdtOptimal[4]*tauxDePertesBD
)
listAgriITKetX <- list(ag01, ag02, ag03, ag04)
# -----------------------------------------------------------------------------

# --- 2. CHARGEMENT DES DONNEES ET FONCTIONS ----------------------------------
bloc01 <- {

# Chargement de la liste des pratiques culturales
itk <- read.csv(file = "pratiques23022023.csv", dec = ",", header = TRUE) 

# Fonctions

# Somme des points requis des cartes jouées
get_RN_score <- function(ressncrs){ 
  scorei <- sapply(seq_along(ressncrs), function(i){
    score <- itk[itk$modalite.abr == ressncrs[i],]$ressources.ncsr
    if(length(score)==0){score <- NA}
    return(score)
    # score <- sum(itk[itk$modalite.abr %in% ressncrs[i],]$ressources.ncsr)
  })
  return(sum(scorei))
}

# Somme de l'impact des pratiques par rapport à BD sur rendement :  si positif,
#   choix de la pratique efficace contre prévalence mouche
get_itk_score <- function(monTK){ 
  scorei <- sapply(seq_along(monTK), function(i){
    score <- sum(itk[itk$modalite.abr %in% monTK[i],]$impactbdsurdt)
  })
  return(sum(scorei))
}

# Verification du nombre de points de jeu pour chaque agriculteur
verifNumPoints <- function(listAgriITKetX){
  msg <- "Vérification du nombre de points"
  for (i in 1:length(listAgriITKetX)) {
    msgi <- paste0(
      "Agri", i, ": ", get_RN_score(ressncrs = listAgriITKetX[[c(i,1)]]), "/", 
      listAgriITKetX[[c(i,2)]], " points utilisés : "
    )
    msgi <- paste0(msgi, switch(
      as.character(get_RN_score(ressncrs = listAgriITKetX[[c(i,1)]]) > listAgriITKetX[[c(i,2)]]),
      "TRUE" = "pas assez de points",
      "FALSE" = "OK",
      "NA" = "erreur de saisie itk"
    ))
    msg <- paste(msg, msgi, sep = "\n")
  }
  return(msg)
}

}

# --- 3. BOUCLE DU JEU --------------------------------------------------------
cat(verifNumPoints(listAgriITKetX)) # vérif nombre de points

scoreindiv <- rep(NA, length(listAgriITKetX))
a <- rep(NA, length(listAgriITKetX))
scorefinal_indivtest<- for (i in 1:length(listAgriITKetX)) {  #Resultat actions individuelles 
  if (get_itk_score(monTK = listAgriITKetX[[c(i,1)]])>= (tauxDePertesBD*rdtOptimal[i])) { # modifier 0.6*n[[c(i,2)]] par rdtOptimal[i]*tauxDePertesBD ? 
    scoreindiv[i] <- rdtOptimal[i] - (rdtOptimal[i])* (tauxDePertesBD-0.1) 
    print (scoreindiv[i])
    print ("Pratiques vertueuses") #positif
    a[i] <- 1
    print(a[i])
  } else {
    scoreindiv[i] <-rdtOptimal[i] - (rdtOptimal[i])* (tauxDePertesBD) 
    print (scoreindiv[i]) #négatif
    print ("Pratiques pas assez efficaces")
    a[i] <- -1
    print(a[i])
  }
  as <- sum(a) #sum(rep(a[i], times = length(listAgriITKetX)))
} 
######### ETAPE 3 : RESULTATS COLLECTIFS
for (i in 1:length(listAgriITKetX)) {
  if (as>=0) {
    listAgriITKetX[[c(i,2)]] <- scoreindiv[i]
  }else{
    listAgriITKetX[[c(i,2)]] <- listAgriITKetX[[c(i,2)]]
  }
}
listAgriITKetX
if (as >= 0) {
  print("Bravo, les pertes liés à la mouche ont baissé de 10%")  #réfléchir comment la mouche diminue d'un tour à l'autre #module de dispersion
  tauxDePertesBD <- tauxDePertesBD-0.1
  }else{
  print ("Mince, le taux de pertes n'a pas diminué cette saison ")
  }
######### ETAPE 4 : NOUVEAUX  CHOIX DE CARTE POUR PARTIE SUIVANTE
listAgriITKetX[[1]][[1]] <- c("lb_nid", "lb_nid","lb_nid")
listAgriITKetX[[2]][[1]] <- c("dsb","lb_nid", "lb_nid","lb_nid")
listAgriITKetX[[3]][[1]] <- c("dsb")
listAgriITKetX[[4]][[1]] <- c("dsb","lb_nid", "lb_nid","lb_nid")
######### ETAPE 5 : retour ETAPE 2

