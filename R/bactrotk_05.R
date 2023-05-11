# -----------------------------------------------------------------------------
# ESSAI N°4 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
#   agricoles (14.03.2023) Audrey N'Gom et François Rebaudo
# -----------------------------------------------------------------------------
setwd("./R")
setwd("C:/Users/etudiant/Documents/2023_AudreyNGom/github/bdorsalis_serious_game/R")
install.packages("xlsx")
library("xlsx")
#data frame résultats et cartes choisies
results_tours <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(results_tours) <- c('Agriculteurs','TauxdePerte','RendementOptim', 'RendementDébut', 'RendementReel', 'NoteRendement','NoteMouche', 'Qualité','Tour')
cartes_tours <- data.frame(matrix(ncol = 17, nrow = 0))
colnames(cartes_tours) <- c("Agriculteurs", "irr", "fer", "dsb", "lab", "tai", "bio", "ins", "pre", "prc", "pma", "pba", "apg", "apm", "lbf", "lbe", "lbn")

# --- 1. INITIALISATION DU JEU ------------------------------------------------
bloc00 <- {
  niveauDeDifficulte <- 3
  tauxDePertesBD <- 0.4
  rdtOptimal <- c(20, 25,60, 75)
  ag01 <- list(
    itk = c("lbn","lbf","fer","bio","irr","fer","pre"), 
    X1_depart = rdtOptimal[1] - rdtOptimal[1]*tauxDePertesBD
  )
  ag02 <- list(
    itk = c("lbn","irr","pba","apm","tai","dsb","pre"), 
    X2_depart = rdtOptimal[2] - rdtOptimal[2]*tauxDePertesBD
  )
  ag03 <- list(
    itk = c("irr","fer","dsb","pre","lab","lbn"), 
    X3_depart = rdtOptimal[3] - rdtOptimal[3]*tauxDePertesBD
  )
  ag04 <- list(
    itk = c("irr","fer","pba","prc","dsb"), 
    X4_depart = rdtOptimal[4] - rdtOptimal[4]*tauxDePertesBD
  )
  listAgriITKetX <- list(ag01 = ag01, ag02 = ag02, ag03 = ag03, ag04 = ag04)
}
#afficher points de départs
# for (i in 1:length(listAgriITKetX)) { print (listAgriITKetX[[c(i,2)]])} 
sapply(listAgriITKetX, "[[", 2)

# --- 2. CHARGEMENT DES DONNEES ET FONCTIONS ----------------------------------
bloc01 <- {
  
# Chargement de la liste des pratiques culturales
itk <- read.csv(file = "pratiques04052023.csv", dec = ",", header = TRUE) 
# la meilleure note technique en réalisant les pratiques de luttes directes et indirectes en fonction du niveau de difficulté
bestITKmouchenote <- sum(itk$gestionlutte[itk$gestionlutte > 0]*itk$frequence[itk$gestionlutte > 0]) / niveauDeDifficulte 
#la meilleure note technique en réalisant les pratiques d'optimisation de rendement
bestITKrdtnote <- sum(itk$gestionrdt[itk$gestionrdt>0]*itk$frequence[itk$gestionrdt>0])
  
# Somme des points requis des cartes jouées
get_RN_score <- function(ressncrs){ 
    scorei <- sapply(seq_along(ressncrs), function(i){
      score <- itk[itk$abreviation_mo == ressncrs[i],]$montant
      if(length(score) == 0){score <- NA}
      return(score)
  })
    return(sum(scorei))
}
  
# Somme de l'impact des pratiques par rapport à BD sur rendement :  si positif,
#   choix de la pratique efficace contre prévalence mouche

get_itkmouche_score <- function(monTK){ 
  if ("ins" %in% monTK & "lbf" %in% monTK |"ins" %in% monTK & "lbn" %in% monTK ) {
    score_adj <- -3
  } else {
    score_adj <- 0
  }
  scorei <- sapply(seq_along(monTK), function(i){
    score <- itk[itk$abreviation_mo == monTK[i],]$gestionlutte
    if(length(score) == 0){score <- NA}
    return(score)
  })
  score_sum <- sum(scorei, na.rm = TRUE) + score_adj
  return(score_sum)
}


# Somme de l'impact des pratiques par rapport au rendement 
get_itkrendement_score <- function(monTK1){ 
    rscorei <- sapply(seq_along(monTK1), function(i){
      rscore <- itk[itk$abreviation_mo == monTK1[i],]$gestionrdt
      if(length(rscore) == 0){rscore <- NA}
      return(rscore)
    })
    if((is.null(rscorei)) | (length(rscorei) == 0)){rscorei <- 0}
    return(sum(rscorei))
}

 
# Verification du nombre de points de jeu pour chaque agriculteur
verifNumPoints <- function(listAgriITKetX){
    msg <- "Vérification du nombre de points"
    for (i in 1:length(listAgriITKetX)) {
      msgi <- paste0(
        "Agri", i, ": ", get_RN_score(ressncrs = listAgriITKetX[[c(i,1)]]), "/", 
        round(listAgriITKetX[[c(i,2)]], digits = 2), " points utilisés : "
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

#Calcul de la note lié aux pratiques de luttes
#Si note > 1, taux de pertes va diminuer
#Si note <1, taux de pertes va augmenter
calculTxNoteITKmouche <- function(){
    sapply(seq_along(listAgriITKetX), function(i){
      tauxMyITKnote <- get_itkmouche_score(monTK = listAgriITKetX[[c(i,1)]])/bestITKmouchenote
    })
  }
  
#Calcul de la note lié aux pratiques d'optimisation du rendement
calculTxNoteITKrdt <- function(){
    sapply(seq_along(listAgriITKetX), function(i){
      tauxMyITKnoterdt <- get_itkrendement_score(monTK1 = listAgriITKetX[[c(i,1)]])/bestITKrdtnote
    })
}
  
calculperteindiv <- function(){
    sapply(seq_along(listAgriITKetX), function(i){
      new_losses <- pmin(pmax(tauxDePertesBD * (1 - 0.5 * (calculTxNoteITKmouche()[i] - 1)), 0), 1) 
    })
}
  
#Calcul de la qualité des mangues
calculqualite <- function(){
    sapply(seq_along(listAgriITKetX), function(i){
      qualite <- (1-calculperteindiv()[i])*10
    })
}
# Calcul nouveau taux de perte et rdt avec le taux de note ITK et incidence BD
calculRdt <- function(){
    if(sum(calculTxNoteITKmouche()>=1)>=(0.5*(length(listAgriITKetX)))){ 
      newTauxDePertesBD <- tauxDePertesBD - tauxDePertesBD*0.1 
    } else {
      newTauxDePertesBD <- tauxDePertesBD + tauxDePertesBD*0.1
    }
    msg <- paste0("Moy. note ITK: ", round(mean(calculTxNoteITKmouche()), digits = 2), "\n",
                  "Taux de pertes BD initial: ", round(tauxDePertesBD, digits= 2), 
                  ". Nouveau taux : ", 
                  newTauxDePertesBD, ".\n"
    )
    cat(msg)
    tauxDePertesBD <<- newTauxDePertesBD
    if(tauxDePertesBD > 1){tauxDePertesBD <- 1}
    rdtReel <<-  sapply(seq_along(listAgriITKetX), function(i){
      rdtReel <- 0.5*rdtOptimal[i] + ((0.5*rdtOptimal[i])* calculTxNoteITKrdt()[i]*(1-calculperteindiv()[i]))
      rdtReel
      if (rdtReel > rdtOptimal [i]){rdtReel <- rdtOptimal[i]} 
      if (rdtReel <0) {rdtReel <- 0} 
      msg <- paste0(
        "Agri", i, 
        ": rdt optimal: ", round(rdtOptimal[i], digits = 2),
        " ; rdt début : ", round(listAgriITKetX[[c(i,2)]], digits = 2),
        " ; rdt réel: ", round(rdtReel, digits = 2),
        " ; note ITK rdt: ", round(calculTxNoteITKrdt()[i], digits = 2), 
        " ; note ITK mouche: ", round(calculTxNoteITKmouche()[i], digits = 2), 
        " ; qualité: ",round(calculqualite()[i], digits = 1),
        "\n"
      )
      cat(msg)
      return(rdtReel)
    }) 
    return(list(tauxDePertesBD, rdtReel))
}
  
}

# --- 3. BOUCLE DU JEU --------------------------------------------------------
#TOUR 1 RECOLTE PRECOCE
sapply(listAgriITKetX, "[[", 2)
cat(verifNumPoints(listAgriITKetX)) # vérif nombre de points
tourDeJeu <- calculRdt() # rdt en fin de tour et nouveau taux de pertes BD
#tauxDePertesBD <- tourDeJeu[[1]]
#rdtReel <- tourDeJeu[[2]]
for (i in 1:length(listAgriITKetX)){ #conserver résultats
  agri <- i
  mouche <- tauxDePertesBD
  rdtOptimaldata <- round(rdtOptimal[i], digits = 2)
  rdtdebut <- round(listAgriITKetX[[c(i,2)]], digits = 2)
  rdtreel <- round(rdtReel[i], digits = 1)
  noteITKrdt <-  round(calculTxNoteITKrdt()[i], digits = 2)
  noteITKmouche <- round(calculTxNoteITKmouche()[i], digits = 2)
  qualite <-  round(calculqualite()[i])
  tour <- rep(1.1, each = length(listAgriITKetX))
  results_tours[nrow(results_tours) + 1,] = c(agri, mouche, rdtOptimaldata, rdtdebut, rdtreel, noteITKrdt, noteITKmouche, qualite,tour)
}
results_tours
write.xlsx(x = results_tours, file = "Points_SessionNiayes1.xlsx",sheet = "Tour 1.pre")

for (i in 1:length(listAgriITKetX)) { #conserver cartes jouées
  agri <- i
  choix_cartes <- c(itk$abreviation_mo) %in% listAgriITKetX[[i]]$itk
  cartes_tours[nrow(cartes_tours) + 1, 1] <- agri
  if (any(choix_cartes)) {
    cartes_tours[nrow(cartes_tours), 2:17] <- (ifelse(choix_cartes, 1, 0))
  }
}
cartes_tours
write.xlsx(x = cartes_tours, file = "Cartes_SessionNiayes1.xlsx",sheet = "Tour 1.pre")

# ---TOUR 1 RECOLTE TARDIVE.--------------------------------------------------------------------------
bloc02 <- {
  niveauDeDifficulte <- 3
  tauxDePertesBD <- 0.5
  rdtOptimal <- c(20, 25,60, 75)
  ag01 <- list(
    itk = c("lbn","lbf","fer","bio","irr","fer","tai"), 
    X1_depart = 20
  )
  ag02 <- list(
    itk = c("lbn","irr","pba","apm","tai","dsb","pre"), 
    X2_depart = 10
  )
  ag03 <- list(
    itk = c("irr","fer","pba","pre","lab","lbn"), 
    X3_depart = 60
  )
  ag04 <- list(
    itk = c("irr","prc","apg","ins","lbn"), 
    X4_depart = 75
  )
  listAgriITKetX <- list(ag01 = ag01, ag02 = ag02, ag03 = ag03, ag04 = ag04)
}
#afficher points de départs
# for (i in 1:length(listAgriITKetX)) { print (listAgriITKetX[[c(i,2)]])} 
sapply(listAgriITKetX, "[[", 2)
cat(verifNumPoints(listAgriITKetX)) # vérif nombre de points
tourDeJeu <- calculRdt() # rdt en fin de tour et nouveau taux de pertes BD
for (i in 1:length(listAgriITKetX)){
  agri <- i
  mouche <- tauxDePertesBD
  rdtOptimaldata <- round(rdtOptimal[i], digits = 2)
  rdtdebut <- round(listAgriITKetX[[c(i,2)]], digits = 2)
  rdtreel <- round(rdtReel[i], digits = 1)
  noteITKrdt <-  round(calculTxNoteITKrdt()[i], digits = 2)
  noteITKmouche <- round(calculTxNoteITKmouche()[i], digits = 2)
  qualite <-  round(calculqualite()[i])
  tour <- rep(1.2, each = length(listAgriITKetX))
  results_tours[nrow(results_tours) + 1,] = c(agri, mouche,rdtOptimaldata, rdtdebut, rdtreel, noteITKrdt, noteITKmouche, qualite,tour[i])
}

results_tours
write.xlsx(x = results_tours, file = "Points_SessionNiayes1.xlsx",sheet = "Tour 1.tar",append=TRUE)
for (i in 1:length(listAgriITKetX)) { 
  agri <- i
  choix_cartes <- c(itk$abreviation_mo) %in% listAgriITKetX[[i]]$itk
  cartes_tours[nrow(cartes_tours) + 1, 1] <- agri
  if (any(choix_cartes)) {
    cartes_tours[nrow(cartes_tours), 2:17] <- (ifelse(choix_cartes, 1, 0))
  }
}
cartes_tours
write.xlsx(x = cartes_tours, file = "Cartes_SessionNiayes1.xlsx",sheet = "Tour 1.tar",append=TRUE)
# ---TOUR 2 RECOLTE PRECOCE-------------------------------------------------------------------------
bloc03 <- {
  niveauDeDifficulte <- 3
  tauxDePertesBD <- 0.5
  rdtOptimal <- c(20, 25,60, 75)
  ag01 <- list(
    itk = c("lab","fer","pre","prc","pre","lbe"), 
    X1_depart = 16
  )
  ag02 <- list(
    itk = c("pre","tai","lab","lbn","dsb","pba","pma","lbe","lbf","bio"), 
    X2_depart = 25
  )
  ag03 <- list(
    itk = c("pba","pba","tai","lab","lbn","dsb","pre","pre","dsb"), 
    X3_depart = 48
  )
  ag04 <- list(
    itk = c("lab","lbn","fer"), 
    X4_depart = 9
  )
  listAgriITKetX <- list(ag01 = ag01, ag02 = ag02, ag03 = ag03, ag04 = ag04)
}
#afficher points de départs
sapply(listAgriITKetX, "[[", 2)
cat(verifNumPoints(listAgriITKetX)) # vérif nombre de points
tourDeJeu <- calculRdt() # rdt en fin de tour et nouveau taux de pertes BD
for (i in 1:length(listAgriITKetX)){
  agri <- i
  mouche <- tauxDePertesBD
  rdtOptimaldata <- round(rdtOptimal[i], digits = 2)
  rdtdebut <- round(listAgriITKetX[[c(i,2)]], digits = 2)
  rdtreel <- round(rdtReel[i], digits = 1)
  noteITKrdt <-  round(calculTxNoteITKrdt()[i], digits = 2)
  noteITKmouche <- round(calculTxNoteITKmouche()[i], digits = 2)
  qualite <-  round(calculqualite()[i])
  tour <- rep(2.1, each = length(listAgriITKetX))
  results_tours[nrow(results_tours) + 1,] = c(agri,mouche, rdtOptimaldata, rdtdebut, rdtreel, noteITKrdt, noteITKmouche, qualite,tour)
}

results_tours
write.xlsx(x = results_tours, file = "Points_SessionNiayes1.xlsx",sheet = "Tour2.pre",append=TRUE)
for (i in 1:length(listAgriITKetX)) { 
  agri <- i
  choix_cartes <- c(itk$abreviation_mo) %in% listAgriITKetX[[i]]$itk
  cartes_tours[nrow(cartes_tours) + 1, 1] <- agri
  if (any(choix_cartes)) {
    cartes_tours[nrow(cartes_tours), 2:17] <- (ifelse(choix_cartes, 1, 0))
  }
}
cartes_tours
write.xlsx(x = cartes_tours, file = "Cartes_SessionNiayes1.xlsx",sheet = "Tour 2.pre",append=TRUE)
# ---TOUR 2 RECOLTE tardive-------------------------------------------------------------------------
bloc03 <- {
  niveauDeDifficulte <- 3
  tauxDePertesBD <- 0.3
  rdtOptimal <- c(20, 25,60, 75)
  ag01 <- list(
    itk = c("lab","fer","pre","prc","pre","irr","fer","dsb"), 
    X1_depart = 16
  )
  ag02 <- list(
    itk = c("pre","tai","lab","lbn","dsb","pba","pma","lbe","lbf","bio"), 
    X2_depart = 4
  )
  ag03 <- list(
    itk = c("lbe","tai","lab","lbn","dsb","pre","pre","dsb"), 
    X3_depart = 48
  )
  ag04 <- list(
    itk = c("lab","lbn","fer"), 
    X4_depart = 9
  )
  listAgriITKetX <- list(ag01 = ag01, ag02 = ag02, ag03 = ag03, ag04 = ag04)
}
#afficher points de départs
sapply(listAgriITKetX, "[[", 2)
cat(verifNumPoints(listAgriITKetX)) # vérif nombre de points
tourDeJeu <- calculRdt() # rdt en fin de tour et nouveau taux de pertes BD
for (i in 1:length(listAgriITKetX)){
  agri <- i
  mouche <- tauxDePertesBD
  tauxDePertesBD <- tauxDePertesBD
  rdtOptimaldata <- round(rdtOptimal[i], digits = 2)
  rdtdebut <- round(listAgriITKetX[[c(i,2)]], digits = 2)
  rdtreel <- round(rdtReel[i], digits = 1)
  noteITKrdt <-  round(calculTxNoteITKrdt()[i], digits = 2)
  noteITKmouche <- round(calculTxNoteITKmouche()[i], digits = 2)
  qualite <-  round(calculqualite()[i])
  tour <- rep(2.2, each = length(listAgriITKetX))
  results_tours[nrow(results_tours) + 1,] = c(agri, mouche,rdtOptimaldata, rdtdebut, rdtreel, noteITKrdt, noteITKmouche, qualite,tour)
}

results_tours
write.xlsx(x = results_tours, file = "Points_SessionNiayes1.xlsx",sheet = "Tour2.tar",append=TRUE)
for (i in 1:length(listAgriITKetX)) { 
  agri <- i
  choix_cartes <- c(itk$abreviation_mo) %in% listAgriITKetX[[i]]$itk
  cartes_tours[nrow(cartes_tours) + 1, 1] <- agri
  if (any(choix_cartes)) {
    cartes_tours[nrow(cartes_tours), 2:17] <- (ifelse(choix_cartes, 1, 0))
  }
}
cartes_tours
write.xlsx(x = cartes_tours, file = "Cartes_SessionNiayes1.xlsx",sheet = "Tour 2.tar",append=TRUE)



# # la meilleure note technique en utilisant toutes les pratiques : 
# bestITKnote <- sum(itk$impactbdsurdt[itk$impactbdsurdt > 0])
# # la note d'un agriculteur
# myITKnote <- get_itk_score(monTK = listAgriITKetX[[c(i,1)]])
# # le rapport entre la note agri et la meilleure note
# tauxMyITKnote <- myITKnote/bestITKnote
# # rdt individuel si rien n'est fait
# rdtOptimal[i] - rdtOptimal[i]*tauxDePertesBD
# # rdt individuel avec les pratiques
# rdtOptimal[i] - rdtOptimal[i]*(tauxDePertesBD - tauxDePertesBD*tauxMyITKnote)

