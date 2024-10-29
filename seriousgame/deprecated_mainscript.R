
# -----------------------------------------------------------------------------
setwd("./seriousgame")
myFile <- paste0(Sys.time(), "_session1.txt")
# --- 1. INITIALISATION DU JEU ------------------------------------------------
source("00_init.R")
# --- 2. CHARGEMENT DES FONCTIONS ---------------------------------------------
source("functions.R")
# --- 3. BOUCLE DU JEU --------------------------------------------------------
## --- TOUR 1 RECOLTE PRECOCE
cat(verifNumPoints(listAgriITKetX)) # vérif nombre de points
storeInitAndCardChoiceToFile()
tourDeJeu <- calculRdt() # rdt en fin de tour et nouveau taux de pertes BD
storeResultsTour()
## --- TOUR 1 RECOLTE TARDIVE.-------------------------------------------------
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

