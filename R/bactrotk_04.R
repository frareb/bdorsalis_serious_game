# -----------------------------------------------------------------------------
# ESSAI N°4 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
#   agricoles (14.03.2023) Audrey N'Gom et François Rebaudo
# -----------------------------------------------------------------------------
setwd("./R")

# --- 1. INITIALISATION DU JEU ------------------------------------------------
bloc00 <- {
  niveauDeDifficulte <- 3
  tauxDePertesBD <- 0.7
  rdtOptimal <- c(20, 20, 20, 20)
  ag01 <- list(
    itk = c("irr","tai","fer","dsb", "ins","pma","apg","pre" ), 
    X1_depart = rdtOptimal[1] - rdtOptimal[1]*tauxDePertesBD
  )
  ag02 <- list(
    itk = c("lab","prc","irr"), 
    X2_depart = rdtOptimal[2] - rdtOptimal[2]*tauxDePertesBD
  )
  ag03 <- list(
    itk = c("bio","pba","lbn","tai"), 
    X3_depart = rdtOptimal[3] - rdtOptimal[3]*tauxDePertesBD
  )
  ag04 <- list(
    itk = c("lab","irr"), 
    X4_depart = rdtOptimal[4] - rdtOptimal[4]*tauxDePertesBD
  )
  listAgriITKetX <- list(ag01, ag02, ag03, ag04)
}
#afficher points de départs
for (i in 1:length(listAgriITKetX)) { print (listAgriITKetX[[c(i,2)]])} 
# -----------------------------------------------------------------------------
# --- 2. CHARGEMENT DES DONNEES ET FONCTIONS ----------------------------------
bloc01 <- {
  
# Chargement de la liste des pratiques culturales
itk <- read.csv(file = "testitk29.03.2023.csv", dec = ",", header = TRUE) 
# la meilleure note technique en réalisant les pratiques de luttes directes et indirectes en fonction du niveau de difficulté
bestITKmouchenote <- sum(itk$impactmosurbd[itk$impactmosurbd > 0]*itk$Nb.de.fois.possible[itk$impactmosurbd > 0]) / niveauDeDifficulte 
# la meilleure note technique en réalisant les pratiques d'optimisation de rendement
bestITKrdtnote <- sum(itk$impactmosurrdt[itk$impactmosurrdt>0]*itk$Nb.de.fois.possible[itk$impactmosurrdt>0])
  
# Somme des points requis des cartes jouées
get_RN_score <- function(ressncrs){ 
  scorei <- sapply(seq_along(ressncrs), function(i){
    score <- itk[itk$modalite.abr == ressncrs[i],]$ressources.ncsr
    if(length(score) == 0){score <- NA}
    return(score)
  })
  return(sum(scorei))
}
  
# Somme de l'impact des pratiques par rapport à BD sur rendement :  si positif,
#   choix de la pratique efficace contre prévalence mouche
get_itkmouche_score <- function(monTK){ 
  scorei <- sapply(seq_along(monTK), function(i){
    score <- itk[itk$modalite.abr == monTK[i],]$impactmosurbd
    if(length(score) == 0){score <- NA}
    return(score)
  })
  return(sum(scorei))
}
# Somme de l'impact des pratiques par rapport au rendement 
get_itkrendement_score <- function(monTK1){ 
  rscorei <- sapply(seq_along(monTK1), function(i){
    rscore <- itk[itk$modalite.abr == monTK1[i],]$impactmosurrdt
    if(length(rscore) == 0){rscore <- NA}
    return(rscore)
  })
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

#Calcul de la qualité des mangues
calculqualite <- function(){
  sapply(seq_along(listAgriITKetX), function(i){
    if (tauxDePertesBD>0.20) {
      if (calculTxNoteITKmouche()[i]>0 & calculTxNoteITKmouche()[i]<2 ) {
        qualite <- abs(1-(tauxDePertesBD+tauxDePertesBD* (1-calculTxNoteITKmouche()[i])))*10
      }else if (calculTxNoteITKmouche()[i]<=0) {
        qualite <- (1-0.9)*10   
      }else if (calculTxNoteITKmouche()[i]>=2) {
        qualite <- 10
      }
    }else{
      qualite <- 8 #à modifier  
    }
  })
}
# Calcul nouveau taux de perte et rdt avec le taux de note ITK et incidence BD
calculRdt <- function(){
  if( sum(calculTxNoteITKmouche()>=1)>=(0.5*(length(listAgriITKetX)))){ 
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
  tauxDePertesBD <- newTauxDePertesBD
  if(tauxDePertesBD > 1){tauxDePertesBD <- 1}
  rdtReel <- sapply(seq_along(listAgriITKetX), function(i){
    if (tauxDePertesBD>0.20){
      if (tauxDePertesBD>0.75){
        rdtReel <- 0.5*rdtOptimal[i] + ((0.5*rdtOptimal[i])* calculTxNoteITKrdt()[i])-(0.5*rdtOptimal[i])*0.95
      }else if(calculTxNoteITKrdt()[i]<=0){
        rdtReel <- 0.5*rdtOptimal[i] - (0.5*rdtOptimal[i])*(tauxDePertesBD+tauxDePertesBD* (1-calculTxNoteITKmouche()[i]))
      }else if(calculTxNoteITKmouche()[i]<=0){
        rdtReel <- 0.5*rdtOptimal[i] + ((0.5*rdtOptimal[i])* calculTxNoteITKrdt()[i])-(0.5*rdtOptimal[i])*0.95
      }else{
        rdtReel <- 0.5*rdtOptimal[i] + ((0.5*rdtOptimal[i])* calculTxNoteITKrdt()[i])-(0.5*rdtOptimal[i])*(tauxDePertesBD+tauxDePertesBD* (1-calculTxNoteITKmouche()[i])) #appliquer taux de perte à tout rdt optimal ou à 50%?
      }
   }else{
        rdtReel <- 0.5*rdtOptimal[i] + ((0.5*rdtOptimal[i])* calculTxNoteITKrdt()[i])
      }
   if (rdtReel > rdtOptimal [i]){rdtReel <- rdtOptimal[i]} 
   if (rdtReel <0) {rdtReel <- 0} 
   msg <- paste0(
        "Agri", i, 
        ": rdt optimal: ", round(rdtOptimal[i], digits = 2),
        " ; rdt début : ", round(listAgriITKetX[[c(i,2)]], digits = 2),
        " ; rdt réel: ", round(rdtReel, digits = 2),
        " ; note ITK rdt: ", round(calculTxNoteITKrdt()[i], digits = 2), 
        " ; note ITK mouche: ", round(calculTxNoteITKmouche()[i], digits = 2), 
        " ; qualité: ",round(calculqualite()[i]),
        "\n"
      )
   cat(msg)
   return(rdtReel)
  })
    return(list(tauxDePertesBD, rdtReel))
  }
  
}
# --- 3. BOUCLE DU JEU --------------------------------------------------------
listAgriITKetX #afficher points
cat(verifNumPoints(listAgriITKetX)) # vérif nombre de points
tourDeJeu <- calculRdt() # rdt en fin de tour et nouveau taux de pertes BD
tauxDePertesBD <- tourDeJeu[[1]]
listAgriITKetX <- lapply(seq_along(listAgriITKetX), function(i){
  listAgriITKetX[[i]][[2]] <- tourDeJeu[[2]][i]
  return(listAgriITKetX[[i]])
})
listAgriITKetX[[1]][[1]] <- c("lbe","tai","pre","apg","fer")
listAgriITKetX[[2]][[1]] <- c("fer","tai")
listAgriITKetX[[3]][[1]] <- c("apg","tai","pre","apg","fer","tai","dsb")
listAgriITKetX[[4]][[1]] <- c("tai","fer","dsb","lbn","tai","fer","dsb","pre","fer","apg","pre")
# --- 4. CHANGEMENTS ANNEXES AU COURS DU JEU --------------------------------------------------------
niveauDeDifficulte <- 2 #nv1 : les agri doivent faire beaucoup de pratiques
#nv2 : les agri doivent au moins faire la moitié des pratiques
#nv3 : les agri doivent faire au moins un tiers des pratiques



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

