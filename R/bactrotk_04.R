# -----------------------------------------------------------------------------
# ESSAI N°4 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
#   agricoles (14.03.2023) Audrey N'Gom et François Rebaudo
# -----------------------------------------------------------------------------
setwd("./R")

# --- 1. INITIALISATION DU JEU ------------------------------------------------
bloc00 <- {
niveauDeDifficulte <- 4
tauxDePertesBD <- 0.1
rdtOptimal <- c(20, 20, 20, 20)
ag01 <- list(
    itk = c("pre", "pma"), 
    X1_depart = rdtOptimal[1] - rdtOptimal[1]*tauxDePertesBD
)
ag02 <- list(
    itk = c("irr","pba"), 
    X2_depart = rdtOptimal[2] - rdtOptimal[2]*tauxDePertesBD
)
ag03 <- list(
    itk = c("pba","bio"), 
    X3_depart = rdtOptimal[3] - rdtOptimal[3]*tauxDePertesBD
)
ag04 <- list(
    itk = c("lbn","dsb"), 
    X4_depart = rdtOptimal[4] - rdtOptimal[4]*tauxDePertesBD
)
listAgriITKetX <- list(ag01, ag02, ag03, ag04)
listAgriITKetX #afficher points
}
# -----------------------------------------------------------------------------
# --- 2. CHARGEMENT DES DONNEES ET FONCTIONS ----------------------------------
bloc01 <- {
  
# Chargement de la liste des pratiques culturales
itk <- read.csv(file = "testitk29.03.2023.csv", dec = ",", header = TRUE) 
# la meilleure note technique en utilisant la moitié, le quart...des pratiques  #prise en compte de la fréquence
bestITKmouchenote <- sum(itk$impactmosurbd[itk$impactmosurbd > 0]*itk$Nb.de.fois.possible[itk$impactmosurbd > 0]) / niveauDeDifficulte 
  
#bestITKrdtnote <- sum(itk$impactmosurrdt[itk$impactmosurrdt > 0])  #prise en compte de la fréquence
bestITKrdtnote <- sum(itk$impactmosurrdt[itk$impactmosurrdt > 0]*itk$Nb.de.fois.possible[itk$impactmosurrdt > 0]) / 2
  
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
get_itkrendement_score <- function(monTK){ 
  scorei <- sapply(seq_along(monTK), function(i){
    score <- itk[itk$modalite.abr == monTK[i],]$impactmosurrdt
    if(length(score) == 0){score <- NA}
    return(score)
  })
  return(sum(scorei))
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

calculTxNoteITKmouche <- function(){
  sapply(seq_along(listAgriITKetX), function(i){
    tauxMyITKnote <- get_itkmouche_score(monTK = listAgriITKetX[[c(i,1)]])/bestITKmouchenote
  })
}
calculTxNoteITKrdt <- function(){
  sapply(seq_along(listAgriITKetX), function(i){
    tauxMyITKnoterdt <- get_itkrendement_score(monTK = listAgriITKetX[[c(i,1)]])/bestITKrdtnote
  })
}
# calcul du rdt avec le taux de note ITK et incidence BD
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
    if (tauxDePertesBD > 0.15) {
      if (calculTxNoteITKmouche()[i]<0.5) {
        rdtReel <-rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD+0.15) 
      }else if(calculTxNoteITKmouche()[i]>=1){ 
        rdtReel <-rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD-0.15)
      }else{ 
        rdtReel <-rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD+0.10)
      } 
      if (calculTxNoteITKrdt()[i]>=1) {
        rdtReel <-rdtReel +  3
      } 
    }else{
      if(calculTxNoteITKrdt()[i]>=1) {
        rdtReel<- rdtOptimal[i]
      }else if (calculTxNoteITKrdt()[i]<0.7) {
        rdtReel<- rdtOptimal[i]*0.75
      }else{
        rdtReel <- rdtOptimal[i]*calculTxNoteITKrdt()[i]
      }  
    }
    msg <- paste0(
      "Agri", i, 
      ": rdt optimal: ", round(rdtOptimal[i], digits = 2),
      " ; rdt début tour: ", round(listAgriITKetX[[c(i,2)]], digits = 2),
      " ; note ITK rdt: ", round(calculTxNoteITKrdt()[i], digits = 2), 
      " ; note ITK mouche: ", round(calculTxNoteITKmouche()[i], digits = 2), 
      " ; rdt réel: ", round(floor(rdtReel), digits = 2),
      "\n"
    )
    cat(msg)
    return(floor(rdtReel))
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
listAgriITKetX[[1]][[1]] <- c("pro_coin","pro_coin", "appat_gf","piege_mala", "lb_nid","taille","appat_gf")
listAgriITKetX[[2]][[1]] <- c( "pro_coin","pro_coin", "appat_gf","piege_mala", "lb_nid","taille","appat_gf")
listAgriITKetX[[3]][[1]] <- c("pro_coin","pro_coin", "appat_gf","piege_mala", "lb_nid","taille","appat_gf")
listAgriITKetX[[4]][[1]] <- c( "pro_coin","pro_coin", "appat_gf","piege_mala", "lb_nid","taille","appat_gf")

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

