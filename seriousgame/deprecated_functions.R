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


storeInitAndCardChoiceToFile <- function(){
  cards <- sapply(listAgriITKetX, "[[", 1)
  sink(file = myFile, append = TRUE)
  print(Sys.time())
  cat("\nINITIALISATION ------------------------------------------------------\n")
  cat("Niveau de difficulté :", niveauDeDifficulte, "\n")
  cat("Taux de perte :", tauxDePertesBD, "\n")
  cat("Rendement potentiel :", rdtOptimal, "\n")
  cat("---------------------------------------------------------------------\n")
  print(cards)
  cat(verifNumPoints(listAgriITKetX))
  cat("\n\n")
  sink()
}


storeResultsTour <- function(){
  results_tours <- do.call(rbind, lapply(seq_along(listAgriITKetX), function(i){ #conserver résultats
    agri <- paste0("Agri", i)
    mouche <- tauxDePertesBD
    rdtOptimaldata <- round(rdtOptimal[i], digits = 2)
    rdtdebut <- round(listAgriITKetX[[c(i,2)]], digits = 2)
    rdtreel <- round(tourDeJeu[[2]][i], digits = 1)
    noteITKrdt <-  round(calculTxNoteITKrdt()[i], digits = 2)
    noteITKmouche <- round(calculTxNoteITKmouche()[i], digits = 2)
    qualite <-  round(calculqualite()[i])
    tour <- 1.1
    return(c(agri, mouche, rdtOptimaldata, rdtdebut, rdtreel, noteITKrdt, noteITKmouche, qualite,tour))
  }))
  colnames(results_tours) <- c(
    "agri", "mouche", "rdtOptimaldata", "rdtdebut", "rdtreel", "noteITKrdt", 
    "noteITKmouche", "qualite","tour"
  )
  write.table(x = results_tours, file = myFile, append = TRUE, row.names = FALSE)
}
