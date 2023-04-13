
itk <- read.csv(file = "./R/testitk29.03.2023.csv", dec = ",", header = TRUE)
get_itkmouche_score <- function(monTK){ 
  scorei <- sapply(seq_along(monTK), function(i){
    score <- itk[itk$modalite.abr == monTK[i],]$impactmosurbd
    if(length(score) == 0){score <- NA}
    return(score)
  })
  if((is.null(scorei)) | (length(scorei) == 0)){scorei <- 0}
  return(sum(scorei))
}
get_itkrendement_score <- function(monTK1){ 
  rscorei <- sapply(seq_along(monTK1), function(i){
    rscore <- itk[itk$modalite.abr == monTK1[i],]$impactmosurrdt
    if(length(rscore) == 0){rscore <- NA}
    return(rscore)
  })
  if((is.null(rscorei)) | (length(rscorei) == 0)){rscorei <- 0}
  return(sum(rscorei))
}
calculTxNoteITKmouche <- function(){
  sapply(seq_along(listAgriITKetX), function(i){
    tauxMyITKnote <- get_itkmouche_score(monTK = listAgriITKetX[[c(i,1)]])/bestITKmouchenote
  })
}
calculTxNoteITKrdt <- function(){
  sapply(seq_along(listAgriITKetX), function(i){
    tauxMyITKnoterdt <- get_itkrendement_score(monTK1 = listAgriITKetX[[c(i,1)]])/bestITKrdtnote
  })
}
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
      qualite <- 7 + calculTxNoteITKmouche()[i]*3  #à modifier  
    }
  })
}
calculRdt <- function(){
  if( sum(calculTxNoteITKmouche()>=1)>=(0.5*(length(listAgriITKetX)))){ 
    newTauxDePertesBD <- tauxDePertesBD - tauxDePertesBD*0.1 
  } else {
    newTauxDePertesBD <- tauxDePertesBD + tauxDePertesBD*0.1
  }
  # msg <- paste0("Moy. note ITK: ", round(mean(calculTxNoteITKmouche()), digits = 2), "\n",
  #               "Taux de pertes BD initial: ", round(tauxDePertesBD, digits= 2), 
  #               ". Nouveau taux : ", 
  #               newTauxDePertesBD, ".\n"
  # )
  # cat(msg)
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
    # msg <- paste0(
    #   "Agri", i, 
    #   ": rdt optimal: ", round(rdtOptimal[i], digits = 2),
    #   " ; rdt début : ", round(listAgriITKetX[[c(i,2)]], digits = 2),
    #   " ; rdt réel: ", round(rdtReel, digits = 2),
    #   " ; note ITK rdt: ", round(calculTxNoteITKrdt()[i], digits = 2), 
    #   " ; note ITK mouche: ", round(calculTxNoteITKmouche()[i], digits = 2), 
    #   " ; qualité: ",round(calculqualite()[i]),
    #   "\n"
    # )
    # cat(msg)
    return(rdtReel)
  })
  return(list(tauxDePertesBD, rdtReel))
}

numGameRounds <- 10 # nombre de parties

dashboardGame <- list()

for(myRound in 1:numGameRounds){
  numPlayers <- sample(x = 4:10, size = 1) # nombre de joueurs
  numRounds <- 20 # nombre de tours par partie
  niveauDeDifficulte <- sample(x = 1:3, size = 1) # niveau de difficulté
  tauxDePertesBD <- sample(x = seq(from = 0, to = 1, by = 0.01), size = 1) # tx de perte début de tour
  rdtOptimal <- sample(x = 30:80, size = numPlayers) # rendement potentiel de chaque agriculteur
  
  bestITKmouchenote <- sum(itk$impactmosurbd[itk$impactmosurbd > 0]*itk$Nb.de.fois.possible[itk$impactmosurbd > 0]) / niveauDeDifficulte 
  bestITKrdtnote <- sum(itk$impactmosurrdt[itk$impactmosurrdt>0]*itk$Nb.de.fois.possible[itk$impactmosurrdt>0])
  
  listAgriITKetX <- lapply(1:numPlayers, function(i){
    list(itk = c(), X_depart = rdtOptimal[i] - rdtOptimal[i]*tauxDePertesBD)
  })
  names(listAgriITKetX) <- paste0("agri", 1:numPlayers)
  
  for(i in 1:length(listAgriITKetX)){
    X_depart <- listAgriITKetX[[i]][[2]]
    while(X_depart > 0){
      itkCHoice <- sample(1:nrow(itk), size = 1)
      X_depart <- X_depart - itk[itkCHoice,"ressources.ncsr"]
      if(X_depart > 0){
        listAgriITKetX[[i]][[1]] <- c(listAgriITKetX[[i]][[1]], itk[itkCHoice,"modalite.abr"])
      } else {
        break
      }
    }
  }
  
  # loop
  recordScores <- data.frame()
  for(i in 1:numRounds){
    recordScores <- rbind(recordScores, sapply(listAgriITKetX, "[[", 2))
    tourDeJeu <- calculRdt() 
    tauxDePertesBD <- tourDeJeu[[1]]
    listAgriITKetX <- lapply(seq_along(listAgriITKetX), function(i){
      listAgriITKetX[[i]] <- list(itk = c(), X_depart = tourDeJeu[[2]][i])
      return(listAgriITKetX[[i]])
    })
    for(i in 1:length(listAgriITKetX)){
      X_depart <- listAgriITKetX[[i]][[2]]
      while(X_depart > 0){
        itkCHoice <- sample(1:nrow(itk), size = 1)
        X_depart <- X_depart - itk[itkCHoice,"ressources.ncsr"]
        if(X_depart > 0){
          listAgriITKetX[[i]][[1]] <- c(listAgriITKetX[[i]][[1]], itk[itkCHoice,"modalite.abr"])
        } else {
          break
        }
      }
    }
  }
  # dashboard
  dashboardGame <- append(dashboardGame, list(recordScores))
}

pdf("dashboardGame.pdf", height = 6, width = 8)
par(mar = c(2, 4, 1, 1))
trash <- lapply(dashboardGame, function(zz){
  plot(
    zz[,1], type = "o", lwd = 2, ylim = c(0, max(rdtOptimal)), 
    xlab = "Time", ylab = "Yield", axes = FALSE
  )
  axis(1, at = 1:numRounds) ; axis(2, las = 1)
  trash <- lapply(2:ncol(zz), function(i){
    points(zz[,i], type = "o", lwd = 2, col = i)
  })
})
dev.off()
