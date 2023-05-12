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

itk <- read.csv(file = "EXT_pratiques04052023.csv", dec = ",", header = TRUE)

# la meilleure note technique en réalisant les pratiques de luttes directes et indirectes en fonction du niveau de difficulté
bestITKmouchenote <- sum(itk$gestionlutte[itk$gestionlutte > 0]*itk$frequence[itk$gestionlutte > 0]) / niveauDeDifficulte 
#la meilleure note technique en réalisant les pratiques d'optimisation de rendement
bestITKrdtnote <- sum(itk$gestionrdt[itk$gestionrdt>0]*itk$frequence[itk$gestionrdt>0])

