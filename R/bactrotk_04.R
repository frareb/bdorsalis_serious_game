# -----------------------------------------------------------------------------
# ESSAI N°4 : Impacts Bactrocera dorsalis en fonction des pratiques/luttes 
#   agricoles (14.03.2023) Audrey N'Gom et François Rebaudo
# -----------------------------------------------------------------------------
setwd("./R")

# --- 1. INITIALISATION DU JEU ------------------------------------------------
niveauDeDifficulte <- 2
tauxDePertesBD <- 0.4
rdtOptimal <- c(30, 30, 30, 30)
ag01 <- list(
  itk = c("irr","ferti", "taille","labour", "lb_nid", "pro_effic","lb_fopius"), 
  X1_depart = rdtOptimal[1] - rdtOptimal[1]*tauxDePertesBD
)
ag02 <- list(
  itk = c("irr","piege_mala","appat_gf", "dsb","insect"), 
  X2_depart = rdtOptimal[2] - rdtOptimal[2]*tauxDePertesBD
)
ag03 <- list(
  itk = c("insect","piege_mala","appat_gf","ferti","taille","dsb"), 
  X3_depart = rdtOptimal[3] - rdtOptimal[3]*tauxDePertesBD
)
ag04 <- list(
  itk = c("labour","dsb", "irr"), 
  X4_depart = rdtOptimal[4] - rdtOptimal[4]*tauxDePertesBD
)
listAgriITKetX <- list(ag01, ag02, ag03, ag04)
# -----------------------------------------------------------------------------

# --- 2. CHARGEMENT DES DONNEES ET FONCTIONS ----------------------------------
bloc01 <- {

# Chargement de la liste des pratiques culturales
itk <- read.csv(file = "pratiques23022023.csv", dec = ",", header = TRUE) 
# la meilleure note technique en utilisant toutes les pratiques. Ici je mets
# la meilleure note sur 2 de façon à avoir un score supérieur à 1 si les 
# agris font au moins la moitié des bonnes pratiques
bestITKnote <- sum(itk$impactbdsurdt[itk$impactbdsurdt > 0]) / niveauDeDifficulte

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
get_itk_score <- function(monTK){ 
  scorei <- sapply(seq_along(monTK), function(i){
    score <- itk[itk$modalite.abr == monTK[i],]$impactbdsurdt
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

calculTxNoteITK <- function(){
  sapply(seq_along(listAgriITKetX), function(i){
    tauxMyITKnote <- get_itk_score(monTK = listAgriITKetX[[c(i,1)]])/bestITKnote
  })
}

# calcul du rdt avec le taux de note ITK et incidence BD
calculRdt <- function(){
  if( sum(calculTxNoteITK()>=1)>=(0.5*(length(listAgriITKetX)))){ #(mean(calculTxNoteITK()) > 1) : ne permet de renvoyer si la plupart ont bien réussi 
    newTauxDePertesBD <- tauxDePertesBD - tauxDePertesBD*0.1 
  } else {
    newTauxDePertesBD <- tauxDePertesBD + tauxDePertesBD*0.1
  }
  msg <- paste0("Moy. note ITK: ", round(mean(calculTxNoteITK()), digits = 2), "\n",
    "Taux de pertes BD initial: ", round(tauxDePertesBD, digits= 2), 
    ". Nouveau taux : ", 
    newTauxDePertesBD, ".\n"
  )
  cat(msg)
  tauxDePertesBD <- newTauxDePertesBD
  if(tauxDePertesBD > 1){tauxDePertesBD <- 1}
  rdtReel <- sapply(seq_along(listAgriITKetX), function(i){
    if (calculTxNoteITK()[i]<0.5) {
      rdtReel <-rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD+0.15) 
    }else if(calculTxNoteITK()[i]>=1){ 
      rdtReel <-rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD-0.20)
    }else{ 
      rdtReel <-rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD+0.10)
    } 
    msg <- paste0(
      "Agri", i, 
      ": rdt optimal: ", round(rdtOptimal[i], digits = 2),
      " ; rdt début tour: ", round(listAgriITKetX[[c(i,2)]], digits = 2),
      " ; note ITK: ", round(calculTxNoteITK()[i], digits = 2), 
      " ; rdt réel: ", round(rdtReel, digits = 2),
      "\n"
    )
    cat(msg)
    return(rdtReel)
  })
  return(list(tauxDePertesBD, rdtReel))
}

#modifier rdtReel : rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD + tauxDePertesBD*calculTxNoteITK()[i]) 
# car renvoie rendement plus élevé pour tous
#idee 1 : rdtReel <- rdtOptimal[i] - rdtOptimal[i]*(tauxDePertesBD) * calculTxNoteITK()[i] 
#ne fonctionne pas
#On a : rdtOptimal[i] -  rdtOptimal[i]*(tauxDePertesBD)
#On veut le moduler en fonction de la note des agri (calculTxNoteITK)


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
listAgriITKetX[[1]][[1]] <- c("ferti","lb_nid","taille","dsb")
listAgriITKetX[[2]][[1]] <- c( "taille","ferti","lb_nid","dsb")
listAgriITKetX[[3]][[1]] <- c("appat_gf","ferti","lb_nid","dsb")
listAgriITKetX[[4]][[1]] <- c(  "lb_nid")

# --- 4. CHANGEMENTS ANNEXES AU COURS DU JEU --------------------------------------------------------
niveauDeDifficulte <- 2 #nv1 : les agri doivent faire beaucoup de pratiques
#nv2 : les agri doivent au moins faire la moitié des pratiques
#nv3 : les agri doivent faire au moins un tiers des pratiques


# scoreindiv <- rep(NA, length(listAgriITKetX))
# a <- rep(NA, length(listAgriITKetX))
# scorefinal_indivtest<- for (i in 1:length(listAgriITKetX)) {  #Resultat actions individuelles 
#   # ici tu compares un rendement théorique (tauxDePertesBD*rdtOptimal[i]) et 
#   # une note basée sur les pratiques agricoles. Pour moi ce n'est pas 
#   # comparable car pas dans les mêmes unités. Par exemple un agri1 peut avoir
#   # un rdt optimal de 100 et il n'arrivera jamais à une note supérieure à 100 ?
#   # Et un agri2 avec un rdt optimal de 2 arrivera toujours avec une note 
#   # supérieure à 2 et donc verra son taux de perte amoindri alors qu'il peut
#   # avoir des pratiques très mauvaises par rapport à l'agri1. Le calcul doit 
#   # être indépendant du rdtOptimal.
#   # il faudrait que tu calcules une note technique qui servent à ajuster le 
#   # tauxDePertesBD. J'ai mis un exemple sous la fonction.
#   if (get_itk_score(monTK = listAgriITKetX[[c(i,1)]])>= (tauxDePertesBD*rdtOptimal[i])) { # modifier 0.6*n[[c(i,2)]] par rdtOptimal[i]*tauxDePertesBD ? 
#     scoreindiv[i] <- rdtOptimal[i] - (rdtOptimal[i])* (tauxDePertesBD-0.1) 
#     print (scoreindiv[i])
#     print ("Pratiques vertueuses") #positif
#     a[i] <- 1
#     print(a[i])
#   } else {
#     scoreindiv[i] <-rdtOptimal[i] - (rdtOptimal[i])* (tauxDePertesBD) 
#     print (scoreindiv[i]) #négatif
#     print ("Pratiques pas assez efficaces")
#     a[i] <- -1
#     print(a[i])
#   }
#   as <- sum(a) #sum(rep(a[i], times = length(listAgriITKetX)))
# } 
# 
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


######### ETAPE 3 : RESULTATS COLLECTIFS
# for (i in 1:length(listAgriITKetX)) {
#   if (as>=0) {
#     listAgriITKetX[[c(i,2)]] <- scoreindiv[i]
#   }else{
#     listAgriITKetX[[c(i,2)]] <- listAgriITKetX[[c(i,2)]]
#   }
# }
# listAgriITKetX
# if (as >= 0) {
#   print("Bravo, les pertes liés à la mouche ont baissé de 10%")  #réfléchir comment la mouche diminue d'un tour à l'autre #module de dispersion
#   tauxDePertesBD <- tauxDePertesBD-0.1
#   }else{
#   print ("Mince, le taux de pertes n'a pas diminué cette saison ")
#   }
######### ETAPE 4 : NOUVEAUX  CHOIX DE CARTE POUR PARTIE SUIVANTE
# listAgriITKetX[[1]][[1]] <- c("lb_nid", "lb_nid","lb_nid")
# listAgriITKetX[[2]][[1]] <- c("dsb","lb_nid", "lb_nid","lb_nid")
# listAgriITKetX[[3]][[1]] <- c("dsb")
# listAgriITKetX[[4]][[1]] <- c("dsb","lb_nid", "lb_nid","lb_nid")
######### ETAPE 5 : retour ETAPE 2

