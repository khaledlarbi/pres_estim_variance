#Données pour colloque

#install.packages("icarus")

library("icarus")
library("data.table")


set.seed(2404)
N_psu <- 10000 #
n_psu <- 1000
nb_strate_psu <- 5
nom_strate_psu <- LETTERS[1:nb_strate_psu]
nb_var_cal <- 5
nb_ssu_per_psu <- 10 #
N_ssu <- N_psu * nb_ssu_per_psu
n_ssu_per_psu <- 4

#Génération des variables selon une loi normale
var_cal <- matrix(rnorm(nb_var_cal*N_ssu), ncol = nb_var_cal)
var_cal <- data.table(var_cal)
colnames(var_cal) <- paste0("var_cal", 1:nb_var_cal)

#Génération des strates
strate_psu <- sample(nom_strate_psu, N_psu, replace = TRUE)
#La table pop_data contient les informations sur l'ensemble de la population.
#On échantillonnera dedans
pop_data <- data.table("id_psu" = paste0("psu", 1:N_psu),
                       "strate_psu" = strate_psu)
link_psu_ssu <- data.table("id_psu" = rep(pop_data$id, each = 10),
                           "id_ssu" = paste0("ssu", 1:N_ssu))


pop_data<- merge(pop_data,
                 link_psu_ssu,
                 by = "id_psu")
pop_data$id_ssu <- paste0("ssu", 1:N_ssu)
pop_data <- cbind(pop_data, var_cal)


#Variable d'intérêt 1/2
pop_data$y <- 100 + as.matrix(var_cal) %*% c(0.2,0.3,0.5,0.1,5) + 30*(pop_data$strate_psu == "A") + 
  15*((pop_data$strate_psu %in% c("B","C"))) + 3*(!(pop_data$strate_psu %in% c("A","B","C")))
pop_data$z <- 3 + (pop_data$y)^2 + rnorm(nrow(pop_data))
boxplot(pop_data$y ~ pop_data$strate_psu) #On a bien des comportements différents selon la strate
boxplot(pop_data$z ~ pop_data$strate_psu) #On a bien des comportements différents selon la strate

#Calcul des marges
marges <- colSums(pop_data[, paste0("var_cal",1:nb_var_cal), with = F])
#marges <- c(marges, c('eff' = nrow(pop_data)))
###Mise au format icarus

marges_table <- cbind(names(marges), "0", marges)
#summary(lm(data = pop_data, y ~ var_cal1 + var_cal2 +  var_cal3 +  var_cal4 +  var_cal5))
#summary(lm(data = pop_data, y ~ var_cal1 + var_cal2 +  var_cal3 +  var_cal4 +  var_cal5-1))


###############################
#####Echantillonnage###########
###############################
#SASSR(10%) dans chaque strate de psu
#SASSR(4) au sein de chaque psu











rm(list = setdiff(ls(), c("pop_data","n_psu","nb_strate_psu",
                          "n_ssu_per_psu", "N_psu", "nb_ssu_per_psu",
                          "marges_table")))



tirage <- function(pop_data, n_psu, N_psu,  nb_strate_psu, n_ssu_per_psu, nb_ssu_per_psu){
  
  sample_psu <- unique(pop_data[, .(strate_psu, id_psu)])
  sample_psu$tirage_psu <- runif(nrow(sample_psu))
  setorder(sample_psu, tirage_psu)
  sample_psu <- sample_psu[,lapply(.SD, function(x){head(unique(x), n = as.integer(n_psu/nb_strate_psu))}), by = strate_psu]
  
  #Vérification
  sample_psu[, .N, strate_psu]
  sample_psu_id <- sample_psu[, id_psu]
  
  
  sample_ssu <- unique(pop_data[id_psu %in% sample_psu_id, .(id_psu, id_ssu)])
  sample_ssu$tirage_ssu <- runif(nrow(sample_ssu))
  setorder(sample_ssu, tirage_ssu)
  sample_ssu <- sample_ssu[,lapply(.SD, function(x){head(unique(x), n = n_ssu_per_psu)}), by = id_psu]
  sample_ssu_id <- sample_ssu[, id_ssu]
  
  
  echantillon <- pop_data[id_ssu %in% sample_ssu_id]
  echantillon$pi_psu <- n_psu/N_psu
  echantillon$pi_ssu_cond_psu <- n_ssu_per_psu/nb_ssu_per_psu
  echantillon[, poids_avant_calage := 1/(pi_psu*pi_ssu_cond_psu)]
  echantillon[, eff := 1]
  echantillon$poids_apres_calage <- calibration(echantillon,
                                                marginMatrix = marges_table,
                                                colWeights = "poids_avant_calage",
                                                description = FALSE)
  
  return(echantillon)
}


simulation <- function(pop_data, 
                       n_psu, 
                       N_psu,
                       nb_strate_psu, 
                       n_ssu_per_psu,
                       nb_ssu_per_psu,
                       nb_sim = 1000L){
  res <- list()
  for(i in 1:nb_sim){
    donnees <- tirage(pop_data, 
                      n_psu, 
                      N_psu,
                      nb_strate_psu, 
                      n_ssu_per_psu,
                      nb_ssu_per_psu)
    res[[i]] <- c("v1" = sum(donnees$y * donnees$poids_apres_calage),
                     "v2" = sum(donnees$y * donnees$poids_apres_calage)/sum(donnees$z * donnees$poids_apres_calage),
                     "v3" = 1/sum(donnees$z * donnees$poids_apres_calage),
                  "v4" = log(sum(donnees$z * donnees$poids_apres_calage)))
    
  }
  return(res)
}


res_var <- simulation(pop_data, 
           n_psu, 
           N_psu,
           nb_strate_psu, 
           n_ssu_per_psu,
           nb_ssu_per_psu,
           nb_sim = 500L)


res <- Reduce(rbind, res_var) #différence mais pourquoi ? 
#problème strate ? 
apply(X = res, 2, var)



