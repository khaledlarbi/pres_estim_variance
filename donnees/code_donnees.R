#Données pour colloque

#install.packages("icarus")

library("icarus")
library("data.table")


set.seed(2404)
N_psu <- 10000
n_psu <- 1000
nb_strate_psu <- 5
nom_strate_psu <- LETTERS[1:nb_strate_psu]
nb_var_cal <- 5
nb_ssu_per_psu <- 10
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
pop_data$z <- 3 + 2*pop_data$var_cal2 - pop_data$var_cal1*pop_data$var_cal4 + rnorm(nrow(pop_data), 0, 1)
boxplot(pop_data$y ~ pop_data$strate_psu) #On a bien des comportements différents selon la strate
boxplot(pop_data$z ~ pop_data$strate_psu) #On a bien des comportements différents selon la strate

#Calcul des marges
marges <- colSums(pop_data[, paste0("var_cal",1:nb_var_cal), with = F])
###Mise au format icarus
marges_table <- cbind(names(marges), "0", marges)

###############################
#####Echantillonnage###########
###############################
#SASSR(10%) dans chaque strate de psu
#SASSR(4) au sein de chaque psu


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



#Calage des données
echantillon$poids_apres_calage <- calibration(echantillon, marginMatrix = marges_table, colWeights = "poids_avant_calage")


#Enregistrement des tabes
info_ech <- echantillon[, .(id_psu, strate_psu, id_ssu,
                            pi_psu, pi_ssu_cond_psu,
                            poids_avant_calage, poids_apres_calage)]
var_ech_cal <- echantillon[, c("id_psu", "id_ssu", "poids_avant_calage",
                           colnames(echantillon)[startsWith(colnames(echantillon), "var_cal")]),
                       with = FALSE]

#On va renommer l'identifiant et permuter les lignes pour montrer l'intérêt de certains
#argument de la fonction gustave::define_variance_wrapper
var_interet <- echantillon[, .("identifiant" = id_ssu, y, z)]
var_interet <- var_interet[sample(nrow(var_interet))]



setDF(var_ech_cal)
setDF(info_ech)
setDF(var_interet)

save(info_ech, var_ech_cal, var_interet, file = "donnees.Rdata")




#Création du wrapper
library(gustave)


#Les variables auxiliaires doivent être renseignées sous la forme d'une matrice
var_aux <- var_ech_cal[, colnames(var_ech_cal)[startsWith(colnames(var_ech_cal), "var_cal")]]
#var_aux$eff <- 1
var_aux <- as.matrix(var_aux)

#Création 
cal <- res_cal(y = NULL, #y = NULL, res_cal va renvoyer une liste avec des éléments permettant de calculer les résidus
               x = var_aux, #Matrice des variables de calage par individu de l'échantillon
               w = var_ech_cal$poids_avant_calage, #Vecteur contenant les poids avant tirage
               id = var_ech_cal$id_ssu #Identifiant 
               )

#Tests
y_test <- matrix(rnorm(nrow(var_ech_cal)), ncol = 1)
rownames(y_test) <- var_ech_cal$id_ssu
r1 <- res_cal(y_test, precalc = cal)
#r2 <- res_cal(y_test[sample(nrow(y_test)),,drop = FALSE], precalc = cal)


# calcul_variance_srs_srs <- function(y, f_up, f_us, strate, 
#                                     up, calage, avec_calage = TRUE,
#                                     w_cal = NULL){
#   #Calcul des résidus
#   if(avec_calage){
#     if(is.null(w_cal)){
#       eps <- res_cal(y = y, precalc = calage)
#     } else {
#       w_cal <- w_cal[calage$id, , drop = FALSE]
#       g <- w_cal/calage$w
#       y_modif <- g*y
#       eps <- res_cal(y = y_modif, precalc = calage)
#     }
#   } else {
#     eps <- y
#   }
#   #eps <- y
#   #Calcul de l'estimation de la variance de l'estimateur du total des résidus
#   ### Calcul des totaux estimés au sein de chaque up
#   tot_par_up <- sum_by(y = eps, by = up, w = rep(1/f_us, nrow(eps)))
#   #tot_par_up2 <- sum_by(y = y, by = up, w = rep(1/f_us, nrow(eps)))
#   ### Nombre d'unités primaires dans l'échantillon
#   n_up <- length(unique(up))
#   ### Nombre d'unités primaires dans la population
#   N_up <- n_up/f_up #10 000 comme dans l'énoncé
#   
#   ### Strate pour chaque up 
#   strate_up <- unique(cbind(strate,up))
#   strate_up <- setNames(strate_up[,1], strate_up[,2]) #il s'agit d'un vecteur dont les noms sont
#   #des noms d'UP et les éléments, les strates correspondantes.
#   
#   ### Première partie 
#   v_a <- var_srs(y = tot_par_up, 
#                  pik = rep(f_up, nrow(tot_par_up)), 
#                  strata = strate_up[rownames(tot_par_up)])
#   # v_a2 <- var_srs(y = tot_par_up2, 
#   #                pik = rep(f_up, nrow(tot_par_up)), 
#   #                strata = strate_up[rownames(tot_par_up)])
#   # print(v_a)
#   # print(v_a2)
#   ### Deuxième partie 
#   #le tirage des SSU est indépendant d'une PSU à l'autre --> ~ tirage stratifié
#   v_b <- (N_up/n_up)*var_srs(y = eps, 
#                  pik = rep(f_us, nrow(eps)),
#                  strata = up)
#   print(v_b)
#   v <- v_a + v_b
#   return(v)
# }
# calcul_variance_srs_srs(y_test,
#                         0.1,
#                         0.4,
#                         echantillon$strate_psu,
#                         echantillon$id_psu,
#                         cal, 
#                         TRUE,
#                         poids_apres_calage)


calcul_variance_srs_srs <- function(y, f_up, f_us, strate, 
                                    up, calage){
  #Calcul des résidus
  eps <- res_cal(y = y, precalc = calage)
   
  #eps <- y
  #Calcul de l'estimation de la variance de l'estimateur du total des résidus
  ### Calcul des totaux estimés au sein de chaque up
  tot_par_up <- sum_by(y = eps, by = up, w = rep(1/f_us, nrow(eps)))
  ### Nombre d'unités primaires dans l'échantillon
  n_up <- length(unique(up))
  ### Nombre d'unités primaires dans la population
  N_up <- n_up/f_up #10 000 comme dans l'énoncé
  
  ### Strate pour chaque up 
  strate_up <- unique(cbind(strate,up))
  strate_up <- setNames(strate_up[,1], strate_up[,2]) #il s'agit d'un vecteur dont les noms sont
  #des noms d'UP et les éléments, les strates correspondantes.
  
  ### Première partie 
  v_a <- var_srs(y = tot_par_up, 
                 pik = rep(f_up, nrow(tot_par_up)), 
                 strata = strate_up[rownames(tot_par_up)])
 
  ### Deuxième partie 
  #le tirage des SSU est indépendant d'une PSU à l'autre --> ~ tirage stratifié
  v_b <- (N_up/n_up)*var_srs(y = eps, 
                             pik = rep(f_us, nrow(eps)),
                             strata = up)
  v <- v_a + v_b
  return(v)
}


poids_apres_calage <- matrix(echantillon$poids_apres_calage, ncol = 1)
rownames(poids_apres_calage) <- echantillon$id_ssu

#Vérification
calcul_variance_srs_srs(y_test,
                        0.1,
                        0.4,
                        echantillon$strate_psu,
                        echantillon$id_psu,
                        cal)



technical <- list()

technical$f_up <- 0.1
technical$f_us <- 0.4
technical$strate <- info_ech$strate_psu
technical$up <- info_ech$id_psu
technical$calage <- cal

# precision_estim <- define_variance_wrapper(
#   variance_function = calcul_variance_srs_srs,
#   reference_id = info_ech$id_ssu,
#   reference_weight = info_ech$poids_apres_calage,
#   default_id = "identifiant",
#   technical_data = technical,
#   technical_param = list("avec_calage" = TRUE, "w_cal" = NULL)
# )

precision_estim <- define_variance_wrapper(
  variance_function = calcul_variance_srs_srs,
  reference_id = info_ech$id_ssu,
  reference_weight = info_ech$poids_apres_calage,
  default_id = "identifiant",
  technical_data = technical
)

log_lin <- define_statistic_wrapper(
  statistic_function = function(y, weight){
    point <- log(sum(y*weight))
    if(point <= 0){
      stop("Non-positive total lead to an error while log.
           Please make sure that the total is greater that zero.")
    }
    lin <- y/sum(y*weight)
    list(point = point, lin = lin, metadata = list(n = length(y)))
  },
  arg_type = list(data = "y", weight = "weight")
)




precision_estim(data = var_interet, total(z))
precision_estim(data = var_interet, ratio(y,z))
precision_estim(data = var_interet, log_lin(z))
