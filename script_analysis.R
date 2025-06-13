#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#
gc()

# Chargement de la configuration
source("config.R")
source(file.path(functions_dir, "Functions_filtering.R")) 


# Définition de l'année d'analyse
YEAR <- 2024
TYPE <- "catlog" #Type de données d'entrée (CATLOG, OFB )
alpage <- "Sanguiniere"
alpages <- "Sanguiniere"

ALPAGES_TOTAL <- list(
  "9999" = c("Alpage_demo"),
  "2013" = c("Combe-Madame"),
  "2014" = c("Combe-Madame"),
  "2015" = c("Combe-Madame"),
  "2016" = c("Combe-Madame"),
  "2017" = c("Combe-Madame"),
  "2018" = c("Ane-et-Buyant", "Bedina", "Pesee", "Sept-Laux"),
  "2019" = c("Ane-et-Buyant", "Bedina", "Pesee", "Sept-Laux"),
  "2020" = c("Ane-et-Buyant", "Bedina", "Pesee","Rieuxclaret", "Sept-Laux"),
  "2021" = c("Ane-et-Buyant", "Bedina", "Pesee","Combe-Madame", "Sept-Laux"),
  "2022" = c("Ane-et-Buyant", "Bedina", "Cayolle", "Combe-Madame", "Grande-Fesse", "Jas-des-Lievres", "Lanchatra", "Pelvas","Pesee", "Sanguiniere","Sept-Laux", "Viso"),
  "2023" = c("Ane-et-Buyant", "Bedina", "Cayolle", "Crouzet", "Combe", "Combe-Madame", "Grande-Cabane", "Lanchatra", "Pesee", "Rouanette", "Sanguiniere", "Sept-Laux", "Vacherie-de-Roubion", "Viso"),
  "2024" = c("Viso", "Cayolle", "Sanguiniere")
)
ALPAGES <- ALPAGES_TOTAL[[as.character(YEAR)]]

if (FALSE){
  # Définition de la période d'échantillionage
  
  ## ENTREE ##
  # Un dossier contenant les trajectoires brutes, au format csv issu des colliers catlog, rangées dans des sous-dossiers au nom de leurs alpages
  
  
  
  ## SORTIE ##
  
  # Création du sous-dossier pour stocker les résultats du filtre de Bjorneraas
  filter_output_dir <- file.path(output_dir, "0. Sampling_Periods")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Fichier pdf de sortie pour visualisation du sampling_periode
  pdf(file.path(filter_output_dir, paste0("0. Sampling_Periods_", YEAR, "_", alpage, ".pdf")), width = 9, height = 9)
  
  # Un .RDS contenant les trajectoires catégorisées par comportement (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment)
  output_rds_file = file.path(output_dir, "0. Sampling_Periods", paste0("Sampling_periods_",YEAR,"_",alpage,".rds"))
  
  
  sampling_periods <- identify_sampling_period(data_dir, YEAR, TYPE, alpages, output_dir)
  
  print(sampling_periods)

}


#### 1. Simplification en GPKG ####
#----------------------------------#
if (T) {  # Mettre TRUE pour exécuter
  library(terra)
  source(file.path(functions_dir, "Functions_filtering.R"))
  
  ## ENTREE ##
  # Un dossier contenant les trajectoires brutes
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  
  # Charger le fichier des périodes d'échantillonnage
  sampling_period_file <- file.path(output_dir, "Sampling_Periods", paste0("Sampling_Periods_", YEAR, "_", alpage, ".rds"))
  sampling_periods <- readRDS(sampling_period_file)
  
  ## SORTIE ##
  # Création du sous-dossier de sortie
  gps_output_dir <- file.path(output_dir, "1. GPS_simple_GPKG")
  if (!dir.exists(gps_output_dir)) {
    dir.create(gps_output_dir, recursive = TRUE)
  }
  
  # Nom du fichier de sortie
  output_file <- file.path(gps_output_dir, paste0("Donnees_brutes_", YEAR, "_", alpages, "_simplifiees.gpkg"))
  
  # Traitement des fichiers par alpage
  lapply(alpages, function(alpage) {
    collar_dir <- file.path(raw_data_dir, alpage)
    
    # Sélection du type de fichier
    file_pattern <- if (TYPE == "catlog") "\\.csv$" else "\\.Rdata$"
    collar_files <- list.files(collar_dir, pattern = file_pattern, full.names = TRUE)
    
    if (length(collar_files) == 0) {
      warning(paste("No files found in", collar_dir, "for TYPE =", TYPE))
      return(NULL)
    }
    
    lapply(collar_files, function(collar_f) {
      # Extraction de l'ID du collier
      collar_ID <- if (TYPE == "catlog") {
        strsplit(basename(collar_f), split = "_")[[1]][1]
      } else {
        strsplit(basename(collar_f), split = "_")[[1]][1]
      }
      
      print(paste("Processing file:", collar_f, "Collar ID:", collar_ID))
      
      # Charger les données GPS
      traject <- switch(
        TYPE,
        "catlog" = load_catlog_data(collar_f),
        "ofb" = load_ofb_data_rdata(collar_f),
        stop("Unrecognized TYPE: please choose 'catlog' or 'ofb'")
      )
      
      # Vérifier si les données sont vides
      if (is.null(traject) || nrow(traject) == 0) {
        warning(paste("Empty dataset after loading:", collar_f))
        return(NULL)
      }
      
      # Récupérer le sampling_period pour ce collier
      sampling_period <- sampling_periods %>%
        filter(ID == collar_ID) %>%
        pull(SAMPLING)
      
      # Si aucun sampling_period trouvé, erreur
      if (length(sampling_period) == 0 || is.na(sampling_period)) {
        stop(paste("ERREUR: Aucun sampling_period trouvé pour le collier", collar_ID))
      }
      
      # Ajustement vers un échantillonnage de 30 minutes
      if (sampling_period != 30) {
        print(paste("Collar", collar_ID, "has sampling_period =", sampling_period, "minutes. Resampling to 30 minutes."))
        
        # Calcul du facteur de réduction
        reduction_factor <- round(30 / sampling_period)
        
        # Application du filtre
        traject <- traject %>% slice(which(row_number() %% reduction_factor == 1))
      }
      
      # Transformation et formatage des données
      traject <- traject %>%
        mutate(ID = collar_ID, date = lubridate::format_ISO8601(date)) %>%
        vect(geom = c("lon", "lat"), crs = CRS_WSG84)
      
      return(traject)
    }) %>% do.call(rbind, .)  # Fusionner les données des colliers d'un même alpage
  }) %>% do.call(rbind, .) -> merged_data  # Fusion finale pour tous les alpages
  
  # Vérifier si les données fusionnées sont vides
  if (is.null(merged_data) || nrow(merged_data) == 0) {
    stop("No data available to export to GPKG. Check input files and processing steps.")
  }
  
  # Exporter les données vers un fichier GPKG
  writeVector(merged_data, filename = output_file, overwrite = TRUE)
}

#### 2.BJONERAAS FILTER CALIBRATION ####
#--------------------------------------#
if (FALSE) {
  # Chargement des fonctions nécessaires
  source(file.path(functions_dir, "Functions_filtering.R"))
  source(file.path(functions_dir, "Functions_map_plot.R"))
  source(file.path(functions_dir, "Functions_check_metadata.R"))
  
  
  ## ENTREES ##
  # Un dossier contenant les trajectoires brutes, au format csv issu des colliers Catlog,
  # rangées dans des sous-dossiers au nom de leurs alpages
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  
  # Un data.frame contenant les dates de pose et de retrait des colliers
  # Doit contenir les colonnes "alpage", "date_pose" et "date_retrait"
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(AIF)
  AIF_data<-read.csv(AIF)
  
  ## SORTIE ##
  # Création du sous-dossier pour stocker les résultats du filtre de Bjorneraas
  filter_output_dir <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  # Fichier pdf de sortie pour visualisation du filtrage
  pdf(file.path(filter_output_dir, paste0("Filtering_calibration_", YEAR, "_", alpage, ".pdf")), width = 9, height = 9)
  
  
  ## CODE ##
  # Liste des fichiers de données brutes pour l'alpage
  files <- list.files(file.path(raw_data_dir, alpage), full.names = TRUE)
  files <- files[1:3]  # Sélection des trois premiers fichiers (évite surcharge mémoire)
  
  # Chargement et concaténation des données des fichiers sélectionnés
  data <- do.call(rbind, lapply(files, function(file) { 
    data <- load_catlog_data(file) # Chargement des fichiers CSV avec la fonction dédiée
    data$ID <- file  # Ajout de l'identifiant du fichier pour tracer son origine
    return(data) 
  }))
  
  
  
  # Chargement et concaténation des données des fichiers sélectionnés
  data <- do.call(rbind, lapply(files, function(file) { 
    # Chargement des fichiers en fonction du TYPE
    data <- switch(
      TYPE,
      "catlog" = load_catlog_data(file),
      "ofb" = load_ofb_data_rdata(file))
    data$ID <- basename(file)# Ajouter l'ID du fichier pour tracer son origine
    return(data)
  }))
  
  
  
  
  
  
  
  
  
  # Récupération des dates de pose et de retrait du collier
  beg_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_pose"), tz="GMT", format="%d/%m/%Y %H:%M")
  end_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M")
  data = date_filter(data, beg_date, end_date) # Filtrage des données en fonction des dates de validité
  if (TYPE == "ofb") {
    initial_count <- nrow(data)  # Nombre de lignes tot
    data <- data %>% filter(!is.na(lat) & !is.na(lon))
    removed_count <- initial_count - nrow(data)  # Nombre de lignes supprimées
    print(paste(removed_count, "points with NA were removed. Remaining points:", nrow(data)))
  } # FIltrage des données si TYPE = ofb
  
  
  
  
  # Projection des données en Lambert93 (EPSG:2154) à partir de WGS84 (EPSG:4326)
  data_xy <- data %>%
    terra::vect(geom = c("lon", "lat"), crs = "EPSG:4326") %>% 
    terra::project("EPSG:2154") %>%
    as.data.frame(geom = "XY")
  
  # Histogramme des intervalles de temps entre points GPS
  temps <- diff(data_xy$date)
  temps <- as.numeric(temps, units = "mins")
  hist(temps, nclass = 30)
  
  # Histogramme des distances parcourues entre deux positions successives
  dist <- sqrt(diff(data_xy$x)^2+diff(data_xy$y)^2)
  h <- hist(dist, nclass = 30, xlab='Distance (m)', xaxt="n")
  
  ### Test de différents filtres
  # Définition des valeurs de test pour le filtre de Bjorneraas
  
  # Définition des seuils en fonction du TYPE
  if (TYPE == "catlog") {
    medcrits <- c(750, 500, 750)  # Seuil médian des distances anormales
    meancrits <- c(500, 500, 350) # Seuil moyen des distances anormales
    spikesps <- c(1500, 1500, 1500)  # Seuil de vitesse maximale (m/h)
    spikecoss <- c(-0.95, -0.95, -0.95)  # Seuil de changement de direction (cosinus d’angle)
  } else if (TYPE == "ofb") {
    medcrits <- c(1000, 750, 1000)  # Seuil médian des distances anormales
    meancrits <- c(750, 750, 650) # Seuil moyen des distances anormales
    spikesps <- c(400, 400, 400)  # Seuil de vitesse maximale (m/h)
    spikecoss <- c(-0.95, -0.95, -0.95) } # Seuil de changement de direction (cosinus d’angle)
 
  
  
  for (i in 1:length(medcrits)) {
    # Application du filtre de Bjorneraas avec chaque combinaison de paramètres
    trajectories <- position_filter(data, medcrit=medcrits[i], meancrit=meancrits[i], spikesp=spikesps[i], spikecos=spikecoss[i])
    
    # Détermination des limites spatiales de la carte
    minmax_xy = get_minmax_L93(trajectories[!(trajectories$R1error | trajectories$R2error ),], buffer = 100)
    
    # Attribution des codes d'erreur pour visualisation
    trajectories$errors = 1
    trajectories$errors[trajectories$R1error] = 2
    trajectories$errors[trajectories$R2error] = 3
    
    # Définition de la palette de couleurs pour la carte
    pal <- c("#56B4E9", "red", "black")
    
    # Affichage des trajectoires GPS avec erreurs détectées
    print(ggplot(trajectories, aes(x, y, col = errors)) +
            geom_path(size = 0.2) +
            geom_point(size = 0.3) +
            coord_equal() +
            xlim(minmax_xy$x_min, minmax_xy$x_max) + ylim(minmax_xy$y_min, minmax_xy$y_max) +
            ggtitle(paste0("medcrit = ", medcrits[i], ", meancrit = ", meancrits[i], ", spikesp = ", spikesps[i], ", spikecos = ", spikecoss[i])) +
            scale_colour_gradientn(colors=pal, guide="legend", breaks = c(1, 2, 3), labels = c("OK", "R1error", "R2error")))
    
    # Zoom sur les cinq premiers jours après la pose
    trajectories <- trajectories %>%
      filter(date < beg_date + 3600*24*5)
    print(ggplot(trajectories, aes(x, y, col = errors)) +
            geom_path(size = 0.2) +
            geom_point(size = 0.3) +
            coord_equal() +
            ggtitle(paste0("5 days only, medcrit = ", medcrits[i], ", meancrit = ", meancrits[i], ", spikesp = ", spikesps[i], ", spikecos = ", spikecoss[i])) +
            scale_colour_gradientn(colors=pal, guide="legend", breaks = c(1, 2, 3), labels = c("OK", "R1error", "R2error")))
  }
  
  # Fermeture du fichier PDF contenant les visualisations
  dev.off()
}

#### 2.1 FILTERING CATLOG DATA ####
#--------------------------------#
if (FALSE) {
  # Chargement des fonctions nécessaires
  source(file.path(functions_dir, "Functions_filtering.R"))
  
  
  ## ENTREES ##
  # Un dossier contenant les trajectoires brutes, au format csv issu des colliers catlog, rangées dans des sous-dossiers au nom de leurs alpages. Coordonnées en WSG84. Le nom des fichiers, sous la forme "ID_quelquechose.csv", sera utilisé pour déterminer l’ID du collier qui doit comporter 3 caractères.
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  # Un fichier contenant les informations sur chaque individu équipé, les dates de pose et de retrait des colliers, ainsi que la proportion de temps pour laquelle les colliers sont programmés pour être allumés (18h par jour = 0.75). Doit contenir les colonnes "Collier", "Alpage", "Espece", "Race", "date_pose", "date_retrait" et "proportion_jour_allume"
  IIF = file.path(raw_data_dir, paste0(YEAR,"_colliers_poses.csv"))
  
  #Load and vérife data collier pose (format)
  IFF_data <- read.csv(IIF, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  ## SORTIES ##
  filter_output_dir <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Un .RDS contenant les trajectoires filtrées (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment). Coordonnées en Lambert93.
  output_rds_file = file.path(filter_output_dir, paste0("Catlog_",YEAR,"_filtered_",alpages,".rds"))
  # Un .csv contenant les performances des colliers (pourcentages de points éliminés à chaque étape, colliers défectueux...)
  indicator_file = file.path(filter_output_dir, paste0(YEAR,"_filtering_",alpages,".csv"))
  
  ## CODE ##
  
  for (alpage in alpages) {
    print(paste("WORKING ON ALPAGE", alpage))
    # Définition du pattern de fichier en fonction du TYPE
    file_pattern <- if (TYPE == "catlog") "\\.csv$" else "\\.Rdata$"
    collar_dir <- file.path(raw_data_dir, alpage) # Construction du chemin des fichiers GPS de l'alpage
    collar_files <- list.files(collar_dir, pattern = file_pattern, full.names = TRUE)# Liste des fichiers correspondant au TYPE
    
    medcrit = get_alpage_info(alpage, AIF, "medcrit")
    meancrit = get_alpage_info(alpage, AIF, "meancrit")
    spikesp = get_alpage_info(alpage, AIF, "spikesp")
    spikecos = as.numeric(gsub(",", ".", get_alpage_info(alpage, AIF, "spikecos")))
    print(paste0("Bjorneraas filter parameters: medcrit=",medcrit,", meancrit=", meancrit, ", spikesp=", spikesp, ", spikecos=", spikecos))
    print(collar_files)
    
    
    
    ######### TRAVAUX : Adapatation avec OFB : Combe-Madame 2013 ##########
    
    # Filtrage des trajectoires et calcul des indicateurs
    indicators <- lapply(collar_files, function(collar) {
      filter_one_collar(
        load_catlog_data(collar),  
        basename(collar),  # On passe uniquement le nom du fichier
        output_rds_file, alpage, beg_date, end_date, IIF,
        bjoneraas.medcrit = medcrit,
        bjoneraas.meancrit = meancrit,
        bjoneraas.spikesp = spikesp,
        bjoneraas.spikecos = spikecos
      )
    }) %>%
      do.call(rbind, .)
    
    indicators_tot = indicators %>%
      filter(worked_until_end == 1) %>% # to compute performance indicators at the alpage level, we remove defective collars
      add_row(name = paste("TOTAL", alpage), worked_until_end = sum(.$worked_until_end), nloc = NA,
              R1error = NA, R2error = NA,
              error_perc = sum(.$nloc*.$error_perc)/sum(.$nloc), localisation_rate = mean(.$localisation_rate))
    indicators = rbind(indicators, indicators_tot[nrow(indicators_tot),])
    
    write.table(indicators, file=indicator_file, append = T, sep=',', row.names=F, col.names=F)
  }
  
  ######### FIN TRAVAUX : Adapatation avec OFB : Combe-Madame 2013
}

#### 2.Bis FILTERING OFB DATA####
#-----------------------------------#
if (TRUE){
  # Partie temporère se basant sur les données deja prélablement filtrés par l'OFB
  # utilisant aussi le filtre de Bjorneraas. Mais dont les paramètres sont adaptés
  # a un temp d'aquisition toutes les 30 minutes ! 
  
  
  if (TYPE == "ofb"){
    ## ENTREES ##
    # Un dossier contenant les trajectoires brutes, au format Rdata issu des colliers OFB,
    # rangées dans des sous-dossiers au nom de leurs alpages
    raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
    
    # Un data.frame contenant les dates de pose et de retrait des colliers
    # Doit contenir les colonnes "alpage", "date_pose" et "date_retrait"
    AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
    
    # Vérification du format du fichier (souvent mal formaté, attention csv UTF8)
    AIF_data <- read.csv(AIF, sep = ",", header = TRUE, row.names = NULL, check.names = FALSE, encoding = "UTF-8")
    
    ## SORTIES ##
    # Dossier de sortie "Filtre_de_Bjorneraas: 
    
    filter_output_dir <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
    if (!dir.exists(filter_output_dir)) {
      dir.create(filter_output_dir, recursive = TRUE)
    }
    
    # Un .RDS contenant les trajectoires filtrées (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment). Coordonnées en Lambert93.
    output_rds_file = file.path(filter_output_dir, paste0("Catlog_",YEAR,"_filtered_",alpages,".rds"))
    
    
    
    ## CODE ##
    files <- list.files(file.path(raw_data_dir, alpage), full.names = TRUE)
    
    data <- do.call(rbind, lapply(files, function(file) { 
      # Chargement des fichiers en fonction du TYPE
      data <-load_ofb_data_rdata(file)
      data$ID <- basename(file)# Ajouter l'ID du fichier pour tracer son origine
      return(data)
    }))
    
    
    # Récupération des dates de pose et de retrait du collier
    beg_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_pose"), tz="GMT", format="%d/%m/%Y %H:%M")
    end_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M")
    data = date_filter(data, beg_date, end_date) # Filtrage des données en fonction des dates de validité
    
    #Supprime les NA
    
    initial_count <- nrow(data)  # Nombre de lignes tot
    data <- data %>% filter(!is.na(lat) & !is.na(lon))
    removed_count <- initial_count - nrow(data)  # Nombre de lignes supprimées
    print(paste(removed_count, "points with NA were removed. Remaining points:", nrow(data)))
    
    # Supprime les lignes où infoloc == "pb_bjorneraas"
    
    initial_count <- nrow(data)  # Mise à jour du nombre total avant ce filtre
    data <- data %>% filter(infoloc != "pb_bjorneraas")
    removed_bjorneraas_count <- initial_count - nrow(data)  # Nombre de lignes supprimées (pb_bjorneraas)
    print(paste(removed_bjorneraas_count, "points with location issues (pb_bjorneraas) were removed. Remaining points:", nrow(data)))
    
    
    
    
    
    # Transformation des colonnes avant enregistrement pour s'adapter a la sortie CATLOG
    data <- data %>%
      rename(time = date) %>%  # Renomme 'date' en 'time'
      mutate(
        ID = sub("_.*", "", ID),  # Garde uniquement la partie avant "_" dans ID
        alpage = alpage,          # Ajoute une colonne 'alpage' remplie avec la valeur de l'objet alpage
        species = "brebis",        # Ajoute une colonne 'species' remplie avec "brebis"
        race = "Merinos"
      ) %>%
      dplyr::select(-infoloc)  # Supprime la colonne 'infoloc'
    
    
    # Enregistrement des données filtrées au format .RDS
    saveRDS(data, output_rds_file)
    
    
  }
  



}

#### 3. HMM FITTING #### 
#----------------------#
if (FALSE) {
  library(snow)
  library(stats)
  library(momentuHMM)
  library(adehabitatLT)
  library(adehabitatHR)
  library(knitr)
  library(rmarkdown)
  source(file.path(functions_dir, "Functions_HMM_fitting.R"))
  
  # ENTREES
  # Un .RDS contenant les trajectoires filtrées
  input_rds_file <- file.path(output_dir, "2. Filtre_de_Bjorneraas", paste0("Catlog_", YEAR, "_filtered_", alpage, ".rds"))
  
  # Un data.frame contenant la correspondance entre colliers et alpages. Doit contenir les colonnes  "ID", "Alpage" et "Periode d’echantillonnage"
  individual_info_file <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"), paste0(YEAR, "_colliers_poses.csv"))
  individual_info_file_data <- read.csv(individual_info_file, stringsAsFactors = FALSE, encoding = "UTF-8")
  
  
  # Charger le fichier des périodes d'échantillonnage
  sampling_period_file <- file.path(output_dir, "0. Sampling_Periods", paste0("Sampling_Periods_", YEAR, "_", alpage, ".rds"))
  sampling_periods <- readRDS(sampling_period_file)
  sampling_table <- readRDS(sampling_period_file)
  
  # SORTIES
  
  # Création du sous-dossier pour stocker les résultats du filtre de Bjorneraas
  filter_output_dir <- file.path(output_dir, "3. HMM_comportement")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  # Un .RDS contenant les trajectoires catégorisées par comportement (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment)
  output_rds_file = file.path(output_dir, "3. HMM_comportement", paste0("Catlog_",YEAR,"_",alpage,"_viterbi.rds"))
  
  
  
  ### LOADING DATA FOR ANALYSES
  data = readRDS(input_rds_file)
  data = data[data$species == "brebis",]
  data = data[data$alpage %in% alpages,]
  
  
  
  
  
  ### ⚠️⚠️ ATTENTION DEGRADATION A 30 MIN DU JEU DE DONNEES 2 MIN ⚠️⚠️
  {
  # Suppression des données collectées entre 21h et 3h
  library(lubridate)
  data = data[!(hour(data$time) >= 21 | hour(data$time) < 3), ]
  

  {
  degradation = 15
  first_index = 0
  
  
  if (degradation == 1) {
    data_resamp <- data
  } else {
    data_resamp = data.frame()
    for(id in unique(data$ID)) {
      indices = which(data$ID==id)
      data_resamp = rbind(data_resamp, data[indices[seq(first_index, length(indices), degradation)] ,])
    }
  }
  
 
  }
  
  data <- data_resamp

  ### ⚠️⚠️ FIN DE DE LA PARTIE DEGRADATION ⚠️⚠️
  }
  
  
  
  
  #ANCIENNE
  
  # Définition de l'objet SAMPLING
  SAMPLING <- 30
  sampling_parameters <- get_sampling_parameters_ancienne()
  
  
  ### HMM FIT
  run_parameters = list(
    # Model
    model = "HMM",
    
    # Resampling
    resampling_ratio = sampling_parameters$resampling_ratio,
    resampling_first_index = 0,
    rollavg = FALSE,
    rollavg_convolution = c(0.15, 0.7, 0.15),
    knownRestingStates = FALSE,
    
    # Observation distributions (step lengths and turning angles)
    dist = list(step = "gamma", angle = "vm"),
    # Design matrices to be used for the probability distribution parameters of each data stream
    DM = list(angle=list(mean = ~1, concentration = ~1)),
    # Covariants formula
    covariants = ~cos(hour*3.141593/12), # ~1 if no covariants used
    
    # 3-state HMM
    Par0 = list(step = c(10, 25, 50, 10, 15, 40), angle = c(tan(pi/2), tan(0/2), tan(0/2), log(0.5), log(0.5), log(3))),
    fixPar = list(angle = c(tan(pi/2), tan(0/2), tan(0/2), NA, NA, NA))
  )
  run_parameters = scale_step_parameters_to_resampling_ratio_ancienne(run_parameters, alpage, sampling_parameters)
  
  
  
  startTime <- Sys.time()
  par_HMM_fit_ancienne(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir)
  endTime <- Sys.time()
  
  
  
  
  
  ##NEW
  
  # Générer les paramètres pour chaque ID
  sampling_parameters_list <- lapply(sampling_periods$SAMPLING, get_sampling_parameters)
  names(sampling_parameters_list) <- sampling_periods$ID
  
  
  
  
  ### HMM FIT
  run_parameters_list <- lapply(names(sampling_parameters_list), function(id) {
    params <- sampling_parameters_list[[id]]
    run_parameters <- list(
      model = "HMM",
      resampling_ratio = params$resampling_ratio,
      resampling_first_index = 0,
      rollavg = FALSE,
      rollavg_convolution = c(0.15, 0.7, 0.15),
      knownRestingStates = FALSE,
      dist = list(step = "gamma", angle = "vm"),
      DM = list(angle=list(mean = ~1, concentration = ~1)),
      covariants = ~cos(hour*3.141593/12),
      Par0 = list(step = c(10, 25, 50, 10, 15, 40), angle = c(tan(pi/2), tan(0/2), tan(0/2), log(0.5), log(0.5), log(3))),
      fixPar = list(angle = c(tan(pi/2), tan(0/2), tan(0/2), NA, NA, NA))
    )
    
    scale_step_parameters_to_resampling_ratio(run_parameters, alpage, params)
  })
  names(run_parameters_list) <- names(sampling_parameters_list)
  
  
  
  
  startTime <- Sys.time()
  results <- par_HMM_fit_test(data, run_parameters_list, ncores, individual_info_file, sampling_table, output_dir)
  endTime <- Sys.time()
  
 
  
  # Verifié la connéxion intenert 



### SAVE RESULTING TRAJECTORIES
data_hmm <- do.call("rbind", lapply(results, function(result) result$data))

viterbi_trajectory_to_rds(data_hmm, output_rds_file, individual_info_file)



}

#### 4. HMM FITTING #### 
#----------------------#
if (F) {
  library(snow)
  library(stats)
  # Movement modelling packages
  library(momentuHMM)
  library(adehabitatLT)
  library(adehabitatHR)
  # Libraries RMarkdown
  library(knitr)
  library(rmarkdown)
  source(file.path(functions_dir, "Functions_HMM_fitting.R"))
  
  # ENTREES
  # Un .RDS contenant les trajectoires filtrées
  input_rds_file <- file.path(output_dir, "Filtre_de_Bjorneraas", paste0("Catlog_", YEAR, "_filtered_", alpage, ".rds"))
  
  # Un data.frame contenant la correspondance entre colliers et alpages. Doit contenir les colonnes  "ID", "Alpage" et "Periode d’echantillonnage"
  individual_info_file <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"), paste0(YEAR, "_colliers_poses.csv"))
  individual_info_file_data <- read.csv(individual_info_file, stringsAsFactors = FALSE, encoding = "UTF-8")
  # Les alpages à traiter
  alpages = ALPAGES
  
  # SORTIES
  
  # Création du sous-dossier pour stocker les résultats du filtre de Bjorneraas
  filter_output_dir <- file.path(output_dir, "HMM_comportement")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  # Un .RDS contenant les trajectoires catégorisées par comportement (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment)
  output_rds_file = file.path(output_dir, "HMM_comportement", paste0("Catlog_",YEAR,"_",alpage,"_viterbi.rds"))
  
  
  #Function 
  
  ### HMM fitting and plotting functions
  hmm_fit <- function(data, runPar, alpage_directory, sampling_period) {
    # Fit a momentuHMM hmm on one individual’s sub-trajectories and save the resulting figures.
    # INPUTS :
    #   data : a data.frame containing a trajectory from a unique individual described as an ID, time, x and y fields.
    #   runPar : a list of parameters to run the model with (see the parameters_to_data_frame function for an extensive list of parameters)
    #   alpage_directory : a directory to save the resulting figures of this individual’s hmm fit
    # OUTPUT : the momemtuHMM object, with a $data field containing the original trajectory (or sub-trajectories), ordered by time,
    #          along with the viterbi states sequence, the state probabilities and the original ID (only the first ID is retained)
    
    ### DATA PREPARATION
    ID = data$ID[1]
    data_hmm <- regularise_trajectories(data, sampling_period)
    if (runPar$rollavg) data_hmm <- rolling_averaging_trajectories(data_hmm, conv= runPar$rollavg_convolution)
    data_hmm <- resample_trajectories(data_hmm, runPar$resampling_ratio, runPar$resampling_first_index)
    data_hmm <- prepare_hmm_trajectories(data_hmm)
    
    knownStates <- rep(NA, nrow(data_hmm))
    
    if(runPar$knownRestingStates) { # if we consider some resting states to be known, then the first and last half-hours of record period are considered to be resting states
      knownStates[(data_hmm$hour > 3 && data_hmm$hour < 3.5) || (data_hmm$hour > 20.5 && data_hmm$hour < 21 )] <- 1
    }
    
    ### MODEL FITTING
    stateNames = c("Repos", "Paturage", "Deplacement")
    run <- fitHMM(data_hmm, nbStates = 3, dist = runPar$dist, DM=runPar$DM, Par0 = runPar$Par0,
                  estAngleMean=list(angle=TRUE), fixPar = runPar$fixPar,
                  stateNames = stateNames,
                  knownStates = knownStates,
                  formula = runPar$covariants,
                  optMethod = "Nelder-Mead")
    
    # Get the most likely states and their probabilities
    run$data$state <- viterbi(run)
    state_proba = stateProbs(run)
    run$data$state_proba = NA
    for (i in 1:nrow(run$data)) { run$data$state_proba[i] = state_proba[i, run$data$state[i]] }
    
    # Plot HMM results and trajectories
    dir.create(paste0(alpage_directory,"individual_trajectories/"), recursive = TRUE)
    plot_results(run, paste0(alpage_directory,"individual_trajectories/",ID))
    
    # Remove NA
    run$data <- run$data[!is.na(run$data$x),]
    # Reorder data by time
    run$data <- run$data[order(run$data$time),]
    
    # Restore original ID
    run$data$ID <- ID
    
    return(run)
  }
  
  
  
  par_HMM_fit_test <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
    # Paralelized wrapper for hmm_fit
    # Fit a momentuHMM hmm on one several individuals’ trajectories and save the resulting figures.
    # INPUTS :
    #   data : a data.frame containing a trajectory from a unique individual described as an ID, time, x and y fields.
    #   runPar : a list of parameters to run the model with (see the parameters_to_data_frame function for an extensive list of parameters)
    #   individual_info_file : path to the csv file containing information about every individual ID, must contain "ID", "Alpage" and "Periode d’echantillonnage" columns.
    # OUTPUT : a list of the individual’s momentuHMM objects. Each momentuHMM object’s $data field contains the original trajectory (or sub-trajectories), ordered by time,
    #          along with the viterbi states sequence, the state probabilities and the original ID (only the first ID is retained)
    
    print(paste0("+++ momentuHMM parallel RUN +++"))
    
    startTime <- Sys.time()
    clus <- makeCluster(ncores, outfile='') # outfile='' is verbose option
    clusterExport(clus, as.list(lsf.str(.GlobalEnv))) # export all loaded functions
    clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
    # Load libraries
    clusterCall(clus, function() {
      options(warn=-1)
      # For the pipes
      suppressPackageStartupMessages(library(tidyverse)) # includes ggplot2 and dplyr among others
      theme_set(theme_bw()) # theme for ggplot2
      library(lubridate)
      # Movement modelling packages
      library(momentuHMM)
      # library(foieGras)
      library(adehabitatLT)
      # GIS packages
      library(sf)
      library(sp)
      library(terra)
      # Load functions and set paths
      source("Functions/Functions_utility.R") # Courtesy Théo Michelot
      BACKGROUND_TYPE = "BDALTI"
      source("Functions/Functions_map_plot.R")
      source("Functions/Constants.R")
      options(warn=0)
    })
    
    results <- parLapply(clus, unique(data$ID),
                         function(ID) {
                           alpage = get_individual_alpage(ID, individual_info_file)
                           
                           # 🔍 Debugging : Vérifier si l'alpage est bien "alpage_demo"
                           print(paste0("🔍 ID: ", ID, " - Alpage récupéré : ", alpage))
                           
                           sampling_period = get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
                           
                           return(hmm_fit(data[data$ID==ID,], run_parameters, paste0(output_dir, alpage, "/"), sampling_period))
                         }
    )
    
    stopCluster(clus)
    endTime <- Sys.time()
    print(paste("+++ Cluster total excecution time :", round(difftime(endTime, startTime, units='mins'),2), "min +++"))
    
    return(results)
  }
  
  
  
  
  
  
  scale_step_parameters_to_resampling_ratio <- function(run_parameters) {
    run_parameters$Par0$step[2] = run_parameters$resampling_ratio * run_parameters$Par0$step[2]
    run_parameters$Par0$step[5] = sqrt(run_parameters$resampling_ratio) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] = run_parameters$resampling_ratio * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] = sqrt(run_parameters$resampling_ratio) * run_parameters$Par0$step[6]
    
    return(run_parameters)
  }
  
  
  
  #NOUVELLE FUNCTION
  
  # Définition d'un objet SAMPLING pour une gestion modulaire
  get_sampling_parameters <- function(sampling_period) {
    if (sampling_period < 10) {
      resampling_ratio <- ceiling(10 / sampling_period)  # On ramène à 10 min
      param_scaling_factor <- 10 / 2  # Facteur basé sur 2 min vers 10 min
    } else {
      resampling_ratio <- 1  # Pas de rééchantillonnage
      param_scaling_factor <- sampling_period / 2  # Échelle basée sur 2 min
    }
    return(list(sampling_period = sampling_period, resampling_ratio = resampling_ratio, param_scaling_factor = param_scaling_factor))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### LOADING DATA FOR ANALYSES
  data = readRDS(input_rds_file)
  data = data[data$species == "brebis",]
  data = data[data$alpage %in% alpages,]
  
  
 
  
  ### HMM FIT
  run_parameters = list(
    # Model
    model = "HMM",
    
    # Resampling
    resampling_ratio = 15,
    resampling_first_index = 0,
    rollavg = FALSE,
    rollavg_convolution = c(0.15, 0.7, 0.15),
    knownRestingStates = FALSE,
    
    # Observation distributions (step lengths and turning angles)
    dist = list(step = "gamma", angle = "vm"),
    # Design matrices to be used for the probability distribution parameters of each data stream
    DM = list(angle=list(mean = ~1, concentration = ~1)),
    # Covariants formula
    covariants = ~cos(hour*3.141593/12), # ~1 if no covariants used
    
    # 3-state HMM
    Par0 = list(step = c(10, 25, 50, 10, 15, 40), angle = c(tan(pi/2), tan(0/2), tan(0/2), log(0.5), log(0.5), log(3))),
    fixPar = list(angle = c(tan(pi/2), tan(0/2), tan(0/2), NA, NA, NA))
  )
  run_parameters = scale_step_parameters_to_resampling_ratio(run_parameters)
  
  print(paste0("Par0 Step après Scaling : ", run_parameters$Par0$step))
  
  
  startTime = Sys.time()
  results = par_HMM_fit_test(data, run_parameters, ncores = ncores, individual_info_file, sampling_period = 120, output_dir)
  endTime = Sys.time()
  # Verifié la connéxion intenert 
  





}













#### 5. FLOCK STOCKING RATE (charge) BY DAY AND BY STATE ####
#-------------------------------------------------------------#
library(adehabitatHR)
library(data.table)
library(snow)
source(file.path(functions_dir, "Functions_map_plot.R"))
source(file.path(functions_dir, "Functions_flock_density.R"))

# ENTREES
# Un .RDS contenant les trajectoires catégorisées par comportement
input_rds_file <- file.path(output_dir, "3. HMM_comportement",  paste0("Catlog_", YEAR, "_", alpage, "_viterbi.rds"))

# Un data.frame contenant les tailles de troupeaux et les évolutions des tailles en fonction de la date
flock_size_file <- file.path(raw_data_dir, paste0(YEAR, "_tailles_troupeaux.csv"))
flock_size_file_data <- read.csv(flock_size_file, stringsAsFactors = FALSE, encoding = "UTF-8")
# Les alpages à traiter
alpages <- ALPAGES

# SORTIES
# Dossier de sortie
save_dir <- file.path(output_dir, "Chargements_calcules")

# Un .RDS par alpage contenant les charges journalières par comportement
state_daily_rds_prefix <- paste0("by_day_and_state_", YEAR, "_")
# Un .RDS par alpage contenant les charges journalières
daily_rds_prefix <- paste0("by_day_", YEAR, "_")
# Un .RDS par alpage contenant les charges par comportement
state_rds_prefix <- paste0("by_state_", YEAR, "_")
# Un .RDS par alpage contenant la charge totale sur toute la saison
total_rds_prefix <- paste0("total_", YEAR, "_")

h <- 25 # Distance caractéristique pour calculer le chargement

for (alpage in alpages) {
  flock_sizes <- get_flock_size_through_time(alpage, flock_size_file)
  prop_time_collar_on <- get_alpage_info(alpage, AIF, "proportion_jour_allume")
  
  # Chargement des données filtrées pour l'alpage
  data <- readRDS(input_rds_file)
  data <- data[data$alpage == alpage,]
  
  # Chargement du raster de phénologie avec le bon chemin
  raster_file <- file.path(raster_dir, paste0("ndvis_", YEAR, "_", alpage, "_pheno_metrics.tif"))
  pheno_t0 <- get_raster_cropped_L93(raster_file, get_minmax_L93(data, 100), reproject = TRUE, band = 2, as = "SpatialPixelDataFrame")
  
  # Définition du dossier de stockage spécifique à l'alpage
  alpage_save_dir <- file.path(save_dir, paste0(alpage, "_", YEAR))
  if (!dir.exists(alpage_save_dir)) dir.create(alpage_save_dir, recursive = TRUE)
  
 
  # BY day and by state 
  
    flock_load_by_day_and_state_to_rds_kernelbb(
    data, 
    pheno_t0, 
    alpage_save_dir,  
    state_daily_rds_prefix, 
    flock_sizes, 
    prop_time_collar_on
  )
  
  gc()
    
  #Fusion des fichier indiv
  merged_file <- flock_merge_rds_files(alpage_save_dir, state_daily_rds_prefix)
  
  
  
  rm(data)
  
  
  charge <- readRDS(file.path(alpage_save_dir, paste0(state_daily_rds_prefix, alpage, ".rds")))
  unique(charge$state)
  
  # By state
  charge_state <- charge %>%
    group_by(x, y, state) %>%
    summarise(Charge = sum(Charge, na.rm = TRUE), .groups = 'drop') %>%
    as.data.frame()
  saveRDS(charge_state, file.path(alpage_save_dir, paste0(state_rds_prefix, alpage, ".rds")))
  rm(charge_state)
  
  # By day
  charge_day <- lapply(unique(charge$day), function(d) {
    charge %>%
      filter(day == d) %>%
      group_by(x, y, day) %>%
      summarise(Charge = sum(Charge, na.rm = TRUE), .groups = 'drop')
  })
  charge_day <- as.data.frame(rbindlist(charge_day, use.names = TRUE))
  saveRDS(charge_day, file.path(alpage_save_dir, paste0(daily_rds_prefix, alpage, ".rds")))
  rm(charge_day)
  
  # Total
  charge_tot <- charge %>%
    group_by(x, y) %>%
    summarise(Charge = sum(Charge, na.rm = TRUE), .groups = 'drop') %>%
    as.data.frame()
  saveRDS(charge_tot, file.path(alpage_save_dir, paste0(total_rds_prefix, alpage, ".rds")))
  rm(charge_tot)
  
  rm(charge)
}












