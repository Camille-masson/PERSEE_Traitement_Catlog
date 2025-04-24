
#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#
gc()

# Chargement de la configuration
source("config.R")

# Définition de l'année d'analyse et des alpages à traiter 
YEAR = 2024
alpage = "Sanguiniere"
alpages = "Cayolle"

ALPAGES_TOTAL <- list(
  "9999" = c("Alpage_demo"),
  "2022" = c("Ane-et-Buyant", "Cayolle", "Combe-Madame", "Grande-Fesse", "Jas-des-Lievres", "Lanchatra", "Pelvas", "Sanguiniere", "Viso"),
  "2023" = c("Cayolle", "Crouzet", "Grande-Cabane", "Lanchatra", "Rouanette", "Sanguiniere", "Vacherie-de-Roubion", "Viso"),
  "2024" = c("Viso", "Cayolle", "Sanguiniere")
)
ALPAGES <- ALPAGES_TOTAL[[as.character(YEAR)]]


#### 1. Simplification en GPKG ####
#----------------------------------#
if (TRUE) {  
  ## DESCRIPTION ##
  
  # Permet d'obtenir un fichier .GPKG pour identifier, selon la méthode
  # détaillée dans le README, les dates de pose et de retrait des colliers GPS.
  # Elle permet ainsi de remplir les trois tables de métadonnées nécessaires à l'exécution
  # de ce script, à savoir :
  # - AAAA_colliers_poses
  # - AAAA_infos_alpages
  # - AAAA_tailles_troupeaux
  
  ## LIBRARY ##
  library(terra)
  source(file.path(functions_dir, "Functions_filtering.R"))
  
  ## ENTREE ##
  # Un dossier contenant les trajectoires brutes, au format csv issu des colliers catlog rangées dans des sous-dossiers au nom de leurs alpages
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  
  ## SORTIE ##
  # Création du sous-dossier de sortie : GPS_simple_GPKG
  gps_output_dir <- file.path(output_dir, "1. GPS_simple_GPKG")
  if (!dir.exists(gps_output_dir)) {
    dir.create(gps_output_dir, recursive = TRUE)
  }
  # Créeation du GPKG de sortie nommé : Donnees_brutes_9999_Alpage_demo_simplifiees.gpkg
  output_file <- file.path(gps_output_dir, paste0("Donnees_brutes_", YEAR, "_", alpage, "_simplifiees.gpkg"))
  
  
  ## CODE ##
  
  lapply(alpages, function(alpage) {
    collar_dir <- file.path(raw_data_dir, alpage) 
    collar_files <- list.files(collar_dir, full.names = TRUE) 
    lapply(collar_files, function(collar_f) {
      collar_ID <- substr(basename(collar_f), 1, 3)
      load_catlog_data(collar_f) %>% 
        slice(which(row_number() %% 30 == 10)) %>% 
        mutate(ID = collar_ID, date = lubridate::format_ISO8601(date)) %>% 
        vect(geom = c("lon", "lat"), crs = CRS_WSG84) 
    }) %>% do.call(rbind, .) 
  }) %>% do.call(rbind, .) %>%
    writeVector(filename = output_file, overwrite = TRUE) 
}

#### 2.1 BJONERAAS FILTER CALIBRATION ####
#----------------------------------------#
if (TRUE) {
  ## DESCRIPTION ##
  
  # Partie optionnelle permettant d'optimiser les paramètres du filtre 
  # de Bjorneraas afin de les adapter aux caractéristiques spécifiques de l'alpage à traiter.
  # Cette étape reste facultative, les paramètres initiaux étant déjà satisfaisants.
  # Si l'utilisateur souhaite néanmoins affiner ces paramètres, il devra modifier
  # les valeurs prédéfinies en dur dans le script 2.2.
  
  
  # Nécéssite :
  # - .csv | "AAAA_infos_alpes.csv"
  # - dossier contenant les données brutes | "Colliers_AAAA_brutes"
  
  # En sortie, un fichier .pdf est généré avec les résultats du filtrage appliqué :
  # outputs/2. Filtres_de_Bjorneraas/Filtering_calibration_9999_Alpage_demo
  
  ## LIBRARY ##
  source(file.path(functions_dir, "Functions_filtering.R"))
  source(file.path(functions_dir, "Functions_map_plot.R"))
  source(file.path(functions_dir, "Functions_check_metadata.R"))
  
  ## ENTREES ##
  # Un dossier contenant les trajectoires brutes, au format csv issu des colliers Catlog,
  # rangées dans des sous-dossiers au nom de leurs alpages
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  
  # Un .csv  "infos_alpages" remplis selon le model du jeu démo
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  # AIF_data <- read.csv(AIF, sep = ",", header = TRUE, row.names = NULL, check.names = FALSE, encoding = "UTF-8")
  # str(AIF_data)
  
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
    data <- load_catlog_data(file)
    data$ID <- file  
    return(data) 
  }))
  
  # Récupération des dates de pose et de retrait du collier
  beg_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_pose"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
  end_date = as.POSIXct(get_alpage_info(alpage, AIF, "date_retrait"), tz="GMT", format="%d/%m/%Y %H:%M:%S")
  data = date_filter(data, beg_date, end_date) 
  
  # Projection des données en Lambert93 (EPSG:2154) à partir de WGS84 (EPSG:4326)
  data_xy <- data %>%
    terra::vect(crs="EPSG:4326") %>%
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
  medcrits = c(750, 500, 750) 
  meancrits = c(500, 500, 350) 
  spikesps = c(1500, 1500, 1500) 
  spikecoss = c(-0.95, -0.95, -0.95) 
  
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

#### 2.2 FILTERING CATLOG DATA ####
#---------------------------------#
if (TRUE) {
  ## DESCRIPTION ##
  
  # Filtrage des données GPS brutes selon le filtre de Bjorneraas,
  # afin d'obtenir des données nettoyées et prêtes pour les étapes suivantes.
  
  # Nécessite :
  # - .csv : "AAAA_infos_alpages.csv"
  # - .csv : "AAAA_colliers_poses.csv"
  # - dossier contenant les données brutes : "Colliers_AAAA_brutes"
  
  # En sortie, un fichier .rds contenant les données filtrées :
  # outputs/2. Filtres_de_Bjorneraas/Catlog_9999_filtered_alpage_demo.rds
  # .csv contenant les performances des colliers :
  # outputs/2. Filtres_de_Bjorneraas/9999_filtering_alpages.csv
  
  
  ## LIBRARY ##
  source(file.path(functions_dir, "Functions_filtering.R"))
  source(file.path(functions_dir, "Functions_check_metadata.R"))
  
  ## ENTREES ##
  # Un dossier contenant les trajectoires brutes
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  
  # Un .CSV "Colliers_poses"
  IIF = file.path(raw_data_dir, paste0(YEAR,"_colliers_poses.csv"))
  check_and_correct_csv(IIF)
  #str(read.csv(IIF, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  # Un .csv  "infos_alpages" remplis selon le model du jeu démo
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  #str(read.csv(AIF, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  ## SORTIES ##
  filter_output_dir <- file.path(output_dir, "2. Filtre_de_Bjorneraas")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Un .RDS contenant les trajectoires filtrées 
  output_rds_file = file.path(filter_output_dir, paste0("Catlog_",YEAR,"_filtered_",alpages,".rds"))
  # Un .csv contenant les performances des colliers 
  indicator_file = file.path(filter_output_dir, paste0(YEAR,"_filtering_",alpages,".csv"))
  
  ## CODE ##
  for (alpage in alpages) {
    print(paste("WORKING ON ALPAGE :", alpage))
    collar_dir <- file.path(raw_data_dir, alpage)
    collar_files <- list.files(collar_dir, pattern = ".csv", full.names = TRUE)
    
    # Optionnel (utilisateurs avancés souhaitant adapter les paramètres à chaque alpage) :
    # remplir le fichier CSV "infos_alpages" avec les bons paramètres.
    #medcrit = get_alpage_info(alpage, AIF, "medcrit")
    #meancrit = get_alpage_info(alpage, AIF, "meancrit")
    #spikesp = get_alpage_info(alpage, AIF, "spikesp")
    #spikecos = as.numeric(gsub(",", ".", get_alpage_info(alpage, AIF, "spikecos")))
    
    
    medcrit = 750
    meancrit = 500
    spikesp = 1500
    spikecos = -0.95
    print(paste0("Bjorneraas filter parameters: medcrit=",medcrit,", meancrit=", meancrit, ", spikesp=", spikesp, ", spikecos=", spikecos))
    print(collar_files)
    
    
    # Filtrage des trajectoires et calcul des indicateurs
    indicators <- lapply(collar_files, function(collar) {
      filter_one_collar(
        load_catlog_data(collar),  
        basename(collar), 
        output_rds_file, alpage, beg_date, end_date, IIF,
        bjoneraas.medcrit = medcrit,
        bjoneraas.meancrit = meancrit,
        bjoneraas.spikesp = spikesp,
        bjoneraas.spikecos = spikecos
      )
    }) %>%
      do.call(rbind, .)
    
    indicators_tot = indicators %>%
      filter(worked_until_end == 1) %>% 
      add_row(name = paste("TOTAL", alpage), worked_until_end = sum(.$worked_until_end), nloc = NA,
              R1error = NA, R2error = NA,
              error_perc = sum(.$nloc*.$error_perc)/sum(.$nloc), localisation_rate = mean(.$localisation_rate))
    indicators = rbind(indicators, indicators_tot[nrow(indicators_tot),])
    
    write.table(indicators, file=indicator_file, append = T, sep=',', row.names=F, col.names=F)
  }
}

#### 3. HMM FITTING #### 
#----------------------#
if (TRUE) {
  ## DESCRIPTION ##
  # Trajectoires caractérisées par le comportement "HHM"
  
  # Nécessite :
  # - .csv : "AAAA_infos_alpages.csv"
  # - .csv : "AAAA_colliers_poses.csv"
  # - fichier .rds contenant les données filtrées : "Catlog_AAAA_filtered_alpage_demo.rds"
  
  # En sortie :
  # - Un fichier .rds contenant les données annotées avec les comportements :
  #   outputs/3. HMM_Comportement/Catlog_AAAA_alpage_demo_viterbi.rds
  # - Un fichier .pdf contenant les trajectoires catégorisées par comportement :
  #   outputs/3. HMM_Comportement/Output_PDF_AAAA_alpage_demo
  
  ## LIBRARY ##
  library(snow)
  library(stats)
  library(momentuHMM)
  library(adehabitatLT)
  library(adehabitatHR)
  library(knitr)
  library(rmarkdown)
  source(file.path(functions_dir, "Functions_HMM_fitting.R"))
  
  ## ENTREES ##
  # Un .RDS contenant les trajectoires filtrées en 2.2
  input_rds_file <- file.path(output_dir, "2. Filtre_de_Bjorneraas", paste0("Catlog_", YEAR, "_filtered_", alpage,".rds"))
  
  # Un data.frame contenant la correspondance entre colliers et alpages. 
  individual_info_file <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"), paste0(YEAR, "_colliers_poses.csv"))
  #str(read.csv(individual_info_file, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  
  ## SORTIES ##
  # Création du sous-dossier pour stocker les résultats du filtre de Bjorneraas
  filter_output_dir <- file.path(output_dir, "3. HMM_comportement")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Création du sous-dossier pour stocker les résultats individuels sous forme d'un PDF
  hmm_pdf_case <- file.path(filter_output_dir, paste0("Output_PDF_", YEAR, "_",alpage))
  if (!dir.exists(hmm_pdf_case)) {
    dir.create(hmm_pdf_case, recursive = TRUE)
  }
  
  # Un .RDS contenant les trajectoires catégorisées par comportement (les nouvelles trajectoires sont ajoutées à la suite des trajectoires traitées précédemment)
  output_rds_file = file.path(output_dir, "3. HMM_comportement", paste0("Catlog_",YEAR,"_",alpage,"_viterbi.rds"))
  
  ## CODE ##
  
  
  data = readRDS(input_rds_file)
  
  run_parameters = list(
    # Model
    model = "HMM",
    
    # Resampling
    resampling_ratio = 5,
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
  
  # Verifié la connéxion intenert
  startTime = Sys.time()
  results = par_HMM_fit(data, run_parameters, ncores = ncores, individual_info_file, sampling_period = 120, output_dir = hmm_pdf_case)
  endTime = Sys.time()
  
  data_hmm <- do.call("rbind", lapply(results, function(result) result$data))
  viterbi_trajectory_to_rds(data_hmm, output_rds_file, individual_info_file)
}

#### 3.1 HMM with night park #### 
#-------------------------------#
if (TRUE) {
  library(dplyr)
  library(lubridate)
  library(dbscan)
  source(file.path(functions_dir, "Functions_HMM_fitting.R"))
  
  for (alpage in alpages) {
    # ENTREE
    case_state_dir      <- file.path(output_dir, "3. HMM_comportement")
    state_rds_file      <- file.path(
      case_state_dir,
      paste0("Catlog_", YEAR, "_", alpage, "_viterbi.rds")
    )
    
    # SORTIE
    output_parc_rds_file <- file.path(
      case_state_dir,
      paste0("Catlog_", YEAR, "_", alpage, "_viterbi_parc.rds")
    )
    
    
    
    # CODE
    # Alpages “Cayolle” et “Sanguiniere” : seuil 500 m, sans critère de durabilité
    if (alpage %in% c("Cayolle", "Sanguiniere")) {
      traj_by_night_park(
        state_rds_file       = state_rds_file,
        output_parc_rds_file = output_parc_rds_file,
        window_minutes       = 30,
        eps_dist             = 500,
        rare_threshold       = 5
      )
    }
    
    # Alpage “Viso” : même params + critère de durabilité (3 nuits)
    if (alpage == "Viso") {
      traj_by_night_park_count(
        state_rds_file       = state_rds_file,
        output_parc_rds_file = output_parc_rds_file,
        window_minutes       = 30,
        eps_dist             = 500,
        rare_threshold       = 5,
        require_consec       = TRUE,
        min_consec_nights    = 3
      )
    }
  }
  
  
  
  # ENTREE
  case_state_dir      <- file.path(output_dir, "3. HMM_comportement")
  input_parc_rds_file <- file.path(
    case_state_dir,
    paste0("Catlog_", YEAR, "_", alpage, "_viterbi_parc.rds"))
  
  
  readRDS(input_parc_rds_file)
  
  
  
  # Chargement des librairies
  library(data.table)
  library(lubridate)
  
  # 1) Lecture du RDS
  dt <- as.data.table(readRDS(input_parc_rds_file))
  
  # 2) Extraction de la date
  dt[, date := as.Date(time)]
  
  # 3) Tri par alpage, parc et date
  setorder(dt, alpage, parc, date)
  
  # 4) Calcul des écarts en jours et découpage en périodes
  dt[
    , diff_jours := as.integer(date - shift(date)), 
    by = .(alpage, parc)
  ][
    , periode_id := 1 + cumsum(diff_jours > 5 | is.na(diff_jours)), 
    by = .(alpage, parc)
  ]
  
  # 5) Construction du tableau des périodes ≥ 4 jours
  periodes <- dt[
    , .(
      date_arrivee = min(date),
      date_depart  = max(date),
      duree        = as.integer(max(date) - min(date)) + 1L
    ),
    by = .(alpage, parc, periode_id)
  ][
    duree >= 4
  ]
  
  # 6) Sélection de la période la plus longue par alpage + parc
  best <- periodes[
    , .SD[which.max(duree)], 
    by = .(alpage, parc)
  ]
  
  # 7) Fonction de libellé “quartier” arrondi aux quinzaines
  label_quartier_scalar <- function(d1, d2) {
    dur <- as.integer(d2 - d1) + 1L
    if (dur <= 7L) {
      return(paste0(format(d1, "%d %b"), " à ", format(d2, "%d %b")))
    }
    m1 <- month(d1); m2 <- month(d2)
    j1 <- day(d1);   j2 <- day(d2)
    lm2 <- days_in_month(d2)
    nom_mois <- function(m) tolower(format(ISOdate(2000,m,1), "%B"))
    
    lbl_d <- if (j1 <= 15) paste0("début ", nom_mois(m1)) else paste0("mi-", nom_mois(m1))
    lbl_f <- if (j2 >= (lm2 - 2)) paste0("fin ", nom_mois(m2))
    else if (j2 >= 15) paste0("mi-", nom_mois(m2))
    else paste0("début ", nom_mois(m2))
    
    paste(lbl_d, "à", lbl_f)
  }
  label_quartier <- Vectorize(label_quartier_scalar, c("d1","d2"))
  
  # 8) Construction du tableau final
  final_table <- best[, .(
    alpage,
    parc,
    date_debut = date_arrivee,
    date_fin   = date_depart,
    période    = label_quartier(date_arrivee, date_depart)
  )]
  
  # 9) Création de la colonne 'rename' sans le numéro du parc, mais avec dates
  final_table[
    , rename := paste0(
      "parc_",
      format(date_debut, "%Y-%m-%d"), "_",
      format(date_fin,   "%Y-%m-%d")
    )
  ]
  
  # 10) Affichage ou export
  print(final_table)
  # fwrite(final_table, "résumé_périodes_par_parc.csv")
  
  
  
}

  
  
  
  
  
  
  
  
# A mettre dans visu !
  



data_sf <- readRDS(input_consensus_file)
# nombre de lignes (ID×date) avec TRUE
n_transitions <- sum(data_sf$jour_de_transition, na.rm = TRUE)
message("Nombre de paires ID-date en transition : ", n_transitions)

# (optionnel) nombre de dates uniques en transition
n_dates <- data_sf %>%
  dplyr::filter(jour_de_transition) %>%
  dplyr::pull(date) %>%
  unique() %>%
  length()
message("Nombre de jours (dates) de transition : ", n_dates)









# Choix : conserver ou non les jours de transition dans les trajectoires
keep_transition_days <- TRUE  # passe à FALSE pour les exclure

for (alpage in alpages) {
  
  # 0) chemins
  case_state_dir       <- file.path(output_dir, "3. HMM_comportement")
  input_consensus_file <- file.path(
    case_state_dir,
    paste0("Catlog_", YEAR, "_", alpage, "_viterbi_parc_v10.rds")  # <- ton RDS à 8 dates
  )
  output_gpkg_file     <- file.path(
    case_state_dir,
    paste0("Trajectoire_by_night_parc_", YEAR, "_", alpage, "_V15.gpkg")
  )
  
  # 1) Lecture + conversion en sf
  data_sf <- readRDS(input_consensus_file) %>%
    sf::st_as_sf(coords = c("x", "y"), crs = 2154)
  
  # 2) On détermine LES 8 dates de transition
  transition_dates <- data_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(jour_de_transition) %>%
    dplyr::distinct(date)
  
  n_dates <- nrow(transition_dates)
  message("Alpage ", alpage, " : ", n_dates,
          " jour(s) de transition détecté(s).")  # => devrait afficher 8
  
  # 3) (Optionnel) on exclut ou pas ces jours
  if (!keep_transition_days) {
    data_sf <- data_sf %>%
      dplyr::filter(! date %in% transition_dates$date)
    message("→ Les jours de transition ont été exclus.")
  } else {
    message("→ Les jours de transition ont été conservés.")
  }
  
  # 4) Construction des trajectoires par ID / date / parc
  traj_sf <- data_sf %>%
    dplyr::arrange(time) %>%
    dplyr::group_by(ID, date, parc, jour_de_transition) %>%
    dplyr::summarise(
      geometry = sf::st_combine(geometry),
      .groups  = "drop"
    ) %>%
    sf::st_cast("LINESTRING")
  
  # 5) Écriture dans un Geopackage (on écrase d'abord si besoin)
  if (file.exists(output_gpkg_file)) file.remove(output_gpkg_file)
  
  sf::st_write(
    data_sf,
    output_gpkg_file,
    layer        = "points",
    driver       = "GPKG"
  )
  sf::st_write(
    traj_sf,
    output_gpkg_file,
    layer        = "trajectoires",
    driver       = "GPKG"
  )
}





# A mettre dans visu !












#### 4. FLOCK STOCKING RATE (charge) BY DAY AND BY STATE ####
#-------------------------------------------------------------#
if (TRUE){
  
  ## DESCRIPTION ##
  # Calcul du taux de chargement selon la méthode "BBMM"
  
  # Nécessite :
  # - .csv : "AAAA_infos_alpages.csv"
  # - fichier .rds contenant les données par comportement : "Catlog_AAAA_alpage_demo_viterbi.rds"
  
  # En sortie :
  # - Un fichier .rds avec le chargement par jour et par comportement :
  #   outputs/4. Chargements_calcules/by_day_and_state_AAAA_alpage_demo
  # - Un fichier .rds avec le chargement par jour :
  #   outputs/4. Chargements_calcules/by_day_AAAA_alpage_demo
  # - Un fichier .rds avec le chargement par comportement :
  #   outputs/4. Chargements_calcules/by_state_AAAA_alpage_demo
  # - Un fichier .rds avec le chargement total :
  #   outputs/4. Chargements_calcules/total_AAAA_alpage_demo
  
  
  ## LIBRARY ##
  library(adehabitatHR)
  library(data.table)
  library(snow)
  source(file.path(functions_dir, "Functions_map_plot.R"))
  source(file.path(functions_dir, "Functions_flock_density.R"))
  
  ## ENTREES ##
  # Un .RDS contenant les trajectoires catégorisées par comportement
  input_rds_file <- file.path(output_dir, "3. HMM_comportement",  paste0("Catlog_", YEAR, "_", alpage, "_viterbi.rds"))
  
  # Un .csv  "infos_alpages" remplis selon le model du jeu démo
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  check_and_correct_csv(csv_path = AIF)
  
  # Un data.frame contenant les tailles de troupeaux et les évolutions des tailles en fonction de la date
  raw_data_dir <- file.path(data_dir, paste0("Colliers_", YEAR, "_brutes"))
  flock_size_file <- file.path(raw_data_dir, paste0(YEAR, "_tailles_troupeaux.csv"))
  check_and_correct_csv(csv_path = flock_size_file)
  #str(read.csv(flock_size_file, stringsAsFactors = FALSE, encoding = "UTF-8"))
  
  
  ## SORTIES ##
  # Dossier de sortie
  save_dir <- file.path(output_dir, "4. Chargements_calcules")
  
  # Un .RDS par alpage contenant les charges journalières par comportement
  state_daily_rds_prefix <- paste0("by_day_and_state_", YEAR, "_")
  # Un .RDS par alpage contenant les charges journalières
  daily_rds_prefix <- paste0("by_day_", YEAR, "_")
  # Un .RDS par alpage contenant les charges par comportement
  state_rds_prefix <- paste0("by_state_", YEAR, "_")
  # Un .RDS par alpage contenant la charge totale sur toute la saison
  total_rds_prefix <- paste0("total_", YEAR, "_")
  
  
  ## CODE ##
  h <- 25 # Distance caractéristique pour calculer le chargement
  
  for (alpage in alpages) {
    flock_sizes <- get_flock_size_through_time(alpage, flock_size_file)
    prop_time_collar_on <- get_alpage_info(alpage, AIF, "proportion_jour_allume")
    
    # Chargement des données filtrées pour l'alpage
    data <- readRDS(input_rds_file)
    data <- data[data$alpage == alpage,]
    
    if(FALSE){
      # Chargement du raster de phénologie avec le bon chemin
      raster_file <- file.path(raster_dir, paste0("ndvis_", YEAR,"_",alpage, "_pheno_metrics.tif"))
      pheno_t0 <- get_raster_cropped_L93(raster_file, get_minmax_L93(data, 100), reproject = TRUE, band = 2, as = "SpatialPixelDataFrame")
    }
    
    # Définition du dossier de stockage spécifique à l'alpage
    alpage_save_dir <- file.path(save_dir, paste0(YEAR, "_", alpage))
    if (!dir.exists(alpage_save_dir)) dir.create(alpage_save_dir, recursive = TRUE)
    
    
    # BY day and by state 
    
    # calcul du chargement basé sur une grille automatique (pixélisation)
    if(TRUE){
      flock_load_by_day_and_state_to_rds_kernelbb_Auto_grid(data, alpage_save_dir,state_daily_rds_prefix, flock_sizes,prop_time_collar_on)
    }
    
    # calcul du chargement basé sur le raster NDVI (inadapté pour les autres utilisateurs)
    if(FALSE){
      flock_load_by_day_and_state_to_rds_kernelbb_NDVI_grid(data, grid, save_dir, save_rds_name, flock_sizes, prop_time_collar_on)
    }
    
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
    
    
}
    ### 4.1 Extensions avec le recalcule du chargement avec la notion de parc ###
    ###-----------------------------------------------------------------------###
    if (TRUE){
      # PARTIE 1 : recalcule par parc de nuit
      for (alpage in alpages){
        # ENTREE
        # Dossier
        case = file.path(output_dir, "4. Chargements_calcules")
        load_case = file.path(case, paste0(YEAR, "_", alpage))
        # Un .RDS par alpage contenant les charges journalières par comportement
        input_load_day_state_rds = file.path(load_case, paste0("by_day_and_state_", YEAR, "_",alpage,".rds"))
        
        # Dossier contenant les fichiers du comportement
        case_state_file = file.path(output_dir, "3. HMM_comportement")
        input_parc_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi_parc.rds"))
        
        
        
        # SORTIE
        output_park_day_state_rds <- file.path(
          load_case,
          paste0("by_park_day_and_state_", YEAR, "_", alpage, ".rds")
        )
        output_park_state_rds     <- file.path(
          load_case,
          paste0("by_park_and_state_", YEAR, "_", alpage, ".rds")
        )
        output_park_rds           <- file.path(
          load_case,
          paste0("by_park_", YEAR, "_", alpage, ".rds")
        )
        
        
        output_park_day_state_transition_filtered_rds <- file.path(
          load_case,
          paste0("by_park_day_and_state_transition_filtered_", YEAR, "_", alpage, ".rds")
        )
        output_park_state_transition_filtered_rds     <- file.path(
          load_case,
          paste0("by_park_and_state_transition_filtered_", YEAR, "_", alpage, ".rds")
        )
        output_park_transition_filtered_rds           <- file.path(
          load_case,
          paste0("by_park_transition_filtered_", YEAR, "_", alpage, ".rds")
        )
        
        if (FALSE){
        compute_charge_by_park(input_load_day_state_rds, input_parc_rds_file, 
                               output_park_day_state_rds, output_park_state_rds, 
                               output_park_rds)
        
        }
        
        if (FALSE){
        compute_charge_by_park_no_transition(input_load_day_state_rds, input_parc_rds_file,
                                             output_park_day_state_transition_filtered_rds, 
                                             output_park_state_transition_filtered_rds,
                                             output_park_transition_filtered_rds)
        }
          
          
        if (TRUE) {
          compute_charge_by_park_no_transition_chunked(
            input_load_day_state_rds,
            input_parc_rds_file,
            output_park_day_state_transition_filtered_rds, 
            output_park_state_transition_filtered_rds,
            output_park_transition_filtered_rds,
            chunk_size = 20
          )
        }
        
        
        
        
      }
        
      # PARTIE 2 : Attrubution des noms de parc corretce
      # PARTIE 2.A
      # ENTREE
      # Dossier contenant les fichiers du comportement
      case_state_file = file.path(output_dir, "3. HMM_comportement")
      input_parc_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi_parc.rds"))
      
     
      # SORTIE
      #Un .RDS avec les date d'utilisation de chaque parc 
      output_table_use_parc = file.path(load_case, paste0("info_table_use_parc", YEAR, "_", alpage, ".rds"))
      
      
      # CODE 
      use_date_parc(input_parc_rds_file,output_table_use_parc)
      
     
      
      # PARTIE 2.B
      
      # ENTREE
      case = file.path(output_dir, "4. Chargements_calcules")
      load_case = file.path(case, paste0(YEAR, "_", alpage))
      
      #Un .RDS avec les date d'utilisation de chaque parc 
      input_table_use_parc = file.path(load_case, paste0("info_table_use_parc", YEAR, "_", alpage, ".rds"))
      
      # Un .RDS avce les parc
      input_park_day_state_transition_filtered_rds <- file.path(
        load_case,
        paste0("by_park_day_and_state_transition_filtered_", YEAR, "_", alpage, ".rds")
      )
      input_park_state_transition_filtered_rds     <- file.path(
        load_case,
        paste0("by_park_and_state_transition_filtered_", YEAR, "_", alpage, ".rds")
      )
      input_park_transition_filtered_rds           <- file.path(
        load_case,
        paste0("by_park_transition_filtered_", YEAR, "_", alpage, ".rds")
      )
      
      # SORTIE
      
      # Un .RDS avce les parc renommé
      output_park_day_state_transition_filtered_rds <- file.path(
        load_case,
        paste0("by_park_day_and_state_transition_filtered_", YEAR, "_", alpage, ".rds")
      )
      output_park_state_transition_filtered_rds     <- file.path(
        load_case,
        paste0("by_park_and_state_transition_filtered_", YEAR, "_", alpage, ".rds")
      )
      output_park_transition_filtered_rds           <- file.path(
        load_case,
        paste0("by_park_transition_filtered_", YEAR, "_", alpage, ".rds")
      )
      
      #CODE
      # Exemple d’appel :
      rename_parc_in_loads(
        input_table_use_parc = input_table_use_parc,
        input_park_day_state  = input_park_day_state_transition_filtered_rds,
        input_park_state      = input_park_state_transition_filtered_rds,
        input_park            = input_park_transition_filtered_rds,
        output_park_day_state = output_park_day_state_transition_filtered_rds,
        output_park_state     = output_park_state_transition_filtered_rds,
        output_park           = output_park_transition_filtered_rds
      )
      
        
        
        
        
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      

    
    
    
    
    
    
    
    
    
    
    
  
  
  
  




























