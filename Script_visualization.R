#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#

gc()

# Chargement de la configuration
source("config.R")
source(file.path(functions_dir, "Functions_filtering.R")) 


# Définition de l'année d'analyse
YEAR <- 2024
YEARS <- c(2022, 2023, 2024)
TYPE <- "catlog" #Type de données d'entrée (CATLOG, OFB )
alpage <- "Cayolle"
alpages <- "Cayolle"
# Liste complète des alpages 2023 : "Cayolle", "Crouzet", "Grande-Cabane", "Lanchatra", "Rouanette", "Sanguiniere", "Vacherie-de-Roubion", "Viso"
# Liste complète des alpages 2022 : "Cayolle", "Combe-Madame", "Grande-Fesse", "Jas-des-Lievres", "Lanchatra", "Pelvas", "Sanguiniere", "Viso"

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





#### 0.1 FORMATAGE HOLD DATA 22 & 23 ####
#---------------------------------------#
if (FALSE) {
#Library
source(file.path(functions_dir, "Functions_Import_hold_data.R")) 

# ENTREES
# Définir l'année traité (alpage non-determinant pour cette partie)
# Définir les chemins d'entrée
case_file <- file.path(data_dir, "Data_2022_2023_format_v1")
input_rds_file <- file.path(case_file, paste0("Catlog_", YEAR, "_viterbi.rds"))
input_rds_list


# SORTIES
#La sortie ce fait directement dans le dossier outputs "HMM_comportement"


# CODE

#Comportement
filtering_and_saving_viterbi_by_alpage(input_rds_file, output_dir, YEAR)
}

#### 1. Extraction des raster CHARGEMENT ####
#-------------------------------------------#
if (FALSE) {
  # Exctraction des raster au format tif 
  # Génération de différent tif : 
  # - Chargement total
  # - Chargement par comportement
  # - Chargement par jour 
  
  # LIBRARY & FUNCTION
  library(raster)
  library(sf)
  library(ggplot2)
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  for (YEAR in YEARS){
  for(alpage in alpages){
  # ENTREE
  #Dossier contenant les sous dossier des chargement
  case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
  #Dossier contenant les fichiers du tot de chargement
  case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
  
  # Un .RDS par alpage contenant les charges journalières
  daily_rds_prefix = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
  daily_rds_file = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
  # Un .RDS par alpage contenant les charges par comportement
  state_rds_prefix = file.path(case_flock_alpage_file, paste0("by_state_",YEAR,"_",alpage,".rds"))
  # Un .RDS par alpage contenant la charge totale sur toute la saison
  total_rds_prefix = file.path(case_flock_alpage_file, paste0("total_",YEAR,"_",alpage,".rds"))
  # Un .RDS par parc et état
  park_state_rds <- file.path(case_flock_alpage_file, paste0("by_park_and_state_", YEAR,"_",alpage,".rds"))
  # Un .RDS par parc
  park_tot_rds   <- file.path(case_flock_alpage_file, paste0("by_park_", YEAR,"_",alpage,".rds"))
  # Un .RDS par parc et état filtré par 
  park_state_filtered_rds <- file.path(case_flock_alpage_file, paste0("by_park_and_state_transition_filtered_", YEAR,"_",alpage,".rds"))
  # Un .RDS par parc
  park_tot_filterd_rds   <- file.path(case_flock_alpage_file, paste0("by_park_transition_filtered_", YEAR,"_",alpage,".rds"))
  
  
  # Un dossier contenant les ratsers des Unités Pastorales (UP)
  case_UP_file = file.path(raster_dir, "UP")
  # Un .SHP avec les Unités pastorales UP
  UP_file = file.path(case_UP_file, "v1_bd_shape_up_inra_2012_2014_2154_all_emprise.shp")
  
  # Un dossier contenant les Infos sur les alpages
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  # Un data.frame contenant les dates de pose et de retrait des colliers
  alpage_info_file <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  
  # SORTIE 
  
  #Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  #Création du sous-dossier Indicateur traitée : Chargement
  output_chargement_case <- file.path(output_visu_case, "Taux_chargement")
  if (!dir.exists(output_chargement_case)) {
    dir.create(output_chargement_case, recursive = TRUE)
  }
  #Création du sous-sous-dossier alpage et années traitée : Chargement
  output_case_alpage <- file.path(output_chargement_case, paste0(YEAR,"_",alpage))
  if (!dir.exists(output_case_alpage)) {
    dir.create(output_case_alpage, recursive = TRUE)
  }
  
  #Créeation du sous dossier pour les park
  output_case_parc <- file.path(output_case_alpage, paste0("Parc_",YEAR))
  if (!dir.exists(output_case_parc)) {
    dir.create(output_case_parc, recursive = TRUE)
  }
  
  
  
  # Un .TIF par alpage contenant les charges par comportement
  output_flock_repos_tif = file.path(output_case_alpage, paste0("repos",YEAR,"_",alpage,".tif"))
  output_flock_deplacement_tif = file.path(output_case_alpage, paste0("deplacement",YEAR,"_",alpage,".tif"))
  output_flock_paturage_tif = file.path(output_case_alpage, paste0("paturage",YEAR,"_",alpage,".tif"))
  output_flock_repos_tif_crop = file.path(output_case_alpage, paste0("repos",YEAR,"_",alpage,"_crop.tif"))
  output_flock_deplacement_tif_crop = file.path(output_case_alpage, paste0("deplacement",YEAR,"_",alpage,"_crop.tif"))
  output_flock_paturage_tif_crop = file.path(output_case_alpage, paste0("paturage",YEAR,"_",alpage,"_crop.tif"))
  # Un .TIF par alpage contenant la charge totale sur toute la saison
  output_flock_tot_tif = file.path(output_case_alpage, paste0("total_",YEAR,"_",alpage,".tif"))
  output_flock_tot_tif_crop = file.path(output_case_alpage, paste0("total_",YEAR,"_",alpage,"_crop.tif"))
  
  
  # CODE

  #Indicateur : Charge total .TIF
  if (FALSE) {
  total_flock_load_tif(total_rds_prefix, output_flock_tot_tif, output_flock_tot_tif_crop, UP_file, alpage, alpage_info_file)
    }
  
  
  #Indicateur : Charge_by_state
  if (FALSE) {
  state_flock_load_tif(state_rds_prefix,output_flock_repos_tif,output_flock_deplacement_tif, output_flock_paturage_tif,
                       output_flock_repos_tif_crop, output_flock_deplacement_tif_crop , output_flock_paturage_tif_crop,
                       UP_file, alpage, alpage_info_file)
    }
  
  #Indicateur : Charge_by_day
  if (FALSE){
    res_raster <- 10 # ou la valeur que tu souhaites explicitement
    day_flock_load_tif_nostack(daily_rds_prefix, output_case_alpage, UP_file, alpage, alpage_info_file, YEAR, res_raster, CROP = "YES")
    
    
  }
  
  #Indicateur : Charge par quinzaine
  if (FALSE){
  quinzaine_flock_load_tif_nostack(daily_rds_file = daily_rds_file,
                                   output_case_alpage = output_case_alpage,
                                   UP_file = UP_file,
                                   alpage = alpage,
                                   alpage_info_file = alpage_info_file,
                                   YEAR = YEAR,
                                   res_raster = 10,
                                   CROP = "NO")
  }
  
  
  #Indicateur : Charge par parc
  if (FALSE){
    park_total_flock_load_tif(
      park_rds    = park_tot_rds,
      output_dir  = output_case_alpage,
      YEAR        = YEAR,
      alpage      = alpage,
      res_raster  = 10              # ou la résolution souhaitée
    )
  }
  
  if (FALSE){
  
  #Indicateur : Charge par parc et état
  park_state_flock_load_tif(park_state_rds,
                            output_case_alpage,
                            UP_file, alpage, alpage_info_file)
  }
  
 
  
  
  #Indicateur : Charge par parc filtered
  if (TRUE){
    park_total_flock_load_tif_filtered(
      park_rds    = park_tot_filterd_rds,
      output_dir  = output_case_parc,
      YEAR        = YEAR,
      alpage      = alpage,
      res_raster  = 10              
    )
  }
  
  if (FALSE){
    
    #Indicateur : Charge par parc et état filtered
    park_state_flock_load_tif_filtered(park_state_filtered_rds ,
                              output_case_alpage,
                              UP_file, alpage, alpage_info_file)
  }
  
  
  
  
  
  }
  
  
  }
  
 }

#### 2. Calcul de la distance et du denivelé ####
#-----------------------------------------------#
if (FALSE) {
  # Calcul de la distance et du denivelé par jour sur l'ensemble des colliers
  # Le tout stocké par alpage et par année souys forme d'un csv
  
  
  
  
  library(dplyr)
  library(lubridate)
  library(terra)
  library(moveHMM) 
  source(file.path(functions_dir, "Functions_Indicateurs.R"))

  for(alpage in alpages){
  # ENTREES
  
  # Dossier contenant les fichiers du comportement
  case_state_file = file.path(output_dir, "3. HMM_comportement")
  # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
  state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
  
  # Un raster d’altitude
  altitude_raster = file.path(raster_dir,"BDALTI.tif")
  
  
  # SORTIE 
  
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Chargement
  output_distance_case <- file.path(output_visu_case, "Distance_&_Denivele")
  if (!dir.exists(output_distance_case )) {
    dir.create(output_distance_case , recursive = TRUE)}
  
  # Un .csv avec la distance et le denivelé par jour par collier
  distance_csv_file <- file.path(output_distance_case, paste0(YEAR,"_",alpage,"_distance_denivele.csv"))
  
  #CODE 
  
  save_distance_denivele(state_rds_file, distance_csv_file , altitude_raster, output_distance_case, YEAR, alpage)
  
  }
  
  
}
  
#### 3. Calcul date de pature ####
#--------------------------------#
if (FALSE) {
  # Calcul de la date de mise en pature de chaque espace pixel par pixel lorsque
  # le chargement dépasse un seuil de paturage on concidère la mise en pature du
  # pixel ainsi le jour julien et noté pour chaque pixel 
  
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  
  # ENTREE
  #Dossier contenant les sous dossier des chargement
  case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
  #Dossier contenant les fichiers du tot de chargement
  case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
  
  # Un .RDS par alpage contenant les charges journalières
  daily_rds_prefix = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
  
  
  # SORTIE 
  #Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  #Création du sous-dossier Indicateur traitée : Espace paturé
  output_espace_pature_case <- file.path(output_visu_case, "Espace_pature")
  if (!dir.exists(output_espace_pature_case)) {
    dir.create(output_espace_pature_case, recursive = TRUE)
  }
  #Création du sous-sous-dossier alpage et années traitée : Chargement
  output_case_alpage <- file.path(output_espace_pature_case, paste0(YEAR,"_",alpage))
  if (!dir.exists(output_case_alpage)) {
    dir.create(output_case_alpage, recursive = TRUE)
  }
  
  # Un .rds avec le jour de déblocage de la zone paturée
  output_new_grazed_area_rds = file.path(output_case_alpage, paste0("new_grazed_area_",YEAR,"_",alpage,".rds"))
  # Un .TIF avec le jour de déblocage de la zone paturée
  output_new_grazed_area_tif = file.path(output_case_alpage, paste0("new_grazed_area_",YEAR,"_",alpage,".tif"))
  
  # Un .rds avec le nombre de jour dont le pixel est paturée
  output_nb_grazing_rds = file.path(output_case_alpage, paste0("nb_grazing_day_",YEAR,"_",alpage,".rds"))
  # Un .TIF avec le nombre de jour dont le pixel est paturée
  output_nb_grazing_tif = file.path(output_case_alpage, paste0("nb_grazing_day_test",YEAR,"_",alpage,".tif"))
  
  #CODE


  
  if(FALSE){
  generate_new_grazed_area(daily_rds_prefix, output_new_grazed_area_rds, output_new_grazed_area_tif, threshold = 1 ) #Valeur de filtre du chargement
  }
  
  
  if(TRUE){
    generate_new_grazed_area_by_day(daily_rds_prefix,output_case_alpage,YEAR,alpage,threshold = 1,crs_string = "+init=epsg:2154")
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  count_nb_grazing_days <- function(daily_rds_prefix,
                                    output_nb_grazing_rds,
                                    output_nb_grazing_tif,
                                    threshold = 10) {
    # Chargement des librairies nécessaires
    library(dplyr)
    library(raster)
    threshold = 1
    # Lecture des données journalières
    daily_data <- readRDS(daily_rds_prefix)
    
    # Vérification de la présence des colonnes nécessaires
    required_cols <- c("x", "y", "day", "Charge", "state")
    if (!all(required_cols %in% names(daily_data))) {
      stop("Les colonnes x, y, day, Charge ou state sont manquantes dans les données.")
    }
    
    # 1) Filtrer pour ne garder que l'état 'Paturage'
    # 2) Agréger par pixel et jour pour obtenir la somme de Charge du jour
    # 3) Créer un flag (1/0) si la Charge dépasse le threshold
    grazing_data <- daily_data %>%
      filter(state == "Paturage") %>%
      group_by(x, y, day) %>%
      summarize(Charge_day = sum(Charge, na.rm = TRUE), .groups = "drop") %>%
      mutate(grazing_flag = if_else(Charge_day >= threshold, 1, 0))
    
    # 4) Calcul du nombre total de jours paturés par pixel
    nb_grazing_data <- grazing_data %>%
      group_by(x, y) %>%
      summarize(nb_grazing_day = sum(grazing_flag), .groups = "drop")
    
    # Sauvegarde du résultat au format RDS
    saveRDS(nb_grazing_data, file = output_nb_grazing_rds)
    cat("Fichier RDS créé avec 'nb_grazing_day' :", output_nb_grazing_rds, "\n")
    
    # Conversion en data.frame si nécessaire
    nb_grazing_data_df <- as.data.frame(nb_grazing_data)
    
    # 5) Création du raster : 1ère col = x, 2ème = y, 3ème = nb_grazing_day
    rast <- rasterFromXYZ(nb_grazing_data_df, crs = CRS("+init=epsg:2154"))
    
    # 6) Export du raster
    writeRaster(rast, filename = output_nb_grazing_tif, format = "GTiff", overwrite = TRUE)
    cat("Raster sauvegardé avec succès :", output_nb_grazing_tif, "\n")
    gc()
    return(nb_grazing_data)
  }
  
  
  
  
}
  
#### 4. Vecteur du comportement ####
#----------------------------------#  
if (FALSE){
  #Library
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  library(sf)
  library(dplyr)
  
  
  for(alpage in alpages){
    
  # ENTREES
  # Dossier contenant les fichiers du comportement
  case_state_file = file.path(output_dir, "3. HMM_comportement")
  # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
  state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
  data = readRDS(state_rds_file)
  
  
  if (TYPE == "ofb"){
  # Charger le fichier des périodes d'échantillonnage
  sampling_period_file <- file.path(output_dir, "0. Sampling_Periods", paste0("Sampling_Periods_", YEAR, "_", alpage, ".rds"))
  sampling_periods <- readRDS(sampling_period_file)
  }
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_state_traj_case <- file.path(output_visu_case, "Trajectoiree_par_comportement")
  if (!dir.exists(output_state_traj_case)) {
    dir.create(output_state_traj_case, recursive = TRUE)
  }
  
  # Un .shp contenant les données de trajectoires catégorisées par comportement et collier
  # Généré autaumatiquement dans la fonctions et déposé dans le dossier si dessus
 
  
  
  # CODE
  if (TYPE == "ofb"){
  generate_trajectory_gpkg_ofb(state_rds_file, output_state_traj_case, YEAR, alpage, sampling_interval = 10, sampling_periods = sampling_periods)
  }
  
  if (TYPE == "catlog"){
  generate_trajectory_gpkg_catlog(state_rds_file,output_state_traj_case,YEAR,alpage,sampling_interval = 10)#Point toute les 30 minutes (reglé de base a 10)
  }
  
  }
  
  
  
  
 }
  
#### 5. Polygon d'utilisation  ####
#---------------------------------#  
if (FALSE){
  
  library(sf)
  library(dplyr)
  library(viridis)
  library(ggplot2)
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  for(YEAR in YEARS){
  for (alpage in alpages){
  # ENTREES
  # Dossier contenant les fichiers du comportement
  case_state_file = file.path(output_dir, "3. HMM_comportement")
  # Un .RDS contenant les trajectoires 
  state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
  # Un .RDS contenant les trajectoires par PARC
  viterbi_parc_rds <- file.path(case_state_file, paste0("Catlog_", YEAR, "_", alpage, "_viterbi_parc_renamme.rds"))
  
  
  # Un dossier contenant les ratsers des Unités Pastorales (UP)
  case_UP_file = file.path(raster_dir, "UP")
  # Un .SHP avec les Unités pastorales UP
  UP_file = file.path(case_UP_file, "v1_bd_shape_up_inra_2012_2014_2154_all_emprise.shp")
  
  # Un dossier contenant les Infos sur les alpages
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  # Un data.frame contenant les dates de pose et de retrait des colliers
  AIF <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_polygon_case <- file.path(output_visu_case, "Utilisation_par_quinzaine")
  if (!dir.exists(output_polygon_case)) {
    dir.create(output_polygon_case, recursive = TRUE)
  }
  
  # Un .shp contenant les données de trajectoires catégorisées par comportement et collier
  output_polygon_use_shp = file.path(output_polygon_case, paste0("Use_polygon_",YEAR,"_",alpage,".shp"))
  
  output_shp <- file.path(output_polygon_case, paste0("Use_polygon_parc_", YEAR, "_", alpage, ".shp"))
  
  # CODE 
  # Ancienne fonction
  if (FALSE){
  generate_presence_polygons(state_rds_file,output_polygon_use_shp,YEAR,alpage,density_threshold = 1e-06,n_grid = 200,small_poly_threshold_percent = 0.05)
    # Seuil pour filtrer les petits polygones
    # Ajustez pour être plus ou moins restrictif
  }
  
  if(FALSE){
    generate_presence_polygons_by_percentage(state_rds_file, output_polygon_use_shp, YEAR, alpage, percentage = 0.85 ,n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )
    
    
  }
    
  
  
  if (TRUE){
    
    generate_presence_polygons_by_parc(
    state_rds_file,
    viterbi_parc_rds,
    output_shp,
    YEAR,
    alpage,
    percentage                  = 0.9,
    n_grid                      = 200,
    small_poly_threshold_percent = 0.05,
    crs                         = 2154
  )
  }
  }
  }
  
  
  
  
  
  
  
  
  
  
  
    
  if (FALSE){
    generate_presence_polygons_by_percentage_per_month(state_rds_file, output_polygon_use_shp, YEAR, alpage, percentage = 0.85 ,n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )  
  } 
    
    
  
  
  }

#### 6. Carto et fond de carte ####
#---------------------------------#
if (FALSE){
  #LIBRARY
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  
  #ENTREE
  # Un dossier contenant les ratsers des Unités Pastorales (UP)
  case_UP_file = file.path(raster_dir, "UP")
  # Un .SHP avec les Unités pastorales UP
  UP_file_mask = file.path(case_UP_file, paste0("UP_",alpage,"_bis1.shp"))
  
  # Un Ratser hill de la zone d'étude
  hillShade_file = file.path(raster_dir, paste0("hillshade_",alpage,".tif"))
  
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_carto_case <- file.path(output_dir, "6. Carto_Fond_Carte")
  if (!dir.exists(output_carto_case)) {
    dir.create(output_carto_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_hill_case <- file.path(output_carto_case, "Hillshade")
  if (!dir.exists(output_hill_case)) {
    dir.create(output_hill_case, recursive = TRUE)
  }
  
  # Un .shp contenant les données de trajectoires catégorisées par comportement et collier
  output_hill_shade_tif = file.path(output_hill_case, paste0("Hillshade_",alpage,"_crop_bis3.tif"))
  
  #CODE
  
  crop_hillshape(UP_file_mask, hillShade_file, output_hill_shade_tif)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

#### 7. Création de GIF ####
#--------------------------#
if (FALSE){
  #LIBRARY
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  #ENTREE
  # Un dossier contenant les ratsers des Unités Pastorales (UP)
  case_GIF_file = file.path(raster_dir, "Image_GIF_greenwave")
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
  if (!dir.exists(output_Plot_Animation_case)) {
    dir.create(output_Plot_Animation_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_GIF_case <- file.path(output_Plot_Animation_case, "GIF")
  if (!dir.exists(output_GIF_case)) {
    dir.create(output_GIF_case, recursive = TRUE)
  }
  
  # Un .GIF contenant les données de trajectoires catégorisées par comportement et collier
  output_gif = file.path(output_GIF_case, "Gif_Cayolle_greenwave_2022.gif")
  
  # CODE
  
  if(TRUE){
  create_gif_from_images(case_GIF_file,output_gif,file_pattern = "Carte_cayolle.*\\.(png|jpg)$",delay_seconds = 1.25)
  }
  
  if(TRUE){
  create_gif_from_images_day_bis (case_GIF_file,output_gif,file_pattern = "Band.*\\.(png|jpg)$",delay_seconds = 0.5)
  }
  
 

}

#### 8. Graph Dénivelé et distance ####
#-------------------------------------#
if (FALSE){
  #LIBRARY
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  #ENTREE
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Chargement
  output_distance_case <- file.path(output_visu_case, "Distance_&_Denivele")
  if (!dir.exists(output_distance_case )) {
    dir.create(output_distance_case , recursive = TRUE)}
  
  # Un .csv avec la distance et le denivelé par jour par collier
  distance_csv_file = file.path(output_distance_case, paste0(YEAR,"_",alpage,"_distance_denivele.csv"))
  
 
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
  if (!dir.exists(output_Plot_Animation_case)) {
    dir.create(output_Plot_Animation_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_dit_deniv_case <- file.path(output_Plot_Animation_case, "Distance_&_Denivele")
  if (!dir.exists(output_dit_deniv_case)) {
    dir.create(output_dit_deniv_case, recursive = TRUE)
  }
  
  # Un .GIF contenant les données de trajectoires catégorisées par comportement et collier
  plot_Dist_Deniv_jpg = file.path(output_dit_deniv_case, paste0("plot_dist_denic_",YEAR,"_",alpage,".jpeg"))
  
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(patchwork)
  data <- read.csv(distance_csv_file)
  # 0) Préparation des données + normalisation de 0 à 1
  data2 <- data %>%
    arrange(date) %>%
    mutate(
      date      = as.Date(date),
      fill_dist = (distance - min(distance)) /
        (max(distance) - min(distance)),
      fill_deniv= (denivelation - min(denivelation)) /
        (max(denivelation) - min(denivelation))
    )
  
  # 1) Fonction pour générer de vrais trapèzes entre jours i et i+1
  make_sloped_traps <- function(df, xvar, yvar, fillvar) {
    map_dfr(seq_len(nrow(df) - 1), function(i) {
      tibble(
        id   = i,
        x    = c(df[[xvar]][i],   df[[xvar]][i+1],
                 df[[xvar]][i+1], df[[xvar]][i]  ),
        y    = c(0,               0,
                 df[[yvar]][i+1], df[[yvar]][i]),
        fill = (df[[fillvar]][i] + df[[fillvar]][i+1]) / 2
      )
    })
  }
  
  traps_dist  <- make_sloped_traps(data2, "date", "distance",     "fill_dist")
  traps_deniv <- make_sloped_traps(data2, "date", "denivelation", "fill_deniv")
  
  # 2) Palette douce et thème minimal
  my_pal  <- c("#00441b", "#41ab5d", "#fed976", "#cb181d")
  th_base <- theme_classic(base_family = "Helvetica", base_size = 14) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0))
  
  # 3) Profil Distance avec trapèzes inclinés
  p_dist <- ggplot() +
    geom_polygon(
      data = traps_dist,
      aes(x = x, y = y, group = id, fill = fill),
      color = NA
    ) +
    geom_line(
      data = data2,
      aes(x = date, y = distance),
      color = "black", size = 0.8
    ) +
    scale_fill_gradientn(colors = my_pal, limits = c(0,1), guide = "none") +
    scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%B") +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Distance quotidienne", y = "Distance (m)", x = NULL) +
    th_base
  
  # 4) Profil Dénivelé avec trapèzes inclinés
  p_deniv <- ggplot() +
    geom_polygon(
      data = traps_deniv,
      aes(x = x, y = y, group = id, fill = fill),
      color = NA
    ) +
    geom_line(
      data = data2,
      aes(x = date, y = denivelation),
      color = "black", size = 0.8
    ) +
    scale_fill_gradientn(colors = my_pal, limits = c(0,1), guide = "none") +
    scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%B") +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = "Dénivelé quotidien", y = "Dénivelé (m)", x = "Date") +
    th_base
  
  # 5) Empilage vertical
  print(p_dist / p_deniv + plot_layout(heights = c(1,1)))
  
  
  
  combined <- p_dist / p_deniv + plot_layout(heights = c(1,1))
  
  # Affichage à l’écran (optionnel)
  print(combined)
  
  # Enregistrement sur disque
  ggsave(
    filename = plot_Dist_Deniv_jpg,
    plot     = combined,
    width    = 12,      # en pouces
    height   = 8,       # en pouces
    dpi      = 300,
    units    = "in"
  )
  
}

#### 9. Chargement paturage par jour ####
#---------------------------------------#
if (FALSE){
  # Library 
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  for(alpage in alpages){
  #ENTREE
  # Un dossier contenant carte de végétation
  carto_file = file.path(raster_dir, "Classifications_fusion_ColorIndexed_sc1_landforms_mnh.tif")
  
  # ENTREE
  #Dossier contenant les sous dossier des chargement
  case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
  #Dossier contenant les fichiers du tot de chargement
  case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
  
  # Un .RDS par alpage contenant les charges journalières
  daily_rds_prefix = file.path(case_flock_alpage_file, paste0("by_day_and_state_",YEAR,"_",alpage,".rds"))
  
  # Un dossier contenant les ratsers des Unités Pastorales (UP)
  case_UP_file = file.path(raster_dir, "UP")
  # Un .SHP avec les Unités pastorales UP
  UP_file = file.path(case_UP_file, "v1_bd_shape_up_inra_2012_2014_2154_all_emprise.shp")
  
  
  # Un dossier contenant les Infos sur les alpages
  raw_data_dir = file.path(data_dir,paste0("Colliers_",YEAR,"_brutes"))
  # Un data.frame contenant les dates de pose et de retrait des colliers
  alpage_info_file <- file.path(raw_data_dir, paste0(YEAR,"_infos_alpages.csv"))
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
  if (!dir.exists(output_Plot_Animation_case)) {
    dir.create(output_Plot_Animation_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_load_veget_case <- file.path(output_Plot_Animation_case, "Chargement&Vegetation")
  if (!dir.exists(output_load_veget_case)) {
    dir.create(output_load_veget_case, recursive = TRUE)
  }
  
  # Un .rds contenant les données de trajectoires catégorisées par comportement et collier
  load_veget_rds = file.path(output_load_veget_case, paste0("charge_by_habitat_day_", YEAR, "_", alpage, ".rds"))
  
  # Un .gif avec le taux de chargement cumulatif par jour et par type d'habitat
  load_veget_day_gif = file.path(output_load_veget_case, paste0("Charge_cumulative_jour_habitat_", YEAR, "_", alpage, ".gif"))
 
  
  #CODE 
  
  if (TRUE){
  load_by_veget(alpage, alpage_info_file, UP_file, daily_rds_prefix, load_veget_rds)
  }
  
  if (TRUE){
    gif_plot_load_by_veget_day(load_veget_rds, load_veget_day_gif, delay_miliseconds = 49 )
  }
  
  
  }
  
  
  library(dplyr)
  library(ggplot2)
  
  # Fonction : median_load_by_habitat_bareh()
  # --------------------------------------------------------------
  # Arguments :
  #   - load_veget_rds : chemin vers le fichier RDS (colonnes : day, vegetation_type, charge_sum)
  #   - output_bar_png  : chemin complet (avec .png) pour sauvegarder le barplot horizontal
  #
  # Cette fonction :
  #   1. Lit le RDS et exclut toute ligne où vegetation_type est NA.
  #   2. Calcule la médiane de charge_sum par habitat.
  #   3. Ordre les habitats par médiane décroissante.
  #   4. Trace un barplot horizontal (geom_col + coord_flip).
  #   5. Applique votre palette de couleurs initiale.
  #   6. Enregistre le résultat dans un PNG.
  # --------------------------------------------------------------
  
  library(dplyr)
  library(ggplot2)
  
  # Fonction : mean_load_by_habitat_bareh()
  # -------------------------------------------------------------
  # Arguments :
  #   - load_veget_rds : chemin vers le RDS contenant (day, vegetation_type, charge_sum)
  #   - output_bar_png  : chemin complet (avec extension .png) pour sauvegarder le barplot
  #
  # Cette fonction :
  #   1. Lit le RDS et exclut les lignes où vegetation_type est NA.
  #   2. Calcule la charge moyenne par habitat.
  #   3. Trie les habitats par charge moyenne décroissante.
  #   4. Trace un barplot horizontal, sans contour, avec une palette soignée.
  #   5. Enregistre le résultat au format PNG.
  # -------------------------------------------------------------
  
  library(dplyr)
  library(ggplot2)
  library(scales)   # pour formatter les axes (comma)
  
  # =============================================================================
  # Fonction : mean_load_by_habitat_bareh()
  # =============================================================================
  # Cette fonction :
  #   • Lit un RDS comportant au moins trois colonnes :
  #       - day (entier, non utilisé ici)
  #       - vegetation_type (type d’habitat, caractère/facteur)
  #       - charge_sum (charge journalière, numérique)
  #   • Exclut toute ligne où vegetation_type est NA
  #   • Calcule la charge moyenne par habitat
  #   • Trie du plus grand au plus petit
  #   • Trace un barplot horizontal épuré sans contours, sans grille superflue
  #   • Enregistre le résultat dans un PNG
  #
  # Arguments :
  #   - load_veget_rds : chemin complet vers le fichier RDS (ex. "…/charge_by_habitat_day_2022_Cayolle.rds")
  #   - output_bar_png  : chemin complet du PNG de sortie (ex. "…/Charge_moyenne_par_habitat_2022_Cayolle.png")
  # =============================================================================
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  # =============================================================================
  # Fonction : mean_load_by_habitat_bareh()
  # =============================================================================
  # - load_veget_rds : chemin vers le RDS contenant (day, vegetation_type, charge_sum)
  # - output_bar_png  : chemin complet (avec .png) pour sauvegarder le barplot
  #
  # Modifications :
  #   1) plus d’étiquettes numériques au bout des barres
  #   2) barres plus épaisses (width = 0.8)
  #   3) tri ASCENDANT, avec la plus grande moyenne en haut
  # =============================================================================
  
  mean_load_by_habitat_bareh <- function(load_veget_rds, output_bar_png) {
    # 1) Lecture des données
    df <- readRDS(load_veget_rds)
    
    # 2) Exclure les NA dans vegetation_type
    df <- df %>% filter(!is.na(vegetation_type))
    
    # 3) Calcul de la charge moyenne par habitat
    df_mean <- df %>%
      group_by(vegetation_type) %>%
      summarize(mean_charge = mean(charge_sum, na.rm = TRUE)) %>%
      ungroup()
    
    # 4) Palette “officielle” (hex codes)
    palette_habitat_init <- c(
      "Formations minérales"             = "#696969",
      "Pelouses productives"             = "#FFD700",
      "Pelouses nivales"                 = "#1E90FF",
      "Pelouses humides"                 = "#20B2AA",
      "Landes"                           = "#800080",
      "Forêts non pastorales"            = "#006400",
      "Nardaies denses du subalpin"      = "#32CD32",
      "Megaphorbiaies et Aulnaies"       = "#7FFF00",
      "P. thermiques écorchées"          = "#FF6347",
      "P. intermédiaires de l’alpin"     = "#FF0000",
      "P. th. méditerranéo-montagnardes" = "#FF4500",
      "P. en bombement de l’alpin"       = "#00CED1",
      "Pelouses nitrophiles"             = "#ADFF2F",
      "Queyrellins"                      = "#FFFF00",
      "Sous-bois pastoraux"              = "#8B4513",
      "Autres"                           = "#FFC0CB"
    )
    
    # 5) Vérifier que tous les habitats figurent dans la palette
    habitats_present <- df_mean$vegetation_type
    missing_pal <- setdiff(habitats_present, names(palette_habitat_init))
    if (length(missing_pal) > 0) {
      stop(
        "Les habitats suivants n’ont pas de couleur définie :\n",
        paste0("  • ", missing_pal, collapse = "\n")
      )
    }
    
    # 6) Trier par mean_charge ASCENDANT, puis transformer en facteur
    df_mean <- df_mean %>%
      arrange(mean_charge) %>%
      mutate(vegetation_type = factor(vegetation_type, levels = vegetation_type))
    
    # 7) Palette restreinte à l’ordre des habitats retenus
    pal_finale <- palette_habitat_init[as.character(df_mean$vegetation_type)]
    
    # 8) Construction du barplot horizontal (sans étiquettes, barres plus épaisses)
    p <- ggplot(df_mean, aes(x = vegetation_type, y = mean_charge, fill = vegetation_type)) +
      geom_col(width = 0.8, color = NA) +      # width = 0.8 pour barres plus épaisses
      scale_fill_manual(values = pal_finale) +
      coord_flip() +
      
      # Titre et axes
      scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0, 0.05))   # un peu d’espace à droite pour ne pas coincer les barres
      ) +
      labs(
        title = paste0("Charge moyenne par habitat – ", basename(load_veget_rds)),
        x     = NULL,
        y     = "Présence du troupeau (brebis·jours) [moyenne]"
      ) +
      
      # Thème ultra-épuré
      theme_minimal(base_size = 14, base_family = "Helvetica") +
      theme(
        plot.background       = element_rect(fill = "white", color = NA),
        panel.background      = element_rect(fill = "white", color = NA),
        plot.title            = element_text(face = "bold", hjust = 0, size = 16),
        axis.text.y           = element_text(size = 12, margin = margin(r = 5)),
        axis.text.x           = element_text(size = 10),
        axis.title.x          = element_text(margin = margin(t = 10)),
        panel.grid.major.x    = element_line(color = "grey90"),
        panel.grid.major.y    = element_blank(),
        panel.grid.minor      = element_blank(),
        axis.line.x           = element_line(color = "black"),
        axis.line.y           = element_blank(),
        axis.ticks.y          = element_blank(),
        legend.position       = "none"
      )
    
    # 9) Sauvegarde au format PNG (10×6 pouces, 300 dpi)
    ggsave(
      filename = output_bar_png,
      plot     = p,
      width    = 10,
      height   = 6,
      dpi      = 300,
      units    = "in"
    )
    
    message("✅ Barplot horizontal (moyenne, tri ascendant) enregistré dans : ", output_bar_png)
  }
  
  
  # ================================================================================
  # Exemple d’appel (à placer dans votre boucle alpage) :
  # ================================================================================
  alpage <- "Cayolle"
  YEAR   <- 2023
  #
  load_veget_rds <- file.path(
     output_load_veget_case,
     paste0("charge_by_habitat_day_", YEAR, "_", alpage, ".rds")
   )
   output_bar_png <- file.path(
     output_load_veget_case,
     paste0("Charge_moyenne_par_habitat_", YEAR, "_", alpage, ".png")
   )

   mean_load_by_habitat_bareh(
     load_veget_rds = load_veget_rds,
     output_bar_png = output_bar_png
   )
  
  
  
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   library(dplyr)
   library(ggplot2)
   library(scales)
   
   # =============================================================================
   # Fonction : delta_load_by_habitat_bareh()
   # =============================================================================
   # Cette fonction :
   #   1. Lit deux fichiers RDS (year1, year2) contenant chacun au moins :
   #        - day (entier, non utilisé ici)
   #        - vegetation_type (habitat)
   #        - charge_sum (charge journalière)
   #   2. Calcule la charge moyenne par habitat pour chaque année.
   #   3. Calcule le delta = mean_charge_year2 - mean_charge_year1.
   #   4. Trie les habitats par delta (du plus négatif au plus positif).
   #   5. Trace un barplot horizontal divergent (droite = +, gauche = −), sans contours,
   #      utilisant votre palette d’habitat.  
   #   6. Enregistre le résultat au format PNG.
   #
   # Arguments :
   #   - load_rds_year1 : chemin complet vers le RDS de l’année 1 (ex. 2022)
   #   - load_rds_year2 : chemin complet vers le RDS de l’année 2 (ex. 2023)
   #   - output_png     : chemin complet (avec .png) pour sauvegarder le barplot
   #   - label_year1    : chaîne de caractères (ex. "2022") pour apparaître dans le titre
   #   - label_year2    : chaîne de caractères (ex. "2023") pour apparaître dans le titre
   # =============================================================================
   
   delta_load_by_habitat_bareh <- function(load_rds_year1,
                                           load_rds_year2,
                                           output_png,
                                           label_year1 = "Year1",
                                           label_year2 = "Year2") {
     # 1) Lecture des deux fichiers RDS
     df1 <- readRDS(load_rds_year1)  # ex. "charge_by_habitat_day_2022.rds"
     df2 <- readRDS(load_rds_year2)  # ex. "charge_by_habitat_day_2023.rds"
     
     # 2) Filtrer les NA sur vegetation_type
     df1 <- df1 %>% filter(!is.na(vegetation_type))
     df2 <- df2 %>% filter(!is.na(vegetation_type))
     
     # 3) Calcul de la charge moyenne par habitat pour chaque année
     mean1 <- df1 %>%
       group_by(vegetation_type) %>%
       summarize(mean_charge1 = mean(charge_sum, na.rm = TRUE)) %>%
       ungroup()
     
     mean2 <- df2 %>%
       group_by(vegetation_type) %>%
       summarize(mean_charge2 = mean(charge_sum, na.rm = TRUE)) %>%
       ungroup()
     
     # 4) Fusionner les deux pour calculer delta ; s’assurer que tous les habitats apparaissent
     df_delta <- full_join(mean1, mean2, by = "vegetation_type") %>%
       # si un habitat n’existait que dans l’une des deux années, mean_charge devient NA
       replace_na(list(mean_charge1 = 0, mean_charge2 = 0)) %>%
       mutate(delta = mean_charge2 - mean_charge1)
     
     # 5) Palette “officielle” (hex codes), identique à celle des graphiques précédents
     palette_habitat_init <- c(
       "Formations minérales"             = "#696969",
       "Pelouses productives"             = "#FFD700",
       "Pelouses nivales"                 = "#1E90FF",
       "Pelouses humides"                 = "#20B2AA",
       "Landes"                           = "#800080",
       "Forêts non pastorales"            = "#006400",
       "Nardaies denses du subalpin"      = "#32CD32",
       "Megaphorbiaies et Aulnaies"       = "#7FFF00",
       "P. thermiques écorchées"          = "#FF6347",
       "P. intermédiaires de l’alpin"     = "#FF0000",
       "P. th. méditerranéo-montagnardes" = "#FF4500",
       "P. en bombement de l’alpin"       = "#00CED1",
       "Pelouses nitrophiles"             = "#ADFF2F",
       "Queyrellins"                      = "#FFFF00",
       "Sous-bois pastoraux"              = "#8B4513",
       "Autres"                           = "#FFC0CB"
     )
     
     # 6) S’assurer que tous les habitats figurent bien dans la palette
     habitats_present <- df_delta$vegetation_type
     missing_pal <- setdiff(habitats_present, names(palette_habitat_init))
     if (length(missing_pal) > 0) {
       stop(
         "Les habitats suivants ne sont pas dans la palette :\n",
         paste0("  • ", missing_pal, collapse = "\n")
       )
     }
     
     # 7) Trier par delta (du plus négatif au plus positif), transformer en facteur
     df_delta <- df_delta %>%
       arrange(delta) %>%
       mutate(vegetation_type = factor(vegetation_type, levels = vegetation_type))
     
     # 8) Palette restreinte à l’ordre des habitats présents
     pal_finale <- palette_habitat_init[as.character(df_delta$vegetation_type)]
     
     # 9) Construction du graphique divergent horizontal
     #    On crée un axe X (delta) qui peut prendre des valeurs négatives (gauche) ou positives (droite).
     p <- ggplot(df_delta, aes(x = vegetation_type, y = delta, fill = vegetation_type)) +
       geom_col(width = 0.8, color = NA) +       # barres épaisses, sans contour
       scale_fill_manual(values = pal_finale) +
       coord_flip() +
       
       # Ligne verticale de référence en zéro
       geom_vline(xintercept = 0, color = "black", size = 0.5) +
       
       # Titre et axes
       scale_y_continuous(
         labels = comma,
         expand = expansion(mult = c(0.0, 0.05))
       ) +
       labs(
         title = paste0(
           "Δ Charge moyenne par habitat : ",
           label_year2, " – ", label_year1
         ),
         x = NULL,
         y = paste0(
           "Δ Présence du troupeau (", label_year2, " – ", label_year1, 
           ") [brebis·jours]"
         )
       ) +
       
       # Thème épuré
       theme_minimal(base_size = 14, base_family = "Helvetica") +
       theme(
         plot.background        = element_rect(fill = "white", color = NA),
         panel.background       = element_rect(fill = "white", color = NA),
         plot.title             = element_text(face = "bold", hjust = 0, size = 16),
         axis.text.y            = element_text(size = 12, margin = margin(r = 5)),
         axis.text.x            = element_text(size = 10),
         axis.title.x           = element_text(margin = margin(t = 10)),
         panel.grid.major.x     = element_line(color = "grey90"),
         panel.grid.major.y     = element_blank(),
         panel.grid.minor       = element_blank(),
         axis.line.x            = element_line(color = "black"),
         axis.line.y            = element_blank(),
         axis.ticks.y           = element_blank(),
         legend.position        = "none"
       )
     
     # 10) Enregistrement du PNG
     ggsave(
       filename = output_png,
       plot     = p,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in"
     )
     
     message("✅ Barplot divergent enregistré dans : ", output_png)
   }
   
   # ================================================================================
   # Exemple d’appel 
   # ================================================================================
   # Supposons que vous ayez déjà produit :
   #   charge_by_habitat_day_2022_Cayolle.rds
   #   charge_by_habitat_day_2023_Cayolle.rds
   #
   # Vous pouvez alors faire :
      alpage <- "Cayolle"
      YEAR1  <- "2022"
      YEAR2  <- "2023"
   #
     rds1 <- file.path(
       output_load_veget_case,
        paste0("charge_by_habitat_day_", YEAR1, "_", alpage, ".rds")
      )
     rds2 <- file.path(
       output_load_veget_case,
       paste0("charge_by_habitat_day_", YEAR2, "_", alpage, ".rds")
     )
     out_png <- file.path(
       output_load_veget_case,
       paste0("Delta_charge_", YEAR2, "_", YEAR1, "_", alpage, ".png")
     )
   #
      delta_load_by_habitat_bareh(
       load_rds_year1 = rds1,
        load_rds_year2 = rds2,
        output_png     = out_png,
        label_year1    = YEAR1,
        label_year2    = YEAR2
      )
   # ================================================================================
   
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 1) Si nécessaire, transformer 'vegetation_type' en facteur et fixer l’ordre
df_delta <- df_delta %>%
  # Pour afficher les habitats du plus petit delta au plus grand en haut :
  arrange(delta) %>%
  mutate(
    vegetation_type = factor(vegetation_type, levels = vegetation_type)
  )

# 2) Palette d’habitat (exemple, la même que plus haut)
palette_habitat_init <- c(
  "Formations minérales"             = "#696969",
  "Pelouses productives"             = "#FFD700",
  "Pelouses nivales"                 = "#1E90FF",
  "Pelouses humides"                 = "#20B2AA",
  "Landes"                           = "#800080",
  "Forêts non pastorales"            = "#006400",
  "Nardaies denses du subalpin"      = "#32CD32",
  "Megaphorbiaies et Aulnaies"       = "#7FFF00",
  "P. thermiques écorchées"          = "#FF6347",
  "P. intermédiaires de l’alpin"     = "#FF0000",
  "P. th. méditerranéo-montagnardes" = "#FF4500",
  "P. en bombement de l’alpin"       = "#00CED1",
  "Pelouses nitrophiles"             = "#ADFF2F",
  "Queyrellins"                      = "#FFFF00",
  "Sous-bois pastoraux"              = "#8B4513",
  "Autres"                           = "#FFC0CB"
)

# S’assurer que la palette couvre exactement les habitats présents
pal_finale <- palette_habitat_init[as.character(df_delta$vegetation_type)]

# 3) Construire le slope‐graph (dumbbell chart)
p_slope <- ggplot(df_delta) +
  # 3.a. Segment qui relie la valeur 2022 à la valeur 2023 pour chaque habitat
  geom_segment(aes(
    y = vegetation_type,
    x = mean_charge1,
    xend = mean_charge2
  ),
  color = "grey70",
  size = 1
  ) +
  # 3.b. Point “2022” à gauche
  geom_point(aes(
    y = vegetation_type,
    x = mean_charge1,
    fill = vegetation_type
  ),
  shape = 21, size = 4, color = "black", stroke = 0.2
  ) +
  # 3.c. Point “2023” à droite
  geom_point(aes(
    y = vegetation_type,
    x = mean_charge2,
    fill = vegetation_type
  ),
  shape = 21, size = 4, color = "black", stroke = 0.2
  ) +
  # 3.d. Palette de remplissage (couleur de l’habitat) pour les deux points
  scale_fill_manual(values = pal_finale) +
  
  # 4) Axes et labels
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Comparaison de l’usage moyen par habitat (2022 vs 2023)",
    x = "Présence du troupeau (brebis·jours) [valeur moyenne]",
    y = NULL,
    subtitle = "À gauche : 2022  –  À droite : 2023"
  ) +
  
  # 5) Thème épuré
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    plot.background       = element_rect(fill = "white", color = NA),
    panel.background      = element_rect(fill = "white", color = NA),
    plot.title            = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle         = element_text(size = 12, hjust = 0),
    axis.text.y           = element_text(size = 12, margin = margin(r = 10)),
    axis.text.x           = element_text(size = 10),
    axis.title.x          = element_text(margin = margin(t = 10)),
    panel.grid.major.y    = element_blank(),
    panel.grid.minor      = element_blank(),
    panel.grid.major.x    = element_line(color = "grey90"),
    axis.line.x           = element_line(color = "black"),
    axis.line.y           = element_blank(),
    legend.position       = "none"
  )

# 6) Sauvegarde en PNG (par exemple 10×7 pouces @ 300 dpi)
ggsave(
  p_slope,
  filename = "Delta_Usage_Habitat_2023-2022_slope.png",
  width    = 10,
  height   = 7,
  dpi      = 300,
  units    = "in"
)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
}

#### 10. Delta de chargement ####
#-------------------------------#
if (FALSE){
  # Library 
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  
  # OBJET
  
  YEAR_1 = 2022
  YEAR_2 = 2023
  
  # ENTREE
  
  #Dossier de sortie des indicateur pour la visualistaion
  file_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  #Sous-dossier Indicateur traitée : Chargement
  file_chargement_case <- file.path(output_visu_case, "Taux_chargement")
  
  #ANNEE 1
  # Sous-sous-dossier alpage et années traitée : Chargement
  file_case_alpage_YEAR_1 <- file.path(output_chargement_case, paste0(YEAR_1,"_",alpage))
  
  # Un .TIF par alpage contenant les charges par comportement
  output_flock_repos_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("repos",YEAR_1,"_",alpage,".tif"))
  output_flock_deplacement_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("deplacement",YEAR_1,"_",alpage,".tif"))
  output_flock_paturage_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("paturage",YEAR_1,"_",alpage,".tif"))
  # Un .TIF par alpage contenant la charge totale sur toute la saison
  output_flock_tot_tif_YEAR_1 = file.path(file_case_alpage_YEAR_1, paste0("total_",YEAR_1,"_",alpage,".tif"))
  
  
  #ANNEE 2
  # Sous-sous-dossier alpage et années traitée : Chargement
  file_case_alpage_YEAR_2 <- file.path(output_chargement_case, paste0(YEAR_2,"_",alpage))
  
  # Un .TIF par alpage contenant les charges par comportement
  output_flock_repos_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("repos",YEAR_2,"_",alpage,".tif"))
  output_flock_deplacement_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("deplacement",YEAR_2,"_",alpage,".tif"))
  output_flock_paturage_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("paturage",YEAR_2,"_",alpage,".tif"))
  # Un .TIF par alpage contenant la charge totale sur toute la saison
  output_flock_tot_tif_YEAR_2 = file.path(file_case_alpage_YEAR_2, paste0("total_",YEAR_2,"_",alpage,".tif"))
  
  
  
  # SORTIE
  
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_Plot_Animation_case <- file.path(output_dir, "7. Plot_et_Animation")
  if (!dir.exists(output_Plot_Animation_case)) {
    dir.create(output_Plot_Animation_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_load_delta_case <- file.path(output_Plot_Animation_case, "DELTA_Chargement")
  if (!dir.exists(output_load_delta_case)) {
    dir.create(output_load_delta_case, recursive = TRUE)
  }
  
  # Un .tif contenant le delta de chargement comportement 
  output_delta_load_paturage = file.path(output_load_delta_case, paste0("delta_chargement_paturage", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  output_delta_load_repos = file.path(output_load_delta_case, paste0("delta_chargement_repos", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  output_delta_load_deplacement = file.path(output_load_delta_case, paste0("delta_chargement_deplacement", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  # Un .tif contenant le delta de chargement total 
  output_delta_load_total = file.path(output_load_delta_case, paste0("delta_chargement_total", YEAR_2, "_" ,YEAR_1, "_", alpage, ".tif"))
  
  #CODE 
  
  
  
  library(raster)
  
  # Lecture des rasters de l'année 1
  repos_YEAR_1 <- raster(output_flock_repos_tif_YEAR_1)
  deplacement_YEAR_1 <- raster(output_flock_deplacement_tif_YEAR_1)
  paturage_YEAR_1 <- raster(output_flock_paturage_tif_YEAR_1)
  total_YEAR_1 <- raster(output_flock_tot_tif_YEAR_1)
  
  # Lecture des rasters de l'année 2
  repos_YEAR_2 <- raster(output_flock_repos_tif_YEAR_2)
  deplacement_YEAR_2 <- raster(output_flock_deplacement_tif_YEAR_2)
  paturage_YEAR_2 <- raster(output_flock_paturage_tif_YEAR_2)
  total_YEAR_2 <- raster(output_flock_tot_tif_YEAR_2)
  
  # Aligner les rasters de l'année 2 sur la grille de l'année 1
  repos_YEAR_2_aligned <- projectRaster(repos_YEAR_2, repos_YEAR_1, method = "bilinear")
  deplacement_YEAR_2_aligned <- projectRaster(deplacement_YEAR_2, deplacement_YEAR_1, method = "bilinear")
  paturage_YEAR_2_aligned <- projectRaster(paturage_YEAR_2, paturage_YEAR_1, method = "bilinear")
  total_YEAR_2_aligned <- projectRaster(total_YEAR_2, total_YEAR_1, method = "bilinear")
  
  # Calcul du delta de chargement (année 2 - année 1)
  delta_repos <- repos_YEAR_2_aligned - repos_YEAR_1
  delta_deplacement <- deplacement_YEAR_2_aligned - deplacement_YEAR_1
  delta_paturage <- paturage_YEAR_2_aligned - paturage_YEAR_1
  delta_total <- total_YEAR_2_aligned - total_YEAR_1
  
  
  ## Sauvegarde des résultats au format TIF
  writeRaster(delta_repos, filename = output_delta_load_repos, format = "GTiff", overwrite = TRUE)
  writeRaster(delta_deplacement, filename = output_delta_load_deplacement, format = "GTiff", overwrite = TRUE)
  writeRaster(delta_paturage, filename = output_delta_load_paturage, format = "GTiff", overwrite = TRUE)
  writeRaster(delta_total, filename = output_delta_load_total, format = "GTiff", overwrite = TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
}

#### 10. Delta de chargement ####
#-------------------------------#
if (FALSE){

  
  
  
  
  
  # Un raster de phénologie
  pheno_raster = file.path(raster_dir,"ndvis_2024_Cayolle_pheno_metrics.tif")
  ndvi_stack <- stack(pheno_raster)
  ndvi   <- raster(pheno_raster)

  
  
  print(paste("Nombre de bandes :", nlayers(ndvi_stack)))
  
  
  
  
  
  
  # Définition du répertoire et des chemins d'entrée / sortie
  raster_dir <- "C:/Users/masso/Documents/STAGE_M2_PERSEE/R_studio/PERSEE_Traitement_Catlog/raster"
  input_file <- file.path(raster_dir, "ndvis_2024_Cayolle_pheno_metrics.tif")
  output_file <- file.path(raster_dir, "ndvis_2024_Cayolle_band_max.tif")
  
  # Charger le raster multi-bandes (13 bandes)
  ndvi_stack <- stack(input_file)
  cat("Nombre de bandes dans le raster :", nlayers(ndvi_stack), "\n")
  
  # Fonction qui retourne, pour un pixel donné, l'indice (la bande) où NDVI est maximum
  get_band_max <- function(pixel_values) {
    # Si toutes les valeurs du pixel sont NA, retourner NA
    if(all(is.na(pixel_values))) return(NA)
    # Calculer l'indice de la bande où la valeur NDVI est maximale
    band_index <- which.max(pixel_values)
    return(band_index)
  }
  
  # Appliquer la fonction sur chaque pixel du stack
  band_max_raster <- calc(ndvi_stack, fun = get_band_max)
  
  # Exporter le résultat dans un fichier GeoTIFF
  writeRaster(band_max_raster, filename = output_file, format = "GTiff", overwrite = TRUE)
  cat("Raster de la bande de NDVI max exporté sous :", output_file, "\n")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  library(raster)
  
  # Définition du répertoire et des chemins d'entrée/sortie
  raster_dir <- "C:/Users/masso/Documents/STAGE_M2_PERSEE/R_studio/PERSEE_Traitement_Catlog/raster"
  input_file <- file.path(raster_dir, "ndvis_2024_Cayolle_pheno_metrics.tif")
  output_file <- file.path(raster_dir, "ndvis_2024_Cayolle_ndvi_max.tif")
  
  # Charger le raster multi-bandes (ici 13 bandes)
  ndvi_stack <- stack(input_file)
  cat("Nombre de bandes dans le raster :", nlayers(ndvi_stack), "\n")
  
  # Calculer, pour chaque pixel, la valeur NDVI maximale en prenant le maximum de la série
  ndvi_max_raster <- calc(ndvi_stack, fun = function(x) { max(x, na.rm = TRUE) })
  
  # Sauvegarder le raster de NDVI maximum
  writeRaster(ndvi_max_raster, filename = output_file, format = "GTiff", overwrite = TRUE)
  cat("Raster de NDVI max exporté sous :", output_file, "\n")
  
  
  
  
  
  
  
  
  
  
  
  
  # Chargement de la bibliothèque nécessaire
  library(raster)
  
  # Définition du répertoire contenant vos rasters
  raster_dir <- "C:/Users/masso/Documents/STAGE_M2_PERSEE/R_studio/PERSEE_Traitement_Catlog/raster"
  
  # Chemin du stack NDVI complet (une bande par date)
  ndvi_stack_file <-  file.path(raster_dir,"ndvis_2024_Cayolle_pheno_metrics.tif")
  
  # Chemin de sortie pour le raster des métriques phénologiques (date du NDVI max)
  pheno_raster <- file.path(raster_dir, "ndvis_2024_Cayolle_pheno_metrics.tif")
  
  # Importation du stack NDVI
  ndvi_stack <- stack(ndvi_stack_file)
  
  # --- Extraction des dates des acquisitions ---
  # On suppose que les noms de couches contiennent la date au format "YYYYMMDD".
  dates <- as.Date(names(ndvi_stack), format = "%Y%m%d")
  if (all(is.na(dates))) {
    # Si l'extraction échoue, on attribue simplement des jours séquentiels
    julian_days <- 1:nlayers(ndvi_stack)
  } else {
    # Conversion des dates en jour julien (valeurs de 1 à 366)
    julian_days <- as.numeric(format(dates, "%j"))
  }
  
  # --- Fonction pour extraire la date du NDVI max pour chaque pixel ---
  get_max_date <- function(x) {
    # x est un vecteur de valeurs NDVI pour un pixel donné sur toute l'année
    if (all(is.na(x))) return(NA)  # Si toutes les valeurs sont manquantes, retourner NA
    
    # Trouver l'indice correspondant à la valeur maximum
    max_index <- which.max(x)
    
    # Retourner le jour julien associé à cet indice
    return(julian_days[max_index])
  }
  
  # Appliquer la fonction sur l'ensemble du stack NDVI (pixel par pixel)
  date_raster <- calc(ndvi_stack, fun = get_max_date)
  
  # Exporter le raster résultant des dates de NDVI max au format GeoTIFF
  writeRaster(date_raster, filename = pheno_raster, format = "GTiff", overwrite = TRUE)
  
  cat("Raster de la date du NDVI max exporté sous :", pheno_raster, "\n")
  
}

#### 11. PLOT DIAPO ####
#----------------------#
if (FALSE){
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(dplyr)
  library(tibble)
  
  library(ggplot2)
  library(dplyr)
  library(tibble)
  
  # 1) Vos données
  df <- tribble(
    ~category,            ~percent, ~ring,
    "Bovins Laitier",     10,       "Zone de plaine",
    "Bovins Viande",      12,       "Zone de plaine",
    "Ovins et Caprins",    6,       "Zone de plaine",
    "Porcins",             6,       "Zone de plaine",
    "Mixte",              17,       "Zone de plaine",
    "Cultures",           49,       "Zone de plaine",
    "Bovins Laitier",     16,       "Zone de montagne",
    "Bovins Viande",      24,       "Zone de montagne",
    "Ovins et Caprins",   14,       "Zone de montagne",
    "Porcins",             4,       "Zone de montagne",
    "Mixte",              18,       "Zone de montagne",
    "Cultures",           24,       "Zone de montagne"
  )
  
  # 2) On définit l’ordre des catégories **inverse** (Cultures en bas)
  original_levels <- c(
    "Bovins Laitier","Bovins Viande","Ovins et Caprins",
    "Porcins","Mixte","Cultures"
  )
  df <- df %>%
    mutate(
      ring     = factor(ring, levels = c("Zone de plaine","Zone de montagne")),
      category = factor(category, levels = rev(original_levels))
    )
  
  # 3) Palette manuelle : Cultures = gris clair, les autres Dark2
  my_cols <- c(
    "Cultures"           = "#D3D3D3",
    "Mixte"              = "#1B9E77",
    "Porcins"            = "#D95F02",
    "Ovins et Caprins"   = "#7570B3",
    "Bovins Viande"      = "#E7298A",
    "Bovins Laitier"     = "#66A61E"
  )
  
  # 4) Le barplot empilé
  ggplot(df, aes(x = ring, y = percent, fill = category)) +
    geom_col(width = 0.6, color = "white", size = 0.3) +
    scale_fill_manual(values = my_cols) +
    # labels % au centre de chaque segment
    geom_text(
      aes(label = paste0(percent, "%")),
      position = position_stack(vjust = 0.5),
      size = 3, color = "grey20"
    ) +
    # thème épuré + transparent
    theme_void() +
    theme(
      legend.position   = "right",
      legend.title      = element_blank(),
      plot.background   = element_rect(fill = NA, color = NA),
      panel.background  = element_rect(fill = NA, color = NA),
      axis.text         = element_blank(),
      axis.ticks        = element_blank()
    )
  
  
  
  
  
  
  
  
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(RColorBrewer)
  
  # 1) Données
  df <- tribble(
    ~category,            ~percent, ~ring,
    "Bovins Laitier",     10,       "Zone de plaine",
    "Bovins Viande",      12,       "Zone de plaine",
    "Ovins et Caprins",    6,       "Zone de plaine",
    "Porcins",             6,       "Zone de plaine",
    "Mixte",              17,       "Zone de plaine",
    "Cultures",           49,       "Zone de plaine",
    "Bovins Laitier",     16,       "Zone de montagne",
    "Bovins Viande",      24,       "Zone de montagne",
    "Ovins et Caprins",   14,       "Zone de montagne",
    "Porcins",             4,       "Zone de montagne",
    "Mixte",              18,       "Zone de montagne",
    "Cultures",           24,       "Zone de montagne"
  )
  
  # 2) Ordre des catégories (Cultures tout en bas)
  df <- df %>%
    mutate(
      ring = factor(ring, levels = c("Zone de plaine","Zone de montagne")),
      category = factor(category,
                        levels = c("Cultures","Mixte","Porcins","Ovins et Caprins","Bovins Viande","Bovins Laitier")
      )
    )
  
  # 3) Palette
  pal <- c(
    "Cultures"           = "#D3D3D3",
    "Mixte"              = brewer.pal(5, "Set2")[1],
    "Porcins"            = brewer.pal(5, "Set2")[2],
    "Ovins et Caprins"   = brewer.pal(5, "Set2")[3],
    "Bovins Viande"      = brewer.pal(5, "Set2")[4],
    "Bovins Laitier"     = brewer.pal(5, "Set2")[5]
  )
  
  # 4) Plot
  ggplot(df, aes(x = ring, y = percent, fill = category)) +
    geom_col(width = 0.6, color = NA) +  
    scale_fill_manual(values = pal) +
    geom_text(
      aes(label = paste0(percent, "%")),
      position = position_stack(vjust = 0.5),
      size = 3.5, color = "grey20", fontface = "bold"
    ) +
    theme_minimal(base_family = "Helvetica") +
    theme(
      # fond gris englobant aussi la légende
      plot.background  = element_rect(fill = "#F5F5F5", color = NA),
      # zone des barres transparente
      panel.background = element_rect(fill = NA, color = NA),
      # légende sur fond transparent
      legend.background = element_rect(fill = NA, color = NA),
      legend.key        = element_rect(fill = NA, color = NA),
      # suppression des lignes / axes inutiles
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks       = element_blank(),
      axis.text.x      = element_text(face = "bold", size = 10),
      # position de la légende
      legend.position  = "right",
      legend.title     = element_blank()
    )
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Packages nécessaires
  library(tidyverse)
  
  # Base de données
  df <- tribble(
    ~category,                      ~percent, ~zone,
    "Élevage laitier et viande",     34,       "Zone de plaine",
    "Polyculture-élevage",           17,       "Zone de plaine",
    "Culture végétale",              49,       "Zone de plaine",
    "Élevage laitier et viande",     58,       "Zone de montagne",
    "Polyculture-élevage",           18,       "Zone de montagne",
    "Culture végétale",              24,       "Zone de montagne"
  )
  
  # Inverser l'ordre pour l'empilement
  df$category <- factor(df$category, levels = c("Culture végétale", "Polyculture-élevage", "Élevage laitier et viande"))
  
  # Palette très douce et naturelle
  palette_couleurs <- c(
    "Culture végétale" = "#7BA05B",       # Vert sauge
    "Polyculture-élevage" = "#C0C0C0",    # Gris clair perle
    "Élevage laitier et viande" = "#A97457" # Brun doux
  )
  
  # Graphique
  ggplot(df, aes(x = zone, y = percent, fill = category)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(percent, "%")), 
              position = position_stack(vjust = 0.5), 
              size = 4, color = "white", fontface = "bold", family = "Helvetica") +
    scale_fill_manual(values = palette_couleurs) +
    theme_minimal(base_size = 14, base_family = "Helvetica") +
    theme(
      panel.background = element_rect(fill = "#fafafa", color = NA), # gris hyper léger
      plot.background = element_rect(fill = "#fafafa", color = NA),
      axis.title = element_blank(),        # Pas de titres axes
      axis.text.y = element_blank(),        # Pas d'axe Y
      axis.ticks = element_blank(),         # Pas de ticks
      axis.line = element_blank(),          # Pas de bordure
      plot.title = element_blank(),         # Pas de titre
      legend.title = element_text(face = "bold", family = "Helvetica"),
      legend.text = element_text(family = "Helvetica"),
      axis.text.x = element_text(face = "bold", family = "Helvetica")
    ) +
    labs(
      fill = "Activité agricole"
    )
  
  
  
  
  
  

#### Table comparaison utilisation parc ####
#------------------------------------------#
if (FALSE){
  
  
  # ENTREE
  
  #Dossier contenant les sous dossier des chargement
  case_flock_file = file.path(output_dir, "4. Chargements_Calcules")
  #Dossier contenant les fichiers du tot de chargement
  case_flock_alpage_file = file.path(case_flock_file,paste0(YEAR,"_",alpage))
  
  # Un .RDS par alpage contenant les charges journalières
  
  input_park_day_state_filtered_rds <- file.path(case_flock_alpage_file, paste0("by_park_day_and_state_transition_filtered_", YEAR,"_",alpage,".rds"))
  
  
  
  # SORTIE 
  # Création du dossier de sortie des indicateur pour la visualistaion
  output_visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  if (!dir.exists(output_visu_case)) {
    dir.create(output_visu_case, recursive = TRUE)
  }
  # Création du sous-dossier Indicateur traitée : Espace paturé
  output_table_parc_case <- file.path(output_visu_case, "Table_comparaison_parc")
  if (!dir.exists( output_table_parc_case)) {
    dir.create( output_table_parc_case, recursive = TRUE)
  }
  
  output_table_png = file.path(output_table_parc_case, paste0("Table_comparaisons_parc_",alpage,".png"))
  
  
  
  
  
  # TABLE DE BASE 
  
  library(data.table)
  
  # Paramètres
  years      <- c(2022, 2023, 2024)
  pixel_size <- 10    # taille du pixel en mètres
  results    <- vector("list", length(years))
  
  for (i in seq_along(years)) {
    yr <- years[i]
    
    # chemins vers les RDS
    base_dir   <- file.path(output_dir, "4. Chargements_Calcules", paste0(yr, "_", alpage))
    rds_daily  <- file.path(
      base_dir,
      paste0("by_park_day_and_state_transition_filtered_", yr, "_", alpage, ".rds")
    )
    rds_total  <- file.path(
      base_dir,
      paste0("by_park_transition_filtered_", yr, "_", alpage, ".rds")
    )
    rds_map    <- file.path(
      base_dir,
      paste0("info_table_use_parc_", yr, "_", alpage, ".rds")
    )
    
    # 1) Lecture des données
    dt_daily <- as.data.table(readRDS(rds_daily))
    dt_total <- as.data.table(readRDS(rds_total))
    map_dt   <- as.data.table(readRDS(rds_map))[, .(parc, rename)]
    
    # 2) Définition de la zone : pixels où Charge > 10
    zone <- dt_total[Charge > 10]
    
    # 3) Surface en hectares par parc (zone)
    pix_cnt <- zone[
      , .(n_pix = uniqueN(paste(x, y))), 
      by = parc
    ]
    pix_cnt[, surface_ha := n_pix * pixel_size^2 / 10000]
    
    # 4) Nombre de jours passés par parc
    days_cnt <- dt_daily[
      , .(n_days = uniqueN(day)), 
      by = parc
    ]
    
    # 5) Médiane de la charge dans la zone, par parc
    med_charge <- zone[
      , .(median_charge = median(Charge, na.rm = TRUE)), 
      by = parc
    ]
    
    # 6) Assemblage des indicateurs
    summary_dt <- merge(pix_cnt[, .(parc, surface_ha)], days_cnt,   by = "parc", all = TRUE)
    summary_dt <- merge(summary_dt,       med_charge, by = "parc", all = TRUE)
    summary_dt <- merge(summary_dt,       map_dt,     by = "parc", all.x = TRUE)
    summary_dt[, `:=`(year = yr, alpage = alpage)]
    
    results[[i]] <- summary_dt
  }
  
  # 7) Construction du tableau final
  final_dt <- rbindlist(results, fill = TRUE)
  setcolorder(final_dt, c("alpage", "rename", "year", "surface_ha", "n_days", "median_charge"))
  
  print(final_dt)
  
  # JOLIE TABLE 
  
  if (alpage == "Sanguiniere"){
  
   # À partir de `final_dt` issu de votre boucle (inchangé) :
  # final_dt doit contenir au minimum :
  #   parc, year, surface_ha, n_days, median_charge
  
  library(data.table)
  library(tidyr)
  library(dplyr)
  library(knitr)
  library(kableExtra)
    
  library(webshot2)
  
  # 1) Préparer le libellé final
  dt <- copy(final_dt)
  # si `rename` est NA, on prend `parc`
  dt[, rename := ifelse(is.na(rename), parc, rename)]
  # fusionner les deux premières périodes sous un même label
  to_fuse <- c("parc_début-juillet_début-août",
               " parc_mi-juin_début-août")
  dt[rename %in% to_fuse, rename := "parc_mi-juin_début-août"]
  
  # 2) Agréger pour n’avoir qu’une ligne par (rename, year)
  agg <- dt[, .(
    surface_ha    = mean(surface_ha,    na.rm=TRUE),
    n_days        = mean(n_days,        na.rm=TRUE),
    median_charge = mean(median_charge, na.rm=TRUE)
  ), by=.(rename, year)]
  
  # 3) Pivot en wide avec data.table::dcast (une colonne métrique×année)
  wide <- dcast(
    agg,
    rename ~ year,
    value.var = c("surface_ha","n_days","median_charge")
  )
  
  # 4) Arrondir à 1 décimale
  num_cols <- setdiff(names(wide),"rename")
  wide[ , (num_cols) := lapply(.SD, round, 1), .SDcols = num_cols ]
  
  # 1) Ordre chronologique des parcs
  ordre_parcs <- c(
    "parc_mi-juin_début-août",
    "parc_début-août_début-septembre",
    "parc_début-septembre_mi-septembre"
  )
  wide[, rename := factor(rename, levels = ordre_parcs)]
  setorder(wide, rename)
  
  # 2) Sous-colonnes = années par métrique (2022, 2023, 2024)
  years       <- sort(unique(agg$year))
  sub_headers <- rep(as.character(years), times = 3)
  
  # 3) Affichage final
  library(kableExtra)
  
  years <- sort(unique(agg$year))
  
  # 1) Sous-colonnes : 2022, 2023, 2024 pour CHAQUE métrique
  sub_headers <- rep(as.character(years), times = 3)
  
  wide %>%
    rename(`Parc (période)` = rename) %>%
    kable(
      format    = "html",
      col.names = c("Parc (période)", sub_headers),
      align     = c("l", rep("c", length(sub_headers))),
      caption   = paste0("Indicateurs par parc pour l’alpage ", alpage)
    ) %>%
    add_header_above(
      c(
        " " = 1,
        "Surface utilisée (ha)" = length(years),
        "Jours de présence"      = length(years),
        "Chargement médian"      = length(years)
      )
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width        = FALSE,
      position          = "center",
      font_size         = 14
    ) %>%
    row_spec(
      0,
      bold      = TRUE,
      extra_css = "background-color:#e8e8e8; line-height:1.4;"
    ) %>%
    # zébrage personnalisé
    row_spec(seq(1, nrow(wide), 2), extra_css = "background-color:#fafafa;") %>%
    row_spec(seq(2, nrow(wide), 2), extra_css = "background-color:#ffffff;") %>%
    # augmenter les paddings colonnes
    column_spec(
      1,
      bold      = TRUE,
      width     = "5cm",
      extra_css = "padding: 12px 8px;"
    ) %>%
    column_spec(
      2:(ncol(wide)),
      width     = "2cm",
      extra_css = "padding: 12px 6px;"
    )
  
  
  # 0) installer si besoin
  
  
  library(kableExtra)
  library(webshot2)
  library(pagedown)
  library(dplyr)
  
  # --- 1) Construire votre tableau kable avec kableExtra
  tbl <- wide %>%
    rename(`Parc (période)` = rename) %>%
    kable(
      format    = "html",
      col.names = c("Parc (période)", rep(as.character(years), each = 3)),
      align     = c("l", rep("c", length(years) * 3)),
      caption   = paste0("Indicateurs par parc pour l’alpage ", alpage)
    ) %>%
    add_header_above(c(
      " " = 1,
      "Surface utilisée (ha)" = length(years),
      "Jours de présence"      = length(years),
      "Chargement médian"      = length(years)
    )) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width       = FALSE,
      position         = "center",
      html_font        = "Arial",
      font_size        = 14
    )
  
  # --- 2) Sauvegarder un HTML auto-contenant
  tmp_html <- tempfile(fileext = ".html")
  save_kable(
    tbl,
    file           = tmp_html,
    self_contained = TRUE
  )
  
  # --- 3) Imprimer en PNG via Chrome headless
  
  webshot2::webshot(
    url      = tmp_html,
    file     = output_table_png,
    selector = "body",        # capture tout le corps pour éviter le découpage
    delay    = 0.5,           # temps pour que le CSS inline s’applique
    zoom     = 2,             # double résolution
    expand   = c(20,20,20,20),# marges supplémentaires
    vwidth   = 2000,          # largeur de la fenêtre Chrome
    vheight  = 600            # hauteur de la fenêtre Chrome
  )
  message("→ PNG généré ici : ", output_table_png)
  message("→ Votre tableau a été exporté au format PNG : ", output_png)
  
  
  
  }
  
  if (alpage == "Cayolle") {
    
    # 1) Préparation et filtrage sur 2023/2024
    dt <- copy(final_dt)
    dt[, rename := ifelse(is.na(rename), parc, rename)]
    dt <- dt[year %in% c(2023, 2024)]
    
    # 2) Mapping vers 5 périodes canoniques (inchangé)
    period_map <- list(
      "parc_mi-juin_début-juillet"        = "parc_mi-juin_début-juillet",
      "parc_début-juillet_mi-juillet"     = c(
        "parc_05-juil._07-juil.",
        "parc_08-juil._13-juil.",
        "parc_10-juil._15-juil.",   # 2023
        "parc_10-juil._16-juil."    # 2024
      ),
      "parc_mi-juillet_début-août"        = c(
        "parc_mi-juillet_début-août",
        "parc_mi-juillet_fin-juillet" # 2024
      ),
      "parc_début-août_début-septembre"   = "parc_début-août_début-septembre",
      "parc_début-septembre_fin-septembre"= c(
        "parc_début-septembre_mi-septembre",
        "parc_début-septembre_fin-septembre"
      )
    )
    for (canon in names(period_map)) {
      dt[rename %in% period_map[[canon]], rename := canon]
    }
    dt <- dt[rename %in% names(period_map)]
    
    # 3) Agrégation
    agg <- dt[, .(
      surface_ha    = sum(surface_ha,    na.rm = TRUE),
      n_days        = sum(n_days,        na.rm = TRUE),
      median_charge = median(median_charge, na.rm = TRUE)
    ), by = .(rename, year)]
    
    # 4) Pivot en wide
    wide <- dcast(
      agg,
      rename ~ year,
      value.var = c("surface_ha","n_days","median_charge")
    )
    # Arrondi
    num_cols <- setdiff(names(wide), "rename")
    wide[, (num_cols) := lapply(.SD, round, 1), .SDcols = num_cols]
    
    # 5) Tri chronologique des périodes
    chrono <- names(period_map)
    wide[, rename := factor(rename, levels = chrono)]
    setorder(wide, rename)
    
    # 6) Construire sub_headers CORRECTEMENT
    years       <- c(2023, 2024)
    sub_headers <- rep(as.character(years), times = 3)
    # => "2023","2024","2023","2024","2023","2024"
    
    # 7) Générer le HTML kable
    tbl_html <- wide %>%
      dplyr::rename(`Parc (période)` = rename) %>%
      kable(
        format     = "html",
        caption    = paste0("Indicateurs par parc pour l’alpage ", alpage),
        col.names  = c("Parc (période)", sub_headers),
        align      = c("l", rep("c", length(sub_headers))),
        table.attr = 'style="width:100%;max-width:1400px;table-layout:auto;"'
      ) %>%
      add_header_above(c(
        " " = 1,
        "Surface utilisée (ha)" = length(years),
        "Jours de présence"     = length(years),
        "Chargement médian"     = length(years)
      )) %>%
      kable_styling(
        bootstrap_options = c("striped","hover","condensed"),
        full_width        = FALSE,
        position          = "center",
        html_font         = "Arial",
        font_size         = 14
      ) %>%
      row_spec(0, bold = TRUE, extra_css = "background-color:#e8e8e8;") %>%
      row_spec(seq(1, nrow(wide), 2), extra_css = "background-color:#fafafa;") %>%
      row_spec(seq(2, nrow(wide), 2), extra_css = "background-color:#ffffff;") %>%
      column_spec(1, bold=TRUE, width="5cm",   extra_css="padding:12px 8px;") %>%
      column_spec(2:(ncol(wide)), width="2cm", extra_css="padding:12px 6px;")
    
    # 8) Export PNG via webshot2
    tmp_html <- tempfile(fileext = ".html")
    save_kable(tbl_html, tmp_html, self_contained = TRUE)
    webshot2::webshot(
      url      = tmp_html,
      file     = output_table_png,
      selector = "table",
      delay    = 0.5,
      zoom     = 2,
      vwidth   = 1400,
      vheight  = 600,
      expand   = c(10,10,10,10)
    )
    
    message("→ PNG créé : ", output_table_png)
  }
  
  if (alpage == "Viso") {
    
    # 1) Préparation & filtrage
    dt <- copy(final_dt)
    dt[, rename := ifelse(is.na(rename), parc, rename)]
    dt <- dt[year %in% c(2022, 2023, 2024)]
    
    # 2) Mapping vers 4 périodes canoniques
    period_map <- list(
      # mi-juin ↝ mi-juillet
      "parc_mi-juin_mi-juillet" = c(
        "parc_mi-juin_mi-juillet",    # 2022
        "parc_début-juin_début-juillet", # 2023
        "parc_mi-juin_mi-juillet"     # 2024
      ),
      # mi-juillet ↝ mi-août
      "parc_mi-juillet_mi-août" = c(
        "parc_mi-juillet_mi-août",      # 2022
        "parc_début-juillet_début-août",# 2023
        "parc_mi-juillet_mi-août"       # 2024
      ),
      # mi-août ↝ début-septembre
      "parc_mi-août_début-septembre" = c(
        "parc_début-août_début-septembre", # 2022
        "parc_début-août_mi-août",         # 2023
        "parc_début-août_début-septembre"  # 2024
      ),
      # début-septembre ↝ début-octobre
      "parc_début-septembre_début-octobre" = c(
        "parc_début-septembre_début-octobre", # 2022
        "parc_début-septembre_début-octobre", # 2023
        "parc_20-sept._20-sept.",             # 2023
        "parc_début-septembre_début-octobre", # 2024
        "parc_mi-septembre_début-octobre"     # 2024
      )
    )
    
    for (canon in names(period_map)) {
      dt[rename %in% period_map[[canon]], rename := canon]
    }
    dt <- dt[rename %in% names(period_map)]
    
    # 3) Agrégation
    agg <- dt[, .(
      surface_ha    = sum(surface_ha,    na.rm = TRUE),
      n_days        = sum(n_days,        na.rm = TRUE),
      median_charge = median(median_charge, na.rm = TRUE)
    ), by = .(rename, year)]
    
    # 4) Pivot “wide”
    wide <- dcast(
      agg,
      rename ~ year,
      value.var = c("surface_ha","n_days","median_charge")
    )
    # Arrondi à 1 décimale
    num_cols <- setdiff(names(wide), "rename")
    wide[, (num_cols) := lapply(.SD, round, 1), .SDcols = num_cols]
    
    # 5) Tri chronologique des périodes
    chrono <- names(period_map)
    wide[, rename := factor(rename, levels = chrono)]
    setorder(wide, rename)
    
    # 6) Préparation des sous-en-têtes (3 années × 3 indicateurs)
    years       <- c(2022, 2023, 2024)
    sub_headers <- rep(as.character(years), times = 3)
    # => "2022","2023","2024","2022","2023","2024","2022","2023","2024"
    
    # 7) Génération du tableau HTML
    tbl_html <- wide %>%
      dplyr::rename(`Parc (période)` = rename) %>%
      kable(
        format     = "html",
        caption    = paste0("Indicateurs par parc pour l’alpage ", alpage),
        col.names  = c("Parc (période)", sub_headers),
        align      = c("l", rep("c", length(sub_headers))),
        table.attr = 'style="width:100%;max-width:1400px;table-layout:auto;"'
      ) %>%
      add_header_above(c(
        " " = 1,
        "Surface utilisée (ha)" = length(years),
        "Jours de présence"      = length(years),
        "Chargement médian"      = length(years)
      )) %>%
      kable_styling(
        bootstrap_options = c("striped","hover","condensed"),
        full_width        = FALSE,
        position          = "center",
        html_font         = "Arial",
        font_size         = 14
      ) %>%
      row_spec(0, bold = TRUE, extra_css = "background-color:#e8e8e8;") %>%
      row_spec(seq(1, nrow(wide), 2), extra_css = "background-color:#fafafa;") %>%
      row_spec(seq(2, nrow(wide), 2), extra_css = "background-color:#ffffff;") %>%
      column_spec(1, bold=TRUE, width="5cm",   extra_css="padding:12px 8px;") %>%
      column_spec(2:(ncol(wide)), width="2cm", extra_css="padding:12px 6px;")
    
    # 8) Export en PNG via webshot2
    tmp_html <- tempfile(fileext = ".html")
    save_kable(tbl_html, tmp_html, self_contained = TRUE)
    webshot2::webshot(
      url      = tmp_html,
      file     = output_table_png,
      selector = "table",
      delay    = 0.5,
      zoom     = 2,
      vwidth   = 1400,
      vheight  = 600,
      expand   = c(10,10,10,10)
    )
    
    message("→ PNG créé : ", output_table_png)
  }
  
  