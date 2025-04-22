#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#

gc()

# Chargement de la configuration
source("config.R")
source(file.path(functions_dir, "Functions_filtering.R")) 


# Définition de l'année d'analyse
YEAR <- 2022
TYPE <- "catlog" #Type de données d'entrée (CATLOG, OFB )
alpage <- "Cayolle"
alpages <- c("Sanguiniere","Cayolle")
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
  if (TRUE){
    park_total_flock_load_tif(
      park_rds    = park_tot_rds,
      output_dir  = output_case_alpage,
      YEAR        = YEAR,
      alpage      = alpage,
      res_raster  = 10              # ou la résolution souhaitée
    )
  }
  
  if (TRUE){
  
  #Indicateur : Charge par parc et état
  park_state_flock_load_tif(park_state_rds,
                            output_case_alpage,
                            UP_file, alpage, alpage_info_file)
  }
  
 
  
  
  #Indicateur : Charge par parc filtered
  if (TRUE){
    park_total_flock_load_tif_filtered(
      park_rds    = park_tot_filterd_rds,
      output_dir  = output_case_alpage,
      YEAR        = YEAR,
      alpage      = alpage,
      res_raster  = 10              
    )
  }
  
  if (TRUE){
    
    #Indicateur : Charge par parc et état filtered
    park_state_flock_load_tif_filtered(park_state_filtered_rds ,
                              output_case_alpage,
                              UP_file, alpage, alpage_info_file)
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
  
#### 5. Polygon d'utilisation tout les 15 jours ####
#--------------------------------------------------#  
if (FALSE){
  
  library(sf)
  library(dplyr)
  library(viridis)
  library(ggplot2)
  source(file.path(functions_dir, "Functions_Indicateurs.R"))
  
  for (alpage in alpages){
  # ENTREES
  # Dossier contenant les fichiers du comportement
  case_state_file = file.path(output_dir, "3. HMM_comportement")
  # Un .RDS contenant les trajectoires (filtrées, éventuellement sous-échantillonnées)
  state_rds_file = file.path(case_state_file, paste0("Catlog_",YEAR,"_",alpage, "_viterbi.rds"))
  
  
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
  
  
  # CODE 
  # Ancienne fonction
  if (FALSE){
  generate_presence_polygons(state_rds_file,output_polygon_use_shp,YEAR,alpage,density_threshold = 1e-06,n_grid = 200,small_poly_threshold_percent = 0.05)
    # Seuil pour filtrer les petits polygones
    # Ajustez pour être plus ou moins restrictif
  }
  
  if(TRUE){
    generate_presence_polygons_by_percentage(state_rds_file, output_polygon_use_shp, YEAR, alpage, percentage = 0.85 ,n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )
    
    
  }
    
    
  if (FALSE){
    generate_presence_polygons_by_percentage_per_month(state_rds_file, output_polygon_use_shp, YEAR, alpage, percentage = 0.85 ,n_grid = 200,small_poly_threshold_percent = 0.05 ,crs = 2154 )  
  } 
    
    
  
  
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
  case_GIF_file = file.path(raster_dir, "Image_GIF")
  
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
  output_gif = file.path(output_GIF_case, "Gif_Cayolle_quinz_2022.gif")
  
  # CODE
  
  if(TRUE){
  create_gif_from_images(case_GIF_file,output_gif,file_pattern = "Carte_cayolle.*\\.(png|jpg)$",delay_seconds = 1.25)
  }
  
  if(TRUE){
  create_gif_from_images_day_bis (case_GIF_file,output_gif,file_pattern = "Carte_cayolle.*\\.(png|jpg)$",delay_seconds = 0.5)
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
  
  
  
  data = read.csv(file = distance_csv_file, sep = ",")
  
  
  library(ggplot2)
  library(scales)
  
  library(ggplot2)
  
  # Supposons que vos données s'appellent 'data' et contiennent :
  # data$date, data$distance, data$denivelation
  # Assurez-vous que data$date est bien de type Date
  # data$date <- as.Date(data$date, format = "%Y-%m-%d")
  
  library(ggplot2)
  library(dplyr)
  
  # Assurez-vous que data$date est bien au format Date et qu'il n'y a pas de NA
  # data$date <- as.Date(data$date, format = "%Y-%m-%d")
  
  # Filtrer les dates sans valeur (distance ou dénivelé manquants)
  data_filtered <- data %>%
    filter(!is.na(distance), !is.na(denivelation))
  
  # Déterminer la plage de dates valides
  x_min <- min(data_filtered$date)
  x_max <- max(data_filtered$date)
  
  # Calcul d'un ratio pour superposer le dénivelé (axe de droite) sur l'échelle de la distance (axe de gauche)
  ratio <- max(data_filtered$distance, na.rm = TRUE) / max(data_filtered$denivelation, na.rm = TRUE)
  
  ggplot(data_filtered, aes(x = date)) +
    # Barres pour la distance (axe de gauche)
    geom_col(aes(y = distance), fill = "steelblue", alpha = 0.7) +
    
    # Courbe lissée (smooth) pour le dénivelé, sans la courbe brute
    geom_smooth(
      aes(y = denivelation * ratio),
      method = "loess",
      se = FALSE,        # pas de bande d'incertitude
      color = "red",
      size = 1,
      span = 0.2         # ajustez ce paramètre pour un lissage plus ou moins prononcé
    ) +
    
    # Axe Y principal (distance) + axe Y secondaire (dénivelé)
    scale_y_continuous(
      name = "Distance (m)",
      # Ajout de lignes horizontales pour les graduations
      # (on s'en occupera via le thème ci-dessous)
      sec.axis = sec_axis(
        ~ . / ratio,
        name = "Dénivelé (m)"
      )
    ) +
    
    # Axe X : on limite la plage aux dates filtrées
    scale_x_date(
      limits = c(x_min, x_max),
      date_breaks = "1 day",
      date_labels = "%d/%m"
    ) +
    
    # Titres et légendes
    labs(
      x = "Date",
      title = "Distance et Dénivelé quotidiens"
    ) +
    
    # Thème plus épuré (classique) et personnalisation
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      
      # Ajouter des lignes horizontales pour les graduations Y
      panel.grid.major.y = element_line(color = "gray80"),
      panel.grid.minor.y = element_line(color = "gray90"),
      
      # Supprimer (ou laisser) les lignes verticales
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  
  
  library(ggplot2)
  library(dplyr)
  
  # 1) Filtrer les données pour retirer les NA
  data_filtered <- data %>%
    filter(!is.na(distance), !is.na(denivelation))
  
  # Vérifier la plage réelle de dates
  print(range(data_filtered$date))
  
  # 2) Calcul du ratio pour superposer le dénivelé (axe de droite)
  ratio <- max(data_filtered$distance, na.rm = TRUE) / max(data_filtered$denivelation, na.rm = TRUE)
  
  # 3) Graphique
  ggplot(data_filtered, aes(x = date)) +
    
    geom_col(
      aes(y = distance), 
      fill = "lightsteelblue2",   # Couleur de remplissage
      alpha = 0.7,         # Transparence (1 = opaque)
      color = "black",   # Couleur de la bordure
      size = 0.5         # Épaisseur de la bordure
    ) +
    
    # Courbe brute pour le dénivelé
    geom_line(aes(y = denivelation * ratio), color = "grey18", size = 1) +
    
    # Définition des axes Y
    scale_y_continuous(
      name = "Distance (m)",
      sec.axis = sec_axis(
        ~ . / ratio,
        name = "Dénivelé (m)"
      )
    ) +
    
    # Définition de l'axe X (dates)
    # - expand = c(0,0) pour ne pas ajouter de marge avant/après
    # - date_breaks = "1 day" si vous tenez vraiment à voir chaque jour
    scale_x_date(
      date_labels = "%d/%m",
      date_breaks = "1 day",
      expand = c(0, 0)
    ) +
    
    # Titres
    labs(
      x = "Date",
      title = "Distance et dénivelé quotidiens"
    ) +
    
    # Thème épuré + lignes horizontales
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      # Lignes horizontales pour les graduations Y
      panel.grid.major.y = element_line(color = "gray80"),
      panel.grid.minor.y = element_line(color = "gray90"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
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
  
  if (FALSE){
  load_by_veget(alpage, alpage_info_file, UP_file, daily_rds_prefix, load_veget_rds)
  }
  
  if (FALSE){
    gif_plot_load_by_veget_day(load_veget_rds, load_veget_day_gif, delay_miliseconds = 49 )
  }
  
  
  }
  
  
  
  
  
  
  
  
  

  
  
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
  
