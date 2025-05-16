####------------- SCRIPT : Variations climatiques inter-annuel -------------####
## Script rédiger dans le cadre du rapport de stage de M2 visant a annalyser 
## l'influence des variations climatiques inter-annuel sur le schéma de paturage
## de l'alapge. 

## Observer d'eventuel report de charge entre des années contrastées (printemps
## précoce ou tardif)


#### 0. LIBRARIES AND CONSTANTS ####
#----------------------------------#

gc()

# Chargement de la configuration
source("config.R")
source(file.path(functions_dir, "Functions_filtering.R")) 


# Définition de l'année d'analyse
YEAR <- 2023
TYPE <- "catlog" #Type de données d'entrée (CATLOG, OFB )
alpage <- "Sanguiniere"
alpages <- "Viso"
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



#### 1. Préparation des données de SMOD ####
#------------------------------------------#
if (FALSE) {
  # Crop du smod par alpage 
  # Calcul du SMOD moyen sur 10 ans (2023 - 2013)
  # Calcul du fsca (fraction of snow cover)
  
  year_1 = "1999-09-01"
  year_2 = "2023-08-31"
  
  # LIBRARY & FUNCTION
  library(raster)
  library(dplyr)  
  library(stringr) 
  library(sf)
  library(tools)
  library(ggplot2)
  source(file.path(functions_dir, "Functions_traitement_smod.R"))

  
  # ENTREE
  
  # Un dossier contenant les SMOD
  case_SMOD_file = file.path(raster_dir, "Snow")
  
  # Un .CSV : correspondance entre les dalles du SMOD et les alpages
  table_corresp_file = file.path(case_SMOD_file, "SMOD_ID_by_alpage.csv")
  
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
  output_clim_case <- file.path(output_dir, "8. Analysis_Climate")
  if (!dir.exists(output_clim_case)) {
    dir.create(output_clim_case, recursive = TRUE)
  }
  #Création du sous-dossier Indicateur traitée : Chargement
  output_SMOD_case <- file.path(output_clim_case, "SMOD")
  if (!dir.exists(output_SMOD_case)) {
    dir.create(output_SMOD_case, recursive = TRUE)
  }
  
  
  
  
  # CODE
  
  # Préparation des SMOD
  process_smod_for_alpage(alpage, year_1, year_2,case_SMOD_file,table_corresp_file,alpage_info_file,UP_file, output_SMOD_case)
  
  # Calcul du SMOD médian
  median_file <- median_smod_for_alpage(alpage, output_SMOD_case)
  
  # Calcul du FSCA
  fsca_file <- compute_fsca_for_alpage(alpage, output_SMOD_case)
  
}
  





## EXTRACTION DU JEU DE DONNee de la date de d'enneigement médian par année et par alpage : 

library(terra)    # pour lire les rasters SMOD
library(dplyr)
library(lubridate)

# ──────────────────────────────
# 1) Paramètre : dossier source
# ──────────────────────────────
output_SMOD_case <- file.path(output_clim_case, "SMOD")
# ────────────────────────────────────────────────────
# 2) Lister tous les fichiers SMOD (exclure FSCA/median)
# ────────────────────────────────────────────────────
smod_files <- list.files(output_SMOD_case,
                         full.names = TRUE,
                         recursive = FALSE) %>%
  # ne garder que ceux dont le nom commence par "SMOD_"
  .[grepl("^SMOD_", basename(.))] %>%
  # exclure les fichiers median et les FSCA
  .[ !grepl("SMOD_median|Fsca", basename(.), ignore.case = TRUE) ] %>%
  # ne prendre que les .tif (ou .img si besoin)
  .[ grepl("\\.tif$", basename(.), ignore.case = TRUE) ]


# ─────────────────────────────────────────────────────────────
# 3) Pour chaque fichier :
#    - extraire start/end date et alpage depuis le nom
#    - lire le raster
#    - calculer la médiane des pixels SMOD (jours depuis 1 Sep)
#    - convertir ce jour-index en date calend. puis en jour julien
# ─────────────────────────────────────────────────────────────
med_list <- lapply(smod_files, function(f){
  nm     <- basename(f)
  parts  <- strsplit(nm, "_")[[1]]
  # parts = c("SMOD", "31TGK", "YYYYMMDD", "YYYYMMDD", "Alpage.tif")
  start  <- as.Date(parts[3], "%Y%m%d")
  end    <- as.Date(parts[4], "%Y%m%d")
  year   <- year(end)                  # année hydrique = année de fin
  alpage <- sub("\\.tif$", "", parts[5])
  
  # lire le raster et extraire toutes les valeurs
  r      <- rast(f)
  vals   <- values(r)
  
  # médiane de l’index (en jours depuis le 1er sept)
  med_idx <- round(mean(vals, na.rm = TRUE))
  
  # 1) date calendrier correspondante
  med_date <- start + (med_idx - 1)
  # 2) jour julien (1 = 1er jan., … 365/366)
  med_jul  <- yday(med_date)
  
  data.frame(
    year           = year,
    alpage         = alpage,
    median_index   = med_idx,
    date_median    = med_date,
    julian_median  = med_jul
  )
})

# concaténer en un seul data.frame
df_smod_med <- bind_rows(med_list)

# ───────────────────────────
# 4) Résultat à l’écran / CSV
# ───────────────────────────
print(df_smod_med)

write.csv(df_smod_med,
          file = file.path(output_SMOD_case, "SMOD_median_summary.csv"),
          row.names = FALSE)
message("▶ SMOD median summary written to SMOD_median_summary.csv")


## FIN






















#### 2. Création du jeu de données : Alti, FSCA, Présence ####
#------------------------------------------------------------#
if (TRUE) {
  # Pourchaque pixel a 20 mètres attribution  : 
  # - FSCA
  # - Altitude
  # - Chargement / Densité de kernel
  # Donc soit l'utilisation par le SHP des polygones de kernel
  # Soit le taux de chargement par quizaine (méthode préféré pour les plots)
  
 
  # LIBRARY & FUNCTION
  library(raster)
  library(dplyr)  
  library(stringr) 
  library(sf)
  library(tools)
  library(ggplot2)
  source(file.path(functions_dir, "Functions_traitement_smod.R"))
  
  # ENTREE
  # Un .TIF du SMOD Médian sur 10 ans 
  clim_case <- file.path(output_dir, "8. Analysis_Climate")
  SMOD_case <- file.path(clim_case, "SMOD")
  SMOD_tif_file <- file.path(SMOD_case, paste0("Fsca_",alpage,".tif"))
  
  
  # Un .TIF du DEM de l'alpage
  Alti_case <- file.path(raster_dir, "Alti")
  MNT_tif_file <- file.path(Alti_case, paste0(alpage,"_MNT.tif"))
  
  
  # Un .SHP de l'utilisation par quinzaine
  visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  polygon_case <- file.path(visu_case, "Utilisation_par_quinzaine")
  polygon_use_shp <- file.path(polygon_case, paste0("Use_polygon_", YEAR, "_", alpage, ".shp"))
  
  
  
  
  
  # Des .TIF du chargement par quizaine
  #Création du dossier de sortie des indicateur pour la visualistaion
  visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  chargement_case <- file.path(visu_case, "Taux_chargement")
  
  
  case_alpage <- file.path(chargement_case, paste0(YEAR,"_",alpage))
  load_16_30_jun_tif_file <- file.path(case_alpage, paste0("by_quinzaine_",YEAR,"_",alpage,"_16_30_jun.tif"))
  load_1_15_jul_tif_file <- file.path(case_alpage, paste0("by_quinzaine_",YEAR,"_",alpage,"_1_15_jul.tif"))
  load_16_30_jul_tif_file <- file.path(case_alpage, paste0("by_quinzaine_",YEAR,"_",alpage,"_16_30_jul.tif"))
  load_1_15_aou_tif_file <- file.path(case_alpage, paste0("by_quinzaine_",YEAR,"_",alpage,"_1_15_aou.tif"))
  load_16_30_aou_tif_file <- file.path(case_alpage, paste0("by_quinzaine_",YEAR,"_",alpage,"_16_30_aou.tif"))
  load_1_15_sep_tif_file <- file.path(case_alpage, paste0("by_quinzaine_",YEAR,"_",alpage,"_1_15_sep.tif"))
  load_apres16_sep_tif_file <- file.path(case_alpage, paste0("by_quinzaine_",YEAR,"_",alpage,"_apres16_sep.tif"))
  
  
  # SORTIE
  #Création du dossier de sortie des indicateur pour la visualistaion
  output_clim_case <- file.path(output_dir, "8. Analysis_Climate")
  if (!dir.exists(output_clim_case)) {
    dir.create(output_clim_case, recursive = TRUE)
  }
  #Création du sous-dossier Indicateur traitée : Chargement
  output_data_case <- file.path(output_clim_case, "Data_Use_Fsca_Alti")
  if (!dir.exists(output_data_case)) {
    dir.create(output_data_case, recursive = TRUE)
  }
  
  # Un .RDS des données final
  output_clim_data_rds_file_shp <- file.path(output_data_case, paste0("Use_Fsca_Alti_",alpage,"_by_shp.rds"))
  # Un .RDS des données final
  output_clim_data_rds_file_raster <- file.path(output_data_case, paste0("Use_Fsca_Alti_",alpage,"_by_raster.rds"))
  # Un .RDS des données final
  output_clim_data_rds_file_raster_parc <- file.path(output_data_case, paste0("Use_Fsca_Alti_",alpage,"_by_raster_and_parc.rds"))
  output_rds_file_parc <- file.path(output_data_case, paste0("Use_Fsca_Alti_", alpage, "_by_raster_and_parc.rds"))
  
  # CODE 
  
  # Création du data.frame (sortie .RDS), basés sur les polygone de densité
  df_final <- generate_presence_data_by_quinzaine(SMOD_tif_file, MNT_tif_file, polygon_case,
                                                  years = 2022:2024, alpage , 
                                                  output_clim_data_rds_file = output_clim_data_rds_file_shp)
  
  
  # Création du data.frame (sortie .RDS), basés sur le chargement (raster)
  df_final <- generate_loading_data_by_quinzaine(SMOD_tif_file, MNT_tif_file, chargement_case, 
                                                 years = 2022:2024, alpage, 
                                                 output_rds_file = output_clim_data_rds_file_raster,
                                                 threshold = 10,res_raster = 10)
  
  
  
  df_final <- generate_loading_data_by_parc(SMOD_tif_file, MNT_tif_file, chargement_case,
                                            years= c(2022,2023,2024),alpage ,
                                            output_rds_file = output_clim_data_rds_file_raster_parc,
                                            threshold = 10, res_raster = 10
  )
  
  
  
  
}











#### 3. Création du plot de l'utilisation en fonction du climat ####
#------------------------------------------------------------------#
if (FALSE) {
  # Réglage :
  # - Réglages des points, seuil : 10 à 1000 ; Gradient de couleur log (cap à 500) par période
  # - Gradient basés sur l'intensité du chargement
  # - Polygon d'utilisation Calculé sur seuil chargement 100 à 1000 (polygone a 70%)
  # 
  # Les différents plots :
  # - Ellipse
  # - Point 
  # - Violin en fonction fsca
  # - Violin en focntion alti
  
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(patchwork)
  library(ellipse)
  library(ggnewscale)
  library(scales)
  source(file.path(functions_dir, "Functions_plot_snow_ndvi.R"))
  
  #Paramètres d'alpages
  alpages <- "Sanguiniere"
  
  # ENTREE
  # Dossier général pour l'analyse climatique
  clim_case <- file.path(output_dir, "8. Analysis_Climate")
  # Création des dossiers si nécessaire
  if (!dir.exists(clim_case)) {
    dir.create(clim_case, recursive = TRUE)
  }
  # Dossier d'entrée contenant les fichiers RDS
  data_case <- file.path(clim_case, "Data_Use_Fsca_Alti")
  if (!dir.exists(data_case)) {
    dir.create(data_case, recursive = TRUE)
  }
  
  
  # SORTIE
  # Dossier de sortie pour les graphiques
  output_plot_case <- file.path(clim_case, "Graphique")
  if (!dir.exists(output_plot_case)) {
    dir.create(output_plot_case, recursive = TRUE)
  }
  
  
  
  # CODE
  
  # Plot avec les ellipses
  if(FALSE){
  plot_fsca_alti_elypse(
    alpages       = alpages,
    data_dir      = data_case,
    output_dir    = output_plot_case,
    years_to_use  = c(2022, 2023,2024),
    ellipse_level = 0.6)
    }
  
  # Plot avec les points
  if(TRUE){
  plot_fsca_alti_points(
    alpages, 
    data_dir = data_case, 
    output_dir = output_plot_case, 
    years_to_use = c(2022, 2023, 2024))
  }
  
  
  
  
  source(file.path(functions_dir, "Functions_plot_snow_ndvi.R"))
  
  
  
  
  plot_fsca_alti_points_parc_verif(
    alpages       = alpages,
    data_dir      = data_case,
    output_dir    = output_plot_case,
    years_to_use  = c(2022,2023, 2024)
  )
  
  
 
  plot_fsca_alti_points_parc(
    alpages       = alpages,
    data_dir      = data_case,
    output_dir    = output_plot_case,
    years_to_use  = c( 2022, 2023, 2024)
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Plot violin avec le fsca
  if(TRUE){
   plot_violin_fsca(
     alpages, 
     data_dir = data_case, 
     output_dir = output_plot_case, 
     years_to_use = c(2022, 2023, 2024))
   }
  
  
  
  
  # Plot violin avec l'altitude
  if(TRUE){
   plot_violin_alti(
     alpages,
     data_dir   = data_case,
     output_dir = output_plot_case,
     years_to_use = c(2022, 2023, 2024))
   }
  
  
  
  
  
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # PART 3 : Plot 
  
  
  # ENTRE
  
  #Création du dossier de sortie des indicateur pour la visualistaion
  clim_case <- file.path(output_dir, "8. Analysis_Climate")
  data_case <- file.path(clim_case, "Data_Use_Fsca_Alti")
  clim_data_rds_file <- file.path(data_case, paste0("Use_Fsca_Alti_",alpage,".rds"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Fonction pour le df (en RDS)
  
  
  
  library(raster)
  
  # Chemin du raster FSCA calculé pour l'alpage (par exemple "Cayolle")
  fsca_raster_path <- "C:/Users/masso/Documents/STAGE_M2_PERSEE/R_studio/PERSEE_Traitement_Catlog/outputs/8. Analysis_Climate/SMOD/Fsca_Cayolle.tif"
  
  # Chemin du raster DEM couvrant l'alpage
  dem_raster_path <- "C:/Users/masso/Documents/STAGE_M2_PERSEE/R_studio/PERSEE_Traitement_Catlog/raster/Alti/Cayolle_MNT.tif"
  
  # Charger les rasters depuis les fichiers
  r_fsca <- raster(fsca_raster_path)
  r_dem  <- raster(dem_raster_path)
  
  # Harmoniser les CRS si nécessaire
  if (!compareCRS(r_fsca, r_dem)) {
    message("Les CRS diffèrent, reprojection de r_fsca...")
    r_fsca <- projectRaster(r_fsca, crs = crs(r_dem))
  }
  
  # Agréger le DEM pour passer de 5 m à 20 m (facteur 4)
  r_dem_agg <- aggregate(r_dem, fact = 4, fun = mean)
  
  # Déterminer l'emprise commune entre les deux rasters
  common_extent <- intersect(extent(r_dem_agg), extent(r_fsca))
  if (is.null(common_extent)) {
    stop("Les extents des rasters ne se recoupent pas. Vérifiez vos données.")
  }
  
  # Recadrer les deux rasters à l'emprise commune
  r_fsca_crop <- crop(r_fsca, common_extent)
  r_dem_crop  <- crop(r_dem_agg, common_extent)
  
  # Resampler le DEM agrégé pour qu'il s'aligne exactement sur la grille du FSCA
  r_dem_resampled <- resample(r_dem_crop, r_fsca_crop, method = "bilinear")
  
  # Maintenant, chaque pixel du FSCA a une altitude associée (dans r_dem_resampled)
  merged_raster <- stack(r_dem_resampled, r_fsca_crop)
  plot(merged_raster)
  
  
  
  
  
  library(raster)
  library(ggplot2)
  library(dplyr)
  
  # Nom de l'alpage (à adapter si besoin)
  alpage <- "Cayolle"
  
  
  
  
  # ENTREE
  
  visu_case <- file.path(output_dir, "5. Indicateurs_visualisation")
  
  polygon_case <- file.path(visu_case, "Utilisation_par_quinzaine")
  
  polygon_use_shp = file.path(polygon_case, paste0("Use_polygon_",YEAR,"_",alpage,".shp"))
  
  
  # Chemin du raster FSCA calculé pour l'alpage
  fsca_raster_path <- "C:/Users/masso/Documents/STAGE_M2_PERSEE/R_studio/PERSEE_Traitement_Catlog/outputs/8. Analysis_Climate/SMOD/Fsca_123_342_Cayolle.tif"
  
  # Chemin du raster DEM couvrant l'alpage
  dem_raster_path <- "C:/Users/masso/Documents/STAGE_M2_PERSEE/R_studio/PERSEE_Traitement_Catlog/raster/Alti/Cayolle_MNT.tif"
  
  # Charger les rasters depuis les fichiers
  r_fsca <- raster(fsca_raster_path)
  r_dem  <- raster(dem_raster_path)
  
  # Harmoniser le CRS si nécessaire
  if (!compareCRS(r_fsca, r_dem)) {
    message("Les CRS diffèrent, reprojection de r_fsca...")
    r_fsca <- projectRaster(r_fsca, crs = crs(r_dem))
  }
  
  # Agréger le DEM pour passer de 5 m à 20 m (facteur 4)
  r_dem_agg <- aggregate(r_dem, fact = 4, fun = mean)
  
  # Définir l'emprise commune entre les deux rasters
  common_extent <- intersect(extent(r_dem_agg), extent(r_fsca))
  if (is.null(common_extent)) {
    stop("Les extents des rasters ne se recoupent pas. Vérifiez vos données.")
  }
  
  # Recadrer les deux rasters sur l'emprise commune
  r_dem_crop  <- crop(r_dem_agg, common_extent)
  r_fsca_crop <- crop(r_fsca, common_extent)
  
  # Resampler le DEM recadré pour qu'il s'aligne exactement sur la grille du FSCA
  r_dem_resampled <- resample(r_dem_crop, r_fsca_crop, method = "bilinear")
  
  # Créer une pile alignée : couche 1 = altitude, couche 2 = FSCA
  merged_raster <- stack(r_dem_resampled, r_fsca_crop)
  
  # Convertir le raster empilé en data frame (chaque pixel devient une ligne)
  df_merged <- as.data.frame(rasterToPoints(merged_raster))
  # Renommer les colonnes pour faciliter l'utilisation
  names(df_merged)[3:4] <- c("altitude", "FSCA")
  
  # Retirer les éventuels NA
  df_merged <- df_merged %>% filter(!is.na(altitude), !is.na(FSCA))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  library(sf)
  library(dplyr)
  
  add_presence_info <- function(df_merged, polygon_use_shp, crs_raster) {
    # Lire le shapefile qui contient les polygones de présence par quinzaine
    polygon_sf <- st_read(polygon_use_shp, quiet = TRUE)
    
    # S'assurer que le shapefile est dans le même CRS que le raster
    polygon_sf <- st_transform(polygon_sf, crs = crs_raster)
    
    # Convertir le data frame des pixels en objet sf (points)
    # On suppose que df_merged contient les colonnes "x" et "y" (coordonnées)
    df_sf <- st_as_sf(df_merged, coords = c("x", "y"), crs = crs_raster)
    
    # Récupérer les périodes (les niveaux de "month_period")
    periods <- unique(polygon_sf$month_period)
    
    # Pour chaque période, calculer une colonne binaire indiquant la présence du troupeau
    for (p in periods) {
      # Sélectionner le polygone pour la période p
      poly_p <- polygon_sf %>% filter(month_period == p)
      # Pour chaque pixel, déterminer s'il intersecte le polygone de la période p
      inter <- st_intersects(df_sf, poly_p)
      # Créer une nouvelle colonne "presence_<p>" (on remplace espaces et tirets par des underscores)
      colname <- paste0("presence_", gsub("[ -]", "_", tolower(p)))
      df_sf[[colname]] <- sapply(inter, function(x) ifelse(length(x) > 0, 1, 0))
    }
    
    # Optionnel : Conserver les coordonnées en colonnes, en retirant la géométrie
    df_result <- cbind(as.data.frame(st_coordinates(df_sf)), st_drop_geometry(df_sf))
    return(df_result)
  }
  
  # Exemple d'utilisation :
  # df_merged est obtenu à partir de vos rasters (voir votre code de base)
  # polygon_use_shp est le chemin vers le shapefile généré
  # crs_raster peut être obtenu par, par exemple, crs(r_fsca)
  df_merged_with_presence <- add_presence_info(df_merged, polygon_use_shp, crs(r_fsca))
  
  # Vous obtenez ainsi un data frame df_merged_with_presence contenant les colonnes "altitude", "FSCA"
  # et pour chaque quinzaine (ex: "presence_avant_15_jun", "presence_16_-_30_jun", etc.) une valeur 1 ou 0.
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Tracé : nuage de points et courbe lissée
  p <- ggplot(df_merged, aes(x = altitude, y = FSCA)) +
    geom_point(alpha = 0.2, size = 0.5, color = "blue") +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    scale_y_reverse(limits = c(1, 0)) +
    labs(x = "Altitude (m)", y = "FSCA",
         title = paste("FSCA vs Altitude (lissée) pour", alpage)) +
    theme_minimal()
  
  print(p)
  
  
  
  
  
  
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  
  # -------------------------------
  # Supposons que df_merged existe et contient :
  #   - altitude : valeurs numériques (ex. de 1600 à 3000)
  #   - FSCA     : valeurs entre 0 et 1
  # -------------------------------
  
  # 1) Calcul des densités "inversées"
  dens_alt <- density(df_merged$altitude, na.rm = TRUE)
  dens_fsca <- density(df_merged$FSCA, na.rm = TRUE)
  
  # Data frame pour la densité de l'altitude (multipliée par -1 pour renverser la courbe)
  df_dens_alt <- data.frame(
    altitude = dens_alt$x,
    dens = -dens_alt$y  # négatif => la bosse pointe vers le bas
  )
  
  # Data frame pour la densité du FSCA (multipliée par -1 pour renverser la courbe)
  df_dens_fsca <- data.frame(
    FSCA = dens_fsca$x,
    dens = -dens_fsca$y  # négatif => la bosse pointe vers la gauche
  )
  
  # 2) Nuage de points principal : Altitude vs FSCA
  p_main <- ggplot(df_merged, aes(x = altitude, y = FSCA)) +
    geom_point(alpha = 0.2, size = 0.5, color = "blue") +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    scale_x_continuous(limits = c(min(df_merged$altitude), max(df_merged$altitude)), expand = c(0, 0)) +
    scale_y_reverse(limits = c(1, 0), expand = c(0, 0)) +  # FSCA : 1 en haut, 0 en bas
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    labs(x = "Altitude (m)", y = "FSCA")
  
  # 3) Densité de l'altitude en bas (p_xdens) : tracée vers le bas (densité négative)
  p_xdens <- ggplot(df_dens_alt, aes(x = altitude, y = dens)) +
    geom_line(color = "black", size = 1) +
    scale_x_continuous(limits = c(min(df_merged$altitude), max(df_merged$altitude)), expand = c(0, 0)) +
    scale_y_continuous(limits = c(min(df_dens_alt$dens), 0), expand = c(0, 0)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # 4) Densité du FSCA à gauche (p_ydens) : tracée vers la gauche (densité négative)
  p_ydens <- ggplot(df_dens_fsca, aes(x = FSCA, y = dens)) +
    geom_line(color = "black", size = 1) +
    scale_x_reverse(limits = c(1, 0), expand = c(0, 0)) +
    scale_y_continuous(limits = c(min(df_dens_fsca$dens), 0), expand = c(0, 0)) +
    coord_flip() +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # 5) Assemblage final avec patchwork
  # - On crée d'abord une rangée avec la densité FSCA (à gauche) et le nuage de points au centre
  # - Puis on ajoute la densité altitude en dessous
  left_and_main <- wrap_plots(list(p_ydens, p_main), ncol = 2, widths = c(0.2, 1))
  final_plot <- wrap_plots(list(left_and_main, p_xdens), ncol = 1, heights = c(1, 0.25))
  
  # Affichage du graphique final
  print(final_plot)
  
  
  
  
  
  
  
  
  
  
  library(ggplot2)
  library(ggExtra)
  
  # ------------------------------------------------------------------
  # 1) Données d'exemple (remplacez par vos données réelles)
  # ------------------------------------------------------------------
  set.seed(123)
  df_merged <- data.frame(
    altitude = runif(1000, 1600, 3000),
    FSCA     = runif(1000, 0, 1)  # valeurs entre 0 et 1
  )
  
  # ------------------------------------------------------------------
  # 2) Nuage de points + courbe lissée + FSCA inversé
  # ------------------------------------------------------------------
  p <- ggplot(df_merged, aes(x = altitude, y = FSCA)) +
    geom_point(alpha = 0.2, size = 1.5, color = "steelblue") +
    geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2) +
    # FSCA inversé : 1 en haut, 0 en bas
    scale_y_reverse(limits = c(1, 0), expand = c(0, 0)) +
    # Altitude sans marge
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal(base_size = 12) +
    labs(x = "Altitude (m)", y = "FSCA")
  
  # ------------------------------------------------------------------
  # 3) Distributions marginales (densités) avec ggMarginal
  #    - On utilise trim=TRUE, from=0, to=1 pour l'axe FSCA
  #      afin que la densité ne dépasse pas le range [0..1].
  #    - size < 2 pour réduire l'espace des marges
  # ------------------------------------------------------------------
  p_marg <- ggMarginal(
    p,
    type       = "density",
    margins    = "both",            # densités en haut et à droite
    fill       = "grey",
    color      = "lightgrey",
    alpha      = 0.5,
    size       = 6,               # plus petit => marges plus étroites
    # Arguments passés à geom_density() pour la marge X (altitude)
    xparams    = list(
      adjust = 1.5,
      trim   = TRUE,
      from   = min(df_merged$altitude),
      to     = max(df_merged$altitude)
    ),
    # Arguments passés à geom_density() pour la marge Y (FSCA)
    yparams    = list(
      adjust = 1.5,
      trim   = TRUE,
      from   = 0,
      to     = 1
    ),
    # Retire axes et ticks sur les marges
    marginalTheme = theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
  )
  
  # ------------------------------------------------------------------
  # 4) Affichage final
  # ------------------------------------------------------------------
  print(p_marg)
  
  
  
  
  
  # Foncyion pour créer les MNT
  
  library(raster)
  
  # 1. Lister les fichiers DEM à fusionner
  dem_files <- c("Cayolle_1.tif", 
                 "Cayolle_2.tif", 
                 "Cayolle_3.tif", 
                 "Cayolle_4.tif")
  
  # 2. Charger chaque fichier en tant qu'objet raster
  dem_list <- lapply(dem_files, raster)
  
  # 3. Fusionner tous les rasters en un seul
  #    do.call(merge, dem_list) applique la fonction 'merge' à tous les éléments de la liste
  mnt_merged <- do.call(merge, dem_list)
  
  # 4. Enregistrer le MNT fusionné sous forme d'un nouveau fichier TIF
  writeRaster(mnt_merged, filename = "Cayolle_MNT.tif", format = "GTiff", overwrite = TRUE)
  
  cat("Fusion terminée. Le fichier 'Cayolle_MNT.tif' a été créé.\n")
  
  