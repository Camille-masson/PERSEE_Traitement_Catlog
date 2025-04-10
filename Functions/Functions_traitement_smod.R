
load_smod_for_alpage <- function(alpage_name = alpage,
                                 year_1,         # Objet de type Date, ex: as.Date("2018-01-01")
                                 year_2,         # Objet de type Date, ex: as.Date("2023-12-31")
                                 smod_dir,       # Dossier contenant les fichiers SMOD
                                 table_corresp_file) {
  
  # 1) Charger la table de correspondance : alpage -> SMOD_ref
  table_corresp <- read.csv(table_corresp_file, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  
  # 2) Récupérer la dalle SMOD_ref correspondant à l’alpage souhaité
  # Ici, on compare la colonne "alpage" avec l'argument alpage_name
  ref_row <- subset(table_corresp, alpage == alpage_name)
  
  if (nrow(ref_row) == 0) {
    stop(paste("Aucune dalle SMOD_ref trouvée pour l'alpage :", alpage_name))
  }
  smod_ref <- ref_row$SMOD_ref[1]
  
  # 3) Lister les fichiers SMOD dans le dossier, qui correspondent à la dalle smod_ref
  pattern_smod <- paste0("SMOD_", smod_ref, "_")
  smod_files <- list.files(path = smod_dir,
                           pattern = pattern_smod,
                           full.names = TRUE) 
  
  if (length(smod_files) == 0) {
    warning(paste("Aucun fichier SMOD trouvé pour la dalle :", smod_ref))
    return(NULL)
  }
  
  # 4) Filtrer les fichiers en fonction de la période demandée
  library(stringr)
  rasters_list <- list()
  
  for (f in smod_files) {
    f_basename <- basename(f)
    parts <- str_split(f_basename, "_")[[1]]
    
    if (length(parts) < 4) {
      warning(paste("Nom de fichier inattendu, ignoré :", f_basename))
      next
    }
    
    start_str <- parts[3]
    end_str   <- str_remove(parts[4], ".tif")
    
    start_date_file <- as.Date(start_str, format = "%Y%m%d")
    end_date_file   <- as.Date(end_str,   format = "%Y%m%d")
    
    # Critère de chevauchement partiel : (start <= year_2) et (end >= year_1)
    if ((start_date_file <= year_2) && (end_date_file >= year_1)) {
      r <- raster(f)
      rasters_list[[f_basename]] <- r
    }
  }
  
  if (length(rasters_list) == 0) {
    warning("Aucun raster SMOD ne correspond à la période demandée.")
    return(NULL)
  }
  
  smod_stack <- stack(rasters_list)
  return(smod_stack)
}









crop_smod_by_UP <- function(stack_smod_viso, alpage, alpage_info_file, UP_file) {
  
  alpage = "Cayolle"
  # Récupération du shapefile UP pour l'alpage
  UP_selected <- get_UP_shp(alpage, alpage_info_file, UP_file)
  
  
  
  # 1) Vérifier le CRS du raster
  crs_raster <- crs(current_raster)  
  # 2) Vérifier le CRS du shapefile (en sf)
  crs_shp <- st_crs(UP_selected)  # si c’est un objet sf
  
  
  if (nrow(UP_selected) == 0) {
    warning("Aucune Unité Pastorale trouvée pour l'alpage spécifié. Le raster crop ne sera pas généré.")
    return(NULL)
  }
  
  # Initialiser une liste pour stocker les rasters recadrés par année
  cropped_by_year <- list()
  
  # Parcourir chaque couche du stack SMOD
  for (i in 1:nlayers(stack_smod_viso)) {
    # Extraction de la couche i
    current_raster <- stack_smod_viso[[i]]
    
    # Crop et mask du raster par l'UP sélectionné
    cropped_raster <- mask(crop(current_raster, UP_selected), UP_selected)
    
    # Extraction de l'année à partir du nom de la couche
    # On suppose que le nom est de la forme : "SMOD_{ref}_{YYYYMMDD}_{YYYYMMDD}.tif"
    layer_name <- names(stack_smod_viso)[i]
    # Retirer l'extension ".tif" si présente
    layer_name <- gsub("\\.tif$", "", layer_name)
    # Découper le nom par "_"
    parts <- strsplit(layer_name, "_")[[1]]
    if (length(parts) < 3) {
      warning(paste("Nom de couche inattendu :", layer_name))
      next
    }
    # La 3ème partie contient la date de début sous le format YYYYMMDD, on en extrait l'année
    year_extracted <- substr(parts[3], 1, 4)
    
    # Stocker le raster crop dans la liste par année
    if (!year_extracted %in% names(cropped_by_year)) {
      cropped_by_year[[year_extracted]] <- list()
    }
    cropped_by_year[[year_extracted]][[length(cropped_by_year[[year_extracted]]) + 1]] <- cropped_raster
  }
  
  # Pour chaque année, agréger (moyenne) les rasters s'il y a plusieurs couches et sauvegarder le résultat
  for (year in names(cropped_by_year)) {
    raster_list <- cropped_by_year[[year]]
    if (length(raster_list) == 0) next
    
    if (length(raster_list) > 1) {
      # Empiler les couches pour l'année et calculer la moyenne pixel par pixel
      raster_stack_year <- stack(raster_list)
      aggregated_raster <- calc(raster_stack_year, fun = mean, na.rm = TRUE)
    } else {
      aggregated_raster <- raster_list[[1]]
    }
    
    # Création du nom de fichier de sortie (ex: "SMOD_Cayolle_2013.tif")
    output_file <- paste0("SMOD_", alpage, "_", year, ".tif")
    
    # Sauvegarde du raster agrégé au format GeoTIFF (écrase le fichier existant si besoin)
    writeRaster(aggregated_raster, filename = output_file, format = "GTiff", overwrite = TRUE)
    message("Raster saved: ", output_file)
  }
  
  # Optionnel : Retourner la liste des rasters crop par année pour un traitement ultérieur
  return(cropped_by_year)
}



library(raster)
library(sf)
library(stringr)
library(tools)  # pour file_path_sans_ext

process_smod_for_alpage <- function(alpage_name, year_1, year_2,
                                    case_SMOD_file, table_corresp_file,
                                    alpage_info_file, UP_file, output_SMOD_case) {
  # 1. Charger la table de correspondance : alpage -> SMOD_ref
  table_corresp <- read.csv(table_corresp_file, header = TRUE, sep = ";", stringsAsFactors = FALSE)
  ref_row <- subset(table_corresp, alpage == alpage_name)
  if(nrow(ref_row) == 0) {
    stop(paste("Aucune dalle SMOD_ref trouvée pour l'alpage :", alpage_name))
  }
  smod_ref <- ref_row$SMOD_ref[1]
  
  # 2. Lister les fichiers SMOD correspondant à la dalle
  pattern_smod <- paste0("SMOD_", smod_ref, "_")
  smod_files <- list.files(path = case_SMOD_file, pattern = pattern_smod, full.names = TRUE)
  if(length(smod_files) == 0) {
    warning(paste("Aucun fichier SMOD trouvé pour la dalle :", smod_ref))
    return(invisible(NULL))
  }
  
  # 3. Filtrer les fichiers selon la période [year_1, year_2]
  filtered_files <- c()
  for(f in smod_files) {
    f_basename <- basename(f)
    parts <- str_split(f_basename, "_")[[1]]
    if(length(parts) < 4) {
      warning(paste("Nom de fichier inattendu, ignoré :", f_basename))
      next
    }
    start_str <- parts[3]                     # Exemple : "19960901"
    end_str   <- str_remove(parts[4], "\\.tif") # Exemple : "19970831"
    
    start_date_file <- as.Date(start_str, format = "%Y%m%d")
    end_date_file   <- as.Date(end_str, format = "%Y%m%d")
    
    if((start_date_file <= year_2) && (end_date_file >= year_1)) {
      filtered_files <- c(filtered_files, f)
    }
  }
  
  if(length(filtered_files) == 0) {
    warning("Aucun fichier SMOD ne correspond à la période demandée.")
    return(invisible(NULL))
  }
  
  
  
  # 4. Charger le shapefile des Unités Pastorales (UP) pour l'alpage via get_UP_shp
  UP_selected <- get_UP_shp(alpage_name, alpage_info_file, UP_file)
  if(nrow(UP_selected) == 0) {
    warning("Aucune Unité Pastorale trouvée pour l'alpage spécifié. Le raster crop ne sera pas généré.")
    return(invisible(NULL))
  }
  
  # 5. Harmoniser le CRS entre le raster et l'UP
  test_raster <- raster(filtered_files[1])
  crs_raster <- crs(test_raster)
  crs_shp    <- st_crs(UP_selected)
  if(!is.null(crs_raster) && !is.null(crs_shp)) {
    if(crs_raster@projargs != crs_shp$wkt) {
      UP_selected <- st_transform(UP_selected, crs_raster@projargs)
    }
  }
  UP_sp <- as(UP_selected, "Spatial")
  
  # 6. Pour chaque fichier filtré, effectuer crop + mask et sauvegarder avec le nom modifié
  output_files <- c()
  for(f in filtered_files) {
    r <- raster(f)
    
    if(is.null(intersect(extent(r), extent(UP_sp)))) {
      warning(paste("Le raster", basename(f), "n'a pas d'overlap avec l'UP. Ignoré."))
      next
    }
    
    cropped_raster <- mask(crop(r, UP_sp), UP_sp)
    
    # Conserver le nom d'origine sans l'extension
    f_basename <- basename(f)
    base_name <- file_path_sans_ext(f_basename)
    
    # Si le nom se termine déjà par _alpage, ne pas le doubler
    if(grepl(paste0("_", alpage_name, "$"), base_name)) {
      output_name <- paste0(base_name, ".tif")
    } else {
      output_name <- paste0(base_name, "_", alpage_name, ".tif")
    }
    output_path <- file.path(output_SMOD_case, output_name)
    
    writeRaster(cropped_raster, filename = output_path, format = "GTiff", overwrite = TRUE)
    message("Raster saved: ", output_path)
    output_files <- c(output_files, output_path)
  }
  
  return(invisible(output_files))
}








library(raster)
library(stringr)
library(sf)
library(tools)  # pour file_path_sans_ext

median_smod_for_alpage <- function(alpage_name, output_SMOD_case) {
  # 1. Lister les fichiers déjà traités pour l'alpage dans output_SMOD_case
  # Les fichiers attendus se terminent par "_<alpage_name>.tif"
  files <- list.files(path = output_SMOD_case, 
                      pattern = paste0("_", alpage_name, "\\.tif$"), 
                      full.names = TRUE)
  
  if(length(files) == 0) {
    stop(paste("Aucun fichier SMOD pour l'alpage", alpage_name, "n'a été trouvé dans", output_SMOD_case))
  }
  
  # 2. Extraire les dates à partir du nom des fichiers
  # On s'attend à des noms de la forme : "SMOD_31TGK_20130901_20140831_Cayolle.tif"
  start_dates <- c()
  end_dates <- c()
  
  for(f in files) {
    base_name <- file_path_sans_ext(basename(f))
    parts <- str_split(base_name, "_")[[1]]
    if(length(parts) < 5) {
      warning(paste("Nom de fichier inattendu :", base_name))
      next
    }
    # La 3ème partie est la date de début, la 4ème la date de fin
    start_dates <- c(start_dates, parts[3])
    end_dates <- c(end_dates, parts[4])
  }
  
  if(length(start_dates) == 0 || length(end_dates) == 0) {
    stop("Aucune date n'a pu être extraite des fichiers.")
  }
  
  # Convertir les dates en objet Date pour comparer
  start_dates_date <- as.Date(start_dates, format = "%Y%m%d")
  end_dates_date <- as.Date(end_dates, format = "%Y%m%d")
  
  overall_start <- format(min(start_dates_date), "%Y%m%d")
  overall_end   <- format(max(end_dates_date), "%Y%m%d")
  
  # 3. Charger tous les rasters en pile
  raster_list <- lapply(files, raster)
  raster_stack <- stack(raster_list)
  
  # 4. Calculer la médiane pixel par pixel
  median_raster <- calc(raster_stack, fun = median, na.rm = TRUE)
  
  # 5. Construire le nom du fichier de sortie
  # Exemple : "SMOD_median_20130901_20240831_Cayolle.tif"
  output_name <- paste0("SMOD_median_", overall_start, "_", overall_end, "_", alpage_name, ".tif")
  output_path <- file.path(output_SMOD_case, output_name)
  
  # 6. Enregistrer le raster médian
  writeRaster(median_raster, filename = output_path, format = "GTiff", overwrite = TRUE)
  message("Median raster saved: ", output_path)
  
  return(invisible(output_path))
}









compute_fsca_for_alpage <- function(alpage_name, output_SMOD_case) {
  # 1. Lister le(s) fichier(s) raster médian déjà créés pour cet alpage.
  # On s'attend à un nom de fichier de la forme : 
  # "SMOD_median_YYYYMMDD_YYYYMMDD_Cayolle.tif"
  median_files <- list.files(path = output_SMOD_case, 
                             pattern = paste0("^SMOD_median_.*_", alpage_name, "\\.tif$"),
                             full.names = TRUE)
  
  if(length(median_files) == 0) {
    stop(paste("Aucun fichier median SMOD trouvé pour l'alpage", alpage_name))
  }
  # On prend le premier trouvé (ou on peut vérifier s'il y en a qu'un)
  median_file <- median_files[1]
  median_raster <- raster(median_file)
  
  # 2. Extraire les valeurs du raster médian (qui représentent la date de déneigement)
  values_med <- getValues(median_raster)
  N <- length(values_med)
  
  if(N == 0) {
    stop("Aucune valeur trouvée dans le raster médian.")
  }
  
  # 3. Calculer pour chaque pixel le FSCA :
  #    FSCA = (nombre de pixels avec date > date du pixel) / (N_valid - 1)
  # On exclut les NA.
  valid_idx <- which(!is.na(values_med))
  fsca_values <- rep(NA, N)
  if(length(valid_idx) > 0) {
    valid_vals <- values_med[valid_idx]
    rnk <- rank(valid_vals, ties.method = "min")
    N_valid <- length(valid_vals)
    fsca_values[valid_idx] <- (N_valid - rnk) / (N_valid - 1)
  }
  
  # 4. Créer un nouveau raster FSCA avec les mêmes propriétés que le raster médian
  fsca_raster <- median_raster
  fsca_raster <- setValues(fsca_raster, fsca_values)
  
  # 5. Déterminer la date de début et la date de fin parmi les valeurs du raster médian
  overall_start <- min(values_med, na.rm = TRUE)
  overall_end   <- max(values_med, na.rm = TRUE)
  # On suppose ici que les valeurs sont au format YYYYMMDD (par exemple 20130901)
  overall_start_str <- as.character(overall_start)
  overall_end_str   <- as.character(overall_end)
  
  # 6. Construire le nom de fichier de sortie de façon interactive
  # Exemple : "SMOD_median_fsca_20130901_20240831_Cayolle.tif"
  output_name <- paste0("Fsca_", alpage_name, ".tif")
  output_path <- file.path(output_SMOD_case, output_name)
  
  # 7. Enregistrer le raster FSCA
  writeRaster(fsca_raster, filename = output_path, format = "GTiff", overwrite = TRUE)
  message("Fsca raster saved: ", output_path)
  
  return(invisible(output_path))
}





generate_presence_data_by_quinzaine <- function(SMOD_tif_file, MNT_tif_file, polygon_case, 
                                                years, alpage, 
                                                output_clim_data_rds_file) {
  # Chargement des librairies nécessaires
  library(raster)
  library(sf)
  library(dplyr)
  library(lubridate)
  library(MASS)
  library(stringr)
  
  # ------------------------------------------------------------
  # 1) Chargement et préparation des rasters
  # ------------------------------------------------------------
  # Charger le raster FSCA (SMOD) et le DEM
  r_fsca <- raster(SMOD_tif_file)
  r_dem  <- raster(MNT_tif_file)
  
  # Harmoniser le CRS si nécessaire
  if (!compareCRS(r_fsca, r_dem)) {
    message("Les CRS diffèrent, reprojection de r_fsca...")
    r_fsca <- projectRaster(r_fsca, crs = crs(r_dem))
  }
  
  # Agréger le DEM pour passer de 5 m à 20 m (facteur 4)
  r_dem_agg <- aggregate(r_dem, fact = 4, fun = mean)
  
  # Définir l'emprise commune
  common_extent <- intersect(extent(r_dem_agg), extent(r_fsca))
  if (is.null(common_extent)) {
    stop("Les extents des rasters ne se recoupent pas. Vérifiez vos données.")
  }
  
  # Recadrer et resampler pour aligner les rasters
  r_dem_crop  <- crop(r_dem_agg, common_extent)
  r_fsca_crop <- crop(r_fsca, common_extent)
  r_dem_resampled <- resample(r_dem_crop, r_fsca_crop, method = "bilinear")
  
  # Créer une pile alignée : couche 1 = altitude, couche 2 = FSCA
  merged_raster <- stack(r_dem_resampled, r_fsca_crop)
  
  # Convertir le raster empilé en data frame (chaque pixel = 1 ligne)
  df_merged <- as.data.frame(rasterToPoints(merged_raster))
  names(df_merged)[3:4] <- c("altitude", "FSCA")
  df_merged <- df_merged %>% filter(!is.na(altitude), !is.na(FSCA))
  
  # ------------------------------------------------------------
  # 2) Conversion en objet sf (points)
  # ------------------------------------------------------------
  df_sf <- st_as_sf(df_merged, coords = c("x", "y"), crs = crs(r_fsca))
  
  # ------------------------------------------------------------
  # 3) Fonction pour nettoyer le nom d'une période
  # ------------------------------------------------------------
  clean_period_name <- function(period) {
    period_clean <- str_replace_all(period, c("é" = "e", "è" = "e", "à" = "a",
                                              "â" = "a", "ù" = "u", "ô" = "o", 
                                              "î" = "i", "ï" = "i", "ç" = "c"))
    period_clean <- tolower(period_clean)
    period_clean <- gsub(" - ", "_", period_clean)
    period_clean <- gsub("[^[:alnum:]_]+", "_", period_clean)
    period_clean <- gsub("_+", "_", period_clean)
    period_clean <- str_replace_all(period_clean, "^_|_$", "")
    return(period_clean)
  }
  
  # ------------------------------------------------------------
  # 4) Pour chaque année, ajouter les informations de présence par quinzaine
  # ------------------------------------------------------------
  for (yr in years) {
    # Construction du chemin du shapefile pour l'année courante
    polygon_use_shp <- file.path(polygon_case, paste0("Use_polygon_", yr, "_", alpage, ".shp"))
    if (!file.exists(polygon_use_shp)) {
      message("Shapefile non trouve pour l'annee ", yr, ": ", polygon_use_shp, ". Passe a l'annee suivante.")
      next
    }
    polygon_sf <- st_read(polygon_use_shp, quiet = TRUE)
    polygon_sf <- st_transform(polygon_sf, crs = crs(r_fsca))
    
    if (!"mnth_pr" %in% names(polygon_sf)) {
      message("Le shapefile pour l'annee ", yr, " ne contient pas le champ 'mnth_pr'.")
      next
    }
    
    for (p in unique(polygon_sf$mnth_pr)) {
      # Filtrer le polygone pour la période p
      poly_p <- polygon_sf %>% filter(mnth_pr == p)
      inter <- st_intersects(df_sf, poly_p)
      
      # Nettoyage du nom de la période et création du nom de colonne
      p_clean <- clean_period_name(as.character(p))
      colname <- paste0("presence_", yr, "_", p_clean)
      
      # Pour chaque point, présence = 1 s'il y a intersection, sinon 0
      df_sf[[colname]] <- sapply(inter, function(x) ifelse(length(x) > 0, 1, 0))
    }
  }
  
  # ------------------------------------------------------------
  # 5) Conversion finale en data frame et ajout d'une colonne "year_range"
  # ------------------------------------------------------------
  df_final <- cbind(as.data.frame(st_coordinates(df_sf)), st_drop_geometry(df_sf))
  
  # Créer une colonne "year_range" indiquant la période d'analyse (ex: "2022-2024")
  df_final$year_range <- paste0(min(years), "-", max(years))
  
  # Enregistrement du résultat au format RDS
  saveRDS(df_final, file = output_clim_data_rds_file)
  message("Les donnees climatiques ont ete enregistrees dans : ", output_clim_data_rds_file)
  
  return(df_final)
}






generate_loading_data_by_quinzaine <- function(SMOD_tif_file, MNT_tif_file, chargement_case, 
                                               years, alpage, output_rds_file,
                                               threshold = 10, res_raster = 10) {
  # Chargement des librairies nécessaires
  library(raster)
  library(dplyr)
  
  # ------------------------------------------------------------
  # 1) Reconstruction du jeu de données de base à partir des rasters SMOD et DEM
  # ------------------------------------------------------------
  r_fsca <- raster(SMOD_tif_file)
  r_dem  <- raster(MNT_tif_file)
  
  if (!compareCRS(r_fsca, r_dem)) {
    message("Les CRS diffèrent, reprojection de r_fsca...")
    r_fsca <- projectRaster(r_fsca, crs = crs(r_dem))
  }
  
  # Agréger le DEM pour passer de 5 m à 10 m (facteur 2)
  r_dem_agg <- aggregate(r_dem, fact = 2, fun = mean)
  
  # Utiliser l'emprise commune entre le DEM agrégé et le FSCA
  common_extent <- intersect(extent(r_dem_agg), extent(r_fsca))
  if (is.null(common_extent)) stop("Les extents des rasters ne se recoupent pas.")
  
  # Recadrer le DEM agrégé et le FSCA sur l'emprise commune
  r_dem_crop <- crop(r_dem_agg, common_extent)
  r_fsca_crop <- crop(r_fsca, common_extent)
  
  # Créer un template à la résolution désirée (10 m) à partir du DEM recadré
  template_raster <- r_dem_crop  # déjà en 10 m
  # Resampler le FSCA sur ce template
  r_fsca_resampled <- resample(r_fsca_crop, template_raster, method = "bilinear")
  
  # Créer une pile alignée : couche 1 = altitude, couche 2 = FSCA (rés. 10 m)
  merged_raster <- stack(r_dem_crop, r_fsca_resampled)
  df_base <- as.data.frame(rasterToPoints(merged_raster))
  names(df_base)[3:4] <- c("altitude", "FSCA")
  df_base <- df_base %>% filter(!is.na(altitude), !is.na(FSCA))
  
  # ------------------------------------------------------------
  # 2) Extraction des données de chargement par quinzaine pour chaque année
  # ------------------------------------------------------------
  # Les périodes attendues (noms des fichiers)
  periods <- c("16_30_jun", "1_15_jul", "16_30_jul", "1_15_aou", "16_30_aou", "1_15_sep", "apres16_sep")
  
  df_loading <- NULL
  for (yr in years) {
    message("Traitement de l'année ", yr)
    # Le dossier pour l'année courante (ex: "2023_Cayolle")
    case_alpage <- file.path(chargement_case, paste0(yr, "_", alpage))
    for (p in periods) {
      message("  Période : ", p)
      # Construction du chemin d'accès
      raster_path <- file.path(case_alpage, paste0("by_quinzaine_", yr, "_", alpage, "_", p, ".tif"))
      if (!file.exists(raster_path)) {
        message("    Fichier non trouvé pour ", yr, " - ", p, ". Passage...")
        next
      }
      r_load <- raster(raster_path)
      # Resample le raster de chargement sur le template (rés. 10 m)
      r_load_resampled <- resample(r_load, template_raster, method = "bilinear")
      df_load <- as.data.frame(rasterToPoints(r_load_resampled))
      if(nrow(df_load) == 0) next
      colname_load <- paste0("load_", yr, "_", p)
      names(df_load)[3] <- colname_load
      colname_presence <- paste0("presence_", yr, "_", p)
      df_load <- df_load %>% mutate(!!colname_presence := ifelse(.[[colname_load]] > threshold, 1, 0))
      
      if (is.null(df_loading)) {
        df_loading <- df_load
      } else {
        df_loading <- merge(df_loading, df_load, by = c("x", "y"), all = TRUE)
      }
    }
  }
  
  # ------------------------------------------------------------
  # 3) Fusion du jeu de données de base et des données de chargement
  # ------------------------------------------------------------
  df_final <- merge(df_base, df_loading, by = c("x", "y"), all.x = TRUE)
  df_final$year_range <- paste0(min(years), "-", max(years))
  
  # Sauvegarde du résultat en RDS
  saveRDS(df_final, file = output_rds_file)
  message("Les données de chargement par quinzaine ont été enregistrées dans : ", output_rds_file)
  
  return(df_final)
}







































### Fonction pour calculer le centroïde d'un polygone fermé (optionnel)
polygon_centroid <- function(x, y) {
  n <- length(x) - 1  # n segments
  A <- 0.5 * sum(x[1:n] * y[2:(n+1)] - x[2:(n+1)] * y[1:n])
  Cx <- sum((x[1:n] + x[2:(n+1)]) * (x[1:n] * y[2:(n+1)] - x[2:(n+1)] * y[1:n])) / (6 * A)
  Cy <- sum((y[1:n] + y[2:(n+1)]) * (x[1:n] * y[2:(n+1)] - x[2:(n+1)] * y[1:n])) / (6 * A)
  return(c(Cx, Cy))
}

### Fonction pour générer le plot pour un alpage
plot_fsca_alti_elypse_one <- function(alpage, data_rds, years_to_use, ellipse_level) {
  # Charger les données depuis le fichier RDS
  data <- readRDS(data_rds)
  
  # Paramètres fixes
  period_levels <- c("16_30_jun", "1_15_jul", "16_30_jul", "1_15_aou", "16_30_aou")
  period_to_number <- c(
    "16_30_jun" = 1,
    "1_15_jul"  = 2,
    "16_30_jul" = 3,
    "1_15_aou"  = 4,
    "16_30_aou" = 5
  )
  
  # Palette de couleurs pour les périodes (utilise des numéros 1 à 5)
  fill_map_nums <- c(
    "1" = "#2c7bb6",  # pour la période 16-30 Juin
    "2" = "#abd9e9",  # pour la période 1-15 Juillet
    "3" = "#ffffbf",  # pour la période 16-30 Juillet
    "4" = "#fdae61",  # pour la période 1-15 Août
    "5" = "#d7191c"   # pour la période 16-30 Août
  )
  
  # Mise en forme longue et filtrage direct
  data_long <- data %>%
    pivot_longer(
      cols = starts_with("load_"),
      names_to = c("load_year", "load_period"),
      names_pattern = "load_(\\d{4})_(.*)",
      values_to = "load"
    ) %>%
    mutate(load_year = as.integer(load_year)) %>%
    filter(load_year %in% years_to_use, load_period %in% period_levels)
  
  # Filtrer les valeurs [50..1000]
  data_in <- data_long %>%
    filter(!is.na(load), load >= 50, load <= 1000)
  
  data_in$load_period <- factor(data_in$load_period, levels = period_levels)
  
  # Plages pour l'altitude et FSCA
  range_alt  <- range(data_in$altitude, na.rm = TRUE)
  range_fsca <- c(0, 1)
  
  ellipses_list <- list()
  
  # Construction des ellipses
  for (pname in period_levels) {
    for (yr in years_to_use) {
      df_sub <- data_in %>% filter(load_period == pname, load_year == yr)
      if(nrow(df_sub) < 5) next
      
      xy <- as.matrix(df_sub[, c("altitude", "FSCA")])
      cov_mat <- cov(xy)
      mean_vec <- colMeans(xy)
      
      ell_coords <- ellipse(cov_mat, centre = mean_vec, level = ellipse_level, npoints = 200)
      ell_df <- as.data.frame(ell_coords)
      names(ell_df) <- c("x", "y")
      
      # Fermer la forme
      ell_df <- rbind(ell_df, ell_df[1, ])
      
      # Tronquer l'ellipse dans la zone [range_alt, 0..1]
      ell_df$x <- pmax(range_alt[1],  pmin(range_alt[2],  ell_df$x))
      ell_df$y <- pmax(range_fsca[1], pmin(range_fsca[2], ell_df$y))
      
      ell_df$load_year   <- yr
      ell_df$load_period <- pname
      ell_df$period_num  <- as.character(period_to_number[pname])
      
      ellipses_list[[paste0(pname, "_", yr)]] <- ell_df
    }
  }
  
  ellipses_all <- bind_rows(ellipses_list)
  ellipses_all$load_period <- factor(ellipses_all$load_period, levels = period_levels)
  ellipses_all$period_num  <- factor(ellipses_all$period_num, levels = c("1","2","3","4","5"))
  
  # Construction du graphique
  p <- ggplot() +
    facet_wrap(~ load_year) +
    
    # Ellipses colorées selon period_num
    geom_polygon(
      data = ellipses_all,
      aes(
        x = y,           # FSCA en X (inversé)
        y = x,           # Altitude en Y
        group = interaction(load_period, load_year),
        fill  = period_num
      ),
      color = "black",
      alpha = 0.4,
      size  = 1
    ) +
    
    # Axes
    scale_x_reverse(limits = c(1, 0), expand = c(0, 0)) +
    scale_y_continuous(limits = range_alt, expand = c(0, 0)) +
    
    labs(
      x = "FSCA",
      y = "Altitude (m)",
      title = alpage
    ) +
    
    # Échelle de couleurs manuelle
    scale_fill_manual(
      name   = "Période",
      values = fill_map_nums,
      breaks = c("1","2","3","4","5"),
      labels = c(
        "16-30 Juin",
        "1-15 Juillet",
        "16-30 Juillet",
        "1-15 Août",
        "16-30 Août"
      )
    ) +
    
    theme_bw(base_size = 14) +
    theme(
      legend.position  = "right", # On peut ajuster ici si besoin
      strip.background = element_rect(fill = "grey90"),
      strip.text       = element_text(face = "bold")
    )
  
  return(p)
}

### Combinaison de plusieurs alpages avec légende commune en bas
plot_multiple_alpages <- function(alpages, data_dir, output_dir, years_to_use, ellipse_level) {
  plots <- list()
  
  for (alpage in alpages) {
    data_rds <- file.path(data_dir, paste0("Use_Fsca_Alti_", alpage, "_by_raster.rds"))
    p_alpage <- plot_fsca_alti_elypse_one(alpage, data_rds, years_to_use, ellipse_level)
    plots[[alpage]] <- p_alpage
  }
  
  # Fusionner les plots en une seule figure avec une légende commune en bas
  combined_plot <- (
    wrap_plots(plots, ncol = 1) + 
      plot_layout(guides = "collect")
  ) & theme(legend.position = "bottom")
  
  # Ajouter un titre global
  combined_plot <- combined_plot + plot_annotation(
    title = paste0(
      "Alpages: ", paste(alpages, collapse = ", "), 
      " | Années: ", paste(years_to_use, collapse = ", "),
      " | Ellipse: ", ellipse_level
    ),
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )
  
  # Sauvegarder le plot
  plot_filename <- paste0(
    "Plot_fsca_alti_elypse_",
    paste(alpages, collapse = "_"),
    "_Years_", paste(years_to_use, collapse = "_"),
    "_Ellipse_", ellipse_level, ".jpg"
  )
  output_file <- file.path(output_dir, plot_filename)
  
  ggsave(
    filename = output_file,
    plot     = combined_plot,
    device   = "jpg",
    width    = 8.27,
    height   = 11.69,
    units    = "in",
    dpi      = 300
  )
  
  return(combined_plot)
}



### Dictionnaire pour renommer les quinzaines dans la légende
label_map <- c(
  "16_30_jun" = "16-30 Juin",
  "1_15_jul"  = "1-15 Juillet",
  "16_30_jul" = "16-30 Juillet",
  "1_15_aou"  = "1-15 Août",
  "16_30_aou" = "16-30 Août"
)

### 1) Fonction pour tracer les points pour un seul alpage, par quinzaine
plot_fsca_alti_points_one <- function(alpage,
                                      data_rds,
                                      years_to_use = c(2022, 2023, 2024)) {
  # Définir les quinzaines dans l'ordre souhaité
  periods <- c("16_30_jun", "1_15_jul", "16_30_jul", "1_15_aou", "16_30_aou")
  
  # Palette de couleurs pour chaque période (gradient)
  color_map <- list(
    "16_30_jun" = c("#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"),
    "1_15_jul"  = c("#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d"),
    "16_30_jul" = c("#fdae6b", "#fd8d3c", "#f16913", "#d94801", "#a63603", "#7f2704"),
    "1_15_aou"  = c("#c994c7", "#df65b0", "#e7298a", "#ce1256", "#980043", "#67001f"),
    "16_30_aou" = c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b")
  )
  
  # Charger les données
  data <- readRDS(data_rds)
  
  # Transformation en format long et filtrage
  data_long <- data %>% 
    pivot_longer(
      cols          = starts_with("load_"),
      names_to      = c("load_year", "load_period"),
      names_pattern = "load_(\\d{4})_(.*)",
      values_to     = "load"
    ) %>%
    mutate(load_year = as.integer(load_year)) %>%
    filter(load_year %in% years_to_use, load_period %in% periods)
  
  # Filtrer la charge [50..1000] et limiter à 500
  data_in <- data_long %>%
    filter(!is.na(load), load >= 50, load <= 1000) %>%
    mutate(capped_load = ifelse(load > 500, 500, load))
  
  # Graphique de base : x = FSCA (inversé), y = altitude, facettes par load_year
  p <- ggplot(data_in, aes(x = FSCA, y = altitude)) +
    facet_wrap(~ load_year) +
    scale_x_reverse(limits = c(1, 0), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x = "FSCA",
      y = "Altitude (m)",
      title = alpage
    ) +
    theme_bw(base_size = 14) +
    theme(
      strip.background = element_rect(fill = "grey90"),
      strip.text       = element_text(face = "bold")
    )
  
  # Pour chaque quinzaine, ajouter une couche de points avec son gradient distinct
  for (pname in periods) {
    df_p <- data_in %>% filter(load_period == pname)
    if (nrow(df_p) == 0) next
    
    # Sélectionner aléatoirement 1/10 des points de cette quinzaine
    df_p <- df_p %>% sample_frac(0.5)
    
    p <- p + new_scale_color() +
      geom_point(
        data = df_p,
        aes(color = capped_load),
        size = 1.5, alpha = 0.6
      ) +
      scale_color_gradient(
        low    = color_map[[pname]][1],
        high   = color_map[[pname]][length(color_map[[pname]])],
        limits = c(50, 500),
        trans  = "log",
        oob    = squish,
        name   = "",  # Le titre sera géré dans guide_colorbar
        breaks = c(50, 500),
        labels = c("50", "500"),
        guide  = guide_colorbar(
          barwidth       = 1,      # Barre très courte en longueur
          barheight      = 0.8,    # Un peu plus haute
          direction      = "horizontal",
          label.position = "top",
          title          = label_map[[pname]],
          title.position = "bottom",
          title.hjust    = 0.5,
          order          = which(periods == pname)
        )
      )
  }
  
  return(p)
}

### 2) Fonction pour empiler plusieurs alpages (en colonne) et sauvegarder au format A4 portrait
plot_multiple_alpages_points <- function(alpages,
                                         data_dir,
                                         output_dir,
                                         years_to_use = c(2022, 2023, 2024),
                                         file_prefix  = "Plot_fsca_alti_points",
                                         dpi          = 300) {
  plots <- list()
  
  for (i in seq_along(alpages)) {
    alpage <- alpages[i]
    data_rds <- file.path(data_dir, paste0("Use_Fsca_Alti_", alpage, "_by_raster.rds"))
    
    # Générer le plot pour cet alpage
    p_alpage <- plot_fsca_alti_points_one(alpage, data_rds, years_to_use)
    
    # Masquer la légende pour tous sauf le dernier afin d'avoir une légende unique
    if (i < length(alpages)) {
      p_alpage <- p_alpage + theme(legend.position = "none")
    }
    plots[[alpage]] <- p_alpage
  }
  
  # Combiner les graphiques en une seule colonne et collecter la légende
  combined_plot <- wrap_plots(plots, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Titre global
  combined_plot <- combined_plot + plot_annotation(
    title = paste0(
      "Alpages: ", paste(alpages, collapse = ", "),
      " | Années: ", paste(years_to_use, collapse = ", ")
    ),
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )
  
  # Nom du fichier de sortie
  plot_filename <- paste0(
    file_prefix, "_",
    paste(alpages, collapse = "_"),
    "_Years_", paste(years_to_use, collapse = "_"),
    ".jpg"
  )
  output_file <- file.path(output_dir, plot_filename)
  
  # Sauvegarder au format JPG (A4 portrait)
  ggsave(
    filename = output_file,
    plot     = combined_plot,
    device   = "jpg",
    width    = 8.27,
    height   = 11.69,
    units    = "in",
    dpi      = dpi
  )
  
  message("Plot saved to: ", output_file)
  return(combined_plot)
}





### 1) Fonction pour tracer le violin plot pour un seul alpage
plot_fsca_alti_violin_one <- function(alpage,
                                      data_rds,
                                      years_to_use = c(2022, 2023, 2024)) {
  # Charger les données et supprimer la colonne 'period' si elle existe
  data <- readRDS(data_rds)
  data_mod <- data %>% select(-any_of("period"))
  
  # Transformation en format long : rassembler les colonnes "presence" et "load"
  data_long <- data_mod %>% 
    pivot_longer(
      cols = matches("^(presence|load)_"),
      names_to = c("var", "year", "period"),
      names_pattern = "(presence|load)_(\\d{4})_(.*)",
      values_to = "value"
    ) %>%
    # Reconstituer une table avec deux colonnes (presence et load)
    pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    # Filtrer pour ne garder que les pixels utilisés, 
    # dont le load est <= 700 et appartenant aux années souhaitées
    filter(presence == 1,
           load <= 700,
           as.integer(year) %in% years_to_use) %>%
    # Exclure explicitement les données correspondant à "apres16_sep"
    filter(period != "apres16_sep") %>%
    # Recoder la variable 'period' pour obtenir des libellés explicites
    mutate(period = recode(period,
                           "16_30_jun" = "16-30 Juin",
                           "1_15_jul"  = "1-15 Juillet",
                           "16_30_jul" = "16-30 Juillet",
                           "1_15_aou"  = "1-15 Août",
                           "16_30_aou" = "16-30 Août",
                           "1_15_sep"  = "1-15 Septembre"))
  
  # Forcer l'ordre souhaité pour les périodes sur l'axe x
  period_levels <- c("16-30 Juin", "1-15 Juillet", "16-30 Juillet", 
                     "1-15 Août", "16-30 Août", "1-15 Septembre")
  data_long$period <- factor(data_long$period, levels = period_levels)
  
  # Création du violin plot esthétique, avec superposition d'un boxplot
  p <- ggplot(data_long, aes(x = period, y = FSCA, fill = factor(year), weight = load)) +
    geom_violin(
      position = position_dodge(width = 0.8),
      scale = "width",      # normalise la largeur entre groupes
      trim = TRUE,          # tronque aux min/max observés
      alpha = 0.6,          # transparence pour mieux voir le chevauchement
      color = "black"       # contour noir pour chaque violon
    ) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.1,
      outlier.shape = NA,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    labs(
      title = alpage,  # Le titre est le nom de l'alpage
      x = "Période (Quinzaine)",
      y = "FSCA",
      fill = "Année"
    ) +
    # Palette personnalisée
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    # Inverser l'axe des y pour que la valeur 1 apparaisse en haut
    scale_y_reverse() +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}

### 2) Fonction pour empiler plusieurs alpages (en colonne) et sauvegarder au format A4 portrait
plot_multiple_alpages_violin <- function(alpages,
                                         data_dir,
                                         output_dir,
                                         years_to_use = c(2022, 2023, 2024),
                                         file_prefix  = "Plot_fsca_alti_violin",
                                         dpi          = 300) {
  plots <- list()
  
  for (i in seq_along(alpages)) {
    alpage <- alpages[i]
    data_rds <- file.path(data_dir, paste0("Use_Fsca_Alti_", alpage, "_by_raster.rds"))
    
    # Générer le plot pour cet alpage
    p_alpage <- plot_fsca_alti_violin_one(alpage, data_rds, years_to_use)
    
    # Masquer la légende pour tous sauf le dernier pour obtenir une légende commune
    if (i < length(alpages)) {
      p_alpage <- p_alpage + theme(legend.position = "none")
    }
    plots[[alpage]] <- p_alpage
  }
  
  # Combiner les graphiques en une seule colonne et collecter la légende commune
  combined_plot <- wrap_plots(plots, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Titre global indiquant les alpages et années traitées
  combined_plot <- combined_plot + plot_annotation(
    title = paste0(
      "Alpages: ", paste(alpages, collapse = ", "),
      " | Années: ", paste(years_to_use, collapse = ", ")
    ),
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )
  
  # Nom du fichier de sortie
  plot_filename <- paste0(
    file_prefix, "_",
    paste(alpages, collapse = "_"),
    "_Years_", paste(years_to_use, collapse = "_"),
    ".jpg"
  )
  output_file <- file.path(output_dir, plot_filename)
  
  # Sauvegarder en format JPG (A4 portrait)
  ggsave(
    filename = output_file,
    plot     = combined_plot,
    device   = "jpg",
    width    = 8.27,
    height   = 11.69,
    units    = "in",
    dpi      = dpi
  )
  
  message("Plot saved to: ", output_file)
  return(combined_plot)
}






### 1) Fonction pour tracer le violin plot pour un seul alpage en utilisant "altitude"
plot_fsca_alti_violin_one <- function(alpage,
                                      data_rds,
                                      years_to_use = c(2022, 2023, 2024)) {
  # Charger les données et supprimer la colonne 'period' si elle existe
  data <- readRDS(data_rds)
  data_mod <- data %>% select(-any_of("period"))
  
  # Transformation en format long : rassembler les colonnes "presence" et "load"
  data_long <- data_mod %>% 
    pivot_longer(
      cols = matches("^(presence|load)_"),
      names_to = c("var", "year", "period"),
      names_pattern = "(presence|load)_(\\d{4})_(.*)",
      values_to = "value"
    ) %>%
    # Reconstituer une table avec deux colonnes (presence et load)
    pivot_wider(
      names_from = var,
      values_from = value
    ) %>%
    # Filtrer pour ne garder que les pixels utilisés, dont le load est <= 700 et appartenant aux années souhaitées
    filter(presence == 1,
           load <= 700,
           as.integer(year) %in% years_to_use) %>%
    # Exclure explicitement les données correspondant à "apres16_sep"
    filter(period != "apres16_sep") %>%
    # Recoder la variable 'period' pour obtenir des libellés explicites
    mutate(period = recode(period,
                           "16_30_jun" = "16-30 Juin",
                           "1_15_jul"  = "1-15 Juillet",
                           "16_30_jul" = "16-30 Juillet",
                           "1_15_aou"  = "1-15 Août",
                           "16_30_aou" = "16-30 Août",
                           "1_15_sep"  = "1-15 Septembre"))
  
  # Forcer l'ordre souhaité pour les périodes sur l'axe x
  period_levels <- c("16-30 Juin", "1-15 Juillet", "16-30 Juillet", 
                     "1-15 Août", "16-30 Août", "1-15 Septembre")
  data_long$period <- factor(data_long$period, levels = period_levels)
  
  # Création du violin plot avec superposition d'un boxplot
  # Modification : utilisation de "altitude" au lieu de "FSCA" et suppression de scale_y_reverse()
  p <- ggplot(data_long, aes(x = period, y = altitude, fill = factor(year), weight = load)) +
    geom_violin(
      position = position_dodge(width = 0.8),
      scale = "width",      # Normalise la largeur entre groupes
      trim = TRUE,          # Tronque aux min/max observés
      alpha = 0.6,          # Transparence pour mieux voir le chevauchement
      color = "black"       # Contour noir pour chaque violon
    ) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.1,
      outlier.shape = NA,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    labs(
      title = alpage,      # Le titre est le nom de l'alpage
      x = "Période (Quinzaine)",
      y = "Altitude",      # Label de l'axe y modifié
      fill = "Année"
    ) +
    # Palette personnalisée
    scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}

### 2) Fonction pour empiler plusieurs alpages (en colonne) et sauvegarder au format A4 portrait
plot_multiple_alpages_violin_alti <- function(alpages,
                                         data_dir,
                                         output_dir,
                                         years_to_use = c(2022, 2023, 2024),
                                         file_prefix  = "Plot_alti_violin",
                                         dpi          = 300) {
  plots <- list()
  
  for (i in seq_along(alpages)) {
    alpage <- alpages[i]
    # On conserve le même format de nommage pour le fichier de données
    data_rds <- file.path(data_dir, paste0("Use_Fsca_Alti_", alpage, "_by_raster.rds"))
    
    # Générer le plot pour cet alpage en utilisant la fonction modifiée
    p_alpage <- plot_fsca_alti_violin_one(alpage, data_rds, years_to_use)
    
    # Masquer la légende pour tous sauf le dernier afin d'obtenir une légende commune
    if (i < length(alpages)) {
      p_alpage <- p_alpage + theme(legend.position = "none")
    }
    plots[[alpage]] <- p_alpage
  }
  
  # Combiner les graphiques en une seule colonne et collecter une légende commune
  combined_plot <- wrap_plots(plots, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Titre global indiquant les alpages et les années traitées
  combined_plot <- combined_plot + plot_annotation(
    title = paste0(
      "Alpages: ", paste(alpages, collapse = ", "),
      " | Années: ", paste(years_to_use, collapse = ", ")
    ),
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  )
  
  # Nom du fichier de sortie
  plot_filename <- paste0(
    file_prefix, "_",
    paste(alpages, collapse = "_"),
    "_Years_", paste(years_to_use, collapse = "_"),
    ".jpg"
  )
  output_file <- file.path(output_dir, plot_filename)
  
  # Sauvegarder en format JPG (A4 portrait)
  ggsave(
    filename = output_file,
    plot     = combined_plot,
    device   = "jpg",
    width    = 8.27,
    height   = 11.69,
    units    = "in",
    dpi      = dpi
  )
  
  message("Plot saved to: ", output_file)
  return(combined_plot)
}


