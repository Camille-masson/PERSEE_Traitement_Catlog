 
### FLOCK LOAD COMPUTING ###
#--------------------------#

# OLD: compute UD using kernel around points (bivariate normal or Epanechnikov kernel)
flock_load_from_daily_and_state_UD <- function(UD, n_points_state, n_points_total, flock_size, prop_time_collar_on) {
    # INPUTS
    #   UD : the kernel computed by adehabitatHR for this day and this state
    #   n_points_state : number of relocation attributed to this state this day
    #   n_points_total : total number of relocations this day
    #   flock_size : number of animals in the flock
    # OUTPUT
    #   a data.frame with Charge and x and y coordinates

    UD <- as.data.frame.estUD(UD)
    colnames(UD) <- c("Charge", "x", "y")
    UD$Charge <- (UD$Charge
        / sum(UD$Charge)
        * n_points_state
        / n_points_total
        * flock_size
        * prop_time_collar_on
        * 10000 / abs(min(diff(unique(UD$x)))) / abs(min(diff(unique(UD$y))))) # To get a result in animals/ha for each pixel
    return(UD)
}

flock_load_by_day_and_state_to_rds_kernelUD <- function(data, grid, save_dir, save_rds_name, flock_size, prop_time_collar_on) {
    # Compute flock load by day and state
    # INPUTS
    #    data : a dataframe of animal relocations with x, y and time columns
    #    grid : a spatialPixelDataFrame object, the load will be computed on the same grid
    #    save_dir : path to the directory the results will be saved to.
    #    save_rds_name : name of the rds file the results will be saved to.
    #    flock_size : the number of animals in the flock
    # OUPUTS
    #    a rds by state file containing a data.frame with "x", "y", "day" and "Charge" columns 

    data$day <- yday(data$time)
    data <- SpatialPointsDataFrame(data[, c("x", "y")], data)
    proj4string(data) <- CRS_L93

    
    for (state in unique(data$state)) {
        save_rds_prefix = paste0(save_dir, save_rds_name,"_",state,"_")
        if (file.exists(paste0(save_dir,save_rds_name))) {
            file.remove(paste0(save_dir,save_rds_name)) #Delete file if it exists
        }

        days = unique(data$day)

        removed_days =  numeric(0) # kernelUD needs at least 5 relocations, days with less relocations are removed
        for (d in days) {
            if(nrow(data[data$state == state & data$day == d,]) < 5) {
                data = data[!(data$state == state & data$day == d),]
                removed_days = append(removed_days, d)
            }
        }

        hr <- kernelUD(data[data$state == state, "day"], grid = grid, h = h, kern = "epa") # kern :a character string. If "bivnorm", a bivariate normal kernel is used. If "epa", an Epanechnikov kernel is used.
        charge_jour <- flock_load_from_daily_and_state_UD(hr[[as.character(days[!(days %in% removed_days)][1])]], 1, 1, flock_size, prop_time_collar_on)
        ordre = order(charge_jour$x, charge_jour$y) # To get the right x and y orders

        for (d in days) {
            print(paste0("Day ",d,"/",max(unique(data$day))," state ",state))
            if (d %in% removed_days) { # If the day was removed, we consider the load is equal to 0 everywhere
                charge_jour <- pheno_t0 %>%
                                as.data.frame(xy = T) %>%
                                dplyr::select("x", "y") %>%
                                mutate(Charge = 0)
                charge_jour <- charge_jour[order(charge_jour$x, charge_jour$y), ]
            } else {
                charge_jour <- flock_load_from_daily_and_state_UD(hr[[as.character(d)]], 
                                    n_points_state = sum(data$day == d & data$state == state),
                                    n_points_total = sum(data$day == d), flock_size, prop_time_collar_on)
                charge_jour <- charge_jour[ordre, ]
            }
            charge_jour$day <- d
            charge_jour$state <- state
            saveRDS(charge_jour, file = paste0(save_rds_prefix,d,".rds"))
        }
        rm(charge_jour)
        rm(hr)
    }

    # Merge the .rds files in one
    files = list.files(save_dir, pattern=paste0("^",save_rds_name), full.names=T)
    charge = as.data.frame(rbindlist(lapply(files, readRDS), use.names=TRUE))
    saveRDS(charge, paste0(save_dir,save_rds_name))
    lapply(files, file.remove)
}


# NEW: compute UD using a Brownian Bridge kernel around trajectories

flock_load_from_daily_and_state_UD_kernelbb <- function(UD, n_points_state, n_points_total, flock_size, prop_time_collar_on) {
    # INPUTS
    #   UD : the kernel computed by adehabitatHR for this day and this state
    #   n_points_state : number of relocation attributed to this state this day
    #   n_points_total : total number of relocations this day
    #   flock_size : number of animals in the flock
    # OUTPUT
    #   a data.frame with Charge and x and y coordinates

    if (is.list(UD)) {
        UD <- lapply(UD, as.data.frame.estUD) %>%
                     do.call(rbind, .)
    }
    else {
        UD <- as.data.frame.estUD(UD)
    }

    UD %>% rename(Charge=ud, x=Var2, y=Var1) %>%
            group_by(x,y) %>%
            summarise(Charge=sum(Charge, na.rm = T), .groups="drop") %>% # it is necessary to remove NA, because if there is only one point in the burst, the corresponding UD is NaN everywhere
            mutate(Charge = (Charge
                            / sum(Charge)
                            * n_points_state
                            / n_points_total
                            * flock_size
                            * prop_time_collar_on
                            * 10000 / abs(min(diff(unique(x)))) / abs(min(diff(unique(y)))))) %>% # To get a result in animals/ha for each pixel
            return()
}

#FONCTION ORIGINAL
flock_load_by_day_and_state_to_rds_kernelbb <- function(data, grid, save_dir, save_rds_name, flock_sizes, prop_time_collar_on) {
    # Compute flock load by day and state
    # INPUTS
    #    data : a dataframe of animal relocations with x, y and time columns
    #    grid : a spatialPixelDataFrame object, the load will be computed on the same grid
    #    save_dir : path to the directory the results will be saved to.
    #    save_rds_name : name of the rds file the results will be saved to.
    #    flock_sizes : a 365-long vector containing the number of animals in the flock for each year_day (values for days without GPS relocations are not used)
    # OUPUTS
    #    a rds by state file containing a data.frame with "x", "y", "day" and "Charge" columns 

    data$day <- yday(data$time)
    # data <- SpatialPointsDataFrame(data[, c("x", "y")], data)
    # proj4string(data) <- CRS_L93
    if (file.exists(paste0(save_dir,save_rds_name))) {
        file.remove(paste0(save_dir,save_rds_name)) #Delete file if it exists
    }
    Tmax = 42 #in minutes for split_at_gap
    hmin = 15
    # Ds <- list(Repos = 0.7, #for BRB
    #            Paturage = 3,
    #            Deplacement = 8)
    Ds <- list(Repos = 1.25,
                Paturage = 3,
                Deplacement = 4.5)
    
    for (state in unique(data$state)) {
        save_rds_prefix = paste0(save_dir, save_rds_name,"_",state,"_")

        days = unique(data$day)

        data_state = data %>%
                filter(state == !!state)
        ltr = as.ltraj(xy = data_state[c("x", "y")], date = as.POSIXct(data_state$time), id = data_state$ID)

        # print(paste("DIFFUSION COEFFICIENTS COMPUTED FOR STATE",state))
        # print(liker(ltr, rangesig1 = c(0,10), sig2 = 10))
        # print(BRB.D(ltr, Tmax = Tmax, Lmin = 0, habitat = NULL, activity = NULL))

        # Prepare cluster for parallelised computation of daily utilisation distribution
        clus <- makeCluster(ncores, outfile='') # outfile='' is verbose option
        clusterExport(clus, list("days", "state", "save_rds_prefix", "data", "data_state", "pheno_t0", "flock_sizes", "prop_time_collar_on"), envir = environment())
        clusterExport(clus, as.list(lsf.str(.GlobalEnv))) # export all manually loaded functions
        clusterCall(clus, function() {
            # For the pipes
            suppressPackageStartupMessages(library(tidyverse)) # includes ggplot2 and dplyr among others
            suppressPackageStartupMessages(library(adehabitatHR))
            options(warn=0)
        })

        parLapply(clus, days,
                    function(d) {
                        print(paste0("Day ",d,"/",max(days)," state ",state))

                        ltr = data_state %>%
                            filter(day == d) %>%
                            mutate(time = as.POSIXct(time))

                        if( nrow(ltr) > 0 ) {
                            ltr = split_at_gap(ltr, max_gap = Tmax) #split the trajectory of each individual into bursts of the specified state (otherwise, every bursts are bridged together when using kernelbb)
                            ltr = as.ltraj(xy = ltr[c("x", "y")], date = ltr$time, id = ltr$ID)

                            hr <- kernelbb(ltr, sig1 = Ds[[state]], sig2=10, grid = grid, same4all = FALSE, byburst = TRUE,
                                    extent = 0.5, nalpha = 25)

                            # hr <- BRB(ltr, Ds[[state]], Tmax = Tmax, Lmin = 1, hmin = hmin, type="UD",
                            # filtershort=TRUE, grid = 20, b=FALSE, same4all=TRUE, extent=1, tau = NULL,
                            # boundary=NULL)

                            charge_jour <- flock_load_from_daily_and_state_UD_kernelbb(hr,
                                                n_points_state = sum(data$day == d & data$state == state),
                                                n_points_total = sum(data$day == d), flock_sizes[d], prop_time_collar_on)

                            charge_jour$day <- d
                            charge_jour$state <- state
                            saveRDS(charge_jour, file = paste0(save_rds_prefix,d,".rds"))
                        }
                } )
        stopCluster(clus)
    }

    # Merge the .rds files in one
    files = list.files(save_dir, pattern=paste0("^",save_rds_name), full.names=T)
    charge = as.data.frame(rbindlist(lapply(files, readRDS), use.names=TRUE))
    saveRDS(charge, paste0(save_dir,save_rds_name))
    lapply(files, file.remove)
}


split_at_gap <- function(data, max_gap = 60, shortest_track = 0) {
    #' Courtesy Théo Michelot
    #' Split track at gaps
    #'
    #' @param data Data frame with (at least) columns for "ID" and "time"
    #' @param max_gap Longest allowed gap, in minutes (track will be split at longer gaps)
    #' @param shortest_track Shortest track to keep after splitting, in minutes. Shorter
    #' tracks will be removed from the output data set.
    #'
    #' @return Data frame with identical structure as input, where ID column
    #' has been replaced by new ID for split tracks. Old ID still accessible as
    #' ID_old column

    # Number of tracks
    n_tracks <- length(unique(data$ID))

    # Save old ID and reinitialise ID column
    data$ID_old <- data$ID
    data$ID <- character(nrow(data))

    # Loop over tracks (i.e., over IDs)
    for(i_track in 1:n_tracks) {
        # Indices for this track
        ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
        track_length <- length(ind_this_track)

        # Time intervals in min
        dtimes <- difftime(data$time[ind_this_track[-1]],
                           data$time[ind_this_track[-track_length]],
                           units = "mins")

        # Indices of gaps longer than max_gap
        ind_gap <- c(0, which(dtimes > max_gap), track_length)

        # Create new ID based on split track
        subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap)) #la diff permet d’avoir la longueur de chaque subtrack, puis le rep va permettre de répéter l’indice ordinal de cette subtrack autant de fois que cette longueur
        data$ID[ind_this_track] <- paste0(data$ID_old[ind_this_track], "-", subtrack_ID)
    }

    # Only keep sub-tracks longer than some duration
    track_lengths <- sapply(unique(data$ID), function(id) {
        ind <- which(data$ID == id)
        difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
    })
    ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
    data <- subset(data, ID %in% ID_keep)

    return(data)
}


get_flock_size_through_time <- function(alpage, flock_size_file) {
    # INPUTS
    #   alpage: the name of the pasture of interest
    #   flock_size_file: path to a CSV file containing the evolutions of flock sizes (one line is the new size of the flock from the specified date)
    #                    Must contain les colonnes  "alpage", "date_debut_periode", "taille_totale_troupeau"
    # OUTPUT
    #   a 365-long vector containing the number of animals in the flock for each year_day (values for days without GPS relocations are not used)
    dates_sizes <- read.csv(flock_size_file, header=TRUE, sep=",") %>%
        filter(alpage == !!alpage) %>%
        mutate(yday = yday(as.POSIXct(date_debut_periode, tz="GMT", format="%d/%m/%Y"))) %>%
        arrange(yday)

    sizes = rep(0, 1, 365)
    for (i in 1:nrow(dates_sizes)) {
        sizes[dates_sizes$yday[i]:365] = dates_sizes$taille_totale_troupeau[i]
    }
    
    return(sizes)
}


recompute_daily_flock_load_by_state <- function(charge_d, flock_size_d, prop_time_collar_on) {
    charge_tot_init = sum(charge_d$Charge)
    charge_tot_fin = flock_size_d * 
                        prop_time_collar_on *
                        10000 / abs(min(diff(unique(charge_d$x)))) / abs(min(diff(unique(charge_d$y)))) # To get a result in animals/ha for each pixel
    n_states = length(unique(charge_d$state))

    for(s in unique(charge$state)) {
        I_s = (charge_d$state == s)
        charge_d$Charge[I_s] = charge_d$Charge[I_s] / charge_tot_init * charge_tot_fin
    }
    return(charge_d)
}



#FONCTION V1 avec les memes fonctionnalisté que l'original basé sur NDVI grid
flock_load_by_day_and_state_to_rds_kernelbb_NDVI_grid <- function(data, grid, save_dir, save_rds_name, flock_sizes, prop_time_collar_on) {
  library(lubridate)
  library(tidyverse)
  library(adehabitatHR)
  
  data$day <- yday(data$time)
  
  # Vérification du dossier
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  Tmax = 42  # Max gap en minutes
  hmin = 15
  Ds <- list(Repos = 1.25, Paturage = 3, Deplacement = 4.5)
  
  all_files <- c()  # Liste des fichiers RDS générés
  
  for (state in unique(data$state)) {
    cat("Traitement de l'état :", state, "\n")
    
    save_rds_prefix <- file.path(save_dir, paste0(save_rds_name, "_", state, "_"))
    days <- unique(data$day)
    data_state <- data %>% filter(state == !!state)
    
    for (d in days) {
      cat("Jour", d, "sur", max(days), "état", state, "\n")
      
      ltr <- data_state %>% filter(day == d) %>% mutate(time = as.POSIXct(time))
      
      if (nrow(ltr) > 0) {
        ltr <- split_at_gap(ltr, max_gap = Tmax) 
        ltr <- as.ltraj(xy = ltr[c("x", "y")], date = ltr$time, id = ltr$ID)
        
        hr <- kernelbb(ltr, sig1 = Ds[[state]], sig2 = 10, grid = grid, same4all = FALSE, byburst = TRUE, extent = 0.5, nalpha = 25)
        
        charge_jour <- flock_load_from_daily_and_state_UD_kernelbb(hr,
                                                                   n_points_state = sum(data$day == d & data$state == state),
                                                                   n_points_total = sum(data$day == d), flock_sizes[d], prop_time_collar_on)
        
        charge_jour$day <- d
        charge_jour$state <- state
        
        save_file <- file.path(save_dir, paste0(save_rds_name, "_", state, "_", d, ".rds"))
        saveRDS(charge_jour, file = save_file)
        
        all_files <- c(all_files, save_file)
      }
    }
  }
  
  cat("Fichiers individuels générés !!!\n")
}



flock_merge_rds_files <- function(save_dir, state_daily_rds_prefix) {
  # Lister tous les fichiers RDS générés
  all_files <- list.files(save_dir, pattern = paste0("^", state_daily_rds_prefix), full.names = TRUE)
  
  # Vérifier s'il y a des fichiers à fusionner
  if (length(all_files) == 0) {
    return(NULL)
  }
  
  # Charger tous les fichiers RDS
  rds_list <- lapply(all_files, function(file) {
    data <- readRDS(file)
    
    # Vérification : Si la colonne state est manquante ou mal définie, on la rajoute
    if (!"state" %in% colnames(data)) {
      state_detected <- stringr::str_extract(basename(file), "_(Repos|Paturage|Deplacement)_")
      state_detected <- gsub("_", "", state_detected)  # Nettoyage du nom
      data$state <- state_detected
    }
    
    return(data)
  })
  
  # Fusionner tous les fichiers en une seule data.frame
  charge_final <- data.table::rbindlist(rds_list, use.names = TRUE, fill = TRUE)
  
  # Sauvegarde du fichier final
  output_file <- file.path(save_dir, paste0(state_daily_rds_prefix, alpage, ".rds"))
  saveRDS(charge_final, output_file)
  
  # Suppression des fichiers intermédiaires
  file.remove(all_files)
  
  return(output_file)
}














#FONCTION V2 base sur une grille automatique

flock_load_by_day_and_state_to_rds_kernelbb_Auto_grid <- function(data, save_dir, save_rds_name, flock_sizes, prop_time_collar_on) {
  library(lubridate)
  library(tidyverse)
  library(adehabitatHR)
  library(raster)
  
  # Si 'grid' n'est pas déjà un SpatialPixelsDataFrame, on crée une grille de 10m x 10m.
  if (!inherits(grid, "SpatialPixelsDataFrame")) {
    buffer <- 100  # marge en mètres autour des points
    xmin <- min(data$x) - buffer
    xmax <- max(data$x) + buffer
    ymin <- min(data$y) - buffer
    ymax <- max(data$y) + buffer
    ext_data <- extent(xmin, xmax, ymin, ymax)
    r <- raster(ext_data, res = 10, crs = "+init=epsg:2154")  # Lambert-93
    grid <- as(r, "SpatialPixelsDataFrame")
  }
  
  # Ajout du jour dans les données (à partir de la colonne 'time')
  data$day <- yday(data$time)
  
  # Vérification et création du dossier de sauvegarde
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  
  Tmax <- 42  # Max gap en minutes
  Ds <- list(Repos = 1.25, Paturage = 3, Deplacement = 4.5)
  
  all_files <- c()  # Liste des fichiers RDS générés
  
  for (state in unique(data$state)) {
    cat("Traitement de l'état :", state, "\n")
    
    save_rds_prefix <- file.path(save_dir, paste0(save_rds_name, "_", state, "_"))
    days <- unique(data$day)
    data_state <- data %>% filter(state == !!state)
    
    for (d in days) {
      cat("Jour", d, "sur", max(days), "état", state, "\n")
      
      ltr <- data_state %>% filter(day == d) %>% mutate(time = as.POSIXct(time))
      
      if (nrow(ltr) > 0) {
        ltr <- split_at_gap(ltr, max_gap = Tmax) 
        ltr <- as.ltraj(xy = ltr[c("x", "y")], date = ltr$time, id = ltr$ID)
        
        hr <- kernelbb(ltr, sig1 = Ds[[state]], sig2 = 10, 
                       grid = grid, same4all = FALSE, byburst = TRUE, 
                       extent = 0.5, nalpha = 25)
        
        charge_jour <- flock_load_from_daily_and_state_UD_kernelbb(hr,
                                                                   n_points_state = sum(data$day == d & data$state == state),
                                                                   n_points_total = sum(data$day == d), 
                                                                   flock_sizes[d], prop_time_collar_on)
        
        charge_jour$day <- d
        charge_jour$state <- state
        
        save_file <- file.path(save_dir, paste0(save_rds_name, "_", state, "_", d, ".rds"))
        saveRDS(charge_jour, file = save_file)
        
        all_files <- c(all_files, save_file)
      }
    }
  }
  
  cat("Fichiers individuels générés !!!\n")
  return(all_files)
}
























flock_load_by_parc_and_state_to_rds_kernelbb_Auto_grid <- function(data,
                                                                   save_dir,
                                                                   save_rds_prefix,
                                                                   flock_size,
                                                                   prop_time_collar_on) {
  # → data : soit un data.frame, soit un chemin (character) vers un .RDS
  # → flock_size : un unique nombre (taille du troupeau à répartir)
  # NB : on suppose que vous avez déjà ajouté la colonne `parc` avec la méthode DBSCAN
  
  library(lubridate)
  library(dplyr)
  library(adehabitatHR)
  library(raster)
  
  # 0. si data est un chemin, on lit ; sinon on le laisse tel quel
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    data <- readRDS(data)
  } else if (!is.data.frame(data)) {
    stop("`data` doit être soit un chemin vers un .RDS soit un data.frame.")
  }
  
  # 1. check de la colonne parc
  if (!"parc" %in% colnames(data)) {
    stop("La colonne 'parc' est introuvable dans vos données.")
  }
  
  # 2. création de la grille 10 m × 10 m (Lambert‑93)
  if (!inherits(grid, "SpatialPixelsDataFrame")) {
    buffer <- 100
    x0 <- min(data$x) - buffer; x1 <- max(data$x) + buffer
    y0 <- min(data$y) - buffer; y1 <- max(data$y) + buffer
    ext <- extent(x0, x1, y0, y1)
    r   <- raster(ext, res = 10, crs = "+init=epsg:2154")
    grid <- as(r, "SpatialPixelsDataFrame")
  }
  
  # 3. paramètres HMM → UD
  Tmax <- 42
  Ds   <- list(Repos = 1.25, Paturage = 3, Deplacement = 4.5)
  
  all_files <- character(0)
  
  # 4. boucle sur états et parcs
  for (state in unique(data$state)) {
    message("▶ État = ", state)
    for (parc in unique(data$parc)) {
      message("   • Parc = ", parc)
      
      sub <- data %>%
        filter(state == state, parc == parc) %>%
        mutate(time = as.POSIXct(time))
      if (nrow(sub) == 0) next
      
      # 4.1 découpe en bursts
      ltr <- split_at_gap(sub, max_gap = Tmax)
      ltr <- as.ltraj(xy = sub[c("x","y")],
                      date = sub$time,
                      id   = sub$ID)
      
      # 4.2 UD kernelBB
      hr <- kernelbb(ltr,
                     sig1     = Ds[[state]],
                     sig2     = 10,
                     grid     = grid,
                     same4all = FALSE,
                     byburst  = TRUE,
                     extent   = 0.5,
                     nalpha   = 25)
      
      # 4.3 chargement
      n_state <- nrow(sub)
      n_total <- nrow(data %>% filter(parc == parc))
      charge_parc <- flock_load_from_daily_and_state_UD_kernelbb(
        UD                 = hr,
        n_points_state     = n_state,
        n_points_total     = n_total,
        flock_size         = flock_size,
        prop_time_collar_on = prop_time_collar_on
      )
      
      charge_parc$parc  <- parc
      charge_parc$state <- state
      
      # 4.4 sauvegarde
      save_file <- file.path(
        save_dir,
        paste0(save_rds_prefix, state, "_", parc, ".rds")
      )
      saveRDS(charge_parc, save_file)
      all_files <- c(all_files, save_file)
    }
  }
  
  message("✅ Généré ", length(all_files), " fichiers .rds")
  return(all_files)
}





































compute_charge_by_park <- function(
    input_load_day_state_rds,
    input_parc_rds_file,
    output_park_day_state_rds,
    output_park_state_rds,
    output_park_rds
) {
  # 0) librairies
  library(dplyr)
  library(lubridate)
  
  # 1) chargement des données
  charge_day_state <- readRDS(input_load_day_state_rds)
  viterbi_parc     <- readRDS(input_parc_rds_file)
  
  # 2) calcul du jour julien (DOY) dans viterbi_parc
  viterbi_parc <- viterbi_parc %>%
    mutate(
      date = as.Date(date),
      day  = yday(date)
    )
  
  # 3) parc « majoritaire » par jour
  day_park <- viterbi_parc %>%
    count(day, parc) %>%
    group_by(day) %>%
    slice_max(n, with_ties = FALSE) %>%
    dplyr::select(day, parc)
  
  # 4) ajout de la colonne parc à chaque ligne de charge_day_state
  charge_with_park <- charge_day_state %>%
    left_join(day_park, by = "day")
  
  # 5a) agrégation PAR PIXEL + JOUR + ÉTAT + PARC
  charge_park_day_state <- charge_with_park %>%
    group_by(x, y, day, state, parc) %>%
    summarise(
      Charge = sum(Charge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 5b) agrégation PAR PARC + ÉTAT
  charge_park_state <- charge_park_day_state %>%
    group_by(x, y, parc, state) %>%
    summarise(
      Charge = sum(Charge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 5c) agrégation PAR PARC toutes nuits et états confondus
  charge_park <- charge_park_state %>%
    group_by(x, y, parc) %>%
    summarise(
      Charge = sum(Charge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 6) sauvegardes
  saveRDS(charge_park_day_state, output_park_day_state_rds)
  saveRDS(charge_park_state,     output_park_state_rds)
  saveRDS(charge_park,           output_park_rds)
  
  # 7) message de fin
  message("Agrégations enregistrées :\n",
          "   - jour/état/park : ", output_park_day_state_rds, "\n",
          "   - état/park      : ", output_park_state_rds, "\n",
          "   - park           : ", output_park_rds)
  
  # 8) retour invisible
  invisible(list(
    park_day_state = charge_park_day_state,
    park_state     = charge_park_state,
    park           = charge_park
  ))
}







































compute_charge_by_park_no_transition <- function(
    input_load_day_state_rds,
    input_parc_rds_file,
    output_park_day_state_rds,
    output_park_state_rds,
    output_park_rds
) {
  # librairies
  library(dplyr)
  library(lubridate)
  
  # 1) chargement des données
  charge_day_state <- readRDS(input_load_day_state_rds)
  viterbi_parc     <- readRDS(input_parc_rds_file) %>%
    mutate(
      date = as.Date(date),
      day  = yday(date)
    )
  
  # 2) on identifie les jours de transition (DOY)
  transition_days <- viterbi_parc %>%
    filter(jour_de_transition) %>%
    distinct(day) %>%
    pull(day)
  
  # 3) parc « majoritaire » par jour
  day_park <- viterbi_parc %>%
    count(day, parc) %>%
    group_by(day) %>%
    slice_max(n, with_ties = FALSE) %>%
    dplyr::select(day, parc)
  
  # 4) on joint et on exclut les jours de transition
  charge_filtered <- charge_day_state %>%
    left_join(day_park, by = "day") %>%
    filter(! day %in% transition_days)
  
  # 5a) agrégation PAR PIXEL + JOUR + ÉTAT + PARC
  charge_park_day_state <- charge_filtered %>%
    group_by(x, y, day, state, parc) %>%
    summarise(
      Charge = sum(Charge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 5b) agrégation PAR PARC + ÉTAT
  charge_park_state <- charge_park_day_state %>%
    group_by(x, y, parc, state) %>%
    summarise(
      Charge = sum(Charge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 5c) agrégation PAR PARC (tous états et tous jours non-transition)
  charge_park <- charge_park_state %>%
    group_by(x, y, parc) %>%
    summarise(
      Charge = sum(Charge, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 6) sauvegardes
  saveRDS(charge_park_day_state, output_park_day_state_rds)
  saveRDS(charge_park_state,     output_park_state_rds)
  saveRDS(charge_park,           output_park_rds)
  
  # 7) message de fin
  message("Agrégations sans jours de transition enregistrées :\n",
          "   - jour/état/park : ", output_park_day_state_rds, "\n",
          "   - état/park      : ", output_park_state_rds, "\n",
          "   - park           : ", output_park_rds)
  
  invisible(list(
    park_day_state = charge_park_day_state,
    park_state     = charge_park_state,
    park           = charge_park
  ))
}


















compute_charge_by_park_no_transition_optimized <- function(
    input_load_day_state_rds,
    input_parc_rds_file,
    output_park_day_state_rds,
    output_park_state_rds,
    output_park_rds
) {
  # Chargement des librairies
  library(data.table)
  library(lubridate)
  
  # 1) Chargement et préparation de viterbi_parc
  viterbi_parc <- readRDS(input_parc_rds_file)
  setDT(viterbi_parc)
  viterbi_parc[, date := as.Date(date)]
  viterbi_parc[, day  := yday(date)]
  
  # Identification des jours de transition
  transition_days <- unique(viterbi_parc[jour_de_transition == TRUE, day])
  
  # Détermination du parc majoritaire par jour
  day_park <- viterbi_parc[, .N, by = .(day, parc)]
  setorder(day_park, day, -N)
  day_park <- day_park[, .SD[1], by = day]
  day_park <- day_park[, .(day, parc)]
  
  # Libération de mémoire
  rm(viterbi_parc)
  gc()
  
  # 2) Chargement et filtrage de charge_day_state
  charge_dt <- readRDS(input_load_day_state_rds)
  setDT(charge_dt)
  charge_dt <- charge_dt[!day %in% transition_days]
  
  # Jointure avec le parc majoritaire (fusion simple pour éviter les problèmes de promesse)
  charge_dt <- merge(charge_dt, day_park, by = "day", all = FALSE)
  
  # 3a) Agrégation PAR PIXEL + JOUR + ÉTAT + PARC
  charge_park_day_state <- charge_dt[, .(
    Charge = sum(Charge, na.rm = TRUE)
  ), by = .(x, y, day, state, parc)]
  saveRDS(charge_park_day_state, output_park_day_state_rds)
  
  # Nettoyage intermédiaire
  rm(charge_dt)
  gc()
  
  # 3b) Agrégation PAR PARC + ÉTAT
  charge_park_state <- charge_park_day_state[, .(
    Charge = sum(Charge, na.rm = TRUE)
  ), by = .(x, y, parc, state)]
  saveRDS(charge_park_state, output_park_state_rds)
  
  # Libération mémoire
  rm(charge_park_day_state)
  gc()
  
  # 3c) Agrégation PAR PARC (tous états et tous jours non-transition)
  charge_park <- charge_park_state[, .(
    Charge = sum(Charge, na.rm = TRUE)
  ), by = .(x, y, parc)]
  saveRDS(charge_park, output_park_rds)
  
  # Nettoyage final
  rm(charge_park_state)
  gc()
  
  # Message de fin
  message("Agrégations sans jours de transition enregistrées :\n",
          "   - jour/état/park : ", output_park_day_state_rds, "\n",
          "   - état/park      : ", output_park_state_rds, "\n",
          "   - park           : ", output_park_rds)
  
  invisible(list(
    park_day_state = output_park_day_state_rds,
    park_state     = output_park_state_rds,
    park           = output_park_rds
  ))
}










compute_charge_by_park_no_transition_chunked <- function(
    input_load_day_state_rds,
    input_parc_rds_file,
    output_park_day_state_rds,
    output_park_state_rds,
    output_park_rds,
    chunk_size = 20
) {
  library(data.table)
  library(lubridate)
  
  # 1) viterbi + jours de transition + parc majoritaire
  viterbi_parc <- readRDS(input_parc_rds_file)
  setDT(viterbi_parc)
  viterbi_parc[, `:=`(date = as.Date(date), day = yday(date))]
  transition_days <- unique(viterbi_parc[jour_de_transition == TRUE, day])
  day_park <- viterbi_parc[, .N, by=.(day, parc)][order(day, -N)][, .SD[1], by=day][, .(day, parc)]
  non_transition_days <- setdiff(day_park$day, transition_days)
  rm(viterbi_parc); gc()
  
  # 2) on charge tout en un data.table
  charge_dt <- readRDS(input_load_day_state_rds)
  setDT(charge_dt)
  
  # 3) boucle en chunks de jours
  chunks <- split(non_transition_days, 
                  ceiling(seq_along(non_transition_days)/chunk_size))
  result_list <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    days_chunk <- chunks[[i]]
    dt_chunk  <- charge_dt[day %in% days_chunk]
    kp_chunk  <- day_park[day %in% days_chunk]
    setkey(dt_chunk, day); setkey(kp_chunk, day)
    dt_chunk  <- dt_chunk[kp_chunk, nomatch=0L]
    
    # agg fine
    result_list[[i]] <- dt_chunk[, .(Charge = sum(Charge, na.rm=TRUE)),
                                 by=.(x, y, day, state, parc)]
    rm(dt_chunk); gc()
  }
  
  # 4a) fusion + sauvegarde jour/état/park
  charge_park_day_state <- rbindlist(result_list)
  saveRDS(as.data.frame(charge_park_day_state), output_park_day_state_rds)
  
  # 4b) agg parc/état + sauvegarde
  charge_park_state <- charge_park_day_state[, 
                                             .(Charge = sum(Charge, na.rm=TRUE)), 
                                             by=.(x, y, parc, state)
  ]
  saveRDS(as.data.frame(charge_park_state), output_park_state_rds)
  
  # 4c) agg parc + sauvegarde
  charge_park <- charge_park_state[, 
                                   .(Charge = sum(Charge, na.rm=TRUE)), 
                                   by=.(x, y, parc)
  ]
  saveRDS(as.data.frame(charge_park), output_park_rds)
  
  message("Chunked : sauvegarde terminée sans erreur de format.")
  invisible(list(
    park_day_state = output_park_day_state_rds,
    park_state     = output_park_state_rds,
    park           = output_park_rds
  ))
}
































































#FONCTION ETABLIE LES DATE POUR CHAQUE PARC

use_date_parc <- function(input_parc_rds_file,output_table_use_parc){
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
  
  write_rds(final_table,output_table_use_parc)

  
  
  

  
}




















# RNOMME DANS LES TAUX DE CHARGEMENT AVEC LE BON NOM DE PARC 

rename_parc_in_loads <- function(
    input_table_use_parc,
    input_park_day_state,
    input_park_state,
    input_park,
    output_park_day_state,
    output_park_state,
    output_park
) {
  library(data.table)
  
  # 1) Lire la table d’info sur chaque parc (= une ligne par parc avec sa colonne 'rename')
  info <- readRDS(input_table_use_parc)
  setDT(info)
  if (!all(c("parc", "rename") %in% names(info))) {
    stop("Le RDS info_table_use_parc doit contenir les colonnes 'parc' et 'rename'")
  }
  
  # 2) Construire un vecteur de mapping nommé
  mapping <- setNames(info$rename, info$parc)
  
  # 3) Fonction interne pour renommer et sauvegarder
  process_and_save <- function(in_rds, out_rds) {
    dt <- readRDS(in_rds)
    setDT(dt)
    if (!"parc" %in% names(dt)) {
      stop(sprintf("Le fichier %s n’a pas de colonne 'parc'", in_rds))
    }
    # Appliquer le mapping
    dt[, parc := mapping[as.character(parc)] ]
    # Vérifier que toutes les valeurs ont été renommées
    if (any(is.na(dt$parc))) {
      warning("Certain·e·s parc n’ont pas de correspondance dans info_table_use_parc")
    }
    # Sauvegarder
    saveRDS(dt, out_rds)
    message("-> Fichier renommé et sauvegardé : ", out_rds)
    invisible(NULL)
  }
  
  # 4) Traiter les trois jeux de données
  process_and_save(input_park_day_state, output_park_day_state)
  process_and_save(input_park_state,     output_park_state)
  process_and_save(input_park,           output_park)
  
  invisible(TRUE)
}


