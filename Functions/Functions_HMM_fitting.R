# Load functions and set paths
source("Functions/Functions_map_plot.R")


### FUNCTIONS ###
#---------------#

### HMM data preparation functions
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

regularise_trajectories <- function(data, sampling_period = 120) {
    # sampling_period in seconds
    # Returns a dataframe

    # Use function from utility_function.R to split data at gaps > 20 min, keep only tracks longer than 2h
    data <- split_at_gap(data = data, max_gap = 20, shortest_track = 2*60)

    # Create adehabitat trajectory padded with NAs
    data <- data %>%
      group_by(ID) %>%
      group_modify( function(df, group_id) {
                      data_na <- setNA(ltraj = as.ltraj(xy = df[, c("x", "y")],
                                                        date = df$time,
                                                        id = group_id),
                                       date.ref = df$time[5],
                                       dt = sampling_period, tol = 60, units = "sec")
                      data_na <- ld(data_na)[, c("x", "y", "date")] #convert back to data.frame and select usefull columns only
                      colnames(data_na) <- c("x", "y", "time")
                      return(data_na)
                  }, .keep = FALSE ) %>%
      ungroup() %>%
      as.data.frame()

    return(data)
}

rolling_averaging_trajectories <- function(data, conv) {
    data <- data %>%
      group_by(ID) %>%
      group_modify( function(df, group_id) { # Rolling average
        conv = conv
        df$x = stats::filter(df$x, conv, sides = 2)
        df$y = stats::filter(df$y, conv, sides = 2)
        return(df[, c("x", "y", "time")])
      }, .keep = FALSE ) %>%
      ungroup() %>%
      as.data.frame()
          
    return(data)
}

prepare_hmm_trajectories <- function(data) {
    # Returns a dataframe
    # Prepare data for HMM (compute step lengths and turning angles)

    data <- prepData(data, type = "UTM", covNames = NULL) #'UTM' because easting/northing is provided, not lat/lon
    data[data$step == 0 & !is.na(data$step), "step"] = 0.01 # to avoid issues with the gamma distribution, 0s are eliminated

    hour <- as.POSIXlt(data$time)
    hour <- hour$h + hour$min/60 + hour$s/3600 + 2 # + 2 to convert from UTC to local time
    data$hour <- hour
    rm(hour)

    return(data)
}

resample_trajectories <- function(data_hmm, resampling_ratio, first_index=0) {
    # INPUTS:
    #   data_hmm : GPS data, with ID field identifying individual trajectories, each having a regular sampling period, padded with NAs if necessary
    #   resampling_ratio the ratio of old to new sampling frequency, integer
    #   first_index the index of the first point of the trajectory to be keeped, integer
    # OUTPUT:
    #   data_hmm, with each trajectory resampled at the new sampling frequency

    data_resamp = data.frame()
    for(id in unique(data_hmm$ID)) {
        indices = which(data_hmm$ID==id)
        data_resamp = rbind(data_resamp, data_hmm[indices[seq(first_index, length(indices), resampling_ratio)] ,])
    }

    return(data_resamp)
}


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

par_HMM_fit <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
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
                           sampling_period = get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
                           
                           return(hmm_fit(data[data$ID==ID,], run_parameters, file.path(output_dir, "HMM_comportement", "individual_trajectories"), sampling_period)) } )
    
    stopCluster(clus)
    endTime <- Sys.time()
    print(paste("+++ Cluster total excecution time :", round(difftime(endTime, startTime, units='mins'),2), "min +++"))

    return(results)
}






par_HMM_fit_test <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
  # Paralelized wrapper for hmm_fit
  print("+++ momentuHMM parallel RUN +++")
  startTime <- Sys.time()
  
  clus <- makeCluster(ncores, outfile = '')
  clusterExport(clus, as.list(lsf.str(.GlobalEnv)))
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
  
  # Chargement des librairies et fonctions dans les clusters
  clusterCall(clus, function() {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    theme_set(theme_bw())
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")  # Courtesy Théo Michelot
    BACKGROUND_TYPE <- "BDALTI"
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    # Récupération de l'alpage pour cet individu
    alpage <- get_individual_alpage(ID, individual_info_file)
    print(paste0("ID: ", ID, " - Alpage récupéré : ", alpage))
    
    # Récupérer la période d'échantillonnage propre à l'individu
    sampling_period_ind <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
    
    # Création d'un sous-dossier pour cet alpage, dans output_dir
    pdf_folder <- file.path(output_dir, paste0("Output_PDF_", alpage))
    if (!dir.exists(pdf_folder)) {
      dir.create(pdf_folder, recursive = TRUE)
    }
    
    # Appel de la fonction hmm_fit en utilisant le dossier pdf_folder pour sauvegarder le PDF
    return(hmm_fit(data[data$ID == ID, ], run_parameters, pdf_folder, sampling_period_ind))
  })
  
  stopCluster(clus)
  print(Sys.time() - startTime)
  return(results)
}






par_HMM_fit_test_1 <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
  # Paralelized wrapper for hmm_fit
  print("+++ momentuHMM parallel RUN +++")
  startTime <- Sys.time()
  
  clus <- makeCluster(ncores, outfile = '')
  clusterExport(clus, as.list(lsf.str(.GlobalEnv)))
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
  
  # Chargement des librairies et fonctions dans les clusters
  clusterCall(clus, function() {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    theme_set(theme_bw())
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")  # Courtesy Théo Michelot
    BACKGROUND_TYPE <- "BDALTI"
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    # Récupération de l'alpage pour cet individu
    alpage <- get_individual_alpage(ID, individual_info_file)
    print(paste0("ID: ", ID, " - Alpage récupéré : ", alpage))
    
    # Récupérer la période d'échantillonnage propre à l'individu
    sampling_period_ind <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
    
    # Création d'un sous-dossier pour cet alpage, dans output_dir
    pdf_folder <- file.path(output_dir, paste0("Output_PDF_", alpage))
    if (!dir.exists(pdf_folder)) {
      dir.create(pdf_folder, recursive = TRUE)
    }
    
    # Appel de la fonction hmm_fit en utilisant le dossier pdf_folder pour sauvegarder le PDF
    return(hmm_fit(data[data$ID == ID, ], run_parameters, pdf_folder, sampling_period_ind))
  })
  
  stopCluster(clus)
  print(Sys.time() - startTime)
  return(results)
}


























### HMM results plotting functions
plot_results <- function(hmm, output_pdf_file) {
    # Plot the hmm optimal parameter set and state-sequence resulting from the momentuHMM fit.
    # INPUTS :
    #   hmm : an hmm object, result of the momentuHMM fit.
    #   output_pdf_file : a path to the file in which to save the figures

    pdf(paste0(output_pdf_file,".pdf"), width = 7, height = 7, title=output_pdf_file)
    
    # momentuHMM’s color palette (modified order 3 1 2 4 5...)
    pal <- c("#009E73", "#E69F00", "#56B4E9", "#F0E442",
           "#0072B2", "#D55E00", "#CC79A7")[1:length(hmm$stateNames)]

    ombrage <- get_ombrage(get_minmax_L93(hmm$data, 100))
    hmm_scale = scale_colour_gradientn(colors = pal, guide="legend", breaks = c(1, 2, 3), labels = hmm$stateNames)

    # X vs time
    print(ggplot(hmm$data, aes(time, x, col = state, group=ID)) +
            geom_path(size = 0.3) +
            geom_point(size = 0.3) +
            hmm_scale +
            xlab("") +
            ylab("") +
            theme(legend.position="none") )
    
    # Y vs time
    print(ggplot(hmm$data, aes(time, y, col = state, group=ID)) +
            geom_path(size = 0.3) +
            geom_point(size = 0.3) +
            hmm_scale +
            xlab("") +
            ylab("") +
            theme(legend.position="none") )
               
    # Y vs X
    print(plot_trajectory_over_ombrage (hmm$data, "state", hmm_scale, ombrage, "", "") +
            guides(color = "none"))

    # Convert time to hour-min-sec only for plotting
    hour <- as.POSIXlt(hmm$data$time)
    hour <- hour$h + hour$min/60 + hour$s/3600 + 2 # + 2 to convert from UTC to local time
    hmm$data$hour <- hour
    rm(hour)

    # state vs hour
    hmm$data$state = hmm$stateNames[hmm$data$state]
    print(ggplot(hmm$data, aes(x = round(hour))) +
            geom_bar(position = "fill", aes(fill = state)) +
            scale_y_continuous(labels = scales::percent_format()) +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_fill_manual(values = pal[c(3,2,1)]) +
            xlab("") +
            ylab("") +
            theme(legend.position="none") )

    # Step-length and Angle distributions
    plot(hmm, breaks=25, ask=FALSE, plotTracks=FALSE, plotCI=TRUE, col=pal)
    
    dev.off() # Close the pdf file
}


# Saving functions
next_index_filename = function(prefix, suffix){
    i=1
    repeat {
        f = paste0(prefix,i,suffix)
        if(!file.exists(f)){return(i)}
        i=i+1
    }
}

parameters_to_data.frame <- function(run_parameters) {
    # extensive list of parameters
    par_df = data.frame(model = run_parameters$model)
    par_df$resampling_ratio = run_parameters$resampling_ratio
    par_df$resampling_first_index = run_parameters$resampling_first_index
    par_df$rollavg = run_parameters$rollavg
    par_df$rollavg_convolution = paste(run_parameters$rollavg_convolution, collapse=", ")
    if (!par_df$rollavg) par_df$rollavg_convolution = ""
    par_df$knownRestingStates = run_parameters$knownRestingStates
    par_df$step_distribution = run_parameters$dist$step
    par_df$angle_distribution = run_parameters$dist$angle
    par_df$covariants= deparse1(run_parameters$covariants)
    par_df$step1_mean = run_parameters$Par0$step[1]
    par_df$step1_std = run_parameters$Par0$step[4]
    par_df$step2_mean = run_parameters$Par0$step[2]
    par_df$step2_std = run_parameters$Par0$step[5]
    par_df$step3_mean = run_parameters$Par0$step[3]
    par_df$step3_std = run_parameters$Par0$step[6]
    par_df$angle1_mean = run_parameters$Par0$angle[1]
    par_df$angle1_std = run_parameters$Par0$angle[4]
    par_df$angle2_mean = run_parameters$Par0$angle[2]
    par_df$angle2_std = run_parameters$Par0$angle[5]
    par_df$angle3_mean = run_parameters$Par0$angle[3]
    par_df$angle3_std = run_parameters$Par0$angle[6]
    par_df$angle1_fixed_mean = run_parameters$fixPar$angle[1]
    par_df$angle1_fixed_std = run_parameters$fixPar$angle[4]
    par_df$angle2_fixed_mean = run_parameters$fixPar$angle[2]
    par_df$angle2_fixed_std = run_parameters$fixPar$angle[5]
    par_df$angle3_fixed_mean = run_parameters$fixPar$angle[3]
    par_df$angle3_fixed_std = run_parameters$fixPar$angle[6]

    return(par_df)
}

hmm_result_to_data.frame <- function(run) {
    result_par = data.frame(ID = 0)
    result_par$ID <- strsplit(as.character(run$data$ID[1]), split = "-")[[1]][1]
    result_par$step1_mean = run$mle$step[1,1]
    result_par$step1_std = run$mle$step[2,1]
    result_par$step2_mean = run$mle$step[1,2]
    result_par$step2_std = run$mle$step[2,2]
    result_par$step3_mean = run$mle$step[1,3]
    result_par$step3_std = run$mle$step[2,3]
    result_par$angle1_mean = run$mle$angle[1,1]
    result_par$angle1_std = run$mle$angle[2,1]
    result_par$angle2_mean = run$mle$angle[1,2]
    result_par$angle2_std = run$mle$angle[2,2]
    result_par$angle3_mean = run$mle$angle[1,3]
    result_par$angle3_std = run$mle$angle[2,3]
    result_par$angle3_std = run$mle$angle[2,3]
    result_par$loglikelihood= -run$mod$minimum

    return(result_par)
}

runs_to_csv <- function(run_index, parameters_df, results_df, output_csv_file) {
    table = cbind(run_index, parameters_df, results_df)

    if(!file.exists(output_csv_file)) {
        write.csv(table, output_csv_file, row.names = FALSE)
    }
    else {
        write.table(table, output_csv_file, sep = ",", append = TRUE, 
                    quote = TRUE, col.names = FALSE, row.names = FALSE)
    }
}

runs_to_pdf <- function(run_index, parameters_df, results_df, data_hmm, exec_time, output_dir, output_pdf_file, show_performance_indicators = FALSE) {
    # Generates a pdf (through rmarkdown) summarizing the model run
    # INPUTS
    #   run_index : index of the model run
    #   parameters_df : data.frame containing the run parameters, output of parameters_to_data.frame
    #   results_df : data.frame containing the results of the run, one line per individual, output of hmm_results_to_data.frame
    #   data_hmm : data.frame of the input with a "state" column containing the states identified by the viterbi algorithm
    #   exec_time : run execution time
    #   output_pdf_file : path to the pdf file to be written
    #   show_performance_indicators : FOR MODEL CALIBRATION and for sites with activity data that have been temporaly aligned with Catlog data, should performance boxplots and test be shown

    title = parameters_df$model
    if (parameters_df$resampling_ratio > 1) title = paste0(title,", rééchantillonné à ",parameters_df$resampling_ratio,"x")
    if (parameters_df$rollavg_convolution != "") title = paste0(title,", lissé")
    if (parameters_df$covariants != ~1) title = paste0(title,", covariable(s)")
    if (parameters_df$knownRestingStates) title = paste0(title,", états connus")

    if(show_performance_indicators) { # Only usefull for model calibration
        aligned_data = readRDS(paste0(data_dir,"Cayolle_2022_aligned.rds"))
        # @TODO : à terme il faudra avoir un fichier avec tous les alpages alignés
        
        data_hmm <- data_hmm %>% dplyr::select(-c(step, angle, x, y, hour))
        aligned_data$ODBA = sqrt(aligned_data$actX^2 + aligned_data$actY^2)
        aligned_data <- aligned_data %>% dplyr::select(-c(lat, lon, Altitude, alpage, species, race, x, y, actX, actY))
        colnames(aligned_data)[1] <- "time"
        aligned_data$time <- as.POSIXct(aligned_data$time, origin = '1970-01-01')
        aligned_data$common_date <- as.POSIXct(aligned_data$common_date, origin = '1970-01-01')

        data_hmm = merge(data_hmm, aligned_data[aligned_data$ID %in% unique(data_hmm$ID),], by = c("ID", "time"))
        aligned_data = aligned_data[!(aligned_data$ID %in% unique(data_hmm$ID)),]
        aligned_data$state = NA
        aligned_data$state_proba = NA

        data_hmm = rbind(data_hmm, aligned_data)
        rm(aligned_data)
        data_hmm$time = data_hmm$common_date
        data_hmm$common_date = NULL
        # data_hmm = reshape(data_hmm, idvar = "time", timevar = "ID", direction = "wide")
    }

    rmarkdown::render("Functions/HMM_fit_template.Rmd", output_file = output_pdf_file)
}



runs_to_pdf_2 <- function(run_index, parameters_df, results_df, data_hmm, exec_time, output_dir, output_pdf_file, show_performance_indicators = FALSE) {
  # Génère un PDF résumant l’exécution du modèle HMM via rmarkdown
  
  # Vérification de parameters_df
  if (nrow(parameters_df) != 1) stop("Erreur : parameters_df doit contenir une seule ligne.")
  
  title = parameters_df$model
  if (parameters_df$resampling_ratio > 1) title = paste0(title,", rééchantillonné à ",parameters_df$resampling_ratio,"x")
  if (parameters_df$rollavg_convolution != "") title = paste0(title,", lissé")
  if (parameters_df$covariants != ~1) title = paste0(title,", covariable(s)")
  if (parameters_df$knownRestingStates) title = paste0(title,", états connus")
  
  if(show_performance_indicators) { 
    aligned_data = readRDS(paste0(data_dir,"Cayolle_2022_aligned.rds"))
    
    # Vérification des colonnes communes avant fusion
    common_cols <- intersect(names(data_hmm), names(aligned_data))
    if (!all(c("ID", "time") %in% common_cols)) stop("Erreur : Les colonnes ID et time sont manquantes dans les données.")
    
    # Sélection des colonnes nécessaires
    data_hmm <- data_hmm %>% dplyr::select(-c(step, angle, x, y, hour))
    aligned_data$ODBA = sqrt(aligned_data$actX^2 + aligned_data$actY^2)
    aligned_data <- aligned_data %>% dplyr::select(-c(lat, lon, Altitude, alpage, species, race, x, y, actX, actY))
    colnames(aligned_data)[1] <- "time"
    aligned_data$time <- as.POSIXct(aligned_data$time, origin = '1970-01-01')
    aligned_data$common_date <- as.POSIXct(aligned_data$common_date, origin = '1970-01-01')
    
    data_hmm = merge(data_hmm, aligned_data[, common_cols, drop = FALSE], by = c("ID", "time"))
    aligned_data = aligned_data[!(aligned_data$ID %in% unique(data_hmm$ID)),]
    aligned_data$state = NA
    aligned_data$state_proba = NA
    
    data_hmm = rbind(data_hmm, aligned_data)
    rm(aligned_data)
    data_hmm$time = data_hmm$common_date
    data_hmm$common_date = NULL
  }
  
  # Vérification de la structure de results_df avant d'envoyer à RMarkdown
  if (length(names(results_df)) != ncol(results_df)) {
    stop("Erreur : Le nombre de noms de colonnes dans results_df ne correspond pas au nombre de colonnes réelles.")
  }
  
  # Vérification avant exécution du rendu
  print(dim(results_df))
  print(names(results_df))
  
  # Génération du rapport PDF avec RMarkdown
  rmarkdown::render("Functions/HMM_fit_template.Rmd", output_file = output_pdf_file)
}



parameters_to_rds <- function(run_parameters, output_rds_file) {
    if(!file.exists(output_rds_file)) {
        saveRDS(run_parameters, file = output_rds_file)
    }
    else {
        param_list = readRDS(output_rds_file)
        param_list[[length(param_list)+1]] <- run_parameters
        saveRDS(param_list, file = output_rds_file)
    }
}

viterbi_trajectory_to_rds  <- function(data_hmm, output_file, individual_info_file) {
    individual_info <- read.csv(individual_info_file, header=TRUE)

    data_save <- as.data.frame(subset(data_hmm, select = -c(step, angle)))
    stateNames <- c("Repos", "Paturage", "Deplacement")
    data_save$state = stateNames[data_save$state]
    data_save <- data_save[!is.na(data_save$x),]

    collar_indexes = as.numeric(sapply(data_save$ID, function(id) which(individual_info$Collier==id)))
    data_save$alpage <- individual_info$Alpage[collar_indexes]
    data_save$species <- individual_info$Espece[collar_indexes]
    data_save$race <- individual_info$Race[collar_indexes]

    # Sauvegarde en RDS
    save_append_replace_IDs(data_save, file = output_file)
}


# Utility functions
scale_step_parameters_to_resampling_ratio <- function(run_parameters) {
    run_parameters$Par0$step[2] = run_parameters$resampling_ratio * run_parameters$Par0$step[2]
    run_parameters$Par0$step[5] = sqrt(run_parameters$resampling_ratio) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] = run_parameters$resampling_ratio * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] = sqrt(run_parameters$resampling_ratio) * run_parameters$Par0$step[6]

    return(run_parameters)
}
















### Fonction parallèle modifiée
par_HMM_fit <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
  # Paralelized wrapper for hmm_fit
  print("+++ momentuHMM parallel RUN +++")
  startTime <- Sys.time()
  
  clus <- makeCluster(ncores, outfile = '')
  clusterExport(clus, as.list(lsf.str(.GlobalEnv)))
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
  
  # Chargement des librairies et fonctions dans les clusters
  clusterCall(clus, function() {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    theme_set(theme_bw())
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")  # Courtesy Théo Michelot
    BACKGROUND_TYPE <- "BDALTI"
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    # Récupération de l'alpage pour cet individu
    alpage <- get_individual_alpage(ID, individual_info_file)
    print(paste0("ID: ", ID, " - Alpage récupéré : ", alpage))
    
    # Récupérer la période d'échantillonnage propre à l'individu
    sampling_period_ind <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
    
    # On utilise directement le dossier fourni (output_dir) pour stocker le PDF
    pdf_folder <- output_dir
    
    # Appel de la fonction hmm_fit en passant pdf_folder
    return(hmm_fit(data[data$ID == ID, ], run_parameters, pdf_folder, sampling_period_ind))
  })
  
  stopCluster(clus)
  print(Sys.time() - startTime)
  return(results)
}


### Fonction de fitting modifiée
hmm_fit <- function(data, runPar, output_dir, sampling_period) {
  # Extraction de l'ID de l'individu
  ID = data$ID[1]
  
  ### DATA PREPARATION
  data_hmm <- regularise_trajectories(data, sampling_period)
  if (runPar$rollavg) {
    data_hmm <- rolling_averaging_trajectories(data_hmm, conv = runPar$rollavg_convolution)
  }
  data_hmm <- resample_trajectories(data_hmm, runPar$resampling_ratio, runPar$resampling_first_index)
  data_hmm <- prepare_hmm_trajectories(data_hmm)
  
  knownStates <- rep(NA, nrow(data_hmm))
  
  if(runPar$knownRestingStates) {
    # Si on considère certaines périodes comme repos, on définit les états connus
    knownStates[(data_hmm$hour > 3 && data_hmm$hour < 3.5) ||
                  (data_hmm$hour > 20.5 && data_hmm$hour < 21)] <- 1
  }
  
  ### MODEL FITTING
  stateNames = c("Repos", "Paturage", "Deplacement")
  run <- fitHMM(data_hmm,
                nbStates = 3,
                dist = runPar$dist,
                DM = runPar$DM,
                Par0 = runPar$Par0,
                estAngleMean = list(angle = TRUE),
                fixPar = runPar$fixPar,
                stateNames = stateNames,
                knownStates = knownStates,
                formula = runPar$covariants,
                optMethod = "Nelder-Mead")
  
  # Calcul des états les plus probables et de leurs probabilités
  run$data$state <- viterbi(run)
  state_proba <- stateProbs(run)
  run$data$state_proba <- NA
  for (i in 1:nrow(run$data)) {
    run$data$state_proba[i] <- state_proba[i, run$data$state[i]]
  }
  
  # Enregistrement du PDF directement dans le dossier output_dir
  pdf_file <- file.path(output_dir, paste0(ID, ".pdf"))
  plot_results(run, pdf_file)
  
  # Nettoyage et réorganisation des données
  run$data <- run$data[!is.na(run$data$x), ]
  run$data <- run$data[order(run$data$time), ]
  
  # Restauration de l'ID original
  run$data$ID <- ID
  
  return(run)
}










traj_by_night_park <- function(state_rds_file,
                               output_parc_rds_file,
                               window_minutes,
                               eps_dist,
                               rare_threshold = 5) {
  library(dplyr)
  library(lubridate)
  library(dbscan)
  library(tibble)
  
  # 1) Lecture + date
  data <- readRDS(state_rds_file) %>%
    mutate(date = as_date(time))
  
  # 2) Fenêtre morning / night
  get_window <- function(df, at_start = TRUE) {
    if (at_start) {
      df %>% filter(time <= min(time) + minutes(window_minutes))
    } else {
      df %>% filter(time >= max(time) - minutes(window_minutes))
    }
  }
  
  # 3) Centroides matin / soir
  centroids <- data %>%
    group_by(ID, date) %>%
    do({
      df_day <- .
      tibble(
        phase = c("morning","night"),
        x     = c(mean(get_window(df_day, TRUE )$x, na.rm=TRUE),
                  mean(get_window(df_day, FALSE)$x, na.rm=TRUE)),
        y     = c(mean(get_window(df_day, TRUE )$y, na.rm=TRUE),
                  mean(get_window(df_day, FALSE)$y, na.rm=TRUE))
      )
    }) %>% ungroup()
  
  # 4) DBSCAN global pour assigner un parc
  coords <- as.matrix(centroids[,c("x","y")])
  labs  <- dbscan(coords, eps=eps_dist, minPts=1)$cluster
  centroids <- centroids %>% 
    mutate(parc = paste0("Parc_", labs))
  
  # 5) Split morning / night
  # 5) Split morning / night
  morning <- centroids %>% 
    dplyr::filter(phase == "morning") %>% 
    dplyr::select(ID, date, parc_m = parc)
  
  night   <- centroids %>% 
    dplyr::filter(phase == "night")   %>% 
    dplyr::select(ID, date, parc_n = parc)
  # 6) Flag initial par ID/jour
  transitions <- inner_join(morning, night, by=c("ID","date")) %>%
    mutate(jour_de_transition = (parc_m != parc_n))
  
  # 7) Consensus à la majorité simple
  summary_day <- transitions %>%
    group_by(date) %>%
    summarise(
      total   = n(),
      n_trans = sum(jour_de_transition),
      .groups = "drop"
    ) %>%
    mutate(global = (n_trans > total/2)) %>%
    dplyr::select(date, global)
  
  transitions2 <- transitions %>%
    dplyr::select(ID, date) %>%
    left_join(summary_day, by = "date") %>%
    rename(jour_de_transition = global)
  
  # 8) Reconstruction intermédiaire
  result <- data %>%
    left_join(morning,    by = c("ID","date")) %>%
    left_join(night,      by = c("ID","date")) %>%
    left_join(transitions2, by = c("ID","date"))
  
  # 9) Identification des parcs rares (< rare_threshold jours uniques)
  rare_parcs <- result %>%
    distinct(parc_n, date) %>%       # un parc par date
    count(parc_n, name = "days") %>%
    filter(days < rare_threshold) %>%
    pull(parc_n)
  
  # 10) Préparer la valeur du parc de nuit du jour précédent
  prev_night <- result %>%
    distinct(ID, date, parc_n) %>%
    mutate(date = date + days(1)) %>%    # on remonte d’un jour
    rename(prev_parc_n = parc_n)
  
  result <- result %>%
    left_join(prev_night, by = c("ID","date"))
  
  # 11) Rattacher les jours dans un parc rare au parc de la nuit précédente
  result <- result %>%
    mutate(
      parc_n = if_else(
        parc_n %in% rare_parcs & !is.na(prev_parc_n),
        prev_parc_n,
        parc_n
      )
    )
  
  # 12) Recalculer jour_de_transition sur ces nouveaux parcs
  result <- result %>%
    mutate(jour_de_transition = (parc_m != parc_n))
  
  # 13) Nettoyage & renommage final
  final <- result %>%
    dplyr::select(-prev_parc_n) %>%
    rename(parc = parc_n)               # on garde le parc de nuit (corrigé)
  
  # 14) Sauvegarde
  saveRDS(final, output_parc_rds_file)
  message("→ Écrit avec succès : ", output_parc_rds_file)
}

























traj_by_night_park_count <- function(state_rds_file,
                               output_parc_rds_file,
                               window_minutes,
                               eps_dist,
                               rare_threshold   = 5,
                               require_consec   = FALSE,
                               min_consec_nights = 3) {
  library(dplyr); library(lubridate); library(dbscan); library(tibble)
  
  ### 1) Lecture + date
  data <- readRDS(state_rds_file) %>%
    mutate(date = as_date(time))
  
  ### 2) Fenêtre morning / night
  get_window <- function(df, at_start = TRUE) {
    if (at_start) {
      df %>% filter(time <= min(time) + minutes(window_minutes))
    } else {
      df %>% filter(time >= max(time) - minutes(window_minutes))
    }
  }
  
  ### 3) Centroides matin / soir
  centroids <- data %>%
    group_by(ID, date) %>%
    do({
      df_day <- .
      tibble(
        phase = c("morning","night"),
        x     = c(mean(get_window(df_day, TRUE )$x, na.rm=TRUE),
                  mean(get_window(df_day, FALSE)$x, na.rm=TRUE)),
        y     = c(mean(get_window(df_day, TRUE )$y, na.rm=TRUE),
                  mean(get_window(df_day, FALSE)$y, na.rm=TRUE))
      )
    }) %>% ungroup()
  
  ### 4) Clustering DBSCAN
  coords <- as.matrix(centroids[,c("x","y")])
  labs  <- dbscan(coords, eps=eps_dist, minPts=1)$cluster
  centroids <- centroids %>% mutate(parc = paste0("Parc_", labs))
  
  ### 5) Split morning / night
  morning <- centroids %>% filter(phase=="morning") %>% select(ID, date, parc_m = parc)
  night   <- centroids %>% filter(phase=="night"  ) %>% select(ID, date, parc_n = parc)
  
  ### 6) Flag initial (par ID/jour)
  transitions <- inner_join(morning, night, by=c("ID","date")) %>%
    mutate(jour_de_transition = (parc_m != parc_n))
  
  ### 7) Consensus à la majorité simple
  summary_day <- transitions %>%
    group_by(date) %>%
    summarise(
      total   = n(),
      n_trans = sum(jour_de_transition),
      .groups = "drop"
    ) %>%
    mutate(global = (n_trans > total/2)) %>%
    select(date, global)
  
  transitions2 <- transitions %>%
    select(ID, date) %>%
    left_join(summary_day, by="date") %>%
    rename(jour_de_transition = global)
  
  ### 8) Reconstruction intermédiaire
  result <- data %>%
    left_join(morning,      by=c("ID","date")) %>%
    left_join(night,        by=c("ID","date")) %>%
    left_join(transitions2, by=c("ID","date"))
  
  ### 9) Rattachement des parcs « rares »
  rare_parcs <- result %>%
    distinct(parc_n, date) %>% 
    count(parc_n, name="days") %>%
    filter(days < rare_threshold) %>%
    pull(parc_n)
  
  prev_night <- result %>%
    distinct(ID, date, parc_n) %>%
    mutate(date = date + days(1)) %>%
    rename(prev_parc_n = parc_n)
  
  result <- result %>%
    left_join(prev_night, by=c("ID","date")) %>%
    mutate(
      parc_n = if_else(
        parc_n %in% rare_parcs & !is.na(prev_parc_n),
        prev_parc_n,
        parc_n
      )
    )
  
  ### 10) Recalcul du flag après rattachement
  result <- result %>%
    mutate(jour_de_transition = (parc_m != parc_n))
  
  ### 11) Option : filtre sur les transitions durables via RLE
  if (require_consec) {
    # a) on calcule le parc majoritaire par date
    herd_parc_df <- result %>%
      distinct(ID, date, parc = parc_n) %>%
      count(date, parc) %>%
      group_by(date) %>%
      slice_max(n, n = 1, with_ties = FALSE) %>%
      arrange(date) %>%
      ungroup()
    
    # b) on fait du rle() sur la colonne 'parc'
    values <- as.character(herd_parc_df$parc)
    dates  <- herd_parc_df$date
    r      <- rle(values)
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    
    # c) on récupère la date de début de chaque run assez long
    good_runs <- which(r$lengths >= min_consec_nights)
    transition_dates <- dates[starts[good_runs]]
    
    # d) on marque TRUE uniquement pour ces dates-là
    result <- result %>%
      mutate(jour_de_transition =
               date %in% transition_dates)
  }
  ### 12) Final & sauvegarde
  final <- result %>%
    select(-prev_parc_n) %>%
    rename(parc = parc_n)
  
  saveRDS(final, output_parc_rds_file)
  message("→ Écrit avec succès : ", output_parc_rds_file)
}











rename_viterbi_parc <- function(
    input_parc_rds_file,
    input_table_use_parc_file,
    output_parc_rds_file_renamme
) {
  library(data.table)
  
  # 1) Charger le mapping (info_table_use_parc) et vérifier les colonnes
  map_dt <- as.data.table(readRDS(input_table_use_parc_file))
  if (!all(c("parc", "rename") %in% names(map_dt))) {
    stop("Le RDS de mapping doit contenir les colonnes 'parc' et 'rename'")
  }
  
  # 2) Construire un vecteur nommé pour la correspondance
  vect_map <- setNames(map_dt$rename, map_dt$parc)
  
  # 3) Charger les données viterbi_parc
  parc_dt <- as.data.table(readRDS(input_parc_rds_file))
  
  # 4) Appliquer le renommage
  parc_dt[, parc := vect_map[as.character(parc)] ]
  
  # 5) Vérifier si certains parcs n’ont pas été renommés
  if (any(is.na(parc_dt$parc))) {
    warning("Certain·e·s parc n’ont pas de correspondance dans le mapping")
  }
  
  # 6) Enregistrer le RDS renommé
  saveRDS(parc_dt, output_parc_rds_file_renamme)
  message("Fichier viterbi_parc renommé et sauvegardé :\n  ", output_parc_rds_file_renamme)
}











