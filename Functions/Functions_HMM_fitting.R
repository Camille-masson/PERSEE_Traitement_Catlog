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



## TEST OFB

# Adapter le HMM pour supporter les différents types de données
hmm_fit <- function(data, runPar, alpage_directory, sampling_type) {
  ID <- data$ID[1]
  settings <- SAMPLING[[sampling_type]]
  sampling_period <- settings$sampling_period
  
  data_hmm <- regularise_trajectories(data, sampling_period)
  if (runPar$rollavg) data_hmm <- rolling_averaging_trajectories(data_hmm, conv= runPar$rollavg_convolution)
  
  runPar <- scale_step_parameters_to_sampling_period(runPar, sampling_type)
  
  # Appliquer le sous-échantillonnage uniquement si nécessaire
  if (settings$resampling_ratio > 1) {
    data_hmm <- resample_trajectories(data_hmm, settings$resampling_ratio, runPar$resampling_first_index)
  }
  
  data_hmm <- prepare_hmm_trajectories(data_hmm)
  
  knownStates <- rep(NA, nrow(data_hmm))
  if(runPar$knownRestingStates) {
    knownStates[(data_hmm$hour > 3 & data_hmm$hour < 3.5) | (data_hmm$hour > 20.5 & data_hmm$hour < 21)] <- 1
  }
  
  stateNames <- c("Repos", "Paturage", "Deplacement")
  run <- fitHMM(data_hmm, nbStates = 3, dist = runPar$dist, DM=runPar$DM, Par0 = runPar$Par0,
                estAngleMean=list(angle=TRUE), fixPar = runPar$fixPar,
                stateNames = stateNames,
                knownStates = knownStates,
                formula = runPar$covariants,
                optMethod = "Nelder-Mead")
  
  run$data$state <- viterbi(run)
  state_proba <- stateProbs(run)
  run$data$state_proba <- sapply(1:nrow(run$data), function(i) state_proba[i, run$data$state[i]])
  
  dir.create(paste0(alpage_directory, "individual_trajectories/"), recursive = TRUE)
  plot_results(run, paste0(alpage_directory, "individual_trajectories/", ID))
  
  run$data <- run$data[!is.na(run$data$x),]
  run$data <- run$data[order(run$data$time),]
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






## TEST OFB

par_HMM_fit_test <- function(data, run_parameters, ncores, individual_info_file, sampling_type, output_dir) {
  print("+++ momentuHMM parallel RUN +++")
  
  startTime <- Sys.time()
  clus <- makeCluster(ncores, outfile='')
  clusterExport(clus, as.list(lsf.str(.GlobalEnv)))
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "SAMPLING"), envir = environment())
  
  clusterCall(clus, function() {
    options(warn=-1)
    suppressPackageStartupMessages(library(tidyverse))
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn=0)
  })
  
  results <- parLapply(clus, unique(data$ID),
                       function(ID) {
                         alpage <- get_individual_alpage(ID, individual_info_file)
                         return(hmm_fit(data[data$ID==ID,], run_parameters, paste0(output_dir, alpage, "/"), sampling_period))
                       }
  )
  
  stopCluster(clus)
  endTime <- Sys.time()
  print(paste("Execution time:", round(difftime(endTime, startTime, units='mins'),2), "min"))
  
  
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





#TEST_OFB
# Ajustement des paramètres en fonction de la fréquence d'échantillonnage
scale_step_parameters_to_sampling_period <- function(run_parameters, sampling_params) {
  param_scaling_factor <- sampling_params$param_scaling_factor
  
  run_parameters$Par0$step[2] <- param_scaling_factor * run_parameters$Par0$step[2]
  run_parameters$Par0$step[5] <- sqrt(param_scaling_factor) * run_parameters$Par0$step[5]
  run_parameters$Par0$step[3] <- param_scaling_factor * run_parameters$Par0$step[3]
  run_parameters$Par0$step[6] <- sqrt(param_scaling_factor) * run_parameters$Par0$step[6]
  
  return(run_parameters)
}



## TETS OFB NEW FUNCTION 

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





































































### PARTIE MODIFI 






### HMM fitting and plotting functions
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
  # Vérification des colonnes obligatoires
  required_columns <- c("ID", "time", "x", "y")
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Les colonnes suivantes sont manquantes dans les données :", 
               paste(setdiff(required_columns, colnames(data)), collapse = ", ")))
  }
  
  ID <- data$ID[1]
  print(paste("🔍 Préparation des données pour l'ID :", ID))
  
  # Régularisation des trajectoires
  data_hmm <- regularise_trajectories(data, sampling_period)
  if (is.null(data_hmm) || nrow(data_hmm) == 0) {
    stop("Erreur lors de la régularisation des trajectoires : données vides ou nulles.")
  }
  
  # Application du rolling average si nécessaire
  if (runPar$rollavg) {
    data_hmm <- rolling_averaging_trajectories(data_hmm, conv = runPar$rollavg_convolution)
  }
  
  # Sous-échantillonnage si nécessaire
  if (runPar$resampling_ratio > 1) {
    data_hmm <- resample_trajectories(data_hmm, runPar$resampling_ratio, runPar$resampling_first_index)
  }
  
  # Préparation finale des trajectoires pour le HMM
  data_hmm <- prepare_hmm_trajectories(data_hmm)
  
  # États connus (optionnel)
  knownStates <- rep(NA, nrow(data_hmm))
  if (runPar$knownRestingStates) {
    knownStates[(data_hmm$hour > 3 & data_hmm$hour < 3.5) | (data_hmm$hour > 20.5 & data_hmm$hour < 21)] <- 1
  }
  
  ### MODEL FITTING
  stateNames <- c("Repos", "Paturage", "Deplacement")
  print(paste("🚀 Ajustement du modèle HMM pour l'ID :", ID))
  
  run <- fitHMM(data_hmm, nbStates = 3, dist = runPar$dist, DM = runPar$DM, Par0 = runPar$Par0,
                estAngleMean = list(angle = TRUE), fixPar = runPar$fixPar,
                stateNames = stateNames, knownStates = knownStates,
                formula = runPar$covariants, optMethod = "Nelder-Mead")
  
  # Récupération des états les plus probables
  run$data$state <- viterbi(run)
  state_proba <- stateProbs(run)
  run$data$state_proba <- NA
  for (i in 1:nrow(run$data)) {
    run$data$state_proba[i] <- state_proba[i, run$data$state[i]]
  }
  
  # Création des répertoires pour les figures
  dir.create(paste0(alpage_directory, "individual_trajectories/"), recursive = TRUE, showWarnings = FALSE)
  plot_results(run, paste0(alpage_directory, "individual_trajectories/", ID))
  
  # Suppression des NA et réorganisation des données
  run$data <- run$data[!is.na(run$data$x), ]
  run$data <- run$data[order(run$data$time), ]
  
  # Restauration de l'ID original
  run$data$ID <- ID
  
  return(run)
}


par_HMM_fit_test <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
  print(paste0("+++ momentuHMM parallel RUN +++"))
  
  startTime <- Sys.time()
  
  # Vérification des colonnes obligatoires
  required_columns <- c("ID", "time", "x", "y")
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Les colonnes suivantes sont manquantes dans les données :", 
               paste(setdiff(required_columns, colnames(data)), collapse = ", ")))
  }
  
  # Création du cluster parallèle
  clus <- makeCluster(ncores, outfile = '')
  clusterExport(clus, as.list(lsf.str(.GlobalEnv)))
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
  
  # Chargement des bibliothèques sur chaque nœud
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
    source("Functions/Functions_utility.R")
    BACKGROUND_TYPE <- "BDALTI"
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  # Exécution parallèle
  results <- parLapply(clus, unique(data$ID), function(ID) {
    tryCatch({
      print(paste0("🚀 Processing ID : ", ID))
      alpage <- get_individual_alpage(ID, individual_info_file)
      print(paste0("🔍 ID: ", ID, " - Alpage récupéré : ", alpage))
      
      sampling_period <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
      data_individual <- data[data$ID == ID, ]
      
      if (nrow(data_individual) < 500) {
        print(paste0("⚠️ ID: ", ID, " ignoré car trop peu de points après sous-échantillonnage (", nrow(data_individual), " points)"))
        return(NULL)
      }
      
      return(hmm_fit(data_individual, run_parameters, paste0(output_dir, alpage, "/"), sampling_period))
    }, error = function(e) {
      message(paste("❌ Erreur pour l'ID :", ID))
      message(e)
      return(NULL)
    })
  })
  
  stopCluster(clus)
  endTime <- Sys.time()
  print(paste("+++ Cluster total execution time :", round(difftime(endTime, startTime, units = 'mins'), 2), "min +++"))
  
  return(results)
}






par_HMM_fit_test_sequentiel <- function(data, run_parameters, individual_info_file, sampling_period, output_dir) {
  print(paste0("+++ momentuHMM SEQUENTIAL RUN +++"))
  
  startTime <- Sys.time()
  
  # Vérification des colonnes obligatoires
  required_columns <- c("ID", "time", "x", "y")
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Les colonnes suivantes sont manquantes dans les données :", 
               paste(setdiff(required_columns, colnames(data)), collapse = ", ")))
  }
  
  results <- list()  # Stockage des résultats
  
  for (ID in unique(data$ID)) {
    tryCatch({
      print(paste0("🚀 Processing ID : ", ID))
      
      alpage <- get_individual_alpage(ID, individual_info_file)
      print(paste0("🔍 ID: ", ID, " - Alpage récupéré : ", alpage))
      
      sampling_period <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
      print(paste0("⏳ Sampling Period récupéré pour ID ", ID, " : ", sampling_period))
      
      data_individual <- data[data$ID == ID, ]
      
      print(paste0("📊 Nombre d'observations pour ID ", ID, " : ", nrow(data_individual)))
      
      if (nrow(data_individual) < 500) {
        print(paste0("⚠️ ID: ", ID, " ignoré car trop peu de points après sous-échantillonnage (", nrow(data_individual), " points)"))
        next  # Passe au suivant sans exécuter HMM
      }
      
      # Vérification des doublons dans les timestamps
      duplicate_times <- data_individual %>%
        group_by(time) %>%
        summarise(count = n(), .groups = "drop") %>%
        filter(count > 1)
      
      if (nrow(duplicate_times) > 0) {
        print(paste0("⚠️ ID: ", ID, " contient ", nrow(duplicate_times), " timestamps dupliqués"))
        print(head(duplicate_times, 10))  # Affiche les 10 premiers doublons
        next
      }
      
      print(paste0("✅ ID: ", ID, " prêt pour le HMM Fit"))
      
      result <- hmm_fit(data_individual, run_parameters, paste0(output_dir, alpage, "/"), sampling_period)
      
      results[[ID]] <- result
      
    }, error = function(e) {
      message(paste("❌ Erreur pour l'ID :", ID))
      message(e)
    })
  }
  
  endTime <- Sys.time()
  print(paste("+++ Total execution time :", round(difftime(endTime, startTime, units = 'mins'), 2), "min +++"))
  
  return(results)
}


































## FONCTIONS QUI MARCHE OK 






get_sampling_parameters <- function() {
  sampling_period <- SAMPLING  # Utilisation de l'objet SAMPLING
  
  if (sampling_period < 10) {
    resampling_ratio <- ceiling(10 / sampling_period)  # On ramène à 10 min
    param_scaling_factor <- 10 / 2  # Facteur basé sur 2 min vers 10 min
  } else {
    resampling_ratio <- 1  # Pas de rééchantillonnage
    param_scaling_factor <- sampling_period / 2  # Échelle basée sur 2 min
  }
  
  return(list(sampling_period = sampling_period, resampling_ratio = resampling_ratio, param_scaling_factor = param_scaling_factor))
}







regularise_trajectories <- function(data, sampling_period = 1800, max_gap = 90) {
  # INPUTS :
  #   - data : jeu de données GPS
  #   - sampling_period : période d’échantillonnage en secondes (ex: 120s pour 2 min, 1800s pour 30 min)
  #   - max_gap : durée maximale d'interruption (en minutes) avant de considérer une nouvelle trajectoire
  # OUTPUT :
  #   - data régularisé avec des trajectoires séparées si interruption > max_gap
  
  print(paste0("🔄 Régularisation des trajectoires avec max_gap = ", max_gap, " min..."))
  
  # Découpage en plusieurs trajs si interruption > max_gap minutes
  data <- split_at_gap(data = data, max_gap = max_gap, shortest_track = 2*60)
  
  # Si les données sont déjà en 30 minutes, on applique juste la segmentation et on retourne directement
  if (sampling_period == 1800) {
    print("✅ Données déjà à 30 min, segmentation appliquée mais pas de rééchantillonnage.")
    return(data)
  }
  
  # Sinon, on applique la régularisation complète
  print("⚙️ Application de la régularisation temporelle avec padding...")
  
  data <- data %>%
    group_by(ID) %>%
    group_modify( function(df, group_id) {
      data_na <- setNA(ltraj = as.ltraj(xy = df[, c("x", "y")],
                                        date = df$time,
                                        id = group_id),
                       date.ref = df$time[1],  # Prendre le premier timestamp de la traj
                       dt = sampling_period, tol = 60, units = "sec")
      data_na <- ld(data_na)[, c("x", "y", "date")] # Convertir en dataframe et garder seulement les colonnes utiles
      colnames(data_na) <- c("x", "y", "time")
      return(data_na)
    }, .keep = FALSE ) %>%
    ungroup() %>%
    as.data.frame()
  
  return(data)
}




scale_step_parameters_to_resampling_ratio <- function(run_parameters, alpage) {
  if (alpage == "Combe-Madame") {
    # Ajustement spécifique à cet alpage pour réduire la dominance du pâturage
    run_parameters$Par0$step[2] = 0.8 * sampling_parameters$param_scaling_factor * run_parameters$Par0$step[2] # Réduction des transitions rapides
    run_parameters$Par0$step[5] = 0.8 * sqrt(sampling_parameters$param_scaling_factor) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] = 0.8 * sampling_parameters$param_scaling_factor * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] = 0.8 * sqrt(sampling_parameters$param_scaling_factor) * run_parameters$Par0$step[6]
  } else {
    # Valeurs normales pour les autres alpages
    run_parameters$Par0$step[2] = sampling_parameters$param_scaling_factor * run_parameters$Par0$step[2]
    run_parameters$Par0$step[5] = sqrt(sampling_parameters$param_scaling_factor) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] = sampling_parameters$param_scaling_factor * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] = sqrt(sampling_parameters$param_scaling_factor) * run_parameters$Par0$step[6]
  }
  
  return(run_parameters)
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
















































### NEW with adaptation Print :



## Fonction : Adapate les paramètres aux Temp d'échantillionage

get_sampling_parameters_ancienne <- function() {
  sampling_period <- SAMPLING  # Utilisation de l'objet SAMPLING
  print(paste0("[INFO] Période d'échantillonnage définie à ", sampling_period, " minutes."))
  
  if (sampling_period < 10) {
    resampling_ratio <- ceiling(10 / sampling_period)  # Ajustement à 10 min
    param_scaling_factor <- 10 / 2  # Facteur basé sur 2 min vers 10 min
    print("[INFO] Un rééchantillonnage est nécessaire : resampling_ratio adaptée pour 10 min")
  } else {
    resampling_ratio <- 1  # Pas de rééchantillonnage
    param_scaling_factor <- sampling_period / 2  # Échelle basée sur 2 min
    print("[INFO] Aucun rééchantillonnage nécessaire : resampling_ratio = 1")
  }
  
  return(list(sampling_period = sampling_period, resampling_ratio = resampling_ratio, param_scaling_factor = param_scaling_factor))
}






# Fonction : Adapter les paramètres aux Temps d'échantillonnage

get_sampling_parameters <- function(sampling_period) {
  print(paste0("[INFO] Période d'échantillonnage définie à ", sampling_period, " minutes."))
  
  if (sampling_period < 10) {
    resampling_ratio <- ceiling(10 / sampling_period)  # Ajustement à 10 min
    param_scaling_factor <- 5  # Facteur basé sur 2 min vers 10 min
    print("[INFO] Un rééchantillonnage est nécessaire : resampling_ratio adaptée pour 10 min")
  } else {
    resampling_ratio <- 1  # Pas de rééchantillonnage
    param_scaling_factor <- sampling_period / 2  # Échelle basée sur 2 min
    print("[INFO] Aucun rééchantillonnage nécessaire : resampling_ratio = 1")
  }
  
  return(list(sampling_period = sampling_period, resampling_ratio = resampling_ratio, param_scaling_factor = param_scaling_factor))
}


## Fonction : Adapte les paramètres aux temps d'échantillonnage
scale_step_parameters_to_resampling_ratio <- function(run_parameters, alpage, sampling_parameters) {
  sampling_period <- sampling_parameters$sampling_period  # Extraction de la période d'échantillonnage
  scaling_factor <- sampling_parameters$param_scaling_factor  # Facteur d'échelle
  
  print(paste0("[INFO] Ajustement des paramètres basé sur un échantillonnage de ", sampling_period, 
               " min avec un facteur de conversion de ", scaling_factor))
  
  if (alpage == "Combe-Madame") {
    print(paste0("[INFO] Réduction des transitions rapides pour minimiser la dominance du pâturage sur l'alpage : ", alpage))
    
    run_parameters$Par0$step[2] <- 0.8 * scaling_factor * run_parameters$Par0$step[2]
    run_parameters$Par0$step[5] <- 0.8 * sqrt(scaling_factor) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] <- 0.8 * scaling_factor * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] <- 0.8 * sqrt(scaling_factor) * run_parameters$Par0$step[6]
  } else {
    print(paste0("[INFO] Application des ajustements standards pour l'alpage : ", alpage))
    
    run_parameters$Par0$step[2] <- scaling_factor * run_parameters$Par0$step[2]
    run_parameters$Par0$step[5] <- sqrt(scaling_factor) * run_parameters$Par0$step[5]
    run_parameters$Par0$step[3] <- scaling_factor * run_parameters$Par0$step[3]
    run_parameters$Par0$step[6] <- sqrt(scaling_factor) * run_parameters$Par0$step[6]
  }
  
  print("[INFO] Ajustement terminé.")
  return(run_parameters)
}


























par_HMM_fit_Ancienne <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
  cat("[INFO] Démarrage de l'exécution parallèle de momentuHMM...\n")
  flush.console()
  
  startTime <- Sys.time()
  clus <- makeCluster(ncores, outfile="par_log.txt")  # Redirection vers un fichier log
  
  clusterExport(clus, as.list(lsf.str(.GlobalEnv))) 
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
  
  clusterEvalQ(clus, {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    log_file <- paste0("log_", ID, ".txt")
    
    sink(log_file, append = TRUE)  # Redirige `print()` vers un fichier pour cet ID
    cat(paste0("[INFO] Traitement de l'individu ID: ", ID, "\n"))
    flush.console()
    
    alpage <- get_individual_alpage(ID, individual_info_file)
    cat(paste0("[INFO] Alpage associé: ", alpage, "\n"))
    
    sampling_period <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
    
    result <- hmm_fit(data[data$ID == ID, ], run_parameters, paste0(output_dir, alpage, "/"), sampling_period)
    
    cat(paste0("[INFO] Fin du traitement pour ID: ", ID, "\n"))
    flush.console()
    
    sink()  # Arrêter la redirection du fichier
    
    return(result)
  })
  
  stopCluster(clus)
  endTime <- Sys.time()
  
  cat(paste0("[INFO] Exécution terminée. Durée totale : ", round(difftime(endTime, startTime, units='mins'), 2), " minutes.\n"))
  
  # 🔹 Lecture et affichage des logs générés
  log_files <- list.files(pattern = "log_.*.txt")
  for (log in log_files) {
    cat("\n---- Logs de ", log, " ----\n")
    print(readLines(log))
    file.remove(log)  # Supprimer le fichier après affichage
  }
  
  return(results)
}




par_HMM_fit_test <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
  cat("[INFO] Démarrage de l'exécution parallèle de momentuHMM...\n")
  flush.console()
  
  startTime <- Sys.time()
  clus <- makeCluster(ncores, outfile = "")  # Supprime l'outfile global pour voir la sortie immédiatement
  
  clusterExport(clus, as.list(lsf.str(.GlobalEnv))) 
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
  
  clusterEvalQ(clus, {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    log_file <- paste0("log_", ID, ".txt")
    
    # 🔥 Capture la sortie en console ET écrit en temps réel dans le fichier
    sink(log_file, append = TRUE, split = TRUE)
    
    cat(paste0("[INFO] Traitement de l'individu ID: ", ID, "\n"))
    flush.console()
    
    alpage <- get_individual_alpage(ID, individual_info_file)
    cat(paste0("[INFO] Alpage associé: ", alpage, "\n"))
    
    sampling_period <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
    
    # 🔥 Si une erreur survient, on capture le message et on l'écrit immédiatement
    result <- tryCatch({
      hmm_fit(data[data$ID == ID, ], run_parameters, paste0(output_dir, alpage, "/"), sampling_period)
    }, error = function(e) {
      cat(paste0("[ERREUR] Problème détecté pour ID: ", ID, " - Message: ", e$message, "\n"))
      flush.console()
      return(NULL)
    })
    
    cat(paste0("[INFO] Fin du traitement pour ID: ", ID, "\n"))
    flush.console()
    
    sink()  # Arrête la redirection du fichier
    
    return(result)
  })
  
  stopCluster(clus)
  endTime <- Sys.time()
  
  cat(paste0("[INFO] Exécution terminée. Durée totale : ", round(difftime(endTime, startTime, units='mins'), 2), " minutes.\n"))
  
  # 🔥 Lecture des logs en temps réel
  log_files <- list.files(pattern = "log_.*.txt")
  for (log in log_files) {
    cat("\n---- Logs de ", log, " ----\n")
    print(readLines(log))
  }
  
  return(results)
}






























### HMM fitting and plotting function
hmm_fit <- function(data, runPar, alpage_directory, sampling_period) {
  ID <- data$ID[1]
  cat(paste0("[INFO] Démarrage de l'ajustement du modèle HMM pour l'individu ID: ", ID, 
             " avec une période d'échantillonnage de ", sampling_period, " secondes.\n"))
  flush.console()
  
  cat("[INFO] Régularisation des trajectoires en cours...\n")
  flush.console()
  data_hmm <- regularise_trajectories(data, sampling_period)
  
  if (runPar$rollavg) {
    cat("[INFO] Application de lissage par moyenne mobile...\n")
    flush.console()
    data_hmm <- rolling_averaging_trajectories(data_hmm, conv = runPar$rollavg_convolution)
  }
  
  cat("[INFO] Rééchantillonnage des trajectoires en cours...\n")
  flush.console()
  data_hmm <- resample_trajectories(data_hmm, runPar$resampling_ratio, runPar$resampling_first_index)
  
  cat("[INFO] Préparation des données pour le modèle HMM...\n")
  flush.console()
  data_hmm <- prepare_hmm_trajectories(data_hmm)
  
  knownStates <- rep(NA, nrow(data_hmm))
  
  if ("hour" %in% colnames(data_hmm) && runPar$knownRestingStates) {
    cat("[INFO] Identification des états de repos connus...\n")
    flush.console()
    knownStates[(data_hmm$hour > 3 & data_hmm$hour < 3.5) | (data_hmm$hour > 20.5 & data_hmm$hour < 21)] <- 1
  }
  
  cat("[INFO] Ajustement du modèle HMM en cours...\n")
  flush.console()
  stateNames <- c("Repos", "Pâturage", "Déplacement")
  
  run <- fitHMM(
    data_hmm, 
    nbStates = 3, 
    dist = runPar$dist, 
    DM = runPar$DM, 
    Par0 = runPar$Par0,
    estAngleMean = list(angle = TRUE), 
    fixPar = runPar$fixPar,
    stateNames = stateNames,
    knownStates = knownStates,
    formula = runPar$covariants,
    optMethod = "Nelder-Mead"
  )
  
  cat("[INFO] Modèle HMM ajusté avec succès.\n")
  flush.console()
  
  cat("[INFO] Calcul des probabilités d'état...\n")
  flush.console()
  run$data$state <- viterbi(run)
  state_proba <- stateProbs(run)
  run$data$state_proba <- apply(state_proba, 1, function(x) x[run$data$state])
  
  save_dir <- paste0(alpage_directory, "individual_trajectories/")
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat(paste0("[INFO] Génération des graphiques pour ID: ", ID, "\n"))
  flush.console()
  plot_results(run, paste0(save_dir, ID))
  
  run$data <- run$data[!is.na(run$data$x), ]
  run$data <- run$data[order(run$data$time), ]
  run$data$ID <- ID
  
  cat("[INFO] Ajustement HMM terminé pour ID: ", ID, "\n")
  flush.console()
  
  return(run)
}



regularise_trajectories <- function(data, sampling_period = 1800, max_gap = 90) {
  cat(paste0("[INFO] Début de la régularisation des trajectoires avec max_gap = ", max_gap, " minutes.\n"))
  flush.console()
  
  data <- split_at_gap(data = data, max_gap = max_gap, shortest_track = 2 * 60)
  cat("[INFO] Segmentation des trajectoires effectuée.\n")
  flush.console()
  
  if (sampling_period == 1800) {
    cat("[INFO] Aucune régularisation supplémentaire nécessaire (échantillonnage déjà à 30 minutes).\n")
    flush.console()
    return(data)
  }
  
  cat("[INFO] Application de la régularisation temporelle...\n")
  flush.console()
  
  if (!("ID" %in% colnames(data))) {
    stop("[ERREUR] La colonne ID est manquante dans les données.")
  }
  
  data <- data %>%
    group_by(ID) %>%
    group_modify(function(df, group_id) {
      if (nrow(df) < 2) return(df)  # Évite les erreurs sur les petits groupes
      
      data_na <- setNA(
        ltraj = as.ltraj(xy = df[, c("x", "y")], date = df$time, id = group_id),
        date.ref = df$time[1],
        dt = sampling_period, tol = 60, units = "sec"
      )
      data_na <- ld(data_na)[, c("x", "y", "date")]
      colnames(data_na) <- c("x", "y", "time")
      return(data_na)
    }) %>%
    ungroup() %>%
    as.data.frame()
  
  cat("[INFO] Régularisation terminée.\n")
  flush.console()
  
  return(data)
}











































































































































































































































































par_HMM_fit_test <- function(data, run_parameters_list, ncores, individual_info_file, sampling_table, output_dir) {
  cat("[INFO] Démarrage de l'exécution parallèle de momentuHMM...\n")
  flush.console()
  
  startTime <- Sys.time()
  clus <- makeCluster(ncores, outfile = "")  # Supprime l'outfile global pour voir la sortie immédiatement
  
  clusterExport(clus, as.list(lsf.str(.GlobalEnv))) 
  clusterExport(clus, list("data", "run_parameters_list", "output_dir", "individual_info_file", "raster_dir", "CRS_L93", "sampling_table"), envir = environment())
  
  clusterEvalQ(clus, {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    log_file <- paste0("log_", ID, ".txt")
    
    # 🔥 Capture la sortie en console ET écrit en temps réel dans le fichier
    sink(log_file, append = TRUE, split = TRUE)
    
    cat(paste0("[INFO] Traitement de l'individu ID: ", ID, "\n"))
    flush.console()
    
    alpage <- get_individual_alpage(ID, individual_info_file)
    cat(paste0("[INFO] Alpage associé: ", alpage, "\n"))
    
    sampling_period <- sampling_table$sampling_period[sampling_table$ID == ID, drop = TRUE]
    run_parameters <- run_parameters_list[[ID]]
    
    # 🔥 Si une erreur survient, on capture le message et on l'écrit immédiatement
    result <- tryCatch({
      hmm_fit(data[data$ID == ID, ], run_parameters, paste0(output_dir, alpage, "/"), sampling_period)
    }, error = function(e) {
      cat(paste0("[ERREUR] Problème détecté pour ID: ", ID, " - Message: ", e$message, "\n"))
      flush.console()
      return(NULL)
    })
    
    cat(paste0("[INFO] Fin du traitement pour ID: ", ID, "\n"))
    flush.console()
    
    sink()  # Arrête la redirection du fichier
    
    return(result)
  })
  
  stopCluster(clus)
  endTime <- Sys.time()
  
  cat(paste0("[INFO] Exécution terminée. Durée totale : ", round(difftime(endTime, startTime, units='mins'), 2), " minutes.\n"))
  
  # 🔥 Lecture des logs en temps réel
  log_files <- list.files(pattern = "log_.*.txt")
  for (log in log_files) {
    cat("\n---- Logs de ", log, " ----\n")
    print(readLines(log))
  }
  
  return(results)
}





par_HMM_fit_test_gpt <- function(data, run_parameters_list, ncores, individual_info_file, output_dir, sampling_table) {
  cat("[INFO] Démarrage de l'exécution parallèle de momentuHMM...\n")
  flush.console()
  
  startTime <- Sys.time()
  clus <- makeCluster(ncores, outfile = "")
  
  # 🔥 Vérification et conversion de `sampling_table`
  if (!is.data.frame(sampling_table)) {
    cat("[AVERTISSEMENT] `sampling_table` n'est pas un data.frame, tentative de conversion...\n")
    sampling_table <- as.data.frame(sampling_table)
  }
  if (!"sampling_period" %in% colnames(sampling_table)) {
    stop("[ERREUR] La colonne `sampling_period` est absente de sampling_table !")
  }
  
  # Charger les packages dans les workers
  clusterEvalQ(clus, {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")
    source("Functions/Functions_map_plot.R")
    source("Functions/Functions_HMM_fitting.R")  
    options(warn = 0)
  })
  
  # Exporter les variables globales aux workers
  clusterExport(clus, as.list(lsf.str(.GlobalEnv))) 
  # Exportation des variables aux workers
  clusterExport(clus, list("data", "run_parameters_list", "output_dir", "individual_info_file", 
                           "sampling_table", "CRS_L93", "CRS_WSG84"), envir = environment())
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    log_file <- paste0("log_", ID, ".txt")
    sink(log_file, append = TRUE, split = TRUE)
    on.exit(sink(), add = TRUE)
    
    cat(paste0("[INFO] Traitement de l'individu ID: ", ID, "\n"))
    flush.console()
    
    alpage <- get_individual_alpage(ID, individual_info_file)
    cat(paste0("[INFO] Alpage associé: ", alpage, "\n"))
    
    if (!ID %in% names(run_parameters_list)) {
      cat(paste0("[ERREUR] ID ", ID, " absent de run_parameters_list\n"))
      return(NULL)
    }
    run_parameters <- run_parameters_list[[ID]]
    
    sampling_period <- sampling_table$sampling_period[sampling_table$ID == ID]
    sampling_period <- ifelse(length(sampling_period) > 0, as.numeric(sampling_period), NA)
    
    if (is.na(sampling_period) || length(sampling_period) == 0) {
      cat(paste0("[ERREUR] Aucun `sampling_period` trouvé pour ID: ", ID, "\n"))
      return(NULL)
    }
    
    cat(paste0("[INFO] ID: ", ID, " | Sampling Period: ", sampling_period, " sec\n"))
    
    # 🔥 Vérification de `CRS_L93`
    if (!exists("CRS_L93")) {
      cat(paste0("[ERREUR] `CRS_L93` est introuvable pour ID: ", ID, "\n"))
      return(NULL)
    }
    
    result <- tryCatch({
      hmm_fit(data[data$ID == ID, ], run_parameters, paste0(output_dir, alpage, "/"), sampling_period)
    }, error = function(e) {
      cat(paste0("[ERREUR] Problème détecté pour ID: ", ID, " - Message: ", e$message, "\n"))
      flush.console()
      return(NULL)
    })
    
    cat(paste0("[INFO] Fin du traitement pour ID: ", ID, "\n"))
    flush.console()
    
    return(result)
  })
  
  stopCluster(clus)
  endTime <- Sys.time()
  
  cat(paste0("[INFO] Exécution terminée. Durée totale : ", round(difftime(endTime, startTime, units='mins'), 2), " minutes.\n"))
  
  return(results)
}



















### HMM fitting and plotting function
hmm_fit <- function(data, runPar, alpage_directory, sampling_period) {
  ID <- data$ID[1]
  cat(paste0("[INFO] Démarrage de l'ajustement du modèle HMM pour l'individu ID: ", ID, 
             " avec une période d'échantillonnage de ", sampling_period, " secondes.\n"))
  flush.console()
  
  cat("[INFO] Régularisation des trajectoires en cours...\n")
  flush.console()
  data_hmm <- regularise_trajectories(data, sampling_period)
  
  if (runPar$rollavg) {
    cat("[INFO] Application de lissage par moyenne mobile...\n")
    flush.console()
    data_hmm <- rolling_averaging_trajectories(data_hmm, conv = runPar$rollavg_convolution)
  }
  
  cat("[INFO] Rééchantillonnage des trajectoires en cours...\n")
  flush.console()
  data_hmm <- resample_trajectories(data_hmm, runPar$resampling_ratio, runPar$resampling_first_index)
  
  cat("[INFO] Préparation des données pour le modèle HMM...\n")
  flush.console()
  data_hmm <- prepare_hmm_trajectories(data_hmm)
  
  knownStates <- rep(NA, nrow(data_hmm))
  
  if ("hour" %in% colnames(data_hmm) && runPar$knownRestingStates) {
    cat("[INFO] Identification des états de repos connus...\n")
    flush.console()
    knownStates[(data_hmm$hour > 3 & data_hmm$hour < 3.5) | (data_hmm$hour > 20.5 & data_hmm$hour < 21)] <- 1
  }
  
  cat("[INFO] Ajustement du modèle HMM en cours...\n")
  flush.console()
  stateNames <- c("Repos", "Pâturage", "Déplacement")
  
  run <- fitHMM(
    data_hmm, 
    nbStates = 3, 
    dist = runPar$dist, 
    DM = runPar$DM, 
    Par0 = runPar$Par0,
    estAngleMean = list(angle = TRUE), 
    fixPar = runPar$fixPar,
    stateNames = stateNames,
    knownStates = knownStates,
    formula = runPar$covariants,
    optMethod = "Nelder-Mead"
  )
  
  cat("[INFO] Modèle HMM ajusté avec succès.\n")
  flush.console()
  
  cat("[INFO] Calcul des probabilités d'état...\n")
  flush.console()
  run$data$state <- viterbi(run)
  state_proba <- stateProbs(run)
  run$data$state_proba <- apply(state_proba, 1, function(x) x[run$data$state])
  
  save_dir <- paste0(alpage_directory, "individual_trajectories/")
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat(paste0("[INFO] Génération des graphiques pour ID: ", ID, "\n"))
  flush.console()
  plot_results(run, paste0(save_dir, ID))
  
  run$data <- run$data[!is.na(run$data$x), ]
  run$data <- run$data[order(run$data$time), ]
  run$data$ID <- ID
  
  cat("[INFO] Ajustement HMM terminé pour ID: ", ID, "\n")
  flush.console()
  
  return(run)
}


regularise_trajectories <- function(data, sampling_period, max_gap = 90) {
  cat(paste0("[INFO] Début de la régularisation des trajectoires avec max_gap = ", max_gap, " minutes.\n"))
  flush.console()
  
  #Définition de max_gap si inférieur ou égale a 10 min vu sous échantillonage autamatique a 10 min max gap = sampling *2, pareil pour le reste
  if (sampling_period <= 600){ max_gap = 20} else {max_gap = sampling_period/60 * 2}
  
  
  # Segmentation en fonction des gaps
  data <- split_at_gap(data = data, max_gap = max_gap, shortest_track = 2 * 60)
  cat("[INFO] Segmentation des trajectoires effectuée.\n")
  flush.console()
  
  #  Si l'échantillonnage est déjà >= 10 min (600 sec), NE PAS RÉGULARISER
  if (sampling_period >= 600) {
    cat(paste0("[INFO] Aucun rééchantillonnage nécessaire (sampling_period = ", sampling_period, " sec).\n"))
    flush.console()
    return(data)
  }
  
  cat("[INFO] Application de la régularisation temporelle...\n")
  flush.console()
  
  if (!("ID" %in% colnames(data))) {
    stop("[ERREUR] La colonne ID est manquante dans les données.")
  }
  
  data <- data %>%
    group_by(ID) %>%
    group_modify(function(df, group_id) {
      if (nrow(df) < 2) return(df)  # Évite les erreurs sur les petits groupes
      
      data_na <- setNA(
        ltraj = as.ltraj(xy = df[, c("x", "y")], date = df$time, id = group_id),
        date.ref = df$time[1],
        dt = sampling_period, tol = 60, units = "sec"
      )
      data_na <- ld(data_na)[, c("x", "y", "date")]
      colnames(data_na) <- c("x", "y", "time")
      return(data_na)
    }) %>%
    ungroup() %>%
    as.data.frame()
  
  cat("[INFO] Régularisation terminée.\n")
  flush.console()
  
  return(data)
}





viterbi_trajectory_to_rds <- function(data_hmm, output_file, individual_info_file) {
  cat("[INFO] Adaptation et sauvegarde des trajectoires HMM en cours...\n")
  flush.console()
  
  # 🔥 Chargement des informations individuelles
  if (!file.exists(individual_info_file)) {
    stop("[ERREUR] Fichier `individual_info_file` introuvable : ", individual_info_file)
  }
  individual_info <- read.csv(individual_info_file, header=TRUE, stringsAsFactors=FALSE)
  
  # 🔥 Vérification des colonnes nécessaires
  required_cols <- c("Collier", "Alpage", "Espece", "Race")
  missing_cols <- setdiff(required_cols, colnames(individual_info))
  if (length(missing_cols) > 0) {
    stop("[ERREUR] Colonnes manquantes dans `individual_info_file` : ", paste(missing_cols, collapse=", "))
  }
  
  # 🔥 Suppression des colonnes inutiles
  data_save <- as.data.frame(subset(data_hmm, select = -c(step, angle)))
  
  # 🔥 Conversion des états numériques en labels
  stateNames <- c("Repos", "Paturage", "Deplacement")
  data_save$state <- factor(stateNames[data_save$state], levels=stateNames)
  
  # 🔥 Suppression des valeurs manquantes
  data_save <- data_save[!is.na(data_save$x),]
  
  # 🔥 Associer les informations de l'individu
  data_save <- merge(data_save, individual_info, by.x="ID", by.y="Collier", all.x=TRUE)
  
  # 🔥 Vérification de la fusion
  if (any(is.na(data_save$Alpage))) {
    cat("[AVERTISSEMENT] Certains individus n'ont pas trouvé d'alpage dans `individual_info_file`.\n")
  }
  
  # 🔥 Sauvegarde en `.RDS`
  tryCatch({
    save_append_replace_IDs(data_save, file = output_file)
    cat("[INFO] Sauvegarde réussie dans : ", output_file, "\n")
  }, error = function(e) {
    cat("[ERREUR] Impossible d'enregistrer le fichier RDS !\nMessage: ", e$message, "\n")
  })
  
  flush.console()
}















par_HMM_fit_ancienne <- function(data, run_parameters, ncores, individual_info_file, sampling_period, output_dir) {
  cat("[INFO] Démarrage de l'exécution parallèle de momentuHMM...\n")
  flush.console()
  
  startTime <- Sys.time()
  clus <- makeCluster(ncores, outfile = "")  # Supprime l'outfile global pour voir la sortie immédiatement
  
  clusterExport(clus, as.list(lsf.str(.GlobalEnv))) 
  clusterExport(clus, list("data", "run_parameters", "output_dir", "individual_info_file", "raster_dir", "CRS_L93"), envir = environment())
  
  clusterEvalQ(clus, {
    options(warn = -1)
    suppressPackageStartupMessages(library(tidyverse))
    library(lubridate)
    library(momentuHMM)
    library(adehabitatLT)
    library(sf)
    library(sp)
    library(terra)
    source("Functions/Functions_utility.R")
    source("Functions/Functions_map_plot.R")
    source("Functions/Constants.R")
    options(warn = 0)
  })
  
  results <- parLapply(clus, unique(data$ID), function(ID) {
    log_file <- paste0("log_", ID, ".txt")
    
    # 🔥 Capture la sortie en console ET écrit en temps réel dans le fichier
    sink(log_file, append = TRUE, split = TRUE)
    
    cat(paste0("[INFO] Traitement de l'individu ID: ", ID, "\n"))
    flush.console()
    
    alpage <- get_individual_alpage(ID, individual_info_file)
    cat(paste0("[INFO] Alpage associé: ", alpage, "\n"))
    
    sampling_period <- get_individual_info(ID, individual_info_file, "Periode_echantillonnage")
    
    # 🔥 Si une erreur survient, on capture le message et on l'écrit immédiatement
    result <- tryCatch({
      hmm_fit_ancienne(data[data$ID == ID, ], run_parameters, paste0(output_dir, alpage, "/"), sampling_period)
    }, error = function(e) {
      cat(paste0("[ERREUR] Problème détecté pour ID: ", ID, " - Message: ", e$message, "\n"))
      flush.console()
      return(NULL)
    })
    
    cat(paste0("[INFO] Fin du traitement pour ID: ", ID, "\n"))
    flush.console()
    
    sink()  # Arrête la redirection du fichier
    
    return(result)
  })
  
  stopCluster(clus)
  endTime <- Sys.time()
  
  cat(paste0("[INFO] Exécution terminée. Durée totale : ", round(difftime(endTime, startTime, units='mins'), 2), " minutes.\n"))
  
  # 🔥 Lecture des logs en temps réel
  log_files <- list.files(pattern = "log_.*.txt")
  for (log in log_files) {
    cat("\n---- Logs de ", log, " ----\n")
    print(readLines(log))
  }
  
  return(results)
}






























### HMM fitting and plotting function
hmm_fit_ancienne <- function(data, runPar, alpage_directory, sampling_period) {
  ID <- data$ID[1]
  cat(paste0("[INFO] Démarrage de l'ajustement du modèle HMM pour l'individu ID: ", ID, 
             " avec une période d'échantillonnage de ", sampling_period, " secondes.\n"))
  flush.console()
  
  cat("[INFO] Régularisation des trajectoires en cours...\n")
  flush.console()
  data_hmm <- regularise_trajectories_ancienne(data, sampling_period)
  
  if (runPar$rollavg) {
    cat("[INFO] Application de lissage par moyenne mobile...\n")
    flush.console()
    data_hmm <- rolling_averaging_trajectories(data_hmm, conv = runPar$rollavg_convolution)
  }
  
  cat("[INFO] Rééchantillonnage des trajectoires en cours...\n")
  flush.console()
  data_hmm <- resample_trajectories(data_hmm, runPar$resampling_ratio, runPar$resampling_first_index)
  
  cat("[INFO] Préparation des données pour le modèle HMM...\n")
  flush.console()
  data_hmm <- prepare_hmm_trajectories(data_hmm)
  
  knownStates <- rep(NA, nrow(data_hmm))
  
  if ("hour" %in% colnames(data_hmm) && runPar$knownRestingStates) {
    cat("[INFO] Identification des états de repos connus...\n")
    flush.console()
    knownStates[(data_hmm$hour > 3 & data_hmm$hour < 3.5) | (data_hmm$hour > 20.5 & data_hmm$hour < 21)] <- 1
  }
  
  cat("[INFO] Ajustement du modèle HMM en cours...\n")
  flush.console()
  stateNames <- c("Repos", "Pâturage", "Déplacement")
  
  run <- fitHMM(
    data_hmm, 
    nbStates = 3, 
    dist = runPar$dist, 
    DM = runPar$DM, 
    Par0 = runPar$Par0,
    estAngleMean = list(angle = TRUE), 
    fixPar = runPar$fixPar,
    stateNames = stateNames,
    knownStates = knownStates,
    formula = runPar$covariants,
    optMethod = "Nelder-Mead"
  )
  
  cat("[INFO] Modèle HMM ajusté avec succès.\n")
  flush.console()
  
  cat("[INFO] Calcul des probabilités d'état...\n")
  flush.console()
  run$data$state <- viterbi(run)
  state_proba <- stateProbs(run)
  run$data$state_proba <- apply(state_proba, 1, function(x) x[run$data$state])
  
  save_dir <- paste0(alpage_directory, "individual_trajectories/")
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat(paste0("[INFO] Génération des graphiques pour ID: ", ID, "\n"))
  flush.console()
  plot_results(run, paste0(save_dir, ID))
  
  run$data <- run$data[!is.na(run$data$x), ]
  run$data <- run$data[order(run$data$time), ]
  run$data$ID <- ID
  
  cat("[INFO] Ajustement HMM terminé pour ID: ", ID, "\n")
  flush.console()
  
  return(run)
}



regularise_trajectories_ancienne <- function(data, sampling_period = 600, max_gap = 20) {
  cat(paste0("[INFO] Début de la régularisation des trajectoires avec max_gap = ", max_gap, " minutes.\n"))
  flush.console()
  
  data <- split_at_gap(data = data, max_gap = max_gap, shortest_track = 2 * 60)
  cat("[INFO] Segmentation des trajectoires effectuée.\n")
  flush.console()
  
  if (sampling_period == 1800) {
    cat("[INFO] Aucune régularisation supplémentaire nécessaire (échantillonnage déjà à 30 minutes).\n")
    flush.console()
    return(data)
  }
  
  cat("[INFO] Application de la régularisation temporelle...\n")
  flush.console()
  
  if (!("ID" %in% colnames(data))) {
    stop("[ERREUR] La colonne ID est manquante dans les données.")
  }
  
  data <- data %>%
    group_by(ID) %>%
    group_modify(function(df, group_id) {
      if (nrow(df) < 2) return(df)  # Évite les erreurs sur les petits groupes
      
      data_na <- setNA(
        ltraj = as.ltraj(xy = df[, c("x", "y")], date = df$time, id = group_id),
        date.ref = df$time[1],
        dt = sampling_period, tol = 60, units = "sec"
      )
      data_na <- ld(data_na)[, c("x", "y", "date")]
      colnames(data_na) <- c("x", "y", "time")
      return(data_na)
    }) %>%
    ungroup() %>%
    as.data.frame()
  
  cat("[INFO] Régularisation terminée.\n")
  flush.console()
  
  return(data)
}
































