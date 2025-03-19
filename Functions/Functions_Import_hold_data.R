# Fonctions adaptation au nouveau format
# Cette fonction permet d'apaté les données de remy et tout les comportement était dans le meme dossier

filtering_and_saving_viterbi_by_alpage <- function(input_rds_file, output_dir, year) {
  # Charger les données
  data <- readRDS(input_rds_file)
  
  # Vérifier la présence de la colonne "alpage"
  if (!"alpage" %in% names(data)) {
    stop("La colonne 'alpage' est introuvable dans les données.")
  }
  
  # Liste des alpages uniques
  alpages <- unique(data$alpage)
  
  # Création du dossier de sortie si inexistant
  filter_output_dir <- file.path(output_dir, "HMM_comportement")
  if (!dir.exists(filter_output_dir)) {
    dir.create(filter_output_dir, recursive = TRUE)
  }
  
  # Boucler et filtrer explicitement par alpage
  for (alpage_courant in alpages) {
    # Filtrer correctement sur l'alpage courant
    data_alpage <- subset(data, alpage == alpage_courant)
    
    # Construire le nom de fichier pour l'alpage courant
    output_rds_file <- file.path(filter_output_dir, paste0("Catlog_", year, "_", alpage_courant, "_viterbi.rds"))
    
    # Sauvegarder les données filtrées
    saveRDS(data_alpage, output_rds_file)
    
    cat("Alpage traité et sauvegardé :", alpage_courant, "\n")
  }
}


