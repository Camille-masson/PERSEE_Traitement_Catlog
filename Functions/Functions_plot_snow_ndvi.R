

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
plot_violin_alti <- function(alpages,
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
plot_fsca_alti_elypse <- function(alpages, data_dir, output_dir, years_to_use, ellipse_level) {
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
plot_fsca_alti_points <- function(alpages,
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


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)      # pour squish
library(ggnewscale)  # new_scale_color()
library(patchwork)   # wrap_plots, plot_layout, plot_annotation

# Utilitaire pour trier chronologiquement
get_start_date <- function(ps) {
  start <- sub("_.*", "", ps)             # "mi-juin"
  parts <- strsplit(start, "-")[[1]]      # c("mi","juin")
  timing   <- parts[1]                    # "mi" ou "début"
  monthstr <- parts[2]
  mmap <- c(juin=6, juillet=7, aout=8, août=8, septembre=9, sept=9)
  m <- mmap[tolower(monthstr)]
  d <- if (timing=="début") 1 else 15
  as.Date(sprintf("2000-%02d-%02d", m, d))
}

plot_fsca_alti_points_one_parc <- function(alpage,
                                           data_rds,
                                           years_to_use = c(2023, 2024)) {
  # Vos 5 palettes d'origine
  color_map <- list(
    c("#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"),
    c("#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"),
    c("#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"),
    c("#c994c7","#df65b0","#e7298a","#ce1256","#980043","#67001f"),
    c("#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b")
  )
  
  # Lecture + pivot + filtres
  df <- readRDS(data_rds) %>%
    pivot_longer(
      cols          = starts_with("load_"),
      names_to      = c("load_year","load_parc"),
      names_pattern = "load_(\\d{4})_(.*)",
      values_to     = "load"
    ) %>%
    mutate(
      load_year   = as.integer(load_year),
      load_round  = round(load/10)*10,
      capped_load = pmin(pmax(load_round,10),1000)
    ) %>%
    filter(
      load_year %in% years_to_use,
      !is.na(load),
      load >= 10, load < 1000
    )
  
  # Base du plot
  p <- ggplot(df, aes(x = FSCA, y = altitude)) +
    facet_wrap(~ load_year) +
    scale_x_reverse(limits = c(1, 0), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "FSCA", y = "Altitude (m)", title = alpage) +
    theme_bw(base_size = 14) +
    theme(
      strip.background = element_rect(fill = "grey90"),
      strip.text       = element_text(face = "bold")
    )
  
  # Extraire et trier les parc
  raw_ps <- unique(sub("^.*parc_", "", df$load_parc))
  raw_ps <- raw_ps[!is.na(sapply(raw_ps, get_start_date))]
  ps_ord <- raw_ps[order(sapply(raw_ps, get_start_date))]
  ps_use <- if (length(ps_ord) > 3) head(ps_ord, -1) else ps_ord
  
  # Boucle sur chaque parc
  for (i in seq_along(ps_use)) {
    ps        <- ps_use[i]
    pal       <- color_map[[(i-1) %% length(color_map) + 1]]
    label_ps  <- gsub("_", " à ", ps)
    full_lps  <- grep(paste0("parc_", ps, "$"), df$load_parc, value = TRUE)
    dfp       <- df %>% filter(load_parc %in% full_lps) %>% sample_frac(0.5)
    if (nrow(dfp) == 0) next
    
    p <- p +
      new_scale_color() +
      geom_point(
        data  = dfp,
        aes(color = capped_load),
        size  = 1.5, alpha = 0.6
      ) +
      scale_color_gradient(
        low    = pal[1],
        high   = pal[6],
        limits = c(10, 1000),
        trans  = "log",
        oob    = squish,
        name   = paste0("Parc : ", label_ps),
        breaks = c(10, 1000),
        labels = c("10", "<1000"),
        guide  = guide_colorbar(
          barwidth       = unit(1, "cm"),
          barheight      = unit(0.8, "cm"),
          direction      = "horizontal",
          label.position = "top",
          title.position = "bottom",
          title.hjust    = 0.5,
          order          = i
        )
      )
  }
  
  return(p)
}

plot_fsca_alti_points_parc <- function(alpages,
                                       data_dir,
                                       output_dir,
                                       years_to_use = c(2023, 2024),
                                       file_prefix  = "Plot_fsca_alti_points_parc",
                                       dpi          = 300) {
  plots <- list()
  for (i in seq_along(alpages)) {
    alpage <- alpages[i]
    rds_f  <- file.path(
      data_dir,
      paste0("Use_Fsca_Alti_", alpage, "_by_raster_and_parc.rds")
    )
    p <- plot_fsca_alti_points_one_parc(alpage, rds_f, years_to_use)
    if (i < length(alpages)) p <- p + theme(legend.position = "none")
    plots[[alpage]] <- p
  }
  
  # 1) Empile et collecte les légendes
  combined <- wrap_plots(plots, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.spacing.x  = unit(0.3, "cm"),
      legend.key.width  = unit(0.8, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.text       = element_text(size = 7),
      legend.title      = element_text(size = 8)
    )
  
  # 2) Titre global
  combined <- combined +
    plot_annotation(
      title = paste0(
        "Alpages : ", paste(alpages, collapse = ", "),
        " | Années : ", paste(years_to_use, collapse = ", ")
      ),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
  
  # 3) Sauvegarde A4 portrait en JPG
  fname <- paste0(
    file_prefix, "_",
    paste(alpages, collapse = "_"),
    "_Years_", paste(years_to_use, collapse = "_"),
    ".jpg"
  )
  out <- file.path(output_dir, fname)
  ggsave(out, combined,
         device = "jpg",
         width  = 8.27,
         height = 11.69,
         units  = "in",
         dpi    = dpi)
  message("Plot saved to: ", out)
  return(combined)
}
























































library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)        # pour squish
library(ggnewscale)    # new_scale_color()
library(patchwork)     # wrap_plots, plot_layout, plot_annotation

# Utilitaire pour trier chronologiquement les noms de parc
get_start_date <- function(ps) {
  start  <- sub("_.*", "", ps)
  parts  <- strsplit(start, "-")[[1]]
  timing <- parts[1]; mstr <- parts[2]
  mmap   <- c(juin=6, juillet=7, aout=8, août=8, septembre=9, sept=9)
  month  <- mmap[tolower(mstr)]
  day    <- if (timing == "début") 1 else 15
  as.Date(sprintf("2000-%02d-%02d", month, day))
}
plot_fsca_alti_points_one_parc_verif <- function(alpage,
                                           data_rds,
                                           years_to_use = c(2023, 2024)) {
  # Palettes
  color_map <- list(
    c("#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"),
    c("#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"),
    c("#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"),
    c("#c994c7","#df65b0","#e7298a","#ce1256","#980043","#67001f"),
    c("#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b")  
  )
  
  
  # Lecture
  df_full <- readRDS(data_rds)
  
  # Fond gris : tous les pixels x alt, répliqué par année
  data_base  <- unique(df_full[, c("FSCA","altitude")])
  data_base2 <- tidyr::crossing(data_base, load_year = years_to_use)
  
  # Points colorés
  df <- df_full %>%
    pivot_longer(
      cols          = starts_with("load_"),
      names_to      = c("load_year","load_parc"),
      names_pattern = "load_(\\d{4})_(.*)",
      values_to     = "load"
    ) %>%
    mutate(
      load_year   = as.integer(load_year),
      load_round  = round(load/10)*10,
      capped_load = pmin(pmax(load_round, 10), 1000)
    ) %>%
    filter(
      load_year %in% years_to_use,
      !is.na(load),
      load >= 10, load < 1000
    )
  
  # Plot de base + fond gris
  p <- ggplot() +
    geom_point(
      data        = data_base2,
      aes(x = FSCA, y = altitude),
      color       = "grey80",
      size        = 0.5,
      alpha       = 0.4,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ load_year) +
    scale_x_reverse(limits = c(1, 0), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "FSCA", y = "Altitude (m)", title = alpage) +
    theme_bw(base_size = 14) +
    theme(
      strip.background = element_rect(fill = "grey90"),
      strip.text       = element_text(face = "bold")
    )
  
  # Tri et suppression conditionnelle du dernier parc
  raw_ps <- unique(sub("^.*parc_", "", df$load_parc))
  raw_ps <- raw_ps[!is.na(sapply(raw_ps, get_start_date))]
  ps_ord <- raw_ps[order(sapply(raw_ps, get_start_date))]
  ps_use <- if (length(ps_ord) > 3) head(ps_ord, -1) else ps_ord
  
  # Ajout des points colorés
  for (i in seq_along(ps_use)) {
    ps       <- ps_use[i]
    pal      <- color_map[[(i-1) %% length(color_map) + 1]]
    label_ps <- gsub("_", " à ", ps)
    full_lps <- grep(paste0("parc_", ps, "$"), df$load_parc, value = TRUE)
    dfp      <- df %>% filter(load_parc %in% full_lps) %>% sample_frac(0.5)
    if (!nrow(dfp)) next
    
    p <- p +
      ggnewscale::new_scale_color() +
      geom_point(
        data = dfp,
        aes(x = FSCA, y = altitude, color = capped_load),
        size  = 1.5, alpha = 0.6
      ) +
      scale_color_gradient(
        low    = pal[1],
        high   = pal[6],
        limits = c(10,1000),
        trans  = "log",
        oob    = scales::squish,
        name   = paste0("Parc : ", label_ps),
        breaks = c(10,1000),
        labels = c("10","<1000"),
        guide  = guide_colorbar(
          barwidth       = unit(1, "cm"),
          barheight      = unit(0.8, "cm"),
          direction      = "horizontal",
          label.position = "top",
          title.position = "bottom",
          title.hjust    = 0.5,
          order          = i
        )
      )
  }
  
  return(p)
}

# 2) Empilement et sauvegarde A4 portrait
plot_fsca_alti_points_parc_verif <- function(alpages,
                                       data_dir,
                                       output_dir,
                                       years_to_use = c(2023, 2024),
                                       file_prefix  = "Plot_fsca_alti_points_parc",
                                       dpi          = 300) {
  plots <- vector("list", length(alpages))
  for (i in seq_along(alpages)) {
    alpage <- alpages[i]
    rds_f  <- file.path(
      data_dir,
      paste0("Use_Fsca_Alti_", alpage, "_by_raster_and_parc.rds")
    )
    p <- plot_fsca_alti_points_one_parc_verif(alpage, rds_f, years_to_use)
    if (i < length(alpages)) p <- p + theme(legend.position="none")
    plots[[i]] <- p
  }
  
  combined <- wrap_plots(plots, ncol = 1) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0(
        "Alpages : ", paste(alpages, collapse=", "),
        " | Années : ", paste(years_to_use, collapse=", ")
      ),
      theme = theme(plot.title=element_text(hjust=0.5, face="bold"))
    ) &
    theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.spacing.x  = unit(0.3, "cm"),
      legend.key.width  = unit(0.8, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.text       = element_text(size = 7),
      legend.title      = element_text(size = 8)
    )
  
  # Sauvegarde JPG A4 portrait
  fname <- paste0(
    file_prefix, "_",
    paste(alpages, collapse="_"),
    "_Years_", paste(years_to_use, collapse="_"),
    "_with_point.jpg"
  )
  out <- file.path(output_dir, fname)
  ggsave(out, combined,
         device = "jpg",
         width  = 8.27,
         height = 11.69,
         units  = "in",
         dpi    = dpi)
  message("Plot saved to: ", out)
  return(combined)
}












library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)        # pour squish()
library(ggnewscale)    # pour new_scale_color()
library(patchwork)     # pour wrap_plots, plot_layout, plot_annotation

# Utilitaire pour trier les noms de parc
get_start_date <- function(ps) {
  parts  <- strsplit(sub("_.*","",ps), "-")[[1]]
  timing <- parts[1]; mstr <- parts[2]
  mmap   <- c(juin=6, juillet=7, aout=8, août=8, septembre=9, sept=9)
  month  <- mmap[tolower(mstr)]
  day    <- if (timing=="début") 1 else 15
  as.Date(sprintf("2000-%02d-%02d", month, day))
}

plot_fsca_alti_points_one_parc <- function(alpage,
                                           data_rds,
                                           years_to_use = c(2022, 2023, 2024),
                                           nb_bins = 40) {
  # — Vos palettes « quinzaine »
  color_map <- list(
    c("#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b"),
    c("#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d"),
    c("#fdae6b","#fd8d3c","#f16913","#d94801","#a63603","#7f2704"),
    c("#c994c7","#df65b0","#e7298a","#ce1256","#980043","#67001f"),
    c("#a1d99b","#74c476","#41ab5d","#238b45","#006d2c","#00441b")  
    )
  
  
  # 1) Lecture complète
  df_full <- readRDS(data_rds)
  
  # 2) Construire la bande enveloppe (0.5% sup) et binning
  pts        <- unique(df_full[, c("FSCA","altitude")])
  alt_max_cut<- quantile(pts$altitude, 0.996)
  pts_trim   <- pts %>% filter(altitude <= alt_max_cut)
  
  breaks     <- seq(min(pts_trim$FSCA), max(pts_trim$FSCA),
                    length.out = nb_bins + 1)
  bin_data   <- lapply(seq_len(nb_bins), function(i) {
    sel <- pts_trim$FSCA >= breaks[i] & pts_trim$FSCA < breaks[i+1]
    sub <- pts_trim[sel, ]
    if (!nrow(sub)) return(NULL)
    data.frame(
      fs_mid  = mean(breaks[i:(i+1)]),
      alt_min = min(sub$altitude),
      alt_max = max(sub$altitude)
    )
  }) %>% bind_rows()
  band_df    <- crossing(bin_data, load_year = years_to_use)
  
  # 3) Préparer les points colorés
  df <- df_full %>%
    pivot_longer(
      cols          = starts_with("load_"),
      names_to      = c("load_year","load_parc"),
      names_pattern = "load_(\\d{4})_(.*)",
      values_to     = "load"
    ) %>%
    mutate(
      load_year   = as.integer(load_year),
      load_round  = round(load / 10) * 10,
      capped_load = pmin(pmax(load_round, 10), 1000)
    ) %>%
    filter(
      load_year %in% years_to_use,
      !is.na(load),
      load >= 10, load < 1000
    )
  
  # 4) Définir marges de dézoom
  f_min <- min(pts_trim$FSCA); f_max <- max(pts_trim$FSCA)
  f_pad <- 0.05 * (f_max - f_min)
  x_limits <- c(f_max + f_pad, f_min - f_pad)  # pour scale_x_reverse
  
  a_min <- min(pts_trim$altitude); a_max <- max(pts_trim$altitude)
  a_pad <- 0.05 * (a_max - a_min)
  y_limits <- c(a_min - a_pad, a_max + a_pad)
  
  # 5) Tracé de base + bande enveloppe en fond
  p <- ggplot() +
    geom_ribbon(
      data        = band_df,
      aes(x = fs_mid, ymin = alt_min, ymax = alt_max, group = load_year),
      fill        = "grey80",
      alpha       = 0.4,
      inherit.aes = FALSE
    ) +
    facet_wrap(~ load_year) +
    scale_x_reverse(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    labs(x = "FSCA", y = "Altitude (m)", title = alpage) +
    theme_bw(base_size = 14) +
    theme(
      strip.background = element_rect(fill = "grey90"),
      strip.text       = element_text(face = "bold")
    )
  
  # 6) Tri chrono des parc & suppression conditionnelle du dernier
  raw_ps <- unique(sub("^.*parc_", "", df$load_parc))
  raw_ps <- raw_ps[!is.na(sapply(raw_ps, get_start_date))]
  ps_ord <- raw_ps[order(sapply(raw_ps, get_start_date))]
  ps_use <- if (length(ps_ord) > 3) head(ps_ord, -1) else ps_ord
  
  # 7) Superposer les points colorés parc par parc
  for (i in seq_along(ps_use)) {
    ps        <- ps_use[i]
    pal       <- color_map[[(i-1) %% length(color_map) + 1]]
    label_ps  <- gsub("_", " à ", ps)
    full_lps  <- grep(paste0("parc_", ps, "$"), df$load_parc, value = TRUE)
    dfp       <- df %>% filter(load_parc %in% full_lps) %>% sample_frac(0.5)
    if (!nrow(dfp)) next
    
    p <- p +
      ggnewscale::new_scale_color() +
      geom_point(
        data = dfp,
        aes(x = FSCA, y = altitude, color = capped_load),
        size  = 1.5, alpha = 0.6
      ) +
      scale_color_gradient(
        low    = pal[1],
        high   = pal[6],
        limits = c(10, 1000),
        trans  = "log",
        oob    = scales::squish,
        name   = paste0("Parc : ", label_ps),
        breaks = c(10, 1000),
        labels = c("10", "<1000"),
        guide  = guide_colorbar(
          barwidth       = unit(1, "cm"),
          barheight      = unit(0.8, "cm"),
          direction      = "horizontal",
          label.position = "top",
          title.position = "bottom",
          title.hjust    = 0.5,
          order          = i
        )
      )
  }
  
  return(p)
}

# 8) Empilement de plusieurs alpages & export A4 portrait
plot_fsca_alti_points_parc <- function(alpages,
                                       data_dir,
                                       output_dir,
                                       years_to_use = c(2022, 2023, 2024),
                                       file_prefix  = "Plot_fsca_alti_points_parc",
                                       dpi          = 300) {
  plots <- vector("list", length(alpages))
  for (i in seq_along(alpages)) {
    alpage <- alpages[i]
    rds_f  <- file.path(
      data_dir,
      paste0("Use_Fsca_Alti_", alpage, "_by_raster_and_parc.rds")
    )
    p <- plot_fsca_alti_points_one_parc(alpage, rds_f, years_to_use)
    if (i < length(alpages)) p <- p + theme(legend.position = "none")
    plots[[i]] <- p
  }
  
  combined <- wrap_plots(plots, ncol = 1) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0(
        "Alpages : ", paste(alpages, collapse = ", "),
        " | Années : ", paste(years_to_use, collapse = ", ")
      ),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ) &
    theme(
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.spacing.x  = unit(0.3, "cm"),
      legend.key.width  = unit(0.8, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.text       = element_text(size = 7),
      legend.title      = element_text(size = 8)
    )
  
  # Sauvegarde JPG A4 portrait
  fname <- paste0(
    file_prefix, "_",
    paste(alpages, collapse = "_"),
    "_Years_", paste(years_to_use, collapse = "_"),
    ".jpg"
  )
  out <- file.path(output_dir, fname)
  ggsave(out, combined,
         device   = "jpg",
         width    = 8.27,
         height   = 11.69,
         units    = "in",
         dpi      = dpi)
  message("Plot saved to: ", out)
  return(combined)
}

































### 1) Fonction pour tracer le violin plot pour un seul alpage
plot_fsca_alti_violin_one <- function(alpage,
                                      data_rds,
                                      years_to_use = c(2022, 2023, 2024)) {
  # Charger les données et supprimer la colonne 'period' si elle existe
  data <- readRDS(data_rds)
  if ("period" %in% colnames(data)) {
    data_mod <- data %>% select(-period)
  } else {
    data_mod <- data
  }
 
  
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
plot_violin_fsca <- function(alpages,
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




