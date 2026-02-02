library(dplyr)
library(ggplot2)
library(factoextra)   
library(cluster)     
library(FactoMineR)
library(tidyr)
library(table1)
library(survival)
library(fmsb)
library(Hmisc)
library(ggradar)

dt <- dt_model
dt <- dt %>% mutate(GDP2015_4g = cut2(GDP2010_buffer005mean_100,g=4))
table(dt$GDP2015_4g,useNA = "ifany")

dt_gdp1 <- subset(dt,GDP2015_4g=="[0.00495,0.0796)")
dt_gdp2 <- subset(dt,GDP2015_4g=="[0.07963,0.1423)")
dt_gdp3 <- subset(dt,GDP2015_4g=="[0.14231,0.3238)")
dt_gdp4 <- subset(dt,GDP2015_4g=="[0.32378,3.4075]")


# PCA and cluster ----
do_pca_cluster <- function(data,
                           pca_var_groups,
                           n_components,
                           cluster_vars,
                           n_clusters = 4,
                           nstart = 25,
                           order_var) {
  
  # 1. Perform PCA on each variable group and extract scores ---
  all_scores <- list()
  all_loadings_plots <- list()
  
  for (group_name in names(pca_var_groups)) {
    # Select variables for the current PCA
    vars_for_pca <- pca_var_groups[[group_name]]
    ncp <- n_components[[group_name]]
    
    # Perform PCA
    pca_res <- PCA(data[, vars_for_pca], scale.unit = TRUE, ncp = ncp, graph = FALSE)
    
    # Extract scores and rename columns
    scores <- as.data.frame(pca_res$ind$coord)
    colnames(scores) <- paste0("PC", 1:ncol(scores))
    all_scores[[group_name]] <- scores
    
    # --- Extract loadings ---
    loadings_fm <- as.data.frame(pca_res$var$coord)
    loadings_fm$Variable <- rownames(loadings_fm)
    
    loadings_long <- loadings_fm %>%
      pivot_longer(cols = -Variable, names_to = "PC", values_to = "Loading")
    
    # --- Build loading plot ---
    loadings_plot <- ggplot(loadings_long, aes(x = reorder(Variable, Loading), 
                                               y = Loading, 
                                               fill = Loading > 0)) +
      geom_bar(stat = "identity") +
      facet_wrap(~PC, scales = "free_y") +
      coord_flip() +
      scale_fill_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
      labs(title = paste("Variable Loadings -", group_name),
           x = "", y = "Loading", fill = "Direction") +
      theme_minimal()
    
    all_loadings_plots[[group_name]] <- loadings_plot
  }
  
  # Combine scores from all PCA runs
  combined_scores <- do.call(cbind, all_scores)
  
  # Add the new PCA scores to the original data
  data_with_scores <- cbind(data, combined_scores)
  
  # 2. Run k-means clustering ---
  km <- kmeans(data_with_scores[, cluster_vars], 
               centers = n_clusters, 
               nstart = nstart, 
               iter.max = 1e6)  
  data_with_scores$cluster <- km$cluster
  
  # 3. Reorder cluster numbers ---
  cluster_means <- data_with_scores %>%
    group_by(cluster) %>%
    summarise(mean_val = mean(.data[[order_var]], na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(mean_val)) %>%
    mutate(new_cluster = row_number())
  
  final_data <- data_with_scores %>%
    left_join(cluster_means %>% select(cluster, new_cluster), by = "cluster") %>%
    mutate(cluster = factor(new_cluster)) %>%
    select(-new_cluster)
  
  #  4. Return results ---
  return(list(
    data = final_data, 
    km = km, 
    pca_scores = combined_scores,
    loading_plots = all_loadings_plots 
  ))
}

AP_neg <- c("PM25_lastyear_10_neg","NO2_lastyear_ugm3_10_neg")
Eco <- c("GDP2010_buffer005mean_100","Nighttime_light_10")
cisi_col <- c("Infras_CISI", "Infras_energy","Infras_transportation","Infras_water",
              "Infras_waste","Infras_telecommunication","Infras_healthcare","Infras_education")
Soc <- c(cisi_col,"Road_lastyear_10k")

var_groups_for_pca <- list(
  ap_neg = AP_neg,
  eco = Eco,
  soc = Soc
)

num_components_to_keep <- c(
  ap_neg = 2,
  eco = 2,
  soc = 5
)

vars_for_clustering <- c(
  "ap_neg.PC1", 
  "eco.PC1", #"GDP2015_buffer005mean_100", 
  "soc.PC1", 
  "soc.PC2",
  "NDVI_lastyear_gt0_01" # This is original variable, not a PCA score
)

plot_radar_clusters_ggplot_final <- function(data, cluster_var = "cluster", radar_vars, dataset_name,
                                             cluster_colors = NULL, var_labels = NULL) {
  cluster_means <- data %>%
    group_by(.data[[cluster_var]]) %>%
    summarise(across(all_of(radar_vars), mean, na.rm = TRUE)) %>%
    as.data.frame()
  
  if (!is.null(var_labels)) {
    colnames(cluster_means)[-1] <- var_labels
    radar_vars <- var_labels
  }
  
  # Scale 0-1
  cluster_means_norm <- cluster_means
  for (v in radar_vars) {
    rng <- range(cluster_means[[v]], na.rm = TRUE)
    cluster_means_norm[[v]] <- if(diff(rng)==0) 0.5 else (cluster_means[[v]] - rng[1])/diff(rng)
  }
  
  # Factor for legend consistency
  cluster_levels <- paste0("Cluster ", cluster_means[[cluster_var]])
  cluster_means_norm[[cluster_var]] <- factor(cluster_levels, levels = cluster_levels)
  cluster_means_norm <- cluster_means_norm %>% rename(group = all_of(cluster_var))
  
  if (is.null(cluster_colors)) cluster_colors <- scales::hue_pal()(nrow(cluster_means_norm))
  names(cluster_colors) <- cluster_means_norm$group
  
  ggradar(
    cluster_means_norm,
    group.colours = cluster_colors,
    grid.min = 0,
    grid.mid = 0.5,
    grid.max = 1,
    grid.label.size = 4,
    axis.label.size = 4,
    group.line.width = 0.8,
    group.point.size = 2,
    plot.title = paste("Radar Plot -", dataset_name),
    legend.position = "none"  
  ) + theme(text = element_text(size = 12))
}

extract_hr <- function(data, cluster_var = "cluster", ref_cluster = "4") {
  # Run cox model
  fml <- as.formula(paste0(
    "Surv(survTime_year, event) ~ relevel(factor(", cluster_var, "), ref='", ref_cluster, "') +
    age + Gender + Education + Marriage + Regular_exercise_2 + Smoking_2 + Drinking_2 +
    strata(prov)"
  ))
  
  fit <- coxph(fml, data = data)
  hr <- summary(fit)$coefficients
  ci <- summary(fit)$conf.int
  pvals <- hr[, "Pr(>|z|)"]
  
  res <- data.frame(
    term = rownames(ci),
    HR   = ci[, "exp(coef)"],
    lower= ci[, "lower .95"],
    upper= ci[, "upper .95"],
    pval = pvals
  )
  
  res <- res[grepl(cluster_var, res$term), ]
  rownames(res) <- NULL
  return(res)
}

plot_forest_clusters <- function(hr_data, cluster_var = "cluster",
                                 ref_cluster = "4",
                                 cluster_colors = NULL,
                                 dataset_name = "",
                                 file_out = NULL) {
  library(ggplot2)
  
  # Extract cluster numbers from term
  hr_data$cluster_num <- sub(".*\\)([0-9]+)$", "\\1", hr_data$term)
  
  # Add reference cluster row
  ref_row <- data.frame(
    term = paste0(cluster_var, ref_cluster),
    HR = 1, lower = NA, upper = NA, pval = NA,
    cluster_num = ref_cluster,
    stringsAsFactors = FALSE
  )
  
  hr_plotdata <- rbind(ref_row, hr_data)
  
  # Create consistent factor levels for the color aesthetic
  cluster_levels_aes <- paste0("Cluster ", sort(unique(as.numeric(hr_plotdata$cluster_num))))
  hr_plotdata$cluster <- factor(paste0("Cluster ", hr_plotdata$cluster_num), levels = cluster_levels_aes)
  
  # Create the labels for the x-axis ticks separately
  x_axis_labels <- cluster_levels_aes
  ref_index <- which(cluster_levels_aes == paste0("Cluster ", ref_cluster))
  x_axis_labels[ref_index] <- paste0(x_axis_labels[ref_index], " (ref)")
  
  # Name the colors vector to match the new levels
  if (!is.null(cluster_colors)) {
    names(cluster_colors) <- cluster_levels_aes
  }
  
  # Plot
  p <- ggplot(hr_plotdata, aes(x = cluster, y = HR, color = cluster)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, na.rm = TRUE) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    
    # Apply the custom x-axis labels and name the scale
    scale_x_discrete(labels = x_axis_labels) +
    scale_color_manual(name = "Cluster", values = cluster_colors) +
    
    labs(title = paste("Forest Plot - ", dataset_name),
         y = "HR (95% CI)", x = NULL) +
    theme_minimal(base_size = 14) 
  
  if (!is.null(file_out)) {
    ggsave(file_out, p, width = 6, height = 5, dpi = 300)
  }
  
  return(p)
}

colors_fixed <- c("darkgreen","blue","orange","red")
pac_var_labels <- c("Air quality", 
                    "Economy", 
                    "General\ninfrastructure", 
                    "Energy\ninfrastructure", 
                    "NDVI")

# store loadings-----
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
summary(dt[,c("PM25_lastyear_10_neg",     "NO2_lastyear_ugm3_10_neg",
      "GDP2010_buffer005mean_100","Nighttime_light_10",
"Infras_CISI",              "Infras_energy",            "Infras_transportation",   
"Infras_water",             "Infras_waste",             "Infras_telecommunication",
"Infras_healthcare",        "Infras_education",         "Road_lastyear_10k" )])

all_loadings <- lapply(names(var_groups_for_pca), function(group_name) {
  vars <- var_groups_for_pca[[group_name]]
  ncp  <- num_components_to_keep[[group_name]]
  
  pca_res <- PCA(dt[, vars], scale.unit = TRUE, ncp = ncp, graph = FALSE)
  loadings <- as.data.frame(pca_res$var$coord)
  loadings$variable <- rownames(loadings)
  loadings$group <- group_name
  return(loadings)
})

all_loadings <- bind_rows(all_loadings)

ndvi_load <- data.frame(
  Dim.1 = 1,   # assign loading
  Dim.2 = 0,
  variable = "NDVI_lastyear_gt0_01",
  group = "greenness"
)

all_loadings <- bind_rows(all_loadings, ndvi_load)

other_term_titles <- c(
  "GDP2010_buffer005mean_100" = "GDP",
  "Nighttime_light_10" = "Nighttime light",
  "Pop_lastyear_100" = "Population density",
  "PM25_lastyear_10_neg" = "Negative PM2.5",
  "NO2_lastyear_ugm3_10_neg" = "Negative NO2",
  "CH4_lastyear_neg" = "Negative CH4",
  "NDVI_lastyear_gt0_01" = "NDVI",
  "Road_lastyear_10k" = "Road density",
  "CISI" = "CISI",
  "Subscore_transportation" = "Transportation",
  "Subscore_healthcare" = "Healthcare",
  "Subscore_education" = "Education",
  "Subscore_energy" = "Energy",
  "Subscore_telecommunication" = "Telecommunication",
  "Subscore_waste" = "Waste",
  "Subscore_water" = "Water"
)

# Replace raw variable names with human-friendly titles
all_loadings$label <- other_term_titles[all_loadings$variable]

all_loadings$domain <- case_when(
  all_loadings$variable %in% c("PM25_lastyear_10_neg","NO2_lastyear_ugm3_10_neg") ~ "Air quality",
  all_loadings$variable == "NDVI_lastyear_gt0_01" ~ "Greenness",
  all_loadings$variable %in% c("GDP2010_buffer005mean_100","Nighttime_light_10") ~ "Economic",
  TRUE ~ "Infrastructure"
)
domain_colors <- c(
  "Air quality" = "blue",
  "Economic" = "purple",
  "Infrastructure" = "orange",
  "Greenness" = "darkgreen"
)

loadings_plot <- ggplot(all_loadings, aes(x = Dim.1, y = Dim.2, color = domain, label = label)) +
  geom_segment(aes(xend = 0, yend = 0), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(hjust = 0.5, vjust = -0.5, size = 3.5) +
  scale_color_manual(values = domain_colors) +
  theme_minimal() +
  labs(title = "Loadings Plot",
       x = "PC1", y = "PC2", color = "Domain")
loadings_plot
ggsave("figure/loadings_plot.png",
       loadings_plot,
       dpi=300
)

# all data ----
colnames(dt)
cluster_alldt <- do_pca_cluster(
  data = dt,
  pca_var_groups = var_groups_for_pca,
  n_components = num_components_to_keep,
  cluster_vars = vars_for_clustering,
  n_clusters = 4,
  nstart = 25,
  order_var = "eco.PC1" # Reorder clusters based on the first economic PC
)
cluster_alldtdt <- cluster_alldt$data
table1(~ ap_neg.PC1+eco.PC1+soc.PC1+soc.PC2+
         PM25_lastyear_10+NDVI_lastyear_gt0_01+Road_lastyear_10k+GDP2010_buffer005mean_100|cluster,
       cluster_alldtdt)

radar_alldtdt <- plot_radar_clusters_ggplot_final(cluster_alldtdt,
                                                  cluster_var = "cluster",
                                                  radar_vars = vars_for_clustering,
                                                  dataset_name = "Environmental Profiles (Relative Score: 0-100)",
                                                  cluster_colors = colors_fixed,
                                                  var_labels = pac_var_labels)
radar_alldtdt
hr_alldtdt <- extract_hr(cluster_alldtdt, cluster_var = "cluster", ref_cluster = "4")
forest_alldtdt <- plot_forest_clusters(hr_alldtdt,
                                       cluster_var = "cluster",
                                       ref_cluster = "4",
                                       cluster_colors = colors_fixed,
                                       dataset_name = "Association with Mortality")
(radar_alldtdt | forest_alldtdt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 

# Add percentile table ----
library(dplyr)
library(gridExtra)

# 2. Calculate Mean Percentiles
cluster_stats <- cluster_alldtdt %>%
  # Rank every row from 1 to 100 for each variable
  mutate(across(all_of(vars_for_clustering), ~ ntile(., 100))) %>% 
  
  # Group by Cluster and calculate the average rank
  group_by(cluster) %>%
  summarise(across(all_of(vars_for_clustering), \(x) round(mean(x, na.rm = TRUE), 1))) %>%
  
  # Rename columns to be readable
  setNames(c("Cluster", pac_var_labels)) %>%
  mutate(Cluster = paste("Cluster", Cluster))
View(cluster_stats)


relative_table <- cluster_stats %>%
  mutate(across(-Cluster, function(x) {
    # Formula: (Value - Min) / (Range) * 100 - forces the Lowest cluster to 0 and Highest to 100
    rng <- range(x, na.rm = TRUE)
    scaled <- (x - rng[1]) / (rng[2] - rng[1])
    return(round(scaled * 100, 1))
  }))

library(gridExtra)
library(grid)

colors_fixed <- c("darkgreen", "blue", "orange", "red")

# Create a Matrix of Text Colors Start with all "black"
text_color_mat <- matrix("black", nrow = nrow(cluster_stats), ncol = ncol(cluster_stats))

# Change the FIRST column (Cluster names) to specific colors
text_color_mat[, 1] <- colors_fixed

# Create a Custom Table Theme
color_table_theme <- ttheme_minimal(
  base_size = 11,
  core = list(
    fg_params = list(
      col = text_color_mat,  
      hjust = 0.5           
    ),
    padding = unit(c(4, 4), "mm")
  ),
  colhead = list(
    fg_params = list(fontface = "bold", col = "black")
  )
)

# 4. Generate the Table Grob
table_colored <- tableGrob(relative_table, rows = NULL, theme = color_table_theme)

library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(patchwork)
# --- Add the Table Lines ---
scientific_theme <- ttheme_default(
  core = list(
    fg_params = list(col = text_color_mat, hjust = 0.5, fontsize = 10),
    bg_params = list(fill = "white", col = NA), 
    padding = unit(c(4, 4), "mm")
  ),
  colhead = list(
    fg_params = list(col = "black", fontface = "bold", hjust = 0.5, fontsize = 11),
    bg_params = list(fill = "white", col = NA),
    padding = unit(c(4, 4), "mm")
  )
)

t1 <- tableGrob(relative_table, rows = NULL, theme = scientific_theme)

add_table_lines <- function(table_grob) {
  nr <- nrow(table_grob)
  nc <- ncol(table_grob)
  
  # 1. Top Line (Thick) - At the TOP of the Header 
  table_grob <- gtable_add_grob(table_grob,
                                grobs = segmentsGrob(y0 = unit(1,"npc"), y1 = unit(1,"npc"), 
                                                     gp = gpar(lwd = 2)),
                                t = 1, b = 1, l = 1, r = nc)
  
  # 2. Middle Line (Thin) - Draw at the TOP of the First Data Row
  table_grob <- gtable_add_grob(table_grob,
                                grobs = segmentsGrob(y0 = unit(1,"npc"), y1 = unit(1,"npc"), 
                                                     gp = gpar(lwd = 1)),
                                t = 2, b = 2, l = 1, r = nc)
  
  # 3. Bottom Line (Thick) - At the BOTTOM of the Last Row
  table_grob <- gtable_add_grob(table_grob,
                                grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), 
                                                     gp = gpar(lwd = 2)),
                                t = nr, b = nr, l = 1, r = nc)
  return(table_grob)
}

table_final <- add_table_lines(t1)

# --- Titles and Spacing ---
title_theme <- theme(
  plot.title = element_text(size = 16, face = "bold", hjust = 0),
  plot.margin = margin(t = 10, r = 10, b = 0, l = 10) # Reduced bottom margin for titles
)

radar_final <- radar_alldtdt + 
  labs(title = "A. Environmental Profiles (Relative Score: 0-100)") +
  theme(legend.position = "none") +
  title_theme + coord_equal(clip = "off")

forest_clean <- forest_alldtdt + 
  labs(title = NULL) + # No title on the inner plot
  theme(legend.position = "none", plot.margin = margin(0,0,0,0))

bottom_row_inner <- plot_spacer() + forest_clean + plot_spacer() + 
  plot_layout(widths = c(0.5, 5, 0.5))

# ---Add Title B to the Full Width Row ---
# We wrap the inner row so it acts like one big plot, then add the title to IT.
bottom_row_titled <- wrap_elements(bottom_row_inner) + 
  labs(title = "B. Environmental Profiles and Mortality (Hazard Ratio)") +
  title_theme

# --- Final Assembly ---
layout_3rows <- radar_final / 
  wrap_elements(table_final) / 
  plot_spacer() / 
  bottom_row_titled + 
  # Adjust heights: Radar, Table, Spacer, BottomRow
  plot_layout(heights = c(3, 1.2, 0.2, 3))
layout_3rows

# Save
# View
ggsave("figure/radar_forest_3.png",
       layout_3rows,
       dpi=300
)



# GDP 1 ----
cluster_gdp1 <- do_pca_cluster(
  data = dt_gdp1,
  pca_var_groups = var_groups_for_pca,
  n_components = num_components_to_keep,
  cluster_vars = vars_for_clustering,
  n_clusters = 4,
  nstart = 25,
  order_var = "eco.PC1"
)
cluster_gdp1dt <- cluster_gdp1$data
table1(~ ap_neg.PC1+eco.PC1+soc.PC1+soc.PC2+
         PM25_lastyear_10+NDVI_lastyear_gt0_01+Road_lastyear_10k+GDP2010_buffer005mean_100|cluster,
       cluster_gdp1dt)

radar_gdp1dt <- plot_radar_clusters_ggplot_final(cluster_gdp1dt,
                                                 cluster_var = "cluster",
                                                 radar_vars = vars_for_clustering,
                                                 dataset_name = "GDP Q1",
                                                 cluster_colors = colors_fixed,
                                                 var_labels = pac_var_labels)
hr_gdp1dt <- extract_hr(cluster_gdp1dt, cluster_var = "cluster", ref_cluster = "4")
forest_gdp1dt <- plot_forest_clusters(hr_gdp1dt,
                                      cluster_var = "cluster",
                                      ref_cluster = "4",
                                      cluster_colors = colors_fixed,
                                      dataset_name = "GDP Q1")

(radar_gdp1dt | forest_gdp1dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 


# GDP 2 ----
cluster_gdp2 <- do_pca_cluster(
  data = dt_gdp2,
  pca_var_groups = var_groups_for_pca,
  n_components = num_components_to_keep,
  cluster_vars = vars_for_clustering,
  n_clusters = 4,
  nstart = 25,
  order_var = "eco.PC1" 
)
cluster_gdp2dt <- cluster_gdp2$data
table1(~ ap_neg.PC1+eco.PC1+soc.PC1+soc.PC2+
         PM25_lastyear_10+NDVI_lastyear_gt0_01+Road_lastyear_10k+GDP2010_buffer005mean_100|cluster,
       cluster_gdp2dt)

radar_gdp2dt <- plot_radar_clusters_ggplot_final(cluster_gdp2dt,
                                                 cluster_var = "cluster",
                                                 radar_vars = vars_for_clustering,
                                                 dataset_name = "GDP Q2",
                                                 cluster_colors = colors_fixed,
                                                 var_labels = pac_var_labels)
hr_gdp2dt <- extract_hr(cluster_gdp2dt, cluster_var = "cluster", ref_cluster = "4")
forest_gdp2dt <- plot_forest_clusters(hr_gdp2dt,
                                      cluster_var = "cluster",
                                      ref_cluster = "4",
                                      cluster_colors = colors_fixed,
                                      dataset_name = "GDP Q2")

(radar_gdp2dt | forest_gdp2dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 

# GDP 3 ----
cluster_gdp3 <- do_pca_cluster(
  data = dt_gdp3,
  pca_var_groups = var_groups_for_pca,
  n_components = num_components_to_keep,
  cluster_vars = vars_for_clustering,
  n_clusters = 4,
  nstart = 25,
  order_var = "eco.PC1" 
)
cluster_gdp3dt <- cluster_gdp3$data
table1(~ ap_neg.PC1+eco.PC1+soc.PC1+soc.PC2+
         PM25_lastyear_10+NDVI_lastyear_gt0_01+Road_lastyear_10k+GDP2010_buffer005mean_100|cluster,cluster_gdp3dt)

radar_gdp3dt <- plot_radar_clusters_ggplot_final(cluster_gdp3dt,
                                                 cluster_var = "cluster",
                                                 radar_vars = vars_for_clustering,
                                                 dataset_name = "GDP Q3",
                                                 cluster_colors = colors_fixed,
                                                 var_labels = pac_var_labels)
hr_gdp3dt <- extract_hr(cluster_gdp3dt, cluster_var = "cluster", ref_cluster = "4")
forest_gdp3dt <- plot_forest_clusters(hr_gdp3dt,
                                      cluster_var = "cluster",
                                      ref_cluster = "4",
                                      cluster_colors = colors_fixed,
                                      dataset_name = "GDP Q3")

(radar_gdp3dt | forest_gdp3dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 



# GDP 4 ----
cluster_gdp4 <- do_pca_cluster(
  data = dt_gdp4,
  pca_var_groups = var_groups_for_pca,
  n_components = num_components_to_keep,
  cluster_vars = vars_for_clustering,
  n_clusters = 4,
  nstart = 25,
  order_var = "eco.PC1" 
)
cluster_gdp4dt <- cluster_gdp4$data
table1(~ ap_neg.PC1+eco.PC1+soc.PC1+soc.PC2+
         PM25_lastyear_10+NDVI_lastyear_gt0_01+Road_lastyear_10k+GDP2010_buffer005mean_100|cluster,cluster_gdp4dt)

radar_gdp4dt <- plot_radar_clusters_ggplot_final(cluster_gdp4dt,
                                                 cluster_var = "cluster",
                                                 radar_vars = vars_for_clustering,
                                                 dataset_name = "GDP Q4",
                                                 cluster_colors = colors_fixed,
                                                 var_labels = pac_var_labels)
hr_gdp4dt <- extract_hr(cluster_gdp4dt, cluster_var = "cluster", ref_cluster = "4")
forest_gdp4dt <- plot_forest_clusters(hr_gdp4dt,
                                      cluster_var = "cluster",
                                      ref_cluster = "4",
                                      cluster_colors = colors_fixed,
                                      dataset_name = "GDP Q4")

(radar_gdp4dt | forest_gdp4dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 

plot_gdp4 <- (radar_gdp4dt | forest_gdp4dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 
plot_gdp1 <- (radar_gdp1dt | forest_gdp1dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 
plot_gdp2 <- (radar_gdp2dt | forest_gdp2dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 
plot_gdp3 <- (radar_gdp3dt | forest_gdp3dt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 
plot_alldt <- (radar_alldtdt | forest_alldtdt) +
  plot_layout(guides = "collect", widths = c(1.7, 1)) 

plot_5dt <- (
  plot_alldt / 
    plot_gdp1 / 
    plot_gdp2 / 
    plot_gdp3 / 
    plot_gdp4 
) + plot_layout(guides = "collect")

plot_5dt <- plot_5dt & theme(plot.margin = margin(5, 40, 5, 40))
plot_5dt <- plot_5dt & coord_cartesian(clip = "off")

plot_5dt <- plot_5dt & theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)
plot_5dt
ggsave("figure/clusterHR.png",plot_5dt,dpi=300,
       height = 17, width=11)

# save the total 
plot_alldt <- (radar_alldtdt | forest_alldtdt) +
  plot_layout(guides = "collect", widths = c(2, 1)) &
  theme(legend.position = "bottom", legend.box = "vertical",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
plot_alldt
ggsave("figure/clusterHR_overall.png",plot_alldt,dpi=300,
       height = 4, width=10)


# code double check ----
pca_APneg_gdp2 <- PCA(dt_gdp2[,AP_neg], scale.unit = TRUE, ncp = 2, graph = FALSE)
pca_eco_gdp2 <- PCA(dt_gdp2[,Eco], scale.unit = TRUE, ncp = 2, graph = FALSE)
pca_soc_gdp2 <- PCA(dt_gdp2[,Soc], scale.unit = TRUE, ncp = 5, graph = FALSE)
pca_APneg_gdp2$eig
pca_eco_gdp2$eig
pca_soc_gdp2$eig

# PCA score
pc_APneg_scores_gdp2 <- as.data.frame(pca_APneg_gdp2$ind$coord)
pc_eco_scores_gdp2 <- as.data.frame(pca_eco_gdp2$ind$coord)
pc_soc_scores_gdp2 <- as.data.frame(pca_soc_gdp2$ind$coord)

# Rename PC score columns
colnames(pc_APneg_scores_gdp2) <- c("PC1_ap_neg_gdp2", "PC2_ap_neg_gdp2")
colnames(pc_eco_scores_gdp2) <- c("PC1_eco_gdp2", "PC2_eco_gdp2")
colnames(pc_soc_scores_gdp2) <- c("PC1_soc_gdp2", "PC2_soc_gdp2", "PC3_soc_gdp2", "PC4_soc_gdp2","PC5_soc_gdp2")

# Merge with original dataset
pc_scores_gdp2 <- cbind(pc_APneg_scores_gdp2, pc_eco_scores_gdp2, pc_soc_scores_gdp2)
colnames(dt_gdp2)
dt_gdp2 <- cbind(dt_gdp2,pc_scores_gdp2)
summary(dt_gdp2$PC1_ap_neg_gdp2)

km_gdp2 <- kmeans(dt_gdp2[,c("PC1_ap_neg_gdp2","PC1_eco_gdp2","PC1_soc_gdp2","PC2_soc_gdp2","NDVI_lastyear_gt0_01")], 
                  centers = 4, nstart = 25)
dt_gdp2$cluster_gdp2 <- NA
dt_gdp2$cluster_gdp2 <- km_gdp2$cluster
# description
table1(~ PC1_ap_neg_gdp2+PC1_eco_gdp2+PC1_soc_gdp2+PC2_soc_gdp2+
         NDVI_lastyear_gt0_01+
         Road_lastyear_10k+GDP2010_buffer005mean_100+Pop_lastyear_100+Nighttime_light_10+
         CISI+Subscore_energy|cluster_gdp2,dt_gdp2)

library(fmsb)
# Prepare data for radar: rescale 0–1
vars_compare <- c("PC1_ap_neg_gdp2","PC1_eco_gdp2","PC1_soc_gdp2","PC2_soc_gdp2","NDVI_lastyear_gt0_01")
radar_data_gdp2 <- dt_gdp2 %>%
  group_by(cluster_gdp2) %>%
  summarise(across(all_of(vars_compare), mean, na.rm = TRUE))

# fmsb requires max/min rows
radar_data_gdp2_scaled <- as.data.frame(scale(radar_data_gdp2[,-1]))
radar_data_gdp2_scaled <- rbind(apply(radar_data_gdp2_scaled, 2, max),
                                apply(radar_data_gdp2_scaled, 2, min),
                                radar_data_gdp2_scaled)
radar_data_gdp2_scaled$cluster_gdp2 <- c("max","min", radar_data_gdp2$cluster_gdp2)
# Plot
radarchart(radar_data_gdp2_scaled[,-ncol(radar_data_gdp2_scaled)],
           axistype=1,
           pcol=c("red","blue","green","purple"),
           plwd=2, plty=1,
           title="Cluster profiles")
legend("bottomright", legend=paste("Cluster", 1:4),
       col=c("red","blue","green","purple"), lwd=2)

summary(coxph(Surv(survTime_year,event) ~ relevel(factor(cluster_gdp2),ref ="2")+
                age+Gender+Education+Marriage+ Regular_exercise_2 + Smoking_2 +Drinking_2+
                strata(prov),
              data=dt_gdp2))

