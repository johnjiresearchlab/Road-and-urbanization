setwd("/Users/linxin/202201/a_CLHLS/m_urbanization/")
load("~/202201/a_CLHLS/m_urbanization/env_model.RData")
library(dplyr)
library(survival)
library(broom)
library(ggplot2)
library(patchwork)
library(table1)

table1(~ coast_dist_cat + river_dist_cat, dt4_adl_mmse_ch4_tc)
# load water 
load("cluster_alldt_water.rdata")
dt4_adl_mmse_ch4_tc <- merge(dt4_adl_mmse_ch4_tc,
      cluster_alldt_water[,c("id","dist_to_coast_km","dist_to_river_km","coast_dist_cat","river_dist_cat")],
      by="id",
      all.x=T)

dt4_adl_mmse_ch4_tc <- merge(dt4_adl_mmse_ch4_tc,
                             cluster_alldt_water[,c("id","ap_infra_9g_label","ap_infra_9g")],
                             by="id",
                             all.x=T)
table1(~dist_to_coast_km|coast_dist_cat,cluster_alldt_water)
table1(~dist_to_river_km|river_dist_cat,cluster_alldt_water)

dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>% mutate(near_coast_100km = ifelse(dist_to_coast_km<100,1,0),
                                                      near_river1j_20km = ifelse(dist_to_river_km<20,1,0))
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>% mutate(dist_to_coast_km_neg = -dist_to_coast_km,
                                                      dist_to_river_km_neg = -dist_to_river_km)
library(janitor)
cluster_alldt_water %>%
  tabyl(coast_dist_cat, ap_infra_9g_label) %>%             # two-way table
  adorn_percentages("row") %>%                    # row-wise %
  adorn_pct_formatting(digits = 1) %>%            # e.g., 12.3%
  adorn_ns() %>% View()   

# add cooking fuel ----
dt4_adl_mmse_ch4_tc <- merge(dt4_adl_mmse_ch4_tc,
             ori_longit11[,c("id","a537")],
             by="id",
             all.x=T)
table(dt4_adl_mmse_ch4_tc$a537,useNA = "ifany")

# eco cols
other_col
cisi_terms
cisi100_col <- c("Infras_CISI100", "Infras_energy100","Infras_transportation100","Infras_water100",
                 "Infras_waste100","Infras_telecommunication100","Infras_healthcare100","Infras_education100")

Eco_col <- c("GDP2010_buffer005mean_100","Nighttime_light","Pop_lastyear_100")
Soc_col <- c("Road_lastyear_10k",
             "Infras_CISI100","Infras_transportation100","Infras_energy100",
             "Infras_healthcare100_nonzero","Infras_healthcare100_log",
             "Infras_education100_nonzero","Infras_education100_log",
             "Infras_telecommunication100_nonzero","Infras_telecommunication100_log",
             "Infras_water100_nonzero","Infras_water100_log",
             "Infras_waste100_nonzero","Infras_waste100_log"
)
Nat_col <- c("PM25_lastyear_10_neg","NO2_lastyear_ugm3_10_neg","NDVI_lastyear_gt0_01") 
# "CH4_lastyear_100_neg" "near_coast_100km","near_river1j_20km"

# cox model
cox_f <- function(x1,x2=NULL,cov,dataset,category){
  f <- reformulate(c(x1,x2,cov),
                   response="Surv(survTime_year,event)")
  coxfit <- coxph(f,
                  data=dataset)
  library(broom)
  HR <- coxfit %>%
    tidy(conf.int=T) %>%
    mutate(
      estimate=exp(estimate),
      conf.low=exp(conf.low),
      conf.high=exp(conf.high)
    ) %>%
    dplyr::select(term, estimate, starts_with("conf"),p.value) %>%  head(category)
  return(data.frame(HR))
}
summary(dt4_adl_mmse_ch4_tc$NO2_lastyear_ugm3_10)
summary(chap_no2_2011_final$CHAP_NO2_2011)

summary(coxph(Surv(survTime_year,event) ~ Road_lastyear_10k + NO2_chap_lastyear_10 +
                GDP2010_buffer005mean_100+Pop_lastyear_100 + Elevation_100 + 
                age + Gender + 
                Education + Marriage + Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                Residence + strata(prov_lab),
              data=dt4_adl_mmse_ch4_tc))
summary(coxph(Surv(survTime_year,event) ~ Road_lastyear_10k + NO2_lastyear_ugm3_10 +
                GDP2010_buffer005mean_100+Pop_lastyear_100 + Elevation_100 + 
                age + Gender + 
                Education + Marriage + Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                Residence + strata(prov_lab),
              data=dt4_adl_mmse_ch4_tc))
# "strata(prov_lab)","Residence",
# "age","Gender","Education","Marriage",
# "Regular_exercise_2","Smoking_2","Drinking_2",
# "GDP2010_buffer005mean_100","Pop_lastyear_100",
# "Road_lastyear_10k",
# "PM25_lastyear_10","NO2_lastyear_ugm3_10"

# Assign colors to all Model categories
# model_colors <- c(
#   "A Unadjusted" = "#1f77b4",
#   "B:A+Demography+Lifestyle" = "#ff7f0e",
#   "C:B+Climate" = "#2ca02c",
#   "D:C+Economy" = "#d62728",
#   "E:D+AP (no road)" = "#9467bd",
#   "F:D+Road (no AP)" = "#8c564b",
#   "G:D+AP+Road" = "#e377c2",
#   "H:G+Adl+Cognition"="#17BECF"
# )

model_colors <- c(
  "A Unadjusted" = "#1f77b4",
  "B:A+Demography and Lifestyle" = "#ff7f0e",
  "C:B+Health" = "#2ca02c",
  "D:C+Climate and Geography" = "#d62728",
  "E:D+Economy" = "#9467bd",
  "F:E+AP (no road)" = "#8c564b",
  "G:F+Road (no AP)" = "#e377c2",
  "H:G+AP+Road"="#17BECF"
)
table(model_satu_2$term)
table(infra_model_satu$term)

cov_demo <- c("age","Gender","Education","Marriage",
              "Regular_exercise_2","Smoking_2","Drinking_2")
cov_climate2 <- c("strata(prov_lab)","Elevation",
                 "Temp_sd_lastyear_1c","Temp_mean_lastyear_1c",
                 "coast_dist_cat","river_dist_cat")
cov_eco <- c("Residence","GDP2010_buffer005mean_100","Nighttime_light","Pop_lastyear_100")
cov_ap <- c("PM25_lastyear_10","NO2_lastyear_ugm3_10")
cov_rd <- "Road_lastyear_10k"
cov_adlmmse <- c("adl_2","CognitiveImpairment")
summary(dt4_adl_mmse_ch4_tc$GDP2010_buffer005mean_100)
# model ----
model_satu_2 <- bind_rows(bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                               cox_f,
                                               x2=NULL,
                                               cov=NULL,
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="A Unadjusted"),
                              bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                               cox_f,
                                               x2=NULL,
                                               cov=cov_demo,
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="B:A+Demography and Lifestyle"),
                              bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                          cox_f,
                                          x2=NULL,
                                          cov=c(cov_demo,cov_adlmmse),
                                          dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="C:B+Health"),
                              bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_adlmmse,cov_climate2),                           
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="D:C+Climate and Geography"),
                              bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_adlmmse,cov_climate2,cov_eco),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="E:D+Economy"),
                              bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_adlmmse,cov_climate2,cov_eco,cov_ap
                                               ),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="F:E+AP (no road)"),
                              bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_adlmmse,cov_climate2,cov_eco,cov_rd),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="G:F+Road (no AP)"),
                              bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_adlmmse,cov_climate2,cov_eco,
                                                     cov_ap,cov_rd),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="H:G+AP+Road")
)

model_satu <- model_satu %>% mutate(sig = ifelse(p.value < 0.05, "*", "")) 
model_satu_2 <- model_satu_2 %>% mutate(sig = ifelse(p.value < 0.05, "*", "")) 

write.csv(model_satu,"model_satu.csv")

model_satuD2 <- bind_rows(lapply(c(Soc_col,Eco_col,Nat_col),
                 cox_f,
                 x2=NULL,
                 cov=c(cov_demo,cov_climate,cov_eco,cov_adlmmse),
                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% 
  mutate(Model="D2:C+Economy",sig = ifelse(p.value < 0.05, "*", ""))

# Sco -----
infra_model_satu <- model_satu_2 %>% filter(term %in% c(Soc_col,
                                                        "Infras_education100_nonzeroPresence","Infras_healthcare100_nonzeroPresence"))

infra_model_satu$term <- factor(infra_model_satu$term,
                                levels = Soc_col)
infra_term_titles <- c(
  "Road_lastyear_10k" = "Road density",
  "Infras_CISI100" = "CISI",
  "Infras_transportation100" = "Transportation infrastructure",
  "Infras_energy100" = "Energy infrastructure",
  
  "Infras_healthcare100_nonzeroPresence" = "Healthcare infrastructure\n(Nonzero vs Zero)",
  "Infras_healthcare100_log" = "Healthcare infrastructure\n(log-scale)",
  
  "Infras_education100_nonzeroPresence" = "Education infrastructure\n(Nonzero vs Zero)",
  "Infras_education100_log" = "Education infrastructure\n(log-scale)",
  
  "Infras_telecommunication100_nonzero" = "Telecommunication infrastructure\n(Nonzero vs Zero)",
  "Infras_telecommunication100_log" = "Telecommunication infrastructure\n(log-scale)",
  
  "Infras_waste100_nonzero" = "Waste infrastructure\n(Nonzero vs Zero)",
  "Infras_waste100_log" = "Waste infrastructure\n(log-scale)",
  
  "Infras_water100_nonzero" = "Water infrastructure\n(Nonzero vs Zero)",
  "Infras_water100_log" = "Water infrastructure\n(log-scale)"
)

Soc_col <- names(infra_term_titles)

Sco_plots <- lapply(Soc_col, function(t) {
  df <- subset(infra_model_satu, term == t)
  
  ggplot(df, aes(x = Model, y = estimate, ymin = conf.low, ymax = conf.high, color = Model)) +
    geom_errorbar(position = position_dodge(width = 0.5),width=0.2) +
    geom_point(position = position_dodge(width = 0.5),size=1) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_text(aes(x = Model,y = conf.high, label = sig),
              position = position_dodge(width = 0.8),
              hjust = -0.3, vjust = 0.5,  size = 6, color = "black", fontface = "bold") + 
    labs(title = infra_term_titles[t], y = "HR (95% CI)") +
    scale_color_manual(values = model_colors, guide = guide_legend(nrow = 1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),      
      axis.title.x = element_blank(),
      legend.position = "top",
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.6)   #  added frame
    )
})

# Combine all plots with shared legend at top
Sco_plots_combined <- wrap_plots(Sco_plots, ncol = 4, guides = "collect") & 
  theme(legend.position = 'bottom')

Sco_plots_combined <- Sco_plots_combined + 
  plot_annotation(title = "Association between infrastructure and mortality",
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))
Sco_plots_combined
getwd()
ggsave("figure/infrat_HR_3.png", 
       plot = Sco_plots_combined, 
       width = 14, 
       height = 12, 
       dpi = 300, 
       units = "in")

# Eco ----
eco_model_satu <- model_satu_2 %>% filter(term %in% Eco_col)
eco_model_satu$term <- factor(eco_model_satu$term,
                              levels = Eco_col)
eco_term_titles <- c(
  "GDP2010_buffer005mean_100" = "GDP",
  "Nighttime_light" = "Nighttime light",
  "Pop_lastyear_100" = "Population density"
)

Eco_col <- names(eco_term_titles)

Eco_plots <- lapply(Eco_col, function(t) {
  df <- subset(eco_model_satu, term == t)
  
  ggplot(df, aes(x = Model, y = estimate, ymin = conf.low, ymax = conf.high, color = Model)) +
    geom_errorbar(position = position_dodge(width = 0.5),width=0.2) +
    geom_point(position = position_dodge(width = 0.5),size=1) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_text(aes(x = Model,y = conf.high, label = sig),
              position = position_dodge(width = 0.8),
              hjust = -0.3, vjust = 0.5,  size = 6, color = "black", fontface = "bold") + 
    labs(title = eco_term_titles[t], y = "HR (95% CI)") +
    scale_color_manual(values = model_colors, guide = guide_legend(nrow = 1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),      
      axis.title.x = element_blank(),
      legend.position = "top",
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.6)   
    )
})

# Combine all plots with shared legend at top
Eco_plots_combined <- wrap_plots(Eco_plots, ncol = 3, guides = "collect") & 
  theme(legend.position = 'bottom')

Eco_plots_combined <- Eco_plots_combined + 
  plot_annotation(title = "Association between economic factors and mortality",
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))
Eco_plots_combined
getwd()
ggsave("figure/eco_HR_3.png", 
       plot = Eco_plots_combined, 
       width = 13, 
       height = 4, 
       dpi = 300, 
       units = "in")

# nature ----
nat_model_satu <- model_satu_2 %>% filter(term %in% Nat_col) 
nat_model_satu$term <- factor(nat_model_satu$term,
                              levels = Nat_col)
nat_term_titles <- c(
  "PM25_lastyear_10_neg" = "Negative PM2.5",
  "NO2_lastyear_ugm3_10_neg" = "Negative NO2",
  # "CH4_lastyear_100_neg" = "Negative CH4",
  "NDVI_lastyear_gt0_01" = "NDVI"
)

Nat_col <- names(nat_term_titles)

Nat_plots <- lapply(Nat_col, function(t) {
  df <- subset(nat_model_satu, term == t)
  
  ggplot(df, aes(x = Model, y = estimate, ymin = conf.low, ymax = conf.high, color = Model)) +
    geom_errorbar(position = position_dodge(width = 0.5),width=0.2) +
    geom_point(position = position_dodge(width = 0.5),size=1) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_text(aes(x = Model,y = conf.high, label = sig),
              position = position_dodge(width = 0.8),
              hjust = -0.3, vjust = 0.5,  size = 6, color = "black", fontface = "bold") + 
    labs(title = nat_term_titles[t], y = "HR (95% CI)") +
    scale_color_manual(values = model_colors, guide = guide_legend(nrow = 1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),      
      axis.title.x = element_blank(),
      legend.position = "top",
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.6)   
    )
})

# Combine all plots with shared legend at top
Nat_plots_combined <- wrap_plots(Nat_plots, ncol = 3, guides = "collect") & 
  theme(legend.position = 'bottom')

Nat_plots_combined <- Nat_plots_combined + 
  plot_annotation(title = "Association between natural factors and mortality",
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))
Nat_plots_combined
getwd()
ggsave("figure/nat_HR_3.png", 
       plot = Nat_plots_combined, 
       width = 13, 
       height = 4, 
       dpi = 300, 
       units = "in")

# CHAP NO2 ----
cox_f(x1="NO2_chap_lastyear_10",
                 x2=NULL,
                 cov=c(cov_demo,cov_climate2,cov_eco,
                       cov_ap,cov_rd,cov_adlmmse),
      dataset = dt4_adl_mmse_ch4_tc,category = 1) %>% mutate(Model="H")
#                  term  estimate conf.low conf.high     p.value Model
# 1 NO2_chap_lastyear_10 0.6550565 0.610362 0.7030237 8.67909e-32     H

# simple plot
View(infra_model_satu)
View(eco_model_satu)
View(nat_model_satu)
View(model_satu)
table(model_satu$Model)
model_satu_H <- subset(model_satu, Model=="H:G+Adl+Cognition")
model_satu_D <- subset(model_satu, Model=="D:C+Economy")
model_satu_E <- subset(model_satu_2, Model=="E:D+Economy")

model_satu_H
Env_col <- c(Eco_col,Soc_col,Nat_col)

term_titles <- c(
  "GDP2010_buffer005mean_100" = "GDP",
  "Nighttime_light" = "Nighttime light",
  "Pop_lastyear_100" = "Population density",
  
  "Road_lastyear_10k" = "Road density",
  "Infras_CISI100" = "CISI",
  "Infras_transportation100" = "Transportation infrastructure",
  "Infras_energy100" = "Energy infrastructure",
  
  "Infras_healthcare100_nonzeroPresence" = "Healthcare infrastructure\n(Nonzero vs Zero)",
  "Infras_healthcare100_log" = "Healthcare infrastructure\n(log-scale)",
  
  "Infras_education100_nonzeroPresence" = "Education infrastructure\n(Nonzero vs Zero)",
  "Infras_education100_log" = "Education infrastructure\n(log-scale)",
  
  "Infras_telecommunication100_nonzero" = "Telecommunication infrastructure\n(Nonzero vs Zero)",
  "Infras_telecommunication100_log" = "Telecommunication infrastructure\n(log-scale)",
  
  "Infras_waste100_nonzero" = "Waste infrastructure\n(Nonzero vs Zero)",
  "Infras_waste100_log" = "Waste infrastructure\n(log-scale)",
  
  "Infras_water100_nonzero" = "Water infrastructure\n(Nonzero vs Zero)",
  "Infras_water100_log" = "Water infrastructure\n(log-scale)",
  
  "PM25_lastyear_10_neg" = "Negative PM2.5",
  "NO2_lastyear_ugm3_10_neg" = "Negative NO2",
  "NDVI_lastyear_gt0_01" = "NDVI"
)

term_titles2 <- c(
  "GDP2010_buffer005mean_100" = "GDP",
  "Nighttime_light" = "Nighttime light",
  "Pop_lastyear_100" = "Population density",
  
  "PM25_lastyear_10_neg" = "Negative PM2.5",
  "NO2_lastyear_ugm3_10_neg" = "Negative NO2",
  "NDVI_lastyear_gt0_01" = "NDVI",

  "Infras_telecommunication100_log" = "Telecommunication infrastructure (log-scale)",
  "Infras_telecommunication100_nonzero" = "Telecommunication infrastructure (Nonzero vs Zero)",
  
  "Infras_waste100_log" = "Waste infrastructure (log-scale)",
  "Infras_waste100_nonzero" = "Waste infrastructure (Nonzero vs Zero)",
  
  "Infras_water100_log" = "Water infrastructure (log-scale)",
  "Infras_water100_nonzero" = "Water infrastructure (Nonzero vs Zero)",
  
  "Infras_energy100" = "Energy infrastructure",
  "Infras_education100_log" = "Education infrastructure (log-scale)",
  "Infras_education100_nonzeroPresence" = "Education infrastructure (Nonzero vs Zero)",
  "Infras_healthcare100_log" = "Healthcare infrastructure (log-scale)",
  "Infras_healthcare100_nonzeroPresence" = "Healthcare infrastructure (Nonzero vs Zero)",
  
  "Infras_transportation100" = "Transportation infrastructure",
  "Infras_CISI100" = "CISI",
  "Road_lastyear_10k" = "Road density"
)

Env_col <- names(term_titles)
Env_col2 <- names(term_titles2)

model_satu_H$term <- factor(model_satu_H$term,
                            levels = Env_col)
model_satu_D$term <- factor(model_satu_D$term,
                            levels = Env_col)
model_satuD2$term <- factor(model_satuD2$term,
                            levels = Env_col)
model_satu_E$term <- factor(model_satu_E$term,
                            levels = Env_col2)
model_satu_H_2 <- subset(model_satu_H, !(term %in% c("Infras_healthcare100_log","Infras_education100_log",
                                                     "Infras_telecommunication100_log","Infras_waste100_log","Infras_water100_log")))

plot_modelH <- ggplot(model_satu_H_2, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +  # reference line at HR=1
  geom_errorbarh(height = 0.2, color = "gray50") +
  geom_point(aes(color = sig), size = 1) +
  scale_color_manual(values = c("*" = "#d7191c", " " = "#2c7bb6"), na.value = "#2c7bb6") +
  scale_x_continuous(trans = "log10",  # optional if you want log scale
                     breaks = c(0.5, 0.75, 1, 1.5, 2, 3),
                     labels = c("0.5", "0.75", "1", "1.5", "2", "3")) +
  scale_y_discrete(labels = term_titles) +   
  labs(x = "HR (95% CI)", y = NULL,
       # title = "Forest plot of HR (95% CI)",
       color = "Significant") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("figure/plot_modelH.png",plot_modelH,
       dpi=300, height=6,width=6)

plot_modelD <- ggplot(model_satu_D, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +  # reference line at HR=1
  geom_errorbarh(height = 0.2, color = "gray50") +
  geom_point(aes(color = sig), size = 1) +
  scale_color_manual(values = c("*" = "#d7191c", " " = "#2c7bb6"), na.value = "#2c7bb6") +
  scale_x_continuous(trans = "log10",  # optional if you want log scale
                     breaks = c(0.5, 0.75, 1, 1.5, 2, 3),
                     labels = c("0.5", "0.75", "1", "1.5", "2", "3")) +
  scale_y_discrete(labels = term_titles) +   
  labs(x = "HR (95% CI)", y = NULL,
       # title = "Forest plot of HR (95% CI)",
       color = "Significant") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
plot_modelD
ggsave("figure/plot_modelD.png",plot_modelD,
       dpi=300, height=6,width=6)

library(dplyr)
library(ggplot2)
library(grid)  # for unit()

# 1. Add category variable 
model_satu_E <- model_satu_E %>%
  mutate(
    category = case_when(
      term %in% c(
        "GDP2010_buffer005mean_100",
        "Nighttime_light",
        "Pop_lastyear_100"
      ) ~ "Economy",
      
      term %in% c(
        "PM25_lastyear_10_neg",
        "NO2_lastyear_ugm3_10_neg",
        "NDVI_lastyear_gt0_01"
      ) ~ "Nature",
      
      term %in% c(
        "Infras_telecommunication100_log",
        "Infras_telecommunication100_nonzero",
        "Infras_waste100_log",
        "Infras_waste100_nonzero",
        "Infras_water100_log",
        "Infras_water100_nonzero",
        "Infras_energy100",
        "Infras_education100_log",
        "Infras_education100_nonzeroPresence",
        "Infras_healthcare100_log",
        "Infras_healthcare100_nonzeroPresence",
        "Infras_transportation100",
        "Infras_CISI100",
        "Road_lastyear_10k"
      ) ~ "Infrastructure",
      
      TRUE ~ "Other"  # fallback, just in case
    )
  )
install.packages("ggh4x")
library(ggh4x)
# 2. Plot with facets as "category labels" on y ------------------------
plot_model_E <- ggplot(
  model_satu_E,
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(height = 0.2, color = "gray50") +
  geom_point(aes(color = sig), size = 1) +
  scale_color_manual(
    values = c("*" = "#d7191c", " " = "#2c7bb6"),
    na.value = "#2c7bb6"
  ) +
  scale_x_continuous(
    trans = "log10",
    breaks = c(0.5, 0.75, 1, 1.5, 2, 3),
    labels = c("0.5", "0.75", "1", "1.5", "2", "3")
  ) +
  scale_y_discrete(labels = term_titles2) +
  
  facet_grid2(
    category ~ .,
    scales = "free_y",
    space  = "free_y",
    switch = "y",
    strip = strip_themed(
      background_x = elem_list_rect(),
      background_y = elem_list_rect(
        fill = c(
          "Economy"        = "#f2e5ff",   # light purple
          "Infrastructure" = "#e6f0ff",    # light blue
          "Nature"         = "#e5f7e9"  # light green
        ),
        colour = NA
      ),
      text_y = elem_list_text(
        angle = 90,
        face = "bold",
        colour = "black"
      )
    )
  ) +
  
  labs(
    x = "HR (95% CI)",
    y = NULL,
    color = "Significant"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.placement = "outside",
    panel.spacing.y = unit(0.4, "lines")
  )

plot_model_E
ggsave("figure/plot_modelE.png",plot_model_E,
       dpi=300, height=6,width=8)


# spline ----
spldt <- dt4_adl_mmse_ch4_tc
library(rms)
dd <- datadist(spldt)
options(datadist='dd')
cox_sf_rd <- cph(Surv(survTime_year, event) ~ rcs(Road_lastyear_10k,3) + 
                   strat(prov_lab) + age + Gender + Education + Marriage +
                   Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                   Residence + GDP2010_buffer005mean_100 + Pop_lastyear_100,
                 data=spldt)
# plot(Predict(cox_sf_rd, Road_lastyear_10k, ref.zero = TRUE,fun=exp), ylab="HR (95% CI)")

summary(cox_sf_rd)
# set the reference value
min(spldt$Road_lastyear_10k)
dd$limits["Adjust to","Road_lastyear_10k"] <- 0
cox_sf_rd <- update(cox_sf_rd)
cox_pre_rd <- Predict(cox_sf_rd, 
                      Road_lastyear_10k,
                      ref.zero = TRUE,
                      fun=exp
)

library(ggplot2)
s1_rd <- ggplot() +
  geom_ribbon(data=cox_pre_rd,aes(x=Road_lastyear_10k,ymin = lower, ymax = upper), fill = "#ADD8E6",alpha=0.2) + # Adjust fill color for the ribbon
  geom_line(data=cox_pre_rd,aes(x=Road_lastyear_10k,y = yhat), color = "#4682B4", linewidth = 1) + # Adjust color for yhat line
  labs(x = expression(Road ~ density ~ (10~km/5 ~ km^2)), y = "HR (95%CI)", title = NULL) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_x_continuous(limits = c(0, 65), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15,face="bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.6) 
  )
s1_rd
ggsave("figure/spline_rd.png",
       s1_rd,dpi=300,
       height=5,width = 7)

# Spline stratified ----
# GDP1
table(spldt$GDP2010_4g,useNA = "ifany")
spldt_gdp1 <- subset(spldt, GDP2010_4g=="[ 0.06,  6.41)")
dd <- datadist(spldt_gdp1)
options(datadist='dd')
cox_sf_rd_gdp1 <- cph(Surv(survTime_year, event) ~ rcs(Road_lastyear_10k,3)+ 
                        strat(prov_lab) + age + Gender + Education + Marriage +
                        Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                        Residence + GDP2010_buffer005mean_100 + Pop_lastyear_100,
                      data=spldt_gdp1)

# set the reference value
min(spldt_gdp1$Road_lastyear_10k)
dd$limits["Adjust to","Road_lastyear_10k"] <- 0
cox_sf_rd_gdp1 <- update(cox_sf_rd_gdp1)
cox_pre_rd_gdp1 <- Predict(cox_sf_rd_gdp1, 
                           Road_lastyear_10k,
                           ref.zero = TRUE,
                           fun=exp
)

# GDP2
spldt_gdp2<-subset(spldt,GDP2010_4g=="[ 6.41, 14.67)")
dd <- datadist(spldt_gdp2)
options(datadist='dd')
cox_sf_rd_gdp2 <- cph(Surv(survTime_year, event) ~ rcs(Road_lastyear_10k,3)+ 
                        strat(prov_lab) + age + Gender + Education + Marriage +
                        Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                        Residence + GDP2010_buffer005mean_100 + Pop_lastyear_100,
                      data=spldt_gdp2)

# set the reference value
min(spldt_gdp2$Road_lastyear_10k)
dd$limits["Adjust to","Road_lastyear_10k"] <- 0
cox_sf_rd_gdp2 <- update(cox_sf_rd_gdp2)
cox_pre_rd_gdp2 <- Predict(cox_sf_rd_gdp2, 
                           Road_lastyear_10k,
                           ref.zero = TRUE,
                           fun=exp
)

# GDP3
spldt_gdp3<-subset(spldt,GDP2010_4g=="[14.67, 41.32)")
dd <- datadist(spldt_gdp3)
options(datadist='dd')
cox_sf_rd_gdp3 <- cph(Surv(survTime_year, event) ~ rcs(Road_lastyear_10k,3)+ 
                        strat(prov_lab) + age + Gender + Education + Marriage +
                        Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                        Residence + GDP2010_buffer005mean_100 + Pop_lastyear_100,
                      data=spldt_gdp3)

# set the reference value
min(spldt_gdp3$Road_lastyear_10k)
dd$limits["Adjust to","Road_lastyear_10k"] <- 0
cox_sf_rd_gdp3 <- update(cox_sf_rd_gdp3)
cox_pre_rd_gdp3 <- Predict(cox_sf_rd_gdp3, 
                           Road_lastyear_10k,
                           ref.zero = TRUE,
                           fun=exp
)

# GDP4 
# [41.32,252.31] 
spldt_gdp4<-subset(spldt,GDP2010_4g=="[41.32,252.31]")
dd <- datadist(spldt_gdp4)
options(datadist='dd')
cox_sf_rd_gdp4 <- cph(Surv(survTime_year, event) ~ rcs(Road_lastyear_10k,3)+ 
                        strat(prov_lab) + age + Gender + Education + Marriage +
                        Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                        Residence + GDP2010_buffer005mean_100 + Pop_lastyear_100,
                      data=spldt_gdp4)

# set the reference value
min(spldt_gdp4$Road_lastyear_10k)
dd$limits["Adjust to","Road_lastyear_10k"] <- 0
cox_sf_rd_gdp4 <- update(cox_sf_rd_gdp4)
cox_pre_rd_gdp4 <- Predict(cox_sf_rd_gdp4, 
                           Road_lastyear_10k,
                           ref.zero = TRUE,
                           fun=exp
)

cox_pre_rd_gdp1 <- cox_pre_rd_gdp1 %>% mutate(Group = "GDP Q1")
cox_pre_rd_gdp2 <- cox_pre_rd_gdp2 %>% mutate(Group = "GDP Q2")
cox_pre_rd_gdp3 <- cox_pre_rd_gdp3 %>% mutate(Group = "GDP Q3")
cox_pre_rd_gdp4 <- cox_pre_rd_gdp4 %>% mutate(Group = "GDP Q4")
combined_data_gdp <- bind_rows(cox_pre_rd_gdp1, cox_pre_rd_gdp2,
                               cox_pre_rd_gdp3,cox_pre_rd_gdp4)

# Plot combined data
library(ggplot2)
library(patchwork)
s1_rd2 <- ggplot() +
  # CHANGE 1: Use Neutral Colors (Grey/Black) to distinguish "Total" from "GDP Q1"
  geom_ribbon(data=cox_pre_rd, aes(x=Road_lastyear_10k, ymin=lower, ymax=upper), 
              fill="grey50", alpha=0.2) + 
  geom_line(data=cox_pre_rd, aes(x=Road_lastyear_10k, y=yhat), 
            color="black", linewidth=1) + 
  
  # CHANGE 2: Match the X-axis label exactly to Plot B
  labs(x = "Road density", 
       y = "HR (95% CI)", 
       title = NULL) +
  
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_x_continuous(limits = c(0, 65), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  
  # CHANGE 3: Match the reference line color to Plot B
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15, face="bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    # CHANGE 4: Removed panel.border to match Plot B's "open" style
    # If you prefer the border, you must add this line to Plot B as well.
    panel.border = element_blank() 
  )

# 1. Define clear labels for the legend
# This maps your data values to readable text in the legend
legend_labels <- c(
  "GDP Q1" = "GDP Q1 (Lowest)", 
  "GDP Q2" = "GDP Q2", 
  "GDP Q3" = "GDP Q3", 
  "GDP Q4" = "GDP Q4 (Highest)"
)

# 2. Create the (B) Plot
combined_plot_gdp <- ggplot() +
  # Ribbons (Confidence Intervals)
  geom_ribbon(data=combined_data_gdp, 
              aes(x=Road_lastyear_10k, ymin=lower, ymax=upper, fill=Group), 
              alpha=0.2) +
  # Lines (Estimates)
  geom_line(data=combined_data_gdp, 
            aes(x=Road_lastyear_10k, y=yhat, color=Group), 
            linewidth=1) +
  # Axis Labels
  labs(
    x = "Road density",
    y = "HR (95% CI)", 
    title = NULL
  ) +
  scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_x_continuous(limits = c(0, 65), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  geom_hline(yintercept=1, linetype="dashed", color="gray30") + # Slightly softer gray for reference
  
  # Manual Colors with Custom Labels
  scale_fill_manual(
    values=c(
      "GDP Q1"="#ADD8E6", 
      "GDP Q2"="#FFD700", 
      "GDP Q3"="#32CD32", 
      "GDP Q4"="#FF6347"
    ),
    labels = legend_labels  # <--- Apply the detailed labels here
  ) +
  scale_color_manual(
    values=c(
      "GDP Q1"="#0000FF", 
      "GDP Q2"="#FFA500", 
      "GDP Q3"="#006400", 
      "GDP Q4"="#FF0000"
    ),
    labels = legend_labels  # <--- Apply the detailed labels here
  ) +  
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(size=15),
    axis.text = element_text(size=15, face="bold"),
    legend.title = element_blank(),
    legend.position = "inside",              
    legend.position.inside = c(0.95, 0.95),    
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = NA), # Adds white bg to legend so lines don't show through
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
combined_plot_gdp
# 3. Final Assembly with Panel Labels (A and B)
# plot_annotation(tag_levels = 'A') automatically adds A and B to the top left
title_theme <- theme(
  plot.title = element_text(size = 15, face = "bold", hjust = 0),
  plot.margin = margin(t = 10, r = 10, b = 0, l = 10) # Reduced bottom margin for titles
)
s1_rd2 <- s1_rd2 +
  labs(title = "A. Spline based on the total sample") +title_theme
combined_plot_gdp <- combined_plot_gdp +
  labs(title = "B. Splines in GDP quartile groups")+title_theme
spline_rd_byGDP2 <- ( s1_rd2 / combined_plot_gdp ) + 
  plot_layout(heights = c(1,1))

# View the plot
spline_rd_byGDP2
ggsave("figure/spline_rd_byGDP2_2.png",
       spline_rd_byGDP2,
       dpi=300,
       height=10,width = 7)



# stratification ----
library(Hmisc)
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>% mutate(NDVI_lastyear_4g = cut2(NDVI_lastyear_gt0_01,g=4),
                                                      NegativePM25_4g = cut2(PM25_lastyear_10_neg,g=4),
                                                      NegativeNO2_4g = cut2(NO2_lastyear_ugm3_10_neg,g=4),
                                                      GDP2010_4g = cut2(GDP2010_buffer005mean_100,g=4),
                                                      Nighttime_light_4g = cut2(Nighttime_light,g=4),
                                                      Population_lastyear_4g = cut2(Pop_lastyear_100,g=4),
                                                      CISI_4g = cut2(CISI,g=4),
                                                      Energy_4g = cut2(Subscore_energy,g=4))

dt4_adl_mmse_ch4_tc$Infras_healthcare100_nonzero <- factor(dt4_adl_mmse_ch4_tc$Infras_healthcare100_nonzero,
                                                           levels = c(0,1),labels=c("No presence","Presence"))
dt4_adl_mmse_ch4_tc$Infras_education100_nonzero <- factor(dt4_adl_mmse_ch4_tc$Infras_education100_nonzero,
                                                          levels = c(0,1),labels=c("No presence","Presence"))
summary(dt4_adl_mmse_ch4_tc[dt4_adl_mmse_ch4_tc$Infras_healthcare100_nonzero=="Presence",]$Infras_healthcare100)
hist(dt4_adl_mmse_ch4_tc[dt4_adl_mmse_ch4_tc$Infras_healthcare100_nonzero=="Presence",]$Infras_healthcare100)
summary(dt4_adl_mmse_ch4_tc[dt4_adl_mmse_ch4_tc$Infras_education100_nonzero=="Presence",]$Infras_education100)

summary(dt4_adl_mmse_ch4_tc$Infras_healthcare100_nonzero)
summary(dt4_adl_mmse_ch4_tc$Infras_education100_nonzero)
table(dt4_adl_mmse_ch4_tc$Infras_healthcare100_4g,useNA = "ifany")
table1(~Infras_education100+Infras_education100_4g|Infras_education100_nonzero,dt4_adl_mmse_ch4_tc)
table1(~Infras_healthcare100+Infras_healthcare100_4g|Infras_healthcare100_nonzero,dt4_adl_mmse_ch4_tc)

# Step 1: Create cut categories only for positive values
cuts_pos_healthcare <- cut2(dt4_adl_mmse_ch4_tc$Infras_healthcare100[
  dt4_adl_mmse_ch4_tc$Infras_healthcare100 > 0],
  g = 3)
cuts_pos_education <- cut2(dt4_adl_mmse_ch4_tc$Infras_education100[
  dt4_adl_mmse_ch4_tc$Infras_education100 > 0],
  g = 3)

# Step 2: Assign categories back to the full dataframe
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>%
  mutate(
    Infras_healthcare100_4g = NA_character_,
    Infras_healthcare100_4g = replace(
      Infras_healthcare100_4g,
      Infras_healthcare100 == 0,
      "0"
    ),
    Infras_healthcare100_4g = replace(
      Infras_healthcare100_4g,
      Infras_healthcare100 > 0,
      as.character(cuts_pos_healthcare)
    ),
    Infras_healthcare100_4g = factor(
      Infras_healthcare100_4g,
      levels = c("0", levels(cuts_pos_healthcare))
    )
  )
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>%
  mutate(
    Infras_education100_4g = NA_character_,
    Infras_education100_4g = replace(
      Infras_education100_4g,
      Infras_education100 == 0,
      "0"
    ),
    Infras_education100_4g = replace(
      Infras_education100_4g,
      Infras_education100 > 0,
      as.character(cuts_pos_education)
    ),
    Infras_education100_4g = factor(
      Infras_education100_4g,
      levels = c("0", levels(cuts_pos_education))
    )
  )


table1(~Infras_healthcare100|Infras_healthcare100_4g,dt4_adl_mmse_ch4_tc)
table1(~Infras_education100|Infras_education100_4g,dt4_adl_mmse_ch4_tc)


stra_variables1 <- c("NDVI_lastyear_4g", "NegativePM25_4g", "NegativeNO2_4g",
                     "Residence", "GDP2010_4g","Nighttime_light_4g", "Population_lastyear_4g",
                     "CISI_4g","Energy_4g",
                     "Infras_healthcare100_4g","Infras_education100_4g" 
                     # "Infras_healthcare100_nonzero","Infras_education100_nonzero",
)
stra_values <- list()

# Loop over each variable and get unique values
for (variable in stra_variables1) {
  stra_values[[variable]] <- unique(dt4_adl_mmse_ch4_tc[[variable]])
}

# Initialize an empty list to store results
stra_forrd_results <- list()

# Loop over each variable and its corresponding unique values
HR_strabycov <- function(dataset,env,cov,category,subvar)
{
  f <- reformulate(c(env,cov),
                   response="Surv(survTime_year,event)")
  coxfit <- coxph(f,
                  data=dataset)
  library(broom)
  HR <- coxfit %>% tidy(conf.int=T) %>%
    mutate(
      estimate=exp(estimate),
      conf.low=exp(conf.low),
      conf.high=exp(conf.high)
    ) %>%
    dplyr::select(term, estimate, starts_with("conf"),p.value) %>%  head(category) %>% mutate(subset=subvar)
  return(data.frame(HR))
}

for (variable in stra_variables1) {
  values <- stra_values[[variable]]  # Fixed variable name
  for (value in values) {  # Changed loop variable name to 'value' instead of 'j'
    subset_name <- paste0(variable, "_", value)
    subset <- subset(dt4_adl_mmse_ch4_tc, get(variable) == value)
    stra_forrd_results[[subset_name]] <- HR_strabycov(
      dataset = subset,
      env = "Road_lastyear_10k",
      cov = c(cov_demo,cov_climate,cov_eco,cov_adlmmse),
      category = 1,
      subvar = subset_name
    )
  }
}

stra_forrd_bind <- do.call(rbind, stra_forrd_results)
library(stringr)
stra_forrd_bind <- stra_forrd_bind %>%
  mutate(
    subset = as.character(subset),
    strat_var   = sub("_[^_]*$", "", subset),   # everything before the LAST "_"
    strat_group = sub("^.*_", "", subset) %>% str_trim()  # everything after the LAST "_"
  )
unique(stra_forrd_bind$strat_var)
stra_forrd_bind <- stra_forrd_bind %>%
  mutate(
    strat_var = sub("_4g$", "", strat_var),               # drop "_4g"
    strat_var = sub("_lastyear$", "", strat_var),         # drop "_lastyear"
    strat_var = sub("Population$", "Population density", strat_var), # rename
    strat_var = sub("NDVI$", "NDVI", strat_var),
    strat_var = sub("GDP2010$", "GDP", strat_var),
    strat_var = sub("Nighttime_light$", "Nighttime light", strat_var), # rename
    strat_var = sub("NegativePM25$", "Negative PM2.5", strat_var),
    strat_var = sub("NegativeNO2$", "Negative NO2", strat_var),
    strat_var = sub("CISI$", "CISI", strat_var),
    strat_var = sub("Energy$", "Energy infrastructure", strat_var),
    strat_var = sub("Infras_healthcare100", "Healthcare infrastructure", strat_var),
    strat_var = sub("Infras_education100", "Education infrastructure", strat_var)
  )

# order the quartile
unique(stra_forrd_bind$strat_group)
stra_forrd_bind <- stra_forrd_bind %>%
  mutate(
    order_key = case_when(
      str_starts(strat_group, "\\[") ~ as.numeric(sub("^\\[\\s*([^,]+),.*$", "\\1", strat_group)),
      TRUE                           ~ suppressWarnings(as.numeric(strat_group))
    )
  ) %>%
  group_by(strat_var) %>%
  arrange(order_key, .by_group = TRUE) %>%
  mutate(strat_group = factor(strat_group, levels = unique(strat_group))) %>%
  ungroup()

unique(stra_forrd_bind$strat_var)
stra_var_order <- c(
  "Residence",
  "GDP", 
  "Nighttime light",
  "Population density", 
  "CISI", 
  "Education infrastructure", 
  "Healthcare infrastructure", 
  "Energy infrastructure",
  "Negative NO2", 
  "Negative PM2.5", 
  "NDVI"
)

stra_forrd_bind <- stra_forrd_bind %>%
  mutate(strat_var = factor(strat_var, levels = stra_var_order))
rank_nonzero_df <- stra_forrd_bind %>%
  filter(strat_var %in% c("Education infrastructure", "Healthcare infrastructure"),
         strat_group != "0") %>%
  group_by(strat_var) %>%
  mutate(rank_nonzero = dense_rank(order_key)) %>%
  select(strat_var, strat_group, rank_nonzero)
stra_forrd_bind2 <- stra_forrd_bind %>%
  group_by(strat_var) %>%
  mutate(rank_within = if_else(
    strat_var == "Residence",
    NA_real_,
    dense_rank(order_key)
  )) %>%
  ungroup()
stra_forrd_bind2 <- stra_forrd_bind2 %>%
  left_join(rank_nonzero_df,
            by = c("strat_var", "strat_group"))
stra_forrd_bind2 <- stra_forrd_bind2 %>%
  mutate(
    strat_label = case_when(
      
      # 1) Residence unchanged
      strat_var == "Residence" ~ strat_group,
      
      # 2) Zero for education & healthcare
      strat_var %in% c("Education infrastructure", "Healthcare infrastructure") &
        strat_group == "0" ~ "0",
      
      # 3) Non-zero for education/healthcare → T1,T2,T3
      strat_var %in% c("Education infrastructure", "Healthcare infrastructure") &
        strat_group != "0" ~ paste0("T", rank_nonzero, " (>0)"),
      
      # 4) All continuous vars → Q1–Q4
      TRUE ~ paste0("Q", rank_within)
    )
  )
library(forcats)
stra_forrd_bind_plot2 <- ggplot(
  stra_forrd_bind2, 
  aes(
    x = estimate, 
    y = fct_rev(strat_label), 
    xmin = conf.low, 
    xmax = conf.high
  )
) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
  
  geom_errorbar(position = position_dodge(width = 0.5),width=0.2,  aes(color = strat_var)) +
  geom_point(position = position_dodge(width = 0.5),size=1, aes(color = strat_var)) +
  # geom_pointrange(shape = 15, size = 0.3, aes(color = strat_var)) +
  facet_grid(strat_var ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "HR (95% CI) of road density",
    y = "Stratification Groups"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, face = "bold"),
    strip.background = element_rect(fill = "lightgray"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10)
  )
stra_forrd_bind_plot2
ggsave("figure/stra_forrd_bind_plot3.png",
       stra_forrd_bind_plot2,dpi=300,
       height=7,width = 5)

stra_forrd_bind_plot <- ggplot(stra_forrd_bind, 
                              aes(x = estimate, y = fct_rev(strat_group), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
  geom_pointrange(shape = 15, size = 0.3, aes(color = strat_var)) +
  
  facet_grid(strat_var ~ ., scales = "free_y", space = "free_y") +
  # scale_x_continuous(breaks = seq(0.85, 1.01, 0.05), limits = c(0.84, 1.01))+
  labs(
    # title = "Forest Plot of Hazard Ratios by Stratification Variables",
    x = "HR (95% CI) of road density",
    y = "Stratification Groups"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, face = "bold"),
    strip.background = element_rect(fill = "lightgray"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10)
  )
stra_forrd_bind_plot
ggsave("figure/stra_forrd_bind_plot.png",
       stra_forrd_bind_plot,dpi=300,
       height=9,width = 7)

stra_forrd_plot
ggsave("figure/stra_forrd_plot.png",
       stra_forrd_plot,dpi=300,
       height=5,width = 12)
getwd()
write.csv(stra_forrd_bind, file="stra_forrd_bind.csv")

# stratify by population ----
stra_variables_pop <- c("Age_group_lab", "Gender", "Education", "Marriage", 
                        "Regular_exercise_2","Smoking_2","Drinking_2",
                        "adl_2","CognitiveImpairment")
# Initialize an empty list to store unique values for each variable
rm(stra_values_pop,stra_forrd_results_pop,stra_forrd_pop_bind)
stra_values_pop <- list()
# Loop over each variable and get unique values
for (variable in stra_variables_pop) {
  stra_values_pop[[variable]] <- unique(dt4_adl_mmse_ch4_tc[[variable]])
}
stra_values_pop[["Smoking_2"]]

stra_forrd_results_pop <- list()
full_cov <- c(cov_demo,cov_climate,cov_eco,cov_ap,cov_adlmmse)
for (variable in stra_variables_pop) {
  values <- stra_values_pop[[variable]]  # Fixed variable name
  for (value in values) {  # Changed loop variable name to 'value' instead of 'j'
    subset_name <- paste0(variable, "_", value)
    subset <- subset(dt4_adl_mmse_ch4_tc, get(variable) == value)
    stra_forrd_results_pop[[subset_name]] <- HR_strabycov(
      dataset = subset,
      env = "Road_lastyear_10k",
      cov = full_cov[!full_cov == variable],
      category = 1,
      subvar = subset_name
    )
  }
}
stra_forrd_pop_bind <- do.call(rbind, stra_forrd_results_pop)
library(stringr)
stra_forrd_pop_bind <- stra_forrd_pop_bind %>%
  mutate(
    subset = as.character(subset),
    strat_var   = sub("_[^_]*$", "", subset),   # everything before the LAST "_"
    strat_group = sub("^.*_", "", subset) %>% str_trim()  # everything after the LAST "_"
  )
unique(stra_forrd_pop_bind$strat_var)
library(dplyr)
library(stringr)
stra_forrd_pop_bind[,c("strat_var","strat_group")] %>% View()
# Clean the strat_var column
stra_forrd_pop_bind <- stra_forrd_pop_bind %>%
  mutate(strat_var = strat_var %>%
           str_remove("_lab$") %>%
           str_remove("_2$") %>%
           str_to_title() %>%
           str_replace_all("_", " ") %>%
           str_replace("Drinking", "Drinking alcohol") %>%
           str_replace("Gender", "Sex") %>%
           str_replace("Adl", "ADL") %>%
           str_replace("Cognitiveimpairment", "Cognitive impairment"))

# Check the result
unique(stra_forrd_pop_bind$strat_var)
getwd()
write.csv(stra_forrd_pop_bind,file="stra_forrd_pop_bind.csv")
# stra_forrd_pop_bind <- read.csv("/Users/linxin/202201/a_CLHLS/m_urbanization/stra_forrd_pop_bind1026.csv")
unique(stra_forrd_pop_bind$strat_var)
stra_var_pop_order <- c(
  "Age group", 
  "Sex",
  "Education", 
  "Marriage", 
  "Regular exercise", 
  "Smoking", 
  "Drinking alcohol",
  "ADL", 
  "Cognitive impairment" 
)

stra_forrd_pop_bind <- stra_forrd_pop_bind %>%
  mutate(strat_var = str_squish(trimws(strat_var))) 
setdiff(unique(stra_forrd_pop_bind$strat_var), stra_var_pop_order)
# Now apply the factor
stra_forrd_pop_bind <- stra_forrd_pop_bind %>%
  mutate(strat_var = factor(strat_var, levels = stra_var_pop_order))
unique(stra_forrd_pop_bind$strat_var)

# stra_forrd_pop_bind$Index = nrow(stra_forrd_pop_bind):1
unique(stra_forrd_pop_bind$strat_var)
unique(stra_forrd_pop_bind$strat_group)

stra_forrd_pop_bind <- stra_forrd_pop_bind %>%
  mutate(strat_group = case_when(
    strat_var == "ADL" ~ paste0("ADL: ", strat_group),
    strat_var == "Cognitive impairment" ~ paste0("CI: ", strat_group),
    TRUE ~ strat_group
  ))
unique(stra_forrd_pop_bind$strat_group)
stra_forrd_pop_bind$strat_group <- ifelse(stra_forrd_pop_bind$strat_group=="not married","Not married",
                                          stra_forrd_pop_bind$strat_group)
stra_forrd_pop_bind_2 <- stra_forrd_pop_bind %>%
  filter(!str_detect(strat_group, "(?i)missing"))
unique(stra_forrd_pop_bind_2$strat_group)

stra_group_order <- c(
  "<80", "80-89", "90-99", "100~",
  "Female", "Male",
  "0 year", "1-6 years", ">6 years",
  "Not married", "Married",
   "Heavy smoker", "Heavy drinker", "Light smoker","Moderate drinker", "Current", "Former", "Never", 
  "No", 
  "ADL: No", "ADL: Yes",
  "CI: No", "CI: Yes"
)
stra_forrd_pop_bind_2 <- stra_forrd_pop_bind_2 %>%
  mutate(strat_group = str_squish(trimws(strat_group)))
setdiff(unique(stra_forrd_pop_bind_2$strat_group), stra_group_order)

stra_forrd_pop_bind_2 <- stra_forrd_pop_bind_2 %>%
  mutate(strat_group = factor(strat_group, levels = stra_group_order))
# geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
#   geom_pointrange(shape = 15, size = 0.3, aes(color = strat_var)) +
#   
stra_forrd_pop_plot <- ggplot(stra_forrd_pop_bind_2,
                              aes(x = estimate, y = fct_rev(strat_group), xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
  geom_errorbar(position = position_dodge(width = 0.5),width=0.2,  aes(color = strat_var)) +
  geom_point(position = position_dodge(width = 0.5),size=1, aes(color = strat_var)) +
  facet_grid(strat_var ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = seq(0.85, 1.01, 0.05), limits = c(0.84, 1.01))+
  labs(
    # title = "Forest Plot of Hazard Ratios by Stratification Variables",
    x = "HR (95% CI) of road density",
    y = "Stratification Groups"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, face = "bold"),
    strip.background = element_rect(fill = "lightgray"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10)
  )
stra_forrd_pop_plot
ggsave(stra_forrd_pop_plot, file="figure/stra_forrd_pop_plot3.png",
       dpi=300,
       height=6,width=5)

# stratify by road ----
summary(dt4_adl_mmse_ch4_tc$Road_lastyear_10k)
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>% mutate(Road_lastyear_5g = ifelse(Road_lastyear_10k==0,1,
                                              ifelse(Road_lastyear_10k>0 &  Road_lastyear_10k<=1.472,2,
                                                     ifelse(Road_lastyear_10k>1.472 &  Road_lastyear_10k<=3.529,3,
                                                            ifelse(Road_lastyear_10k>3.529 &  Road_lastyear_10k<=10,4,
                                                                   ifelse(Road_lastyear_10k>10,5,NA))))))
dt4_adl_mmse_ch4_tc$Road_5g <- factor(dt4_adl_mmse_ch4_tc$Road_lastyear_5g, levels = 1:5, labels = c("0","(0,1.472]","(1.472,3.529]",
                                                                   "(3.529,10]","(10,65.073]"))
table(dt4_adl_mmse_ch4_tc$Road_5g, dt4_adl_mmse_ch4_tc$Road_lastyear_5g,useNA = "ifany")   

HR_strat_byroad <- function(dataset, var, cov, subvar) {
  f <- reformulate(c(var, cov), response = "Surv(survTime_year, event)")
  coxfit <- coxph(f, data = dataset)
  library(broom)
  HR <- coxfit %>% tidy(conf.int = TRUE) %>%
    filter(term %in% var | str_detect(term, paste0("^", var)))%>%
    mutate(
      estimate = exp(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high),
      strat_var = var,
      road_group = subvar
    ) %>%
    select(strat_var, road_group, estimate, conf.low, conf.high, p.value)
  return(HR)
}


strabyrd_variables1 <- c("NDVI_lastyear_gt0_01", "PM25_lastyear_10_neg", "NO2_lastyear_ugm3_10_neg",
                     "Residence", "GDP2010_buffer005mean_100","Nighttime_light", "Pop_lastyear_100",
                     "CISI","Subscore_energy","Infras_healthcare100_nonzero","Infras_education100_nonzero"
)

road_groups <- levels(dt4_adl_mmse_ch4_tc$Road_5g)
cov_4c <- c(cov_demo,cov_climate2,cov_adlmmse,cov_eco)

strabyrd_results_list <- list()
for (v in strabyrd_variables1) {
  for (r in road_groups) {
    subset_dt <- dt4_adl_mmse_ch4_tc %>% filter(Road_5g == r)
    strabyrd_results_list[[paste(v, r, sep = "_")]] <- HR_strat_byroad(
      dataset = subset_dt,
      var = v,
      cov = cov_4c[cov_4c!=v],
      subvar = r
    )
  }
}

strabyrd_df <- bind_rows(strabyrd_results_list)
table(strabyrd_df$strat_var)
summary(strabyrd_df)
strabyrd_df <- strabyrd_df %>%
  mutate(
    strat_var = recode(strat_var,
                       "NDVI_lastyear_gt0_01" = "NDVI",
                       "PM25_lastyear_10_neg" = "Negative PM2.5",
                       "NO2_lastyear_ugm3_10_neg" = "Negative NO2",
                       "Residence" = "Residence",
                       "GDP2010_buffer005mean_100" = "GDP",
                       "Nighttime_light"="Nighttime light",
                       "Pop_lastyear_100" = "Population density",
                       "CISI" = "CISI score",
                       "Subscore_energy" = "Energy infrastructure score",
                       "Infras_healthcare100_nonzero" = "Healthcare infrastructure presence",
                       "Infras_education100_nonzero" = "Education infrastructure presence"
    )
  )
stra_var_order2 <- c(
  "Residence",
  "GDP", 
  "Nighttime light",
  "Population density", 
  "CISI score", 
  "Education infrastructure presence", 
  "Healthcare infrastructure presence", 
  "Energy infrastructure score",
  "Negative NO2", 
  "Negative PM2.5", 
  "NDVI"
)
strabyrd_df <- strabyrd_df %>%
  mutate(strat_var = factor(strat_var, levels = stra_var_order2))
strabyrd_df <- strabyrd_df %>%
  mutate(road_group = factor(road_group,
                             levels = c("0","(0,1.472]","(1.472,3.529]",
                                        "(3.529,10]","(10,65.073]")))
plot_strabyrd_df <- ggplot(strabyrd_df,
                    aes(x = estimate, y = road_group,
                        xmin = conf.low, xmax = conf.high)) +
  geom_point(size = 1) +
  geom_errorbarh(height = 0.18) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_wrap(~ strat_var, scales = "free_x") +  
  labs(
    x = "HR (95% CI) of exposures across road density strata",
    y = "Road density strata",
    # title = "HR of exposures across Road density strata"
  ) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"))
plot_strabyrd_df
ggsave("figure/straby_rd_other.png",
       plot_strabyrd_df,dpi=300,
       height=5,width = 12)

# stratify by road 2 ----
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>%
  mutate(
    Road_5g = cut(
      Road_lastyear_10k,
      breaks = c(-Inf, 0, 1.472, 3.529, 10, Inf),
      labels = c(
        "0",
        "(0, 1.472]",
        "(1.472, 3.529]",
        "(3.529, 10]",
        "(10, 65.073]"
      ),
      right = TRUE
    )
  )

table(dt4_adl_mmse_ch4_tc$Road_5g, useNA = "ifany")

HR_strat_byroad <- function(dataset, exposure, covars, road_group) {
  
  cov_use <- setdiff(covars, exposure)
  fml <- reformulate(c(exposure, cov_use),
                     response = "Surv(survTime_year, event)")
  
  fit <- try(coxph(fml, data = dataset), silent = TRUE)
  
  if (inherits(fit, "try-error")) {
    return(
      tibble(
        strat_var  = exposure,
        road_group = road_group,
        estimate   = NA,
        conf.low   = NA,
        conf.high  = NA,
        p.value    = NA
      )
    )
  }
  
  broom::tidy(fit, conf.int = TRUE) %>%
    filter(term == exposure) %>%
    transmute(
      strat_var  = exposure,
      road_group = road_group,
      estimate   = exp(estimate),
      conf.low   = exp(conf.low),
      conf.high  = exp(conf.high),
      p.value
    )
}

env_vars <- c(
  "NDVI_lastyear_gt0_01",
  "PM25_lastyear_10_neg",
  "NO2_lastyear_ugm3_10_neg",
  "Residence",
  "GDP2010_buffer005mean_100",
  "Nighttime_light",
  "Pop_lastyear_100",
  "CISI",
  "Subscore_energy",
  "Infras_healthcare100_nonzero",
  "Infras_education100_nonzero"
)

road_groups <- levels(dt4_adl_mmse_ch4_tc$Road_5g)
library(purrr)
strabyrd_df <- map_dfr(env_vars, function(v) {
  map_dfr(road_groups, function(r) {
    subset_dt <- dt4_adl_mmse_ch4_tc %>% filter(Road_5g == r)
    HR_strat_byroad(
      dataset = subset_dt,
      exposure = v,
      covars   = cov_4c,
      road_group = r
    )
  })
})
strabyrd_df <- strabyrd_df %>%
  mutate(
    strat_var = recode(
      strat_var,
      "NDVI_lastyear_gt0_01" = "NDVI",
      "PM25_lastyear_10_neg" = "Negative PM2.5",
      "NO2_lastyear_ugm3_10_neg" = "Negative NO2",
      "Residence" = "Residence",
      "GDP2010_buffer005mean_100" = "GDP",
      "Nighttime_light" = "Nighttime light",
      "Pop_lastyear_100" = "Population density",
      "CISI" = "CISI score",
      "Subscore_energy" = "Energy infrastructure score",
      "Infras_healthcare100_nonzero" = "Healthcare infrastructure presence",
      "Infras_education100_nonzero" = "Education infrastructure presence"
    ),
    strat_var = factor(strat_var, levels = c(
      "Residence",
      "GDP",
      "Nighttime light",
      "Population density",
      "CISI score",
      "Education infrastructure presence",
      "Healthcare infrastructure presence",
      "Energy infrastructure score",
      "Negative NO2",
      "Negative PM2.5",
      "NDVI"
    )),
    road_group = factor(
      road_group,
      levels = c("0","(0, 1.472]","(1.472, 3.529]",
                 "(3.529, 10]","(10, 65.073]")
    )
  )

strabyrd_df <- strabyrd_df %>% filter(!(strat_var %in% c("CISI score",
                                                        "Education infrastructure presence",
                                                        "Healthcare infrastructure presence",
                                                        "Energy infrastructure score")))
strabyrd_df$strat_var
plot_strabyrd_df <- ggplot(
  strabyrd_df,
  aes(
    x = road_group,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high,
    color = road_group
  )
) +
  geom_errorbar(width = 0.2, linewidth = 0.6) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.6) +
  
  facet_wrap(
    ~ strat_var,
    scales = "free_y",
    ncol = 3,
    strip.position = "top"
  ) +
  
  labs(
    y = "HR (95% CI)",
    x = NULL
  ) +
  scale_color_manual(
    name = "Road density strata",
    values = c(
      "0" = "#2166AC",
      "(0, 1.472]" = "#67A9CF",
      "(1.472, 3.529]" = "#FDAE61",
      "(3.529, 10]" = "#F46D43",
      "(10, 65.073]" = "#B2182B"
    ),
    guide = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  ) +
  
  
  theme_bw(base_size = 12) +
  theme(
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey90", color = NA),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(1.2, "cm")
  )

plot_strabyrd_df
ggsave("figure/plot_strabyrd_22.png",plot_strabyrd_df,dpi=300,
     width = 8, height =6)


# road interaction ----
dt4_adl_mmse_ch4_tc
stra_var_order
stra_variables1
strabyrd_variables1
variables_to_scale <- setdiff(strabyrd_variables1,c("Residence","Infras_healthcare100_nonzero",
                                                    "Infras_education100_nonzero"))
for (variable in variables_to_scale) {
  dt4_adl_mmse_ch4_tc[[paste0("scaled_", variable)]] <- scale(dt4_adl_mmse_ch4_tc[[variable]])
}
dt4_adl_mmse_ch4_tc$scaled_Road_lastyear_10k <- scale(dt4_adl_mmse_ch4_tc$Road_lastyear_10k)

colnames(dt4_adl_mmse_ch4_tc)
int_col <- c("scaled_NDVI_lastyear_gt0_01","scaled_PM25_lastyear_10_neg", "scaled_NO2_lastyear_ugm3_10_neg",
             "Residence","scaled_GDP2010_buffer005mean_100","scaled_Nighttime_light","scaled_Pop_lastyear_100",
             "scaled_CISI","scaled_Subscore_energy","Infras_healthcare100_nonzero", "Infras_education100_nonzero")

cox_int_f <- function(x1,x2,cov,dataset,int) {
  if (int==1) {f <- reformulate(c(x1,x2,paste(x1,x2,sep="*"),cov),
                                response="Surv(survTime_year,event)")}
  else {f <- reformulate(c(x1,x2,cov),
                         response="Surv(survTime_year,event)")}
  
  coxfit <- coxph(f,
                  data=dataset)
  library(broom)
  HR <- coxfit %>%
    tidy(conf.int=T) %>%
    mutate(
      HR=exp(estimate),
      conf.low=exp(conf.low),
      conf.high=exp(conf.high)
    ) %>%
    dplyr::select(term, HR, starts_with("conf"), estimate,p.value) 
  HR <-  data.frame(HR) %>% 
    filter(grepl(paste(c(x1,x2), collapse = "|"),term)) 
  return(HR)
}

int_rd <- bind_rows(lapply(int_col,
                           cox_int_f, x1="scaled_Road_lastyear_10k",
                           cov=c(cov_demo,cov_climate),
                           dataset = dt4_adl_mmse_ch4_tc,
                           int = 1))
summary(coxph(Surv(survTime_year,event) ~ scaled_Road_lastyear_10k*Infras_healthcare100_nonzero+
                age+Gender+Education+Marriage+
                Regular_exercise_2+Smoking_2+Drinking_2+
                strata(prov_lab)+Elevation+Temp_sd_lastyear_1c+Temp_mean_lastyear_1c,
              data=dt4_adl_mmse_ch4_tc))
summary(coxph(Surv(survTime_year,event) ~ Road_length_10km+
                age+Gender+Education+Marriage+
                Regular_exercise_2+Smoking_2+Drinking_2+
                strata(prov_lab)+Elevation+Temp_sd_lastyear_1c+Temp_mean_lastyear_1c,
              data=aaa))
# km curve stratification ----
library(survival)
library(survminer)
library(ggpubr)
library(dplyr)
library(patchwork)
stra_variables1
stra_variables_pop
table(dt4_adl_mmse_ch4_tc$Road_lastyear_g,useNA = "ifany")
fit_rd <- survfit(Surv(survTime_year, event) ~ Road_lastyear_g,
                     data = dt4_adl_mmse_ch4_tc)
ggsurvplot(fit_rd)
survfit2 <- survfit(Surv(survTime_year,event) ~ 1, data = dt4)
png(filename = "figure/kmcurve.png",res=200,width = 1000, height = 800 )
ggsurvplot(fit_rd) +
  labs(
    x = "Years",
    fontsize=5
  )
dev.off()

dt4_adl_mmse_ch4_tc$Road_lastyear_2g <- cut2(dt4_adl_mmse_ch4_tc$Road_lastyear_10k,
                                             g=2)
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>%
  mutate(GDP2010_4g = factor(
    GDP2010_4g,
    levels = c("[ 0.06,  6.41)", "[ 6.41, 14.67)", "[14.67, 41.32)", "[41.32,252.31]")
  ))
table1(~Road_lastyear_10k |Road_lastyear_5g,dt4_adl_mmse_ch4_tc)
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>% mutate(Road_lastyear_5g = ifelse(Road_lastyear_10k==0,1,
                                                          ifelse(Road_lastyear_10k>0 &  Road_lastyear_10k<=1.472,2,
                                                                 ifelse(Road_lastyear_10k>1.472 &  Road_lastyear_10k<=3.529,3,
                                                                        ifelse(Road_lastyear_10k>3.529 &  Road_lastyear_10k<=10,4,
                                                                               ifelse(Road_lastyear_10k>10,5,NA))))))
dt4_adl_mmse_ch4_tc <- dt4_adl_mmse_ch4_tc %>% mutate(Road_lastyear_5g = factor(
  Road_lastyear_5g,
  levels = 1:5,
  labels = c(
    "0",                     # 1
    ">0–1.472",              # 2
    "1.472–3.529",           # 3
    "3.529–10",              # 4
    ">10"                    # 5
  ))
)
levels(data_use$Road_lastyear_5g)
data_use <- dt4_adl_mmse_ch4_tc

# Stratification variables
stra_variables1 <- c("NDVI_lastyear_4g", "NegativePM25_4g", "NegativeNO2_4g",
                     "GDP2010_4g", "Population_lastyear_4g", "CISI_4g",
                     "Energy_4g", "Education_4g", "Healthcare_4g")
summary(data_use[,stra_variables1])

# Create output list for plots
table(data_use$Road_lastyear_2g,data_use$GDP2010_4g)

ggsurvplot(survfit(Surv(survTime_year, event) ~ Road_lastyear_2g,
                  data = data_use[data_use$GDP2010_4g=="[ 0.06,  6.41)",]))
ggsurvplot(survfit(Surv(survTime_year, event) ~ Road_lastyear_2g,
                   data = data_use[data_use$GDP2010_4g=="[41.32,252.31]",]))
ggsurvplot(survfit(Surv(survTime_year, event) ~ Road_lastyear_2g,data=data_use))
summary(data_use[data_use$GDP2010_4g=="[ 0.06,  6.41)",]$Road_lastyear_2g)

# make GDP nicely ordered
table1(~Road_lastyear_10k |Road_lastyear_5g,data_use)
levels(data_use$Road_lastyear_5g)
# 1) define a global named palette for ALL 5 road levels
# master colors stay with the full survminer names

# master palette keyed by YOUR factor labels
road_cols_named <- c(
  "0"            = "#1F78B4",
  ">0–1.472"     = "#33A02C",
  "1.472–3.529"  = "#FF7F00",
  "3.529–10"     = "#E31A1C",
  ">10"          = "#6A3D9A"
)

make_km_panel <- function(df, gdp_label) {
  # drop unused road levels in this GDP group, but keep order
  df <- droplevels(df)
  
  # what road groups are present here?
  levs  <- levels(df$Road_lastyear_5g)           # e.g. GDP4 -> ">0–1.472" ... ">10"
  cols  <- road_cols_named[levs]                 # pick only those colors, in that order
  
  fit <- survfit(Surv(survTime_year, event) ~ Road_lastyear_5g, data = df)
  
  p <- ggsurvplot(
    fit,
    data = df,
    conf.int = TRUE,
    risk.table = TRUE,
    risk.table.y.text = FALSE,
    risk.table.title = NULL,
    xlab = "Follow-up, years",
    ylab = "Survival probability",
    legend.title = "Road density",
    legend.labs  = levs,         # legend shows clean labels
    palette      = cols,         # 👈 same colors go to KM + risk table
    ggtheme = theme_bw(),
    title = gdp_label
  )
  
  # tidy risk table axis label
  p$table <- p$table +
    ylab(NULL) +
    theme(axis.title.y = element_blank())
  
  p
}

# split data by GDP and build 4 panels
p1_gdp1 <- make_km_panel(
  subset(data_use, GDP2010_4g == "[ 0.06,  6.41)"),
  "GDP Q1"
)
p1_gdp1
p2_gdp2 <- make_km_panel(
  subset(data_use, GDP2010_4g == "[ 6.41, 14.67)"),
  "GDP Q2"
)
p2_gdp2

p3_gdp3 <- make_km_panel(
  subset(data_use, GDP2010_4g == "[14.67, 41.32)"),
  "GDP Q3"
)
p3_gdp3
p4_gdp4 <- make_km_panel(
  subset(data_use, GDP2010_4g == "[41.32,252.31]"),
  "GDP Q4"
)
p4_gdp4

g1 <- (p1_gdp1$plot / p1_gdp1$table) + plot_layout(heights = c(4, 1.3))
g2 <- (p2_gdp2$plot / p2_gdp2$table) + plot_layout(heights = c(4, 1.3))
g3 <- (p3_gdp3$plot / p3_gdp3$table) + plot_layout(heights = c(4, 1.3))
g4 <- (p4_gdp4$plot / p4_gdp4$table) + plot_layout(heights = c(4, 1.3))

ggsave(filename = "figure/KM_p1_gdp1.png",  plot     = g1,
  width    = 6, height   = 6, 
  dpi      = 300)
ggsave(filename = "figure/KM_p2_gdp2.png",  plot     = g2,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_p3_gdp3.png",  plot     = g3,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_p4_gdp4.png",  plot     = g4,
       width    = 6, height   = 6, 
       dpi      = 300)
# Urban vs rural
p1_urban <- make_km_panel(
  subset(data_use, Residence == "Urban"),
  "Residence: Urban"
)
p1_urban
p1_rural <- make_km_panel(
  subset(data_use, Residence == "Rural"),
  "Residence: Rural"
)
p1_rural

r1 <- (p1_urban$plot / p1_urban$table) + plot_layout(heights = c(4, 1.3))
r2 <- (p1_rural$plot / p1_rural$table) + plot_layout(heights = c(4, 1.3))
ggsave(filename = "figure/KM_p1_urban.png",  plot     = r1,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_p1_rural.png",  plot     = r2,
       width    = 6, height   = 6, 
       dpi      = 300)


# High vs low NO2
table(data_use$NegativeNO2_4g)
p_no2_1 <- make_km_panel(
  subset(data_use, NegativeNO2_4g == "[-9.917,-2.0347)"),
  "Negative NO2: Q1"
)
p_no2_2 <- make_km_panel(
  subset(data_use, NegativeNO2_4g == "[-2.035,-1.4431)"),
  "Negative NO2: Q2"
)
p_no2_3 <- make_km_panel(
  subset(data_use, NegativeNO2_4g == "[-9.917,-2.0347)"),
  "Negative NO2: Q1"
)
p_no2_4 <- make_km_panel(
  subset(data_use, NegativeNO2_4g == "[-2.035,-1.4431)"),
  "Negative NO2: Q2"
)


# High vs low infrastructure
table(data_use$Infras_healthcare100_4g)
p_healthcare_1 <- make_km_panel(
  subset(data_use, Infras_healthcare100_4g == "0"),
  "Healthcare Infrastructure Intensity: 0"
)
p_healthcare_2 <- make_km_panel(
  subset(data_use, Infras_healthcare100_4g == "[0.0089,0.0254)"),
  "Healthcare Infrastructure Intensity: [0.0089,0.0254)"
)
p_healthcare_3 <- make_km_panel(
  subset(data_use, Infras_healthcare100_4g == "[0.0254,0.0942)"),
  "Healthcare Infrastructure Intensity: [0.0254,0.0942)"
)
p_healthcare_4 <- make_km_panel(
  subset(data_use, Infras_healthcare100_4g == "[0.0942,5.2841]"),
  "Healthcare Infrastructure Intensity: [0.0942,5.2841]"
)

hc1 <- (p_healthcare_1$plot / p_healthcare_1$table) + plot_layout(heights = c(4, 1.3))
hc2 <- (p_healthcare_2$plot / p_healthcare_2$table) + plot_layout(heights = c(4, 1.3))
hc3 <- (p_healthcare_3$plot / p_healthcare_3$table) + plot_layout(heights = c(4, 1.3))
hc4 <- (p_healthcare_4$plot / p_healthcare_4$table) + plot_layout(heights = c(4, 1.3))

ggsave(filename = "figure/KM_healthcare1_gdp1.png",  plot = hc1,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_healthcare2_gdp2.png",  plot = hc2,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_healthcare3_gdp3.png",  plot = hc3,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_healthcare4_gdp4.png",  plot = hc4,
       width    = 6, height   = 6, 
       dpi      = 300)


table(data_use$Infras_education100_4g)
p_education_1 <- make_km_panel(
  subset(data_use, Infras_education100_4g == "0"),
  "Education Infrastructure Intensity: 0"
)
p_education_2 <- make_km_panel(
  subset(data_use, Infras_education100_4g == "[0.00228, 0.646)"),
  "Education Infrastructure Intensity: [0.00228, 0.646)"
)
p_education_3 <- make_km_panel(
  subset(data_use, Infras_education100_4g == "[0.64635, 2.525)"),
  "Education Infrastructure Intensity: [0.64635, 2.525)"
)
p_education_4 <- make_km_panel(
  subset(data_use, Infras_education100_4g == "[2.52483,23.167]"),
  "Education Infrastructure Intensity: [2.52483,23.167]"
)

edu1 <- (p_education_1$plot / p_education_1$table) + plot_layout(heights = c(4, 1.3))
edu2 <- (p_education_2$plot / p_education_2$table) + plot_layout(heights = c(4, 1.3))
edu3 <- (p_education_3$plot / p_education_3$table) + plot_layout(heights = c(4, 1.3))
edu4 <- (p_education_4$plot / p_education_4$table) + plot_layout(heights = c(4, 1.3))

ggsave(filename = "figure/KM_education1_gdp1.png",  plot = edu1,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_education2_gdp2.png",  plot = edu2,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_education3_gdp3.png",  plot = edu3,
       width    = 6, height   = 6, 
       dpi      = 300)
ggsave(filename = "figure/KM_education4_gdp4.png",  plot = edu4,
       width    = 6, height   = 6, 
       dpi      = 300)

# water -----
# "dist_to_coast_km","dist_to_river_km","coast_dist_cat","river_dist_cat"
cov_climate2
summary(coxph(Surv(survTime_year,event) ~ PM25_lastyear_10_neg+ 
                coast_dist_cat + Temp_sd_lastyear_1c+Temp_mean_lastyear_1c+
                Elevation+
                age + Gender + 
                Education + Marriage + Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                strata(prov_lab),
              data=dt4_adl_mmse_ch4_tc))
summary(coxph(Surv(survTime_year,event) ~ river_dist_cat+Road_lastyear_10k + NO2_chap_lastyear_10+
                GDP2010_buffer005mean_100+Pop_lastyear_100 + Elevation_100 + 
                age + Gender + 
                Education + Marriage + Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                Residence + strata(prov_lab),
              data=dt4_adl_mmse_ch4_tc))
spldt <- dt4_adl_mmse_ch4_tc
library(rms)
dd <- datadist(spldt)
options(datadist='dd')
cox_sf_coast <- cph(Surv(survTime_year, event) ~ rcs(dist_to_coast_km,3) + 
                      Road_lastyear_10k + NO2_chap_lastyear_10+
                   age + Gender + Education + Marriage +
                   Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                   Residence + GDP2010_buffer005mean_100 + Pop_lastyear_100,
                 data=spldt)
cox_sf_coast2 <- cph(Surv(survTime_year, event) ~ rcs(dist_to_coast_km,3),
                    data=spldt)
plot(Predict(cox_sf_coast, dist_to_coast_km, ref.zero = TRUE,fun=exp), ylab="HR (95% CI)")
plot(Predict(cox_sf_coast2, dist_to_coast_km, ref.zero = TRUE,fun=exp), ylab="HR (95% CI)")

cox_sf_river <- cph(Surv(survTime_year, event) ~ rcs(dist_to_river_km,3) + 
                      Road_lastyear_10k + NO2_chap_lastyear_10+
                      age + Gender + Education + Marriage +
                      Regular_exercise_2 + Smoking_2 + Drinking_2 + 
                      Residence + GDP2010_buffer005mean_100 + Pop_lastyear_100,
                    data=spldt)
plot(Predict(cox_sf_river, dist_to_river_km, ref.zero = TRUE,fun=exp), ylab="HR (95% CI)")

near_water <- c("near_coast_100km","near_river1j_20km","dist_to_coast_km_neg","dist_to_river_km_neg")
model_satu_water <- bind_rows(bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=NULL,
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="A Unadjusted"),
                              bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=cov_demo,
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="B:A+Demography+Lifestyle"),
                              bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_climate),                           
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="C:B+Climate"),
                              bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_climate,cov_eco),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="D:C+Economy"),
                              bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_climate,cov_eco,cov_ap
                                               ),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="E:D+AP (no road)"),
                              bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_climate,cov_eco,
                                                     cov_rd),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="F:D+Road (no AP)"),
                              bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_climate,cov_eco,
                                                     cov_ap,cov_rd
                                               ),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="G:D+AP+Road"),
                              bind_rows(lapply(near_water,
                                               cox_f,
                                               x2=NULL,
                                               cov=c(cov_demo,cov_climate,cov_eco,
                                                     cov_ap,cov_rd,cov_adlmmse),
                                               dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="H:G+Adl+Cognition")
)





