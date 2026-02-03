library(dplyr)
library(table1)
library(Hmisc)
library(ggplot2)
library(survival)
library(survminer)
# load demo data 2
dt_demo <- read.csv("demo_data2.csv")

table1(~ Age.0.0 + Sex.0.0 + edu.0.0_high + Income.0.0 +
         Smoking + Alcohol + 
         urban_2g + 
         NO2_2010 + PM25_2010  + NDVI_500m_mean_2+ IMD_INCOME_SCORE_060708 +country+
         Den_CE + Den_CM + Den_CT+ Den_CU+ Den_LP, dt_demo) 

# Data-Field 24015 Description:	Sum of road length of major roads within 100m  (2008.
# Sum of road length of major roads from the central road network within a 100 metre circular buffer of residential address. The Central road network is taken from Eurostreets version 3.1 digital road network (scale 1:10000), derived from 
# the TeleAtlas MultiNet TM dataset for the year 2008. All roads of class 0, 1and 2 (motorways, main roads of major importance and other main roads) were classified as major roads. Based upon local knowledge classes 3 and 4 (secondary roads and local connecting roads) were also classified as major roads.
# Data-Field 40000 escription:	Date of death
# death time 2008 beyond
# summary(dt$f.40000.0.0)
# summary(dt$date_survey.0.0)
# 1. exclude participants before 2008 ----
# dt1 <- subset(dt,date_survey.0.0>=as.Date("2008-01-01"))

# survival time calculation ----
# summary(dt1$date_survey.0.0)
# table(substr(dt1$f.40000.0.0,1,4))

# dt1 <- dt1 %>%
#   mutate(
#     end_date = if_else(
#       !is.na(f.40000.0.0),
#       f.40000.0.0,
#       as.Date("2021-01-01")
#     ),
#     dtime = as.numeric(difftime(end_date, date_survey.0.0, units = "days")) / 365.25,
#     death = ifelse(!is.na(f.40000.0.0), 1, 0)
#   )

# class(dt1$end_date)
# summary(dt1$dtime)
# table(dt1$death)
# View(dt1[500:520,c("f.eid","date_survey.0.0", "f.40000.0.0","end_date", "dtime", "death")])

# cox model ----
cox_f <- function(x1,x2=NULL,cov,dataset,category){
  f <- reformulate(c(x1,x2,cov),
                   response="Surv(dtime, death)")
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

env_col2 <- c("NO2_2010_neg", "PM25_2010_neg","NDVI_500m_mean_2_0.1",
              "roadlength_10", "MEAN_Length_R400c_1000", "MEAN_Links_R400c_10",
              "Den_CE_nonzero", "Den_CM_nonzero", "Den_CT_nonzero", "Den_CU_nonzero", "Den_LP_nonzero",
              "IMD_INCOME_neg")

Env_results2 <- bind_rows(bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=NULL,
                                           dataset = dt_demo,category = 1))  %>% mutate(Model="1 Unadjusted"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0"),
                                           dataset = dt_demo,category = 1)) %>% mutate(Model="1+AgeSex"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol"),
                                           dataset = dt_demo,category = 1)) %>% mutate(Model="2+EduHouseIncLifes"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)"),
                                           dataset = dt_demo,category = 1)) %>% mutate(Model="3+Geography"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)",
                                                 "IMD_INCOME_neg"),
                                           dataset = dt_demo,category = 1)) %>% mutate(Model="4+Economy"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)",
                                                 "IMD_INCOME_neg",
                                                 "PM25_2010_neg","NO2_2010_neg"),
                                           dataset = dt_demo,category = 1)) %>% mutate(Model="5+AirQuality"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)",
                                                 "IMD_INCOME_neg",
                                                 "PM25_2010_neg","NO2_2010_neg",
                                                 "roadlength_10"),
                                           dataset = dt_demo,category = 1)) %>% mutate(Model="6+Road")
)


# define clean labels for all variables included in env_col
env_labels <- c(
  # Natural environment
  "NDVI_500m_mean_2_0.1" = "NDVI",
  "PM25_2010_neg" = "Negative PM2.5",
  "NO2_2010_neg" = "Negative NO2",
  
  # Built environment
  "roadlength_10" = "Road length",
  "MEAN_Length_R400c_1000" = "Street length",
  "MEAN_Links_R400c_10" = "Street links",
  "Den_CE_nonzero" = "Education LUD",
  "Den_CM_nonzero" = "Medical LUD",
  "Den_CT_nonzero" = "Transport LUD",
  "Den_CU_nonzero" = "Utility LUD",
  "Den_LP_nonzero" = "Parks LUD",
  
  # Economic context
  "IMD_INCOME_neg" = "Local income"
)
# pepare data for plotting
Env_results_plot_data <- Env_results2 %>%
  mutate(term_label = recode(term, !!!env_labels),    
         term_label = factor(term_label, levels = rev(unname(env_labels))),
         Model = factor(Model, 
                        levels = c( "1 Unadjusted","1+AgeSex","2+EduHouseIncLifes","3+Geography",
                                    "4+Economy","5+AirQuality","6+Road")))

colnames(Env_results_plot_data)

Env_results_plot_data <- Env_results_plot_data %>%
  mutate(x_id = as.numeric(term_label))

Env_results_plot_data <- Env_results_plot_data %>% mutate(sig = ifelse(p.value < 0.05, "*", "")) 
View(Env_results_plot_data)
unique(Env_results_plot_data$term)

# forest plot
Env_plot <- ggplot(
  Env_results_plot_data,
  aes(x = Model, y = estimate, ymin = conf.low, ymax = conf.high, color = Model)
) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray50") +
  geom_errorbar(position = position_dodge(width = 0.5),width=0.2) +
  geom_point(position = position_dodge(width = 0.5),size=1) +
  facet_wrap(
    ~ term_label,
    scales = "free_y",
    ncol = 3
  ) +
  scale_color_manual(values = c(
    "1 Unadjusted" = "#0072B2",
    "1+AgeSex" = "#CC79A7",
    "2+EduHouseIncLifes" = "#009E73",
    "3+Geography" = "#D55E00",
    "4+Economy" = "#E69F00",
    "5+AirQuality" = "#8c564b",
    "6+Road" = "#4D4D4D"
  )) +
  labs(
    y = "HR (95% CI)",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),

    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.6
    ),
    strip.background = element_rect(
      "grey90", color = NA
    ),
    
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
Env_plot
ggsave(Env_plot,file="figure/Env_plot.png", height=9, width=8,
       dpi=300)

# check ----
summary(coxph(Surv(dtime,death) ~MEAN_Links_R400c_10 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high + Smoking + Alcohol + 
                urban_2g + NO2_2010 + PM25_2010 + IMD_INCOME_SCORE_060708+strata(Assesscenter.0.0),
              data=dt_demo))

summary(coxph(Surv(dtime,death) ~MEAN_Links_R400c_10 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high + Smoking + Alcohol + 
                urban_2g + NO2_2010 + PM25_2010 + IMD_INCOME_SCORE_060708+strata(Assesscenter.0.0)+
                roadlength,
              data=dt_demo))
