# road and Mortality
library(dplyr)
library(table1)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(tidyr)
library(svglite)
library(gridExtra)
library(psych)
library(survival)
library(survminer)
library(purrr)
library(patchwork)
---------------------------------------------------------------------
urban <- urban %>% mutate(urban = ifelse(f.20118.0.0 %in% c(1,5,11,12),"Urban",
                                         ifelse(f.20118.0.0 %in% c(2,6,13,14,15),"Town",
                                                ifelse(f.20118.0.0 %in% c(3,4,7,8,16,17,18),"Rural",NA
                                                ))))
urban <- urban %>% mutate(urban_2g = ifelse(is.na(f.20118.0.0),NA,
                                            ifelse(f.20118.0.0 %in% c(1,5,11,12),"Urban","Other"
                                            )))

UKB_NDVI[abs(UKB_NDVI$NDVI_500m_mean)>1, c("f.eid","NDVI_500m_mean")]%>% View()
UKB_NDVI$NDVI_500m_mean_2 <- ifelse(abs(UKB_NDVI$NDVI_500m_mean)>1,NA,UKB_NDVI$NDVI_500m_mean)

dt <- merge(dt,
            street12_50m_mean %>% select(-Encoded.anonymised.participant.ID),
            by="f.eid",
            all.x=T)

dt <- merge(dt,
            LocalEnv,
            by="f.eid",
            all.x = T)

# socialdemo col
col_ses <- c("Age.0.0","Sex.0.0","Income.0.0","edu.0.0_high","Ethnic.0.0_2g",
             "TDI.0.0") # ,"TDI.0.0_quintile"
dt <- merge(dt,
            SociaDemo_clean,
            by="f.eid",
            all.x = T)

dt <- merge(dt,
            reception[,c("f.eid","date_survey.0.0","date_survey.1.0","date_survey.2.0","date_survey.2.0")],
            by="f.eid",
            all.x = T)

dt <- merge(dt,
            urban[,c("f.eid","f.20118.0.0","urban","urban_2g")],
            by="f.eid",
            all.x = T)

# lifestyle col
col_lifestyle <- c("IPAQ_activity","IPAQ_activity_2g","MET_score","MET_define",
                   "Sleephour","Sleephour_3g","Smoking","Alcohol")
dt <- merge(dt,
            PhysicalActivity,
            by="f.eid",
            all.x = T)
dt$Sleephour_3g <- as.factor(dt$Sleephour_3g)

dt <- merge(dt,
            UKB_NDVI,
            by="f.eid",
            all.x = T)

dt <- merge(dt,
            Lifestyle_clean,
            by="f.eid",
            all.x=T)

dt <- dt %>% rename(PM25_2010 = f.24006.0.0,
                    NO2_2010 = f.24003.0.0,
                    NOx_2010 = f.24004.0.0,
                    PM10_2010 = f.24005.0.0,
                    PM25_10_2010 = f.24008.0.0,
                    roadlength = f.24015.0.0)
table(dt$Smoking,useNA = "ifany")
dt <- dt %>%  mutate(MEAN_Links_R400c_10 = MEAN_Links_R400c/10,
                     MEAN_Length_R400c_100 = MEAN_Length_R400c/100)
colnames(dt)
# Data-Field 24015 Description:	Sum of road length of major roads within 100m  (2008.
# Sum of road length of major roads from the central road network within a 100 metre circular buffer of residential address. The Central road network is taken from Eurostreets version 3.1 digital road network (scale 1:10000), derived from 
# the TeleAtlas MultiNet TM dataset for the year 2008. All roads of class 0, 1and 2 (motorways, main roads of major importance and other main roads) were classified as major roads. Based upon local knowledge classes 3 and 4 (secondary roads and local connecting roads) were also classified as major roads.
summary(dt$f.24015.0.0)


# Data-Field 40000 escription:	Date of death
# death time 2008 beyond
summary(dt$f.40000.0.0)
summary(dt$date_survey.0.0)


# merge IMD, assessment center, infrastructure ----
dt <- merge(dt,
               UKB_IMD_income_employ_merged,
               by="f.eid", 
               all.x = T)

dt <- merge(dt,
               AssessCenter,
               by="f.eid", 
               all.x = T)

NM_LUD_col <- c("Den_CA01", "Den_CB", "Den_CC", "Den_CE", "Den_CH", "Den_CI01", "Den_CI02",
                "Den_CI03", "Den_CI04", "Den_CI05", "Den_CI06", "Den_CI07", "Den_CI08", "Den_CL02", 
                "Den_CL03", "Den_CL04", "Den_CL06", "Den_CL07", "Den_CL10", "Den_CM", "Den_CN", 
                "Den_CO01", "Den_CR01", "Den_CR02", "Den_CR04", "Den_CR05", "Den_CR06", "Den_CR07", 
                "Den_CR08", "Den_CR09", "Den_CR10", "Den_CR11", "Den_CS", "Den_CT", "Den_CU", "Den_CX",
                "Den_CZ", "Den_LL", "Den_LM", "Den_LO", "Den_LP", "Den_LU", "Den_LW", "Den_M", "Den_R", "Den_Z")

# definition
# Den_CA01
# Agricultural Farm / Non-Residential  Associated Building
# Den_CC
# Community 
# Services
# Den_ CE
# Education
# Den_ CH
# Hotel / Motel 
# / Boarding / 
#   Guest House
# Den_ CI01
# Factory/Manufacturing
# Den_ CL06
# Indoor / Outdoor 
# Leisure / Sporting 
# Activity / Centre
# Den_ CL07
# Bingo Hall / Cinema / 
#   Conference / 
#   Exhibition Centre / 
#   Theatre / Concert Hall
# Den_ CM
# Medical
# Den_ CR01
# Bank / Financial Service
# Den_ CR04
# Market
# Den_ CT
# Transport
# Den_ CU
# Utility (electricity, water, waste, telecommunications, etc.)
# Den_LP
# Park

dt <- merge(dt,
               UKB_LU_Density_LSOAs_den[,c("f.eid",NM_LUD_col)],
               by="f.eid",
               all.x = T)
summary(dt[,NM_LUD_col])

table1(~ Age.0.0 + Sex.0.0 + edu.0.0_high + Income.0.0 +
         Smoking + Alcohol + 
         urban_2g + 
         NO2_2010 + PM25_2010  + NDVI_500m_mean_2+ IMD_INCOME_SCORE_060708 + IMD_INCOME_SCORE_060708_Decile+country+
         Den_CE + Den_CM + Den_CT+ Den_CU+ Den_LP, dt) 

# 1. exclude participants before 2008 ----
dt1 <- subset(dt,date_survey.0.0>=as.Date("2008-01-01"))


# survival time calculation ----
summary(dt1$date_survey.0.0)
table(substr(dt1$f.40000.0.0,1,4))


dt1 <- dt1 %>%
  mutate(
    end_date = if_else(
      !is.na(f.40000.0.0),
      f.40000.0.0,
      as.Date("2021-01-01")
    ),
    dtime = as.numeric(difftime(end_date, date_survey.0.0, units = "days")) / 365.25,
    death = ifelse(!is.na(f.40000.0.0), 1, 0)
  )

class(dt1$end_date)
summary(dt1$dtime)
table(dt1$death)

View(dt1[500:520,c("f.eid","date_survey.0.0", "f.40000.0.0","end_date", "dtime", "death")])

# 2. exclude road length missing ----
dt1_0 <- subset(dt1, !is.na(roadlength))

table1(~dtime+ factor(death) + Age.0.0 + Sex.0.0 + edu.0.0_high + Income.0.0 +
        Smoking + Alcohol + 
         urban_2g + 
         NO2_2010 + PM25_2010  + NDVI_500m_mean_2+ IMD_INCOME_SCORE_060708 + IMD_INCOME_SCORE_060708_Decile+country, dt1_0) 
# 3. exclude missing in household income,smoking, alcohol ----
dt1_0 <- subset(dt1_0,  !is.na(Income.0.0) & !is.na(Smoking) & !is.na(Alcohol))

# 4. exclude  scotland (no PM2.5) and missing in urban, IMD_income ----
summary(dt1_0[dt1_0$country %in% "Scotland", ]$PM25_2010)

dt1_0 <- subset(dt1_0, country!="Scotland" & !is.na(urban_2g) & !is.na(PM25_2010) & !is.na(IMD_INCOME_SCORE_060708))

# 5. exclude  missing in street length 405033 ----
dt1_0 <- subset(dt1_0,  !is.na(MEAN_Length_R400c_100))


# table 1 ----
dt1_0$IMD_INCOME_neg = -dt1_0$IMD_INCOME_SCORE_060708
dt1_0$NO2_2010_neg = -dt1_0$NO2_2010
dt1_0$PM25_2010_neg = -dt1_0$PM25_2010

dt1_0$Age_group <- cut(
  dt1_0$Age.0.0,
  breaks = c(38,45, 55, 65, 72),
  labels = c("37-45", "46–55", "56–65", "66–72"),
  include.lowest = TRUE, 
  right = TRUE          
)
table1(~ Age.0.0|Age_group,data=dt1_0)

my.render.cont <- function(x) {
  x <- stats::na.omit(x)
  q <- quantile(x, probs = c(0.25, 0.5, 0.75))
  c(
    "Range" = sprintf("%.2f – %.2f", min(x), max(x)),
    "Mean (SD)" = sprintf("%.2f (%.2f)", mean(x), sd(x)),
    "Median [P25, P75]" = sprintf("%.2f [%.2f, %.2f]", q[2], q[1], q[3])
  )
}

table1( ~ dtime+ factor(death) + Age.0.0 + Age_group + Sex.0.0 + edu.0.0_high + Income.0.0 + Smoking + Alcohol + 
         urban_2g + NO2_2010_neg + PM25_2010_neg+ 
         IMD_INCOME_neg + IMD_INCOME_SCORE_060708_Decile+
         roadlength + MEAN_Length_R400c_100 + MEAN_Links_R400c_10 + 
         NDVI_500m_mean_2 + Den_CE + Den_CM + Den_CT + Den_CU + Den_LP+
         country+Assesscenter.0.0, 
       dt1_0,render.continuous=my.render.cont)

# correlations ----
library(corrplot)
M2 <- cor(dt1_0[, c("PM25_2010","NO2_2010", "NDVI_500m_mean_2",
                    "roadlength", "MEAN_Length_R400c_100", "MEAN_Links_R400c_10", 
                    "Den_CE", "Den_CM", "Den_CT",
                    "Den_CU", "Den_LP",
                    "IMD_INCOME_neg")],
          use = "pairwise.complete.obs")


# Create clean labels
clean_labels <- c(
  "PM25_2010" = "PM2.5",
  "NO2_2010" = "NO2",
  "NDVI_500m_mean_2" = "NDVI",
  "roadlength" = "Major road density",
  "MEAN_Length_R400c_100" = "Street length",
  "MEAN_Links_R400c_10" = "Street links",
  "Den_CE" = "Education LUD",
  "Den_CM" = "Medical LUD",
  "Den_CT" = "Transport LUD",
  "Den_CU" = "Utilities LUD",
  "Den_LP" = "Parks LUD",
  "IMD_INCOME_neg" = "Negative income\ndeprivation"
)

# Apply cleaned labels to the correlation matrix
colnames(M2) <- rownames(M2) <- clean_labels[colnames(M2)]
getwd()
svglite::svglite(filename = "figure/cor_plot_ukb_3.svg", width = 7, height = 6)
corrplot(M2,
         method = "color",
         tl.cex = 0.8,         # Variable label font size
         tl.srt = 90,        # Variable label rotation
         tl.col = "black",   # Variable label color
         mar = c(0,0,0,0),   # Margin
         # cl.cex = 1.5        # Color legend font size (increase as needed)
)
dev.off()
plot(dt1_0$IMD_INCOME_SCORE_060708,dt1_0$NO2_2010)
cor(dt1_0$IMD_INCOME_SCORE_060708,dt1_0$NO2_2010)

# K-M curve ----
dt1_0 <- dt1_0 %>% mutate(roadlength_3g= ifelse(roadlength==0,"No major road",
                                            ifelse(roadlength<100,"<100","≥100")))
table(dt1_0$roadlength_3g)

km_fit_rd <- survfit(Surv(dtime, death) ~ roadlength_3g, 
                     data = dt1_0)
ggsurvplot(km_fit_rd, 
           data = dt1_0, 
           xlab = "Time (Year)", 
           ylab = "Survival Probability", 
           ylim = c(0.9, 1),
           conf.int = TRUE, 
           pval = TRUE, 
           risk.table = TRUE, 
           ggtheme = theme_minimal())


# cox model ----
library(survival)
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

cox_int_f <- function(x1,x2,cov,dataset,int) {
  if (int==1) {f <- reformulate(c(x1,x2,paste(x1,x2,sep="*"),cov),
                                response="Surv(dtime, death)")}
  else {f <- reformulate(c(x1,x2,cov),
                         response="Surv(dtime, death)")}
  
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


dt1_0 <- dt1_0 %>%
  mutate(
    Den_CE_log = ifelse(Den_CE == 0, 0, log1p(Den_CE)),  # log(1+x) to avoid Inf
    Den_CE_nonzero = ifelse(Den_CE == 0, 0,1),
    
    Den_CM_log = ifelse(Den_CM == 0, 0, log1p(Den_CM)),  
    Den_CM_nonzero = ifelse(Den_CM == 0, 0,1),
    
    Den_CT_log = ifelse(Den_CT == 0, 0, log1p(Den_CT)), 
    Den_CT_nonzero = ifelse(Den_CT == 0, 0,1),
    
    Den_CU_log = ifelse(Den_CU == 0, 0, log1p(Den_CU)), 
    Den_CU_nonzero = ifelse(Den_CU == 0, 0,1),
    
    Den_LP_log = ifelse(Den_LP == 0, 0, log1p(Den_LP)),  
    Den_LP_nonzero = ifelse(Den_LP == 0, 0,1)
  )

dt1_0 <- dt1_0 %>%
  mutate(NDVI_500m_mean_2_0.1 = NDVI_500m_mean_2*10)
summary(dt1_0$IMD_INCOME_neg)
dt1_0 <- dt1_0 %>%
  mutate(MEAN_Length_R400c_1000 = MEAN_Length_R400c/1000)
dt1_0 <- dt1_0 %>%
     mutate(roadlength_10 = roadlength/10)

env_col2 <- c("NO2_2010_neg", "PM25_2010_neg","NDVI_500m_mean_2_0.1",
              "roadlength_10", "MEAN_Length_R400c_1000", "MEAN_Links_R400c_10",
              "Den_CE_nonzero", "Den_CM_nonzero", "Den_CT_nonzero", "Den_CU_nonzero", "Den_LP_nonzero",
              "IMD_INCOME_neg")

Env_results2 <- bind_rows(bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=NULL,
                                           dataset = dt1_0,category = 1))  %>% mutate(Model="1 Unadjusted"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0"),
                                           dataset = dt1_0,category = 1)) %>% mutate(Model="1+AgeSex"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol"),
                                           dataset = dt1_0,category = 1)) %>% mutate(Model="2+EduHouseIncLifes"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)"),
                                           dataset = dt1_0,category = 1)) %>% mutate(Model="3+Geography"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)",
                                                 "IMD_INCOME_neg"),
                                           dataset = dt1_0,category = 1)) %>% mutate(Model="4+Economy"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)",
                                                 "IMD_INCOME_neg",
                                                 "PM25_2010_neg","NO2_2010_neg"),
                                           dataset = dt1_0,category = 1)) %>% mutate(Model="5+AirQuality"),
                          bind_rows(lapply(env_col2,
                                           cox_f,
                                           x2=NULL,
                                           cov=c("Age.0.0","Sex.0.0","edu.0.0_high","Income.0.0","Smoking","Alcohol",
                                                 "urban_2g","strata(Assesscenter.0.0)",
                                                 "IMD_INCOME_neg",
                                                 "PM25_2010_neg","NO2_2010_neg",
                                                 "roadlength_10"),
                                           dataset = dt1_0,category = 1)) %>% mutate(Model="6+Road")
)


# define clean labels for all variables included in env_col
env_labels <- c(
  # Natural environment
  "NDVI_500m_mean_2_0.1" = "NDVI",
  "PM25_2010_neg" = "Negative\nPM2.5",
  "NO2_2010_neg" = "Negative\nNO2",
  
  # Built environment
  "roadlength_10" = "Road\nlength",
  "MEAN_Length_R400c_1000" = "Street\nlength",
  "MEAN_Links_R400c_10" = "Street\nlinks",
  "Den_CE_nonzero" = "Education\nLUD",
  "Den_CM_nonzero" = "Medical\nLUD",
  "Den_CT_nonzero" = "Transport\nLUD",
  "Den_CU_nonzero" = "Utility\nLUD",
  "Den_LP_nonzero" = "Parks LUD",
  
  # Economic context
  "IMD_INCOME_neg" = "Local\nincome"
)

env_labels4 <- c(
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
  mutate(term_label = recode(term, !!!env_labels4),    
         term_label = factor(term_label, levels = rev(unname(env_labels4))),
         Model = factor(Model, 
                        levels = c( "1 Unadjusted","1+AgeSex","2+EduHouseIncLifes","3+Geography",
                                    "4+Economy","5+AirQuality","6+Road")))

# Check structure
colnames(Env_results_plot_data)

Env_results_plot_data <- Env_results_plot_data %>%
  mutate(x_id = as.numeric(term_label))

Env_results_plot_data <- Env_results_plot_data %>% mutate(sig = ifelse(p.value < 0.05, "*", "")) 
View(Env_results_plot_data)
unique(Env_results_plot_data$term)
Env_results_plot_data2 <- subset(Env_results_plot_data,!(term %in% c("Den_CE_nonzero","Den_CM_nonzero",
                                                                     "Den_CT_nonzero","Den_CU_nonzero","Den_LP_nonzero")))
Env_results_plot_data2 <- Env_results_plot_data2 %>%
  mutate(
    term_label = droplevels(term_label),
    x_id = as.numeric(term_label)
  )
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
env_col2
summary(coxph(Surv(dtime,death) ~MEAN_Links_R400c_10 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high + Smoking + Alcohol + 
                urban_2g + NO2_2010 + PM25_2010 + IMD_INCOME_SCORE_060708+strata(Assesscenter.0.0),
              data=dt1_0))

summary(coxph(Surv(dtime,death) ~MEAN_Links_R400c_10 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high + Smoking + Alcohol + 
                urban_2g + NO2_2010 + PM25_2010 + IMD_INCOME_SCORE_060708+strata(Assesscenter.0.0)+
                roadlength,
              data=dt1_0))

summary(coxph(Surv(dtime,death) ~ MEAN_Length_R400c_100 + Age.0.0 + Sex.0.0 ,
              data=dt1_0))
summary(coxph(Surv(dtime,death) ~ MEAN_Length_R400c_100 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high ,
              data=dt1_0))
summary(coxph(Surv(dtime,death) ~ MEAN_Length_R400c_100 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol,
              data=dt1_0))
summary(coxph(Surv(dtime,death) ~ MEAN_Length_R400c_100 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol + urban_2g + IMD_INCOME_SCORE_060708,
              data=dt1_0))
summary(coxph(Surv(dtime,death) ~ MEAN_Length_R400c_100 + Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol + urban_2g + IMD_INCOME_SCORE_060708+ 
                NO2_2010 + PM25_2010 + Assesscenter.0.0,
              data=dt1_0))


# Spline ----
library(rms)
dt1_0 <- dt1_0 %>%
  mutate(INCOME_Q = ntile(IMD_INCOME_SCORE_060708, 4))
table(dt1_0$INCOME_Q,useNA = "ifany")

library(scales)
dt1_nona <- dt1_0 %>% dplyr::select(dtime, death, INCOME_Q, Age.0.0, Sex.0.0,
                                    edu.0.0_high, Income.0.0, Smoking, Alcohol, urban_2g, IMD_INCOME_SCORE_060708,
                                    NO2_2010, PM25_2010,Assesscenter.0.0,
                                    MEAN_Links_R400c_10,roadlength_10, MEAN_Length_R400c_1000) %>%
  tidyr::drop_na()
dd <- datadist(dt1_nona)

options(datadist = 'dd')

dt1_nona <- dt1_nona %>% mutate(
  Sex.0.0 = factor(Sex.0.0,ordered=F),
  edu.0.0_high = as.factor(edu.0.0_high),
  Income.0.0 = as.factor(Income.0.0),
  Smoking = factor(Smoking,ordered=F),
  Alcohol = factor(Alcohol,ordered=F),
)

get_predict_df <- function(data, varname) {
  f <- as.formula(paste0(
    "Surv(dtime, death) ~ Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol + 
                urban_2g + IMD_INCOME_SCORE_060708+ 
                NO2_2010 + PM25_2010 + Assesscenter.0.0+
     rcs(as.numeric(", varname, "), 4)"
  ))
  fit <- rms::cph(f, data = data, x = TRUE, y = TRUE, surv = TRUE)
  p <- do.call(rms::Predict, list(fit, as.name(varname), ref.zero = TRUE, fun = exp))
  as.data.frame(p) |>
    dplyr::rename(x = !!varname) |>
    dplyr::select(x, yhat, lower, upper)
}

build_exposure_overlap <- function(varname, xlab) {
  # total
  df_total <- get_predict_df(dt1_nona, varname)
  # quartiles
  df_q1 <- get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 1), varname) |> dplyr::mutate(group = "Income Q1")
  df_q2 <- get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 2), varname) |> dplyr::mutate(group = "Income Q2")
  df_q3 <- get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 3), varname) |> dplyr::mutate(group = "Income Q3")
  df_q4 <- get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 4), varname) |> dplyr::mutate(group = "Income Q4")
  df_inc <- dplyr::bind_rows(df_q1, df_q2, df_q3, df_q4) |>
    dplyr::mutate(group = factor(group, levels = c("Income Q1","Income Q2","Income Q3","Income Q4")))
  
  # shared y limits within this exposure
  y_min <- min(df_total$lower, df_inc$lower, na.rm = TRUE)
  y_max <- max(df_total$upper, df_inc$upper, na.rm = TRUE)
  y_pad <- 0.02 * (y_max - y_min)
  y_lim <- c(max(0, y_min - y_pad), y_max + y_pad)
  
  # Total plot 
  p_total <- ggplot(df_total, aes(x = x, y = yhat)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#ADD8E6", alpha = 0.20) +  
    geom_line(color = "#4682B4", linewidth = 1) +                                    
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    scale_y_continuous(breaks = seq(floor(y_lim[1]), ceiling(y_lim[2]), by = 1)) +   
    coord_cartesian(ylim = y_lim, expand = TRUE) +
    labs(x = xlab, y = "HR (95% CI)", title = "Total sample") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  #  Quartiles plot 
  p_income <- ggplot(df_inc, aes(x = x, y = yhat, color = group, fill = group)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20, linewidth = 0) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
    scale_y_continuous(breaks = seq(floor(y_lim[1]), ceiling(y_lim[2]), by = 1)) +   
    coord_cartesian(ylim = y_lim, expand = TRUE) +
    labs(x = xlab, y = "Hazard Ratio (95% CI)", title = "Income quartiles") +
    scale_fill_manual(values = c(
      "Income Q1" = "#ADD8E6",  # Light Blue
      "Income Q2" = "#FFD700",  # Gold
      "Income Q3" = "#32CD32",  # Lime Green
      "Income Q4" = "#FF6347"   # Tomato
    )) +
    scale_color_manual(values = c(
      "Income Q1" = "#0000FF",  # Blue (line)
      "Income Q2" = "#FFA500",  # Orange
      "Income Q3" = "#006400",  # Dark Green
      "Income Q4" = "#FF0000"   # Red
    )) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_blank(),
      legend.position = c(0.05, 0.95),
      legend.justification = c("left", "top"),
      legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  list(total = p_total, income = p_income)
}

build_exposure_plots <- function(varname, xlab) {
  
  # ---- Total sample 
  df_total <- get_predict_df(dt1_nona, varname)
  
  # ---- Income quartiles (separate models)
  df_inc <- dplyr::bind_rows(
    get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 1), varname) |> mutate(group = "Income Deprivation Q1"),
    get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 2), varname) |> mutate(group = "Income Deprivation Q2"),
    get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 3), varname) |> mutate(group = "Income Deprivation Q3"),
    get_predict_df(dplyr::filter(dt1_nona, INCOME_Q == 4), varname) |> mutate(group = "Income Deprivation Q4")
  ) |>
    mutate(group = factor(group, levels = c("Income Deprivation Q1","Income Deprivation Q2",
                                            "Income Deprivation Q3","Income Deprivation Q4")))
  
  # ---- Shared y limits 
  y_min <- min(df_total$lower, df_inc$lower, na.rm = TRUE)
  y_max <- max(df_total$upper, df_inc$upper, na.rm = TRUE)
  y_pad <- 0.03 * (y_max - y_min)
  y_lim <- c(max(0, y_min - y_pad), y_max + y_pad)
  
  # ---- Total plot
  p_total <- ggplot(df_total, aes(x = x, y = yhat)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "#ADD8E6", alpha = 0.25) +
    geom_line(color = "#4682B4", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_cartesian(ylim = y_lim) +
    labs(x = xlab, y = "HR (95% CI)", title = "Total sample") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # ---- Income-stratified panels (NO overlap) 
  p_income <- ggplot(df_inc, aes(x = x, y = yhat)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "#ADD8E6", alpha = 0.25) +
    geom_line(color = "#000000", linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_cartesian(ylim = y_lim) +
    facet_wrap(~ group, nrow = 2) +
    labs(x = xlab, y = "HR (95% CI)") +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  list(total = p_total, income = p_income)
}

# --- run for  three exposures
# 1) Road length (per 10 m / 100 m label)
plots_len10 <- build_exposure_plots("roadlength_10", "Road length")
p_len10_total  <- plots_len10$total
p_len10_income <- plots_len10$income
p_len10_total
p_len10_income

# 2) Street links (per 10 links)
plots_links <- build_exposure_plots("MEAN_Links_R400c_10", "Street links")
p_links_total   <- plots_links$total
p_links_income  <- plots_links$income
p_links_total
p_links_income
# 3) Street length (per 1000 m)
plots_len1000 <- build_exposure_plots("MEAN_Length_R400c_1000", "Street length")
p_len1000_total  <- plots_len1000$total
p_len1000_income <- plots_len1000$income


p_len10_total    <- p_len10_total + labs(title = NULL)
p_len1000_total  <- p_len1000_total + labs(title = NULL)
p_links_total    <- p_links_total + labs(title = NULL)

library(patchwork)
p_spline_total <- 
  (p_len10_total + labs(title = "A. Spline for road length")) /
  (p_len1000_total + labs(title = "B. Spline for street network length")) /
  (p_links_total + labs(title = "C. Spline for street network links")) &
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 13)
  )
p_spline_total


p_spline_strat <- 
  (p_len10_income + labs(title = "A. Spline for road length")) /
  (p_len1000_income + labs(title = "B. Spline for street network length")) /
  (p_links_income + labs(title = "C. Spline for street network links")) &
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 13)
  )

p_spline_strat


getwd()
ggsave("figure/p_spline_total.png", p_spline_total, width = 5, height = 12, dpi = 300)
ggsave("figure/p_spline_strat.png", p_spline_strat, width = 9, height = 15, dpi = 300)

# stratification ----
library(survival)
library(dplyr)
library(broom)
library(purrr)

# ---- define reusable function ----
run_stratified_env <- function(data, strat_var, env_vars, covars) {
  
  data <- data %>% filter(!is.na(.data[[strat_var]]))
  
  strata_levels <- levels(factor(data[[strat_var]]))
  
  map_dfr(strata_levels, function(lvl) {
    
    dsub <- data %>% filter(.data[[strat_var]] == lvl)
    
    ## remove stratifier from covariates if present
    cov_use <- setdiff(covars, strat_var)
    
    map_dfr(env_vars, function(env) {
      
      f <- reformulate(
        c(env, cov_use),
        response = "Surv(dtime, death)"
      )
      
      fit <- try(coxph(f, data = dsub), silent = TRUE)
      
      if (inherits(fit, "try-error")) return(NULL)
      
      broom::tidy(fit, conf.int = TRUE) %>%
        filter(term == env) %>%
        mutate(
          HR = exp(estimate),
          CI_lower = exp(conf.low),
          CI_upper = exp(conf.high),
          strat_var = strat_var,
          stratum   = lvl,
          variable  = env
        ) %>%
        select(strat_var, stratum, variable, HR, CI_lower, CI_upper, p.value)
      
    })
  })
}


plot_stratified_forest <- function(res_df, strat_title) {
  
  ggplot(res_df,
         aes(x = stratum, y = HR,
             ymin = CI_lower, ymax = CI_upper,
             color = stratum)) +
    
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    
    geom_errorbar(width = 0.2, linewidth = 0.6) +
    geom_point(size = 2) +
    
    facet_wrap(~ term_label, scales = "free_y", ncol = 2) +
    
    scale_color_brewer(
      palette = "Dark2",
      name = strat_title
    ) +
    
    labs(
      y = "HR (95% CI)",
      x = NULL
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey90", color = NA),
      legend.position = "bottom",
      
      ## frame each panel (as in Env_plot)
      panel.border = element_rect(
        color = "grey60",
        fill = NA,
        linewidth = 0.5
      ),
      
      # remove x-axis text & ticks
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
}

env_col <- c("IMD_INCOME_neg","roadlength_10",
             "MEAN_Links_R400c_10",
             "NO2_2010_neg",
             "PM25_2010_neg",
             "NDVI_500m_mean_2_0.1"
)

env_labels3 <- c(
  # Natural environment
  "NDVI_500m_mean_2_0.1" = "NDVI",
  "PM25_2010_neg" = "Negative PM2.5",
  "NO2_2010_neg" = "Negative NO2",
  
  # Built environment
  "roadlength_10" = "Road length",
  # "MEAN_Length_R400c_1000" = "Street length",
  "MEAN_Links_R400c_10" = "Street links",
  
  # Economic context
  "IMD_INCOME_neg" = "Local income"
)

cov <- c("Age.0.0", "Sex.0.0", 
         "Income.0.0", "edu.0.0_high", "Smoking", "Alcohol", 
         "urban_2g", "IMD_INCOME_SCORE_060708", 
         "NO2_2010", "PM25_2010", "Assesscenter.0.0")

# stratify by income  ----
strat_incomedep <- run_stratified_env(
  data      = dt1_0,
  strat_var = "INCOME_Q",
  env_vars  = env_col,
  covars    = cov
)
strat_incomedep <- strat_incomedep %>%  
  mutate(
    term_label = recode(variable, !!!env_labels3),
    term_label = factor(term_label, levels = rev(unname(env_labels3)))
  )
plot_strat_incomedep <- plot_stratified_forest(strat_incomedep, "Income Deprivation Quartile")
plot_strat_incomedep
ggsave("figure/stra_forest_incomedep.png",
       plot_strat_incomedep,
       height = 6, width = 6, dpi = 300)
# stratify by population characteristics 
strat_age <- run_stratified_env(
  data      = dt1_0,
  strat_var = "Age_group",
  env_vars  = env_col,
  covars    = cov
)
strat_age <- strat_age %>%  
  mutate(
    term_label = recode(variable, !!!env_labels3),
    term_label = factor(term_label, levels = rev(unname(env_labels3)))
  )

plot_strat_age <- plot_stratified_forest(strat_age, "Age group")
getwd()
ggsave("figure/plot_strat_age.png",
       plot_strat_age,
       height = 6, width = 6, dpi = 300)

# by sex
strat_sex <- run_stratified_env(
  data      = dt1_0,
  strat_var = "Sex.0.0",
  env_vars  = env_col,
  covars    = cov
)
strat_sex <- strat_sex %>%  
  mutate(
    term_label = recode(variable, !!!env_labels3),
    term_label = factor(term_label, levels = rev(unname(env_labels3)))
  )

plot_strat_sex <- plot_stratified_forest(strat_sex, "Sex")
plot_strat_sex
ggsave("figure/plot_strat_sex.png",
       plot_strat_sex,
       height = 6, width = 6, dpi = 300)

# by sex
strat_sex <- run_stratified_env(
  data      = dt1_0,
  strat_var = "Sex.0.0",
  env_vars  = env_col,
  covars    = cov
)
strat_sex <- strat_sex %>%  
  mutate(
    term_label = recode(variable, !!!env_labels3),
    term_label = factor(term_label, levels = rev(unname(env_labels3)))
  )

plot_strat_sex <- plot_stratified_forest(strat_sex, "Sex")
getwd()
ggsave("figure/plot_strat_sex.png",
       plot_strat_sex,
       height = 6, width = 6, dpi = 300)


# check 
env_col2
summary(coxph(Surv(dtime,death) ~  Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol + urban_2g + IMD_INCOME_neg+ 
                NO2_2010_neg + PM25_2010_neg + Assesscenter.0.0,
              data=dt1_0))
summary(coxph(Surv(dtime,death) ~ Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol + urban_2g + IMD_INCOME_neg+ 
                NO2_2010 + PM25_2010 + Assesscenter.0.0,
              data=dt1_0[dt1_0$INCOME_Q==2,]))
summary(coxph(Surv(dtime,death) ~  Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol + urban_2g + IMD_INCOME_neg+ 
                NO2_2010 + PM25_2010 + Assesscenter.0.0,
              data=dt1_0[dt1_0$INCOME_Q==3,]))
summary(coxph(Surv(dtime,death) ~  Age.0.0 + Sex.0.0  +
                Income.0.0 + edu.0.0_high  + Smoking + Alcohol + urban_2g + IMD_INCOME_neg+ 
                NO2_2010 + PM25_2010 + Assesscenter.0.0,
              data=dt1_0[dt1_0$INCOME_Q==4,]))


# stratify for road density ----
dt1_0 <- dt1_0 %>%
  mutate(
    ## Air pollution (higher = worse)
    NO2_Q  = ntile(NO2_2010, 4),
    PM25_Q = ntile(PM25_2010, 4),
    
    ## Greenness (higher = greener)
    NDVI_Q = ntile(NDVI_500m_mean_2_0.1, 4),
    
  )

dt1_0 <- dt1_0 %>%
  mutate(
    NO2_Q  = factor(NO2_Q,  labels = paste0("Q", 1:4)),
    PM25_Q = factor(PM25_Q, labels = paste0("Q", 1:4)),
    NDVI_Q = factor(NDVI_Q, labels = paste0("Q", 1:4))
  )
dt1_0$INCOME_Q <- factor(dt1_0$INCOME_Q,
                         levels=c(1,2,3,4),
                         labels = c("Q1","Q2","Q3","Q4"))

run_road_stratified <- function(data, strat_var, strat_label, covars) {
  
  ## remove stratification variable from covariates
  cov_use <- setdiff(covars, strat_var)
  
  cov_string <- paste(paste0("`", cov_use, "`"), collapse = " + ")
  
  data %>%
    filter(!is.na(.data[[strat_var]])) %>%
    group_by(.data[[strat_var]]) %>%
    group_modify(~ {
      
      fit <- coxph(
        as.formula(
          paste0(
            "Surv(dtime, death) ~ roadlength_10 + ",
            cov_string
          )
        ),
        data = .x
      )
      
      broom::tidy(fit) %>%
        filter(term == "roadlength_10") %>%
        mutate(
          HR = exp(estimate),
          CI_lower = exp(estimate - 1.96 * std.error),
          CI_upper = exp(estimate + 1.96 * std.error),
          stratum = as.character(.y[[1]]),
          strat_type = strat_label
        )
    }) %>%
    ungroup() %>%
    select(strat_type, stratum, HR, CI_lower, CI_upper)
}

stra_road_all <- bind_rows(
  run_road_stratified(dt1_0, "Age_group", "Age", cov),
  run_road_stratified(dt1_0, "Sex.0.0", "Sex", cov),
  run_road_stratified(dt1_0, "edu.0.0_high", "Education", cov),
  run_road_stratified(dt1_0, "Income.0.0", "Household income", cov),
  run_road_stratified(dt1_0, "NO2_Q", "NO2 Quartiles", cov),
  run_road_stratified(dt1_0, "PM25_Q", "PM2.5 Quartiles", cov),
  run_road_stratified(dt1_0, "NDVI_Q", "NDVI Quartiles", cov),
  run_road_stratified(dt1_0, "INCOME_Q", "Income Deprivation Quartiles", cov)
)



stra_road_all <- stra_road_all %>%
  mutate(
    stratum = case_when(
      strat_type == "Income Deprivation Quartiles" & stratum == "1" ~ "Q1",
      strat_type == "Income Deprivation Quartiles" & stratum == "2" ~ "Q2",
      strat_type == "Income Deprivation Quartiles" & stratum == "3" ~ "Q3",
      strat_type == "Income Deprivation Quartiles" & stratum == "4" ~ "Q4",
      TRUE ~ stratum
    )
  )
stra_road_all <- stra_road_all %>%
  mutate(
    y_label = paste0(stratum),
    y_label = factor(y_label, levels = rev(unique(y_label)))
  )

stra_road_all <- stra_road_all %>%
  mutate(
    strat_type = factor(
      strat_type,
      levels = c(
        "Age",
        "Sex",
        "Education",
        "Household income",
        "Income Deprivation Quartiles",
        "NDVI Quartiles",
        "NO2 Quartiles",
        "PM2.5 Quartiles"
      )
    )
  )


plot_stra_road <- ggplot(
  stra_road_all,
  aes(
    y = y_label,
    x = HR,
    xmin = CI_lower,
    xmax = CI_upper
  )
) +
  
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  
  geom_errorbar(position = position_dodge(width = 0.5),width=0.2,  aes(color = strat_type)) +
  geom_point(position = position_dodge(width = 0.5),size=1, aes(color = strat_type)) +
  
  facet_grid(
    strat_type ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  labs(
    x = "HR (95% CI) of road length",
    y = "Stratification groups"
  ) +
  
  theme_minimal(base_size = 13) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, face = "bold", size = 10),
    strip.background = element_rect(fill = "lightgray"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text = element_text(size = 9)
  )
plot_stra_road
ggsave("figure/plot_stra_forroad.png",
       plot_stra_road,
       height = 7, width = 6, dpi = 300)



