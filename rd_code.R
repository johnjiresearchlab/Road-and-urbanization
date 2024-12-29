load("~/202201/a_CLHLS/a_datacodeforall/Survey/ori_longit11.rdata")
dt2 <- subset(dt1, age>=65)
# 9765-9679 = 86
dt3 <- subset(dt2,!is.na(endYear_2) & !is.na(survTime_year))
cov_col_2 <- c("id","age", "Age_group_lab", "Gender", "Education", "Marriage", 
               "Regular_exercise_2", "Smoking_2", "Drinking_2", 
               "Residence", "region", "prov_lab","prov",
               "survTime_year","event")
env_col <- c("NDVI_lastyear_gt0_01","Road_lastyear_10k",
             "NO2_lastyear_ugm3_10","PM25_lastyear_10","O3_lastyear_10",
             "Temp_sd_lastyear_1c","Elevation_100",
             "GDP2010_buffer005mean_100","Pop_lastyear_100")
summary(dt3[,c(cov_col_2,env_col)])
dt4 <- dt3[complete.cases(dt3[,c(cov_col_2,env_col)]),]

# table 1 ----
table1(~ survTime_year+factor(event)+age+Gender+Education+Marriage+
         Regular_exercise_2+Smoking_2+Drinking_2+
         adl_2+CognitiveImpairment+
         Residence+region+
         Road_lastyear_10k+Pop_lastyear_100+GDP2010_buffer005mean_100+
         NO2_lastyear_ugm3+PM25_lastyear+CH4_lastyear+Vehicle_total_1k+clean_nonclean_ratio_100+Temp_sd_lastyear_1c+
         NDVI_lastyear_gt0_01+Elevation,dt4)

# Figure 1 ----
# correlation matrix
M = cor(dt4_adl_mmse_ch4_tc[,c("NDVI", "Elevation","Road_density","NO2", "PM2.5",
                               "CH4_2011_mean","Vehicle_total_1k","clean_nonclean_ratio_100",
                               "Population_density","GDP")],use = "pairwise.complete.obs")
svglite(filename = "figure/cor_p5.svg",
        width = 10, height = 9)
corrplot.mixed(M)
dev.off()

# Plot the boxplot
p_rdyear <- ggplot(dt4_rd_melted, aes(x = Variable, y = value, fill = Variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange")) +
  labs(x = "Year", y = "Road density", title = "Boxplot of Road Density by Year")
svglite(filename = "figure/p_rdyear.svg",
        width = 6, height =5)
p_rdyear
dev.off()

# Model ----
library(survival)
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

# saturated model
HR_rd123y_saturate <- bind_rows(bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov=NULL,
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="A"),
                                bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov="strata(prov_lab)",
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="B"),
                                bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov=c("strata(prov_lab)","Residence","Elevation_100"),
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="C"),
                                bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov=c("strata(prov_lab)","Residence","Elevation_100",
                                                       "GDP2010_buffer005mean_100","Pop_lastyear_100"),
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="D"),
                                bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov=c("strata(prov_lab)","Residence","Elevation_100","GDP2010_buffer005mean_100","Pop_lastyear_100",
                                                       "age","Gender","Education","Marriage"),
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="E"),
                                bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov=c("strata(prov_lab)","Residence","Elevation_100","GDP2010_buffer005mean_100","Pop_lastyear_100",
                                                       "age","Gender","Education","Marriage",
                                                       "Regular_exercise_2","Smoking_2","Drinking_2"
                                                 ),
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="F"),
                                bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov=c("strata(prov_lab)","Residence","Elevation_100","GDP2010_buffer005mean_100","Pop_lastyear_100",
                                                       "age","Gender","Education","Marriage",
                                                       "Regular_exercise_2","Smoking_2","Drinking_2",
                                                       "PM25_lastyear_10","NO2_lastyear_ugm3_10"
                                                 ),
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="G"),
                                bind_rows(lapply(c("Road_lastyear_10k", "Road_2year_10k","Road_3year_10k"),
                                                 cox_f,
                                                 x2=NULL,
                                                 cov=c("strata(prov_lab)","Residence","Elevation_100","GDP2010_buffer005mean_100","Pop_lastyear_100",
                                                       "age","Gender","Education","Marriage",
                                                       "Regular_exercise_2","Smoking_2","Drinking_2",
                                                       "PM25_lastyear_10","NO2_lastyear_ugm3_10",
                                                       "CH4_lastyear","Vehicle_total","clean_nonclean_ratio_100"
                                                 ),
                                                 dataset = dt4_adl_mmse_ch4_tc,category = 1)) %>% mutate(Model="H")
)

HR_rd123y_saturate <- HR_rd123y_saturate %>% mutate(Time_window=ifelse(term=="Road_lastyear_10k","One year",
                                                                       ifelse(term=="Road_2year_10k","Two year",
                                                                              ifelse(term=="Road_3year_10k","Three year",NA))))

HR_rd123y_saturate$Time_window <-  factor(HR_rd123y_saturate$Time_window, levels = c("One year", "Two year", "Three year"))                                                                      
HR_rd123y_saturate$index <- c(nrow(HR_rd123y_saturate):1)
HR_rd123y_saturate$index <- factor(HR_rd123y_saturate$index, levels = rev(unique(HR_rd123y_saturate$index)))

p_rd123y_satu_2 <- ggplot(data = HR_rd123y_saturate, aes(x = Model, y = estimate, ymin = conf.low, ymax = conf.high, color = Time_window)) +
  geom_point(position = position_dodge(width = 0.5), size = 1.5) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(
    "One year" = "#FF6666", "Two year" = "#6666FF", "Three year" = "#66FF66"
  )) +
  labs(title = '', y = 'HR (95% CI)', x = 'Model adjustments') +
  theme_minimal() +
  geom_hline(yintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme(
    axis.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, face = "bold", vjust = 2),
    axis.text.y = element_text(size = 15, face = "bold", color = "Maroon"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )
svglite(filename = "figure/p_rd123y_satu_3.svg",
        width = 7, height = 5)
p_rd123y_satu_2
dev.off()

# spline
spldt<-dt4
library(rms)
dd <- datadist(spldt)
options(datadist='dd')
cox_sf_rd <- cph(Surv(survTime_year, event) ~ rcs(Road_lastyear_10k,3)+ 
                   strat(prov_lab) + Residence + Elevation_100+
                   GDP2010_buffer005mean_100+Pop_lastyear_100+
                   age+Gender+Education+Marriage+
                   Regular_exercise_2+Smoking_2+Drinking_2,
                 data=spldt)

# set the reference value
min(spldt$Road_lastyear_10k)
dd$limits["Adjust to","Road_lastyear_10k"] <- 0
cox_sf_rd <- update(cox_sf_rd)
cox_pre_rd <- Predict(cox_sf_rd, 
                      Road_lastyear_10k,
                      ref.zero = TRUE,
                      fun=exp
)

max(spldt$Road_lastyear_10k)
colnames(cox_pre_rd)
s1_rd <- ggplot() +
  geom_ribbon(data=cox_pre_rd,aes(x=Road_lastyear_10k,ymin = lower, ymax = upper), fill = "#ADD8E6") + # Adjust fill color for the ribbon
  geom_line(data=cox_pre_rd,aes(x=Road_lastyear_10k,y = yhat), color = "#4682B4", size = 1) + # Adjust color for yhat line
  labs(x = expression(Road ~ density ~ (10~km/5 ~ km^2)), y = "HR (95%CI)", title = NULL) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_x_continuous(limits = c(0, 65), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15,face="bold")
  )
s1_rd



# table S1 ----
conti_summary <- function(x,y,dt) {
  dt %>% dplyr::group_by(!!rlang::sym(x)) %>% dplyr::summarise(
    n=length(!!rlang::sym(y)),
    mean=mean(!!rlang::sym(y),na.rm = T),
    sd=sd(!!rlang::sym(y),na.rm = T),
    median=median(!!rlang::sym(y),na.rm = T),
    p25=quantile(!!rlang::sym(y),0.25,na.rm = T),
    p75=quantile(!!rlang::sym(y),0.75,na.rm = T)) %>% 
    rename(var=!!rlang::sym(x)) %>% 
    mutate(var=as.factor(var),ylab=y)
}

row_table1 <-  c("Age_group_lab", "Gender","Education","Marriage","Regular_exercise", "Smoking",
                 "Drinking", "BMI_group", "Residence", "region")

bind_rows(lapply(c(row_table1),
                 conti_summary,y="Road_density",dt=dt4)) %>% View()
bind_rows(lapply(c(row_table1),
                 conti_summary,y="Road_lastyear_10k",dt=dt4)) %>% View()
bind_rows(lapply(c(row_table1),
                 conti_summary,y="NDVI",dt=dt4)) %>% View()
bind_rows(lapply(c(row_table1),
                 conti_summary,y="NO2",dt=dt4)) %>% View()
bind_rows(lapply(c(row_table1),
                 conti_summary,y="PM2.5",dt=dt4)) %>% View()
bind_rows(lapply(c(row_table1),
                 conti_summary,y="CH4_2011_mean",dt=dt4_adl_mmse_ch4_tc)) %>% View()

