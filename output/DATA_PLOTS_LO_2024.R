#########################################
### FINAL PLOTS ########################
#########################################
####library
#set proper paths#https://www.accelebrate.com/library/how-to-articles/r-rstudio-library
myPaths <- .libPaths()  #.libPaths(myPaths) 
myPaths <- c(myPaths[2], myPaths[1]) 
.libPaths(myPaths) 
# install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.10.tar.gz", repos = NULL, type="source")
# install.packages("tidyverse")
# packageVersion("rlang")
source("R/functions_cleanoutputs.R")
source("R/FUNCTIONS_MB.R")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(igraph)
library(cowplot)
library(broom)
library(stringr)
library(forcats)
library(egg) #replace plot_grid with ggarrange, as the gap seems to have been made worse in update. 
#source("packages.R")
#Coarse breakdown data for model comparison - not used to calculate breakdown rate 

coarse_bkd <- read.csv("data/final_lmer_model_network/all_si_lb_phys_chem_yr1_yr2_1Feb24.csv") %>%
  mutate(date_dep = as.Date(date_dep),
         month_dep = fct_relevel(as.factor(month(date_dep, label = TRUE)), "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"),
         type = as.factor(rhodo_acer),
         month_date = if_else(month(date_dep) > 7, as.Date(paste0("2020-", month(date_dep),"-01")), as.Date(paste0("2021-", month(date_dep),"-01")))
)
#%>%
  # group_by(rhodo_acer, month) %>%
  # summarize(
  #   coarse_k = mean_k_coarse
  # )


ggplot(coarse_bkd)+
  geom_boxplot(aes(x = as.factor(month_dep), y = mean_k_coarse, fill = type))


####################
frag_model <- c("M", "UCI", "LCI")
scen_temp <- read_rds("data/scenarios_temperature.rds")
## Data Inputs ## 
types <- c("acer_", "rhodo_", "mix_")
###########################
### Read in Acer Runs #####
###########################
df <- lapply(c("acer_", "rhodo_", "mix_"), function(lit_type) {
  lapply(scen_temp, function(ScT){
          if(file.exists(paste0("output_v4/data/network_", lit_type ,ScT,"_ts_day_", frag_model[1], "_bf_nonserialC.RDS"))){
            rbind(read_rds(paste0("output_v4/data/network_", lit_type ,ScT,"_ts_day_", frag_model[1], "_bf_nonserialC.RDS"))%>%
                    mutate(scenario = ScT,
                           frag_mod = frag_model[1],
                           type = lit_type), 
                  try(read_rds(paste0("output_v4/data/network_", lit_type ,ScT,"_ts_day_", frag_model[2], "_bf_nonserialC.RDS"))%>%
                    mutate(scenario = ScT,
                           frag_mod = frag_model[2],
                           type = lit_type)),
                  try(read_rds(paste0("output_v4/data/network_", lit_type ,ScT,"_ts_day_", frag_model[3], "_bf_nonserialC.RDS"))%>%
                    mutate(scenario = ScT,
                           frag_mod = frag_model[3],
                           type = lit_type))
            )
          }else{
                 return(NA)}
      })#end scen_temp
    })#end type
# 
# df_ts_all_BASEMIXED <- read_rds(paste0("output_v2/data/network_ls_", "rhodo_" ,"base","_ts_all_", frag_model[1], "_nonserialC.RDS"))
# 
# # df_day <-  readRDS(paste0("output_v2/data/network_", "mixed_" ,"base","_ts_day_", "UCI", "_nonserialC.RDS"))%>%
# #   mutate(scenario = "Base",
# #          type = "Mixed",
# #          frag_mod = "M",
# #          date_d = date
# #   )
# ###############
# ## Creae df summaruze plots -
# df_all <- lapply(c("acer_", "mixed_", "rhodo_"), function(lit_type) {
#   lapply(scen_temp, function(ScT){
#     if(file.exists(paste0("output_v2/network_", lit_type ,ScT,"_ts_all_", frag_model[1], "_nonserialC.RDS"))){
#       message(ScT)
#         rbind(
#               daily_clean_summarise(read_rds(paste0("output_v2/network_", lit_type ,ScT,"_ts_all_", frag_model[1], "_nonserialC.RDS")))%>%
#               mutate(scenario = ScT,
#                      frag_mod = frag_model[1],
#                      type = lit_type), 
#               daily_clean_summarise(read_rds(paste0("output_v2/network_", lit_type ,ScT,"_ts_all_", frag_model[2], "_nonserialC.RDS")))%>%
#               mutate(scenario = ScT,
#                      frag_mod = frag_model[2],
#                      type = lit_type),
#               daily_clean_summarise(read_rds(paste0("output_v2/network_", lit_type ,ScT,"_ts_all_", frag_model[3], "_nonserialC.RDS")))%>%
#               mutate(scenario = ScT,
#                      frag_mod = frag_model[3],
#                      type = lit_type)
#             )
#       
#     }else{
#       return(NA)}
#   })#end scen_temp
# })#end type


# # Pull out base hydrologic values 
# df_day_hydro <- df_ts_all %>%
#   group_by(date_d)%>%
#   summarise(
#     WS_benthic_area_m2 = mean(Bedarea_m2)
#   )%>%
#   mutate(date_d = as.Date(date_d, format = "%Y-%m-%d"))


#### CLean up df 

Tsc_rename = c(
  "Base" = "base", 
  "ShalGW" = "shalGW",
  "DeepGW" = "deepGW", 
  "NoGW" = "lowGW", 
  "Base_Warm" = "base_2",
  "NoGW_Warm" = "low_GW_2", 
  "DeepGW_Warm" = "deepGW_05", 
  "ShalGW_Warm" = "shalGW_2"
)

list <- unlist(df, recursive = FALSE)
df_day <- do.call("rbind", list)%>%
  na.omit()%>%
  dplyr::filter(!str_detect(type, "Error"))%>% #2024 added "try" to read in so removed "errors" which are the nonUCI reports
  mutate(scenario = as.factor(scenario))%>%
  mutate(date_d = as.Date(date, format = "%Y-%m-%d"),
         frag_mod = as.factor(frag_mod),
         type = as.factor(type))%>%
  mutate_if(is.character, as.numeric)%>%
  mutate(date = as.character(date_d)) #add character date back  in

#recode factors - cant seem to work within pipe(?)
df_day$scenario <- recode_factor(df_day$scenario,  
           base = "Base", 
           base2 = "Base_Warm",
           shalGW = "ShalGW",
           deepGW = "DeepGW", 
           low_GW = "NoGW", 
           low_GW_2 = "NoGW_Warm", 
           deepGW_05 = "DeepGW_Warm", 
           shalGW_2 = "ShalGW_Warm")

df_day$type <- recode_factor(df_day$type,
                             acer_ = "Acer",
                             rhodo_ = "Rhodo",
                             mix_ = "Mixed"
)

#### 
#saveRDS(df_day, "output_v4/data/network_model_output_daily.RDS")

### READ IN TO NOT HAVE TO RERUN
# df_day <- read_rds("results/network_model_output_daily.RDS")
## Calculate Max values to create model fragmentation uncertainity bounds
df_day_bounds <- df_day%>%
  group_by(type, scenario, date_d)%>%
  summarise(
     max_breakdown_daily = max(C_breakdownTQ_gC),
     min_breakdown_daily = min(C_breakdownTQ_gC),
     max_StStock_daily = max(C_StStock_gC_total),
     min_StStock_daily = min(C_StStock_gC_total)
  )

df_day <- left_join(df_day, df_day_bounds)#%>% #have  max and min expected breakdown based on uncertainity with fragmentation values 
  # left_join(., df_day_hydro) #add hydro values (curently just bedarea as all that is needed for plots)


#Compile to one plot
df_day <- df_day%>%
  mutate(Jdate = yday(date_d))%>%
  dplyr::filter(date_d > as.Date("2019-07-31", format = "%Y-%m-%d") & date_d < as.Date("2020-07-31", format = "%Y-%m-%d"))%>%#remove year 1 
  mutate(date_d = date_d + years(1), #add year to align with input data, model dates are attribtrary 
         WS_benthic_area_m2 = WS_bentic_area_m2)#name fix 

df_day <- df_day %>%
  dplyr::filter(frag_mod == "M")#only present as M as the max and min will capture this 

df_day %>%
  dplyr::filter(frag_mod == "M" )%>% #& !str_detect(scenario, "Base")
  dplyr::mutate(C_breakdownTQ_kgC =C_breakdownTQ_gC/1000)%>%
  ggplot()+
  geom_line(aes(x = date_d, y = C_breakdownTQ_kgC, color = scenario, linetype = type))+
  #geom_ribbon(aes(date, ymin = min_breakdown_daily/1000, ymax = max_breakdown_daily/1000, fill = scenario), alpha = 0.5)+
  #geom_line(aes(date, C_StStock_gC_total/1000, color = scenario, linetype = type))+

  #scale_color_manual(values= scen_colors)+
  ggtitle("Temperature Scenarios: POC Breakdown gC")+
  theme_bw()+
  labs(y = "POC Breakdown (kgC)", x = "")+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")
ggsave("output_v4/figures/TemperatureSc_POCBreakdowngC.png")






#############################
## Observed Standing Stock ##
#############################
#INPUTS
cbom_confi_core <- readRDS("output/data/cbom_confi_core.RDS")
cbom_confi_tr <- readRDS("output/data/cbom_confi_tr.RDS")
cbom_pred_core <- readRDS("output/data/cbom_pred_core.RDS")
cbom_pred_tr <- readRDS("output/data/cbom_pred_tr.RDS")

#MERGE
compare_core <- df_day %>%
  mutate(Jdate = yday(date_d)) %>%
  left_join(., as.data.frame(cbom_pred_core), by = "Jdate")%>%
  left_join(., as.data.frame(cbom_confi_core), by = c("Jdate", "cbom_gC_m2"), suffix = c(".p", ".c"),)%>%
  #left_join(., as.data.frame(cbom_df), by = "Jdate")%>%
  mutate(cbom_AFDM_gm2.lwr.c = ifelse(cbom_AFDM_gm2.lwr.c <0, 0 , cbom_AFDM_gm2.lwr.c),
         WS_observed_StStock_gC = cbom_gC_m2 * WS_benthic_area_m2,
         WS_observed_StStock_gC.l95 = cbom_AFDM_gm2.lwr.c *0.484 * WS_benthic_area_m2,
         WS_observed_StStock_gC.u95 = cbom_AFDM_gm2.upr.c *0.484 * WS_benthic_area_m2)

compare_tr <- df_day %>%
  mutate(Jdate = yday(date_d)) %>%
  left_join(., as.data.frame(cbom_pred_tr), by = "Jdate")%>%
  left_join(., as.data.frame(cbom_confi_tr), by = c("Jdate", "cbom_gC_m2"), suffix = c(".p", ".c"),)%>%
  #left_join(., as.data.frame(cbom_df), by = "Jdate")%>%
  mutate(cbom_AFDM_gm2.lwr.c = ifelse(cbom_AFDM_gm2.lwr.c <0, 0 , cbom_AFDM_gm2.lwr.c),
         WS_observed_StStock_gC = cbom_gC_m2 * WS_bentic_area_m2,
         WS_observed_StStock_gC.l95 = cbom_AFDM_gm2.lwr.c *0.484 *WS_bentic_area_m2,
         WS_observed_StStock_gC.u95 = cbom_AFDM_gm2.upr.c *0.484 * WS_bentic_area_m2)

### Look at dominanting controls 
df_turnover <- df_day %>%
  group_by(scenario, type)%>%
  mutate(C_turnover =  C_StStock_gC_total - C_LitterIn_gC,
         C_breakdown_lag = C_breakdownTQ_gC - dplyr::lag(C_breakdownTQ_gC, n = 1),
         C_StStock_lag = C_StStock_gC_total - dplyr::lag(C_StStock_gC_total, n = 1),
         C_turnover_lag = C_turnover - dplyr::lag(C_turnover, n = 1),
         temp_control = if_else(C_StStock_lag < 0 & C_breakdown_lag > 0, 1, if_else(C_StStock_lag > 0 & C_breakdown_lag < 0,1, 0)),
         temp_control_multi = if_else(dplyr::lag(temp_control, n = 1) == 1 & temp_control == 1, 1, 0) #has to be consective days
  )


###########################################
## Figure 2 Evaluate Model performance
###########################################
scen_colors_GW <- c(
  "#fa7e1e", #shalGW
  "#90a6fd",#deepGW
  "#d62976" #noGW
)


custom_color_GW <- scale_color_manual(name = "",#name = "Thermal Regime", 
                                      values = scen_colors_GW,
                                      labels = c('Shallow GW-Fed','Deep GW-Fed', "Air-Coupled")
)


scen_colors_type <- c(

  "#50C878",#acer
  "#097969", #rhodo
  "#E4D00A" #mixed
)


custom_color_type <- scale_color_manual(name = "",#name = "Thermal Regime", 
                                        values = scen_colors_type,
                                        labels = c('Acer', 'Rhododendron',"Mixed")
)


scen_colors_mix <- c(
  "red",
  "black",
  "orange"
)
coeff_t <- 10

p_mix_brD <- df_day %>%
  dplyr::filter(str_detect(scenario, "Base") & !str_detect(scenario, "Warm") )%>%
  dplyr::filter(frag_mod == "M")%>%
  dplyr::filter(type == "Mixed")%>%
  ggplot(.)+
  geom_line(aes(date_d, C_breakdownTQ_gC/1000, linetype = type), color = "black", linewidth = 1)+
  #geom_ribbon(aes(date_d, ymin = min_breakdown_daily/1000, ymax = max_breakdown_daily/1000, fill = scenario, alpha = type))+
  geom_line(aes(date_d, min_breakdown_daily/1000, linetype = type), color = "black", linewidth = 1, alpha = 0.2)+
  geom_line(aes(date_d, max_breakdown_daily/1000, linetype = type), linewidth = 1, alpha = 0.2)+
  geom_line(aes(date_d, tempC*10, linetype = type),  color = "#581845", linewidth = 1, alpha = 0.5)+
  scale_y_continuous(
    # Features of the first axis
    name = expression(atop("POC Breakdown Flux", (kg~C~d^-1))),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./10, name= bquote("Stream Temperature"~degree*C))
  ) +
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values= scen_colors_mix)+
  scale_fill_manual(values= scen_colors_GW)+
  scale_alpha_discrete(range = c(0.1, 0.15))+
  #ggtitle("Warming Scenario")+
  #labs(x = "", y = paste0("Watershed POC ", "\n", "Breakdown (kg C/day)"))+
  theme_bw(base_size = 8)+
  #ylim(0,NA)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

#ggsave("output_v2/figures/TemperatureSc_GW_Breakdown.png", width = 4, height = 3, units = "in" )

p_mix_firstOrder <- df_lscp_j %>%
  dplyr::filter(stream != "TOWR")%>%
  ggplot(.)+
  geom_line(aes(date, BrD_total * 24* 0.484,  color = stream), linewidth = 1)+
  geom_line(data = dplyr::filter(df_lscp_UCI_j, stream != "TOWR"), aes(date, BrD_total * 24 * 0.484,  color = stream), linewidth = 1, alpha = 0.3)+
  geom_line(data = dplyr::filter(df_lscp_LCI_j, stream != "TOWR"), aes(date, BrD_total * 24* 0.484,  color = stream), linewidth = 1, alpha = 0.3)+
  geom_point(data = df_compare, aes(date.x, FTOC_flux_AFDMhr_obs * 24 * 0.484,  color = stream))+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values= scen_colors_mix)+
  #ggtitle("Warming Scenario")+
  labs(x = "", y = paste0("1st Order Stream Reaches", "\\\\n", "POC Breakdown (gC/day)"))+
  theme_bw(base_size = 8)+
  ylim(0,NA)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

#ggsave("output_v2/figures/FirstOrder_Breakdown.png", width = 4, height = 3, units = "in" )


mixed_SS <- df_day%>%
  #dplyr::filter(str_detect(scenario, "NoGW|NoGW2|NoGW4") )%>%
  dplyr::filter(type == "Mixed" & str_detect(scenario, "Base") )%>%
  ggplot(.)+
  geom_line(aes(date_d, C_StStock_gC_total/1000),color = "black", linetype = 1,linewidth = 1)+
  geom_line(aes(date_d, min_StStock_daily/1000),color = "black", linewidth = 1, alpha = 0.2)+
  geom_line(aes(date_d, max_StStock_daily/1000),color = "black", linewidth = 1, alpha = 0.2)+
  #geom_ribbon(aes(date_d, ymin = min_StStock_daily/1000, ymax = max_StStock_daily/1000, fill = scenario), alpha = 0.3)+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values= scen_colors_mix)+
  #scale_color_manual(values=c("black", "yellow", "#FF00FF", "orange", "#2E8BC0", "green", "purple", "red", "blue"))+
  geom_line(data = compare_tr, aes(date_d, WS_observed_StStock_gC/1000), linetype =3, color = "#3a4c5a")+
  #geom_line(data = compare_tr, aes(date_d, WS_observed_StStock_gC.l95), linetype = 2, color = "grey")+
  #geom_line(data = compare_tr, aes(date_d, WS_observed_StStock_gC.u95), linetype = 2, color = "grey")+
  geom_line(data = compare_core, aes(date_d, WS_observed_StStock_gC/1000), linetype = 3, color = "#3a4c5a")+
  #geom_line(data = compare_core, aes(date_d, WS_observed_StStock_gC.l95), linetype = 2, color = "pink")+
  #geom_line(data = compare_core, aes(date_d, WS_observed_StStock_gC.u95), linetype = 2, color = "pink")+
  #geom_ribbon(data = compare_core, aes(x = date_d, ymin=WS_observed_StStock_gC,ymax=compare_tr$WS_observed_StStock_gC), fill="grey", alpha=0.5)+
  #ggtitle("Temperature Scenarios: Total Watershed POC StandingStock gC")+
  scale_y_continuous(expression(atop("Litter Standing Stock", "(kg C)")))+#bquote("Stream Temperature"~degree*C),
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(#legend.position="none",
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

ggsave("output_v4/figures/TemperatureSc_All_POCStandingStock_comparedtoObserved.png")


dodge <- position_dodge(width=0.5)



coarse_bkd_avg <- coarse_bkd %>%
  dplyr::filter(is.finite(mean_k_coarse)) %>%
  group_by(type)%>% # month_dep
  summarise(avg_k = mean(mean_k_coarse, na.rm = TRUE))

p_k_mixed <- df_day %>%
  dplyr::filter(str_detect(scenario, "Base") )%>%
  ggplot(.)+
  geom_line(aes(date_d, kA_avg), color = "black", linewidth = 1, linetype = 1)+
  geom_line(aes(date_d, kR_avg), color = "black", linewidth = 1 , linetype = 2)+
  geom_boxplot(data = coarse_bkd, aes(x = month_date, y = mean_k_coarse, group = interaction(month_date, type), color = type))+
  custom_color_type+
  labs(x = "") +#, y = bquote("Stream Temperature"~degree*C))+
  theme_bw(base_size = 8)+
  labs(x = "")+
  scale_x_date(date_labels="%b",date_breaks  ="1 month",
               limits = as.Date(c('2020-07-14', '2021-08-14')),
               expand = c(0,0))+
  scale_y_continuous(
    # Features of the first axis
    name = expression(atop("Mean Breakdown rate", (d^-1)))#bquote("Stream Temperature"~degree*C),
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~.*coeff, name= paste0("Litter Input" , "\\\\n", "(kgC day-1)"))
  ) +
  #facet_grid(rows = "regime")+
  theme(#legend.position = "none",
        #axis.text.x=element_blank(),
        #legend.position = c(0.5, 0.7),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        #strip.text.y = element_blank(),
        #axis.title.y.left = element_text(colour="blue"),
        # axis.title.y.right = element_text(colour="darkgreen"),
        # axis.text.y.right = element_text(colour="darkgreen"))+
  )+
  guides(color=guide_legend(ncol=1))
#ggsave("output_v2/figures_final/k_timeseries.png", height = 2, width = 3.25, units = "in")
#ggsave("output_v2/figures_final/k_timeseries.svg", height = 2, width = 3.25, units = "in")

fig2_ABC <- plot_grid(mixed_SS + theme(legend.position = "right",
                           axis.text.x = element_blank(),
                     axis.title.x = element_blank()),
          p_k_mixed + theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank()),
          p_mix_brD + theme(#legend.position = "top",
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.background = element_rect(fill = NA, color = "black"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.title.x = element_blank()
            #strip.text.y = element_blank()
          )+
            guides(color=guide_legend(ncol=1),
                   linetype = guide_legend(ncol = 1)),
          #temp_control_plot,  
          labels = c('A', 'B', 'C'), label_size = 12, ncol = 1,
          #rel_heights = c(2,2,3),
          align = "hv", 
          axis = "rl"
)

Compare_Coweeta_Obs <- egg::ggarrange(mixed_SS + theme(legend.position = "right",
                                axis.text.x = element_blank(),
                                axis.title.x = element_blank()),
               p_k_mixed + theme(axis.text.x = element_blank(),
                                 axis.title.x = element_blank()),
               p_mix_brD + theme(#legend.position = "top",
                 legend.position = "bottom",
                 legend.direction = "horizontal",
                 legend.background = element_rect(fill = NA, color = "black"),
                 axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                 axis.title.x = element_blank()
                 #strip.text.y = element_blank()
               ), 
          ncol = 1)

ggsave(Compare_Coweeta_Obs,filename = "output_v4/figures_final/Compare_Coweeta_Obs.svg", width = 4, height = 6, units = "in")
ggsave(Compare_Coweeta_Obs,filename = "output_v4/figures_final/Compare_Coweeta_Obs.png", width = 4, height = 6, units = "in")

###########################################
## Supplemental 4: Breakdown Rate #########
p_k_shred <-ggplot(coarse_bkd)+
  geom_boxplot(mapping = aes(x = month_date, y = mean_k_shred_new, group = interaction(month_date, type), color = type))+
  custom_color_type+
  labs(x = "") +#, y = bquote("Stream Temperature"~degree*C))+
  theme_bw(base_size = 8)+
  labs(x = "")+
  scale_x_date(date_labels="%b",date_breaks  ="1 month",
               limits = as.Date(c('2020-07-14', '2021-08-14')),
               expand = c(0,0))+
  scale_y_continuous(
    # Features of the first axis
    name = expression(atop("Detritivore Breakdown Rate Coefficient", (d^-1)))#bquote("Stream Temperature"~degree*C),
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~.*coeff, name= paste0("Litter Input" , "\\\\n", "(kgC day-1)"))
  ) +
  #facet_grid(rows = "regime")+
  theme(
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  )+
  guides(color=guide_legend(ncol=1))

p_k_micro <-ggplot(coarse_bkd)+
  geom_boxplot(mapping = aes(x = month_date, y = mean_k_fine, group = interaction(month_date, type), color = type))+
  custom_color_type+
  labs(x = "", y = expression(atop("Microbial Breakdown Rate Coefficient", (d^-1)))) +#, y = bquote("Stream Temperature"~degree*C))+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month",
               limits = as.Date(c('2020-07-14', '2021-08-14')),
               expand = c(0,0))+
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  )+
  guides(color=guide_legend(ncol=1))+
  ylim(NA, 0.05)

supp4_fig <- egg::ggarrange(p_k_micro, p_k_shred, ncol = 2 )

ggsave(supp4_fig,filename = "output_v4/figures_final/Supplemental_4.svg", width = 7, height = 4, units = "in")
ggsave(supp4_fig, filename ="output_v4/figures_final/Supplemental_4.png", width = 7, height = 4, units = "in")


###########################################
## Figure 3 Compare GW Thermal Regimes
###########################################

scen_colors_GW <- c(
  "#fa7e1e", #shalGW
  "#90a6fd",#deepGW
  "#d62976" #noGW
)


p1 <- df_day %>%
  dplyr::filter(scenario == "NoGW" | scenario == "DeepGW" | scenario == "ShalGW")%>%
  dplyr::filter(frag_mod == "M")%>%
  ggplot(.)+
  geom_line(aes(date_d, C_breakdownTQ_gC/1000,  color = scenario, linetype = type), linewidth = 1)+
  #geom_ribbon(aes(date_d, ymin = min_breakdown_daily/1000, ymax = max_breakdown_daily/1000, fill = scenario, alpha = type))+
  #geom_line(aes(date_d, min_breakdown_daily/1000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  #geom_line(aes(date_d, max_breakdown_daily/1000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  custom_color_GW+
  scale_fill_manual(values= scen_colors_GW)+
  scale_alpha_discrete(range = c(0.1, 0.15))+
  #ggtitle("Warming Scenario")+
  labs(x = "", y = expression("POC Breakdown Flux ", (kg~C~d^-1)))+
  theme_bw(base_size = 8)+
  ylim(0,NA)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(#legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))
ggsave("output_v4/figures/TemperatureSc_GW_Breakdown.png", width = 4, height = 3, units = "in" )

p2 <- df_day %>%
  dplyr::filter(scenario == "NoGW" | scenario == "DeepGW" | scenario == "ShalGW")%>%
  dplyr::filter(frag_mod == "M")%>%
  ggplot(.)+
  #geom_ribbon(aes(date_d, ymin = min_StStock_daily/1000, ymax = max_StStock_daily/1000, fill = scenario, alpha = type))+
  geom_line(aes(date_d, C_StStock_gC_total/1000000,  color = scenario, linetype = type), linewidth = 1)+
  #geom_line(aes(date_d, min_StStock_daily/1000000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  #geom_line(aes(date_d, max_StStock_daily/1000000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  custom_color_GW+
  scale_fill_manual(values= scen_colors_GW)+
  scale_alpha_discrete(range = c(0.1, 0.15))+
  #ggtitle("Warming Scenario")+
  labs(x = "", y = paste0("Watershed", "\n", "POC Standing Stock", "\n", "(Mg C)"))+
  ylim(0,NA)+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

ggsave("output_v4/figures/TemperatureSc_GW_StandingStock.png", width = 4, height = 3, units = "in" )

p <- plot_grid(p2, p1,  labels = c('A', 'B'), label_size = 12, ncol = 1)
ggsave("output_v4/figures_final/Compare_GW_noribbon.svg", width = 4, height = 6, units = "in")
ggsave("output_v4/figures_final/Compare_GW_noribbon.png", width = 4, height = 6, units = "in")

dodge <- position_dodge(width=0.5)

p_k <- df_day %>%
  dplyr::filter(!str_detect(scenario, "Warm"))%>%
  dplyr::filter(!str_detect(scenario, "Base"))%>%
  ggplot(.)+
  geom_line(aes(date_d, kA_avg, color = scenario), linewidth = 1, linetype = 1)+
  geom_line(aes(date_d, kR_avg, color = scenario), linewidth = 1 , linetype = 2)+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  custom_color_GW+
  theme_bw(base_size = 8)+
  labs(x = "")+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  scale_y_continuous(
    # Features of the first axis
    name = expression(atop("Average Breakdown Rate Coefficient"), d^-1) #paste0("Average Breakdown", "\n", "Rate Coefficient (day-1)")#bquote("Stream Temperature"~degree*C),
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~.*coeff, name= paste0("Litter Input" , "\\\\n", "(kgC day-1)"))
  ) +
  #facet_grid(rows = "regime")+
  theme(legend.position = "none",
        #axis.text.x=element_blank(),
        #legend.position = c(0.5, 0.7),
        #legend.direction = "horizontal",
        #legend.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        #strip.text.y = element_blank(),
        #axis.title.y.left = element_text(colour="blue"),
        # axis.title.y.right = element_text(colour="darkgreen"),
        # axis.text.y.right = element_text(colour="darkgreen"))+
  )+
  guides(color=guide_legend(ncol=1))



ggsave("output_v4/figures_final/k_timeseries.png", height = 2, width = 3.25, units = "in")
ggsave("output_v4/figures_final/k_timeseries.svg", height = 2, width = 3.25, units = "in")



egg::ggarrange(p2 + theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank()),
          p_k + theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank()),
          p1 +   theme(#legend.position = "top",
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.background = element_rect(fill = NA, color = "black"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
            #strip.text.y = element_blank()
          ),
            # guides(color=guide_legend(ncol=1),
            #        linetype = guide_legend(ncol = 1)),
          #temp_control_plot,  
          #labels = c('A', 'B', 'C'), label_size = 12, 
          ncol = 1
          )

ggsave("output_v4/figures_final/Compare_GW_Scenarios.svg", width = 4, height = 7, units = "in")
ggsave("output_v4/figures_final/Compare_GW_Scenarios.png", width = 4, height = 7, units = "in")


#########################################################################################################
## Comparing GW figures
df_day <- dplyr::filter(df_day, type != "Mixed") #remove mixed categores for rest of figures

df_day_base <- dplyr::filter(df_day, type == "Mixed" & scenario == "Base") 

df_day <- rbind(df_day, df_day_base)
### DATA Results
###Cumlative 
GW_cuml <- df_day%>%
  group_by(type, scenario, frag_mod)%>%
  mutate(across(c("C_LitterIn_gC", "C_breakdownTQ_gC"), #, "C_breakdownTQ_gC_frag", "C_breakdownTQ_gC_micrb","max_breakdown_daily", "min_breakdown_daily", "C_gw_gC") 
                  cumsum))

write.csv(GW_cuml, "output/df_cuml.csv")

GW_totals <- df_day%>%
  group_by(type, scenario, frag_mod)%>%
  mutate(across(c("C_LitterIn_gC", "C_breakdownTQ_gC", "C_breakdownTQ_gC_frag", "C_breakdownTQ_gC_micrb",
                  "max_breakdown_daily", "min_breakdown_daily", "C_gw_gC") , cumsum))%>%
 summarise(across(c("C_LitterIn_gC", "C_breakdownTQ_gC", "C_breakdownTQ_gC_frag", "C_breakdownTQ_gC_micrb",
                  "max_breakdown_daily", "min_breakdown_daily", "C_gw_gC") , max))


write.csv(GW_totals, "output/df_totals.csv")

GW_mean <- df_day %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

write.csv(GW_mean, "output/df_day_avg.csv")

GW_sd<- df_day %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
write.csv(GW_sd, "output/df_day_sd.csv")

#WHICH DAY HAS THE Lowest VALUE
GW_min<- df_day %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ min(.x, na.rm = TRUE)))

write.csv(GW_min, "output/df_day_min.csv")

GW_min_date<- df_day %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ df_day$Jdate[which.min(.x)]))

GW_min_BenthicSize<- df_day %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ df_day$WS_bentic_area_m2[which.min(.x)]))

#WHICH DAY HAS THE HIGHEST VALUE
GW_max<- df_day %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE)))%>%
  mutate(det_max_range = max_breakdown_daily - min_breakdown_daily,
         )

write.csv(GW_max, "output/df_day_max.csv")

GW_max_date<- df_day %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ df_day$Jdate[which.max(.x)]))

write.csv(GW_max_date, "output/df_day_max_Jdate.csv")

GW_max_seasonal<- df_day %>%
  mutate(season = quarter(date, fiscal_start = 12))%>%
  group_by(type, scenario, season)%>%
  summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE)))%>%
  mutate(det_max_range = max_breakdown_daily - min_breakdown_daily,
  )%>%
  dplyr::select(1:3, C_breakdownTQ_gC, C_StStock_gC_total)
write.csv(GW_max_seasonal, "output/df_day_max_seasonal_value.csv")

GW_max_seasonal_date<- df_day %>%
  #mutate(season = quarter(date, fiscal_start = 12))%>%
  group_by(type, scenario)%>%
  summarise(across(where(is.numeric), ~ df_day$Jdate[which.max(.x)]))%>%
  mutate(det_max_range = max_breakdown_daily - min_breakdown_daily,
  )%>%
  dplyr::select(1:3, C_breakdownTQ_gC, C_StStock_gC_total, tempC)%>%
  mutate(Max_date_TbrK = C_breakdownTQ_gC - tempC) #get date difference between maximums

write.csv(GW_max_seasonal_date, "output/df_day_max_seasonal_Jdate.csv")



total_breakdown <- GW_cuml %>%
  group_by(scenario, type, frag_mod) %>%
  summarise(tot_gC = max(C_breakdownTQ_gC))

total_breakdown_perday = max(total_breakdown$tot_gC)/365 #with no dependancy just same breakdown everyday

df_day$constant_gC <- total_breakdown_perday
df_day$C_breakdownTQ_gC_frag_min <- df_day$min_breakdown_daily - df_day$C_breakdownTQ_gC_micrb
df_day$C_breakdownTQ_gC_frag_max <- df_day$max_breakdown_daily - df_day$C_breakdownTQ_gC_micrb

GW_cuml <- df_day%>%
  group_by(type, scenario, frag_mod)%>%
  mutate(across(c("C_LitterIn_gC","C_breakdownTQ_gC", "C_breakdownTQ_gC_frag", "C_breakdownTQ_gC_micrb", 
                  "C_breakdownTQ_gC_frag_min",
                  "C_breakdownTQ_gC_frag_max", "constant_gC", "C_gw_gC"), cumsum)) %>%
  dplyr::filter(date_d <= as.Date("2021-07-31")) #remove aug 2021 from ggplot so figures line up. 


GW_max_cuml<- GW_cuml %>%
  group_by(type, scenario, frag_mod)%>%
  summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE)))%>%
  mutate(det_max_range = max_breakdown_daily - min_breakdown_daily,
  )


#dplyr::select(type, scenario, frag_mod, date_d, C_gw_gC, C_breakdownTQ_gC, constant_gC)


####################################
## Figure 5 Compare to GW ##########
####################################

## Make DOC supp;y and Constant a "scenario" for ease of plotting and legend making 
GW_CSupply <- GW_cuml %>%
  ungroup()%>%
  dplyr::select(frag_mod, date_d, C_gw_gC)%>%
  distinct()%>%
  mutate(scenario = "GW DOC Supply",
         cuml_wsC = C_gw_gC,
         type = "All")%>%
  dplyr::select(-C_gw_gC)

GW_Constant <- GW_cuml %>%
  ungroup()%>%
  dplyr::select(frag_mod, date_d, constant_gC)%>%
  distinct()%>%
  mutate(scenario = "Constant Breakdown Flux",
         cuml_wsC = constant_gC,
         type = "All")%>%
  dplyr::select(-constant_gC)

Input_Cuml <- GW_cuml %>%
  ungroup()%>%
  dplyr::select(frag_mod, date_d, C_LitterIn_gC)%>%
  distinct()%>%
  mutate(scenario = "Litter Input",
         cuml_wsC = C_LitterIn_gC,
         type = "All")%>%
  dplyr::select(-C_LitterIn_gC)

GW_cuml_v2 <-  GW_cuml %>%
  ungroup()%>%
  mutate(cuml_wsC = C_breakdownTQ_gC)%>%
  dplyr::select(type, scenario, frag_mod, date_d, cuml_wsC)%>%
  rbind(GW_Constant)%>%
  rbind(Input_Cuml)
  #rbind(GW_CSupply)

scen_colors_compare<- c(
  "#fa7e1e", #shalGW
  "#90a6fd",#deepGW
  "#d62976", #noGW
  "grey",
  "#29AB87"

)

custom_color_compare <- scale_color_manual(name = "",#"Scenario", 
                                     values = scen_colors_compare,
                                     labels = c('Shallow GW-fed','Deep GW-fed', "Air-Coupled", "Constant Breakdown Flux", "Litter Input")#, "DOC GW Supply")
)

p_GW_cuml_GW <- GW_cuml_v2 %>%
  dplyr::filter(!str_detect(scenario, "Warm"),!str_detect(scenario, "Base"), scenario != "GW DOC Supply")%>%
  ggplot(.)+
  geom_line(aes(date_d, cuml_wsC/1000000,  color = scenario, linetype = type), linewidth = 1)+
  custom_color_compare  + 
  scale_linetype_manual(values = c(1,3, 6), labels=c('Acer','Rhododendron', "All"), name = "")+ #Breakdown Rate")+
  #ggtitle("Total Watershed POC Breakdown")+
  labs(x = "", y = expression("Cumulative Watershed ", "\n", "Totals (Mg C)"))+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal")
  #scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  #facet_grid(rows = "regime")+


ggsave("output_v4/figures/TemperatureSc_In_StandingStock_Cuml.png", width = 4, height = 3.3, units = "in" )
ggsave("output_v4/figures/TemperatureSc_In_StandingStock_Cuml.svg", width = 4, height = 3.3, units = "in" )


fig3_ABCD <- egg::ggarrange(p2 + theme(axis.text.x = element_blank(),
                                  axis.title.x = element_blank()),
                                  #plot.margin = unit(c(0, 0, 0, 0), "cm")),
                       p_k + theme(axis.text.x = element_blank(),
                                   axis.title.x = element_blank()),
                                   #plot.margin = unit(c(0, 0, 0, 0), "cm")),
                       p1 +   theme(#legend.position = "top",
                         legend.position = "none",
                         axis.text.x = element_blank(),
                         axis.title.x = element_blank()
                         #plot.margin = unit(c(0, 0, 0, 0), "cm")
                         #strip.text.y = element_blank()
                       ),
                       
                       p_GW_cuml_GW + theme(
                         
                         legend.position = "bottom",
                         legend.direction = "horizontal",
                         legend.background = element_rect(fill = NA, color = "black"),
                         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
                       )+
                         guides(color=guide_legend(ncol=2),
                                linetype = guide_legend(ncol = 1)),
                       ncol= 1
                       #temp_control_plot,  
                       # labels = c('A', 'B', 'C', "D"), label_size = 12, ncol = 1,
                       # align = "hv"#,#, 
                       #rel_heights = c(2, 1.5, 2, 3.3)
)

ggsave(fig3_ABCD, filename =  "output_v4/figures_final/Fig3_ABCD.svg", width = 4, height = 10, units = "in")
ggsave(fig3_ABCD, filename = "output_v4/figures_final/Fig3_ABCD.png", width = 4, height = 10, units = "in")


#########
## DF to make monthly avg and compare temperature effect to DOC contribution 
compare_months <- df_day %>%#network_ts_day_df %>% #do.call(rbind,  network_ts_day_ss_DEEP50) %>%
  mutate(month_d = fct_relevel(as.factor(month(date_d, label = TRUE)), "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")) %>%
  dplyr::group_by(scenario, type, month_d) %>%
  dplyr::summarise(
    POCbreakdown_frag = mean(C_breakdownTQ_gC_frag),
    POCbreakdown_micrb = mean(C_breakdownTQ_gC_micrb),
    POCbreakdown = mean(C_breakdownTQ_gC),
    POCbreakdown_low = mean(min_breakdown_daily),
    POCbreakdown_high = mean(max_breakdown_daily),
    DOCseep = mean(C_gw_gC),
    POCin = mean(C_LitterIn_gC),
    Temp_avg = mean(tempC),
    BQ_avg_outlet = mean(Q_outlet)) %>%
  dplyr::filter(str_detect(scenario, "NoGW|ShalGW|DeepGW") )

#make a dataframe with just Air-Coupled, as that is what we are comparing againist
noGW_months <- compare_months %>%
  dplyr::filter(scenario == "NoGW")

GW_CSupply <- compare_months %>%
  ungroup()%>%
  dplyr::select(month_d, DOCseep)%>%
  distinct()%>%
  mutate(scenario = "Avg GW DOC Supply",
         cuml_wsC = DOCseep,
         type = "All")%>%
  dplyr::select(-DOCseep)

diff_noGW_months <- compare_months %>%
  left_join(noGW_months, by = c("month_d", "type"))%>%
  mutate(POCbreakdown_diff = POCbreakdown.x - POCbreakdown.y,
         POCbreakdown_diff_low = POCbreakdown_low.x - POCbreakdown_low.y,
         POCbreakdown_diff_high = POCbreakdown_high.x - POCbreakdown_high.y,
         POCbreakdown_diff_pro = (POCbreakdown.x - POCbreakdown.y) /POCbreakdown.y )

scen_colors_GW <- c(
  "#fa7e1e", #shalGW
  "#90a6fd",#deepGW
  "#d62976" #noGW
)

scen_colors_black <- c(
  "black", #shalGW
  "black",#deepGW
  "black" #noGW
)

custom_fill_GW <- scale_fill_manual(name = "",#name = "Thermal Regime", 
                                 values = scen_colors_GW,
                                 labels = c('Shallow GW-fed','Deep GW-fed', "Air-Coupled")
)

custom_color_GW <- scale_color_manual(name = "", # "Thermal Regime", 
                                    values = scen_colors_black,
                                    labels = c('Shallow GW-Fed','Deep GW-Fed', "Air-Coupled")
)

p_tempEffect_month <- diff_noGW_months %>%
  dplyr::filter(scenario.x != "NoGW")%>%
  mutate(GW_C = "DOC GW Supply")%>%
  #dplyr::filter(site_loc == "North Carolina" & !str_detect(scenario.x , "k"), & !str_detect(scenario.x , "k"))%>%
  dplyr::filter(str_detect(scenario.x , "GW") & !str_detect(scenario.x , "Warm") & !str_detect(scenario.x , "Base"))%>%
  na.omit() %>%
  ggplot(.) +
  geom_col(aes(month_d, DOCseep.x/1000),  fill = "#29AB87", color = "black", width=.5, position = "dodge") +
  geom_col(aes(month_d,  POCbreakdown_diff/1000, fill = scenario.x, alpha = type), width=.5, position = "dodge")+
  geom_errorbar(aes(x=month_d, ymin = POCbreakdown_diff_low/1000,
                                ymax = POCbreakdown_diff_high/1000, color = scenario.x, alpha = type), width=.5, position="dodge")+
  custom_fill_GW+
  custom_color_GW+
  xlab("")+
  scale_alpha_discrete(range = c(0.55, 0.95), name = "")+
  scale_y_continuous(
    # Features of the first axis
    name = str_wrap("Average Leaf Breakdown Compared to Air-Coupled Scenario (kgC)", width = 25),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name= str_wrap("Average Monthly GW-Derived DOC (kgC)", width = 25)),
    limits = c(-100, 55)

  ) +
  theme_bw(base_size = 8)+
  #scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  #facet_grid(rows = "regime")+
  theme(#legend.position = "top",
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.text.y = element_blank()
  )+
  guides(color=guide_legend(ncol=1))



ggsave("output_v4/figures_final/GWTemp_Supply_Compare.png", width = 4, height = 3, units = "in" )
ggsave("output_v4/figures_final/GWTemp_Supply_Compare.svg", width = 4, height = 3, units = "in" )


####FIGURE 4
plot_grid(p_GW_cuml_GW+
            theme(#legend.position = "top",
                  legend.position = "right",
                  legend.direction = "horizontal",
                  legend.background = element_rect(fill = NA, color = "black"),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                  strip.text.y = element_blank())+
                  guides(color=guide_legend(ncol=1),
                         linetype=guide_legend(ncol=1)),
           p_tempEffect_month +   
            theme(#legend.position = "top",
                  legend.position = "right",
                  legend.direction = "horizontal",
                  legend.background = element_rect(fill = NA, color = "black"),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
                  #strip.text.y = element_blank()
                )+
                guides(fill=guide_legend(ncol=1),
                   alpha = guide_legend(ncol = 1)),
          #temp_control_plot,  
          labels = c('A', 'B'), label_size = 12, ncol = 1,
          #align = "hv",
          rel_widths = c(1.4, 1)
          #rel_heights = c(2, 2, 4)
)

ggsave("output_v4/figures/Compare_GW_Scenarios.svg", width = 6, height = 6, units = "in")
ggsave("output_v4/figures/Compare_GW_Scenarios.png", width = 6, height = 6, units = "in")




noGW_daily <- df_day %>%
  dplyr::filter(scenario == "NoGW")

diff_noGW_daily <- df_day%>%
  dplyr::filter(str_detect(scenario, "GW") & !str_detect(scenario , "Warm") & !str_detect(scenario, "2") | str_detect(scenario, "Base"))%>%
  left_join(noGW_daily, by = c("date_d", "type"))%>%
  mutate(POCbreakdown_diff = C_breakdownTQ_gC.x -  C_breakdownTQ_gC.y,
         POCbreakdown_diff_pro = ( C_breakdownTQ_gC.x -  C_breakdownTQ_gC.y) / C_breakdownTQ_gC.y ) %>%
  left_join(., GW_cuml, by = c("type", "date_d", "scenario.x" = "scenario")) %>%
  group_by(type, scenario.x)%>%
  mutate(across(c( "POCbreakdown_diff", "POCbreakdown_diff_pro"), cumsum))



#do cumsum again to compare to temperature for GW
p_GW_tempVsupply <- diff_noGW_daily  %>%
  dplyr::filter(scenario.x == "DeepGW" | scenario.x == "ShalGW")%>%
  #dplyr::filter(type == "Acer")%>%
  ggplot(.)+
  geom_line(aes(date_d, C_gw_gC/1000), color = "black", linetype = 5, size = 1)+
  geom_line(aes(date_d, POCbreakdown_diff/1000,  color = scenario.x, linetype = type), size = 0.5)+
  scale_color_manual(values= scen_colors_GW,  labels=c('Shallow GW-fed','Deep GW-fed', 'Air-Coupled'), name = "")+ #"Regime")+
  scale_linetype_manual(values = c(1,3), labels=c('Fast','Slow'), name = "Breakdown Rate")+
  #ggtitle("Total Watershed POC Breakdown")+
  labs(x = "", y = paste0("Cumulative Watershed", "\\\\n", " Totals (kg C)"))+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(#legend.position = "top",
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.text.y = element_blank()
  )+
  guides(color=guide_legend(ncol=1))





litter_name <- c(
  "Acer" = "Fast",
  "Rhodo" = "Slow"
)

scen_colors_GWb<- c(
  "purple" ,#Base
  "#fa7e1e", #shalGW
  "#90a6fd"#deepGW

)

#therm_effect <- 
diff_noGW_daily %>%
  dplyr::filter(!str_detect(scenario.x , "NoGW"))%>%
  na.omit() %>%
  ggplot(.) +
  geom_line(aes(date_d, C_gw_gC/1000), color = "black", size=0.5)+
  geom_line(aes(date_d,  POCbreakdown_diff/1000, color = scenario.x, linetype = type), size=0.5)+
  scale_linetype_manual(values = c(1,3), labels=c('Fast','Slow'), name = "Breakdown Rate")+
  geom_hline(yintercept = 0, color = "grey", size = 1)+
  scale_color_manual(values= scen_colors_GWb)+
  ylab(str_wrap("Daily Breakdown Differnece from Air-Coupled Scenario (kg C)", width = 25))+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  xlab("")+
  # facet_grid(rows = "type",labeller = labeller(
  #   type = litter_name
  # ))+
  #facet_grid(rows = "regime")+
  theme(#legend.position = "top",
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
        #strip.text.y = element_blank()
        )+
  guides(color=guide_legend(ncol=1))

# plot_grid(p_GW_cuml_GW,
#           therm_effect,  
#           labels = c('A', 'B'), label_size = 12, ncol = 1,
#           align = "hv", 
#           rel_heights = c(2.5, 2)
# )

ggsave("output_v4/figures/GW_ThermalEffect_cuml.png", width = 4, height = 2, units = "in" )
ggsave("output_v4/figures/GW_ThermalEffect_cuml.svg", width = 4, height = 2, units = "in" )

#########################################################
### Redo Figure 3 with Cumluative

plot_grid(p2,p1, p_GW_cuml_GW + theme(legend.position = "none"),temp_control_plot,  
          labels = c('A', 'B', 'C', 'D'), label_size = 12, ncol = 1,
          align = "hv", 
          rel_heights = c(3, 3, 3, 2)
)

ggsave("output_v3/figures_final/Compare_GW.svg", width = 4, height = 8, units = "in")



###########################################
## Figure 2 Compare Base Qualites
###########################################


plot_grid(p2 + theme(axis.text.x = element_blank(),
                                             axis.title.x = element_blank()), 
          p_k + theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank()),
          p1 + theme(
                      #axis.text.x = element_blank(),
                     #axis.title.x = element_blank(),
                    legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.background = element_rect(fill = NA, color = "black"))+
            guides(color=guide_legend(nrow=3),
                   linetype = guide_legend(nrow=2)),
          #temp_control_plot,
          labels = c('A', 'B'), label_size = 12, ncol = 1,
          align = "hv", 
          rel_heights = c(2, 2, 3, 1)
)
ggsave("output_v3/figures_final/TemperatureSc_All_POCStandingStock_comparedtoObserved_nR.svg", height = 9, width = 4, units = "in")
ggsave("output_v3/figures_final/TemperatureSc_All_POCStandingStock_comparedtoObserved_nR.png", height = 9, width = 4, units = "in")


## Use plot from Compare Seston Fragmentation  -> p_seston_compare


plot_grid(mixed + theme(legend.position="bottom", legend.box = "horizontal"), 
          p_seston_compare + theme(legend.position="bottom", legend.box = "horizontal"),
          labels = c('A', 'B'), label_size = 12, ncol = 2,
          align = "hv", 
          rel_heights = c(3, 3)
)
ggsave("output_v2/figures_final/Fig_compareSSandSeston.svg", height = 4, width = 7, units = "in")
ggsave("output_v2/figures_final/Fig_compareSSandSeston.png", height = 4, width = 7, units = "in")



###Cumlative 
mixed_cuml <- df_day%>%
  group_by(type, scenario)%>%
    mutate(across(c(5:10) , cumsum))

mixed_cuml_total <- mixed_cuml %>%
  group_by(type, scenario)%>%
  summarize(across(c(5:10), max))

mixed_mean <- df_day%>%
  group_by(type, scenario)%>%
  summarize(across(c(2:10) , mean))

mixed_cuml_maxi <- df_day %>%
  group_by(type, scenario)%>%
  summarise(across(c(2:10), which.max))

mixed_cuml_max <- df_day %>%
  group_by(type, scenario)%>%
  summarize(across(c(5:10), max))

df_day$Jdate <- yday(df_day$date_d)
mixed_cuml_maxi$breakdown_maxdate<- df_day$Jdate[mixed_cuml_maxi$C_breakdownTQ_gC]
mixed_cuml_maxi$breakdown_maxValue<- df_day$C_breakdownTQ_gC[mixed_cuml_maxi$C_breakdownTQ_gC]


# qual_df$Jdate[120] #max standing stock day for shallow and deep acer
# qual_df$Jdate[148] #max standing stock day for shallow and deep rhodo
# qual_df$Jdate[154] #max standing stock day for air-coupled rhodo
# 
# 
# qual_df$Jdate[118] #max breakdown day for shallow and deep acer
# qual_df$Jdate[119] #max breakdown day for shallow and deep rhodo
# qual_df$Jdate[118] #max rekdoenwb day for air-coupled rhodo

mixed_cuml_mini <- qual_df %>%
  group_by(type, scenario)%>%
  summarise(across(c(5:10), which.min))

mixed_cuml_min <- qual_df %>%
  group_by(type, scenario)%>%
  summarize(across(c(5:10), min))

# qual_df$Jdate[24] #min standing stock day for shallow  acer
# qual_df$Jdate[18] #min standing stock day for deep acer
# qual_df$Jdate[52] #min standing stock day for shallow  rho
# qual_df$Jdate[54] #min standing stock day for deep rho

mixed_cuml_avg <- qual_df %>%
  group_by(type, scenario)%>%
  summarize(across(c(5:10), mean))

max_mat <- as.matrix(mixed_cuml_max[2:8]) #,3:11])

max_Jdate <-matrix(mixed_cuml$Jdate[max_mat]) %>%
  cbind(., mixed_cuml_max)

mix_k_cuml <- mixed_cuml%>%
  #dplyr::filter(str_detect(scenario, "NoGW|NoGW2|NoGW4") )%>%
  ggplot(.)+
  geom_line(aes(date_d, C_breakdownTQ_gC/1000, color = type), size = 0.5)+ #, linetype = type
  # scale_linetype_manual(name = "",
  #                       values = c("R" = 2, "M" = 6, "A" = 1), guide = "none")+
  # #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(name = "", values= scen_colors_mix,
                     labels=c('Fast','Observed Mixed', 'Slow'))+
  #ggtitle("Total Watershed POC Breakdown")+
  labs(x = "", y = paste0("Cumulative Watershed", "\n", "POC Breakdown (kg C)"), subtitle = "C")+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1), strip.text.y = element_blank())
ggsave("output_v3/figures/SpeciesSc_Breakdown.png", height = 4, width = 3.25, units = "in")



plot_grid(mixed, #+ theme(axis.text.x = element_blank(),
                 #       axis.title.x = element_blank()),
          mix_k, 
          mix_k_cuml,
          # labels = c('A', 'B', 'C'), 
          # label_size = 12, 
          ncol = 1,
          rel_heights = c(1,1,1.25),
          align = "hv")

ggsave("output_v4/figures_final/ComparedtoObserved_andbreakdown.svg", height = 5, width = 3.25, units = "in")


###########################################
## Figure 4 % Changes 50-years
###########################################
## Percent Average
compare_warm <- df_day%>%
  mutate(scen_type = str_c(substr(type,1,1), "_", scenario))%>%
  mutate(month = month(date_d, label = TRUE))%>%
  group_by(scen_type, month)%>%
  summarise(StStock_avgmonth = mean(C_StStock_gC_total),
            Breakdown_totmon = sum(C_breakdownTQ_gC))%>%
  na.omit()

compare_warm_annual <- df_day%>%
  mutate(scen_type = str_c(substr(type,1,1), "_", scenario))%>%
  mutate(month = month(date_d, label = TRUE))%>%
  group_by(scen_type)%>%
  summarise(Breakdown_tot = sum(C_breakdownTQ_gC))%>%
  na.omit()%>%
  pivot_wider(names_from = scen_type, values_from = Breakdown_tot)%>%
  mutate(
    A_Deep_50y = (A_DeepGW_Warm - A_DeepGW)/1000,
    R_Deep_50y = (R_DeepGW_Warm - R_DeepGW)/1000,
    A_Shal_50y = (A_ShalGW_Warm - A_ShalGW)/1000,
    R_Shal_50y = (R_ShalGW_Warm - R_ShalGW)/1000,
    A_NoGW_50y = (A_NoGW_Warm - A_NoGW)/1000,
    R_NoGW_50y = (R_NoGW_Warm - R_NoGW)/1000
  )%>%
  pivot_longer(cols = 19:24)%>%
  dplyr::select(name, value) #%>%


compare_warm_st_TS <- df_day%>%
  mutate(scen_type = str_c(substr(type,1,1), "_", scenario))%>%
  dplyr::select(date, scen_type, C_StStock_gC_total)%>%
  pivot_wider(names_from = scen_type, values_from = C_StStock_gC_total)%>%
  mutate(
    A_Deep_50y = (A_DeepGW_Warm - A_DeepGW)/1000,
    R_Deep_50y = (R_DeepGW_Warm - R_DeepGW)/1000,
    A_Shal_50y = (A_ShalGW_Warm - A_ShalGW)/1000,
    R_Shal_50y = (R_ShalGW_Warm - R_ShalGW)/1000,
    A_NoGW_50y = (A_NoGW_Warm - A_NoGW)/1000,
    R_NoGW_50y = (R_NoGW_Warm - R_NoGW)/1000)%>%
  pivot_longer(cols = 19:24)%>% #the calculated values/percents
  dplyr::select(date, name, value) %>%
  mutate(type = if_else(str_starts(name, "R"), "Rhododendron", "Acer"),
         regime = if_else(str_detect(name, "Shal"), "Shallow", if_else(str_detect(name, "Deep"), "Deep", "Low GW")),
         regime = fct_relevel(regime, c("Shallow", "Deep", "Low GW"))
  )%>%
  dplyr::filter(date <= as.Date("2020-07-31"))

    
### find difference values 
compare_warm_st_VALUE <- compare_warm %>%
  group_by(scen_type)%>%
  summarise(StStock_avgmonth = mean(StStock_avgmonth),
            Breakdown_meantotalmon = mean(Breakdown_totmon))%>%
  dplyr::select(-Breakdown_meantotalmon)%>%
  pivot_wider(names_from = scen_type, values_from = StStock_avgmonth)%>%
  mutate(
    A_Deep_50y = (A_DeepGW_Warm - A_DeepGW)/1000,
    R_Deep_50y = (R_DeepGW_Warm - R_DeepGW)/1000,
    A_Shal_50y = (A_ShalGW_Warm - A_ShalGW)/1000,
    R_Shal_50y = (R_ShalGW_Warm - R_ShalGW)/1000,
    A_NoGW_50y = (A_NoGW_Warm - A_NoGW)/1000,
    R_NoGW_50y = (R_NoGW_Warm - R_NoGW)/1000
  )%>%
  pivot_longer(cols = 19:24)%>%
  dplyr::select(name, value) #%>%


compare_warm_st <- compare_warm %>%
  dplyr::select(-Breakdown_totmon)%>%
  pivot_wider(names_from = scen_type, values_from = StStock_avgmonth)%>%
  mutate(
    A_Deep_50y = (A_DeepGW_Warm - A_DeepGW)/A_DeepGW,
    R_Deep_50y = (R_DeepGW_Warm - R_DeepGW)/R_DeepGW,
    A_Shal_50y = (A_ShalGW_Warm - A_ShalGW)/A_ShalGW,
    R_Shal_50y = (R_ShalGW_Warm - R_ShalGW)/R_ShalGW,
    A_NoGW_50y = (A_NoGW_Warm - A_NoGW)/A_NoGW,
    R_NoGW_50y = (R_NoGW_Warm - R_NoGW)/R_NoGW
  )%>%
  pivot_longer(cols = 20:25)%>% #the calculated values/percents
  dplyr::select(month, name, value) %>%
  mutate(type = if_else(str_starts(name, "R"), "Rhododendron", "Acer"),
         regime = if_else(str_detect(name, "Shal"), "Shallow", if_else(str_detect(name, "Deep"), "Deep", "Low GW")),
         regime = fct_relevel(regime, c("Shallow", "Deep", "Low GW")),
         month = factor(month , ordered = FALSE ),
         month = factor(month, levels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"), ordered = TRUE) 
  )

compare_warm_st$value_perc <- format(compare_warm_st$value *100, scientific=F)

compare_warm_st_VALUE_month <- compare_warm %>%
  group_by(scen_type, month)%>%
  dplyr::select(-Breakdown_totmon)%>%
  pivot_wider(names_from = scen_type, values_from = StStock_avgmonth)%>%
  mutate(
    A_Deep_50y = (A_DeepGW_Warm - A_DeepGW),
    R_Deep_50y = (R_DeepGW_Warm - R_DeepGW),
    A_Shal_50y = (A_ShalGW_Warm - A_ShalGW),
    R_Shal_50y = (R_ShalGW_Warm - R_ShalGW),
    A_NoGW_50y = (A_NoGW_Warm - A_NoGW),
    R_NoGW_50y = (R_NoGW_Warm - R_NoGW)
  )%>%
  pivot_longer(cols = 20:25)%>%
  dplyr::select(month, name, value) 

compare_warm_st_annual <- compare_warm_st %>%
  group_by(name, type)%>%
  summarise(
    avg_yrper = mean(value) * 100
  )

compare_warm_k <- compare_warm %>%
  dplyr::select(-StStock_avgmonth)%>%
  pivot_wider(names_from = scen_type, values_from = Breakdown_totmon)%>%
  mutate(
    A_Deep_50y = (A_DeepGW_Warm - A_DeepGW)/A_DeepGW,
    R_Deep_50y = (R_DeepGW_Warm - R_DeepGW)/R_DeepGW,
    A_Shal_50y = (A_ShalGW_Warm - A_ShalGW)/A_ShalGW,
    R_Shal_50y = (R_ShalGW_Warm - R_ShalGW)/R_ShalGW,
    A_NoGW_50y = (A_NoGW_Warm - A_NoGW)/A_NoGW,
    R_NoGW_50y = (R_NoGW_Warm - R_NoGW)/R_NoGW
  )%>%
  pivot_longer(cols = 20:25)%>%
  dplyr::select(month, name, value) %>%
  mutate(type = if_else(str_starts(name, "R"), "Rhododendron", "Acer"),
         regime = if_else(str_detect(name, "Shal"), "Shallow", if_else(str_detect(name, "Deep"), "Deep", "Low GW")),
         regime = fct_relevel(regime, c("Shallow", "Deep", "Low GW")),
         month = factor(month , ordered = FALSE ),
         month = factor(month, levels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"), ordered = TRUE) 
  )

compare_warm_k$value_perc <- format(compare_warm_k$value *100, scientific=F)
### PLOTS
scen_colors_GW_50 <- c(
  "#b33605", #shal2,
  "#3752bd", #deep0.5
  "#961d53" #noGW2


)

custom_fill_50 <- scale_fill_manual(name = "Thermal Regime", 
                                                    values = scen_colors_GW_50,
                                                    labels = c('Shallow GW-fed','Deep GW-fed', "Air-Coupled")
)

custom_color_50 <- scale_color_manual(name = "Thermal Regime", 
                                     values = scen_colors_GW_50,
                                     labels = c('Shallow GW-fed', 'Deep GW-fed', "Air-Coupled")
)

warm_st <- ggplot(compare_warm_st)+
  geom_col(aes(month, value *100, fill = regime), position = "dodge", width = 0.5)+
  custom_fill_50+
  labs(x = "", y = paste0("50-year % Change in","\n", "Avg Monthly Standing Stock"))+
  theme_bw(base_size = 8)+
  #ylim(0,NA)+
  #scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))+
  facet_grid(~type)

ggsave("output_v4/figures/TemperatureSc_perc50year_ststock.png", width = 4, height = 3, units = "in" )

warm_k <- ggplot(compare_warm_k)+
  geom_col(aes(month, value *100, fill = regime), position = "dodge", width = 0.5)+
  custom_fill_50+
  labs(x = "", y = paste0("50-year % Change in","\\\\n", "Total Monthly POC Breakdown"))+
  theme_bw(base_size = 8)+
  #ylim(0,NA)+
  #scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)
        )+
  facet_grid(~type)

#warm <- 
plot_grid(warm_st, warm_k +
            theme(
              legend.position = "bottom",
              legend.direction = "horizontal",
              legend.background = element_rect(fill = NA, color = "black"))+
            guides(color=guide_legend(ncol=1))
          , labels = c('A', 'B'), label_size = 12, ncol = 1,
          rel_heights = c(0.8, 1))
ggsave("output_v4/figures_final/TemperatureSc_perc50year_k.svg", width = 4, height = 4, units = "in" )
ggsave("output_v4/figures_final/TemperatureSc_perc50year_k.png", width = 4, height = 4, units = "in" )

#### Revsions ## change figure to remove breakdown and add similar to figure3 
scen_colors_50 <- c(
  #"black", #base
  "#fa7e1e", #shalGW
  "#90a6fd", #deepGW
  "#d62976", #noGW
  "#961d53", #noGW2
  "#182c7a", #deep0.5
  "#b33605" #shal2
)

p2_warm <- df_day %>%
  dplyr::filter(scenario != "Base") %>% #scenario == "NoGW" | scenario == "DeepGW" | scenario == "ShalGW")%>%
  dplyr::filter(frag_mod == "M")%>%
  ggplot(.)+
  #geom_ribbon(aes(date_d, ymin = min_StStock_daily/1000, ymax = max_StStock_daily/1000, fill = scenario, alpha = type))+
  geom_line(aes(date_d, C_StStock_gC_total/1000,  color = scenario, linetype = type), linewidth = 1)+
  geom_line(aes(date_d, min_StStock_daily/1000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  geom_line(aes(date_d, max_StStock_daily/1000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  scale_color_manual(values= scen_colors_50)+
  scale_alpha_discrete(range = c(0.1, 0.15))+
  #ggtitle("Warming Scenario")+
  labs(x = "", y = paste0("Watershed POC", "\n", "50-year Difference in", "\n", "Standing Stock (kg C)"))+
  ylim(0,NA)+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

ggsave("output_v4/figures/TemperatureSc_GW_StandingStock_50year.png", width = 4, height = 3, units = "in" )

p2_warm_perc <- compare_warm_st_TS %>%
  #dplyr::filter(scenario != "Base") %>% #scenario == "NoGW" | scenario == "DeepGW" | scenario == "ShalGW")%>%
  #dplyr::filter(frag_mod == "M")%>%
  ggplot(.)+
  geom_line(aes(as.Date(date), value/1000,  color = regime, linetype = type), linewidth = 1)+
  #scale_color_manual(values= scen_colors_50)+
  custom_color_50+
  #ggtitle("Warming Scenario")+
  labs(x = "", y = paste0("50-year Difference in", "\n", "Watershed Standing Stock", "\n", "(Mg C)"))+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month", expand =c(0,0))+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        strip.text.x = element_blank())+
  facet_grid(~type)

ggsave("output_v4/figures/TemperatureSc_GW_StandingStock_50year.png", width = 4, height = 3, units = "in" )

p_warm_TS <- df_day %>%
  dplyr::filter(scenario != "Base") %>%
  dplyr::filter(frag_mod == "M")%>%
  dplyr::mutate(scen_50 = ifelse(str_detect(scenario,"NoGW"), "Air-Coupled", 
                                 ifelse(str_detect(scenario,"DeepGW"), 'Deep GW-fed', 'Shallow GW-fed')),
                type = if_else(str_starts(type, "R"), "Rhododendron", "Acer"))%>%
  
  ggplot(.)+
  #geom_ribbon(aes(date_d, ymin = min_StStock_daily/1000, ymax = max_StStock_daily/1000, fill = scenario, alpha = type))+
  geom_line(aes(date_d, C_StStock_gC_total/1000000,  color = scenario, linetype = type), linewidth = 1)+
  #geom_line(aes(date_d, min_StStock_daily/1000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  #geom_line(aes(date_d, max_StStock_daily/1000,  color = scenario, linetype = type), linewidth = 1, alpha = 0.2)+
  scale_color_manual(values= scen_colors_50)+
  #scale_alpha_discrete(range = c(0.1, 0.15))+
  #ggtitle("Warming Scenario")+
  labs(x = "", y = paste0("Watershed", "\n", "POC Standing Stock", "\n", "(Mg C)"))+
  ylim(0,NA)+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month", expand =c(0,0))+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        strip.text.y = element_blank())+
  facet_grid(scen_50 ~type)

p_warm_st <- plot_grid(warm_st, p2_warm +
                         theme(
                           legend.position = "top",
                           legend.direction = "horizontal",
                           legend.background = element_rect(fill = NA, color = "black"))+
                         guides(color=guide_legend(ncol=2))
                       , labels = c('A', 'B'), label_size = 10, ncol = 1,
                       rel_heights = c(0.8, 1))
ggsave("output_v4/figures_final/TemperatureSc_perc50year_TS.svg", width = 4, height = 7, units = "in" )
ggsave("output_v4/figures_final/TemperatureSc_perc50year_TS.png", width = 4, height = 7, units = "in" )

p_warm_st <- plot_grid(p_warm_TS+ 
                         theme(
                           legend.position = "top",
                           legend.direction = "horizontal",
                           legend.text= element_text(size=8),
                           legend.background = element_rect(fill = NA, color = "black"))+
                         guides(fill = "none", linetype = "none"),
                       p2_warm_perc, 
                       warm_st+
                        
                         guides(fill = "none", linetype = "none"), #guide_legend(ncol=2))
                       labels = c('A', 'B', "C"), label_size = 8, ncol = 1,
                       rel_heights = c(1, 0.5, 0.8))

ggsave("output_v4/figures_final/TemperatureSc_perc50year_TS.svg", width = 4, height = 6, units = "in" )
ggsave("output_v4/figures_final/TemperatureSc_perc50year_TS.png", width = 4, height = 6, units = "in" )

dodge <- position_dodge(width=0.5)





###################################
## Figure 1 Temperature and Breakdown and Input
# Also Warming Figures Not Used
#################################
scen_colors_50 <- c(
  #"black", #base
  "#fa7e1e", #shalGW
  "#90a6fd", #deepGW
  "#d62976", #noGW
  "#961d53", #noGW2
  "#182c7a", #deep0.5
  "#b33605" #shal2
)

names(scen_colors_50) <- levels(df_day$scenario)[-1] #remove base
custom_colors <- scale_colour_manual(name = "Thermal Regime", values = scen_colors_50)

qual_dfA <- df_day %>%
  dplyr::filter(type == "Acer")


p_T <- ggplot(qual_dfA)+
  geom_line(aes(date_d, tempC, color = scenario), linewidth = 0.5)+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  custom_colors+
  labs(x = "", y = bquote("Stream Temperature"~degree*C))+
  theme_bw(base_size = 10)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  #facet_grid(rows = "regime")+
  theme(legend.position = "none",
        #legend.position = c(0.5, 0.92),
        #legend.direction = "horizontal",
        #legend.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y = element_blank())

ggsave("output_v4/figures_final/TempC_scen.svg", height = 2, width = 4, units = "in")


## plotting IN and Temperatuer together

scen_colors_50 <- c(
  "black", #base
  "#fa7e1e", #shalGW
  "#90a6fd", #deepGW
  "#d62976", #noGW
  "#961d53", #noGW2
  "#182c7a", #deep0.5
  "#b33605" #shal2
)

#names(scen_colors_50) <- levels(df_day$scenario)[-1] #remove base
names(scen_colors_50) <- levels(df_day$scenario)[-5:-7] #remove warming conditions

#set up legend 
custom_colors <- scale_colour_manual(name = "Stream Thermal Regime", 
                                     values = scen_colors_50,
                                     labels = c('Coweeta','Shallow GW-Fed','Deep GW-Fed', "Air-Coupled")
)



max(qual_dfA$C_LitterIn_gC)

coeff <- 28 

p_T_In <- qual_dfA%>%
  dplyr::filter(!str_detect(scenario, "Warm"))%>%
  ggplot(.)+
  geom_line(aes(date_d, C_LitterIn_gC/1000/coeff), color = "darkgreen", linewidth = 1)+
  geom_line(aes(date_d, tempC, color = scenario), linewidth = 0.5)+
  custom_colors+
  labs(x = "") +#, y = bquote("Stream Temperature"~degree*C))+
  theme_bw(base_size = 10)+
  labs(x = "")+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  scale_y_continuous(
    # Features of the first axis
    name = bquote("Stream Temperature"~degree*C),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name= paste0("Litter Input" , "\n", "(kgC day-1)"))
  ) +
  #facet_grid(rows = "regime")+
  theme(#legend.position = "none",
        axis.text.x=element_blank(),
        legend.position = "top",
        #legend.position = c(0.5, 0.7),
        #legend.direction = "horizontal",
        legend.background = element_rect(fill = NA, color = "black"),
        #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.text.y = element_blank(),
        #axis.title.y.left = element_text(colour="blue"),
        axis.title.y.right = element_text(colour="darkgreen"),
        axis.text.y.right = element_text(colour="darkgreen"))+
  guides(color=guide_legend(ncol=2))

max(df_day$Q_outlet/3600)
library(scales)
coeff_QW <- 1E5

p_Q_width <- ggplot(qual_dfA)+
  geom_line(aes(date_d, Q_outlet/3600), color = "blue", linewidth = 0.5)+
  geom_line(aes(date_d, WS_benthic_area_m2/coeff_QW), color = "brown", linewidth = 0.5)+
  #labs(x = "", y = bquote("Baseflow @ Network Outlet (m3/s)"))+
  theme_bw(base_size = 10)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month", name = "")+
  scale_y_continuous(
    # Features of the first axis
    name = paste0("Baseflow ","\n", "Outlet (m3/s)"),
    #element_text(colour = "blue"),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_QW, name= expression(paste("Summed Watershed", "\n", "Benthic Area (m"^"-2"*")")),
                        labels = scales::scientific)
  ) +
  #facet_grid(rows = "regime")+
  theme(legend.position = "none",
    #legend.position = c(0.5, 0.92),
    #legend.direction = "horizontal",
    #legend.background = element_rect(fill = "white", color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.text.y = element_blank(),
    axis.title.y.left = element_text(colour="blue"),
    axis.text.y.left = element_text(colour="blue"),
    axis.title.y.right = element_text(colour="brown"),
    axis.text.y.right = element_text(colour="brown"))



#https://stackoverflow.com/questions/59826366/ggplot-change-the-x-axis-label-colors-dynamically

egg::ggarrange(p_T_In, p_Q_width, labels = c('C', 'D'), label_size = 12, ncol = 1,
          align = "hv",
          rel_heights = c(2, 1))


ggsave("output_v4/ModelInputParameters.svg", width = 7, height = 4, units = "in" )
ggsave("output_v4/figures_final/ModelInputParameters.png", width = 7, height = 4, units = "in" )







#### FIgure GW temp versus supply

plot_grid(p_GW_cuml_GW, labels = c('A', 'B'), label_size = 12, ncol = 1,
          align = "hv",
          rel_heights = c(2, 2))




p_IN <- df_base_sp_dailysum %>%
  dplyr::filter(date_d > as.Date("2019-07-31", format = "%Y-%m-%d") & date_d < as.Date("2020-07-31", format = "%Y-%m-%d")) %>%
  mutate( date_d = as.Date(date_d, format = "%Y-%m-%d")) %>%
  ggplot(.)+
  geom_ribbon(aes(x = date_d, ymin = 0, ymax = ClocalLit_AFDMg_slow *0.484/1000), fill = "darkgreen", size = 0.5, alpha = 0.5)+
  geom_ribbon(aes(date_d, ymin = 0, ymax = (ClocalLit_AFDMg_slow + ClocalLit_AFDMg_fast)*0.484/1000), colour = "black", alpha = 0.5)+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values= scen_colors_mix)+
  labs(x = "", y = "POC Input (kg C /day)")+
  theme_bw(base_size = 8)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
        strip.text.y = element_blank())

ggsave("output_v4/figures_final/LitterInput.png", height = 2, width = 3.25, units = "in")
ggsave("output_v4/figures_final/LitterInput.svg", height = 2, width = 3.25, units = "in")

plot_grid(p_T, p_k, p_IN, labels = c('A', 'B', 'C'), label_size = 12, ncol = 2,
          align = "hv")
ggsave("output_v4/figures_final/Input_Parameters.svg", height = 4, width = 7, units = "in")

df_noGW_R_all_1 <- df_noGW_R_all %>%
  filter(date_d == as.Date("2020-01-01"))

test <- rep(1:365, each=440)

##_-----------------------------------------##
## Create Supplemental 4 Model Output Tables
## -----------------------------------------##

library(sjPlot)
library(lme4)
library(webshot)
library(nlme)
library(sjstats)
mod_k_AM <- readRDS("data/final_lmer_model_network/acer_microbes_model.RDS")
mod_k_AF <- readRDS("data/final_lmer_model_network/acer_shredder_model.RDS")
mod_k_RM <- readRDS("data/final_lmer_model_network/rhodo_microbes_model.RDS")
mod_k_RF <- readRDS("data/final_lmer_model_network/rhodo_shredder_model.RDS")

tab_model(mod_k_AM, title = "Acer Microbial Litter Bag Breakdown Rate Linear Mixed Effects Model")
mse(mod_k_AM)
tab_model(mod_k_AF, title = "Acer Detritivore Litter Bag Breakdown Rate Linear Mixed Effects Model")
mse(mod_k_AF)
tab_model(mod_k_RM, title = "Rhododendron Microbial Litter Bag Breakdown Rate Linear Mixed Effects Model")
mse(mod_k_RM)
tab_model(mod_k_RF, title = "Rhododendron Detritivore Litter Bag Breakdown Rate Linear Mixed Effects Model")
mse(mod_k_RF)

