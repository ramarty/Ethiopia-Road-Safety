# Ethiopia Road Safety: Figures and Maps

# Black spots:
# https://docs.wixstatic.com/ugd/0bf2db_df739a5e70d54fa6b248f642a259ffe4.pdf

# Run master to setup ----------------------------------------------------------
if(Sys.info()[["user"]] == "johnloesser") source("~/Dropbox/research/2017/ethroads/Ethiopia IE/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "robmarty") source("~/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")
if(Sys.info()[["user"]] == "WB521633") source("C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Road Safety/code/etre_analysis/master.R")

GENERATE_TRAFFIC_PLOTS <- FALSE # Traffic dataset is large, so only load if need to recreate plots

# Load Data --------------------------------------------------------------------
load(file.path(intermediate_data_file_path, "accident_data.Rda"))
load(file.path(intermediate_data_file_path, "traffic_accidents_hourly_long.Rda"))
load(file.path(intermediate_data_file_path, "traffic_accidents_daily_long.Rda"))
load(file.path(intermediate_data_file_path, "addis_adama_points_data.Rda"))

dataset_hours_long$precipitation[is.na(dataset_hours_long$precipitation)] <- 0

# ** FIGURES ** ================================================================

# Traffic Flow -----------------------------------------------------------------
if(F){
load(file.path(intermediate_data_file_path, "traffic_data.Rda"))
traffic_all$direction_to_adama <- traffic_all$ENT_PlazaID < traffic_all$Plaza_ID

# Entrance/Exit KMs
entrance_df <- cbind(c(0,101,201,301,401,501,503,601),
                     c(2,2,  16, 33, 52, 60, 60, 64)) %>% as.data.frame
names(entrance_df) <- c("plaza","entrance_km")

exit_df <- cbind(c(102, 202, 302, 402, 502, 504, 602),
                 c(2,16,33,52,60,60,64)) %>% as.data.frame
names(exit_df) <- c("plaza","exit_km")

traffic_all <- merge(traffic_all, entrance_df, by.x="ENT_PlazaID", by.y="plaza")
traffic_all <- merge(traffic_all, exit_df, by.x="Plaza_ID", by.y="plaza")

traffic_all$N <- 1
traffic_all_sum <- summaryBy(N ~ entrance_km + exit_km + direction_to_adama, data=traffic_all, FUN=sum, keep.names=T)
traffic_all_sum <- traffic_all_sum[traffic_all_sum$entrance_km != traffic_all_sum$exit_km,]

traffic_all_sum <- traffic_all_sum[order(-traffic_all_sum$N),] 
traffic_all_sum$N_length <- nchar(as.character(traffic_all_sum$N))
traffic_all_sum$N_pretty <- prettyNum(traffic_all_sum$N, big.mark=",",scientific=FALSE)

traffic_all_sum_toaddis <- traffic_all_sum[traffic_all_sum$direction_to_adama == FALSE,]
traffic_all_sum_toadama <- traffic_all_sum[traffic_all_sum$direction_to_adama == TRUE,]

traffic_all_sum_toadama$y <- 1:nrow(traffic_all_sum_toadama)*2 + c(0,0,0,0,-.1,-.2,-.3,-.4,-.5,-.6,-.7,-.8,-.9,-1,-1.1)
traffic_all_sum_toaddis$y <- 1:nrow(traffic_all_sum_toaddis)*2 + c(0,0,0,0,-.1,-.2,-.3,-.4,-.5,-.6,-.7,-.8,-.9,-1,-1.1)

traffic_flows <- ggplot() + 
  geom_vline(xintercept=2,color="gray80",alpha=1) + 
  geom_vline(xintercept=16,color="gray80",alpha=1) + 
  geom_vline(xintercept=33,color="gray80",alpha=1) +
  geom_vline(xintercept=52,color="gray80",alpha=1) + 
  geom_vline(xintercept=60,color="gray80",alpha=1) + 
  geom_vline(xintercept=64,color="gray80",alpha=1) + 
  geom_segment(data=traffic_all_sum_toaddis, 
               aes(x=entrance_km, xend=exit_km, y=-y, yend=-y), 
               size=(traffic_all_sum_toaddis$N/max(traffic_all_sum_toaddis$N))*6+.1,
               arrow = arrow(type = "open"),
               lineend ="butt",linejoin ="mitre",color="darkorange2") +
  geom_segment(data=traffic_all_sum_toadama,
               aes(x=entrance_km, xend=exit_km, y=y, yend=y), 
               size=(traffic_all_sum_toadama$N/max(traffic_all_sum_toadama$N))*6+.1,
               arrow = arrow(type = "open"),
               lineend ="butt",linejoin ="mitre",color="dodgerblue3") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank()) + 
  scale_x_continuous(breaks=c(2, 16, 33, 52, 60, 64)) + 
  labs(x=paste0("Addis Ababa",paste(rep(" ",120),collapse=""), "Adama"),
       y="") + 
  geom_text(data=traffic_all_sum_toadama, aes(x=exit_km+(N_length-2.5), y=y, label=N_pretty),color="gray40", size=3.5) +
  geom_text(data=traffic_all_sum_toaddis, aes(x=exit_km-(N_length-2.5), y=-y, label=N_pretty),color="gray40",size=3.5)
ggsave(traffic_flows, file=file.path(figures_file_path,"traffic_flows.png"), height=9,width=8)
}

# Accident Map -----------------------------------------------------------------
accidents_all$N <- 1
accidents_all$km <- round(accidents_all$accident_loc_dist / 1000)

accidents_km <- summaryBy(latitude+longitude+N~km,data=accidents_all, FUN=c(sum,mean))

coordinates(accidents_km) <- ~longitude.mean+latitude.mean
crs(accidents_km) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Make Map
pal <- colorNumeric(c("blue", "orange", "red"), 1:max(accidents_km$N.sum))

leaflet() %>% 
  # Base groups
  addTiles(group = "OSM (Default)") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>% 
  # Layers
  addCircles(data=accidents_km, fillOpacity = 1, stroke=T, fillColor = ~pal(N.sum),weight=1, color="black", radius=~N.sum*35+10, group="Accidents Every 10km") %>%
  # Layers Controls
  addLayersControl(
    baseGroups = c("OSM (Default)", "CartoDB"),
    overlayGroups = c("Accidents Every 10km", "Accident Locations"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

# Accident Frequency Per Day ---------------------------------------------------
accidents_per_day <- accidents_all$`Accident Date` %>% 
  table %>% 
  as.data.frame %>% 
  dplyr::rename(day = ".") %>% 
  dplyr::rename(N_accidents = Freq)
accidents_per_day <- accidents_per_day[!grepl("1900", accidents_per_day$day),]

days_df <- seq(as.Date("2015-01-01"), as.Date("2017-12-31"), by="days") %>% 
  as.data.frame %>% 
  dplyr::rename(day = ".")
days_df$day <- days_df$day %>% as.character
days_df <- merge(days_df, accidents_per_day, by="day",all.x=T)
days_df$N_accidents[is.na(days_df$N_accidents)] <- 0
days_df$year <- days_df$day %>% substring(1,4)

days_df$num_days <- 1
days_accidents_df <- summaryBy(num_days ~ N_accidents+year, data=days_df, FUN=sum, keep.names=T)

accidents_in_day <- ggplot(data=days_accidents_df, aes(x=N_accidents, y=num_days, fill=year)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(x="Number of Crashes That Occur in a Day", y="Days", fill="") +
  scale_x_continuous(breaks=0:8) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave(accidents_in_day, file=file.path(figures_file_path,"accidents_in_day.png"), height=4,width=8)

# Traffic vs Accidents ---------------------------------------------------------
dataset_days_long <- dataset_days_long[,1:21]
MA21_traffic_accident_trends <- ggplot(data=dataset_days_long[dataset_days_long$direction == "Total",], aes(x=day)) + 
  geom_line(aes(y=MA_21_day_traffic, color="Average Traffic Flow Within 3 Week Period")) + 
  geom_line(aes(y=MA_21_day_accidents/.0001, color="Total Number of Crashes within 3 Week Period")) + 
  scale_y_continuous(sec.axis = sec_axis(~.*.0001, name = "Crashes")) +
  scale_colour_manual(values = c("darkorange3", "dodgerblue3")) +
  labs(y = "Traffic",
       x = "",
       colour = "",
       title = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.22, 0.96)) 
ggsave(MA21_traffic_accident_trends, file=file.path(figures_file_path,"MA21_traffic_accident_trends.png"), height=4,width=8)

MA21_traffic_accident_plot <- ggplot(data=dataset_days_long[dataset_days_long$direction == "Total",],
                                     aes(x=MA_21_day_traffic, y=MA_21_day_accidents)) + 
  geom_point(size=.75) +
  geom_smooth(method = "lm", se = FALSE, color="red3", size=.75) +
  labs(x="Traffic (Average within 3 week period)",y="Crashes (Total within 3 Week period)", title="") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave(MA21_traffic_accident_plot, file=file.path(figures_file_path,"MA21_traffic_accident_plot.png"), height=4,width=5)

# Speed ------------------------------------------------------------------------
speed_histogram <- ggplot(dataset_days_long[dataset_days_long$direction == "Total",], aes(speed_perc_0.5)) +
  geom_histogram(bins=60,color="white",fill="darkslategray4") + 
  theme_minimal() + 
  labs(x="Median Speed, Daily [km/hr]",y="Number of Days")
ggsave(speed_histogram, file=file.path(figures_file_path,"speed_histogram.png"), height=6,width=8)

# Accident Cause ---------------------------------------------------------------
# Summary Table
accidents_all$accident_cause_simple[accidents_all$accident_cause_simple %in% "Vehicle Crush"] <- "Vehicle Crash"
accident_cause <- ggplot(accidents_all[accidents_all$accident_cause_vehicle_human != "Other",]) +
  geom_bar(aes(accident_cause_simple, fill = accident_cause_vehicle_human),color="black") +
  coord_flip() +
  theme_minimal() + 
  labs(x="", y="Number of Crashes",fill="Crash Cause\nCategory",title="Crash Cause") +
  scale_fill_manual(values=c("darkslategray4","chocolate1")) +
  theme(plot.title = element_text(hjust = 0.5))
accident_cause
ggsave(accident_cause, file=file.path(figures_file_path,"accident_cause.png"), height=4,width=7)

# Distributions
accident_distribution_theme <- theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size=10),
          panel.border = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank())

y_scale_limit_max_acc_cause <- (c(addis_adama_points_data$MA2000_N_accidents_AddisDirection_speeding/addis_adama_points_data$traffic_to_addis,
                       addis_adama_points_data$MA2000_N_accidents_AdamaDirection_speeding/addis_adama_points_data$traffic_to_adama,
                       addis_adama_points_data$MA2000_N_accidents_AddisDirection_unethicaldriving/addis_adama_points_data$traffic_to_addis,
                       addis_adama_points_data$MA2000_N_accidents_AdamaDirection_unethicaldriving/addis_adama_points_data$traffic_to_adama) * 1000) %>% max

accident_cause_speeding <- ggplot(addis_adama_points_data, aes(x=distance_adama_direction)) +
  geom_line(aes(y=MA2000_N_accidents_AddisDirection_speeding/traffic_to_addis*1000), color="blue",size=1) +
  geom_line(aes(y=MA2000_N_accidents_AdamaDirection_speeding/traffic_to_adama*1000), color="orange",size=1) +
  labs(x=paste0("Addis Ababa",paste(rep(" ",110),collapse=""), "Adama"), y="", title="Speeding") +
  scale_y_continuous(limits=c(0,y_scale_limit_max_acc_cause)) +
  accident_distribution_theme

accident_cause_unethical <- ggplot(addis_adama_points_data, aes(x=distance_adama_direction)) +
  geom_line(aes(y=MA2000_N_accidents_AddisDirection_unethicaldriving/traffic_to_addis*1000), color="blue",size=1) +
  geom_line(aes(y=MA2000_N_accidents_AdamaDirection_unethicaldriving/traffic_to_adama*1000), color="orange",size=1) +
  labs(x=paste0("Addis Ababa",paste(rep(" ",110),collapse=""), "Adama"), y="", title="Unethical Driving") +
  scale_y_continuous(limits=c(0,y_scale_limit_max_acc_cause)) +
  accident_distribution_theme

accident_cause_append <- grid.arrange(accident_cause_speeding, 
             accident_cause_unethical,
             left="Accidents/1000 Vehicles within 1km")
ggsave(accident_cause_append, file=file.path(figures_file_path,"accident_cause_distributions.png"), height=6,width=7)

# Accident Type ----------------------------------------------------------------
# Summary Table
accidents_all$accident_type_simple <- gsub("Crush", "Crash", accidents_all$accident_type_simple)
accident_type <- ggplot(accidents_all[accidents_all$accident_cause_vehicle_human != "Other",]) +
  geom_bar(aes(accident_type_simple, fill=accident_cause_vehicle_human),color="black") +
  coord_flip() +
  theme_minimal() + 
  scale_fill_manual(values=c("darkslategray4","chocolate1")) +
  labs(x="", y="Number of Crashes",fill="Crash Cause\nCategory",title="Crash Type") +
  theme(plot.title = element_text(hjust = 0.5))
accident_type
ggsave(accident_type, file=file.path(figures_file_path,"accident_type.png"), height=4,width=7)

# Distributions

y_scale_limit_max_acc_type <- (c(addis_adama_points_data$MA2000_N_accidents_AddisDirection_crush_gaurdrail/addis_adama_points_data$traffic_to_addis,
                                  addis_adama_points_data$MA2000_N_accidents_AdamaDirection_crush_gaurdrail/addis_adama_points_data$traffic_to_adama,
                                  addis_adama_points_data$MA2000_N_accidents_AddisDirection_crush_property/addis_adama_points_data$traffic_to_addis,
                                  addis_adama_points_data$MA2000_N_accidents_AdamaDirection_crush_property/addis_adama_points_data$traffic_to_adama,
                                 addis_adama_points_data$MA2000_N_accidents_AddisDirection_overthrow/addis_adama_points_data$traffic_to_addis,
                                 addis_adama_points_data$MA2000_N_accidents_AdamaDirection_overthrow/addis_adama_points_data$traffic_to_adama,
                                 addis_adama_points_data$MA2000_N_accidents_AddisDirection_crush/addis_adama_points_data$traffic_to_addis,
                                 addis_adama_points_data$MA2000_N_accidents_AdamaDirection_crush/addis_adama_points_data$traffic_to_adama,
                                 addis_adama_points_data$MA2000_N_accidents_AddisDirection_vehicle_crush/addis_adama_points_data$traffic_to_addis,
                                 addis_adama_points_data$MA2000_N_accidents_AdamaDirection_vehicle_crush/addis_adama_points_data$traffic_to_adama) * 1000) %>% max

accident_type_gaurdrail <- ggplot(addis_adama_points_data, aes(x=distance_adama_direction)) +
  geom_line(aes(y=MA2000_N_accidents_AddisDirection_crush_gaurdrail/traffic_to_addis*1000), color="blue",size=1) +
  geom_line(aes(y=MA2000_N_accidents_AdamaDirection_crush_gaurdrail/traffic_to_adama*1000), color="orange",size=1) +
  labs(x=paste0("Addis Ababa",paste(rep(" ",110),collapse=""), "Adama"), y="", title="Gaurdrail") +
  scale_y_continuous(limits=c(0,y_scale_limit_max_acc_type)) +
  accident_distribution_theme

accident_type_crush_property <- ggplot(addis_adama_points_data, aes(x=distance_adama_direction)) +
  geom_line(aes(y=MA2000_N_accidents_AddisDirection_crush_property/traffic_to_addis*1000), color="blue",size=1) +
  geom_line(aes(y=MA2000_N_accidents_AdamaDirection_crush_property/traffic_to_adama*1000), color="orange",size=1) +
  labs(x=paste0("Addis Ababa",paste(rep(" ",110),collapse=""), "Adama"), y="", title="Crush Property") +
  scale_y_continuous(limits=c(0,y_scale_limit_max_acc_type)) +
  accident_distribution_theme

accident_type_overthrow <- ggplot(addis_adama_points_data, aes(x=distance_adama_direction)) +
  geom_line(aes(y=MA2000_N_accidents_AddisDirection_overthrow/traffic_to_addis*1000), color="blue",size=1) +
  geom_line(aes(y=MA2000_N_accidents_AdamaDirection_overthrow/traffic_to_adama*1000), color="orange",size=1) +
  labs(x=paste0("Addis Ababa",paste(rep(" ",110),collapse=""), "Adama"), y="", title="Overthrow") +
  scale_y_continuous(limits=c(0,y_scale_limit_max_acc_type)) +
  accident_distribution_theme

accident_type_crush <- ggplot(addis_adama_points_data, aes(x=distance_adama_direction)) +
  geom_line(aes(y=MA2000_N_accidents_AddisDirection_crush/traffic_to_addis*1000), color="blue",size=1) +
  geom_line(aes(y=MA2000_N_accidents_AdamaDirection_crush/traffic_to_adama*1000), color="orange",size=1) +
  labs(x=paste0("Addis Ababa",paste(rep(" ",110),collapse=""), "Adama"), y="", title="Crush") +
  scale_y_continuous(limits=c(0,y_scale_limit_max_acc_type)) +
  accident_distribution_theme

accident_type_vehicle_crush <- ggplot(addis_adama_points_data, aes(x=distance_adama_direction)) +
  geom_line(aes(y=MA2000_N_accidents_AddisDirection_vehicle_crush/traffic_to_addis*1000), color="blue",size=1) +
  geom_line(aes(y=MA2000_N_accidents_AdamaDirection_vehicle_crush/traffic_to_adama*1000), color="orange",size=1) +
  labs(x=paste0("Addis Ababa",paste(rep(" ",110),collapse=""), "Adama"), y="", title="Vehicle Crush") +
  scale_y_continuous(limits=c(0,y_scale_limit_max_acc_type)) +
  accident_distribution_theme

accident_type_append <- grid.arrange(accident_type_gaurdrail, 
             accident_type_crush_property,
             accident_type_overthrow,
             accident_type_crush,
             accident_type_vehicle_crush,
             left="Accidents/1000 Vehicles within 1km",
             ncol=1)
ggsave(accident_type_append, file=file.path(figures_file_path,"accident_type_distributions.png"), height=11,width=7)

##### Accidents by Axel Number ####
accidents_all$axle_number_category <- NA
accidents_all$axle_number_category[accidents_all$axle_number %in% "2"] <- "2 Axels"
accidents_all$axle_number_category[accidents_all$axle_number %in% "3"] <- "3 Axels"
accidents_all$axle_number_category[accidents_all$axle_number %in% c("4","5",">6")] <- ">4 Axels"
accidents_all$axle_number_category <- as.factor(accidents_all$axle_number_category)
accidents_all$axle_number_category <- factor(accidents_all$axle_number_category,levels(accidents_all$axle_number_category)[c(2,3,1)])

ggplot(accidents_all[accidents_all$accident_cause_vehicle_human != "Other" & !is.na(accidents_all$axle_number),]) +
  #geom_bar(aes(accident_type_simple), fill="darkslategray4",color="black") +
  geom_bar(aes(axle_number_category, fill=accident_cause_vehicle_human),color="black") +
  coord_flip() +
  theme_minimal() + 
  scale_fill_manual(values=c("darkslategray4","chocolate1")) +
  labs(x="", y="Number of Accidents",fill="Accident Cause\nCategory",title="Accident Type") +
  theme(plot.title = element_text(hjust = 0.5))

# Axel Probability
load(file.path(intermediate_data_file_path, "traffic_data.Rda"))
traffic_vehtype_freq_df <- traffic_all$VehType %>% table %>% as.data.frame %>% dplyr::rename(vehicle_type = ".") %>% dplyr::rename(traffic_vehicle_type_freq=Freq)
accident_vehtype_freq_df <- accidents_all$`Vehicle Type (V)` %>% table %>% as.data.frame %>% dplyr::rename(vehicle_type = ".") %>% dplyr::rename(accident_vehicle_type_freq=Freq)

vehtype_freq_df <- merge(traffic_vehtype_freq_df, accident_vehtype_freq_df, by="vehicle_type", all=F)
vehtype_freq_df$prob_accident <- vehtype_freq_df$accident_vehicle_type_freq / vehtype_freq_df$traffic_vehicle_type_freq

vehtype_freq_df$vehicle_type <- as.numeric(as.character(vehtype_freq_df$vehicle_type))
vehtype_freq_df$vehicle_type_factor <- factor(vehtype_freq_df$vehicle_type,
       labels=c("Small automobiles",
                "Minibus",
                "Medium Bus, ISUZU",
                "Big Size Bus, Medium Truck",
                "Heavy Truck Trailers [4 axel]",
                "Heavy Truck Trailers [5 axel]",
                "Heavy Truck Trailers [>5 axel]"))


accident_axel_freq <- ggplot(vehtype_freq_df) +
  geom_col(aes(x=vehicle_type_factor, y=accident_vehicle_type_freq),fill="darkcyan") +
  coord_flip() +
  labs(x="",y="Number of Accidents",title="Accidents by Vehicle Type") +
  theme_minimal()
ggsave(accident_axel_freq, file=file.path(figures_file_path,"accident_axel_freq.png"), height=7,width=6)

accident_axel_prob <- ggplot(vehtype_freq_df) +
  geom_col(aes(x=vehicle_type_factor, y=prob_accident),fill="darkcyan") +
  coord_flip() +
  labs(x="",y="Probability of Accidents",title="Probability of Accident by Vehicle Type") +
  theme_minimal()
ggsave(accident_axel_prob, file=file.path(figures_file_path,"accident_axel_prob.png"), height=7,width=6)



##### Accident Cause Type Alluvial 
accidents_all$freq <- 1
accidents_all_causetypesum <- summaryBy(freq ~ accident_type_simple+accident_cause_simple,data=accidents_all,keep.names=T,FUN=sum)

library(alluvial)
alluvial(accidents_all_causetypesum[,1:2], freq=accidents_all_causetypesum$freq,
         alpha=accidents_all_causetypesum$freq/max(accidents_all_causetypesum$freq),
         #col = ifelse(datW$dep16.4 == "depressed", "#ff0000", "#D3D3D3"),
         axis_labels = c("Year 1", "Year 2"),
         cex = 0.7)


##### Accident Locations
addis_adama_points_data$latitude_0 <- addis_adama_points_data$latitude - min(addis_adama_points_data$latitude) + .22
plaza_stations <- addis_adama_points_data[addis_adama_points_data$distance_adama_direction %in% c(2000,16000,33000,52000,60000,64000),]

scaleFactor=80
accidents_along_expressway <- ggplot(addis_adama_points_data, aes(x=longitude)) +
  geom_line(data=addis_adama_points_data, 
            aes(y=MA1000_N_accidents_AddisDirection_all, color="Towards Adama")) +
  geom_line(data=addis_adama_points_data, 
            aes(y=MA1000_N_accidents_AdamaDirection_all, color="Towards Addis")) +
  geom_line(data=addis_adama_points_data,
            aes(y=latitude_0 *scaleFactor)) +
  geom_point(data=plaza_stations,
             aes(y=latitude_0 *scaleFactor,fill="Toll Plaza"),color="red", size=1.5) +
  scale_y_continuous(sec.axis=sec_axis(~./scaleFactor),limits = c(0, 47)) + 
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y.right=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        axis.text.y.right=element_blank()) +
  labs(y="Accidents",color="",fill="",
       x=paste0("Addis Ababa",paste(rep(" ",120),collapse=""), "Adama")) +
  scale_color_manual(values=c("orange","blue")) +
  geom_text(aes(label="Addis Adama Expressway",x=39.11,y=38))
ggsave(accidents_along_expressway, file=file.path(figures_file_path,"accidents_along_expressway.png"), height=4,width=8)

addis_adama_points_data$traffic_to_addis <- addis_adama_points_data$traffic_to_addis_2015 + addis_adama_points_data$traffic_to_addis_2016 + addis_adama_points_data$traffic_to_addis_2017
addis_adama_points_data$traffic_to_adama <- addis_adama_points_data$traffic_to_adama_2015 + addis_adama_points_data$traffic_to_adama_2016 + addis_adama_points_data$traffic_to_adama_2017

scaleFactor=.000012*1000
accidents_per_vehicle_along_expressway <- ggplot(addis_adama_points_data, aes(x=longitude)) +
  geom_line(data=addis_adama_points_data, 
            aes(y=MA1000_N_accidents_AddisDirection_all/traffic_to_addis*1000, color="Towards Adama")) +
  geom_line(data=addis_adama_points_data, 
            aes(y=MA1000_N_accidents_AdamaDirection_all/traffic_to_adama*1000, color="Towards Addis")) +
  geom_line(data=addis_adama_points_data,
            aes(y=latitude_0 *scaleFactor)) +
  geom_point(data=plaza_stations,
             aes(y=latitude_0 *scaleFactor,fill="Toll Plaza"),color="red", size=1.5) +
  scale_y_continuous(sec.axis=sec_axis(~./scaleFactor),limits = c(0, 0.000007*1000)) + 
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y.right=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        axis.text.y.right=element_blank()) +
  labs(y="Accidents Per 1000 Vehicles",color="",fill="",
       x=paste0("Addis Ababa",paste(rep(" ",120),collapse=""), "Adama")) +
  scale_color_manual(values=c("orange","blue")) +
  geom_text(aes(label="Addis Adama Expressway",x=39.11,y=0.0000057*1000))
ggsave(accidents_per_vehicle_along_expressway, file=file.path(figures_file_path,"accidents_per_vehicle_along_expressway.png"), height=4,width=8)

# Summarize Accident Data ------------------------------------------------------
##### Accidents By Hour
accidents_by_hour <- table(accidents_all$hour) %>% as.data.frame
names(accidents_by_hour) <- c("hour","number_accidents")

accidents_by_hour_fig <- ggplot() +
  geom_col(data=accidents_by_hour, 
             aes(x=hour, y=number_accidents), fill="orange3") + 
  theme_minimal() + 
  labs(x="Hour") + 
  labs(y="Number of Accidents")
ggsave(accidents_by_hour_fig, file=file.path(figures_file_path,"accidents_by_hour.png"), height=4,width=7)

##### Accidents by Day of Week
accidents_all$Day %>% unique %>% sort
accidents_all$Day[grepl("Sun", accidents_all$Day)] <- "Sunday"
accidents_all$Day[grepl("Mon", accidents_all$Day)] <- "Monday"
accidents_all$Day[grepl("Tus|Tusday", accidents_all$Day)] <- "Tuesday"
accidents_all$Day[grepl("Wedn|Wedns|Wednsday|Wends|Wens|Wesdn|Wensday", accidents_all$Day)] <- "Wednesday"
accidents_all$Day[grepl("Thur|Thure|Thuresd", accidents_all$Day)] <- "Thursday"
accidents_all$Day[grepl("Firday", accidents_all$Day)] <- "Friday"
accidents_all$Day[grepl("Sat|Satu", accidents_all$Day)] <- "Saturday"

accidents_by_dayofweek <- accidents_all[!is.na(accidents_all$Day),]
accidents_by_dayofweek <- table(accidents_by_dayofweek$Day) %>% as.data.frame
accidents_by_dayofweek <- accidents_by_dayofweek[accidents_by_dayofweek$Freq>1,]

names(accidents_by_dayofweek) <- c("Day","Accidents")

accidents_by_dayofweek$Day <- as.factor(as.character(accidents_by_dayofweek$Day))

accidents_by_dayofweek$Day <- factor(accidents_by_dayofweek$Day,levels(accidents_by_dayofweek$Day)[c(2,6,7,5,1,4,3)])

accidents_by_dayofweek_fig <- ggplot() +
  geom_col(data=accidents_by_dayofweek, 
           aes(x=Day, y=Accidents), fill="orange3") + 
  theme_minimal() + 
  labs(x="") + 
  labs(y="Number of Accidents")
ggsave(accidents_by_dayofweek_fig, file=file.path(figures_file_path,"accidents_by_dayofweek.png"), height=4,width=7)

##### Quick Tables
sink(file.path(tables_file_path, "table_accident_type.tex"))
df <- accidents_all$`Type of accident` %>% tolower %>% table  %>% as.data.frame
names(df) <- c("Type","Freq")
xtable(df, caption="Accident Type")
sink()

sink(file.path(tables_file_path, "table_driver_age.tex"))
df <- accidents_all$`Driver Age` %>% as.numeric %>% table  %>% as.data.frame
names(df) <- c("Age","Freq")
xtable(df, caption="Driver Age")
sink()

sink(file.path(tables_file_path, "table_etre_asset_damage.tex"))
df <- accidents_all$`ETRE Asset Damage` %>% tolower %>% table  %>% as.data.frame
names(df) <- c("Type","Freq")
a<-xtable(df, caption="ETRE Asset Damage")
print(a,only.contents=T)
sink()

sink(file.path(tables_file_path, "table_extent_damage.tex"))
df <- accidents_all$`Extent of Damage` %>% tolower %>% table  %>% as.data.frame
names(df) <- c("Type","Freq")
xtable(df, caption="Extent of Damage")
sink()

sink(file.path(tables_file_path, "table_light_injury.tex"))
df <- accidents_all$`Light Injury` %>% as.numeric %>% table  %>% as.data.frame
names(df) <- c("Number Light Injuries in 1 Accident","Freq")
xtable(df, caption="Light Injury")
sink()

sink(file.path(tables_file_path, "table_serious_injury.tex"))
df <- accidents_all$`Serious Injury` %>% as.numeric %>% table  %>% as.data.frame
names(df) <- c("Number Serious Injuries in 1 Accident","Freq")
xtable(df, caption="Serious Injury")
sink()

accidents_all_N <- is.na(accidents_all) %>% as.data.frame
accidents_all_N$all <- 1
for(var in names(accidents_all_N)){
  accidents_all_N[[var]] <- 1-as.numeric(accidents_all_N[[var]])
}

names(accidents_all_N) <- gsub(" ","",names(accidents_all_N))
names(accidents_all_N) <- gsub("\\(","",names(accidents_all_N))
names(accidents_all_N) <- gsub(")","",names(accidents_all_N))

vars <- summaryBy(.~all,data=accidents_all_N,keep.names=T,FUN=sum,na.rm=T) %>% t %>% as.data.frame
names(vars) <- c("N")

sink(file.path(tables_file_path, "variables.tex"))
xtable(vars, caption="Variables")
sink()

# Prep Variables for Regressions -----------------------------------------------
dataset_days_long$year <- substring(dataset_days_long$day,1,4) %>% as.numeric
dataset_hours_long$year <- substring(dataset_hours_long$day,1,4) %>% as.numeric

dataset_days_long$traffic_1000 <- dataset_days_long$traffic/1000
dataset_hours_long$traffic_1000 <- dataset_hours_long$traffic/1000

#dataset_days_long$precipitation <- dataset_days_long$precipitation > 0
#dataset_hours_long$precipitation <- dataset_hours_long$precipitation > 0

dataset_days_long$ln_MA_21_day_accidents <- log(dataset_days_long$MA_21_day_accidents)
dataset_days_long$ln_MA_21_day_traffic <- log(dataset_days_long$MA_21_day_traffic)
dataset_days_long$ln_MA_21_speed_perc_0.5 <- log(dataset_days_long$MA_21_speed_perc_0.5)
dataset_days_long$ln_MA_21_speed_perc_0.75 <- log(dataset_days_long$MA_21_speed_perc_0.75)

dataset_hours_long$night_earlymorning <- (dataset_hours_long$hour %in% c(19:23,0:1)) %>% as.numeric

dataset_hours_long$friday_saturday <- (dataset_hours_long$day_of_week %in% c("Friday","Saturday")) %>% as.numeric
dataset_days_long$friday_saturday <- (dataset_days_long$day_of_week %in% c("Friday","Saturday")) %>% as.numeric

# Models: Accidents, Traffic & Precip ------------------------------------------
# Elasticity of Accidents and Traffic
dataset_days_long_noInf <- dataset_days_long[log(dataset_days_long$MA_21_day_accidents) != -Inf,]
lm_elas_traff_acc <- lm(ln_MA_21_day_accidents ~ ln_MA_21_day_traffic, data=dataset_days_long_noInf[dataset_days_long_noInf$direction != "Total",])

stargazer(lm_elas_traff_acc,
          column.labels="log(Accidents Within 3 Weeks)",
          covariate.labels = "log(Average Traffic Within 3 Weeks)",
          #dep.var.caption="",
          dep.var.labels.include=F,
          omit.stat = c("f","ser"),
          float=F,
          out=file.path(tables_file_path,"lm_elas_traff_acc.tex"))

# Linear Probability Models: Explaining Occurence of Accident
dataset_hours_long$night_earlymorning <- (dataset_hours_long$hour %in% c(19:23,0:1)) %>% as.numeric
dataset_hours_long$friday_saturday <- (dataset_hours_long$day_of_week %in% c("Friday","Saturday")) %>% as.numeric
dataset_hours_long$direction <- as.character(dataset_hours_long$direction) %>% as.factor
dataset_hours_long$year <- substring(dataset_hours_long$day,1,4) %>% as.numeric

accidents_all$hour <- accidents_all$hour - 1 #1-24, but traffic goes from 0-23
accidents_all$hour_str <- as.character(accidents_all$hour)
accidents_all$hour_str[accidents_all$hour %in% 0:9] <- paste0("0",accidents_all$hour_str[accidents_all$hour %in% 0:9])
accidents_all$dayhour <- paste(accidents_all$`Accident Date`, accidents_all$hour_str)
accidents_all$dayhour[grepl("NA|1900", accidents_all$dayhour)] <- NA
accidents_all$year <- substring(accidents_all$`Accident Date`,1,4)

accidents_subset <- accidents_all[!is.na(accidents_all$dayhour),]
accidents_subset <- accidents_subset[!is.na(accidents_subset$accident_cause_simple) & !is.na(accidents_subset$accident_type_simple) & !is.na(accidents_subset$year),]
dataset_hours_long$accident_sub_yn <- dataset_hours_long$dayhour %in% unique(accidents_subset$dayhour)

lm_accidents_days_traffic_speed <- lm(accident_sub_yn ~ traffic_1000 + speed_perc_0.5 + direction + friday_saturday + holidays_1day_buff + precipitation + factor(year), data=dataset_days_long[dataset_days_long$direction != "Total",])
lm_accidents_hours_traffic_speed <- lm(accident_sub_yn ~ traffic_1000 + speed_perc_0.5 + direction +  friday_saturday + holidays_1day_buff + precipitation + factor(year) + night_earlymorning, data=dataset_hours_long[dataset_hours_long$direction != "Total",])

lm_accidents_days_traffic <- lm(accident_sub_yn ~ traffic_1000 + direction + friday_saturday + holidays_1day_buff + precipitation + factor(year), data=dataset_days_long[dataset_days_long$direction != "Total",])
lm_accidents_hours_traffic <- lm(accident_sub_yn ~ traffic_1000 + direction +  friday_saturday + holidays_1day_buff + precipitation + factor(year) + night_earlymorning, data=dataset_hours_long[dataset_hours_long$direction != "Total",])

lm_accidents_days_speed <- lm(accident_sub_yn ~ speed_perc_0.5 + direction + friday_saturday + holidays_1day_buff + precipitation + factor(year), data=dataset_days_long[dataset_days_long$direction != "Total",])
lm_accidents_hours_speed <- lm(accident_sub_yn ~ speed_perc_0.5 + direction +  friday_saturday + holidays_1day_buff + precipitation + factor(year) + night_earlymorning, data=dataset_hours_long[dataset_hours_long$direction != "Total",])

lm_accidents_days_traffic_speed_noyear <- lm(accident_sub_yn ~ traffic_1000 + speed_perc_0.5 + direction + friday_saturday + holidays_1day_buff + precipitation, data=dataset_days_long[dataset_days_long$direction != "Total",])
lm_accidents_hours_traffic_speed_noyear <- lm(accident_sub_yn ~ traffic_1000 + speed_perc_0.5 + direction +  friday_saturday + holidays_1day_buff + precipitation + night_earlymorning, data=dataset_hours_long[dataset_hours_long$direction != "Total",])

lm_accidents_days_traffic_noyear <- lm(accident_sub_yn ~ traffic_1000 + direction + friday_saturday + holidays_1day_buff + precipitation, data=dataset_days_long[dataset_days_long$direction != "Total",])
lm_accidents_hours_traffic_noyear <- lm(accident_sub_yn ~ traffic_1000 + direction +  friday_saturday + holidays_1day_buff + precipitation  + night_earlymorning, data=dataset_hours_long[dataset_hours_long$direction != "Total",])

lm_accidents_days_speed_noyear <- lm(accident_sub_yn ~ speed_perc_0.5 + direction + friday_saturday + holidays_1day_buff + precipitation, data=dataset_days_long[dataset_days_long$direction != "Total",])
lm_accidents_hours_speed_noyear <- lm(accident_sub_yn ~ speed_perc_0.5 + direction +  friday_saturday + holidays_1day_buff + precipitation  + night_earlymorning, data=dataset_hours_long[dataset_hours_long$direction != "Total",])

stargazer(lm_accidents_hours_traffic_speed,
          lm_accidents_hours_traffic,
          lm_accidents_hours_speed,
          lm_accidents_hours_traffic_speed_noyear,
          lm_accidents_hours_traffic_noyear,
          lm_accidents_hours_speed_noyear,
          column.labels=c("","","","","",""),
          covariate.labels = c("Traffic (1000 Vehicles)",
                               "Median Speed (km/hr)",
                               "Driving Towards Addis",
                               "Friday or Saturday",
                               "Holiday (+/- 1 day)",
                               "Precpitation (mm)",
                               "Year: 2016",
                               "Year: 2017",
                               "Early Morning or Night"),
          dep.var.caption="Accident Occuring in an Hour",
          dep.var.labels.include=F,
          omit.stat = c("f","ser"),
          float=F,
          out=file.path(tables_file_path,"lm_accidents.tex"))

##### Models: Accident Locations
addis_adama_points_data$MA1000_N_accidents_all <- addis_adama_points_data$MA1000_N_accidents_AddisDirection_all + addis_adama_points_data$MA1000_N_accidents_AdamaDirection_all

lm_accidents_total <- lm(MA1000_N_accidents_all ~ dist_plaza_nearest_km + MA1000_turnagle_50m, data=addis_adama_points_data)
lm_accidents_toAddis <- lm(MA1000_N_accidents_AddisDirection_all ~ dist_plaza_nearest_km + MA1000_turnagle_50m, data=addis_adama_points_data)
lm_accidents_toAdama <- lm(MA1000_N_accidents_AdamaDirection_all ~ dist_plaza_nearest_km + MA1000_turnagle_50m, data=addis_adama_points_data)

stargazer(lm_accidents_total,
          lm_accidents_toAddis,
          lm_accidents_toAdama,
          column.labels=c("Total","Towards Addis", "Towards Adama"),
          covariate.labels = c("Distance Nearest Toll Plaza (km)",
                               "Average Turn Angle Within 1km"),
          dep.var.caption="Number of Accidents within 1km",
          dep.var.labels.include=F,
          omit.stat = c("f","ser"),
          float=F,
          out=file.path(tables_file_path,"lm_accidents_locations.tex"))




