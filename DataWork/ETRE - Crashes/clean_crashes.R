# Clean Crash Data

# OUTLINE
# 1. Load / Append Data
# 2. Cleanup Variable Names
# 3. Clean Date/Time
# 4. Clean Location
# 5. Variable Clean
# 6. Create simplified variables
# 7. Export

# 1. Load / Append Data --------------------------------------------------------
crashes_df <- file.path(etre_crashes_dir, "RawData") %>%
  list.files(full.names = T,
             pattern = ".xlsx") %>%
  map_df(function(path){
    
    df <- path %>%
      read_xlsx %>%
      mutate(`Case No` = `Case No` %>% as.character,
             `Driver Age` = `Driver Age` %>% as.character)
    
    df$year <- path %>% str_replace_all(".*/", "") %>% substring(1,4)
    
    return(df)
  })

# 2. Cleanup Variable Names ----------------------------------------------------
## Variable names - lowercase, no space
names(crashes_df) <- names(crashes_df) %>%
  tolower() %>%
  str_replace_all(" ", "_") %>%
  str_replace_all("\\(v\\)", "") %>%
  str_replace_all("_$", "")

## Deal with nearly duplicate variable names
# vehicle_brand
table(!is.na(crashes_df$vehicle_brand) & !is.na(crashes_df$vehicle__brand)) # Never both not NA at same time

crashes_df$vehicle_brand[!is.na(crashes_df$vehicle__brand)] <- 
  crashes_df$vehicle__brand[!is.na(crashes_df$vehicle__brand)]

crashes_df$vehicle__brand <- NULL

# drivers_license_level
table(!is.na(crashes_df$drivers_license_level) & !is.na(crashes_df$drivers_license__level)) # Never both not NA at same time

crashes_df$drivers_license_level[!is.na(crashes_df$drivers_license__level)] <- 
  crashes_df$drivers_license__level[!is.na(crashes_df$drivers_license__level)]

crashes_df$drivers_license__level <- NULL

# license_region
table(!is.na(crashes_df$license_region) & !is.na(crashes_df$license__region)) # Never both not NA at same time

crashes_df$license_region[!is.na(crashes_df$license__region)] <- 
  crashes_df$license__region[!is.na(crashes_df$license__region)]

crashes_df$license__region <- NULL

## Remove "__" from variable names; need to do now after deal with nearly
# duplicate names
names(crashes_df) <- names(crashes_df) %>% str_replace_all("__","_")

## Individual changes
crashes_df <- crashes_df %>%
  dplyr::rename(slight_injury = light_injury)

# 3. Clean Date/Time -----------------------------------------------------------
crashes_df <- crashes_df %>%
  
  ## Separate crash time into hour/minute
  mutate(time_of_accident_num = time_of_accident %>%
           str_replace_all("[a-z A-Z]", "")) %>%
  separate(col = time_of_accident_num,
           into = c("time_of_accident_hour", "time_of_accident_minute"),
           remove = T) %>%
  mutate(time_of_accident_minute = case_when((is.na(time_of_accident_minute) & 
                                                !is.na(time_of_accident_hour)) ~ "00",
                                             TRUE ~ time_of_accident_minute)) %>%
  
  ## Cleanup
  mutate(time_of_accident        = time_of_accident        %>% tolower %>% str_squish(),
         time_of_accident_hour   = time_of_accident_hour   %>% as.numeric,
         time_of_accident_minute = time_of_accident_minute %>% as.numeric)

## Determine AM or PM
# TODO: If "night" but hour is "1", should this be "am"?
crashes_df$ampm <- NA
crashes_df$ampm[grepl("am$", crashes_df$time_of_accident)] <- "am"
crashes_df$ampm[grepl("pm$", crashes_df$time_of_accident)] <- "pm"
crashes_df$ampm[grepl("morning", crashes_df$time_of_accident)] <- "am"
crashes_df$ampm[grepl("nitght|night|evening", crashes_df$time_of_accident)] <- "pm"
crashes_df$ampm[grepl("day", crashes_df$time_of_accident) & crashes_df$time_of_accident_hour %in% 7:11]       <- "am"
crashes_df$ampm[grepl("day", crashes_df$time_of_accident) & crashes_df$time_of_accident_hour %in% c(12, 1:6)] <- "pm"
crashes_df$ampm[is.na(crashes_df$ampm) & !is.na(crashes_df$time_of_accident)] <- "am"

## If PM, add 12 to hour
crashes_df$time_of_accident_hour[crashes_df$ampm %in% "pm"] <- 
  crashes_df$time_of_accident_hour[crashes_df$ampm %in% "pm"] + 12

crashes_df$time_of_accident_hour[crashes_df$time_of_accident_hour %in% 24] <- 0

## Make date/time variable
crashes_df$accident_datetime <- paste0(crashes_df$accident_date, " ",
                                       crashes_df$time_of_accident_hour, ":",
                                       crashes_df$time_of_accident_minute, " ") %>%
  str_squish() %>%
  ymd_hm()

crashes_df$time_of_accident <- NULL
crashes_df$ampm <- NULL

# 4. Clean Location ------------------------------------------------------------
#### ** 4.1 Create file of points every 10 meters along road #####
addis_adama_express <- readRDS(file.path(addisadama_express_dir, "Data", "addis_adama_express.Rds"))

equal_distant_projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")
addis_adama_express <- spTransform(addis_adama_express, CRS(equal_distant_projection)) 

num_points <- gLength(addis_adama_express) / 10
addis_adama_points <- spsample(addis_adama_express, n = num_points, type = "regular")
addis_adama_points <- spTransform(addis_adama_points, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
addis_adama_points <- as.data.frame(addis_adama_points)
names(addis_adama_points) <- c("longitude","latitude")

addis_adama_points$distance_adama_direction <- (1:nrow(addis_adama_points))*10

#### ** 4.2 Calculate Distance from Addis #####
loc_to_distance <- function(loc){
  loc <- strsplit(loc, "\\+")[[1]]
  loc <- as.numeric(loc)
  if(is.na(loc[2])){
    loc[2] <- 0
  }
  loc.dist <- loc[1]*1000 + loc[2]
  return(loc.dist)
}

crashes_df$accident_loc_dist <- lapply(crashes_df$accident_location, loc_to_distance) %>% unlist

# If not in Adama or Addis direction, replace with NA
# TODO: (1) Figure out locations of others
# TODO: (1) Should location be flipped if going towards Addis?
crashes_df$direction[!grepl("adama|addis abeba|addis", crashes_df$direction)] <- NA

crashes_df <- merge(crashes_df,
                    addis_adama_points,
                    by.x = "accident_loc_dist",
                    by.y = "distance_adama_direction",
                    all.x = T,
                    all.y = F)

crashes_df$accident_loc_dist <- NULL

# 5. Variable Clean ------------------------------------------------------------
# Cleanup individual variables

crashes_df <- crashes_df %>%
  dplyr::mutate_if(is.character, . %>% tolower %>% str_squish) %>%
  dplyr::mutate(day = case_when(day %in% "firday" ~ "friday",
                                day %>% str_detect("mon") ~ "monday",
                                day %>% str_detect("tu") ~ "tuesday",
                                day %>% str_detect("we") ~ "wednesday",
                                day %>% str_detect("thu") ~ "thursday",
                                day %>% str_detect("fri") ~ "friday",
                                day %>% str_detect("sat") ~ "saturday",
                                day %>% str_detect("sun") ~ "sunday"),
                
                accident_date = accident_date %>% as.Date(tz = "Africa/Nairobi"),
                
                slight_injury = slight_injury %>% as.numeric,
                serious_injury = serious_injury %>% as.numeric,
                fatality = fatality %>% as.numeric,
                
                vehicle_type = vehicle_type %>% 
                  labelled(c("Small Autos: Cars, Jeep, L/Rover, Taxi, Pick Up" = 1,
                             "Minibus" = 2,
                             "Medium Bus, ISUZU" = 3,
                             "Big Size Bus, Medium Truck" = 4,
                             "Heavy Truck Trailers - 4 axles" = 5,
                             "Heavy Truck Trailers - 5 axles" = 6,
                             "Heavy Truck Trailers - >5 axles" = 7)),
                axle_number = case_when(vehicle_type %in% 1:3 ~ "2",
                                        vehicle_type %in% 4 ~ "3",
                                        vehicle_type %in% 5 ~ "4",
                                        vehicle_type %in% 6 ~ "5",
                                        vehicle_type %in% 7 ~ ">5"))

# 6. Simplify Variables --------------------------------------------------------
# Sometimes character variables list multiple things; this simplifies the categories

##### ** 6.1 Cause of Accident #####
crashes_df <- crashes_df %>%
  mutate(cause_of_accident_simple = 
           case_when(cause_of_accident %in% c("other",
                                              "front & back",
                                              "water problem",
                                              "pedestrian",
                                              "rainy",
                                              "wire problem",
                                              "over load") ~ "other",
                     cause_of_accident %in% c("animal crossing",
                                              "animals crossing") ~ "animal crossing",
                     cause_of_accident %in% c("brake problem",
                                              "brake malfunction",
                                              "brake problem (dx)",
                                              "braker problem",
                                              "breaker problem",
                                              "brecker problem",
                                              "beraker problem",
                                              "ud brake problem",
                                              "loose brake") ~ "brake problem",
                     cause_of_accident %in% c("crush", 
                                              "crush by vehicle",
                                              "crush vehicle to vehicle",
                                              "crushed with other vehicle",
                                              "vehicles crush",
                                              "pick up crushe",
                                              "back crush") ~ "vehicle crash",
                     cause_of_accident %in% c("fatigue",
                                              "sleep",
                                              "sleeping",
                                              "sleeping problem",
                                              "sleeping problem (isuzu)") ~ "fatigue",
                     cause_of_accident %in% c("fiat tire",
                                              "flat tire",
                                              "tire flat",
                                              "tire problem",
                                              "tire problem (abdulla)",
                                              "tyre problem",
                                              "tyre prblem",
                                              "tyre prblem (lifane)") ~ "flat tire/tire problem",
                     cause_of_accident %>% str_detect("flat tire") ~ "flat tire/tire problem",
                     cause_of_accident %in% c("speed",
                                              "over speed",
                                              "over speeding",
                                              "overspeeding",
                                              "overspeed",
                                              "over speed pickup") ~ "speeding",
                     cause_of_accident %>% str_detect("overspeeding") ~ "speeding",
                     cause_of_accident %in% c("steering wheel malfunction",
                                              "steering wheel problem",
                                              "streeing wheel problem",
                                              "steering wheeel",
                                              "steering wheel maltunction") ~ "steering wheel malfunction",
                     cause_of_accident %in% c("techinical malfunction",
                                              "technical malfunction",
                                              "technical prblem",
                                              "tehical problem",
                                              "technical problem",
                                              "technical problem",
                                              "transmition problem",
                                              "thchnical problem") ~ "technical malfunction",
                     cause_of_accident %in% c("unethical driving",
                                              "drunk /alcohol") ~ "unethical driving"),
         cause_of_accident_simple = cause_of_accident_simple %>% replace_na("unspecified"))

#crashes_df$cause_of_accident[crashes_df$cause_of_accident_simple %in% "unspecified"] %>% unique()

##### ** 6.2 Type of Accident #####
crashes_df <- crashes_df %>%
  mutate(type_of_accident_simple = 
           case_when(type_of_accident %in% c("car overthrow",
                                             "overthrow",
                                             "car overtherow",
                                             "car overt herow") ~ "overthrow",
                     type_of_accident %in% c("crush with guardrail",
                                             "car crushed with guardrail",
                                             "crush with guard rail",
                                             "crush with guardrail",
                                             "cross median guardrail",
                                             "crush with guardrail፡",
                                             "crush with guardrail",
                                             "crushed with guardrail",
                                             "crush guardrail") ~ "crash with gaurdrail",
                     type_of_accident %in% c("crush vehicle tovehicle",
                                             "cehicles collision",
                                             "crush each other",
                                             "crush vehicle to vehicle",
                                             "crush vehiclewith vehicle",
                                             "crush with another car",
                                             "vehicles crush",
                                             "vehicles collision",
                                             "vehicles  crush",
                                             "Head on Crush",
                                             "front rear crush",
                                             "crush wih sino truck",
                                             "cursh with sino truck",
                                             "honda cursh with isuzu and out of road",
                                             "truck car crush him") ~ "vehicle to vehicle crash",
                     type_of_accident %in% c("crushed with poperty",
                                             "crush with island") ~ "crushed with poperty",
                     type_of_accident %in% c("crush") ~ "crash",
                     type_of_accident %in% c("front back crushed",
                                             "front rearcrush") ~ "front back crushed",
                     type_of_accident %in% c("crush with concrete",
                                             "crush with crain",
                                             "crush with galvanized",
                                             "crush with parked vehicle",
                                             "crushing",
                                             "dolfine crush pickup",
                                             "isuzu crush others",
                                             "the vitz crush the isuzu",
                                             "sino crush isuzu",
                                             "sidecrush",
                                             "side crush",
                                             "head on crush",
                                             "head on crush?s?",
                                             " head on crush?s?",
                                             "crush with ditch",
                                             "crush with side dicth",
                                             "out of road",
                                             "isuzu crushother",
                                             "ford crush pickup",
                                             "near to near crushed",
                                             "isuzu crushothers",
                                             "head on crushከ",
                                             "ivco crush fuel truck",
                                             "back crush") ~ "crash, other",
                     type_of_accident %in% c("front & back",
                                             "direction",
                                             "direction change",
                                             "flat tire",
                                             "other") ~ "other"),
         type_of_accident_simple = type_of_accident_simple %>% replace_na("unspecified"))

#crashes_df$type_of_accident[crashes_df$type_of_accident_simple %in% "unspecified"] %>% unique()

##### ** 6.3 Accident Cuase - Human/Vehicle #####
crashes_df <- crashes_df %>%
  mutate(accident_cause_vehicle_human = 
           case_when(cause_of_accident_simple %in% c("brake problem",
                                                     "vehicle crash",
                                                     "flat tire/tire problem",
                                                     "steering wheel malfunction",
                                                     "technical malfunction") ~ "vehicle error",
                     cause_of_accident_simple %in% c("fatigue",
                                                     "speeding",
                                                     "unethical driving") ~ "human error",
                     cause_of_accident_simple %in% c("animal crossing") ~ "other"))

# 7. Export --------------------------------------------------------------------
saveRDS(crashes_df,   file.path(etre_crashes_dir, "FinalData", "crashes.Rds"))
write_dta(crashes_df, file.path(etre_crashes_dir, "FinalData", "crashes.dta"))





