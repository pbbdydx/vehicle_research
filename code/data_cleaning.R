# load libraries

library(tidyverse)


# will focus modeling injury severity on 3 main files: CRASH, PERSON, VEHICLE
# start with binary modeling, move onto multi-class if want to
# do statistics stuff later since it takes too much time (unfortunately)
crash <- read_csv('data/2024/CRASH_2024.csv')
person <- read_csv('data/2024/PERSON_2024.csv')
vehicle <- read_csv('data/2024/VEHICLE_2024.csv')

# flag only contains crash level data. possible to do further modeling
# but any model improvement or insight gain is spurious and requires much more care
# not worth dealing with for now
# flag <- read_csv('data/2024/FLAG_2024.csv')

# ---------- ANALYSIS WORK BEGINS HERE --------------
joined <- crash %>%
  inner_join(person, by = 'CRN') %>%
  inner_join(vehicle, by = c('CRN', 'UNIT_NUM')) # allows one row to be one driver in one vehicle by adding UNIT_NUM

# select important columns with data dictionary. dont need CRN/UNIT_NUM

model_variables <- c(
  # CRASH.csv
  "COLLISION_TYPE", "HOUR_OF_DAY", "CRASH_MONTH", "DAY_OF_WEEK", "DISTRICT", "ILLUMINATION", "INTERSECT_TYPE",
  "RELATION_TO_ROAD", "RDWY_ALIGNMENT", "ROAD_CONDITION", "TCD_TYPE", "URBAN_RURAL", "WEATHER1", "WORK_ZONE_IND",
  # PERSON.csv
  "AGE", "INJ_SEVERITY", "RESTRAINT_HELMET", "SEX", "VULNERABLE_ROAD_USER", "PERSON_TYPE", "VULNERABLE_ROAD_USER",
  # VEHICLE.csv
  "BODY_TYPE", "GRADE", "IMPACT_POINT", "TRAVEL_SPD", "VEH_MOVEMENT", "VEH_ROLE_CD", "VEH_TYPE"
)




df <- joined %>% select(all_of(model_variables))
print(paste("Initial rows:", nrow(df)))


# check missingness by column
na_counts <- colSums(is.na(df))
na_counts

# ignoring columns that have high NA counts
# hour of day : 235356 missing (96.3%)
# illumination : 244173 missing (100%)


df <- df %>% filter(PERSON_TYPE == 1)
print(paste("After PERSON_TYPE filter:", nrow(df)))

df <- df %>% filter(INJ_SEVERITY %in% 0:4)
print(paste("After INJ_SEVERITY filter:", nrow(df)))

df <- df %>% filter(COLLISION_TYPE %in% 1:6)
print(paste("After COLLISION_TYPE filter:", nrow(df)))

df <- df %>% filter(TRAVEL_SPD < 200)
print(paste("After TRAVEL_SPD filter:", nrow(df)))

df <- df %>% filter(!is.na(CRASH_MONTH))
print(paste("After CRASH_MONTH filter:", nrow(df)))

df <- df %>% filter(!is.na(ROAD_CONDITION))
print(paste("After ROAD_CONDITION filter:", nrow(df)))

df <- df %>% filter(!is.na(RDWY_ALIGNMENT))
print(paste("After RDWY_ALIGNMENT filter:", nrow(df)))

df <- df %>% filter(!is.na(TRAVEL_SPD))
print(paste("After TRAVEL_SPD NA filter:", nrow(df)))

df <- df %>% filter(!is.na(GRADE))
print(paste("After GRADE filter:", nrow(df)))

df <- df %>% filter(!is.na(WEATHER1))
print(paste("After WEATHER1 filter:", nrow(df)))

df <- df %>% filter(!is.na(BODY_TYPE))
print(paste("After BODY_TYPE filter:", nrow(df)))

names(df) <- tolower(names(df))

# print dim  and check missingness by column again
dim(df)
colSums(is.na(df))
# removing columns that have high NA counts after the filter
# veh_role_cd : 73.2% missing
# illumination : 100.0% missing
# hour_of_day : 96.3% missing

df <- df %>% select(-veh_role_cd, -illumination, -hour_of_day)


clean_df <- df %>% rename(
  roadway_alignment = rdwy_alignment,
  traffic_control_device = tcd_type
) %>% mutate(
  collision_type = factor(case_when(
    collision_type == 1 ~ "Rear End",
    collision_type == 2 ~ "Head On",
    collision_type == 3 ~ "Backing",
    collision_type == 4 ~ "Angle",
    collision_type == 5 ~ "Sideswipe (Same Direction)",
    collision_type == 6 ~ "Sideswipe (Opposite Direction)"
  )),
  crash_month = factor(crash_month),
  day_of_week = factor(case_when(
    day_of_week == 1 ~ "Sunday",
    day_of_week == 2 ~ "Monday",
    day_of_week == 3 ~ "Tuesday",
    day_of_week == 4 ~ "Wednesday",
    day_of_week == 5 ~ "Thursday",
    day_of_week == 6 ~ "Friday",
    day_of_week == 7 ~ "Saturday"
  )),
  district = factor(district),
  intersect_type = factor(case_when(
    intersect_type == 0 ~ "Corridor",
    intersect_type == 1 ~ "4-way",
    intersect_type == 2 ~ "T",
    intersect_type == 3 ~ "Y",
    intersect_type == 5 ~ "Multi-Road Intersection",
    intersect_type %in% c(6,7) ~ "Ramp",
    intersect_type == 8 ~ "Crossover",
    intersect_type == 9 ~ "RRXing",
    intersect_type == 0 ~ "Other",
    intersect_type == 1 ~ "L/Corner",
    intersect_type %in% c(12,13) ~ "Roundabout"
  )),
  relation_to_road = factor(case_when(
    relation_to_road == 1 ~ "On Roadway",
    relation_to_road == 2 ~ "Shoulder",
    relation_to_road == 3 ~ "Median",
    relation_to_road == 4 ~ "Roadside",
    relation_to_road == 5 ~ "Outside Trafficway",
    relation_to_road == 6 ~ "In Parking Lane",
    relation_to_road == 7 ~ "Gore Zone",
    relation_to_road == 9 ~ "Unknown"
  )),
  restraint_helmet = factor(case_when(
    restraint_helmet == 0 ~ "None",
    restraint_helmet == 1 ~ "Shoulder Belt",
    restraint_helmet == 2 ~ "Lap Belt",
    restraint_helmet == 3 ~ "Lap and Shoulder Belt",
    restraint_helmet == 5 ~ "Motorcycle Helmet",
    restraint_helmet == 6 ~ "Nonmotorist Wearing Helmet", # filter out?
    restraint_helmet == 10 ~ "Improper Use",
    restraint_helmet == 12 ~ "Improper Use (Helmet)",
    restraint_helmet %in% c(98, 99, 14, NA) ~ "Other"
  )),
  roadway_alignment = factor(case_when(
    roadway_alignment == 1 ~ "Straight",
    roadway_alignment == 3 ~ "Curve Left",
    roadway_alignment == 4 ~ "Curve Right",
    roadway_alignment == 9 ~ "Unknown"
  )),
  road_condition = factor(case_when(
    road_condition == 01 ~ "Dry",
    road_condition == 02 ~ "Ice/Frost",
    road_condition == 03 ~ "Mud, Dirt, Gravel",
    road_condition == 04 ~ "Oil",
    road_condition == 05 ~ "Sand",
    road_condition == 06 ~ "Slush",
    road_condition == 07 ~ "Snow",
    road_condition == 08 ~ "Water (Standing or Moving)",
    road_condition == 09 ~ "Wet",
    road_condition == 98 ~ "Other",
    road_condition == 99 ~ "Unknown"
  )),
  traffic_control_device = factor(case_when(
    traffic_control_device == 0 ~ "None",
    traffic_control_device == 1 ~ "Flashing Traffic Signal",
    traffic_control_device == 2 ~ "Traffic Signal",
    traffic_control_device == 3 ~ "Stop Sign",
    traffic_control_device == 4 ~ "Yield Sign",
    traffic_control_device == 5 ~ "Active RRXing",
    traffic_control_device == 6 ~ "Passive RRXing",
    traffic_control_device == 7 ~ "Police/Flagman",
    traffic_control_device == 8 ~ "Other",
    traffic_control_device == 9 ~ "Unknown"
  )),
  urban_rural = factor(case_when(
    urban_rural == 1 ~ "Rural",
    urban_rural == 2 ~ "Urban"
  )),
  veh_movement = factor(case_when(
    veh_movement == "01" ~ "Going straight",
    veh_movement == "02" ~ "Slowing or stopping in lane",
    veh_movement == "03" ~ "Stopped in traffic lane",
    veh_movement == "04" ~ "Passing or overtaking vehicle",
    veh_movement == "05" ~ "Leaving a parked position",
    veh_movement == "06" ~ "Parked",
    veh_movement == "08" ~ "Trying to avoid animal/ped/obj/vehicle",
    veh_movement == "09" ~ "Turning right on red",
    veh_movement == "10" ~ "Turning right",
    veh_movement == "11" ~ "Turning left on red",
    veh_movement == "12" ~ "Turning left",
    veh_movement == "13" ~ "Making a U-turn",
    veh_movement == "14" ~ "Backing up",
    veh_movement == "15" ~ "Changing lanes or merging",
    veh_movement == "16" ~ "Negotiating curve - right",
    veh_movement == "17" ~ "Negotiating curve - left",
    veh_movement == "18" ~ "Entering traffic lane",
    veh_movement == "19" ~ "Leaving traffic lane",
    veh_movement %in% c("98","99", NA) ~ "Unknown",
  )),
  weather1 = factor(case_when(
    weather1 %in% c("01", "02", "08") ~ "Wind",
    weather1 == "03" ~ "Clear",
    weather1 == "04" ~ "Cloudy",
    weather1 == "05" ~ "Fog/Smog/Smoke",
    weather1 == "06" ~ "Freezing Rain",
    weather1 == "07" ~ "Rain",
    weather1 == "09" ~ "Sleet/Hail",
    weather1 == "10" ~ "Snow",
    weather1 == "98" ~ "Other",
    weather1 == "99" ~ "Unknown"
  )),
  work_zone_ind = factor(work_zone_ind),
  age = as.numeric(age),
  inj_severity = factor(case_when(
    inj_severity == 0 ~ "None",
    inj_severity == 1 ~ "Killed",
    inj_severity == 2 ~ "Serious",
    inj_severity == 3 ~ "Minor",
    inj_severity == 4 ~ "Possible Injury"
  )),
  sex = factor(sex),
  grade = factor(case_when(
    grade == 1 ~ "Level",
    grade == 2 ~ "Uphill",
    grade == 3 ~ "Downhill",
    grade == 4 ~ "Bottom of Hill",
    grade == 5 ~ "Crest of Hill",
    grade == 9 ~ "Unknown"
  )),
  body_type = factor(case_when(
    # this is a data driven simplification of all the body type values encountered by proportions.
    # see: df %>% group_by(body_type) %>% summarize(count = n(), prop = count/nrow(df)) %>% arrange(desc(prop)) %>% print(n=40)
    body_type == "04" ~ "Car",
    body_type == "15" ~ "SUV",
    body_type == "51" ~ "Pickup Truck",
    body_type == "69" ~ "Light/Medium Truck",
    body_type == "09" ~ "Unknown",
    body_type %in% c("02", "03", "05", "06") ~ "Other Car",
    body_type %in% c("12") ~ "Small SUV",
    body_type %in% c("40", "41", "42", "49") ~ "Van",
    body_type %in% c("20", "23", "24", "25", "28", "29") ~ "Motorcycle",
    body_type %in% c("72", "73", "75", "79") ~ "Heavy Truck",
    body_type %in% c("08", "19", "39", "50", "78", "98") ~ "Other/Low-Freq",
    TRUE ~ "Other/Low-Freq"
  )),
  travel_spd = as.numeric(travel_spd)
) %>%
  select(-vulnerable_road_user, -person_type, -impact_point, -veh_type) %>%
  na.omit() # omit missing

# justification of dropped variables
# vulnerable_road_user: all 0
# person_type: all 1 by design
# impact_point: made redundant by collision_type
# veh_type: made redundant by body_type



# save data
saveRDS(clean_df, 'data/2024/2024_clean.rds')
