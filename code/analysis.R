## prelim (loading libraries, read data, etc)
library(tidyverse)
library(vtable)

# will focus modeling injury severity on 3 main files: CRASH, PERSON, VEHICLE
# start with binary modeling, move onto multi-class if want to
# do statistics stuff later since it takes too much time (unfortunately)
crash <- read_csv('data/CRASH_2023.csv')
person <- read_csv('data/PERSON_2023.csv')
vehicle <- read_csv('data/VEHICLE_2023.csv')

# flag only contains crash level data. possible to do further modeling
# but any model improvement or insight gain is spurious and requires much more care
# not worth dealing with for now
# flag <- read_csv('data/FLAG_2023.csv')

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
  "BODY_TYPE", "GRADE", "IMPACT_POINT", "TRAVEL_SPD", "VEH_MOVEMENT", "VEH_ROLE", "VEH_TYPE"
  )

df <- joined %>%
  select(all_of(model_variables)) %>%
  filter(
    PERSON_TYPE == 1, # person is driver
    INJ_SEVERITY %in% 0:4, # injury is known and valid
    COLLISION_TYPE %in% c(1,2,3,4,5,6), # useful collision type
    VEH_ROLE %in% c(1,2), # hitter or hit
    TRAVEL_SPD < 150, # reasonable speed
    # rest are obvious
    !is.na(CRASH_MONTH),
    !is.na(HOUR_OF_DAY),
    !is.na(ROAD_CONDITION),
    !is.na(RDWY_ALIGNMENT),
    !is.na(ILLUMINATION),
    !is.na(TRAVEL_SPD),
    !is.na(GRADE),
    !is.na(WEATHER1),
    !is.na(BODY_TYPE)
    )

names(df) <- tolower(names(df))

# quick vtable after filtering
sumtable(df)

df <- df %>% rename(
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
  hour_of_day = as.numeric(hour_of_day),
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
  illumination = factor(case_when(
    illumination == 1 ~ "Daylight",
    illumination == 2 ~ "Dark Streetlight",
    illumination == 3 ~ "Dark No Streetlight",
    illumination == 4 ~ "Dusk",
    illumination == 5 ~ "Dawn",
    illumination == 6 ~ "Dark Unknown",
    illumination == 8 ~ "Other",
    illumination == 9 ~ "Unknown",
    )),
  intersect_type = factor(case_when(
    intersect_type == "00" ~ "Corridor",
    intersect_type == "01" ~ "4-way",
    intersect_type == "02" ~ "T",
    intersect_type == "03" ~ "Y",
    intersect_type == "05" ~ "Multi-Road Intersection",
    intersect_type %in% c("06","07") ~ "Ramp",
    intersect_type == "08" ~ "Crossover",
    intersect_type == "09" ~ "RRXing",
    intersect_type == "10" ~ "Other",
    intersect_type == "11" ~ "L/Corner",
    intersect_type %in% c("12","13") ~ "Roundabout"
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
  roadway_alignment = factor(case_when(
    roadway_alignment == 1 ~ "Straight",
    roadway_alignment == 3 ~ "Curve Left",
    roadway_alignment == 4 ~ "Curve Right",
    roadway_alignment == 9 ~ "Unknown"
    )),
  road_condition = factor(case_when(
    road_condition == "01" ~ "Dry",
    road_condition == "02" ~ "Ice/Frost",
    road_condition == "03" ~ "Mud, Dirt, Gravel",
    road_condition == "04" ~ "Oil",
    road_condition == "05" ~ "Sand",
    road_condition == "06" ~ "Slush",
    road_condition == "07" ~ "Snow",
    road_condition == "08" ~ "Water (Standing or Moving)",
    road_condition == "09" ~ "Wet",
    road_condition == "22" ~ "Mud, Sand, Dirt, Oil (Expired)",
    road_condition == "98" ~ "Other",
    road_condition == "99" ~ "Unknown"
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
  veh_role = factor(case_when(
    veh_role == 1 ~ "Striking",
    veh_role == 2 ~ "Struck"
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
  select(-vulnerable_road_user, -person_type, -impact_point, veh_type)
  # justification of dropped variables
  # vulnerable_road_user: all 0
  # person_type: all 1 by design
  # impact_point: made redundant by collision_type
  # veh_type: made redundant by body_type

# another table:
sumtable(df)

# add binary variable for binary injury prediction
df$inj_severity_bin <- factor(if_else(df$inj_severity %in% c("Killed","Serious"), 1 , 0))
sumtable(df)


# make two matrices for modeling

train_ind <- sample(1:nrow(df), floor(0.85 * nrow(df)))

# binary
df_bin <- df %>% select(-inj_severity)
y_bin <- df_bin$inj_severity_bin
x_bin <- model.matrix(~ . - inj_severity_bin, data = df_bin)[, -1]

# use SMOTE here to alleviate class imbalance

# multi
df_multi <- df %>% select(-inj_severity_bin)
y_multi <- df_multi$inj_severity
x_multi <- model.matrix(~ . - inj_severity, data = df_multi)[, -1]

x_multi_train <- x_multi[train_ind, ]
y_multi_train <- y_multi[train_ind]
x_multi_test <- x_multi[-train_ind, ]
y_multi_test <- y_multi[-train_ind]


# model time