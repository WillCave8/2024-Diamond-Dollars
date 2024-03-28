library(tidyverse)

data <- read_csv("SCeraAll.csv")


on_base_data <- data %>%
  filter(game_year %in% c(2021,2022,2023)) %>%
  group_by(game_pk, at_bat_number) %>%
  mutate(on_base = ifelse(any(events %in% c("walk", "hit_by_pitch", "single", "double", "triple", "home_run")), 1, 0)) %>%
  ungroup() %>%
  arrange(game_date, game_pk, at_bat_number, pitch_number) %>%
  select(c(game_pk, game_year, pitch_type, batter, pitcher, zone, at_bat_number, pitch_number, description, stand, p_throws,
           balls, strikes, plate_x, plate_z, on_3b, on_2b, on_1b, sz_top, sz_bot, on_base)) %>%
  filter(!is.na(plate_x) & !is.na(plate_z) & !is.na(sz_top) & !is.na(sz_bot)) %>%
  mutate(stf = case_when(
    pitch_type %in% c("FF", "FA") ~ as.character(stf_cmd_dat$Stf_plus_FA[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "SI" ~ as.character(stf_cmd_dat$Stf_plus_SI[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "KC" ~ as.character(stf_cmd_dat$Stf_plus_KC[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "CH" ~ as.character(stf_cmd_dat$Stf_plus_CH[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type %in% c("ST", "SL") ~ as.character(stf_cmd_dat$Stf_plus_SL[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type %in% c("CU", "SV") ~ as.character(stf_cmd_dat$Stf_plus_CU[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "FO" ~ as.character(stf_cmd_dat$Stf_plus_FO[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "FC" ~ as.character(stf_cmd_dat$Stf_plus_FC[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    TRUE ~ as.character(stf_cmd_dat$Stuff_plus[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))])
  )) %>%
  mutate(loc = case_when(
    pitch_type %in% c("FF", "FA") ~ as.character(stf_cmd_dat$Loc_plus_FA[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "SI" ~ as.character(stf_cmd_dat$Loc_plus_SI[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "KC" ~ as.character(stf_cmd_dat$Loc_plus_KC[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "CH" ~ as.character(stf_cmd_dat$Loc_plus_CH[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type %in% c("ST", "SL") ~ as.character(stf_cmd_dat$Loc_plus_SL[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type %in% c("CU", "SV") ~ as.character(stf_cmd_dat$Loc_plus_CU[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "FO" ~ as.character(stf_cmd_dat$Loc_plus_FO[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    pitch_type == "FC" ~ as.character(stf_cmd_dat$Loc_plus_FC[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))]),
    TRUE ~ as.character(stf_cmd_dat$Location_plus[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))])
  )) %>%
  mutate(avg_p_stf = as.character(stf_cmd_dat$Stuff_plus[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))])) %>%
  mutate(avg_p_loc = as.character(stf_cmd_dat$Location_plus[match(paste(game_year, pitcher), paste(stf_cmd_dat$Season, stf_cmd_dat$MLBAMID))])) %>%
  mutate(vertical_miss = abs(ifelse(sz_top < plate_z, plate_z - sz_top,
                                    ifelse(sz_bot > plate_z, plate_z - sz_bot, 0))),
         horizontal_miss = abs(ifelse(plate_x < -0.85, plate_x - (-0.85),
                                      ifelse(plate_x > 0.85, plate_x - 0.85, 0))))


test <- read_csv("fangraphs_season_level.csv")

stf_cmd_dat <- test %>%
  select(c(MLBAMID, Season, Stf_plus_CH, Stf_plus_CU, Stf_plus_FA, 
           Stf_plus_SI, Stf_plus_SL, Stf_plus_KC,
           Stf_plus_FC, Stf_plus_FS, Stf_plus_FO, 
           Loc_plus_CH, Loc_plus_CU, Loc_plus_FA, 
           Loc_plus_SI, Loc_plus_SL, Loc_plus_KC,
           Loc_plus_FC, Loc_plus_FS, Loc_plus_FO,
           Stuff_plus, Location_plus))

library(dplyr)

library(dplyr)

columns_to_shift <- c('pitch_type', 'zone', 'description', 'plate_x', 'plate_z', 'sz_top', 'sz_bot', 'stf', 'loc', 'vertical_miss', 'horizontal_miss')

shift_within_group <- function(df, cols) {
  for (col in cols) {
    df[[paste0('prev_', col)]] <- lag(df[[col]])
  }
  return(df)
}

pbp_data <- on_base_data %>%
  group_by(game_pk, at_bat_number) %>%
  do(shift_within_group(., columns_to_shift)) %>%
  ungroup()

zone_ba <- read_csv("ba_by_zone_final.csv")

get_top_zones <- function(data) {
  data %>%
    pivot_longer(cols = -batter, names_to = "zone", values_to = "ba") %>%
    mutate(zone = as.integer(gsub("ba_zone", "", zone))) %>%
    group_by(batter) %>%
    arrange(batter, desc(ba)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 3) %>%
    pivot_wider(names_from = rank, values_from = zone, names_prefix = "best_zone_") %>%
    ungroup() %>%
    rename(best_zone = best_zone_1, second_best_zone = best_zone_2, third_best_zone = best_zone_3) %>%
    select(batter, best_zone, second_best_zone, third_best_zone)
}

# Get top 3 zones for each batter
top_zones <- get_top_zones(zone_ba)


library(dplyr)
library(tidyr)

get_top_zones <- function(data) {
  data %>%
    pivot_longer(cols = -batter, names_to = "zone", values_to = "ba") %>%
    print() %>%
    mutate(zone = as.integer(gsub("ba_zone", "", zone))) %>%
    group_by(batter) %>%
    print() %>%
    mutate(rank = row_number(desc(ba))) %>%
    filter(rank <= 3) %>%
    pivot_wider(names_from = rank, values_from = zone, names_prefix = "best_zone_") %>%
    ungroup() %>%
    rename(best_zone = best_zone_1, second_best_zone = best_zone_2, third_best_zone = best_zone_3) %>%
    select(batter, best_zone, second_best_zone, third_best_zone)
}

# Get top 3 zones for each batter
top_zones <- get_top_zones(zone_ba)

best_zone <- top_zones %>%
  select(batter, best_zone) %>%
  na.omit()

sec_best_zone <- top_zones %>%
  select(batter, second_best_zone) %>%
  na.omit()

th_best_zone <- top_zones %>%
  select(batter, third_best_zone) %>%
  na.omit()


joined_df <- left_join(best_zone, sec_best_zone, by = "batter") %>%
  left_join(th_best_zone, by = "batter")

pbp_data_new <- merge(pbp_data, joined_df) 

pbp_data_final <- pbp_data_new %>%
  mutate(
    prev_match_top_zone = if_else(
      prev_zone == best_zone | prev_zone == second_best_zone | prev_zone == third_best_zone,
      1, 0
    )
  ) %>%
  mutate(
    match_top_zone = if_else(
      zone == best_zone | zone == second_best_zone | zone == third_best_zone,
      1, 0
    )
  ) %>%
  select(-c(best_zone, second_best_zone, third_best_zone))


write.csv(pbp_data_final, "Final_PBP_Data.csv")

model_data <- read_csv("Final_PBP_Data.csv")

fg21 <- read_csv("fg_hitting_leaders_2021.csv")
fg22 <- read_csv("fg_hitting_leaders_2022.csv")
fg23 <- read_csv("fg_hitting_leaders_2023.csv")

fg21_obp <- fg21 %>%
  select(c(xMLBAMID, OBP, Season))

fg22_obp <- fg22 %>%
  select(c(xMLBAMID, OBP, Season))

fg23_obp <- fg23 %>%
  select(c(xMLBAMID, OBP, Season))

rbind(fg21_obp, fg22_obp, fg23_obp) -> fg_bat

write.csv(fg_bat, "fg_bat.csv")

model_data_with_obp <- pbp_data_final %>%
  left_join(fg_bat %>% 
              select(Season, xMLBAMID, OBP) %>%  # Assuming the column containing OBP is named 'OBP' in fg_bat
              distinct(),                      # Remove duplicate rows
            by = c("game_year" = "Season", "batter" = "xMLBAMID")) %>%
  select(-c(on_3b, on_2b, on_1b))

model_data_with_obp$stf %>% is.na() %>% sum()

# Calculate column means for columns 'stf' and 'loc'
mean_stf <- mean(model_data_with_obp$stf, na.rm = TRUE)
mean_loc <- mean(model_data_with_obp$loc, na.rm = TRUE)
mean_avg_stf <- mean(model_data_with_obp$avg_p_stf, na.rm = TRUE)
mean_avg_loc <- mean(model_data_with_obp$avg_p_loc, na.rm = TRUE)

# Impute missing values with the calculated means
model_data_with_obp$stf[is.na(model_data_with_obp$stf)] <- mean_stf
model_data_with_obp$loc[is.na(model_data_with_obp$loc)] <- mean_loc
model_data_with_obp$avg_p_stf[is.na(model_data_with_obp$avg_p_stf)] <- mean_avg_stf
model_data_with_obp$avg_p_loc[is.na(model_data_with_obp$avg_p_loc)] <- mean_avg_loc

write.csv(model_data_with_obp, "PBP_W_OBP.csv")


         