
library(tidyverse)
fouls_model_data <- read_csv("fouls_model_data.csv")
fouls_model_data_2 <- fouls_model_data %>%
  mutate(miss_by = ifelse(miss_by == 0, 100, -100 * (abs(miss_by) / max(abs(miss_by)))))

get_stf_value <- function(stf) {
  ifelse(stf > 200, 100,
         ifelse(stf < 0, -100,
                stf - 100))
}

get_loc_value <- function(loc) {
  ifelse(loc > 200, 100,
         ifelse(loc < 0, -100,
                loc - 100))
}

get_pitch_number_value <- function(pitch_number) {
  ifelse(pitch_number == 1, -60.7,
         ifelse(pitch_number == 2, -100,
                ifelse(pitch_number == 3, -88.8,
                       ifelse(pitch_number == 4, -29.4,
                              ifelse(pitch_number == 5, 7.74,
                                     ifelse(pitch_number == 6, 98.2,
                                            ifelse(pitch_number == 7, 76.0,
                                                   ifelse(pitch_number >= 8, 100, NA))))))))
}

get_middle_middle_value <- function(middle_middle) {
  ifelse(middle_middle == 1, -100, 100)
}

get_strike_value <- function(strikes) {
  ifelse(strikes == 0, -100,
         ifelse(strikes == 1, -100,
                ifelse(strikes == 2, 100, NA)))
}

get_ball_value <- function(balls) {
  ifelse(balls == 0, -65.6,
    ifelse(balls == 1, -100,
      ifelse(balls == 2, -80.9,
        ifelse(balls == 3, 100, NA))))
}

get_match_top_zone_value <- function(match_top_zone) {
  ifelse(match_top_zone == 1, -100, 100)
}

get_in_zone_value <- function(in_zone) {
  ifelse(in_zone == 1, 100, -100)
}

new_fouls_model_data <- fouls_model_data_2 %>%
  mutate(
    stf_value = get_stf_value(stf),
    loc_value = get_loc_value(loc),
    pitch_number_value = get_pitch_number_value(pitch_number),
    middle_middle_value = get_middle_middle_value(middle_middle),
    strike_value = get_strike_value(strikes),
    ball_value = get_ball_value(balls),
    match_zone_value = get_match_top_zone_value(match_top_zone),
    in_zone_value = get_in_zone_value(in_zone),
    miss_by_value = miss_by
  )

calculate_objective <- function(coefficients) {
  stf_coeff <- coefficients[1]
  loc_coeff <- coefficients[2]
  pitch_number_coeff <- coefficients[3]
  middle_middle_coeff <- coefficients[4]
  strike_coeff <- coefficients[5]
  ball_coeff <- coefficients[6]
  match_zone_coeff <- coefficients[7]
  in_zone_coeff <- coefficients[8]
  miss_by_coeff <- coefficients[9]  # New coefficient for miss_by
  
  foul_rating <- new_fouls_model_data %>%
    mutate(frr = (stf_value * stf_coeff + loc_value * loc_coeff + 
                    pitch_number_value * pitch_number_coeff + middle_middle_value * middle_middle_coeff +
                    strike_value * strike_coeff + ball_value * ball_coeff +
                    match_zone_value * match_zone_coeff + in_zone_value * in_zone_coeff + 
                    miss_by_value * miss_by_coeff))
  
  same_sign_percentage <- sum((foul_rating$change > 0 & foul_rating$frr > 0) |
                                (foul_rating$change < 0 & foul_rating$frr < 0)) / nrow(foul_rating) * 100
  
  negative_percentage <- sum(foul_rating$frr < 0) / length(foul_rating$frr) * 100
  
  # Return optimal coefficients, negative percentage, and same sign percentage
  return(list(coefficients = coefficients, 
              negative_percentage = negative_percentage,
              same_sign_percentage = same_sign_percentage))
}


# Grid search
coefficients <- expand.grid(
     stf_coeff = 0.08,
     loc_coeff = 0.08,
     pitch_number_coeff = 0.20,
     middle_middle_coeff = 0.04, # binary
     strike_coeff = 0.20,
     ball_coeff = 0.25,
     match_zone_coeff = 0.05, # binary
     in_zone_coeff = 0.04, # binary
     miss_by_coeff = 0.05  # New coefficient for miss_by
  )

valid_coefficients <- coefficients[rowSums(coefficients) == 1, ]

objective_results <- apply(valid_coefficients, 1, calculate_objective)

min_index <- which.min(sapply(objective_results, function(result) abs(result$negative_percentage - 74.31236) + (100 - result$same_sign_percentage)))
#min_index <- which.min(sapply(objective_results, function(result) (100 - result$same_sign_percentage)))
optimal_result <- objective_results[[min_index]]

print(optimal_result$coefficients)
print(optimal_result$negative_percentage)
print(optimal_result$same_sign_percentage)


foul_rating <- new_fouls_model_data %>%
  mutate(frr = (stf_value * 0.07 + loc_value * 0.07 + 
                  pitch_number_value * 0.15 + middle_middle_value * 0.04 +
                  strike_value * 0.2 + ball_value * 0.25 +
                  match_zone_value * 0.07 + in_zone_value * 0.05 + 
                  miss_by_value * 0.1))

bichette_1 <- foul_rating %>% filter(game_pk == 716492, at_bat_number == 24)
bichette_2 <- foul_rating %>% filter(game_pk == 718134, at_bat_number == 39)

write.csv(foul_rating, "with_frr_metric.csv")

read_csv("with_frr_metric.csv") -> w_frr

w_frr %>% filter(frr < 0.01 & frr > -0.01) -> neu

w_frr %>% 
  mutate(pitch_number = ifelse(pitch_number > 8, 8, pitch_number)) %>%
  group_by(pitch_number) %>% 
  summarize(mean(change_in_obp))
