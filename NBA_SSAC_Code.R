library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(clue)
library(tibble)

# nba_SSAC_data <- read_excel("Downloads/nba_SSAC_data.xlsx")
# Once nba_SSAC_data.xlsx is imported, the code is as follows.

nba_summary <- nba_SSAC_data

nba_summary$avg_distance_hour <- nba_summary$avg_speed
nba_summary$avg_speed <- nba_summary$avg_distance_hour * 48 / 60 # Average distance per game
nba_summary$pmpg <- nba_summary$total_plus_minus / (nba_summary$total_minutes / 48) # Plus-minus per game

nba_clean <- nba_summary %>%
  filter(!is.na(age), !is.na(position_num), !is.na(RAT))

match_champs_vs_finalists <- function(df, 
                                      scale_within_season = TRUE,
                                      bigM = 1e9,
                                      require_all_delta_vars = TRUE) {
  # Coerce to numeric early
  df <- df %>%
    mutate(
      age = as.numeric(age),
      position_num = as.numeric(position_num),
      RAT = as.numeric(RAT),
      effort = as.numeric(avg_speed),
      performance = as.numeric(pmpg),
      player_id = as.character(player_id)
    )
  
  # Compute deltas (next - current) based on SAME name+team and NEXT season only
  df <- df %>%
    mutate(season_start = season) %>%
    arrange(name, team, season_start) %>%
    group_by(name, team) %>% 
    mutate(
      next_season_start        = lead(season_start),
      delta_effort             = lead(effort)      - effort,
      delta_performance        = lead(performance) - performance,
      has_both_effort          = !is.na(effort) & !is.na(lead(effort)) &
        effort != 0 & lead(effort) != 0,
      has_both_performance     = !is.na(performance) & !is.na(lead(performance)) &
        performance != 0 & lead(performance) != 0
    ) %>%
    ungroup()
  
  # Keep only rows that truly have the following season for the SAME name+team
  if (require_all_delta_vars) {
    df <- df %>% filter(
      has_both_effort, has_both_performance,
      !is.na(next_season_start), next_season_start == season_start + 1
    )
  } else {
    df <- df %>% filter(
      (has_both_effort | has_both_performance),
      !is.na(next_season_start), next_season_start == season_start + 1
    )
  }
  
  # Normalize finals_result and keep needed cols (carry the deltas, not raw metrics)
  dat <- df %>%
    filter(finals_result %in% c("Champion","Finalist")) %>%
    select(
      season, name, player_id, team, position, finals_result,
      age, position_num, RAT,
      delta_effort, delta_performance
    ) %>%
    filter(!is.na(finals_result), finals_result %in% c("Champion","Finalist"),
           !is.na(age), !is.na(position_num), !is.na(RAT))
  
  # Optional z-scoring for match features only
  if (scale_within_season) {
    dat <- dat %>%
      group_by(season) %>%
      mutate(
        age = as.numeric(scale(age)),
        position_num = as.numeric(scale(position_num)),
        RAT = as.numeric(scale(RAT))
      ) %>%
      ungroup()
  }
  
  build_cost <- function(champs, finals) {
    if (nrow(champs) == 0 || nrow(finals) == 0) {
      return(matrix(0, nrow = nrow(champs), ncol = nrow(finals)))
    }
    A <- champs %>% select(age, position_num, RAT)
    B <- finals %>% select(age, position_num, RAT)
    c_age <- outer(A$age,          B$age,          function(x,y) (x - y)^2)
    c_pos <- outer(A$position_num, B$position_num, function(x,y) (x - y)^2)
    c_rat <- outer(A$RAT,          B$RAT,          function(x,y) (x - y)^2)
    cm <- c_age + c_pos + c_rat
    mode(cm) <- "numeric"
    cm
  }
  
  results <- dat %>%
    group_by(season) %>%
    group_split() %>%
    map_df(function(season_df) {
      champs <- season_df %>% filter(finals_result == "Champion") %>% arrange(player_id)
      finals <- season_df %>% filter(finals_result == "Finalist") %>% arrange(player_id)
      
      nr <- nrow(champs); nc <- nrow(finals)
      if (nr == 0 || nc == 0) {
        return(tibble(
          season = unique(season_df$season),
          champion_player_id = character(0), 
          champion_name = character(0),
          finalist_player_id = character(0), 
          finalist_name = character(0),
          cost = numeric(0)
        ))
      }
      
      cost <- build_cost(champs, finals)
      
      # pad to square
      if (nr > nc)        cm <- cbind(cost, matrix(bigM, nrow = nr, ncol = nr - nc))
      else if (nc > nr)   cm <- rbind(cost, matrix(bigM, nrow = nc - nr, ncol = nc))
      else                cm <- cost
      
      cm <- as.matrix(cm); storage.mode(cm) <- "double"
      assign_cols <- solve_LSAP(cm)
      
      matches <- tibble(row = seq_len(nrow(cm)), col = as.integer(assign_cols)) %>%
        mutate(valid = row <= nr & col <= nc) %>%
        filter(valid)
      
      tibble(
        season               = season_df$season[1],
        
        champion_player_id   = as.character(champs$player_id[matches$row]),
        champion_name        = champs$name[matches$row],
        champion_team        = champs$team[matches$row],
        champion_pos         = champs$position[matches$row],
        champion_age         = champs$age[matches$row],
        champion_posnum      = champs$position_num[matches$row],
        champion_RAT         = champs$RAT[matches$row],
        
        finalist_player_id   = as.character(finals$player_id[matches$col]),
        finalist_name        = finals$name[matches$col],
        finalist_team        = finals$team[matches$col],
        finalist_pos         = finals$position[matches$col],
        finalist_age         = finals$age[matches$col],
        finalist_posnum      = finals$position_num[matches$col],
        finalist_RAT         = finals$RAT[matches$col],
        
        cost                 = cm[cbind(matches$row, matches$col)],
        
        # matching feature diffs (Champion − Finalist)
        age_diff             = champs$age[matches$row] - finals$age[matches$col],
        posnum_diff          = champs$position_num[matches$row] - finals$position_num[matches$col],
        RAT_diff             = champs$RAT[matches$row] - finals$RAT[matches$col],
        
        # DELTAS (next − current) and their Champion − Finalist differences
        delta_effort_champion     = champs$delta_effort[matches$row],
        delta_effort_finalist     = finals$delta_effort[matches$col],
        delta_effort_diff         = champs$delta_effort[matches$row] -
          finals$delta_effort[matches$col],
        
        delta_performance_champion = champs$delta_performance[matches$row],
        delta_performance_finalist = finals$delta_performance[matches$col],
        delta_performance_diff     = champs$delta_performance[matches$row] -
          finals$delta_performance[matches$col]
      ) %>%
        arrange(cost)
    })
  
  results
}



pairs <- match_champs_vs_finalists(
  nba_clean,
  scale_within_season = TRUE,        # standardizes age/pos/RAT in the cost
  require_all_delta_vars = TRUE      # ensures both-season data for ALL five deltas
)

# One-sided t-tests: did Champions improve less (Δ < 0 vs Finalists) ?
t_spd      <- t.test(pairs$delta_effort_diff,     mu = 0, alternative = "less")
t_totalpm  <- t.test(pairs$delta_performance_diff, mu = 0, alternative = "less")

t_spd; t_totalpm

t_spd$stderr
t_totalpm$stderr

mean(pairs$delta_effort_diff)/sd(pairs$delta_effort_diff)
mean(pairs$delta_performance_diff)/sd(pairs$delta_performance_diff)
