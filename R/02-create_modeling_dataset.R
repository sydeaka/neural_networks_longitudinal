
message('Bring in data sources')
bstats <- battingStats() %>%
  arrange(playerID, yearID) %>%
  filter(stint==1 & yearID >= min_year)



### Filter on plate appearances to focus on batters



## Filling in empty years
pa_padded <- bstats %>%
  select(playerID, yearID, PA) %>%
  arrange(playerID, yearID) %>%
  mutate(date = as.Date(paste0(yearID, '-12-31'))) %>% 
  arrange(playerID, date) %>%
  pad(interval='year', by='date', group='playerID') %>% 
  mutate(yearID = year(date), PA = replace_na(PA, 0)) %>%
  select(-date) 


## Get windows that show 3 seasons + 1 follow-up season
 pa_window <- pa_padded %>%
    group_by(playerID) %>%
  rename(PA_current = PA) %>%
  mutate(PA_minus1 = lag(PA_current, 1),
         PA_minus2 = lag(PA_current, 2),
         PA_plus1 = lead(PA_current, 1)
         ) %>%
  select(playerID, yearID, PA_minus2, PA_minus1, PA_current, PA_plus1) %>%
  filter(!is.na(PA_minus1) & !is.na(PA_minus2) & !is.na(PA_plus1)) %>% 
  ungroup
 





### Apply plate appearances filter
# This table contains the final list of players to include in the analysis.


message('Apply plate appearances filter')
pa_filtered_raw <- pa_window %>%
   filter(PA_minus1 >= min_plate_appearances & 
            PA_minus2 >= min_plate_appearances & 
            PA_current >= min_plate_appearances & 
            PA_plus1 >= min_plate_appearances) %>%
  apply_data_filters






# Training/testing/validation split on player IDs so as to avoid data leakage between test/validation and training sets.
message('Training/testing/validation split on player IDs')
set.seed(random_seed)


## Vector of all players


players_vec <- unique(pa_filtered_raw$playerID)
nplayers <- length(players_vec)
ntrain <- ceiling(pct_train * nplayers)

## Training set
playerIDs_train <- sample(x=players_vec, size=ntrain, replace=F)

## Testing and validations sets
playerIDs_testvalid <- players_vec[players_vec %in% playerIDs_train == F]
ntest <- ceiling(pct_test * length(playerIDs_testvalid))
playerIDs_valid <- sample(playerIDs_testvalid, size=ntest, replace=F)
playerIDs_test <- playerIDs_testvalid[playerIDs_testvalid %in% playerIDs_valid == F]

## All players accounted for?
all_accounted_for <- length(playerIDs_train) + length(playerIDs_test) + length(playerIDs_valid) == nplayers
cat('All players accounted for?', all_accounted_for, '\n')


# Add train/test/validation split labels to the filtering dataframe

pa_filtered <- pa_filtered_raw %>%
  mutate(data_split = case_when(playerID %in% playerIDs_train ~ 'train',
                   playerID %in% playerIDs_test ~ 'test',
                   playerID %in% playerIDs_valid ~ 'valid',
                   TRUE ~ 'other'),
          min_year = yearID - 2,
          max_year = yearID + 1) 


# For each player, determine the years of data that contain the minimum amount of plate appearances
message('Find players with correct number of plate appearances')


player_years <- lapply(players_vec, get_player_years) %>% bind_rows
player_years %>% head


message('Batting Stats table...')
batting <- bstats %>%
  rename(Batting_AtBats = AB, 
         Batting_Runs = R, #RunsCreated=RC, RunsProduced=RP,
         Batting_BaseOnBalls = BB, 
         Batting_Hits = H, 
         Batting_HomeRuns = HR, 
         Batting_Games = G,
         Batting_Doubles = X2B,
         Batting_Triples = X3B,
         Batting_RunsBattedIn = RBI,
         Batting_StolenBases = SB,
         Batting_CaughtStealing = CS,
         Batting_Strikeouts = SO,
         Batting_IntentionalWalks = IBB,
         Batting_HitByPitch = HBP,
         Batting_SacrificeHits = SH,
         Batting_Sacrifice_Flies = SF,
         Batting_GroundedIntoDoublePlays = GIDP,
         Batting_PlateAppearances = PA,
         Batting_BattingAverage = BA,
         Batting_OnBasePct = OBP,
         Batting_SlugPct = SlugPct
  ) %>%
  select(playerID, lgID, yearID, teamID,  
Batting_AtBats, Batting_Runs, Batting_BaseOnBalls, Batting_Hits, # Batting
          Batting_HomeRuns, Batting_Games, Batting_BattingAverage, Batting_PlateAppearances, 
          Batting_BattingAverage, Batting_OnBasePct, Batting_SlugPct,
          Batting_Doubles, Batting_Triples, Batting_RunsBattedIn, Batting_StolenBases,
          Batting_CaughtStealing, Batting_Strikeouts, Batting_IntentionalWalks, 
          Batting_HitByPitch, Batting_SacrificeHits, Batting_Sacrifice_Flies,
          Batting_GroundedIntoDoublePlays) %>% 
  distinct()




baseball_raw <- batting %>% # base table
  inner_join(player_years, by=c('playerID', 'yearID')) %>% # filters players of primary interest
  arrange(playerID, yearID) %>%
  select(playerID, lgID, yearID, teamID, data_split,
          Batting_AtBats, Batting_Runs, Batting_BaseOnBalls, Batting_Hits, # Batting
          Batting_HomeRuns, Batting_Games, Batting_BattingAverage, Batting_PlateAppearances, 
          Batting_BattingAverage, Batting_OnBasePct, Batting_SlugPct,
          Batting_Doubles, Batting_Triples, Batting_RunsBattedIn, Batting_StolenBases,
          Batting_CaughtStealing, Batting_Strikeouts, Batting_IntentionalWalks, 
          Batting_HitByPitch, Batting_SacrificeHits, Batting_Sacrifice_Flies,
          Batting_GroundedIntoDoublePlays
         ) %>%
  apply_data_filters() %>%
  #select_at(cols_include) %>%
  replace(is.na(.), 0) %>% # replace missing values with zeroes
  mutate(idx = as.numeric(as.factor(playerID))) %>%
  select_at(c('playerID', 'idx', colnames(.)[colnames(.) %in% c('playerID', 'idx') == F])) %>%
  distinct() 




#Export to CSV
message('Export to CSV')
baseball_raw %>%write_csv('baseball_dat_modeling.csv')




