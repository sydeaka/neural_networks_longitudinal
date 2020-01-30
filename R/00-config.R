## Load packages
message('Load packages')


install.packages('pacman')

library(pacman)
pacman::p_load(
    ## Needed for data pre-processing and modeling 
    dplyr, tidyverse, readxl, padr, gridExtra, caret, tictoc, #googledrive,

    ## Data pre-processing only
    rsample, lubridate, fastDummies, Lahman , 

    ## Modeling only
    keras, tensorflow #, tfruns, tfestimators, 
    )



## Set parameters
message('Set parameters')
random_seed = 100
min_plate_appearances = 85 # suggestion from Jimmy E. to require PA > 100 to focus on hitters
min_year = 1916 # earliest year to include so that stadium attendance is captured.
# If we remove attendance attribute, we could remve this requirement
pct_train = .80
pct_test = .50 * (1 - pct_train)
pct_valid = pct_test

look_back = 3
look_forward = 1
min_length = look_back + look_forward
max_length = 20
above_pct = 0.50


## Slash line outcomes
slash_line_outcomes = c('Batting_BattingAverage', 'Batting_OnBasePct', 'Batting_SlugPct') 
slash_line_outcomes_fmt = slash_line_outcomes %>% gsub('Batting_', '', .)