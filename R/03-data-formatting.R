message('Data pre-processing')
baseball_thresh = baseball_raw %>%
  filter(lgID %in% c('NL', 'AL'))

message('Identify and select fields and features')
ids = c('playerID', 'idx', 'data_split')
other_features = colnames(baseball_raw)[colnames(baseball_raw) %in% c(ids) == F]
selected_columns = c(ids, other_features)

message('Reorder columns')
baseball <- baseball_raw %>%
  select_at(selected_columns)

message('Get training, testing, and validation splits as individual dataframes')
dat_train = get_split('train')
dat_test  = get_split('test')
dat_valid = get_split('valid')

message('Normalize data to 0-1 range')
normalize_model  =  preProcess(dat_train[,-1], method = "range", rangeBounds = c(0, 1))
dat_train_scaled = normalize_model %>% predict(dat_train)
dat_test_scaled  = normalize_model %>% predict(dat_test)
dat_valid_scaled = normalize_model %>% predict(dat_valid)

message('Get attribute and target column IDs and names')
target_col_names = slash_line_outcomes
cnames = colnames(dat_train) 
att_col_names = cnames[cnames %in% c('idx', 'playerID', 'lgID', 
                                     'teamID', 'team_name', 'data_split') == F]

message('Get attribute and target column numbers')
target_cols = sapply(target_col_names, function(u) which(colnames(dat_train) == u))
att_cols = sapply(att_col_names, function(u) which(colnames(dat_train) == u))

message('Get rolling window samples for each data split')
get_samples(dat_scaled=dat_train_scaled, target_col_names=target_col_names) %>% list2env(., envir=.GlobalEnv)
get_samples(dat_scaled=dat_test_scaled, target_col_names=target_col_names) %>% list2env(., envir=.GlobalEnv)
get_samples(dat_scaled=dat_valid_scaled, target_col_names=target_col_names) %>% list2env(., envir=.GlobalEnv)
