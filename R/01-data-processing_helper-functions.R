


message('Load miscellaneous helper functions')


## Could include additional filters later if desired
apply_data_filters = function(dat) {
  dat %>%
    filter(yearID >= min_year)
}






# Find players with correct number of plate appearances
# selected_player = 'abbotku01'

get_player_years = function(selected_player) {
  #cat(selected_player, '\n')
  player_dat = pa_filtered %>%
    filter(playerID == selected_player) %>%
    select(playerID, yearID, min_year, max_year, data_split) 
  
  year_mat = sapply(player_dat$yearID, function(yr) (yr-2):(yr+1))
  year_vec = as.vector(year_mat) %>% unique %>% sort
  out_df = data.frame(playerID=selected_player, 
                      yearID=year_vec, 
                      data_split = player_dat$data_split[1],
                      stringsAsFactors=F)
  return(out_df)
}




# Get training, testing, and validation splits as individual dataframes
get_split = function(split_name) {
    dat = baseball_thresh %>% filter(data_split == split_name)
    dat$data_split = NULL
    pct = round(100 * nrow(dat) / nrow(baseball_thresh), 2)
    print(glue::glue("{split_name} split contains {nrow(dat)} rows ({pct}%) and {ncol(dat)} columns."))
    return(dat)
}





# Get rolling window samples for each data split
get_samples_for_idx = function(dat, selected_idx, return_type = 'x', target_col_names=target_col_names) {
  ro = rolling_origin(data=dat %>% filter(idx == selected_idx), initial=look_back, assess=look_forward, cumulative=F, skip=0)
  ro_splits = ro$splits
  
  if (return_type == 'x') {
    dat_x = lapply(ro_splits, function(spl) spl %>% analysis %>% select_at(att_col_names))
    return(dat_x)
  }
  
  if (return_type == 'y') {
    dat_y = lapply(ro_splits, function(spl) spl %>% assessment %>% select_at(target_col_names))
    return(dat_y)
  }
}






get_samples = function(dat_scaled, target_col_names=target_col_names) {
  df_name = eval(deparse(substitute(dat_scaled))) %>% gsub('(dat_)|(_scaled)', '', .)
  cat('Processing', df_name, '...')

  ids = unique(dat_scaled$idx)

  rolling_windows_test_x = lapply(ids, function(selected_idx) {
    get_samples_for_idx(dat_scaled, selected_idx, return_type = 'x', target_col_names=target_col_names) %>%
      lapply(as.matrix)
    })
  
  rolling_windows_test_y = lapply(ids, function(selected_idx) {
    get_samples_for_idx(dat_scaled, selected_idx, return_type = 'y', target_col_names=target_col_names) %>%
      lapply(as.matrix)
    })

    if (length(ids) == 1) {
        x_list = rolling_windows_test_x[[1]]
        y_list = rolling_windows_test_y[[1]]
        rtn = list(x = x_list, y = y_list)
        return(rtn)
    } else {
        x_list = do.call(c, rolling_windows_test_x)
        y_list = do.call(c, rolling_windows_test_y)
    }

  arr_x = array(0, c(length(x_list), look_back, length(att_col_names))) #; arr_x[1,,]
  arr_y = matrix(0, nrow=length(y_list), ncol=length(target_col_names)); arr_y[1,]
  for (i in 1:length(x_list)) arr_x[i,,] = x_list[[i]]
  for (i in 1:length(y_list)) arr_y[i,] = y_list[[i]]

  out = list(arr_x, arr_y)
  names(out) = paste0(df_name, '_', c('x', 'y'))
  cat('done.\n')
  
  return(out)
}



wide_fmt = function(k, datx=train_x, daty=train_y) {
    dimx = dim(datx)
    dimy = dim(daty)

    if (length(dimx) == 3) {
        x = datx[k,,] 
        colnames(x) = colnames(dat_train_scaled)[-1]
    } else {
        x = datx
    }

    if (length(dimy) == 2) {
      if (nrow(daty) > 1) y = daty[k,] else y = daty
    } else {
        y = daty
    }


    

    vals_y = y
    vals_x = lapply(1:nrow(x), function(u) {
        dt = x[u, ]
        names(dt) = paste0('t', u, '_', names(dt))
        dt
    }) %>% unlist

    out_names = gsub('Batting_', '', colnames(daty))
    names(y) = out_names
    vals = c(vals_x, y)
    return(vals)
}




get_ols_dat = function(datx, daty) {
    wide_dat_list = lapply(1:(dim(datx)[1]), function(k) wide_fmt(k, datx=datx, daty=daty))
    wide_dat = sapply(wide_dat_list, function(u) u) %>% t %>% data.frame
    return(wide_dat)
} 




get_ols_idx = function(idx_samples) {
    wide_dat_list = lapply(1:length(idx_samples$x), function(k) {
    wide_fmt(k, datx=idx_samples$x[[k]], daty=idx_samples$y[[k]])
})

    wide_dat = sapply(wide_dat_list, function(u) u) %>% t %>% data.frame
    return(wide_dat)
}
