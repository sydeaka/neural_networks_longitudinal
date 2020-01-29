message('Helper functions for modeling')
fit_model = function(
    training_df = NULL,
    num_dense_layers = 4,
    num_recurrent_layers = 1,
    tsteps = look_back,
    batch_size = 32,
    num_epochs = 3,
    num_dense_units = 100,
    optimizer_name = 'adam',
    learning_rate = 0.0001,
    lstm_nodes = 300,
    lstm_dropout = 0.20,
    dense_activation = 'relu',
    alpha = 0.5,
    patience_lr = 4,
    patience_es = 4,
    max_lr_reductions = 12,
    lr_reduce_factor = 0.80,
    min_delta_lr = .0001,
    min_delta_es = .001,
    loss_weights = c(2.0, 1.0, 2.0),
    l1_term = .0001,
    l2_term = .0001
) {
    ## Parameters
    n_samples = length(unique(training_df$idx))
    n_features = length(att_col_names)
    n_outputs = length(target_col_names)

    optimizer = optimizers[[optimizer_name]]


    ## Clear the Keras session
    k_clear_session()

    ## By default the use_session_with_seed() function disables GPU and CPU 
    ## parallelism, since both can result in non-deterministic execution 
    ## patterns (see https://stackoverflow.com/questions/42022950/).

    # use_session_with_seed(42)

    ## Build the model with specified parameters
    input <- layer_input(shape = c(tsteps, n_features), 
        dtype = 'float32', 
        name = 'main_input')

    ## Helper function to add recurrent layer
    add_recurrent_layer = function(layer_in, ret_seq, lstm_nodes, input_shape = NULL) {
        layer_out = layer_in %>% layer_gru(units            = lstm_nodes,  # layer_cudnn_gru
                input_shape      = input_shape, 
                return_sequences = ret_seq,
                kernel_regularizer = regularizer_l1_l2(l1 = l1_term, l2 = l2_term)) %>% 
                layer_batch_normalization() %>% 
                layer_activation_leaky_relu(alpha = alpha) %>%
        layer_dropout(rate=lstm_dropout, seed=random_seed)

        return(layer_out)

    }

    if (num_recurrent_layers == 1) {
        output = input %>% add_recurrent_layer(ret_seq=F, lstm_nodes, input_shape = c(tsteps, n_features))
    } else {
        output = input %>% add_recurrent_layer(ret_seq=T, lstm_nodes, input_shape = c(tsteps, n_features))
        for (k in 2:num_recurrent_layers) {
            ## Nodes decreasing by factors of 2
            num_nodes = lstm_nodes / (2^(k-1))
            ret_seq = ifelse(k == num_recurrent_layers, F, T)
            output = output %>% add_recurrent_layer(ret_seq=ret_seq, num_nodes, input_shape = NULL)
        }
    }

    
        

    for (k in 1:num_dense_layers) {
        ## Nodes decreasing by factor of 2
        num_nodes = num_dense_units / (2^(k-1))

        output = output %>%
        layer_dense(units=num_nodes) %>% # , activation = dense_activation
        layer_batch_normalization() %>%
        layer_activation_leaky_relu(alpha = alpha) %>%
        layer_dropout(rate=lstm_dropout, seed=random_seed)
    }            

    

    multi_output = lapply(1:length(target_col_names), function(k) {
         output %>% layer_dense(units = 1, name=paste0('output_', k))
        })
    



    model <- keras_model(
    inputs = c(input), 
    outputs = multi_output
)



    model %>% 
        compile(loss = 'mse', 
        optimizer = optimizer(lr = learning_rate, clipvalue = 1.0),
        loss_weights = loss_weights
        )
        

    message(paste('Model has', 
        count_params(model) %>% prettyNum(big.mark=',', scientific=F), 
        'parameters.'))


    ## Fit the model
    training_time = system.time({history = model %>% 
        fit(train_x, 
            lapply(1:length(target_col_names), function(k) train_y[,k]),
            epochs=num_epochs, 
            shuffle = T, 
            batch_size = batch_size,
            validation_data = list(valid_x, 
                                    lapply(1:length(target_col_names), function(k) valid_y[,k])
                                    ),
            view_metrics=F, 
            callbacks = list(
                #callback_model_checkpoint("checkpoints.h5"),
                callback_reduce_lr_on_plateau(monitor = "val_loss", factor = lr_reduce_factor, 
                    min_delta = min_delta_lr, verbose = 1, patience = patience_lr),
                callback_early_stopping(monitor = "val_loss", 
                    min_delta = min_delta_es, patience = patience_es, verbose = 1, mode = "min") #,
                #callback_progbar_logger(count_mode = "samples", stateful_metrics = NULL)
        
        ))
            }); print(training_time)


    ## Plot validation and training loss
    history_plot = plot(history, smooth=F, theme_bw=F) +  
        geom_line() + 
        ylim(0.0025, 0.01) 


    ## Plot predicted vs actual
    fit = history
    mod = model
    valid_pred_y = predict(mod, valid_x) %>% sapply(function(u) u)
    valid_true_y = valid_y

    if (length(target_col_names) == 1) {
            valid_pred_y = data.frame(x1=valid_pred_y)
        } 
    colnames(valid_pred_y) = target_col_names
    colnames(valid_true_y) = target_col_names

    df_plot = lapply(target_col_names, function(cname) data.frame(Statistic = cname, actual=valid_true_y[,cname], pred=valid_pred_y[,cname] ) %>%
        mutate(Statistic = gsub('Batting_', '', Statistic))
    ) %>% bind_rows

    pred_plot = df_plot %>%
        ggplot(aes(x=actual, y=pred)) +
        geom_point() +
        geom_abline(slope = 1, intercept = -0.10, linetype="dashed", color = "red") +
        geom_abline(slope = 1, intercept = 0) + 
        geom_abline(slope = 1, intercept = 0.10, linetype="dashed", color = "red") +
        facet_wrap(~Statistic, nrow=length(target_col_names)) +
        xlim(0, 1) +
        ylim(0, 1)
        
    #pred_plot %>% print()

    grid.arrange(history_plot, pred_plot, ncol=2) %>% print()

    print(history)
    
    return(list(history=history, model=model))


}






random_fit = function(params=NULL, testing=FALSE, set_params=NULL) {
    if (!is.null(set_params)) {
        message('Using specific parameters provided')
        random_params = set_params
    } else {
        message('Random draw of parameter values')
        random_params = lapply(params, function(u) {
            if (length(u) > 1) sample(u, 1) else u
        })
        random_params$loss_weights = params$loss_weights

    }
    
    random_params$patience_es = random_params$patience_lr * random_params$max_lr_reductions + 
            ceiling(random_params$patience_lr/2)


    if (testing) {
        message('Reset some values for testing')
        random_params$num_epochs = 2
        random_params$num_dense_layers = 1
        random_params$num_dense_units = 10
        random_params$lstm_nodes = 10
        random_params$nodes_last_layer = 10
    }

    ## Other global parameters
    if (F) {
        message('Other global parameters')
        random_params$min_length = min_length
        random_params$max_length = max_length
        random_params$min_plate_appearances = min_plate_appearances
        random_params$min_year = min_year
        random_params$nplayers = nplayers
        random_params$pct_train = pct_train
        random_params$pct_test = pct_test
        random_params$look_forward = look_forward
        random_params$look_back = look_back
    }

    message('Using the following parameters:')
    print_params = random_params
    print_params$loss_weights = paste(print_params$loss_weights, collapse=', ')
    print_params %>% as.data.frame %>% print()

    model_number = ifelse(is.null(leader), 1, nrow(leader) + 1)
    message(paste('\nModel #', model_number))

    message('\nFit the model')
    message(paste('Training started at ', Sys.time()))
    tic(quiet=F)
    fitx = fit_model(
        training_df = dat_train_scaled,
        num_dense_layers = random_params$num_dense_layers,
        num_recurrent_layers = random_params$num_recurrent_layers,
        tsteps = look_back,
        batch_size = random_params$batch_size,
        num_epochs = random_params$num_epochs,
        num_dense_units = random_params$num_dense_units,
        optimizer_name = random_params$optimizer_name,
        learning_rate = random_params$learning_rate,
        lstm_nodes = random_params$lstm_nodes,
        lstm_dropout = random_params$lstm_dropout,
        dense_activation = random_params$dense_activation,
        alpha = random_params$alpha,
        patience_lr = random_params$patience_lr,
        patience_es = random_params$patience_es,
        max_lr_reductions = random_params$max_lr_reductions,
        lr_reduce_factor = random_params$lr_reduce_factor,
        min_delta_lr = random_params$min_delta_lr,
        min_delta_es = random_params$min_delta_es,
        loss_weights = random_params$loss_weights,
        l1_term = random_params$l1_term,
        l2_term = random_params$l2_term
    )
    toc(quiet=F)
    message(paste('Training stopped at ', Sys.time(), '.'))

    #message('Last value observed for each of the performance metrics')
    last_value_of_metrics = sapply(fitx$history$metrics, function(u) u[length(u)])
    last_value_of_metrics = last_value_of_metrics %>% sapply(function(u) round(u, 5))
    last_value_of_metrics['epoch_stop'] = length(fitx$history$metrics$val_loss)


    message('Summary dataframe containing selected parameter values and performance metrics')
    out_metrics = data.frame(model_num = model_number,
    t(unlist(c(random_params, last_value_of_metrics))), stringsAsFactors = F)
    rtn = list(fit=fitx, random_params=random_params, last_value_of_metrics=last_value_of_metrics)

    if (is.null(leader)) {
        leader_out = out_metrics
    } else {
        leader_out = bind_rows(leader %>% as.data.frame, out_metrics) %>% 
        arrange(val_loss)
    }

    leader_out = leader_out %>%
        mutate(outcomes = paste(target_col_names, collapse=','))

    leader <<- leader_out
    leader %>% write.csv(file=output_csv)
    #drop_upload(output_csv, dtoken = token)
    model_output[[model_number]] <<- rtn

    return(invisible(rtn))
}




vloss_rename = function(vname) {
    out_name = gsub('_loss', '', vname)
    out_name = gsub('val_output_', 'vloss_', out_name)
    return(out_name)
}







leaderboard_preview = function (leader) {
    if (length(target_col_names) > 1) {
        loss_names = sapply(1:length(target_col_names), function(k) paste("val_output", 
        k, "loss", sep = "_"))
    } else {
        loss_names = NULL
    }
 
    out_tbl = leader %>% select_at( 
        c("model_num", "val_loss", loss_names, "epoch_stop", "num_recurrent_layers", 
        "num_dense_layers", "num_dense_units",  "lstm_nodes", 
            "alpha", "learning_rate", "optimizer_name", "lr_reduce_factor", 
            "batch_size", "lstm_dropout", "min_delta_lr", "outcomes")) 
            
    if (length(target_col_names) > 1) out_tbl = 
        out_tbl %>% rename_if(startsWith(colnames(.), "val_output_"), vloss_rename) %>%
        filter(num_epochs > 2) 
        
    out_tbl %>% head()
}
