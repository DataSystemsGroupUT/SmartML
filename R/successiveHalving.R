#' @importFrom caret createDataPartition
#' @importFrom stringr str_glue
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows

#' @keywords internal
successive_halving <- function(df, params_config, n, r, eta = 3, max_iter = 81, s_max = 4, evaluations = data.frame()) {

  final_df <- params_config

  for (i in 0:s_max) {

    gc()

    n_i = n * (eta ** -i)

    r_i = r * (eta ** i)

    r_p = r_i / max_iter

    min_train_datapoints = (length(unique(df$class)) * 3) + 1

    min_prob_datapoints = min_train_datapoints / nrow(df$class)

    df_indexes = suppressWarnings(caret::createDataPartition(y = df$class,
                                                             p = max(min(r_p, 0.8), min_prob_datapoints),
                                                             list = F,
                                                             times = 1))

    train_df = df[df_indexes, ] %>% as_tibble()

    test_df = df[-df_indexes, ] %>% as_tibble()

    configs = final_df

    model_list = configs$model %>% as.character()

    params_list = configs$params %>% as.character()

    partial_eval = partial(.f = eval_loss,
                            train_df = train_df,
                            test_df = test_df)

    test_sample = vector(mode = "double", length = length(model_list))

    for(j in 1:length(model_list)) {

     test_sample[[j]] <- partial_eval(model = model_list[[j]], params = params_list[[j]]) %>%
       .$perf

    }

    configs$acc <- test_sample

    final_df <- configs %>%
      as_tibble() %>%
      mutate(acc = round(as.numeric(acc), 4)) %>%
      arrange(desc(acc)) %>%
      mutate_all(.funs = as.character) %>%
      mutate(budget = r_i,
             rp = r_p)

    evaluations <- evaluations %>% bind_rows(final_df)

    final_df <- final_df %>%
      head(max(n_i/eta, 1))

   if(i == s_max){

      return(list("answer" = final_df, "sh_runs" = evaluations))

    }

  }
}
