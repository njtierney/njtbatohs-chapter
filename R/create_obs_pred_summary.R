#' create_obs_pred_summary
#' 
#' This function gets summary information for the predicted Y in the JAGS model, as I have laid it out, and combines it with the data so you can do really cool plots
#'
#' @param jags_dt_summary a data.table summary of the jags model
#' @param jags_dat_model the data that went into the jags model
#' @param jags_dat_prep the preperatory data that went into the jags model - purely so I can get the SEG information.
#'
#' @return data.frame 
#' @export
#'
#' @import data.table, dplyr
create_obs_pred_summary <- function(jags_dt_summary, 
                                 jags_dat_model,
                                 jags_dat_prep){
    
    obs_pred_summary <- 
    jags_dt_summary[parameter %like% "y_pred"] %>%
    mutate(outcome = jags_dat_model$y) %>%
    dplyr::rename(pred_mean = mean,
           pred_025 = q_02.5,
           pred_010 = q_10,
           pred_median = median,
           pred_90 = q_90,
           pred_975 = q_975) %>%
    mutate(ID = jags_dat_model$ID,
           SEG = jags_dat_prep$SEG,
           days_since_arrival = as.numeric(jags_dat_model$x),
           param_num = readr::parse_number(parameter),
           in_range_95 = if_else(outcome >= pred_025 & outcome <= pred_975,
                              true = "inside",
                              false = "outside"),
           in_range_80 = if_else(outcome >= pred_010 & outcome <= pred_90,
                                 true = "inside",
                                 false = "outside")) %>%
    group_by(ID) %>%
    mutate(n_visit = 1:n(),
           max_visits = max(n_visit)) %>%
    ungroup()
# View(dat_pred_y_sum)
    
    return(obs_pred_summary)

} # close function