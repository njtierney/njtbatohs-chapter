#' plot_fig_four
#' 
#' plot figure four, which shows the observed and predicted values along with their credible intervals, colouring these according to whether the observed value lies within the 95% credible interval
#'
#' @param obs_pred_summary the summary data obtained from `create_obs_pred_summary.R`
#'
#' @return ggplot2 plot figure
#' @export
#'
#' @import ggplot2, dplyr
#' 
plot_fig_four <- function(obs_pred_summary){
    
    obs_pred_summary %>% 
        filter(forecasted == FALSE) %>%
    ggplot(aes(x = outcome,
               y = pred_mean)) +
        theme_hc() +
        labs(x = "Observed",
             y = "Posterior Mean Values") +
        geom_abline(slope = 1, intercept = 0) +
        geom_linerange(aes(ymin = pred_025,
                           ymax = pred_975,
                           colour = as.factor(in_range_95)),
                       alpha = 0.2) +
        geom_point(aes(colour = as.factor(in_range_95)),
                   alpha = 0.5) +
        scale_colour_manual(values = c("steelblue", "red")) +
        theme(legend.position = "none")

} # close function