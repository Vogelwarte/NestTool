## plot_move_metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot seasonal movement metrics of tracked individuals during breeding season
#'
#' \code{plot_move_metrics} produces a series of plots of movement metrics summarised over a moving time window
#' to help diagnose whether a breeding attempt was successful or not.
#'
#' Input data for this function must be derived from \code{\link{move_metric_extraction}}
#'
#'
#' @param movemetrics data.frame derived from \code{\link{move_metric_extraction}}.
#' This table will have multiple columns for different movement metrics, and a column for the individual ("id"),the time period ("week")
#' and age and sex of the respective individual in that season.
#' @param individual character. Unique identifier for individual seasons.
#' Must be contained in column "id" of \code{movemetrics}
#'
#' @return Returns a ggplotly plot and writes a pdf version of the plot into a directory.
#'
#'   
#' @export 
#' @importFrom dplyr filter rename mutate ungroup
#' @importFrom tidyr gather
#' @importFrom lubridate dmy
#' @importFrom ggplot2 ggplot geom_point aes geom_line facet_wrap labs scale_x_date theme element_text element_rect element_line ggsave
#' @importFrom plotly ggplotly
#' @importFrom here here
#' 

plot_move_metrics <- function(movemetrics, individual) {
  
  #### INPUT CHECKS
  
  ## check that df is either a data.frame or a data.table
  if (! "data.frame" %in% class(movemetrics)) {
    if (! "data.table" %in% class(movemetrics)) {
      stop("Object is not a data.table or data.frame. Please try again.")
    }
  }
  
  ## check that individual is in input dataframe
  if (! individual %in% unique(movemetrics$id)) {
      stop("This individual does not exist in the data.frame movemetrics, or the data.frame does not have a column labelled 'id'.")
  }
  
  #### convert to data.frame instead of data.table
  movemetrics <- as.data.frame(movemetrics)

#######~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#######################
######~~~~~~~~~~~~ DIAGNOSTIC PLOTS AS EXAMPLE FOR HOW TO INCLUDE IN SHINY APP ~~~~~~~~~~~~~~~~~########################
#######~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#######################

# plotting

plot_df<-movemetrics %>% 
  dplyr::filter(id==individual) %>%  ## select a single animal at a time
  dplyr::rename(`Home range size (95% MCP, ha)`= MCP,
         `Median daily travel distance (m)`= median_daydist,
         `Median max distance from nest (m)`= median_nestdist,
         `Median daily time at nest (hrs)`= median_time_at_nest,
         `Max time away from nest (hrs)`= max_time_away_from_nest
  ) %>%
  tidyr::gather(key="MoveMetric",value="Value",-id,-week,-age_cy,-sex) %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::mutate(Date=lubridate::dmy(paste0(week, substr(id,1,4)))) %>%
  dplyr::ungroup()
  
  
plot<-ggplot2::ggplot(plot_df) +
  ggplot2::geom_point(ggplot2::aes(x = Date, y=Value, color=MoveMetric), size = 2) +
  ggplot2::geom_line(ggplot2::aes(x = Date, y=Value, color=MoveMetric, group=MoveMetric), linewidth = 1) +
  ggplot2::facet_wrap(~MoveMetric, scales="free_y",ncol=1) + 

  ggplot2::labs(y = "", x = "5 day moving window over season",
       title = paste0("ID: ",
                      plot_df$id[1],
                      " - ",
                      plot_df$sex[1],
                      " - ",
                      plot_df$age_cy[1],
                      " years")) +
  ggplot2::scale_x_date(date_breaks="2 weeks",date_labels=format("%d %b")) +
  ggplot2::theme(plot.title = ggplot2::element_text(colour = "darkolivegreen",
                                  size = 12,hjust = 0.5),
        panel.background=ggplot2::element_rect(fill="#ecf0f1", colour="black"),
        plot.background=ggplot2::element_rect(fill="#ecf0f1"),
        legend.position="none",
        # plot.margin = margin(t = 20,  # Top margin
        #                      r = 5,  # Right margin
        #                      b = 20,  # Bottom margin
        #                      l = 20), # Left margin
        panel.grid.major = ggplot2::element_line(colour = "gray70", size = .05),
        panel.grid.minor = ggplot2::element_line(colour = "gray70"),
        axis.text=ggplot2::element_text(size=10, color="black"),
        # axis.title.y=element_text(margin=margin(0,12,0,0)),
        # axis.title.x=element_text(margin=margin(3,0,0,0)),
        axis.title=ggplot2::element_text(size=10), 
        strip.text=ggplot2::element_text(size=10, color="black"), 
        strip.background=ggplot2::element_rect(fill="#ecf0f1", colour="black")
  )

return(plotly::ggplotly(plot))

# saving plot
ggplot2::ggsave(here::here(sprintf("plots/%s_movement_metrics_season_plot.pdf",i)), width = 2500, height = 4500, units = "px", limitsize = F)


} # end of function call
