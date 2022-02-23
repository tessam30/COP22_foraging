# UTILITY FUNCTIONS

#' @title Extract Data
#'
#'
extract_data <- function(df_msd, ...,
                         fy = NULL,
                         agencies = NULL,
                         ind = "HTS_TST_POS",
                         disagg = "Total Numerator") {
  
  # Retrieve current fiscal year
  curr_fy <- df_msd %>% identifypd(pd_type = "year")
  
  if (is.null(fy)) {
    fy = curr_fy
  }
  
  # Retrieve agencies
  if (is.null(agencies)) {
    agencies <- df_msd %>%
      filter(str_to_lower(fundingagency) != "DEDUP") %>%
      distinct(fundingagency) %>%
      pull()
  }
  
  # Summarize data by
  df_msd %>%
    filter(fiscal_year == fy,
           fundingagency %in% agencies,
           indicator == ind,
           standardizeddisaggregate == disagg) %>%
    rename(results = cumulative) %>%
    group_by(fiscal_year, fundingagency, ..., indicator) %>%
    summarize(across(matches("targets|results"), sum, na.rm = T), .groups = "drop")
}


#' @title Decompose Results
#'
#'
decompose_results <- function(df_rsts, ...) {
  
  df_rsts %>%
    group_by(..., indicator) %>%
    mutate(rslt_capped = ifelse(results > targets, targets, results),
           rslt_gap = results - targets,
           rslt_deficit = ifelse(results < targets, results - targets, NA_real_),
           rslt_surplus = ifelse(results >= targets, results - targets, NA_real_),
           achv = results / targets,
           achv_adj = rslt_capped / targets,
           over_achv = results > targets) %>%
    ungroup() %>%
    arrange(rslt_gap)
}


#' @title Calculate Gap Share
#'
#'
calculate_gapshare <- function(df_rsts, ...) {
  
  df_rsts %>%
    group_by(..., over_achv) %>%
    mutate(rslt_gap_tot = sum(rslt_gap)) %>%
    ungroup()%>%
    mutate(rslt_gap_sh = rslt_gap / rslt_gap_tot,
           gap_running_sh = cumsum(rslt_gap_sh),
           gap_running_target = cumsum(rslt_gap)
    )
}


#' @title Roll up achv and adjusted achv
#'
#'
rollup_achv <- function(df_rsts, ...) {
  
  df_rsts %>%
    group_by(...) %>%
    mutate(across(c(targets:rslt_surplus), sum, na.rm = T,
                  .names = "{.col}_grp")) %>%
    ungroup() %>%
    mutate(achv_grp = results_grp / targets_grp,
           achv_adj_grp = rslt_capped_grp / targets_grp,
           deficit_sh = abs(rslt_deficit_grp)/targets_grp,
           surplus_sh = rslt_surplus_grp / targets_grp) %>%
    mutate(#grp_order = fct_reorder(operatingunit, results),
      facet_label = ifelse(over_achv == TRUE,
                           "Achieved Targets",
                           "Unachieved"))
}



#' @title Group Visualization
#'
#'
viz_group <- function(df_rsts, grp_name = "Global") {
  
  df_rsts %>%
    ggplot(aes(y = grp_name)) +
    # What is benchmark?
    geom_col(aes(x = targets_grp), fill = trolley_grey_light) +
    # What is total rollup?
    geom_col(aes(x = results_grp), fill = scooter_med) +
    # What is target capped rollup
    geom_col(aes(x = rslt_capped_grp), fill = scooter) +
    # What is target capped deficit?
    geom_col(aes(x = rslt_deficit_grp), fill = old_rose_light) +
    geom_text(aes(x = rslt_capped_grp, label = percent(achv_adj_grp)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_text(aes(x = results_grp, label = percent(achv_grp)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_text(aes(x = rslt_deficit_grp, label = percent(deficit_sh)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_vline(xintercept = 0, size = 0.5, color = grey90k) +
    scale_x_continuous(labels = scales::label_number_si())+
    si_style_xgrid() +
    coord_cartesian(clip = "off", expand = F) +
    labs(x = NULL, y = NULL)
  # labs(x = NULL, y = NULL, title = glue("GLOBAL ACHIEVEMENT IS {scales::percent({achv_lab})},
  #                                         ADJUSTED ACHIEVEMENT IS {percent({achv_adj_lab})}"))
  
}


#' @title Group Details
#'
#'
viz_details <- function(df_rsts) {
  
  df_rsts %>%
    ggplot(aes(y = grp_order)) +
    geom_col(aes(x = targets), fill = trolley_grey_light) +
    geom_col(aes(x = results), fill = scooter_med) +
    geom_col(aes(x = rslt_capped), fill = scooter) +
    geom_errorbar(aes(xmin = targets, xmax = targets,
                      color = ifelse(!is.na(rslt_surplus),
                                     "white", NA_character_)), size = 0.5) +
    geom_col(aes(x = rslt_deficit), fill = old_rose_light) +
    geom_text(aes(x = results, label = percent(achv, 1)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_vline(xintercept = 0, size = 0.5, color = grey90k) +
    facet_grid(facet_label~., scales = "free_y", space = "free_y") +
    scale_x_continuous(labels = scales::label_number_si())+
    si_style_xgrid() +
    scale_color_identity()  +
    coord_cartesian(clip = "off", expand = F) +
    labs(x = NULL, y = NULL)
  #labs(x = NULL, y = NULL, title = glue("{ou_count} OUs ACHIEVED TARGETS AS OF {curr_pd}"))
  
}


#' @title Group Decomposition
#'
#'
viz_decomposition <- function(df_rsts,
                              grp_name = "Global",
                              filename = NULL) {
  
  # Produce individual Vis
  viz_grp <- df_rsts %>% slice(1) %>% viz_group(grp_name)
  
  viz_details <- df_rsts %>% viz_details()
  
  # Set viz dimensions
  max_dims <- get_max_dim(viz_grp, viz_details)
  
  set_dim(viz_details, max_dims)
  
  # Group all viz
  viz <- viz_grp / viz_details + plot_layout(heights = c(1, 7))
  
  if (!is.null(filename))
    si_save(file.path("./Images/adj_achievement.png"), scale = 1.45)
  
  return(viz)
}


