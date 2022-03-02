# PURPOSE: Munge and Analysis of data for VPM Presentations
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-02-18
# NOTES: Create a tool for PEDS site visis

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(gophr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(ggtext)
library(patchwork)
library(ggtext)
library(googlesheets4)
library(glue)
library(collapse)
library(gt)

#TODO: REVIEW AND FUNCTIONALIZE

# PREP DATA AND METADATA --------------------------------------------------

  merdata <- glamr::si_path("path_msd")
  msd <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY20-22_20220211_v1_1_Zambia")

  authors <- c("Tim Essam")
  
  msd_source <- source_info(msd)
  
  curr_fy <- source_info(msd, return = "fiscal_year")
  curr_qtr <- source_info(msd, return = "quarter")
  curr_pd <- source_info(msd, return = "period")
  
  df <- read_msd(msd) 


# FUNCTIONS ---------------------------------------------------------------

  # snu1_rollups <- function(df, ...) {
  #   df %>% 
  #   bind_rows(df %>% mutate(snu1 = "PEPFAR")) %>% 
  #     group_by(fiscal_year, indicator, ...) %>% 
  #     summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
  #     reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>% 
  #     arrange(snu1, period) %>% 
  #     mutate(fy = substr(period, 3, 4)) %>% 
  #     group_by(snu1, fy) %>% 
  #     fill(targets, .direction = "down") %>% 
  #     filter(period %ni% c("FY20", "FY21", "FY22")) %>% 
  #     group_by(snu1, fy) %>% 
  #     mutate(cmlt_result = cumsum(results)) %>% 
  #     ungroup() %>% 
  #     mutate(achv = (cmlt_result/targets), 
  #            snu1 = str_remove_all(snu1, " Province"),
  #            snu1_order = fct_reorder(snu1, targets, .desc = T),
  #     )
  # }
  # 
  
  
  indic_collapse <- function(df, ...){
    df %>% 
      bind_rows(df %>% mutate(snu1 = "PEPFAR")) %>% 
      group_by(fiscal_year, indicator, ...) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T)
  }
  
  
  # Fills targets based on an extracted fiscal year
  # and custom groupings
  fill_targets <- function(df, ...){
    df %>% 
      mutate(fy = substr(period, 3, 4)) %>% 
      group_by(fy, ...) %>% 
      fill(targets, .direction = "down") %>% 
      filter(period %ni% c("FY20", "FY21", "FY22")) 
  }
  
  
  create_cmlt <- function(df, ...){
    df %>% 
      group_by(fy, snu1, ...) %>% 
      mutate(cmlt_result = cumsum(results)) %>% 
      ungroup() %>% 
      mutate(achv = (cmlt_result/targets), 
             snu1 = str_remove_all(snu1, " Province"),
             snu1_order = fct_reorder(snu1, targets, .desc = T),
      )
  }
  
  return_age_disag <- function(df, disag, indic) {
    df %>% 
      filter(indicator %in% {{indic}},
             standardizeddisaggregate == {{disag}},
             fiscal_year == curr_fy
      ) %>% 
      group_by(fiscal_year, indicator, ageasentered, sex, snu1) %>%
      summarise(across(matches("targ|cum"), sum, na.rm = TRUE)) %>% 
      filter(ageasentered %ni% c("<10", "<01")) %>% 
      mutate(achv = cumulative/targets,
             new_targ = ifelse(sex == "Female", targets, -targets),
             new_cmlt = ifelse(sex == "Female", cumulative, -cumulative),
             label_achv = ifelse(achv < 1, achv, NA_real_),
             fill_val = ifelse(sex == "Female", moody_blue, genoa),
             snu1 = str_remove_all(snu1, " Province")
      )
  }  
  
  return_bar_sort <- function(df) {
    df %>% 
      mutate(order_var = str_c(ageasentered, sex),
             achv_chunk = ifelse(achv >= 1, 1, 0)
      )  %>% 
      arrange(sex, ageasentered, desc(achv_chunk), cumulative) %>% 
      group_by(sex, ageasentered) %>% 
      mutate(sort_order = row_number(), 
             hline_var = case_when(
               lag(achv_chunk, n = 1) != achv_chunk ~ 1,
               TRUE ~ NA_real_
             )) %>% 
      ungroup() %>% 
      mutate(snu1_order = reorder_within(snu1, sort_order, order_var),
             newvar = case_when(
               hline_var == 1 ~ factor(snu1_order), 
               TRUE ~ snu1_order))
  }
  
  
 data_source <-  glue("{msd_source}\n Created by: {authors} | SI Team")
 
 cust_seq <- seq(0, 1, .25)[2:5]
 cust_label <- cust_seq %>% percent()

# HTS_TST -----------------------------------------------------------------

  # HTS_TST_POS Achievement by quarter
  df_hts <- 
    df %>% 
    filter(indicator == "HTS_TST_POS", 
           standardizeddisaggregate == "Total Numerator") 
 
 df_hts_viz <- 
   indic_collapse(df_hts, snu1) %>% 
   fill_targets(snu1) %>% 
   create_cmlt()
  
  df_hts_peds <- 
    df %>% 
    filter(indicator == "HTS_TST_POS", 
           trendscoarse < "15",
           standardizeddisaggregate == "Modality/Age/Sex/Result")
  
  df_hts_peds_viz <- 
    indic_collapse(df_hts_peds, snu1) %>% 
    fill_targets(snu1) %>% 
    create_cmlt()

  # VIZ - ACHV by SNU
  # Toggle from ALL TO PEDS
  df_hts_viz %>% 
    filter(snu1 == "PEPFAR") %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = targets), fill = trolley_grey_light) +
      geom_col(aes(y = cmlt_result, fill = case_when(
        snu1 == "PEPFAR" & fy == 20 ~ "#5B82D8",
        snu1 == "PEPFAR" & fy == 21 ~ "#5B82D8", 
        snu1 == "PEPFAR" & fy == 22 ~ "#074895",
        snu1 != "PEPFAR" & fy == 21 ~ "#228AA8",
        snu1 != "PEPFAR" & fy == 20 ~ "#74CCEC",
        TRUE ~ scooter)
          )
        ) +
     geom_errorbar(aes(ymin = targets, ymax = targets, color = ifelse(achv > 1, "white", "white")), size = 2) +
      geom_errorbar(aes(ymin = targets, ymax = targets, color = ifelse(achv > 1, old_rose, old_rose)), size = 1) +
      geom_label(data = . %>% filter(period %in% c("FY20Q4", "FY21Q4", "FY22Q1")),
                aes(y = cmlt_result, label = percent(achv, 1)), 
                    size = 12/.pt, family = "Source Sans Pro", vjust = 0.75) +
      facet_wrap(~snu1_order, scales = "free_y")+
      scale_color_identity() +
      scale_fill_identity() +
      si_style_ygrid(facet_space = 0.5, text_scale = 1.5) +
      scale_y_continuous(labels = comma) +
    labs(title = "HTS_TST_POS QUARTERLY ACHIEVEMENT",
         #subtitle = "Provinces sorted by HTS_TST_POS targets",
         caption = data_source, 
         x = NULL, y = NULL)
  si_save("Images/HTS_TST_POS_quarterly_achv_PEPFAR.png", height = 3.58, width = 9.67, scale = 1.5)
  si_save("Images/HTS_TST_POS_quarterly_achv_PEPFAR_peds.png", height = 3.58, width = 9.67, scale = 1.5)
  
  remove(df_hts_viz)

# HTS VOLUME BY POSITIVITY ------------------------------------------------

  df_hts_sex <- 
    df %>% 
    filter(indicator %in% c("HTS_TST_POS", "HTS_TST"),
           standardizeddisaggregate == "Modality/Age/Sex/Result")
  
 df_hts_sex <- 
   df_hts_sex %>% 
    group_by(fiscal_year, indicator, sex) %>% 
    summarise(across(matches("qtr"), sum, na.rm = T, .group = "drop")) %>% 
    reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>% 
   spread(indicator, results) %>% 
   mutate(positivity = HTS_TST_POS/HTS_TST) 
 
 
 df_hts_trend <- 
   df_hts_sex %>% 
   group_by(period) %>% 
   summarise(across(matches("HTS"), sum, na.rm = T, .group = "drop")) %>% 
   mutate(positivity = HTS_TST_POS/HTS_TST)
 
 
  top <-  df_hts_sex %>% 
   ggplot(aes(x = period, y = HTS_TST_POS)) +
   geom_bar(position = "dodge", stat = "identity", aes(fill = sex)) +
   scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa)) +
    labs(title = "HTS_TST_POS VOLUME BY SEX",
         subtitle = "Positivity shown as a percent by sex, across time",
         x = NULL, y = "Volume",
         fill = "") +
   #scale_fill_identity() +
   ggnewscale::new_scale_fill() +
   geom_label(aes(x = period, y = positivity*5e4, group = sex, label = percent(positivity, 1)), 
              position = position_dodge(width = 1), size = 11/.pt, family = "Source Sans Pro") +
   si_style_ygrid(text_scale = 1.25) +
   scale_y_continuous(labels = comma, expand = c(0.01, 0)) 
   
    
 # HTS by MODALITY\
 df_hts_mod <- 
   df %>% 
   filter(indicator %in% c("HTS_TST_POS"),
          standardizeddisaggregate == "Modality/Age/Sex/Result") 
 
 df_hts_mod_agg <- 
   df_hts_mod %>% 
    mutate(modality = case_when(
      str_detect(modality, "Index") ~ "Index",
     str_detect(modality, "(PITC|Inpat)") ~ "Other PITC",
    TRUE ~ "Other"
    )) %>% 
   group_by(fiscal_year, indicator, modality) %>% 
   summarise(across(matches("qtr"), sum, na.rm = T)) %>% 
   reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>%
   group_by(period) %>% 
   mutate(share = results / sum(results)) %>% arrange(period)
 
 
bottom <-  df_hts_mod_agg %>% 
    ungroup() %>% 
    mutate(modality = fct_relevel(modality, "Other", "Other PITC", "Index")) %>% 
    ggplot(aes(x = period, y = share, fill = modality)) +
    geom_bar(stat = "identity", alpha = 0.85, color = "white") +
    geom_text(aes(label = percent(share, 1)),
               position = position_stack(vjust = 0.5),
                                         size = 11/.pt, family = "Source Sans Pro") +
    scale_y_continuous(breaks = cust_seq, labels = cust_label) +
    scale_fill_manual(values = c("Index" = denim,  "Other PITC" = old_rose, "Other" = trolley_grey_light)) +
    si_style_ygrid(text_scale = 1.25) +
      coord_cartesian(expand = FALSE) +
   labs(title = "PEPFAR HTS_TST_POS MODALITY MIX ACROSS OVER TIME",
        subtitle = "Share of total positives by modality",
        caption = data_source, 
        x = NULL, y = "Modality % of Total",
        fill = "Modality Type")
   
  combo <- top / bottom 
  si_save("Images/HTS_TST_POS_combo_plot.png", scale = 1.5, height = 4.15, width = 9.67)

  remove(df_hts_sex, df_hts_trend)

# SAME AS ABOVE BUT SLIGHTLY DIFFEREN TOP ---------------------------------

  #  COMBO GRAPH - HTS_TST to HTS_TST_POS ON TOP / MODALITY BOTTOM ----------
  
  df_hts_combo <- 
    df %>% 
    filter(indicator %in% c("HTS_TST_POS", "HTS_TST"), 
           standardizeddisaggregate == "Total Numerator") 
  
  df_hts_combo <- 
    indic_collapse(df_hts_combo, snu1) %>% 
    fill_targets(snu1) %>% 
    filter(snu1 == "PEPFAR") %>% 
    select(-targets) %>% 
    spread(indicator, results) %>% 
    mutate(positivity = HTS_TST_POS/HTS_TST)
  
  hts_top <- df_hts_combo %>% 
    ggplot(aes(x = period)) +
    geom_col(aes(y = HTS_TST), fill = "#419FBE", width = 0.75) + 
    geom_col(aes(y = HTS_TST_POS), fill = "#004964", width = 0.75, 
             position = position_nudge(x = 0.1)) +
    geom_label(aes(y = (positivity * 1e6), label = percent(positivity, 1)),
               position = position_nudge(x = 0.1)) +
    si_style_ygrid(text_scale = 1.25) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         title = str_to_upper("PEPFAR Zambia continued to identify positives throughout the COVID-19 pandemic"),
         subtitle = "<span style = 'color:#419FBE;'><b> HTS_TST</b></span> compared to <span style = 'color:#004964;'><b>HTS_TST_POS</b></span>, positivity rate listed in white box") +
    theme(plot.subtitle = element_textbox_simple(family = "Source Sans Pro Light"))
  
  hts_top / bottom
  si_save("Images/HTS_TST_POS_combo_plot2.png", scale = 1.5, height = 4.15, width = 9.67)
  
  remove(df_hts_combo)

# FY22 Q1 TST SUMMARY -----------------------------------------------------

  df_hts_combo_snu1 <- 
    df %>% 
    filter(indicator %in% c("HTS_TST_POS", "HTS_TST"), 
           standardizeddisaggregate == "Total Numerator") 
  
  df_hts_combo_snu1 <- 
    indic_collapse(df_hts_combo_snu1, snu1) %>% 
    fill_targets(snu1) %>% 
    filter(period == curr_pd) %>% 
    mutate(achv = results/targets) %>% 
    ungroup() %>% 
    mutate(snu1 = str_remove_all(snu1, " Province"))
  
  # PEDS VERSION
  df_hts_peds <- 
    df %>% 
    filter(indicator %in% c("HTS_TST_POS", "HTS_TST"),
           trendscoarse < "15",
           standardizeddisaggregate == "Modality/Age/Sex/Result")
  
  df_hts_peds_viz <- 
    indic_collapse(df_hts_peds, snu1) %>% 
    fill_targets(snu1) %>% 
    create_cmlt(indicator) %>% 
    ungroup() %>% 
    filter(period == curr_pd) %>% 
    mutate(snu1 = str_remove_all(snu1, " Province"))
  

 b <-  df_hts_combo_snu1 %>% 
    filter(indicator == "HTS_TST_POS") %>% 
    mutate(snu1 = fct_reorder(snu1, achv),
           facet_var = ifelse(snu1 == "PEPFAR", "PEPFAR", "Province")) %>% 
    ggplot(aes(y = snu1)) +
    geom_col(aes(x = targets), fill = trolley_grey_light) +
    geom_col(aes(x = results), fill = genoa_light) +
    geom_text(aes(x = results, label = percent(achv, 1)), size = 10/.pt, family = "Source Sans Pro", hjust = -0.25)+
    facet_grid(facet_var~., scales = "free", space = "free", ) +
    scale_fill_identity() +
    si_style_xgrid(facet_space = 0.25, text_scale = 1.25) +
    scale_x_continuous(labels = comma) +
    theme(strip.text = element_blank()) +
    labs(y = NULL, x = NULL, title = "HTS_TST_POS FY22Q1 RESULTS TO TARGETS", 
         caption = data_source)
 
 a <- df_hts_combo_snu1 %>% 
   filter(indicator == "HTS_TST") %>% 
   mutate(snu1 = fct_reorder(snu1, achv),
          facet_var = ifelse(snu1 == "PEPFAR", "PEPFAR", "Province")) %>% 
   ggplot(aes(y = snu1)) +
   geom_col(aes(x = targets), fill = trolley_grey_light) +
   geom_col(aes(x = results), fill = genoa) +
   geom_text(aes(x = results, label = percent(achv, 1)), size = 10/.pt, family = "Source Sans Pro", hjust = -0.25)+
   facet_grid(facet_var~., scales = "free", space = "free", ) +
   scale_fill_identity() +
   si_style_xgrid(facet_space = 0.25, text_scale = 1.25) +
   scale_x_continuous(labels = comma) +
   scale_y_discrete(expand = c(0.1, 0))+
   theme(strip.text = element_blank()) +
   labs(y = NULL, x = NULL, title = "HTS_TST FY22Q1 RESULTS TO TARGETS", 
       )

  (a/b)
 si_save("Images/HTS_FY22Q1_summary_by_snu1.png", scale = 1.5, height = 4.4, width = 9.66)
 

# OFFSET BARS TESTING BY SNUS ---------------------------------------------

 df_peds_hts <- return_age_disag(df %>% filter(trendscoarse == "<15"), 
                                 disag = "Modality/Age/Sex/Result", 
                                 indic = c("HTS_TST", "HTS_TST_POS"))

 curr_fy <- 2021L
 df_5_14_hts <- return_age_disag(df %>% filter(ageasentered %in% c("05-09", "10-14")),  
                                 disag = "Modality/Age/Sex/Result", 
                                 indic = c("HTS_TST", "HTS_TST_POS"))
 
 df_hts_age_sex <- return_age_disag(df %>% filter(ageasentered %in% c("35-39", "40-44", "45-49", "50+")),
                                    disag = "Modality/Age/Sex/Result", 
                                    indic = c("HTS_TST", "HTS_TST_POS")) %>% 
   filter(ageasentered != "Unknown Age")
 
curr_fy <- 2021L
 df_ht_sex <- return_age_disag(df %>% mutate(ageasentered = trendscoarse),
                               disag = "Modality/Age/Sex/Result", 
                               indic = c("HTS_TST_POS")) 
 curr_fy <- source_info(msd, return = "fiscal_year")

 df_peds_hts
 
 
 # Custom sort on achievement + cumulative results
 # Change TITLE, INDICATOR, SAVE NAME, 
 df_bars <-  
    df_peds_hts %>% 
   ungroup() %>% 
   filter(indicator == "HTS_TST_POS",
          str_detect(snu1, "Military", negate = T),
          ageasentered != "Unknown Age") %>% 
   return_bar_sort()

 # Check if any disag combo has achieved targets
  Switch = !is.na(df_bars$hline_var) %>% sum()
 
 df_bars %>%
   ggplot(aes(y = snu1_order)) +
   geom_col(aes(x = targets), fill = grey20k, width = 0.3, position = position_nudge(y = -0.1)) +
   geom_col(aes(x = cumulative, fill = ifelse(achv_chunk == 1, genoa_light, "#cf7e8d")), width = 0.3) +
   geom_text(aes(label = percent(label_achv, 1), x = cumulative),
             size = 10/.pt, family = "Source Sans Pro", hjust = -0.2 ) +
   {if(Switch) geom_col(data = . %>% filter(hline_var == 1), 
            aes(y = snu1_order, x = Inf), width = 0.05, 
            position = position_nudge(y = -0.55), fill = grey30k)} +  
   facet_wrap(sex ~ ageasentered, scales = "free", 
              labeller = labeller(.multi_line = FALSE), nrow = 2) +
   scale_y_reordered() +
   scale_color_identity()+
   scale_fill_identity()+
   scale_x_continuous(labels = comma) +
   si_style_xgrid(facet_space = 1, text_scale = 1.25) +
   labs(x = NULL, y = NULL,
        #title = glue("HTS_TST_POS FOR FY21 BY PROVINCE"),
        caption = glue("Source: {msd_source}")) +
   theme(legend.position = "none") +
   coord_cartesian(expand = FALSE)
 
 si_save("Images/ZMB_province_HTS_TST_POS_FY21_ACHV_trendscoarse.png", scale = 1.5, height = 4.37, width = 9.54)
 

# HTS_INDEX_POS AS HTS_TST_POS --------------------------------------------

 df_index <- 
   df %>% 
   filter(indicator %in% c("HTS_INDEX_NEWPOS", "HTS_TST_POS"),
          standardizeddisaggregate == "Total Numerator")
 
 df_index <- indic_collapse(df_index, snu1) %>% 
   filter(!is.na(results)) %>% 
   spread(indicator, results) %>% 
   mutate(pct_pos = HTS_INDEX_NEWPOS/HTS_TST_POS)
 
 df_index_peds <- 
   df %>% 
   filter(indicator %in% c("HTS_INDEX_NEWPOS", "HTS_TST_POS"),
          standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/Result"),
          trendscoarse == "<15") 
 
 df_index_peds <- indic_collapse(df_index_peds, snu1) %>% 
   filter(!is.na(results)) %>% 
   spread(indicator, results) %>% 
   mutate(pct_pos = HTS_INDEX_NEWPOS/HTS_TST_POS)
 
 
 df_index %>% 
   filter(snu1 == "PEPFAR") %>% 
   ungroup() %>% 
   mutate(snu1 = fct_reorder(str_remove_all(snu1, " Province"), pct_pos, .fun = sum, .desc = T)) %>% 
   ggplot(aes(x = period, group = snu1)) +
   geom_area(aes(y = pct_pos), fill = scooter_med, alpha = 0.5)+
   geom_point(aes(y = pct_pos), color = scooter, size = 6) +
   geom_line(aes(y = pct_pos), color = scooter, size = 1)+
   geom_point(aes(y = pct_pos), color = "black", alpha = 0.85, stroke = 0.25 , size = 6,shape = 1) +
   geom_text(aes(y = pct_pos, label = percent(pct_pos, 1)), size = 12/.pt, family = "Source Sans Pro",
             vjust = -1.5, hjust = 0.2) +
   scale_y_continuous(position = "right", expand = c(0, 0.0), breaks = c(0, .25, .5), lim = c(0, 0.75)) +
   scale_x_discrete(expand = c(0.05, 0.05))+
   # scale_y_continuous(lim = c(0, 1)) +
   # scale_x_discrete(labels = c("FY20Q1", "", "", "",
   #                               "FY21Q1", "", "", "",
   #                               "FY22Q1")) +
   si_style(text_scale = 1.5) +
   labs(x = NULL, y = NULL,
        title = "PERCENT POSITIVIES VIA ALL INDEX TESTING AMONG PEDS",
        subtitle = "HTS_INDEX_NEWPOS / HTS_TST_POS",
        caption = data_source) +
   theme(axis.text.y =  element_blank()) +
   facet_wrap(~snu1)
 
 si_save("Images/ZMB_PCT_positives_via_index_PEDS.png", scale = 1.75, height = 3.96, width = 9.59)
   
   
   
  
# HTS NEEDED TO TEST ------------------------------------------------------
  #Number needed to test
 # HTS by MODALITY\
 df_hts_nnt <- 
   df %>% 
   filter(indicator %in% c("HTS_TST_POS", "HTS_TST"),
          standardizeddisaggregate == "Modality/Age/Sex/Result") 
   
 df_hts_nnt_peds <- 
   df %>% 
   filter(indicator %in% c("HTS_TST_POS", "HTS_TST"),
          standardizeddisaggregate == "Modality/Age/Sex/Result", 
          ageasentered %in% c("15-19", "20-24")) 
  
 
 # TOGGLE DF THE FEEDS INTO munging operations BELOW
 
 nnt_calc <- function(df, ...){
   df %>% 
   group_by(fiscal_year, indicator, modality, ...) %>% 
     summarise(across(matches("qtr"), sum, na.rm = T)) %>% 
     reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>%
     spread(indicator, results) %>% 
     mutate(NNT = ifelse(HTS_TST_POS >0, HTS_TST/HTS_TST_POS, NA_real_),
            `HTS Positivity` = 1/NNT) %>% 
     group_by(period) %>% 
     arrange(modality, period) %>% 
     filter(period == curr_pd) %>% 
     ungroup() %>% 
     mutate(modality = fct_reorder(modality, NNT))
 }
 
 df_viz <-  nnt_calc(df_hts_nnt_peds, ageasentered) %>% 
   filter(modality != "SNS")
 

   df_viz %>% 
   pivot_longer(HTS_TST:`HTS Positivity`) %>% 
   mutate(name = fct_relevel(name, c("NNT", "HTS Positivity", "HTS_TST", "HTS_TST_POS")),
          text_lab = case_when(
            name == "HTS Positivity" ~ percent(value, 1),
            TRUE ~ label_number_si()(value)
            )
          ) %>% 
   ggplot(aes(y = modality)) +
   geom_col(data = . %>% filter(ageasentered == "15-19"), 
            aes(x = value), 
            fill = "#afc9f0", 
            width = 0.25, 
            position = position_nudge(y = 0.125)) +
     geom_col(data = . %>% filter(ageasentered != "15-19"), 
              aes(x = value), 
              fill = "#f0b6af", 
              width = 0.25, 
              position = position_nudge(y = -0.125)) +
   geom_vline(xintercept = 0, size = 0.25, color = grey90k)+
   geom_text(data = . %>% filter(ageasentered == "15-19"), 
             aes(x = value, label = text_lab), 
             hjust = "inward", 
             size = 9/.pt, 
             family = "Source Sans Pro",
             position = position_nudge(y = 0.12), 
             color = denim) +
  geom_text(data = . %>% filter(ageasentered != "15-19"),
            aes(x = value, label = text_lab), 
            hjust = "inward", 
            size = 9/.pt, 
            family = "Source Sans Pro",
            position = position_nudge(y = -0.12), 
            color = old_rose)+
   facet_wrap(~name, nrow = 1, scales = "free_x", strip.position = "top") +
    scale_x_continuous(labels = comma) +
     si_style_xgrid(facet_space = 1, text_scale = 1.5) +
     labs(x = NULL, y = NULL,
          title = "FY22 Q1 NUMBER NEEDED TO TEST TO FIND ONE POSITIVE, AGES 15-19 & 20-24", 
          subtitle = "<span style = 'color:#2057a7;'><b> Ages 15-19 in blue </b></span>
          <span style = 'color:#c43d4d;'> | <b> Ages 20-24 in red</b></span>",
          caption = data_source) +
  theme(plot.subtitle = element_textbox_simple(family = "Source Sans Pro Light"),
        axis.text.x = element_blank(),
        strip.text=element_text(hjust = 0.05))
   
      si_save("Images/ZMB_NNT_modality_summary_peds.png", scale = 1.75, height = 4.28, width = 9.56)
   
   
   
 
 
 
 
 

 neg_offset <- -100
 sz = 12
 df_hts_nnt %>% 
   filter(period == curr_pd, modality != "SNS") %>% 
   mutate(modality = fct_reorder(modality, NNT)) %>% 
   ggplot(aes(y = modality)) +
   geom_col(aes(x = NNT), fill = scooter_med) +
   geom_point(aes(x = neg_offset, color = positivity), size = sz) +
   #geom_point(aes(x = neg_offset), size = sz, shape = 1, stroke = 0.25, color = "black", alpha = 0.8) +
   geom_text(aes(x = NNT, label = comma(NNT, 1)), hjust = -0.2, size = 9/.pt, 
             family = "Source Sans Pro") +
   scale_color_si(palette = "scooters") +
   ggnewscale::new_scale_color()+
   geom_text(aes(x = neg_offset, label = percent(positivity, 1), color = ifelse(positivity >.10, "white", "black")), size = 9/.pt, 
             family = "Source Sans Pro") +
   scale_color_identity() +
   scale_x_continuous(breaks = c(-600, -350, -100, 0, 500, 1000, 1500), labels = c("HTS_TST_POS", "HTS_TST", "Positivity", "", "500", "1000", "1500"), position = "top") +
   geom_point(aes(x = neg_offset - 250, fill = HTS_TST), shape = 22, size = sz) +
   scale_fill_si(palette  = "scooters") +
   geom_text(aes(x = neg_offset - 250, label = label_number_si()(HTS_TST), color = ifelse(HTS_TST > 50000, "white", "black")), 
             size = 9/.pt, 
             family = "Source Sans Pro") +
   ggnewscale::new_scale_fill() +
   geom_point(aes(x = neg_offset -500, fill = HTS_TST_POS), shape = 22, size = sz) +
   geom_text(aes(x = neg_offset - 500, label = label_number_si()(HTS_TST_POS), color = ifelse(HTS_TST_POS > 2000, "white", "black")), 
             size = 9/.pt, 
             family = "Source Sans Pro") +
   scale_fill_si(palette  = "denims") +
   si_style_nolines() 
  

# HTS DISAG ---------------------------------------------------------------
 # Hts pos (disagg, 15-19, 20-24 age bands, sex disagg m/f, province disagg) 
 # trends over time by quarter (most recent back 7 quarters) and then by modality:
 
 df_hts_agyw <- 
   df %>% 
   filter(indicator %in% c("HTS_TST_POS", "HTS_TST"),
          standardizeddisaggregate == "Modality/Age/Sex/Result",
          trendssemifine %in% c("15-19", "20-24")) 
 
 df_hts_agyw_viz <-
   indic_collapse(df_hts_agyw, snu1, sex, trendssemifine) %>% 
   fill_targets(snu1, indicator, sex, trendssemifine) %>% 
   create_cmlt(., indicator, sex, trendssemifine)
 
 df_hts_agyw_viz %>% 
    mutate(snu1 = fct_reorder(snu1, targets)) %>% 
   filter(indicator == "HTS_TST_POS", trendssemifine == "20-24", 
          snu1 != "PEPFAR") %>% 
   ggplot(aes(x = period)) +
   geom_col(aes(y = targets), fill = trolley_grey_light) +
   geom_col(aes(y = cmlt_result, 
                fill = case_when(
                   sex == "Female" & fy == 21 ~ "#877EC9",
                   sex == "Female" & fy == 22 ~ "#5A559B",
                   sex == "Male" & fy == 21 ~ "#459688",
                   TRUE ~ "#0D6C5F")
                ), 
            alpha = 0.85) +
   geom_errorbar(aes(ymax = targets, ymin = targets, color = ifelse(achv >1, "white", "black"), group = sex)) +
  facet_wrap(snu1 ~ sex, scales = "free_y") +
   si_style_yline(facet_space = 0.5) +
   scale_fill_identity() +
   scale_color_identity() +
   scale_y_continuous(labels = comma) +
   labs(x = NULL, y = NULL,
        title = "HTS_TST_POS DISAGGREGATES BY AGE, SEX & PROVINCE",
        caption = data_source)
    
   # PEDS VERSION
 
 df_hts_peds <- 
   df %>% 
   filter(indicator == "HTS_TST_POS", 
          trendscoarse < "15",
          standardizeddisaggregate == "Modality/Age/Sex/Result")

# TREAMTENT CASCASE -------------------------------------------------------

  cascade <- 
   df %>% filter(
     indicator %in% c("HTS_TST_POS", "HTS_TST", "TX_CURR", "TX_PVLS"),
     standardizeddisaggregate %in% c(
       "Age/Sex/HIVStatus",
       "Modality/Age/Sex/Result",
       "Age/Sex/Indication/HIVStatus") 
     ) %>% 
   mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
   group_by(trendscoarse, sex, snu1, indicator, fiscal_year) %>% 
   summarise(across(matches("qtr"), sum, na.rm = T)) %>% 
   reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T)
 

# INDEX TESTING ACROSS TIME -----------------------------------------------

 #aggregate modalities by quarter
 df_peds_ind <- df %>% 
   filter(indicator == "HTS_TST_POS",
          standardizeddisaggregate == "Modality/Age/Sex/Result",
          #trendscoarse == "<15"
   ) %>% count(modality)
   mutate(mod_type = case_when(
     str_detect(modality, "Index") ~ "Index",
     str_detect(modality, "VCT") ~ "VCT",
     TRUE ~ "All Other")
   ) %>% 
   group_by(fiscal_year, mod_type) %>%
   summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
   ungroup() %>% 
   reshape_msd(clean = TRUE)  %>% 
   select(-period_type) %>% 
   group_by(period) %>% 
   mutate(contribution = value/sum(value)) %>% 
   ungroup() %>% 
   mutate(start = case_when(period == min(period) ~ contribution),
          end = case_when(period == max(period) ~ contribution)) 

 df_peds_ind %>% 
   mutate(mod_type = fct_relevel(mod_type, c("All Other",  "VCT", "Index"))) %>% 
   ggplot(aes(period, value)) +
   geom_col(alpha = .2, width = 0.75) +
   geom_col(data = df_peds_ind %>% filter(mod_type == "VCT"), fill = moody_blue, width = 0.5,  position = position_nudge(x = 0.12)) +
   geom_col(data = df_peds_ind %>% filter(mod_type == "Index"), fill = burnt_sienna, width = 0.5, position = position_nudge(x = -0.12)) +
   labs(x = NULL, y = NULL,
        title = glue(""),
        caption = glue("Source: {msd_source}")) +
   theme(legend.position = "none") +
   scale_y_continuous(label = comma) +
   # scale_x_discrete(labels = c("FY19Q1", "", "", "",
   #                             "FY20Q1", "", "", "", 
   #                             "FY21Q1", "", "", "")) +
   si_style_ygrid(facet_space = 0.5) 
 si_save("Images/ZMB_peds_index_texting_modality_pos.png", scale = 1.25, height = 5, width = 10) 
 
 
 df_peds_ind %>% 
   mutate(mod_type = fct_relevel(mod_type, c("All Other",  "VCT", "Index"))) %>% 
   ggplot(aes(period, contribution , group = mod_type, color = mod_type)) +
   geom_line(size = 1.5) +
   geom_point(aes(y = start), size = 4, na.rm = TRUE) +
   geom_point(aes(y = end), shape = 21, stroke = 1.5,
              size = 4, na.rm = TRUE, fill = "white") +
   scale_color_manual(values = c("Index" = burnt_sienna, "VCT" = moody_blue, "Other" = grey50k)) +
   scale_fill_manual(values = c("Index" = burnt_sienna, "VCT" = moody_blue, "Other" = grey50k)) +
   geom_text(aes(y = end, label = mod_type), hjust = -0.25, size = 14/.pt) +
   expand_limits(y = 0) +
   scale_x_discrete(expand = c(0.12, 0.))+
   scale_y_continuous(label = percent_format(1), breaks = seq(0, 0.6, 0.2), limits = c(0, 0.6)) +
   si_style_ygrid(text_scale = 1.5) +
   labs(color = "Testing Modality") +
   labs(x = NULL, y = NULL, fill = NULL,
        title = glue("SHARE OF POSITIVE TESTS BY MODALITY FOR ALL AGES (0-50+)"),
        caption = glue("Source: {msd_source}")) +
   theme(legend.position = "none")
 si_save("Images/ZMB_ALL_index_texting_modality.png", scale = 1.25, height = 4.09, width = 8.53)
 
 
 # Table of Index Testing
 df_hts_modality <- df %>% 
   filter(indicator == "HTS_TST_POS",
          standardizeddisaggregate == "Modality/Age/Sex/Result",
   ) %>%
 mutate(mod_type = case_when(
   modality =="Index" ~ "Facility - Index",
   modality == "IndexMod" ~ "Community - Index",
   modality == "VCT" ~ "Facility - VCT",
   modality == "VCTMod" ~ "Community - VCT",
   str_detect(modality, "PITC") ~ "Other - PITC",
   str_detect(modality, "TBClinic") ~ "Facility - TB Clinic",
   str_detect(modality, "Inpat") ~ "Facility - Inpatient",
   str_detect(modality, "Pediatric") ~ "Facility - Pediatric",
   str_detect(modality, "MobileMod|OtherMod|SNSMod") ~ "Community - Other",
   TRUE ~ "Facility - Other")
 ) %>% 
   group_by(fiscal_year, mod_type) %>%
   summarise(across(starts_with("cumulative"), sum, na.rm = TRUE)) %>%
   ungroup() %>% 
   # ungroup() %>% 
   # reshape_msd(clean = TRUE)  %>% 
   # select(-period_type) %>% 
   group_by(fiscal_year) %>% 
   mutate(contribution = cumulative/sum(cumulative))
 
 df_hts_modality %>% 
   select(-cumulative) %>% 
   spread(fiscal_year, contribution) %>% 
   rename(`FY21 APR` = `2021`, `FY22 Q1`=`2022`,
          Modality = mod_type) %>% 
   arrange(desc(`FY22 Q1`)) %>% 
   gt() %>% 
   fmt_percent(columns = 2:4, decimals = 1) %>% 
   gt::tab_source_note(
     source_note = gt::md(glue::glue("**Source**: {msd_source}"))
   ) %>% 
   tab_header(
     title = glue::glue("Percent contribution to total positives identified (0-50+)")
   ) %>% 
   cols_label(
     Modality = md("**Modality**"),
     `FY21 APR` = md("**FY21 APR**"),
     `FY22 Q1` = md("**FY22 Q1**")
   ) %>% 
    cols_hide(2) %>% 
   tab_style(
     style = list(
       cell_fill(color = genoa_light, alpha = 0.5)
     ),
     locations = cells_body(
       columns = everything(),
       rows = 1:3
     )
   ) %>% 
   
   gtsave(., filename = "Images/HTS_POS_modality_0_50.png")
 
 
 # MDB TABLE ---------------------------------------------------------------
 
 # Time metadata needed  
 # USE FULL OU data set
 
 library(selfdestructin5)
 library(fontawesome)
 load_secrets()
 
 
 # Creates a markdown chunk to be inserted as a subtitle with legend pngs
 legend_chunk_q1 <- gt::md(glue::glue("Legend: Cumulative Indicators <img src= '{legend_q1}' style='height:15px;'>    &emsp; Snapshot (TX_CURR) <img src= '{legend_snapshot}' style='height:15px;'> "))
 
 # Bold Q3 data in the VLS/VLC table
 bold_column <- function(gt_obj, col){
   gt_obj %>% 
     tab_style(
       style = list(
         gt::cell_fill(color = "#e6e7e8", alpha = 0.5),
         gt::cell_text(weight = 700)
       ),
       locations = cells_body(
         columns = {{col}},
       )
     )
 }
 
 # Bold Agency names - used to increase stroke on row group label
 bold_rowgroup <- function(gt_obj){
   gt_obj %>% 
     tab_style(
       style = list(
         gt::cell_text(weight = 700)
       ),
       locations = cells_row_groups(groups = everything())
     )
 }
 
 # Embiggen font size
 embiggen <- function(gt_obj){
   gt_obj %>% 
     tab_options(
       source_notes.font.size = 10,
       table.font.size = 15,
       footnotes.font.size = 10)
 }
 
 pd <- create_pd(df)
 msd_source <- source_info(msd)
 mdb_out <- "Images"
 
 # Main Table
 mdb_df   <- make_mdb_df(df) %>% filter(indicator != "GEND_GBV")
 mdb_tbl  <- reshape_mdb_df(mdb_df, pd)  
 
 # Create the treatment data frame needed for derived indicators
 mdb_df_tx    <- make_mdb_tx_df(df)
 mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd)
 
 
 # Create and save tables for Zambia for uploading into google presentation
 create_mdb(mdb_tbl, ou = "Zambia", type = "main", pd, msd_source, legend = legend_chunk_q1) %>% 
   embiggen() %>% 
   bold_rowgroup(.) %>% 
   cols_hide(columns = matches("past")) %>% 
   gtsave(., path = mdb_out, filename = glue::glue("{pd}_Zambia_KEY_INDICATORS_MD.png"))
 
 create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", pd, msd_source) %>% 
   bold_column(., Q1) %>% 
   bold_rowgroup(.) %>% 
   embiggen() %>% 
   gtsave(., path = mdb_out, filename = glue::glue("{pd}_Zambia_MMD_VL_MD.png"))

# # PEDS PULLS ------------------------------------------------------------
# Trend analysis of (last 5-6 quarters?)
 
 # HTS TST 
 # HTS TST POS
 # TX CURR
 # NET NEW
 # VLC
 # VLS
 # 
 # by IP for 15-19 and then 20-24
 
 df %>% 
   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_PVLS"), 
          ageasentered %in% c("15-19", "20-24")) %>% 
   count(indicator, standardizeddisaggregate) %>% prinf()
 
 create_cmlt2 <- function(df, ...){
    df %>% 
       group_by(fy, ...) %>% 
       mutate(cmlt_result = case_when(
                    indicator %in% snapshot_ind ~ results,
                 TRUE ~ cumsum(results))
       ) %>% 
       ungroup() %>% 
       mutate(achv = (cmlt_result/targets), 
              mech_order = fct_reorder(paste0(mech_name, "-", mech_code), targets, .desc = T),
       )
 }
 
 df_mech_youth <- df %>% 
   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_PVLS"), 
          ageasentered %in% c("15-19", "20-24"),
          standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus"),
          mech_name != "Dedup") %>% 
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
    group_by(ageasentered, indicator, fiscal_year, mech_name, mech_code) %>% 
    summarise(across(matches("qtr|targ"), sum, na.rm = T)) %>% 
    reshape_msd(direction = "semi-wide") %>% 
    fill_targets(mech_name, mech_code, ageasentered, indicator)  %>% 
    create_cmlt2(mech_name, mech_code, ageasentered, indicator) 
 
 write_csv(df_mech_youth, "Dataout/ZMB_15_24_indicators_by_mech.csv")

   mech_trends <- function(fltr_var){
     df_mech_youth %>% 
           filter(indicator == fltr_var, 
                  period %in% c("FY20Q4", "FY21Q1", "FY21Q2", "FY21Q3", "FY21Q4", "FY22Q1")) %>% 
       ggplot(aes(x = period)) +
       geom_line(aes(y = results, group = ageasentered, color = ageasentered)) +
       geom_point(aes(y = results, group = ageasentered, color = ageasentered), shape = 19) +
       scale_color_manual(values = c("15-19" = golden_sand, "20-24" = denim)) +
       facet_wrap(~mech_order, labeller = as_labeller(label_wrap_gen(35))) +
       si_style(facet_space = 0.5) +
       scale_y_continuous(labels = comma) +
       labs(title = glue("{fltr_var} QUARTERLY RESULTS BY MECH"), color = "Age group")
   }
   mech_trends("HTS_TST_POS")
   
   mech_list <- df_mech_youth %>% distinct(indicator) %>% pull()        
   
   map(mech_list, ~mech_trends(.x) %>% 
          si_save(filename = glue("Images/{.x}_quarterly_results_by_mechanism.png"), scale = 1.5))
 
 
 
 
 
 
 
 
  
 df_mech_youth %>% 
    mutate(mech = paste0(mech_name, "-", mech_code)) %>% 
    filter(indicator == "HTS_TST") %>% 
    ggplot(aes(y = mech, x = period)) +
    geom_tile(aes(fill = achv), color =  "white") +
    geom_text(aes(label = percent(achv, 1), color = ifelse(achv > 1, "white", grey90k)), size = 8/.pt) +
    scale_fill_si(palette = "genoas", oob=scales::squish, limits = c(0, 2)) +
    facet_wrap(~ageasentered) +
    scale_color_identity() +
    si_style()
 
 
 

# CASCADE ----------------------------------------------------------------------
  df_cascade <- df %>% 
   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_NET_NEW", "TX_CURR", "TX_PVLS"),
          standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
          fiscal_year == curr_fy) %>%
   mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
   group_by(indicator, fiscal_year) %>% 
   summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
   reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>% 
   fill_targets() %>% 
   mutate(achv = ifelse(targets > 0, results/targets, NA_real_),
          indic_colors = case_when(
            indicator == "HTS_TST" ~ moody_blue_light,
            indicator == "HTS_TST_POS" ~ moody_blue,
            indicator == "TX_NEW" ~ "#535356",
            indicator == "TX_NET_NEW" ~ "#838484",
            indicator == "TX_CURR" ~ golden_sand, 
            indicator == "TX_PVLS_D" ~ scooter_med,
            indicator == "TX_PVLS" ~ scooter
            ),
          cascade = case_when(
            str_detect(indicator, "HTS") ~ "1st 90",
            str_detect(indicator, "TX_NE") ~ "2nd 90",
            TRUE ~ "3rd 90") 
          ) 
 
df_cascade <- 
  df_cascade %>% 
   bind_rows(df_cascade %>% filter(indicator == "HTS_TST_POS") %>% mutate(cascade = "2nd 90")) %>% 
   mutate(indicator = fct_relevel(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", 
                                               "TX_NET_NEW", "TX_CURR", "TX_PVLS_D",
                                               "TX_PVLS")))
 
 df_cascade %>% 
   ggplot(aes(x = indicator, fill = indic_colors)) +
   geom_col(aes(y = targets), fill = grey20k, width = 0.5) +
   geom_col(aes(y = results), width = 0.5, position = position_nudge(x = 0.1)) +
   geom_text(aes(y = results, label = comma(results)), size = 12/.pt, vjust = -0.45, 
             family = "Source Sans Pro", 
             position = position_nudge(x = 0.1)) +
   geom_label(aes(y = results, label = percent(achv, 1)), size = 9/.pt, vjust = 1.2, 
             family = "Source Sans Pro", 
             position = position_nudge(x = 0.1), fill = "white") +
   geom_text(data = . %>% filter(indicator == "HTS_TST"), aes(y = targets, label = "FY22 Targets"),
             size = 12/.pt, family = "Source Sans Pro", hjust = 0.4) +
   scale_y_continuous(labels = comma, expand = c(0.02, 1)) +
   scale_fill_identity() +
   facet_wrap(~cascade, scales = "free") +
   si_style_ygrid(text_scale = 1.25) +
   labs(x = NULL, y = NULL, title = "ZAMBIA CASCASE - FY22 Q1 RESULTS TO TARGETS (GRAY BARS)",
        subtitle = "FY22 Q1 results numbers listed above colored bar, achievement in box below",
        caption = data_source)
 
 si_save("Images/ZMB_FY22Q1_CASCADE.png", scale = 1.25, height = 3.94, width = 9.59)
 
 

#  RIGHT TO CARE ----------------------------------------------------------

df_mech <- 
   df %>% 
   filter(mech_code == 18304,
              indicator %in% c("HTS_TST_POS"),
              standardizeddisaggregate %in% "Modality/Age/Sex/Result", 
              fiscal_year == 2021, 
              str_detect(snu1, "Luapula"), 
              trendsfine != "Unknown Age") %>% 
   group_by(snu1, trendsfine, sex, indicator, fiscal_year, primepartner, psnu) %>% 
   summarise(across(matches("targ|cumu"), sum, na.rm = T)) %>% 
   mutate(achv = ifelse(targets > 0, cumulative / targets, NA_real_),
          gap = ifelse(targets > 0, targets - cumulative, NA_integer_),
          snu1 = str_remove_all(snu1, " Province")) %>% 
   group_by(snu1, sex, psnu) %>% 
   mutate(total_gap = sum(gap, na.rm = T), 
          total_targets = sum(targets)) %>% 
   ungroup()
 
 
 mech_name <- df_mech %>% distinct(primepartner) %>% pull()
 
 df_mech %>% 
   filter(sex == "Female") %>% 
   ggplot(aes(x = trendsfine)) +
   geom_col(aes(y = targets), fill = grey20k) +
   geom_col(aes(y = cumulative), fill = scooter) +
   geom_errorbar(aes(ymin = targets, ymax = targets, color = ifelse(achv>1, "white", grey90k))) +
   geom_text(aes(y = -100, label = percent(achv, 1)), size = 9/.pt, family = "Source Sans Pro") +
   facet_wrap(sex~paste0(psnu, " has a total results gap of ", comma(total_gap, 1)), labeller = as_labeller(label_wrap_gen(50))) +
   scale_color_identity() +
   scale_y_continuous(labels = comma) +
   si_style_ygrid(facet_space = 0.25) +
   labs(x = "Age bands", y = "", 
        title = glue("FY21 RESULTS TO TARGETS GAP SUMMARY FOR {mech_name}, DIVIDED BY FEMALES/MALES BY SELECT PROVINCES"),
        subtitle = "Results show in blue-green and targets in gray. \nPercent achievement shown between bars and age bands.",
        caption = data_source)
 si_save("Images/ZMB_right_to_care_summary_Luapula_females.png", scale = 1.75, height = 4.79, width = 10)
 

# CAPPED ACHV -------------------------------------------------------------

   #Show where gaps are in HTS_TST_POS ACHV using Adjusted Achv
   source("Scripts/00_maskless_achv_setup.R")
 
   df_achv <- df %>% 
    clean_agency() %>% 
    clean_psnu() %>% 
    filter(fundingagency != "DEDUP")

  df_achv_htspos <-
    df_achv %>% 
    extract_data(df_msd = ., 
                 operatingunit, snu1, psnu,
                 fy = 2021, 
                 ind = "HTS_TST_POS", 
                 disagg = "Total Numerator", 
                 agencies = c("USAID", "CDC")) %>% 
    filter(targets != 0) %>% 
    decompose_results(operatingunit) %>%
    calculate_gapshare(operatingunit) %>% 
    rollup_achv(operatingunit) %>% 
    mutate(grp_order = fct_reorder(paste(fundingagency, snu1, psnu, sep = "-"), results))

 indic <- df_achv_htspos %>% distinct(indicator) %>% pull()
 
 df_achv_htspos %>% 
     filter(facet_label == "Unachieved",
            achv > 0, targets > 200) %>% 
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
    facet_wrap(facet_label~., scales = "free_y", nrow = 2) +
    scale_x_continuous(labels = scales::label_number_si())+
    si_style_xgrid(facet_space = 0.25) +
    scale_color_identity()  +
    coord_cartesian(clip = "off", expand = TRUE) +
    labs(x = NULL, y = NULL, title = glue("AGENCY SHORTFALLS BY PSNU - {indic}"),
         subtitle = "Pink is shortfall gap, blue is results, gray is targets",
         caption = data_source)
 
 si_save(glue("Images/ZMB_{indic}_PSNU-AGENCY-HTS_SHORTFALLS_FY21.png"), scale = 1.25)
 
 

# TX_CURR PEDS ------------------------------------------------------------

  df %>% 
   filter(indicator == "TX_CURR",
          standardizeddisaggregate == "Age/Sex/HIVStatus",
          trendsfine %in% c("15-19", "20-24")) %>%
   group_by(sex, indicator, fiscal_year) %>%
   summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
   reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>% 
   fill_targets(sex) %>% 
   ungroup() %>% 
   mutate(age = "15-24", 
          fill_color = ifelse(sex == "Female", "#877EC9", "#459688"),
          achv = results/targets,
          facet_labels = ifelse(sex == "Female", "FEMALES 15-24", "MALES 15-24")) %>% 
   ggplot(aes(x = period)) +
   geom_col(aes(y = targets), fill = grey20k) +
   geom_col(aes(y = results, fill = fill_color)) +
   geom_hline(yintercept = seq(2e4, 8e4, 2e4), size = 0.1, color = "white", linetype = "dotted") +
   geom_errorbar(aes(ymin = targets, ymax = targets), linetype = "dotted",
                 size = 0.5, color = grey90k) +
   geom_label(aes(y = results, label = percent(achv, 1)), size = 9/.pt, 
              family = "Source Sans Pro", vjust = -0.1)+
   geom_text(aes(y = results, label = comma(results, 1)), size = 9/.pt, 
              family = "Source Sans Pro", vjust = 1.25)+
   facet_wrap(~facet_labels) +
   scale_fill_identity() +
   scale_x_discrete(labels = c("FY20Q1", "", "", "", 
                               "FY21Q1", "", "", "",
                               "FY22Q1")) +
   scale_y_continuous(position = "right", labels = comma) +
   si_style_nolines() +
   labs(x = NULL, y = NULL) +
   si_save("Images/ZMB_TX_CURR_15_24.svg", scale = 1.1)
 

# HTS TARGETS COP22 -------------------------------------------------------

 # HTS COP22 Targets to Previous years
 
 df <- read_excel("Data/HTS Achievements 02252022.xlsx", sheet = 1)
 
 df %>% 
   mutate(ymax = ifelse(indicator == "HTS_TST", 2.9e6, 2.25e5), 
          ymin = 0) %>% 
   mutate(facet_order = fct_relevel(indicator, "HTS_TST", 
                                    "HTS_TST_POS",
                                    "HTS_SELF")) %>% 
   ggplot(aes(x = period)) +
   geom_col(aes(y = targets), fill = grey30k, 
            width = 0.5, position = position_nudge(x = -0.125)) +
   geom_col(aes(y = results, fill = case_when(
     period == "FY21" ~ "#2D8073",
     TRUE ~ "#004137"
   )), 
   position = position_nudge(x = 0.125), 
   width = 0.5) +
   # geom_errorbar(aes(ymin = targets, ymax = targets, 
   #                   color = ifelse(targets < results, "white", grey90k)),
   #                   linesize = 0.25, width = 0.75,
   #               position = position_nudge(x = .25)) +
   geom_text(aes(y = results, label = percent(achv, 1)), 
             size = 10/.pt, family = "Source Sans Pro", vjust = -0.5,
             position = position_nudge(x = 0.125)) +
   geom_text(aes(y = targets, label = label_number_si(accuracy = 1.00)(targets)), 
             size = 10/.pt, family = "Source Sans Pro", vjust = -0.5,
             position = position_nudge(x = -0.125)) +
   facet_wrap(~facet_order) +
   scale_fill_identity() +
   geom_blank(aes(y = ymin)) +
   geom_blank(aes(y = ymax)) +
   scale_y_continuous(labels = label_number_si()) +
   scale_color_identity() +
   si_style_ygrid() +
   coord_cartesian(expand = FALSE) +
   labs(x = NULL, y = NULL,
        caption = "Source: COP22 Datapack & FY22Q1i MSD") +
   theme(axis.text.y = element_blank()) 
 si_save("Images/HTS_cop22_targets_same_axis.svg", scale = 1.25)
 
 

 