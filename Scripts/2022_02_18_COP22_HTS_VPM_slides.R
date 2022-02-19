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



# PREP DATA AND METADATA --------------------------------------------------

  merdata <- glamr::si_path("path_msd")
  msd <- return_latest(folderpath = merdata, pattern = "20220211_v1_1_Zambia")

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
      ungroup() %>% 
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
        snu1 == "PEPFAR" & fy == 21 ~ "#436EC1", 
        snu1 == "PEPFAR" & fy == 22 ~ "#074895",
        snu1 != "PEPFAR" & fy == 21 ~ "#228AA8",
        snu1 != "PEPFAR" & fy == 20 ~ "#74CCEC",
        TRUE ~ scooter)
          )
        ) +
      geom_errorbar(aes(ymin = targets, ymax = targets, color = ifelse(achv > 1, "white", grey90k))) +
      geom_label(data = . %>% filter(period %in% c("FY20Q4", "FY21Q4", "FY22Q1")),
                aes(y = cmlt_result, label = percent(achv, 1)), 
                    size = 12/.pt, family = "Source Sans Pro", vjust = 0.75) +
      facet_wrap(~snu1_order, scales = "free_y")+
      scale_color_identity() +
      scale_fill_identity() +
      si_style_ygrid(facet_space = 0.5, text_scale = 1.25) +
      scale_y_continuous(labels = comma) +
    labs(title = "HTS_TST_POS QUARTERLY ACHIEVEMENT <15s",
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
    summarise(across(matches("qtr"), sum, na.rm = T)) %>% 
    reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>% 
   spread(indicator, results) %>% 
   mutate(positivity = HTS_TST_POS/HTS_TST) 
 
 
 
 df_hts_trend <- 
   df_hts_sex %>% 
   group_by(period) %>% 
   summarise(across(matches("HTS"), sum, na.rm = T)) %>% 
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
  

 b <-  df_hts_peds_viz %>% 
    filter(indicator == "HTS_TST_POS") %>% 
    mutate(snu1 = fct_reorder(snu1, results),
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
    labs(y = NULL, x = NULL, title = "HTS_TST_POS FY22Q1 RESULTS TO TARGETS PEDS (<15)", 
         caption = data_source)
 
 a <- df_hts_peds_viz %>% 
   filter(indicator == "HTS_TST") %>% 
   mutate(snu1 = fct_reorder(snu1, results),
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
   labs(y = NULL, x = NULL, title = "HTS_TST FY22Q1 RESULTS TO TARGETS PEDS (<15)", 
       )

  (a/b)
 si_save("Images/HTS_FY22Q1_summary_by_snu1_UNDER15s.png", scale = 1.5, height = 4.15, width = 9.67)
 

# OFFSET BARS TESTING BY SNUS ---------------------------------------------

 df_peds_hts <- return_age_disag(df %>% filter(trendscoarse == "<15"), 
                                 disag = "Modality/Age/Sex/Result", 
                                 indic = c("HTS_TST", "HTS_TST_POS"), 
                                 )
 
 df_hts_age_sex <- return_age_disag(df %>% filter(ageasentered %in% c("35-39", "40-44", "45-49", "50+")),
                                    disag = "Modality/Age/Sex/Result", 
                                    indic = c("HTS_TST", "HTS_TST_POS")) %>% 
   filter(ageasentered != "Unknown Age")
 
  curr_fy <- 2021L
 df_ht_sex <- return_age_disag(df %>% mutate(ageasentered = trendscoarse),
                               disag = "Modality/Age/Sex/Result", 
                               indic = c("HTS_TST_POS")) 
 curr_fy <- source_info(msd, return = "fiscal_year")

 
 
 # Custom sort on achievement + cumulative results
 # Change TITLE, INDICATOR, SAVE NAME, 
 df_bars <-  
   df_ht_sex %>% 
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
        title = glue("HTS_TST_POS FOR FY21 BY PROVINCE"),
        caption = glue("Source: {msd_source}")) +
   theme(legend.position = "none") +
   coord_cartesian(expand = FALSE)
 
 si_save("Images/ZMB_province_HTS_TST_POS_FY21_ACHV_trendscoarse.png", scale = 1.75, height = 3.96, width = 9.59)
 
 
  
# HTS NEEDED TO TEST ------------------------------------------------------
  #Number needed to test
 # HTS by MODALITY\
 df_hts_nnt <- 
   df %>% 
   filter(indicator %in% c("HTS_TST_POS", "HTS_TST"),
          standardizeddisaggregate == "Modality/Age/Sex/Result", 
          trendsfine %in% c("15-19", "20-24", "25-29")) %>%
   group_by(fiscal_year, indicator, modality) %>% 
   summarise(across(matches("qtr"), sum, na.rm = T)) %>% 
   reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>%
   spread(indicator, results) %>% 
   mutate(NNT = ifelse(HTS_TST_POS >0, HTS_TST/HTS_TST_POS, NA_real_),
          positivity = 1/NNT) %>% 
    group_by(period) %>% 
   arrange(modality, period)

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
   
