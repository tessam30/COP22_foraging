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
library(patchwork)
library(ggtext)
library(googlesheets4)
library(glue)
library(collapse)



# PREP DATA AND METADATA --------------------------------------------------

  merdata <- glamr::si_path("path_msd")
  msd <- return_latest(file.path(merdata, "Genie"), "Zambia-Daily-2022-02-18.zip*")

  authors <- c("Tim Essam")
  
  msd_source <- source_info(msd)
  
  curr_fy <- source_info(msd, return = "fiscal_year")
  curr_qtr <- source_info(msd, return = "quarter")
  
  df <- read_msd(msd) 


# FUNCTIONS ---------------------------------------------------------------

  snu1_rollups <- function(df, ...) {
    df %>% 
    bind_rows(df %>% mutate(snu1 = "PEPFAR")) %>% 
      group_by(fiscal_year, indicator, ...) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      reshape_msd(direction = "semi-wide", clean = T, qtrs_keep_cumulative = T) %>% 
      arrange(snu1, period) %>% 
      mutate(fy = substr(period, 3, 4)) %>% 
      group_by(snu1, fy) %>% 
      fill(targets, .direction = "down") %>% 
      filter(period %ni% c("FY21", "FY22")) %>% 
      group_by(snu1, fy) %>% 
      mutate(cmlt_result = cumsum(results)) %>% 
      ungroup() %>% 
      mutate(achv = (cmlt_result/targets), 
             snu1 = str_remove_all(snu1, " Province"),
             snu1_order = fct_reorder(snu1, targets, .desc = T),
      )
  }
  

# HTS_TST -----------------------------------------------------------------

  # HTS_TST_POS Achievement by quarter
  df_hts <- 
    df %>% 
    filter(indicator == "HTS_TST_POS", 
           standardizeddisaggregate == "Total Numerator") 
  
  df_hts_peds <- 
    df %>% 
    filter(indicator == "HTS_TST_POS", 
           trendscoarse < "15",
           standardizeddisaggregate == "Modality/Age/Sex/Result")
  
  
  
  df_hts_viz <- 
   snu1_rollups(df_hts, snu1)
  
  df_hts_peds_viz <- 
    snu1_rollups(df_hts_peds, snu1, trendscoarse)

    # VIZ - ACHV by SNU
  df_hts_peds_viz %>% 
    filter(snu1 == "PEPFAR") %>% 
      ggplot(aes(x = period)) +
      geom_col(aes(y = targets), fill = trolley_grey_light) +
      geom_col(aes(y = cmlt_result, fill = case_when(
        snu1 == "PEPFAR" & fy == 21 ~ "#436EC1", 
        snu1 == "PEPFAR" & fy == 22 ~ "#074895",
        snu1 != "PEPFAR" & fy == 21 ~ scooter_med,
        TRUE ~ scooter)
          )
        ) +
      geom_errorbar(aes(ymin = targets, ymax = targets, color = ifelse(achv > 1, "white", grey90k))) +
      geom_label(data = . %>% filter(period %in% c("FY21Q4", "FY22Q1")),
                aes(y = cmlt_result, label = percent(achv, 1)), 
                    size = 10/.pt, family = "Source Sans Pro", vjust = 0.75) +
      facet_wrap(~snu1_order, scales = "free_y")+
      scale_color_identity() +
      scale_fill_identity() +
      si_style_ygrid(facet_space = 0.5) +
      scale_y_continuous(labels = comma) +
    labs(title = "HTS_TST_POS QUARTERLY ACHIEVEMENT",
         subtitle = "Provinces sorted by HTS_TST_POS targets",
         caption = glue("{msd_source}\n Created by: {authors} | SI Team"))
  

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
   
   
   
   ggplot(aes(x = period, y = HTS_TST_POS, fill = ifelse(sex == "Male", genoa, moody_blue))) +
   geom_bar(position = "dodge", stat = "identity", ) +
   scale_fill_identity() +
   ggnewscale::new_scale_fill() +
   geom_label(aes(x = period, y = positivity*1e4, label = percent(positivity, 1)), 
              position = position_dodge(width = 1)) +
   si_style_ygrid()
 
 # HTS by MODALITY\
 df_hts_mod <- 
   df %>% 
   filter(indicator %in% c("HTS_TST_POS"),
          standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
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
 
 
  df_hts_mod %>% 
    ungroup() %>% 
    mutate(modality = fct_relevel(modality, "Other", "Other PITC", "Index")) %>% 
    ggplot(aes(x = period, y = share, fill = modality)) +
    geom_bar(stat = "identity", alpha = 0.85) +
    geom_text(aes(label = percent(share, 1)),
               position = position_stack(vjust =0.5)) +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = c("Other" = trolley_grey_light, "Other PITC" = old_rose, "Index" = denim)) +
    si_style_ygrid()
    

    
  
