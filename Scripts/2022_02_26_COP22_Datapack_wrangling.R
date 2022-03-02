# PURPOSE: Munge COP22 Data pack
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-02-25
# NOTES: Data pack munging

# LOCALS & SETUP ============================================================================

# Libraries
library(tameDP)
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
library(readxl)

# LOAD data

  path <- "../../../Downloads/2022-02-24 Data Pack_Zambia_For OGAC review.xlsx"
  path2 <- "../../../Downloads/2021-06-04 Datapack_20210603_134748.xlsx"
  

  msd <- return_latest(si_path(), pattern = "PSNU_IM_FY20-22_20220211_v1_1_Zambia")
  msd_df <- read_msd(msd)
  
  df_dp <- tame_dp(path)
  df_dp2 <- tame_dp(path2)
  

# return previous fiscal year results and targets by different dis --------

  get_msd_disags <- function(df, indics, disags, ...){
    df %>% 
      filter(indicator %in% indics,
             standardizeddisaggregate %in% disags) %>% 
      group_by(indicator, fiscal_year, ...) %>% 
      summarise(across(matches("targ|cumu"), sum, na.rm = T)) %>% 
      ungroup()
  }
  
  get_dp_disags <- function(df, indics, ...){
    df %>% 
      filter(indicator %in% indics, fiscal_year == 2023,
             str_detect(standardizeddisaggregate, "KeyPop", negate = T)) %>% 
      group_by(indicator, fiscal_year, ...) %>% 
      summarize(targets = sum(targets, na.rm = T), .groups = "drop")
  }
  
  plot_targets <- function(df, ind, snu) {
    
    snu_label <- snu
    ind_label <- ind
    
    df %>%  
      complete(psnu, indicator, fiscal_year) %>%
      clean_psnu %>% 
      mutate(snu1 = str_remove_all(snu1, " Province"),
             sort_value = ifelse(fiscal_year == 2023, targets, NA_integer_)) %>% 
      group_by(indicator, psnu) %>% 
      fill(sort_value, .direction = "up") %>% 
      ungroup() %>% 
      filter(indicator == ind, snu1 == snu) %>% 
      mutate(psnu_order = fct_reorder(psnu, sort_value, .desc = T))  %>%
      group_by(indicator, psnu) %>% 
      mutate(delta = ifelse(targets > lag(targets, order_by = fiscal_year), "up", "down"),
             change = (targets - lag(targets, order_by = fiscal_year)),
             pct_chg = ((targets - lag(targets))/lag(targets)),
             fill_color = ifelse(delta == "up", "#228aa8", "#a84022")) %>% 
      ungroup() %>% 
      ggplot(aes(y = factor(fiscal_year))) +
      geom_col(aes(x = targets, 
                   fill = case_when(
                     fiscal_year == 2023 ~ fill_color, 
                     TRUE ~ grey20k
                   )), width = 0.5) +
      geom_col(aes(x = cumulative), 
               width = 0.25, fill = genoa, alpha = 0.75, 
               position = position_nudge(y = -0.2),
            ) +
            geom_label(data = . %>% filter(fiscal_year == 2023, 
                                    indicator == ind, 
                                    snu1 == snu), 
                aes(x = targets, 
                    label = percent(pct_chg, 1)), 
                size = 9/.pt, 
                family = "Source Sans Pro", 
                color = grey70k, 
                hjust = "inward", 
                label.size = NA, 
                alpha = 0.75) +
      facet_wrap(~psnu_order, 
                 labeller = labeller(.multi_line = FALSE)) +
      si_style_xgrid(facet_space = 1) +
      scale_x_continuous(labels = label_number_si()) +
      scale_fill_identity() +
      labs(x = "FY Targets", y = NULL,
           title = glue("{snu} Province targets for {ind} by PSNU"),
           subtitle = 
           "Blue (brown) bars indicate increase (decrease) in targets from previous COP.\nPercent is the the delta from previous years' targets.\nGreen bars show cumulative results for previous years.\nGray bars are targets." )
    
    si_save(glue("Images/COP22_targets/{ind}_for_{snu}.png"), scale = 1.25)
  }
  
# TESTING TARGETS ---------------------------------------------------------

  # Grab targets/results for FY20 and FY21
  tst_indic <- c("HTS_INDEX", "HTS_RECENT", "HTS_SELF", "HTS_TST", "HTS_TST_POS")

  hts_msd <- get_msd_disags(msd_df, tst_indic, disags = "Total Numerator")
  hts_msd_psnu <- get_msd_disags(msd_df, tst_indic, disags = "Total Numerator", 
                                 psnu, snu1)
    
  dp_hts_psnu <- get_dp_disags(df_dp, tst_indic, psnu, snu1)
  
  hts_psnu <- bind_rows(hts_msd_psnu, dp_hts_psnu) %>% arrange(indicator, fiscal_year)
 

# TX_CURR Targets ---------------------------------------------------------

  tx_indic <- c("TX_CURR", "TX_NEW")
  
  tx_msd_psnu <- get_msd_disags(msd_df, tx_indic, disags = "Total Numerator", psnu, snu1)
  dp_tx_psnu <- get_dp_disags(df_dp, tx_indic, psnu, snu1)
  tx_psnu <- bind_rows(tx_msd_psnu, dp_tx_psnu)
  
  tx_psnu %>% 
    filter(indicator == "TX_CURR") %>% 
    bind_rows(tx_psnu %>% filter(indicator == "TX_CURR") %>% 
                mutate(snu1 = " PEPFAR")) %>% 
    group_by(indicator, fiscal_year, snu1) %>% 
    summarise(across(matches("tar|cumu"), sum, na.rm = T)) %>%
    ungroup() %>% 
    mutate(ymax = ifelse(snu1 == "ALL PEPFAR", 1.6e6, 3.5e5)) %>% 
    arrange(snu1, fiscal_year) %>%
    group_by(snu1, indicator) %>% 
    mutate(delta = targets - lag(targets, order_by = fiscal_year),
           pct_chg = (targets-lag(targets, order_by = fiscal_year))/lag(targets),
           fill_color = ifelse(pct_chg >= 0, "#228aa8", "#a84022")) %>%
    ungroup() %>% 
    mutate(snu1 = fct_reorder(str_remove_all(snu1, " Province"), targets, .desc = T)) %>% 
      ggplot(aes(x = fiscal_year)) +
    geom_col(aes(y = targets, fill = ifelse(fiscal_year == 2023, fill_color, grey30k)),
             width = 0.5) +
    geom_col(aes(y = cumulative), fill = genoa, alpha = 0.75, width = 0.25, 
             position = position_nudge(x = 0.125)) +
    geom_blank(aes(y = ymax)) +
    geom_label(aes(y = ifelse(targets > cumulative, targets, cumulative), 
                  label = comma(targets, 1)), size = 8/.pt, 
              family = "Source Sans Pro", vjust = "outward", alpha = 0.75,
              label.size = NA,)+
    facet_wrap(~snu1, labeller = labeller(.multi_line = FALSE), scales = "free_y")+
    scale_y_continuous(labels = comma, expand = c(0.1, 0)) +
    si_style(facet_space = 0.5)+
    scale_fill_identity() +
    labs(x = "FY Targets", y = NULL,
         title = glue("SNU targets for TX_CURR"),subtitle = "Blue (brown) bars indicate increase (decrease) in targets from previous COP")
    
  



  plot_targets(tx_psnu, "TX_CURR", "Southern")
  
  snus <- hts_psnu %>% 
          distinct(snu1) %>% 
          filter(str_detect(snu1, "_Military", negate = T)) %>% 
          mutate(snu1 = str_remove_all(snu1, " Province")) %>% pull()
  
  walk(snus, ~plot_targets(hts_psnu, "HTS_TST_POS", .x))
  walk(snus, ~plot_targets(hts_psnu, "HTS_TST", .x))
  walk(snus, ~plot_targets(hts_psnu, "HTS_SELF", .x))
  walk(snus, ~plot_targets(hts_psnu, "HTS_INDEX", .x))
  
  walk(snus, ~plot_targets(tx_psnu, "TX_CURR", .x))
  walk(snus, ~plot_targets(tx_psnu, "TX_NEW", .x))


dp_tx <- df_dp %>% 
  filter(str_detect(indicator, "TX_CURR"), fiscal_year == 2023,
         str_detect(standardizeddisaggregate, "KeyPop", negate = T)) %>% 
  group_by(indicator) %>% 
  summarize(targets = sum(targets, na.rm = T), .groups = "drop")  


# PLHIV
tmp1 <- df_dp %>% 
  mutate(ageasentered = case_when(
    ageasentered %in% c("55-59", "60-64", "65+") ~ "50+",
    TRUE ~ ageasentered
  )) %>%
  group_by(ageasentered, indicator, sex) %>% 
  summarize(targets = sum(targets, na.rm = T)) %>% 
  spread(indicator, targets)


tmp2 <- df_dp2 %>% 
  group_by(indicator, ageasentered, sex) %>% 
  summarize(targets = sum(targets, na.rm = T)) %>% 
  spread(indicator, targets)

tmp1 %>% 
  ggplot(aes(x = sex)) +
  geom_col(data = tmp2, aes(y = POP_EST), fill = genoa_light,
           position = position_nudge(x = 0.2),
           width = 0.25) +
  geom_col(aes(y = POP_EST), width = 0.25, fill = genoa,
           position = position_nudge(x = 0.3)) +
  geom_col(data = tmp2, aes(y = DIAGNOSED_SUBNAT), 
           position = position_nudge(x = -0.2), width = 0.25,
           fill = golden_sand_light) +
  geom_col(aes(y = DIAGNOSED_SUBNAT), 
           position = position_nudge(x = -0.1), width = 0.25,
           fill = golden_sand) +
  facet_wrap(~ageasentered, scales = "free")





