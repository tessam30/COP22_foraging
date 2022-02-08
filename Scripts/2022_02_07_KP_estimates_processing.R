# PURPOSE: Munge and Analysis of KP Projections
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-02-07
# NOTES: Process KP projects for Integration with MER data

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
    library(readxl)
    library(purrr)
    
  
  # Set paths  
    merdata <- glamr::si_path("path_msd")

  # NOTES
  # CREATED new headers in the files to make the reading in and munging easier for here
  # and team in Zambia
  

# LOAD DATA ============================================================================  

  file_list <- list.files("Data", pattern = "Zambia_FSW|MSM|TG", full.names = T)
  map(file_list, ~excel_sheets(.x))
  skip = 3
    
  fsw <- map2_dfr(.x = file_list[-2], .y = list("fsw", "tg"),
                  .f = ~read_excel(.x, skip = skip, sheet = 1) %>% 
                    mutate(pop = .y) %>% 
                    filter(Province != "Total")) %>% 
    mutate(across(matches("_"), as.numeric))
  
  msm <- map2_dfr(.x = file_list[2], .y = list(1, 3), 
                  ~read_excel(.x, skip = skip, sheet = .y) %>% 
                    mutate(pop = "msm", sheet = .y) %>% 
                    filter(Province != "Total")) %>% 
    mutate(across(matches("_"), as.numeric))
  
  
  # Fetch geographic info
  msd <- return_latest(merdata, "PSNU_IM_FY19-22.*\\Zambia.zip")
  psnu_list <- read_msd(msd) %>% 
    distinct(psnu, psnuuid, snu1) %>% 
    filter(psnuuid != "?", 
           str_detect(psnu, "Militar", negate = T)) %>% 
    clean_psnu()

# MUNGE ============================================================================
  
  #  Combine all populations together
  kp_est <- bind_rows(msm, fsw) %>% 
    rename(snu1 = Province,
           psnu = District) %>% 
    mutate(pop = case_when(
      pop == "msm" & sheet == 1 ~ "msm simple",
      pop == "msm" & sheet == 3 ~ "msm stratified",
      TRUE ~ pop
      ),
      pop_fct = fct_relevel(pop, c("tg", "fsw", "msm simple", "msm stratified")),
      psnu = str_replace_all(psnu, "\\*", "")
    ) 
  
  setdiff(kp_est %>% distinct(psnu) %>% arrange(psnu) %>% pull(), psnu_list %>% distinct(psnu) %>% pull())
  setdiff(psnu_list %>% distinct(psnu) %>% arrange(psnu) %>%  pull(), kp_est %>% distinct(psnu) %>% pull())

  
# VIZ ============================================================================

  # What does a basic SNU / PSNU breakdown look like?
  plot_kp <- function(estimate  = predicted_15_49){
    kp_est %>% 
    mutate(psnu_order = reorder_within(psnu, {{estimate}}, snu1)) %>% 
    ggplot(aes(x = {{estimate}}, y = psnu_order, color = pop_fct)) +
    geom_point(size = 3, alpha = 0.75) +
    geom_point(shape = 1, color = "black", size = 3, stroke = 0.8) +
    facet_wrap(~snu1, scales = "free") +
    scale_y_reordered() +
    si_style(facet_space = 0.5) +
    scale_color_manual(values = c("tg" = denim, "fsw" = genoa, "msm simple" = burnt_sienna, "msm stratified" = golden_sand)) +
    scale_x_continuous(labels = comma) +
    labs(color = "Key Population") 
  }
  

# SPINDOWN ============================================================================

