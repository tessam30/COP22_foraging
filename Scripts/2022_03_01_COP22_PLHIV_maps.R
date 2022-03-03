# PURPOSE: Munge and Analysis of COP22 Spectrum Data to make PLHIV Incidence maps at PSNU
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-03-01
# NOTES: Request for Zambia SI team received on 2022-03-01

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(readxl)
    library(glue)
    library(ggdist)
    
  
  # Set paths
  data <- "Data"
  dataout <- "Dataout"
  images <- "Images"
  graphs <- "Graphics"

  merdata <- glamr::si_path("path_msd")
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim <- glamr::si_path("path_datim")

  # Excel data path and sheets info
  spectrum <- "Data/Spectrum Data dump_v3.xlsx"
  pop <- "Data/Adjusted District Populations using 2010 data 22-12-2021.xlsx"
  tx <- "Data/MoH December TX_CURR 2021.xlsx"

  # MSD Data for VLC
  msd <- return_latest(si_path(), pattern = "PSNU_IM_FY20-22_20220211_v1_1_Zambia")
  msd_source <- source_info(msd)

  # Functions
  # extract the fourth row of the sheets in the pop file
  # remove 1st and last column of df, reshape long
  extract_pop <- function(file = pop, sheet) {
    read_excel(pop, sheet = sheet, skip = 2, n_max = 2) %>%
      select(2:(ncol(.) - 1)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "psnu",
        values_to = "pop"
      )
  }

  # distinct psnus
  distinct_psnu <- function(df) {
    df %>%
      select(psnu) %>%
      distinct()
  }

  # Compare sets
  # df1 and df2 are the two datasets being compared, each has a psnu column
  compare_psnu <- function(df1, df2) {
    
    if (!"psnu" %in% names(df1,df2)) {
      cat("\nERROR - psnu column is not available as a column.\n")
      return(NULL)
    }
    
    xy <- setdiff(distinct_psnu(df1), distinct_psnu(df2)) %>% pull()
    yx <- setdiff(distinct_psnu(df2), distinct_psnu(df1)) %>% pull()

    print(glue("The difference bewteen 1 and 2 is:\n"), xy, "\n")
    print(glue("The difference bewteen 2 and 1 is:\n"), yx)
  }
    

# LOAD DATA ============================================================================  

    msd_df <- read_msd(msd)

    # SPECTRUM review of sheets
    excel_sheets(spectrum)

    # Review of updated pop estimate sheet
    sheet_list <- excel_sheets(pop)
    pop_est <- map_dfr(sheet_list, ~ extract_pop(pop, sheet = .x)) %>%
      mutate(
        psnu = str_to_title(psnu),
        psnu = case_when(
          psnu == "Kasenegwa" ~ "Kasenengwa",
          psnu == "Sambya" ~ "Samfya",
          TRUE ~ psnu
        )
      )

    # TX_CURR
    tx_df <- read_excel(tx, skip = 4) %>%
      select(
        psnu = 1,
        peds = 2,
        female = 3,
        male = 4,
        TX_CURR_moh = `...5`
      ) %>%
      filter(str_detect(psnu, "Province|Total", negate = T))
    
    # Pull in psnu shapefile
    # build terrain map to form base
    # terr <- get_raster(path = si_path('path_raster'))
    #
    # map_terr <- terrain_map(countries = 'Zambia',
    #                         mask = TRUE,
    #                         terr = terr)

    # Grab PSNU boundaries for Zambia
    shp_zmb <- st_read(file.path("../Zambezi/GIS/zambia_snu1.shp"))
    psnu_zmb <- st_read(file.path("..//Zambezi/GIS/zambia_psnu.shp"))

# MUNGE ============================================================================

  # VLC cacualtions - need TX_CURR_lag2 and TX_PVLS_D
  # Check disaggregates needed to calculate VLS/VLC
  msd_df %>%
    filter(indicator %in% c("TX_CURR", "TX_PVLS")) %>%
    count(standardizeddisaggregate, indicator, numeratordenom)

  vlc <- msd_df %>%
    filter(
      indicator %in% c("TX_CURR", "TX_PVLS"),
      standardizeddisaggregate %in% c("Age/Sex/Indication/HIVStatus", "Age/Sex/HIVStatus")
    ) %>%
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
    clean_psnu() %>%
    group_by(psnu, fiscal_year, indicator, psnuuid, snu1, snu1uid) %>%
    summarise(across(matches("qtr"), sum, na.rm = T)) %>%
    reshape_msd(direction = "semi-wide") %>%
    spread(indicator, results) %>%
    arrange(psnu, period) %>%
    group_by(psnu) %>%
    mutate(
      TX_CURR_lag2 = lag(TX_CURR, n = 2, order_by = period),
      VLC = TX_PVLS_D / TX_CURR_lag2,
      VLS = TX_PVLS / TX_PVLS_D
    ) %>%
    ungroup() %>%
    filter(period == "FY22Q1")

  vlc_map_df <- vlc %>%
    full_join(psnu_zmb, by = c("psnuuid" = "uid")) %>%
    mutate(psnu = case_when(
      is.na(psnu) ~ str_remove_all(orgunit, " District"),
      TRUE ~ psnu
    ))

  # SPECTRUM MUNGING

  plhiv_count <- read_excel(spectrum, sheet = "PLHIV District ", skip = 3) %>%
    rename(psnu = `Row Labels`) %>%
    select(Province:`Y065_999...14`) %>%
    rowwise() %>%
    mutate(tot_plhiv = sum(c_across(`Y000_014...3`:`Y065_999...14`))) %>%
    select(Province, psnu, tot_plhiv) %>%
    filter(!is.na(Province)) %>%
    mutate(
      psnu = str_to_title(psnu),
      psnu = case_when(
        psnu == "Senga Hill" ~ "Senga",
        psnu == "Shangombo" ~ "Shang'ombo",
        psnu == "Chikankanta" ~ "Chikankata",
        psnu == "Milengi" ~ "Milenge",
        TRUE ~ psnu
      )
    )
  
  plhiv_peds_count <- read_excel(spectrum, sheet = "PLHIV District ", skip = 3) %>% 
    rename(psnu = `Row Labels`) %>% 
    select(Province, psnu, plhiv_youth = 3) %>% 
    filter(!is.na(Province)) %>%
    mutate(
      psnu = str_to_title(psnu),
      psnu = case_when(
        psnu == "Senga Hill" ~ "Senga",
        psnu == "Shangombo" ~ "Shang'ombo",
        psnu == "Chikankanta" ~ "Chikankata",
        psnu == "Milengi" ~ "Milenge",
        psnu == "Chiengi" ~ "Chienge",
        psnu == "Itezhi-Tezhi" ~ "Itezhi-tezhi",
        psnu == "Kapiri Mposhi" ~ "Kapiri-Mposhi",
        psnu == "Mushindano" ~ "Mushindamo",
        TRUE ~ psnu
      )
    )

  # Check if psnu names are consistent
  compare_psnu(plhiv_count, pop_est)

  # Merge plhiv with pop est
  plhiv <- left_join(plhiv_count, pop_est, by = c("psnu")) %>%
    mutate(
      pct_hiv = tot_plhiv / pop,
      psnu = case_when(
        psnu == "Chiengi" ~ "Chienge",
        psnu == "Itezhi-Tezhi" ~ "Itezhi-tezhi",
        psnu == "Kapiri Mposhi" ~ "Kapiri-Mposhi",
        psnu == "Mushindano" ~ "Mushindamo",
        TRUE ~ psnu
      )
    )

  # Compare the TX_CURR data to plhiv data
  compare_psnu(plhiv, tx_df)

  tx_df_psnu <- tx_df %>%
    mutate(psnu = case_when(
      psnu == "Lavushi Manda" ~ "Lavushimanda",
      psnu == "Ikelengi" ~ "Ikelenge",
      psnu == "Shangombo" ~ "Shang'ombo",
      psnu == "Mushindano" ~ "Mushindamo",
      psnu == "Senga hill" ~ "Senga",
      TRUE ~ psnu
    ))



  # Combine all the dataframes into 1
  sds_maps <-
    left_join(vlc_map_df, plhiv) %>%
    left_join(., tx_df_psnu) %>%
    mutate(
      zmb_plhiv = sum(tot_plhiv, na.rm = T),
      zmb_pop = sum(pop, na.rm = T),
      zmb_pct_plhiv = zmb_plhiv / zmb_pop,
      zmb_plhiv_nlt_ave = zmb_plhiv / n(),
      VLC = ifelse(psnu == "Lunga", NA_real_, VLC),
      VLS = ifelse(psnu == "Lunga", NA_real_, VLS),
      zmb_tx_curr_lag2 = sum(TX_CURR_lag2, na.rm = T),
      zmb_TX_PVLS_D = sum(TX_PVLS_D, na.rm = T),
      zmb_VLC = zmb_TX_PVLS_D / zmb_tx_curr_lag2,
      coverage = TX_CURR_moh / tot_plhiv,
      zmb_tx_moh = sum(TX_CURR_moh, na.rm = T),
      zmb_coverage = zmb_tx_moh / zmb_plhiv,
      zmb_tx = sum(TX_CURR, na.rm = T),
      zmb_pepfar_cov = zmb_tx / zmb_plhiv
    )

  compare_psnu(plhiv_peds_count, vlc_map_df)
  
  peds_art <- left_join(vlc_map_df, plhiv_peds_count) %>% 
    left_join(., tx_df_psnu) %>% 
    mutate(peds_coverage = peds / plhiv_youth,
           zmb_peds_cov = sum(peds, na.rm = T) / sum(plhiv_youth, na.rm = T))

    

  # Create a psnu unioned border

  psnu_outline <- st_union(st_make_valid(st_set_precision(psnu_zmb, 1e7)))
  st_crs(psnu_outline)
  
  
# VIZ ============================================================================

  # Function to make a map from the combined data
  # df - data frame containing the combined PLHIV, TX_CURR and MOH TX_CURR
  # metric - One of the four metrics created for the 
  get_map <- function(df, metric) {
    ggplot() +
      geom_sf(data = psnu_outline, color = grey20k, size = 1.5) +
      geom_sf(data = df, aes(fill = {{ metric }}, geometry = geometry), color = "white") +
      si_legend_fill() +
      si_style_void() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank()
      )
  }

  get_hist <- function(df, metric, bins = 30, lines = 13) {
    df %>%
      filter(str_detect(psnu, "_Military", negate = T)) %>%
      ggplot(aes(x = {{ metric }}, fill = ..x..)) +
      geom_histogram(bins = bins, color = "white") +
      scale_y_continuous(expand = c(0.01, 0)) +
      theme(
        axis.text.y = element_blank(),
        legend.position = "none"
      ) +
      labs(x = NULL, y = NULL) +
      geom_hline(yintercept = seq(0, lines), size = 0.1, color = "White") +
      si_style_xline() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(size = 0.5, color = "#d3d3d3")
      )
  }
  
  
  # Maps needed
  # 1) PLHIV PCT by psnu
  # 2) PLHIV #s by psnu
  # 3) TX_CURR Coverage
  # 4) VLC


# PLHIV AS PCT ------------------------------------------------------------

 pct_plhiv <- get_map(sds_maps, metric = pct_hiv) +
    scale_fill_viridis_c(alpha = 0.75, labels = percent, direction = -1) 

 zmb_ave <- max(sds_maps$zmb_pct_plhiv)
 
 pct_hist <- get_hist(sds_maps, metric = pct_hiv) +
   scale_x_continuous(labels = percent, breaks = seq(0.0, 15, 0.025)) +
   scale_fill_viridis_c(alpha = 0.75, direction = -1, guide = "none" ) +
   geom_vline(xintercept = zmb_ave, size = 0.5, color = grey90k, linetype = "dotted") 
   
 # Inset
 pct_plhiv + labs(title ="TEST") +
   inset_element(pct_hist, left = 0, bottom = 0.75, right = 0.5, top = 1) 
 si_save("Graphics/plhiv_pct_inset.svg", scale = 1.25)
  
 # On top
 # pct_hist / pct_plhiv +
 #   plot_layout(heights = c(1, 4))
 # si_save("Graphics/plhiv_pct_stackt.svg", scale = 1.25)
 # 

# PLHIV TOTAL -------------------------------------------------------------

 plhiv_tot <- get_map(sds_maps, metric = tot_plhiv) +
   scale_fill_viridis_c(
     alpha = 0.75, trans = "log", option = "B",
     direction = -1,
     labels = comma,
     breaks = 10^(3:6)
   )

 plhiv_hist <- get_hist(sds_maps, metric = tot_plhiv) +
   scale_x_continuous(
     trans = "log", labels = comma,
     breaks = c(1000, 2500, 5000, 10000, 25000, 70000, 275000)
   ) +
   scale_fill_viridis_c(
     alpha = 0.75, option = "B",
     direction = -1,
     labels = comma,
     guide = "none"
   )

 plhiv_tot +
   inset_element(plhiv_hist, left = 0, bottom = 0.75, right = 0.5, top = 1)
 si_save("Graphics/plhiv_total_inset.svg", scale = 1.25)
 

# ART COVERAGE ------------------------------------------------------------

 zmb_cov_ave <- max(sds_maps$zmb_coverage)

 cov_map <- get_map(sds_maps, metric = coverage) +
   scale_fill_viridis_c(
     alpha = 0.75, option = "A", direction = -1, labels = percent,
     na.value = grey20k, guide = "none",
     limits = c(0, 1.3),
     oob = scales::squish
   )


 cov_hist <- get_hist(sds_maps, metric = coverage, lines = 13, bins = 40) +
   scale_x_continuous(
     labels = percent(seq(0, 1.3, 0.25), 1), breaks = seq(0, 1.3, 0.25),
     limit = c(0, 1.3)
   ) +
   geom_vline(xintercept = zmb_cov_ave, size = 0.5, color = grey90k, linetype = "dotted") +
   geom_vline(xintercept = 1, size = 1, color = grey30k) +
   scale_fill_viridis_c(
     alpha = 0.75, option = "A", direction = -1, labels = percent,
     na.value = grey20k, guide = "none",
     limit = c(0, 1.3),
     oob = scales::squish
   )

 cov_map +
   inset_element(cov_hist, left = 0, bottom = 0.75, right = 0.5, top = 1)
 si_save("Graphics/art_coverage_inset.svg", scale = 1.25)


# VIRAL LOAD COVERAGE -----------------------------------------------------

 # How many NA values
 sds_maps %>%
   filter(is.na(VLC)) %>%
   count()

 # Grab min and maxs
 zmb_vlc_ave <- max(sds_maps$zmb_VLC)
 min(sds_maps$VLC, na.rm = T)
 max(sds_maps$VLC, na.rm = T)
 vlc_min <- 0.4
 vlc_max <- 0.95


 vlc_map <- get_map(sds_maps, metric = VLC) +
   scale_fill_viridis_c(
     alpha = 0.75, option = "C", direction = -1, labels = percent,
     na.value = grey20k, guide = "none",
     limits = c(vlc_min, vlc_max)
   )

 vlc_hist <- get_hist(sds_maps, metric = VLC, bins = 40) +
   scale_x_continuous(
     labels = percent(seq(0, 1, 0.1), 1), breaks = seq(0, 1, 0.1),
     limits = c(vlc_min, vlc_max)
   ) +
   geom_vline(xintercept = zmb_vlc_ave, size = 0.5, color = grey90k, linetype = "dotted") +
   scale_fill_viridis_c(
     alpha = 0.75, option = "C", direction = -1, labels = percent,
     na.value = grey20k, guide = "none",
     limits = c(vlc_min, vlc_max)
   )

 vlc_map +
   inset_element(vlc_hist, left = 0, bottom = 0.75, right = 0.5, top = 1)
 si_save("Graphics/vlc_inset.svg", scale = 1.25)
 

# PEDS COVERAGE -----------------------------------------------------------

 peds_cov_ave <- max(peds_art$zmb_peds_cov)
 
 cov_peds_map <- get_map(peds_art, metric = peds_coverage) +
   scale_fill_viridis_c(
     alpha = 0.75, option = "E", direction = -1, labels = percent,
     na.value = grey20k, guide = "none",
     limits = c(0, 1.3),
     oob = scales::squish
   )
 
 
 cov_peds_hist <- get_hist(peds_art, metric = peds_coverage, lines = 13, bins = 40) +
   scale_x_continuous(
     labels = percent(seq(0, 1.3, 0.25), 1), breaks = seq(0, 1.3, 0.25),
     limit = c(0, 1.3)
   ) +
   geom_vline(xintercept = peds_cov_ave, size = 0.5, color = grey90k, linetype = "dotted") +
   scale_fill_viridis_c(
     alpha = 0.75, option = "E", direction = -1, labels = percent,
     na.value = grey20k, guide = "none",
     limit = c(0, 1.3),
     oob = scales::squish
   )
 
 cov_peds_map +
   inset_element(cov_peds_hist, left = 0, bottom = 0.75, right = 0.5, top = 1)
 si_save("Graphics/art_peds_coverage_inset.svg", scale = 1.25)

# SPINDOWN ============================================================================
  sds_maps %>% 
   select(-geometry) %>% 
   write_csv("Dataout/ZMB_COP22_PLHIV_maps.csv")

 st_write("GIS/ZMB_COP22_PLHIV_maps.shp", obj = sds_maps)  

    
    