library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(segmented)
library(shinycssloaders)
library(here)
# title logo image
# converts an image into a url for shiny::img()
title.image <- base64enc::dataURI(file="core.jpeg", mime="image/png")

# col width
col_width <- 12

# box width
rates_box_width <- 8

# age group list
ag_list <- list(
  binary_18 = c("all","0-18","19+"),
  Gershon = c("all","0-4","5-9","10-14","15-39","40-69","70+"),
  GINA = c("all","0-4","5-11","12-39","40-64","65+"),
  Larsen = c("all","0-4","5-9","10-14","15-64","65+")
)

# import the global rates data
df_global <- readr::read_rds(here::here('df_des.rds'))

# change underlying level names
for (i in 1:length(df_global)){
  df_global[[i]]$sex <- factor(df_global[[i]]$sex,levels = c("F","M"),c("Female","Male"))
  df_global[[i]]$age_group <- factor(df_global[[i]]$age_group,levels=ag_list[[i]][-1])
}

# import cp data
df_cp_global <- readr::read_rds(here::here('cp.rds')) %>% 
  lapply(.,function(df){
    df %>% 
      rename(`Age group`=Age.group ,
              `Segments (calendar year)`=`Segments.(calendar.year)`,
             `Trend (95% CI)`=`Trend.(95%.CI)`,
              `Relative change (%)`=`Relative.change.(%)`)
  })

gg_cp_global <- readr::read_rds(here::here("gg_cp_list.rds"))

# figure settings
# figure settings
min_year <- 2006 
max_year <- 2017
fig_dpi <- 900
tick_label_size <-10
label_size <- 13
ylab_rate <- "Rate of Asthma Hospital Admissions per 100,000"
theme_text_size <- theme(axis.text.x = element_text(size=tick_label_size),
                         axis.text.y = element_text(size=tick_label_size),
                         text = element_text(size=label_size))

theme_text_size2 <- theme(axis.text.x = element_text(size=8),
                         axis.text.y = element_text(size=8),
                         text = element_text(size=10))
