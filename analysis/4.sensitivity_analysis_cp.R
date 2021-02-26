# import packages for data manipulation -----------------------------------
library(tidyverse)
library(here)

# sensitivity analysis for binary
df_annual_master <- read_rds(here("results",'binary_18','rates.rds'))[[1]] %>% 
  filter(Year <= 2017) %>% 
  filter(province=="CA") %>% 
  filter(sex!='B' & province=='CA') %>% 
  mutate(Date=Year) %>% 
  select(-n)

df_annual <- df_annual_master %>%
  filter(province=="CA" &
           sex == "M" &
           age_group == '0-18') %>% 
  mutate(Year=Year-min(Year))


# package for bayesian change point analysis
library(mcp)
# pkg for model comparison
library(loo)

# list of models
# the first num indicates the num of changes in the intercept
# the second num indicates the num of changes in the slope
list_mcp_models <-
  list(
    zero_zero = list(rate ~ 1 + Year),
    one_zero = list(rate ~ 1 + Year,
               ~ 1), 
    zero_one = list(rate ~ 1 + Year,
                   ~ 0 + Year),
    one_one = list(rate ~ 1+ Year,
               ~ 1 + Year),
    two_zero = list(rate ~ 1 + Year,
                    ~ 1,
                    ~ 1),
    zero_two = list(rate ~ 1 + Year,
                    ~ 0 + Year,
                    ~ 0 + Year),
    two_one = list(rate ~ 1+ Year,
                   ~1 + Year,
                   ~1 ),
    one_two = list(rate ~ 1+ Year,
                   ~1 + Year,
                   ~0 + Year),
    two_two = list(rate ~ 1 + Year,
                   ~ 1 + Year,
                   ~ 1 + Year),
    three_zero = list(rate ~ 1 + Year,
                   ~ 1 ,
                   ~ 1 ,
                   ~ 1),
    zero_three = list(rate ~ 1 + Year,
                      ~ 0 + Year ,
                      ~ 0 + Year  ,
                      ~ 0 + Year )
    )

# number of sex-age groups
df_annual_levels <- df_annual_master %>% 
  distinct(province,sex,age_group) %>% 
  select(province,sex,age_group) %>% 
  mutate(index = row_number())

# empty list for storing
loo_comparison_list <- c()
mcp_result_list <- c()

# use 5000 burn-in, 5000 samples, and 50 chains
for(lvl in 1:nrow(df_annual_levels)){
  tmp_level <- df_annual_levels[lvl,]
  tmp_prov <- tmp_level$province
  tmp_sex <- tmp_level$sex
  tmp_age_group <- tmp_level$age_group
  
  df_annual <- df_annual_master %>%
    filter(province==tmp_prov &
             sex == tmp_sex &
             age_group == tmp_age_group) %>% 
    mutate(Year = Year - min(Year))
  
  mcp_result <- lapply(list_mcp_models,function(mod){
    mcp(mod,data=df_annual,sample='both',adapt=5000,iter=5000,chains=50,cores=3)
  })
  
  # compute loo
  mcp_loo <- lapply(mcp_result,function(result){
    loo::loo(result)
  })
  
  # compare & save results
  loo_comparison_list[[lvl]] <- loo_compare(mcp_loo)
  mcp_result_list[[lvl]] <- mcp_result
}
tmp <- lapply(mcp_result_list, function(x) {
  lapply(x,function(y) {
    summary(y)
  })
})

write_rds(tmp,here("results","binary_18","mcp_result.rds"))
write_rds(loo_comparison_list,here("results","binary_18","loos_comparison_list.rds"))

# secondary set of models
# try a diff sigma
list_mcp_models2 <-
  list(
    zero_one = list(rate ~ 1 + Year,
                    ~ 0 + Year),
    zero_one_sigma = list(rate ~ 1 + Year,
                    ~ 0 + Year +sigma(1))
  )

loo_comparison_list2 <- c()
mcp_result_list2 <- c()

for(lvl in 1:nrow(df_annual_levels)){
  tmp_level <- df_annual_levels[lvl,]
  tmp_prov <- tmp_level$province
  tmp_sex <- tmp_level$sex
  tmp_age_group <- tmp_level$age_group
  
  df_annual <- df_annual_master %>%
    filter(province==tmp_prov &
             sex == tmp_sex &
             age_group == tmp_age_group) %>% 
    mutate(Year = Year - min(Year))
  
  mcp_result <- lapply(list_mcp_models2,function(mod){
    mcp(mod,data=df_annual,sample='both',adapt=5000,iter=5000,chains=50,cores=3)
  })
  
  # compute loo
  mcp_loo <- lapply(mcp_result,function(result){
    loo::loo(result)
  })
  
  # compare & save results
  loo_comparison_list2[[lvl]] <- loo_compare(mcp_loo)
  mcp_result_list2[[lvl]] <- mcp_result
}

tmp <- lapply(mcp_result_list2, function(x) {
  lapply(x,function(y) {
    summary(y)
  })
})
write_rds(tmp,here("results","binary_18","mcp_result2.rds"))
write_rds(loo_comparison_list,here("results","binary_18","loos_comparison_list2.rds"))


# tidy up -----------------------------------------------------------------
loo_comparsion <- read_rds(here("results","binary_18","loos_comparison_list.rds"))

