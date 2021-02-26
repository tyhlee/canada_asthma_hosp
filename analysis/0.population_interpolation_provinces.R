rm(list=ls())

library(tidyverse)
library(lubridate)
############# pop interpolation ###################
df_pop <- read_csv("data/clean_pop_est.csv") %>% 
  filter(!(province %in% "Northwest Territories including Nunavut")) %>% 
  filter(fiscal_year >= 2000)

df_pop_TR <- df_pop %>% 
  filter(province %in% c("Yukon","Northwest Territories","Nunavut")) %>% 
  group_by(fiscal_year,sex,age_group,age_year) %>% 
  summarise(value = sum(value,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(province='TR') %>% 
  select(fiscal_year,province,sex,age_group,value,age_year)

df_pop_master <- df_pop %>% 
  filter(!(province %in% c("Yukon","Northwest Territories","Nunavut"))) %>% 
  rbind(.,df_pop_TR) %>% 
  select(fiscal_year,province,sex,age_year,value) %>% 
  rename(N=value,
         age_group=age_year) %>% 
  filter(fiscal_year %in% c(2005:2018))

# for each province, do the pop interpolation
provinces <- unique(df_pop_master$province)
pop_interpolation <- c()
for(ppp in 1:length(provinces)){
  tmp_province <- provinces[ppp]
  
  df_pop_all <- df_pop_master %>% 
    filter(province==tmp_province) %>% 
    filter(age_group=='all')

  df_pop_all_list <- split(df_pop_all,df_pop_all$sex)
  
  df_pop_all <- cbind(df_pop_all_list[[1]],n_female=df_pop_all_list[[2]]$N) %>% 
    mutate(prop_female=n_female/N) %>%
    select(fiscal_year,N,prop_female) %>% 
    mutate(fiscal_year=fiscal_year-2005) %>% 
    mutate(fiscal_month=7+12*fiscal_year)
  
  tmp_y <- c()
  for(yr in 1:(nrow(df_pop_all)-1)){
    x <- c(1,13)
    tmp_lm <- lm(df_pop_all$N[yr:(yr+1)]~x)
    new_x <- c(1:13)
    pred_lm<-predict(tmp_lm,newdata = list(x=new_x))
    plot(tmp_lm$model$`df_pop_all$N[yr:(yr + 1)]`,x=x,type='l',main=tmp_province)
    lines(tmp_lm$fitted.values,x=x,col='red')
    lines(pred_lm,col='orange')
    tmp_y[[yr]] <- pred_lm %>% as.vector()
  }
  tmp_y <- tmp_y %>% unlist()
  tmp_y <- tmp_y[-seq(13,13*12,by=13)]
  months <- seq(6,12*13+6,by=1)
  df_pop_est <- data.frame(fiscal_year=c(rep(0,7),
                                         lapply(df_pop_all$fiscal_year[-c(1,(nrow(df_pop_all)))],function(x) rep(x,12)) %>% unlist(),
                                         rep((nrow(df_pop_all)-1),6)),
                           months=(months-1)%%12,
                           N=tmp_y) %>% 
    mutate(fiscal_year = fiscal_year+2005,
           months = months+1) %>% 
    rename(month=months) %>% 
    mutate(Q=if_else(month %in% c(1:3),"Q1",
                     if_else(month %in% c(4:6),'Q2',
                             if_else(month %in% c(7:9),'Q3','Q4')))) %>% 
    select(fiscal_year,Q,month,N)
  
  df_gg_pop <- rbind(df_pop_all %>% 
                       mutate(fiscal_year=fiscal_year+2005,
                              month=6,
                              type = 'raw') %>% 
                       select(fiscal_year,month,N,type),
                     df_pop_est %>% 
                       mutate(type='predicted') %>% 
                       select(fiscal_year,month,N,type))
  
  ggplot(data=df_gg_pop %>% 
           mutate(year_month = if_else(str_count(month)==1,
                                       ymd(paste0(fiscal_year,'-0',month,truncated=1)),
                                       ymd(paste0(fiscal_year,'-',month,truncated=1)))),
         aes(x=year_month,y=N,colour=type))+
    geom_line()
  
  sex_age_proportion <- df_pop_master %>% 
    filter(province==tmp_province) %>% 
    select(fiscal_year,sex,age_group,N) %>% 
    group_by(fiscal_year) %>% 
    mutate(max_N = max(N)) %>% 
    ungroup() %>% 
    mutate(prop=N/max_N)
  
  df_pop_final <- df_pop_est %>% 
    rename(max_N = N) %>% 
    left_join(sex_age_proportion %>% 
                select(fiscal_year,sex,age_group,prop),by='fiscal_year') %>% 
    mutate(N=max_N*prop) %>% 
    mutate(province=tmp_province) %>% 
    select(province,fiscal_year,Q,month,sex,age_group,N,prop)
  pop_interpolation[[ppp]] <- df_pop_final
  
}
pop_interpolation <- do.call(rbind,pop_interpolation)
write_csv(pop_interpolation,'data/df_pop_all_analysis.csv')
