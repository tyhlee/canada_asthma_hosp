rm(list=ls())

##### import packages ####
library(tidyverse)
library(lubridate)

###### Initial settings ####

# import the estimated & interpolated Canadian population data
# variables:
# province, fiscal_year, Q, month, sex, age_group, N, prop
df_pop_master <- read_csv("data/df_pop_all_analysis.csv") %>% 
  filter(fiscal_year>=2006) %>% 
  filter(!(fiscal_year>=2018 & month>3))

# labels for provinces & territories
prov_code <- c("CA","AB","BC","MA","NB","NF","NS","ON","PE","QC","SK","TR")
prov <- c("Canada","Alberta",'British Columbia','Manitoba','New Brunswick','Newfoundland and Labrador',
          "Nova Scotia","Ontario","Prince Edward Island","Quebec","Saskatchewan","TR")

# recoding the province
df_pop_master$province <- prov_code[match(df_pop_master$province,prov)] 

# import hosp data
df_cihi_master_all <- read_csv("data/cihi_no_diag_cleaned_Nov25.csv",guess_max = 685865) %>% 
  mutate(ad_year = year(addate),
         discharge_year = year(sepdate),
         los = as.numeric(sepdate-addate),
         ad_Q=if_else(month(addate) %in% c(1,2,3),'Q1',
                      if_else(month(addate) %in% c(4,5,6),'Q2',
                              if_else(month(addate) %in% c(7,8,9),'Q3','Q4'))),
         ad_month = month(addate)) %>% 
  select(row_num,studyid,province,
         addate,sepdate,
         ad_year,ad_Q,ad_month,
         discharge_year,
         los,
         sex,age,
         icd_type) %>% 
  filter(ad_year>=2006)

# check the num of unknown sex
# if less than 5 (privacy issues), exclude them
if (sum(df_cihi_master_all$sex=='O')<5){
  df_cihi_master_all <- df_cihi_master_all %>% 
    filter(sex !="O") %>% 
    mutate(sex=as.factor(sex))
}

# list of provinces
provinces <- unique(df_pop_master$province)

# list of time aggregation units
aggregation_unit <- c("yearly",'quarterly','monthly')


# list of age groups i want t explore
list_age_group <- list(
  binary_18 = c("all","0-18","19+"),
  Gershon = c("all","0-4","5-9","10-14","15-39","40-69","70+"),
  GINA = c("all","0-4","5-11","12-39","40-64","65+"),
  Larsen = c("all","0-4","5-9","10-14","15-64","65+")
)

# aggregation
for (index in 2:length(list_age_group)){
  
  # choose age groups
  chosen_age_group <-list_age_group[[index]]
  
  # turn them into cut-points for data processing
  age_group_num <- str_split(chosen_age_group,'-') %>% 
    lapply(.,function(x){x[[1]]}) %>% 
    unlist() %>% 
    str_replace_all(.,'[[+]]','')
  
  age_group_num <- c(age_group_num[-1],Inf) %>% as.numeric()
  
  # create the dir for the chosen age group
  age_dir <- file.path(paste0('results/',names(list_age_group)[index],'/'))
  dir.create(age_dir)
  
  
  # shape pop data and hosp data with the selected age grouping
  df_pop <- df_pop_master
  df_pop$new_age_group <- NA
  for ( i in 1:length(chosen_age_group)){
    tmp_age_group <- chosen_age_group[i]
    if(tmp_age_group=='all'){
      
      df_pop <- df_pop %>% 
        mutate(new_age_group = ifelse(age_group %in% "all",
                                      "all",
                                      new_age_group))
    } else if (i==length(chosen_age_group)){
      # remove a potential "+"
      tmp <- str_replace(tmp_age_group,'[[+]]',"")
      df_pop <- df_pop %>% 
        mutate(new_age_group = ifelse(age_group %in% seq(tmp,110),
                                      tmp_age_group,
                                      new_age_group))
    } else{
      tmp <- str_split(tmp_age_group,'-')[[1]]
      df_pop <- df_pop %>% 
        mutate(new_age_group = ifelse(age_group %in% seq(tmp[1],tmp[2]),
                                      tmp_age_group,
                                      new_age_group))
    }
  }
  
  df_pop <- df_pop %>% 
    filter(!is.na(new_age_group)) %>% 
    mutate(new_age_group= factor(new_age_group,levels = chosen_age_group, labels=chosen_age_group)) %>% 
    rename(tmp = age_group) %>% 
    rename(age_group = new_age_group )
  
  df_cihi_master <- df_cihi_master_all %>% 
    mutate(age_group = findInterval(age, age_group_num),
           age_group = factor(age_group,
                              levels=seq(1,length(age_group_num)-1),
                              labels=chosen_age_group[-1]))
  
  # los
  df_los <- df_cihi_master %>% 
    filter(los <= 100000)
  
  df_los %>% 
    group_by(ad_year,age_group,sex) %>% 
    filter(ad_year <=2017) %>% 
    summarise(avg = mean(los),
              med = median(los),
              q1 = quantile(los,.25),
              q3=quantile(los,.75)) %>% 
    arrange(sex,age_group,ad_year) -> tab.los
  
  write_csv(tab.los,paste0(age_dir,'/los.csv'))
  
  for (q in 1:length(aggregation_unit)){
    # choose time agg. unit
    chosen_aggregation_unit <- aggregation_unit[q]
    # create the dir for chosen_province (outer loop)
    prov_dir <- file.path(paste0(age_dir,'/',chosen_aggregation_unit,'/'))
    dir.create(prov_dir)
    
    for( qq in 1:length(provinces)){
      # create the dir for chosen_aggregation_unit within the chosen prov dir (inner loop)
      chosen_province <- provinces[qq]
      dir <- file.path(paste0(prov_dir,'/',chosen_province,'/'))
      dir.create(dir)
      
      ######################## Objective 1 ##################
      df_pop_analysis <- df_pop %>% 
        filter(province==chosen_province) %>% 
        select(-province) 
      
      time_aggregation_index <- case_when(chosen_aggregation_unit == 'yearly' ~ 1,
                                          chosen_aggregation_unit == 'quarterly'~2,
                                          chosen_aggregation_unit == 'monthly' ~3)
      
      
      df_pop_analysis <- df_pop_analysis %>% 
        # time aggregation unit
        group_by(across(c(1:time_aggregation_index)),sex,age_group) %>% 
        summarise(N=sum(N)) %>% 
        mutate(max_N =max(N),
               prop=N/max_N) %>% 
        select(-max_N)
      
      tmp_age_group <- unique(df_pop_analysis$age_group)
      
      if(chosen_province != "CA"){
        df_cihi <- df_cihi_master %>%
          filter(province==chosen_province)
      } else{
        df_cihi <- df_cihi_master
      }
      
      
      ################ Table 1 #################
      
      if (chosen_aggregation_unit == 'yearly'){
        join_key = 'ad_year'
      } else if (chosen_aggregation_unit=='quarterly'){
        join_key = c('ad_year','ad_Q')
      } else if (chosen_aggregation_unit=='monthly'){
        join_key = c('ad_year','ad_Q','ad_month')
      } else{
        warning('aggregation unit is not supported')
      }
      
      
      cihi_time_aggregation_index <- match(join_key,colnames(df_cihi))
      
      # save results
      table1 <- c()
      
      # Year - N
      df_cihi %>% 
        group_by(across(cihi_time_aggregation_index)) %>% 
        summarise(n=n()) -> table1[[1]]
      
      # Year - N - Sex
      df_cihi %>% 
        group_by(across(cihi_time_aggregation_index),sex) %>% 
        summarise(n=n()) %>% 
        spread(sex,n)-> table1[[2]]
      
      # Year - N - Age group
      df_cihi %>% 
        group_by(across(cihi_time_aggregation_index),age_group) %>% 
        summarise(n=n()) %>% 
        spread(age_group,n)-> table1[[3]]
      
      # Year - N - Sex - Age group
      df_cihi %>% 
        mutate(sex_age_group = paste0(sex,"_",age_group)) %>% 
        group_by(across(cihi_time_aggregation_index),sex_age_group) %>% 
        summarise(n=n()) %>% 
        spread(sex_age_group,n)-> table1[[4]]
      
      table1.final <- plyr::join_all(table1,by=join_key,type='left')
      table1.final[is.na(table1.final)] <- 0
      
      tmp_checker <-  df_pop_analysis %>% 
        ungroup() %>% 
        filter(sex!='B' & age_group!='all') %>% 
        group_by(across(1:time_aggregation_index)) %>% 
        summarise(n=sum(N)) %>% 
        select(-n)
      
      if( nrow(tmp_checker)!=nrow(table1.final) ){
        tmp <- table1.final[,1:(time_aggregation_index)]
        tmp2 <- tmp_checker
        tmp3 <- colnames(tmp)
        colnames(tmp2) <- tmp3
        
        tmp2 %>% 
          left_join(tmp %>% 
                      mutate(flag=T),by=tmp3) -> look
        tmp2[which(is.na(look$flag)),]
        tmp4 <- rbind(table1.final,tmp2[which(is.na(look$flag)),])
        tmp4[is.na(tmp4)] <- 0
        tmp4 <- tmp4 %>% 
          arrange(across(1:(time_aggregation_index)))
        table1.final <- tmp4
      }
      
      table1.final.prop <- table1.final[,-c(1:time_aggregation_index)]/pmax(table1.final$n,1)*100
      # table1.final.prop[which(is.na(table1.final.prop))] <- 0
      tmp_table1 <- table1.final[,-c(1:time_aggregation_index)]
      tmp_result <- matrix(0,nrow=nrow(table1.final.prop),
                           ncol=ncol(table1.final.prop))
      for (i in 1:nrow(table1.final.prop)){
        for(j in 1:ncol(table1.final.prop)){
          tmp_result[i,j] <- paste0(tmp_table1[i,j],' (',format(round(table1.final.prop[i,j],1),nsmall=1),')')
          
        }
      }
      colnames(tmp_result) <- colnames(tmp_table1)
      tmp_result <- tmp_result %>% 
        as.data.frame()
      table1.final.csv <- cbind(table1.final %>% 
                                  select(1:time_aggregation_index),tmp_result)
      
      write_csv(table1.final.csv,paste0(dir,'/','tab1_raw.csv'))
      
      ################# Table 2: Crude Rate ################
      table2.total.n <- c()
      
      # Year - N
      df_pop_analysis %>% 
        ungroup() %>% 
        filter(sex!='B' & age_group!='all') %>% 
        group_by(across(1:time_aggregation_index)) %>% 
        summarise(n=sum(N)) -> table2.total.n[[1]]
      
      # Year - N - Sex
      df_pop_analysis %>% 
        ungroup() %>% 
        filter(sex!='B' & age_group!='all') %>% 
        group_by(across(1:time_aggregation_index),sex) %>% 
        summarise(n=sum(N)) %>% 
        spread(sex,n) -> table2.total.n[[2]]
      
      # Year - N - Age group
      df_pop_analysis %>% 
        ungroup() %>% 
        filter(sex!='B' & age_group!='all') %>% 
        group_by(across(1:time_aggregation_index),age_group) %>% 
        summarise(n=sum(N)) %>% 
        spread(age_group,n) -> table2.total.n[[3]]
      
      
      # Year - N - Sex - Age group
      df_pop_analysis %>% 
        ungroup() %>% 
        filter(sex!='B' & age_group!='all') %>% 
        mutate(sex_age_group = paste0(sex,"_",age_group)) %>% 
        group_by(across(1:time_aggregation_index),sex_age_group) %>% 
        summarise(n=sum(N)) %>% 
        spread(sex_age_group,n) -> table2.total.n[[4]]
      
      table2.total.n <- plyr::join_all(table2.total.n,
                                       by=colnames(table2.total.n[[1]])[1:time_aggregation_index],
                                       type='left')
      
      
      per_pop <- 100000
      tmp_raw <- table1.final[,-c(1:time_aggregation_index)]
      tmp_n <- table2.total.n[,-c(1:time_aggregation_index)]
      table2.raw <- tmp_raw
      table2.rate <- tmp_raw/tmp_n
      table2.var <- table1.final[,-c(1:time_aggregation_index)]/(tmp_n^2)
      qcrit <- qnorm(1-0.05/2)
      # Byar Method
      table2.lower <- table2.rate*((1-1/(9*table2.raw)-qcrit/3*sqrt(1/table2.raw))^3)
      table2.lower[is.na(table2.lower)]<-0
      table2.upper <- ((table2.raw+1)/tmp_n)*((1-1/(9*(table2.raw+1))+qcrit/3*sqrt(1/(1+table2.raw)))^3)
      table2.rate <- table2.rate* per_pop
      table2.upper <- table2.upper * per_pop
      table2.lower <- table2.lower *per_pop
      
      tmp_table <- table2.rate
      tmp_result <- matrix(0,nrow=nrow(table2.rate),
                           ncol=ncol(table2.rate))
      for (i in 1:nrow(table2.rate)){
        for(j in 1:ncol(table2.rate)){
          tmp_result[i,j] <- paste0(format(round(tmp_table[i,j],1),nsmall=1),
                                    ' (',
                                    format(round(table2.lower[i,j],1),nsmall=1),
                                    '-',
                                    format(round(table2.upper[i,j],1),nsmall=1),
                                    ')')
          
        }
      }
      colnames(tmp_result) <- colnames(tmp_n)
      tmp_result <- tmp_result %>% 
        as.data.frame()
      
      table2.csv <- cbind(table2.total.n %>% 
                            select(1:time_aggregation_index),tmp_result)
      
      write_rds(list(raw=table2.raw,n=table2.total.n,
                     rate=table2.rate,
                     CI_upper=table2.upper,
                     CI_lower=table2.lower),
                paste0(dir,'/','tab2.rds'))
      
      write_csv(table2.csv, paste0(dir,'/', 'tab2_rate.csv'))
      
      ################### age adjusted rate ###################
      # OVERALL
      per_pop <- 100000
      
      tab3.overall.raw <- table2.raw[,match(chosen_age_group[-1],colnames(table2.raw))]
      
      tab3.overall.n <- table2.total.n[,match(chosen_age_group[-1],colnames(table2.total.n))]
      
      df_pop_analysis %>% 
        ungroup() %>% 
        filter(age_group %in% chosen_age_group[-1]) %>% 
        filter(sex =='B') %>% 
        select(c(1:time_aggregation_index),age_group,prop) %>% 
        group_by(across(c(1:time_aggregation_index)),age_group) %>%
        arrange(age_group) %>% 
        spread(age_group,prop) %>% 
        filter(fiscal_year==2017) %>% 
        ungroup() -> tab3.reference.prop
      
      # make the reference prop the same size as the overall n
      if(nrow(tab3.reference.prop)==1){
        tab3.reference.prop <- replicate(nrow(tab3.overall.n) %/% nrow(tab3.reference.prop),
                                         tab3.reference.prop[,-c(1:time_aggregation_index)],
                                         simplify = F) %>% 
          do.call(rbind,.)
      } else{
        tmp <- tab3.reference.prop
        tab3.reference.prop <- replicate(nrow(tab3.overall.n) %/% nrow(tab3.reference.prop),
                                         tab3.reference.prop[,-c(1:time_aggregation_index)],
                                         simplify = F) %>% 
          do.call(rbind,.) 
        
        if ((nrow(tab3.overall.n) %% nrow(tab3.reference.prop)) !=0){
          tab3.reference.prop <- tab3.reference.prop %>% 
            rbind(.,tmp[1:(nrow(tab3.overall.n) %% nrow(tmp)),
                        -c(1:time_aggregation_index)])
        }
        
      }
      
      age_std_rate_func_remover <- function(tab.raw,tab.prop,tab.n,remover=c(1:2),alpha=0.05){
        tab.saver <- tab.raw[,remover]
        tab.raw <- tab.raw[,-remover]
        tab.prop <- tab.prop[,-remover]
        tab.n <- tab.n[,-remover]
        
        tab.age.std.rate <- tab.raw/tab.n*tab.prop %>%
          as.data.frame()
        tab.age.std.var <- tab.raw/(tab.n)^2*(tab.prop)^2
        tab.age.std.var <- apply(tab.age.std.var,1,sum) %>% unlist() %>% as.vector()
        tab.age.std.rate <- apply(tab.age.std.rate,1,sum) %>% unlist() %>% as.vector()
        qcrit <- qnorm(1-alpha/2)
        return(cbind(tab.saver,
                     data.frame(
                       age_std_rate=tab.age.std.rate,
                       CI_lower=tab.age.std.rate-qcrit*sqrt(tab.age.std.var),
                       CI_upper=tab.age.std.rate+qcrit*sqrt(tab.age.std.var))
        )
        )
      }
      
      age_std_rate_func <- function(tab.raw,tab.prop,tab.n,alpha=0.05){
        
        tab.age.std.rate <- tab.raw/tab.n*tab.prop %>%
          as.data.frame()
        tab.age.std.var <- tab.raw/(tab.n)^2*(tab.prop)^2
        tab.age.std.var <- apply(tab.age.std.var,1,sum) %>% unlist() %>% as.vector()
        tab.age.std.rate <- apply(tab.age.std.rate,1,sum) %>% unlist() %>% as.vector()
        qcrit <- qnorm(1-alpha/2)
        return(
          data.frame(
            age_std_rate=tab.age.std.rate,
            CI_lower=tab.age.std.rate-qcrit*sqrt(tab.age.std.var),
            CI_upper=tab.age.std.rate+qcrit*sqrt(tab.age.std.var)
          )
        )
      }
      
      tab3.overall <- age_std_rate_func(tab.raw=tab3.overall.raw,
                                        tab.prop=tab3.reference.prop,
                                        tab.n=tab3.overall.n)
      tab3.overall<-tab3.overall*per_pop
      
      tab3.overall <- cbind(table1.final.csv %>% 
                              select(1:time_aggregation_index),tab3.overall) %>% 
        mutate(age_group ="all",
               sex='B')
      
      # male & female
      df_pop_analysis %>% 
        ungroup() %>% 
        filter(age_group %in% chosen_age_group[-1]) %>% 
        filter(sex !='B') %>% 
        select(1:time_aggregation_index,sex,age_group,N) %>% 
        group_by(across(c(1:time_aggregation_index)),sex,age_group) %>%
        arrange(age_group) %>% 
        spread(age_group,N) -> tab3.sex.n
      
      # reference prop
      df_pop_analysis %>% 
        ungroup() %>% 
        filter(age_group %in% chosen_age_group[-1]) %>% 
        filter(sex !='B') %>% 
        select(1:time_aggregation_index,sex,age_group,prop) %>% 
        group_by(across(c(1:time_aggregation_index)),sex,age_group) %>%
        arrange(age_group) %>% 
        spread(age_group,prop) %>% 
        filter(fiscal_year==2017) %>% 
        ungroup() -> tab3.sex.prop
      
      tmp <- tab3.sex.prop[,-c(1:(time_aggregation_index+1))]
      for(j in 1:nrow(tmp)){
        tmp[j,] <- tmp[j,]/sum(tmp[j,])
      }
      
      tab3.sex.prop[,-c(1:(time_aggregation_index+1))] <- tmp
      
      tmp <- tab3.sex.prop
      tab3.sex.prop <- replicate(nrow(tab3.sex.n) %/% nrow(tab3.sex.prop),
                                 tab3.sex.prop[,-c(1:(1+time_aggregation_index))],
                                 simplify = F) %>% 
        do.call(rbind,.)
      
      if ((nrow(tab3.sex.n) %% nrow(tab3.sex.prop))!=0){
        tab3.sex.prop <- tab3.sex.prop %>% 
          rbind(.,tmp[1:(nrow(tab3.sex.n) %% nrow(tmp)),
                      -c(1:(time_aggregation_index+1))])
        
      }
      
      tab3.sex.prop <- cbind(tab3.sex.n %>% 
                               select(1:(time_aggregation_index+1)),
                             tab3.sex.prop)  
      
      df_cihi %>% 
        mutate(sex_age_group = paste0(sex,'_',age_group)) %>% 
        group_by(across(cihi_time_aggregation_index),sex,age_group) %>% 
        summarise(n=n()) %>%
        spread(age_group,n,fill=0) -> tab3.sex.raw
      
      # need to fix tab3.sex.raw
      if( nrow(tab3.sex.raw)!=nrow(tab3.sex.n) ){
        tmp <- tab3.sex.n[,1:(time_aggregation_index+1)]
        tmp2 <- tab3.sex.raw[,1:(time_aggregation_index+1)]
        tmp3 <- colnames(tmp2)
        colnames(tmp) <- tmp3
        
        tmp %>% 
          left_join(tmp2 %>% 
                      mutate(flag=T),by=tmp3) -> look
        tmp4 <- rbind(tab3.sex.raw,tmp[which(is.na(look$flag)),])
        tmp4[is.na(tmp4)] <- 0
        tmp4 <- tmp4 %>% 
          arrange(across(1:(time_aggregation_index+1)))
        tab3.sex.raw <- tmp4
      }
      
      # by sex
      tab3.sex<- age_std_rate_func_remover(tab.raw=tab3.sex.raw,
                                           tab.prop=tab3.sex.prop,
                                           tab.n=tab3.sex.n,
                                           remover=c(1:(1+time_aggregation_index)))
      tab3.sex[,-c(1:(1+time_aggregation_index))] <-tab3.sex[,-c(1:(1+time_aggregation_index))]*per_pop
      tab3.sex <- tab3.sex %>% 
        mutate(age_group='all')
      # by age group
      
      subgroup_rate_func <- function(tab.raw,tab.prop,tab.n,remover=c(1:2),alpha=0.05,
                                     per_pop = 100000){
        # tab.raw = tab3.sex.raw
        # tab.prop = tab3.sex.prop
        # tab.n = tab3.sex.n
        tab.saver <- tab.raw[,remover]
        tab.raw <- tab.raw[,-remover]
        tab.prop <- tab.prop[,-remover]
        tab.n <- tab.n[,-remover]
        
        tab.age.rate <- tab.raw/tab.n %>%
          as.data.frame()
        tab.age.var <- tab.raw/(tab.n)^2
        qcrit <- qnorm(1-alpha/2)
        
        # Byar Method
        tab.lower <- tab.age.rate*((1-1/(9*tab.raw)-qcrit/3*sqrt(1/tab.raw))^3)
        tab.lower[is.na(tab.lower)]<-0
        tab.upper <- ((tab.raw+1)/tab.n)*((1-1/(9*(tab.raw+1))+qcrit/3*sqrt(1/(1+tab.raw)))^3)
        
        tab.age.rate <- tab.age.rate* per_pop
        tab.upper <- tab.upper * per_pop
        tab.lower <- tab.lower *per_pop
        
        group_num_index <- c((max(remover)+1):(max(remover)+length(age_group_num)-1))
        tab.age.rate <- gather(cbind(tab.saver,tab.age.rate),key = "age_group", value="rate",group_num_index)
        tab.age.CI_lower <- gather(cbind(tab.saver,tab.lower),key = "age_group", value="CI_lower",group_num_index)
        tab.age.CI_upper <- gather(cbind(tab.saver,tab.upper),key = "age_group", value="CI_upper",group_num_index)
        
        
        return(tab.age.rate %>% 
                 left_join(tab.age.CI_lower,by=colnames(tab.age.rate)[1:(ncol(tab.age.rate)-1)]) %>% 
                 left_join(tab.age.CI_upper,by=colnames(tab.age.rate)[1:(ncol(tab.age.rate)-1)])
        )
        
      }
      
      tab3.age.group <- subgroup_rate_func(tab3.sex.raw,
                                           tab3.sex.prop,
                                           tab3.sex.n,
                                           remover=c(1:(time_aggregation_index+1)),
                                           alpha=0.05,
                                           per_pop=per_pop)
      
      
      tab3 <- rbind(tab3.overall %>% 
                      rename(rate=age_std_rate) %>% 
                      select(!!!colnames(tab3.age.group)),
                    tab3.sex %>% 
                      rename(rate=age_std_rate) %>% 
                      select(!!!colnames(tab3.age.group)),
                    tab3.age.group)
      
      write_csv(tab3,paste0(dir,'/', 'tab3_rate.csv'))
    }
  }
}
