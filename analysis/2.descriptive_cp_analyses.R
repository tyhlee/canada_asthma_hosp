# import package ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(here)

# process the data for descriptive & chage-point analyses -----------------
# time unit list
# tu_list <- c("yearly","quarterly","monthly")
# only yearly for the paper
tu_list <- c("yearly")

# provincial list
# province_list <- c("CA","BC","AB","SK",
#                    "MA","ON","QC","NB",
#                    "NS","PE","NF","TR")
# only CA for the paper
province_list <- c("CA")

# data
# data name list
dat_names <- list.files(here("results"))

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

# main result: binary_18
# choose the index for binary_18
index <- 1
dat_name <- dat_names[index]

dir <- here('results',dat_name,tu)

# Extract the aggregated dataset and tidy it up the for descriptive analysis --------
# empty list to store data
tab3_list <- c()
for (i in 1:length(tu_list)){
  tu <- tu_list[i]
  tab3_final <- c()
  for (j in 1:length(province_list)){
    
    province <- province_list[j]
    
    # extract raw, rate, 95% CIs
    tmp <- read_rds(here::here('results',dat_name,tu,province,'tab2.rds'))[c(1,3:5)] 
    
    tmp_n <- tmp[[1]] %>% 
      mutate(Year = rep(min_year:(max_year+1),1))
    
    tmp <- tmp[-1] %>% 
      lapply(., function(x){
        x %>% 
          mutate(Year=tmp_n$Year)
      })
    
    tmp <- tmp %>% 
      lapply(.,function(x) {
        tmp_df <- x [,-c(1:3)] %>% 
          gather(group,rate,-7) %>% 
          mutate(sex = case_when(
            substr(group,1,1) == "F" ~ "F",
            substr(group,1,1) == "M" ~ "M",
            TRUE ~ "B"),
            age_group = str_remove(group,"[a-zA-z]\\_")
          ) %>% 
          select(Year,age_group,sex,rate) %>%
          arrange(sex,age_group,Year)
      }) 
    
    tmp_n <- tmp_n[,-c(1:3)] %>% 
      gather(group,n,-7) %>%  
      mutate(sex = case_when(
        substr(group,1,1) == "F" ~ "F",
        substr(group,1,1) == "M" ~ "M",
        TRUE ~ "B"),
        age_group = str_remove(group,"[a-zA-z]\\_")
      ) %>% 
      select(Year,age_group,sex,n) %>%
      arrange(sex,age_group,Year)
    
    tab3_final[[j]] <- left_join(tmp_n,tmp[[1]],by=c("Year","age_group","sex")) %>% 
      left_join(tmp[[3]] %>% 
                  rename(CI_lower=rate),
                by=c("Year","age_group","sex")) %>% 
      left_join(tmp[[2]] %>% 
                  rename(CI_upper=rate),
                by=c("Year","age_group","sex")) %>% 
      mutate(province=province)
  }
  # store
  tab3_final <- do.call(rbind,tab3_final)
  tab3_list[[i]] <- tab3_final
}

names(tab3_list) <- tu_list
path_rates <- here('results',dat_name,'rates.rds')
write_rds(tab3_list,path_rates)

# Generate figures for descriptive analysis -------------------------------
fig.dir <- here("results",dat_name,"figures")
dir.create(fig.dir)

# LOS ---------------------------------------------------------------------
df_los <- read_csv(here("results",dat_name,"los.csv"))
ggplot(data=df_los,aes(x=ad_year,y=avg)) +
  geom_point() +
  geom_line()+
  facet_grid(sex~age_group)+
  ylim(0,5)+
  scale_x_continuous(breaks=c(seq(2006,max_year,2))) +
  xlab("Year") +
  ylab("Average Length of Stay (days)") +
  theme_text_size -> gg.los

ggsave(filename = here(fig.dir,"los.png"),plot=gg.los,dpi=fig_dpi)

# Figure 1 - annual numbers (bar plot) and adjusted rates by the binary age group --------
# 1 for yearly
df_fig <- read_rds(path_rates)[[1]] %>% 
  filter(province=="CA" & Year <= max_year)

# ped & both & n & rate
tmp_df <- df_fig %>%
  filter(age_group=='0-18' & sex=='B')

ggplot(data=tmp_df %>% 
         mutate(Year=as.factor(Year))) +
  geom_point(aes(y=rate,x=Year)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper,x=Year), width=.2,
                position=position_dodge(.9)) +
  geom_line(aes(y=rate,x=as.numeric(Year)))+
  geom_col(aes(y=n/1000,x=Year),fill="#bdbdbd") +
  scale_y_continuous(sec.axis=sec_axis(~(.)*1000,name="Number of Asthma Hospital Admissions")) +
  theme_classic() +
  ylab("Rate of Asthma Hospital Admissions per 100,000") +
  theme_text_size -> fig1.ped

ggsave(filename = here(fig.dir,'fig1_ped_18.png'),dpi=fig_dpi,plot=fig1.ped)

tmp_df <- df_fig %>%
  filter(age_group=='19+' & sex=='B')

ggplot(data=tmp_df %>% 
         mutate(Year=as.factor(Year))) +
  geom_point(aes(y=rate,x=Year)) +
  geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper,x=Year), width=.2,
                position=position_dodge(.9)) +
  geom_line(aes(y=rate,x=as.numeric(Year)))+
  geom_col(aes(y=n/4000,x=Year),fill="#bdbdbd") +
  scale_y_continuous(sec.axis=sec_axis(~(.)*4000,name="Number of Asthma Hospital Admissions")) +
  theme_classic() +
  ylab(ylab_rate) +
  theme_text_size -> fig1.adult

ggsave(filename = here(fig.dir,'fig1_adult_18.png'),dpi=fig_dpi,plot=fig1.adult)

# CP ----------------------------------------------------------------------

# Figure 2 - Change-point analysis ----------------------------------------
library(segmented)

# change the figure settings
tick_label_size <-18
label_size <- 23
theme_text_size <- theme(axis.text.x = element_text(size=tick_label_size),
                         axis.text.y = element_text(size=tick_label_size),
                         text = element_text(size=label_size))

df_annual_master <- df_fig %>% 
  filter(sex!='B' & province=='CA' & Year <=max_year) %>% 
  mutate(Date=Year) %>% 
  select(-n)

df_annual_levels <- df_annual_master %>% 
  distinct(province,sex,age_group) %>% 
  select(province,sex,age_group) %>% 
  mutate(index = row_number())

gg_cp_list <- c()
tab_cp_list <- c()
alpha <- 0.05
alpha_CI <-0.05

df_annual_master %>% 
  group_by(age_group) %>% 
  summarise(max_y = ceiling(max(rate))) -> max_y_group

num_digits <- 2

df_cp_list <- c()
df_annual_list <- c()

for(lvl in 1:nrow(df_annual_levels)){
  tmp_level <- df_annual_levels[lvl,]
  tmp_prov <- tmp_level$province
  tmp_sex <- tmp_level$sex
  tmp_age_group <- tmp_level$age_group
  
  df_annual <- df_annual_master %>%
    filter(province==tmp_prov &
             sex == tmp_sex &
             age_group == tmp_age_group)
  
  
  tmp_lm <- lm(rate ~ Year, data=df_annual)
  cp <- selgmented(tmp_lm,seg.Z=~Year,bonferron =F,alpha = alpha)
  cp_date_range <- range(df_annual$Date)
  cp.name <- 'psi'
  
  # check whether there is a cp
  cp_colours <- c("#ca0020",'#0571b0','blue')
  # alpha <- qnorm(1-alpha_CI/2)
  if(cp.name %in% attributes(cp)$names) {
    
    x <- seq(cp_date_range[1],cp_date_range[2],by=0.01)
    tmp.psi <- cp$psi %>%
      as.data.frame()
    cp.int <- coef(cp)[1]
    tmp.slope <- slope(cp)[[1]] %>%
      as.data.frame()
    cp.fitted_lines <- c()
    for(jjj in 1:length(tmp.slope[,1])){
      if(jjj==1){ # first
        cp.fitted_lines[[jjj]] <- data.frame(fitted_values=x*tmp.slope[jjj,1] + cp.int,
                                             type=c(rep('fitted',sum(x<=tmp.psi$Est.[jjj])),
                                                    rep('counterfactual',sum(x>tmp.psi$Est.[jjj]))),
                                             colour=cp_colours[jjj])
      } else if (jjj==length(tmp.slope[,1])){ # last
        cp.int.new <- tmp.psi$Est.[jjj-1]*tmp.slope[jjj-1,1]+cp.int - tmp.psi$Est.[jjj-1]*tmp.slope[jjj,1]
        cp.fitted_lines[[jjj]] <- data.frame(fitted_values=x*tmp.slope[jjj,1] + cp.int.new,
                                             type=c(rep('counterfactual',sum(x<tmp.psi$Est.[jjj-1])),
                                                    rep('fitted',sum(x>=tmp.psi$Est.[jjj-1]))),
                                             colour=cp_colours[jjj])
      } else{
        cp.int.new <- tmp.psi$Est.[jjj-1]*tmp.slope[jjj-1,1]+cp.int - tmp.psi$Est.[jjj-1]*tmp.slope[jjj,1]
        cp.fitted_lines[[jjj]] <- data.frame(fitted_values=x*tmp.slope[jjj,1] + cp.int.new,
                                             type=c(rep('counterfactual',sum(x<tmp.psi$Est.[jjj-1])),
                                                    rep('fitted',sum(x>=tmp.psi$Est.[jjj-1] &
                                                                       x< tmp.psi$Est.[jjj])),
                                                    rep('counterfactual',sum(x>=tmp.psi$Est.[jjj]))),
                                             colour=cp_colours[jjj])
      }
    }
    
    df_cp <- cbind(x=rep(x,length(cp.fitted_lines)),do.call(rbind,cp.fitted_lines)%>%
                     as.data.frame())
    # %>%
    #   gather(key='segments',value='fitted_values',1+(1:length(cp.fitted_lines)))
    cp_est <- data.frame(confint(cp))
    colnames(cp_est) <- c("x",'CI_lower',"CI_upper")
    cp_est$y <- 0
    gg_cp <- ggplot(df_annual)+
      geom_line(data=df_cp,aes(x=x,y=fitted_values,col=colour,linetype=type),show.legend = F)+
      geom_point(aes(y=rate,x=Year))+
      geom_errorbar(aes(x=Year,ymin=CI_lower,ymax=CI_upper),width=0.2)+
      ylim(0,ceiling(max(df_annual$CI_upper)))+
      scale_colour_manual(values=cp_colours)+
      scale_linetype_manual(values=c("counterfactual"="dashed",'fitted'='solid'))+
      scale_x_continuous(breaks=c(2006:2017),labels=c(2006:2017)) +
      theme_classic() + 
      ylab(ylab_rate)+
      xlab("Year") +
      # geom_point(data=cp_est,aes(y=y,x=x),col="#bdbdbd")+
      # geom_errorbarh(data=cp_est,aes(y=y,xmin=CI_lower,xmax=CI_upper),col="#bdbdbd")+
      theme_text_size +
      ylim(c(0,max_y_group %>% 
               filter(age_group==tmp_age_group) %>% 
               select(max_y) %>% unlist()))
    
    n_cp <- nrow(tmp.psi)
    
    tab_cp <- data.frame(province=tmp_prov,
                         sex=tmp_sex,
                         age_group = tmp_age_group,
                         n=rep(n_cp,n_cp),
                         changepoints_est = tmp.psi$Est.,
                         changepoints_std = tmp.psi$St.Err,
                         slope_before = tmp.slope$Est.[-(n_cp+1)],
                         slope_before_std = tmp.slope$St.Err.[-(n_cp+1)],
                         slope_after = tmp.slope$Est.[-1],
                         slope_after_std = tmp.slope$St.Err.[-1]) %>%
      mutate(slope_relative_change = (slope_after-slope_before)/abs(slope_before)*100)
    
    df_annual_list[[lvl]] <- df_annual
    df_cp_list[[lvl]] <- df_cp
  }
  
  else{
    gg_cp <- ggplot(df_annual)+
      geom_point(aes(y=rate,x=Date))+
      geom_errorbar(data=df_annual,aes(x=Date,ymin=CI_lower,ymax=CI_upper),width=0.2)+
      geom_line(data=data.frame(rate=tmp_lm$fitted.values,
                                Date=df_annual$Date),
                aes(y=rate,x=Date))+
      ylim(0,ceiling(max(df_annual$CI_upper)))+
      theme_classic()
    tab_cp <- data.frame(province=tmp_prov,
                         sex=tmp_sex,
                         age_group = tmp_age_group,
                         n=0,
                         changepoints_est = NA,
                         changepoints_std = NA,
                         slope_before = NA,
                         slope_before_std = NA,
                         slope_after = NA,
                         slope_after_std = NA,
                         slope_relative_change =NA)
    
  }
  gg_cp_list[[lvl]] <- gg_cp
  tab_cp_list[[lvl]] <- tab_cp
}
options(mc.cores = 1)


tab_cp_compiled <- do.call(rbind,tab_cp_list)
qcrit <- qnorm(1-alpha_CI/2)
tab3 <- tab_cp_compiled %>% 
  select(-province) %>% 
  mutate(cp_CI_upper = changepoints_est +qcrit*changepoints_std,
         cp_CI_lower = changepoints_est -qcrit*changepoints_std,
         cp = paste0(format(round(changepoints_est,num_digits),nsmall = num_digits)
                     ,' (',
                     format(round(cp_CI_lower,num_digits),nsmall = num_digits),
                     " - ",
                     format(round(cp_CI_upper,num_digits),nsmall = num_digits)
                     ,')')) %>% 
  mutate(slope_before_CI_upper = slope_before +qcrit*slope_before_std,
         slope_before_CI_lower = slope_before -qcrit*slope_before_std,
         trend_before = paste0(format(round(slope_before,num_digits),nsmall = num_digits),
                               ' (',
                               format(round(slope_before_CI_lower,num_digits),nsmall = num_digits),
                               " - ",
                               format(round(slope_before_CI_upper,num_digits),nsmall = num_digits),
                               ')')) %>% 
  mutate(slope_after_CI_upper = slope_after +qcrit*slope_after_std,
         slope_after_CI_lower = slope_after -qcrit*slope_after_std,
         trend_after = paste0( format(round(slope_after,num_digits),nsmall=num_digits),' (',
                               format(round(slope_after_CI_lower,num_digits),nsmall = num_digits),
                               " - ",
                               format(round(slope_after_CI_upper,num_digits),nsmall = num_digits),')')) %>%
  select(sex,age_group,n,cp,trend_before,trend_after,slope_relative_change) %>% 
  mutate(sex=if_else(sex=='F',"Female","Male"),
         slope_relative_change = paste0(round(slope_relative_change,0),'%')) %>% 
  rename(`Number of change-points` = n,
         `Change-point` = cp,
         `Trend before` = trend_before,
         `Trend after` = trend_after,
         `Relative change` = slope_relative_change,
         `Age group` = age_group,
         Sex = sex)
tab3_old <- tab3
tab3 <- tab3 %>% 
  arrange(`Age group`)

# alter. table
tab4 <- c()
# special case n is just one all the time
m <- 2

est_CI_computer <- function(est,sd,dec=2,alpha=0.05){
  qcrit <- qnorm(1-alpha/2)
  tmp_upper <- est + qcrit*sd
  tmp_lower <- est - qcrit*sd
  paste0(format(round(est,dec),nsmall = dec),
         ' (',
         format(round(tmp_lower,dec),nsmall = dec),
         " - ",
         format(round(tmp_upper,dec),nsmall = dec),
         ')')
}

for( i in 1:nrow(tab_cp_compiled)){
  tmp <- tab_cp_compiled[i,]
  tab4[[i]] <- data.frame(sex=rep(tmp$sex,m),
                          age_group = rep(tmp$age_group,m),
                          segments = c(paste0(min_year," - ", floor(tmp$changepoints_est)),
                                       paste0(ceiling(tmp$changepoints_est), " - ", max_year)),
                          trend = c(est_CI_computer(tmp$slope_before,tmp$slope_before_std),
                                    est_CI_computer(tmp$slope_after,tmp$slope_after_std)),
                          relative_change = rep(paste0(round(tmp$slope_relative_change,0),'%'),2))
  
}
tab4 <- do.call(rbind,tab4) %>% 
  arrange(age_group) 

tab4 <- tab4 %>% 
  mutate(sex=if_else(sex=='M',"Male","Female")) %>% 
  rename(Sex = sex,
         `Age group`=age_group,
         `Trend (95% CI)` = trend,
         `Segments (calendar year)` = segments,
         `Relative change (%)` = relative_change )

# save all the results ----------------------------------------------------
library(openxlsx)
tab.dir <- here("results",dat_name,"tables")
dir.create(tab.dir)
wb <- createWorkbook()
addWorksheet(wb,"Binary 18 table")
writeData(wb,sheet=1,x=df_fig)
addWorksheet(wb,"CP Table")
writeData(wb,sheet=2,x=tab3)
addWorksheet(wb,"CP Table II")
writeData(wb,sheet=3,x=tab4)
saveWorkbook(wb, here(tab.dir,"table_results.xlsx"), overwrite = TRUE)

# save individual cp plot
mapply(function(cp_plot,index){
  tmp <- tab3_old[index,]
  ggsave(filename = here(fig.dir,paste0(tmp$Sex,"_",tmp$`Age group`,'.png')),plot=cp_plot,dpi=fig_dpi)
},gg_cp_list,c(1:length(gg_cp_list)))

# CP replotting in one plot 
df_annual <- do.call(rbind,df_annual_list) %>% 
  as.data.frame()
tmp_ag <- rep(c('0-18','19+'),2)
tmp_sex <- c("F","F","M","M")
df_cp <-c()
for (i in 1:length(df_cp_list)){
  df_cp[[i]] <- df_cp_list[[i]] %>% 
    as.data.frame() %>% 
    mutate(age_group=tmp_ag[i],
           sex = tmp_sex[i])
}
df_cp <- do.call(rbind,df_cp)

df_annual$sex <- factor(df_annual$sex,levels=c("F","M"),labels=c("Female","Male"))
df_annual$age_group <- factor(df_annual$age_group,levels=c("0-18","19+"),labels=c("Paediatric Group","Adult Group"))
df_cp$sex <- factor(df_cp$sex,levels=c("F","M"),labels=c("Female","Male"))
df_cp$age_group <- factor(df_cp$age_group,levels=c("0-18","19+"),labels=c("Paediatric Group","Adult Group"))
gg_cp <- ggplot(df_annual)+
  geom_line(data=df_cp,aes(x=x,y=fitted_values,col=colour,linetype=type),size=1.2,show.legend = F)+
  geom_point(aes(y=rate,x=Year),size=2)+
  geom_errorbar(aes(x=Year,ymin=CI_lower,ymax=CI_upper),width=0.2)+
  scale_colour_manual(values=cp_colours)+
  scale_linetype_manual(values=c("counterfactual"="dashed",'fitted'='solid'))+
  facet_grid(sex~age_group)+
  scale_x_continuous(breaks=seq(min_year,max_year,by=2),
                     labels=seq(min_year,max_year,by=2))+
  ylim(0,15)+
  ylab(ylab_rate)+
  xlab("Year") +
  theme_bw()+
  theme_text_size 
ggsave(filename = here(fig.dir,"cp_merged.png"),plot=gg_cp,dpi=fig_dpi)

# custom legend for the merged plot
df_legend <- data.frame(x <- c(4.5, 4.5),
                        y <- c(1, 2),
                        sdy <- c(0.3, 0.3))
library("Hmisc")
# add symbols to legend
with(df_legend,
     errbar(x = x,
            y = y,
            yplus = y + sdy,
            yminus = y - sdy,
            pch = 1, cex =.5, cap = .0025,
            errbar.col = c('black','red'),
            add = TRUE))

legend <- plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", 
       legend =c('Observed rate with 95% CI',
                 'Trend before a change point',
                 'Trend after a change point'), 
       pch=c(16,NA,NA),
       pt.cex=c(1.5,2,2),
       lty=c(NA,1,1),
       lwd=c(NA,2,2),
       bty='n',
       col = c('black', '#0571b0', '#ca0020'))
# manually save legend plot