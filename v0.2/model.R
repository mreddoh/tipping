
## SET PACKAGES
library("fitzRoy")
library("dplyr")
library("lubridate")
library("here")
library("ggplot2")


## LOAD DATA
load(here::here("data", "mysample.Rda"))

# plot variables
mysample %>% select(preELOdiff,outcome) %>%
             mutate(preELOdiff_grp = cut(preELOdiff, breaks=seq(-0.5,0.5,0.025))) %>%
             group_by(preELOdiff_grp) %>%
             summarise(mean_prob=mean(outcome), cnt=n()) %>%
             ggplot(aes(x=preELOdiff_grp, y=mean_prob)) + geom_point(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1))

mysample %>% select(form.5.diff,outcome) %>%
             mutate(form.5.diff_grp = cut(form.5.diff, breaks=seq(-150,150,5))) %>%
             group_by(form.5.diff_grp) %>%
             summarise(mean_prob=mean(outcome), cnt=n()) %>%
             ggplot(aes(x=form.5.diff_grp, y=mean_prob)) + geom_point(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1))

names(mysample)
mysample %>% select(avg_MIDrating_p15.diff,outcome) %>%
  mutate(avg_MIDrating_p10.diff_grp = cut(avg_MIDrating_p15.diff, breaks=seq(-0.5,0.5,0.025))) %>%
  group_by(avg_MIDrating_p10.diff_grp) %>%
  summarise(mean_outcome=mean(outcome), cnt=n()) %>%
  ggplot(aes(x=avg_MIDrating_p10.diff_grp, y=mean_outcome)) + geom_point(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1))

mysample %>% select(avg_BCKrating_p10.diff,outcome) %>%
  mutate(avg_BCKrating_p10.diff_grp = cut(avg_BCKrating_p10.diff, breaks=seq(-1,1,0.025))) %>%
  group_by(avg_BCKrating_p10.diff_grp) %>%
  summarise(mean_outcome=mean(outcome), cnt=n()) %>%
  ggplot(aes(x=avg_BCKrating_p10.diff_grp, y=mean_outcome)) + geom_point(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1))

mysample %>% select(avg_FWDrating_p10.diff,outcome) %>%
  mutate(avg_BCKrating_p10.diff_grp = cut(avg_FWDrating_p10.diff, breaks=seq(-0.175,0.175,0.025))) %>%
  group_by(avg_BCKrating_p10.diff_grp) %>%
  summarise(mean_outcome=mean(outcome), cnt=n()) %>%
  ggplot(aes(x=avg_BCKrating_p10.diff_grp, y=mean_outcome)) + geom_point(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1))


#winsorize the variable
mysample <- mysample %>% 
            mutate(preELOdiff_f = case_when(preELOdiff <= -0.25 ~ -0.25, preELOdiff >= 0.25 ~ 0.25, TRUE ~ preELOdiff)) %>%
            mutate(form.10.diff_f = case_when(form.10.diff <= -85 ~ -85, form.10.diff >= 85 ~ 85, TRUE ~ form.10.diff)) %>%
            mutate(form.5.diff_f = case_when(form.5.diff <= -85 ~ -85, form.5.diff >= 85 ~ 85, TRUE ~ form.5.diff))  %>%
            mutate(avg_FWDrating_p10.diff_f = case_when(avg_FWDrating_p10.diff <= -0.5 ~ -0.5, avg_FWDrating_p10.diff >= 0.5 ~ 0.5, TRUE ~ avg_FWDrating_p10.diff))  %>%
            mutate(avg_MIDrating_p10.diff_f = case_when(avg_MIDrating_p10.diff <= -0.5 ~ -0.5, avg_MIDrating_p10.diff >= 0.5 ~ 0.5, TRUE ~ avg_MIDrating_p10.diff))  %>%
            mutate(avg_BCKrating_p10.diff_f = case_when(avg_BCKrating_p10.diff <= -0.175 ~ -0.175, avg_BCKrating_p10.diff >= 0.175 ~ 0.175, TRUE ~ avg_BCKrating_p10.diff)) 

model_ds <- mysample %>% filter(year(Date)!=2021)
#apply transform to variable

model <- glm(formula = outcome ~ expectedResult + 
                                 #form.5.diff_f + 
                                 avg_FWDrating_p10.diff_f +
                                 avg_MIDrating_p10.diff_f +
                                 avg_BCKrating_p10.diff_f,
             family = binomial(link = "logit"), data = model_ds)

summary(model)

mysample %>% filter(year(Date)==2021 & Status=="Home") %>%
             mutate(fitted.results = predict(model,newdata=.,type='response')) %>%
             mutate(fitted.results = ifelse(fitted.results > 0.5,1,0)) %>%
             mutate(result = fitted.results == outcome) %>%
             summarise(mean = mean(result))

mysample %>% filter(year(Date)==2021 & Status=="Home") %>%
             mutate(expectedResult = ifelse(expectedResult > 0.5,1,0)) %>%
             mutate(result = expectedResult == outcome) %>%
             summarise(mean = mean(result))

save(model, file = here("models","model_v.02.Rda"))

# check other tipsters
load(here::here("data", "tipsters.Rda"))

tipsters %>% filter(year(date)==2021) %>% group_by(source) %>% summarise(mean = mean(correct))
