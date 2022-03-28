library(tidyverse)
library(lubridate)

D_settle <- read_csv("~/Desktop/detroit_edited.csv")

D_settle_2<-D_settle %>% mutate(closed_month = str_c(str_sub(closed_date,1,7),"-01") %>% ymd,closed_year = str_sub(closed_date,1,4))

#what happened in 2014? there were no cases closed in 2014. that's weird. More recently there have been fewer cases. Is that just how long it takes one of these 
#cases to move through the legal system?
D_settle_2%>% group_by(closed_month) %>% summarise(n = n()) %>% ggplot(aes(x = closed_month, y = n))+geom_point()

#largest award in early 2015
D_settle_2 %>% group_by(closed_month)%>% summarise(award=mean(amount_awarded))%>% ggplot(aes(x = closed_month, y = award))+geom_point()

D_settle_2 %>% group_by(closed_year)%>% summarise(award=mean(amount_awarded), n = n())
D_settle_2 %>% ggplot(aes(x = closed_year, y = amount_awarded))+geom_boxplot()

#a lot of events happning at similar places. many seem to be associated with law firms. are these just the addresses of the firms?
D_settle_2 %>% mutate(location = location %>% tolower) %>% pull(location) %>% table %>% sort(decreasing = T)

#the vast majority of cases are under "settled". 
D_settle %>% group_by(case_outcome) %>% summarise(amount = median(amount_awarded), n = n()) %>% arrange(desc(amount))

##not anywhere near perfect but we have a collection of repeat offenders. who's john appling? Looking at names
D_settle_2 %>% mutate(defendants = matter_name %>% str_split(.," v ")%>% lapply(function(x)return(x[[length(x)]])) %>% unlist %>% 
                        str_split(.,"vs")%>% lapply(function(x)return(x[[length(x)]])) %>% unlist %>% 
                        str_remove_all("et al") %>% 
                        str_split(.,"v.")%>% lapply(function(x)return(x[[length(x)]])) %>% unlist %>% 
                        str_remove("COD") %>% tolower %>% 
                        str_remove("city of detroit") %>% 
                        str_remove_all("\\.") %>% 
                        str_remove_all(",") %>% 
                        str_remove("-") %>%
                        str_remove_all("[0-9]") %>% 
                        str_remove("-no") %>% 
                        str_remove("-cz") %>% 
                        str_remove("detroit police officer") %>% 
                        str_remove("sgt") %>% 
                        str_remove("po") %>% 
                        str_remove("p o") %>% 
                        str_remove("cz") %>% 
                        str_remove("no") %>% 
                        trimws
                      ) %>% pull(defendants) %>% table %>% sort(decreasing=T)


#allegations
D_settle_3<-D_settle_2 %>% mutate(allegations = str_remove(summary_allegations,"Violation of constitutional rights: ")) 

#pretty incredible to see $1,000, $2,500, and $2,500 on here
D_settle_3 %>% filter(allegations %in%c("Dog owners claim civil rights violations by animal control","3 dogs shot during drug raid w/ warrant",
                                        "3 dogs shot during narcotics raid","4th and 14th amendment violations, narcotics search, dead dog"
                                        ,"German Shepherd Shot by Detroit Police Department","Officer shot chained dog","Police shooting of pit bull during raid",
                                        "Shooting of Dog","Shooting of Dogs")) %>% pull(amount_awarded)

##what kinds of cases brought in less than 1000 dollars? one where the plaintiff was shot in the leg. 
D_settle_3 %>% filter(is.na(amount_awarded)==FALSE) %>% filter(amount_awarded<1000) %>% pull(allegations)
