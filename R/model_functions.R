###########################################
#These functions either ready data to be
#used for fitting delay and incidence models,
#or actually fit those models.
###########################################


#This function outputs a dataset that can easily be used to
#make an incidence model labeling a miss as 1, and the index
#as 0. It takes as arguments:
#time_map: a formatted time map
#demo_data: demographic data for the enrollees
#change_point: the change_point to use when segmenting
#days_before: number of days before index to label as a miss


#Roxygen comments

#' Prepare a time map for incidence modeling
#'
#' @param time_map A time map in the format of the package example
#' @param demo_data A dataframe of demographic data for the enrollees
#' @param change_point The changepoint on which to base the dataset (must be <=-1)
#' @param days_before Number of days before index to start classifying as a miss
#' @return A dataframe containing the outcome "miss" and covariates for each row
#' @examples
#' get_incidence_data(time_map=final_time_map, , change_point=-90, days_before= -1)
get_incidence_data <- function(time_map, demo_data, change_point, days_before = -1){

  #Require certain packages
  #require(tidyverse)
  #require(delayDX)
  #require(lubridate)


  #Join this data to the time map and format
  #demographic data, extract age
  demo_time_map <- time_map %>% inner_join(demo_data, by='enrolid') %>%
    mutate(admdate= index_date + days_since_dx) %>%
    mutate(year=year(as_date(admdate))) %>%
    mutate(age=year-dobyr)

  #Try to clean up a little bit, and reformat
  demo_time_map <- demo_time_map %>%
    mutate(age_cat=cut(age,breaks = c(-1,17,35,45,55,65,130))) %>%
    mutate_at(vars(sex),funs(as.factor)) %>%
    mutate(year = year(as_date(admdate))) %>%
    mutate(emprel=as.factor(emprel)) %>%
    mutate(day=as.numeric(as_date(admdate)),
           year.factor=as.factor(year(as_date(admdate))),
           year=year(as_date(admdate)),
           month.factor=as.factor(month(as_date(admdate))),
           month=month(as_date(admdate)))

  #Get indicators for plan type and industry
  demo_time_map <- demo_time_map %>%
    mutate(plan = factor(plantyp,
                  levels = c(0, 2:9),
                  labels = c("Potentially Basic/major medical (0 in data, no 1",
                             "Comprehensive",
                             "Exclusive Provider",
                             "Health Maintenance ",
                             "NonCapitatedPointofService",
                             "Preferred Provider",
                             "Capitated/Partially",
                             "Consumer-DrivenPlan",
                             "High DeductiblePlan"))) %>%
    mutate(indus = factor(indstry,
                          levels = 1:7,
                          labels = c("Oil & Gas Extraction, Mining",
                                     "Manufacturing,DurGoods",
                                     "Manufacturing,NondurGoods",
                                     "Transportation,Comms,Util",
                                     "RetailTrade",
                                     "Finance,Insurance,RealEstate",
                                     "Services")))

  #get misses and non miss visits, for outcome we are modeling
  miss_cases <- demo_time_map %>%
    filter(any_ssd == 1,
           between(days_since_dx, change_point, days_before)) %>%
    mutate(miss=1L)

  non_miss_cases <- demo_time_map %>%
    filter(days_since_dx == 0) %>%
    mutate(miss=0L)

  final_model_data <- bind_rows(miss_cases,non_miss_cases)

  #Include indicators for different types of visits
  #Categories are: Out Only, In only, ED only, ED and Out, ED and In, In and Out, All 3
  #Reference will be outpatient only

  final_model_data <- final_model_data %>%
    mutate(visit_type = as.character(.01*outpatient + .1*inpatient + ED)) %>%
    mutate(visit_type = ifelse(visit_type=="0.01","Outpatient Only",visit_type)) %>%
    mutate(visit_type = ifelse(visit_type=="0.1","Inpatient Only",visit_type)) %>%
    mutate(visit_type = ifelse(visit_type=="1","ED Only",visit_type)) %>%
    mutate(visit_type = ifelse(visit_type=="1.01","ED and Outpatient Only",visit_type)) %>%
    mutate(visit_type = ifelse(visit_type=="1.1","ED and Inpatient Only",visit_type)) %>%
    mutate(visit_type = ifelse(visit_type=="0.11","Outpatient and Inpatient Only",visit_type)) %>%
    mutate(visit_type = ifelse(visit_type=="1.11","Outpatient, ED, and Inpatient",visit_type))
  #Put in factor form
  final_model_data$visit_type <- factor(final_model_data$visit_type,
                                        levels=c("Outpatient Only", "Inpatient Only", "ED Only",
                                                 "ED and Outpatient Only","ED and Inpatient Only",
                                                 "Outpatient and Inpatient Only",
                                                 "Outpatient, ED, and Inpatient"))

  return(final_model_data)

}



