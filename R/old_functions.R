# run_sim_miss <- function(pred_data,time_map_data,n_reps=500,cores=6){
#
#   high_vals <- filter(pred_data,change_period==TRUE) %>% .$high
#
#   sim_window <- tibble(days_before=1:max(high_vals)) %>%
#     mutate(high=map_dbl(days_before,~min( high_vals[high_vals>= .]))) %>%
#     mutate(days_since_dx=-days_before) %>%
#     inner_join(select(pred_data,low,high,t),by="high")
#
#   sim_data <- time_map_data %>%
#     inner_join(sim_window,by="days_since_dx") %>%
#     filter(any_miss_ind==1,days_since_dx<0)
#
#   len_sim_data <- nrow(sim_data)
#
#   temp_sim <- function(){
#     sim_vals <- filter(pred_data,change_period==TRUE) %>%
#       mutate(excess1=pred1-expected1) %>%
#       mutate(sim1_val=n_visits*map2_dbl(excess1,excess_se,~rnorm(1,.x,.y)),      # draw an excess number of cases
#              sim2_val=n_visits*map2_dbl(expected1,expected_se,~rnorm(1,.x,.y)),  # draw expected rate then compute expected number
#              sim3_val=map2_dbl(pred2,expected_se2,~rnorm(1,.x,.y))) %>%          # draw expected number not accounting for rate
#       select(t,sim1_val:sim3_val)
#
#     sim_set <- sim_data %>%
#       filter(any_miss_ind==1) %>%
#       mutate(rand=runif(len_sim_data)) %>%
#       arrange(t,rand) %>%
#       group_by(t) %>%
#       mutate(sim_num=row_number()) %>%
#       inner_join(sim_vals,by="t") %>%
#       mutate(miss_sim1=sim_num<=sim1_val,  # number of misses are just the excess, so look at these first few cases
#              miss_sim2=sim_num>=sim2_val,  # number of misses are those that exceed the expected
#              miss_sim3=sim_num>=sim3_val) %>%  # number of misses are those that exceed the expected
#       ungroup()
#
#     sim_res <- bind_cols(sim_set %>%
#                            mutate(dur1=ifelse(miss_sim1,-days_since_dx,NA),
#                                   dur2=ifelse(miss_sim2,-days_since_dx,NA),
#                                   dur3=ifelse(miss_sim3,-days_since_dx,NA)) %>%
#                            summarise(n_pat1=n_distinct(miss_sim1*enrolid),
#                                      n_pat2=n_distinct(miss_sim2*enrolid),
#                                      n_pat3=n_distinct(miss_sim3*enrolid),
#                                      n_vis1=sum(miss_sim1),
#                                      n_vis2=sum(miss_sim2),
#                                      n_vis3=sum(miss_sim3),
#                                      mn_dur1=mean(dur1,na.rm=T),
#                                      md_dur1=median(dur1,na.rm=T),
#                                      max_dur1=max(dur1,na.rm=T),
#                                      min_dur1=min(dur1,na.rm=T),
#                                      mn_dur2=mean(dur2,na.rm=T),
#                                      md_dur2=median(dur2,na.rm=T),
#                                      max_dur2=max(dur2,na.rm=T),
#                                      min_dur2=min(dur2,na.rm=T),
#                                      mn_dur3=mean(dur3,na.rm=T),
#                                      md_dur3=median(dur3,na.rm=T),
#                                      max_dur3=max(dur3,na.rm=T),
#                                      min_dur3=min(dur3,na.rm=T)),
#                          sim_set %>%
#                            group_by(enrolid) %>%
#                            summarise(miss1=sum(miss_sim1),
#                                      miss2=sum(miss_sim2),
#                                      miss3=sum(miss_sim3)) %>%
#                            mutate_at(vars(miss1:miss3),funs(ifelse(.==0,NA,.))) %>%
#                            summarise(mn_miss1=mean(miss1,na.rm=T),
#                                      mn_miss2=mean(miss2,na.rm=T),
#                                      mn_miss3=mean(miss3,na.rm=T),
#                                      md_miss1=median(miss1,na.rm=T),
#                                      md_miss2=median(miss2,na.rm=T),
#                                      md_miss3=median(miss3,na.rm=T),
#                                      min_miss1=min(miss1,na.rm=T),
#                                      min_miss2=min(miss2,na.rm=T),
#                                      min_miss3=min(miss3,na.rm=T),
#                                      max_miss1=max(miss1,na.rm=T),
#                                      max_miss2=max(miss2,na.rm=T),
#                                      max_miss3=max(miss3,na.rm=T)),
#                          sim_set %>%
#                            group_by(enrolid) %>%
#                            summarise(miss1=sum(miss_sim1),
#                                      miss2=sum(miss_sim2),
#                                      miss3=sum(miss_sim3)) %>%
#                            summarise(mn_miss1_1=mean(miss1,na.rm=T),
#                                      mn_miss2_1=mean(miss2,na.rm=T),
#                                      mn_miss3_1=mean(miss3,na.rm=T)))
#
#     return(sim_res)
#   }
#
#   cluster <- create_cluster(cores = cores)
#   reps <- tibble(iter=1:n_reps) %>%
#     partition(iter, cluster = cluster)
#
#   cluster_copy(reps,temp_sim)
#   cluster_library(reps, "tidyverse")
#   sim_out <- reps %>%
#     do(res = temp_sim()) %>%
#     collect() %>%
#     unnest()
# }


# get_window <- function(days,count_group,hiv_group){
#   out_data <- all_counts %>%
#     dplyr::filter(by_days==days,
#                   count_group==count_group,
#                   hiv_group==hiv_group) %>%
#     tidyr::unnest() %>%
#     dplyr::filter(high<=270) %>%
#     find_change_point("frac_miss","AIC")
#
#   return(out_data$change_point)
# }

# simpleCap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1,1)), substring(s, 2),
#         sep="", collapse=" ")
# }
