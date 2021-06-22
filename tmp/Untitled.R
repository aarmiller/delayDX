

time_map_data <- time_map
cp <- 21
out <- prep_sim_data(time_map_data, by_days=1, start_day=1, event_name = "any_ssd", cp_method = "set_cp", specify_cp = cp,
                     set_cp_method = "cubic")

out$change_point <- 21
sim_miss_visits(out)

sim_dat <- out[c("time_map","miss_bins_visits","change_point","total_patients")]

sim_miss_visits(sim_dat)

save(sim_dat,file = "../delaySim/data/sim_data.RData")

miss_draw <- out$time_map %>%
  dplyr::filter(miss_ind==1) %>%  # filter to miss (i.e., SSD) visits
  dplyr::inner_join(dplyr::select(out$miss_bins_visits,period, num_miss), by = "period") %>% # join in number missed by bin
  dplyr::mutate(rand = runif(n = n())) %>% # draw random number
  dplyr::arrange(period, rand) %>% # arrange by random draw
  dplyr::group_by(period) %>%
  dplyr::filter(row_number() <= num_miss) %>% # filter to the number in each bin corresponding to number missed
  dplyr::ungroup()

sim_miss_num <- miss_draw %>%
  dplyr::group_by(enrolid_new) %>%
  dplyr::summarise(n_vis = dplyr::n(),
                   n_vis_out = sum(ed==0 & inpatient==0),
                   n_vis_ed = sum(ed==1),
                   n_vis_inpatient = sum(inpatient==1),
                   dur = max(-(days_since_dx)))


tmp_num_not_drawn <- out$total_patients-nrow(sim_miss_num)

# stats when 0 miss patients are included
w0_stats <- sim_miss_num %>%
  dplyr::full_join(tibble(enrolid_new=rep(-99,tmp_num_not_drawn)),by = "enrolid_new") %>%
  dplyr::mutate(across(.cols = everything(),~replace_na(.,0))) %>%
  dplyr::summarise_at(dplyr::vars(-enrolid_new),list(mean_w0=mean,median_w0=median))

## build table of number of missed visits
sim_trial_n_visit_table <- compute_sim_trial_n_visit_table(sim_miss_num_data = sim_miss_num,
                                                           total_patients = out$total_patients)


## build table of dureation of missed visits
sim_trial_durration_table <- compute_sim_trial_duration_table(sim_miss_num_data = sim_miss_num,
                                                              upper_bound = out$change_point)
#sim_trial_durration_table <- NULL

## Compute summary statistics across all patients ###
miss_summary <- sim_miss_num %>%
  dplyr::summarise(n_pat = n(),                          # number of patients missed
                   mean_n_vis = mean(n_vis),             # mean number of misses per patient
                   median_n_vis =median(n_vis),          # median number of misses per patient
                   mean_n_vis_out = mean(n_vis_out),     # mean number of ed misses per patient
                   median_n_vis_out =median(n_vis_out),  # median number of ed misses per patient
                   mean_n_vis_ed = mean(n_vis_ed),       # mean number of ed misses per patient
                   median_n_vis_ed =median(n_vis_ed),    # median number of ed misses per patient
                   mean_n_vis_inpatient = mean(n_vis_inpatient),     # mean number of ed misses per patient
                   median_n_vis_inpatient = median(n_vis_inpatient),  # median number of ed misses per patient
                   min_dur = min(dur),           # min duration
                   mean_dur = mean(dur),         # mean miss duration
                   median_dur = median(dur),     # median miss duration
                   max_dur = max(dur),
                   n_vis = sum(n_vis),
                   n_vis_out = sum(n_vis_out),
                   n_vis_ed = sum(n_vis_ed),
                   n_vis_inpatient = sum(n_vis_inpatient)) %>%            # max miss duration
  dplyr::bind_cols(w0_stats)

## Sim Duration for Regression ##
if (sim_duartion_for_regression == TRUE){

  key <- time_map %>%
    dplyr::distinct(enrolid,enrolid_new)

  dur_model_boot <- sim_miss_num %>%
    dplyr::select(enrolid_new,dur) %>%
    dplyr::inner_join(key) %>%
    dplyr::select(-enrolid_new)

} else{
  dur_model_boot <- NULL
}

return(list(sum_n_miss=sim_trial_n_visit_table,
            sum_duration=sim_trial_durration_table,
            miss_summary=miss_summary,
            dur_model_boot=dur_model_boot))
