

fit_cp_spline <- function(data,x,return_all=FALSE){

  fit_spline <- lm(Y ~ splines::bs(t,knots = c(x)), data = data)

  if (return_all==TRUE){

    fit_base <- RegBsplineAsPiecePoly(fit_spline, "splines::bs(t, knots = c(x))",
                                                    shift = FALSE)

    intercept <- coef(fit_spline)[[1]]

    pc <- fit_base$PiecePoly$coef

    preds_out <- data %>%
      dplyr::select(period,Y,t) %>%
      modelr::add_predictions(fit_spline) %>%
      dplyr::mutate(pred1=intercept+pc[1,1]+pc[2,1]*t+pc[3,1]*(t^2)++pc[4,1]*(t^3),
                    pred2=intercept+pc[1,2]+pc[2,2]*t+pc[3,2]*(t^2)++pc[4,2]*(t^3),
                    se_fit=predict(fit_spline, se.fit = TRUE)$se.fit,
                    pred_low=(pred - (2*se_fit)),
                    pred_high=(pred + (2*se_fit)))

    out <- list(fit=fit_spline,
                pred=preds_out,
                pred_base=fit_base$PiecePoly$coef,
                stats=broom::glance(fit_spline))
  } else {

    out <- broom::glance(fit_spline)
  }
  return(out)
}

fit_cp_cube <- function(data,x,return_all=FALSE){

  new_data <- data %>%
    dplyr::mutate(post=t>x,
                  shift=t-x,
                  shift2=shift^2,
                  shift3=shift^3)

  fit <- glm(Y~I(post):I(shift)+I(post):I(shift2)+I(post):I(shift3),data=new_data)

  ilink <- family(fit)$linkinv

  if (return_all==TRUE){

    preds_out <- new_data %>%
      dplyr::select(period,Y,t,post,shift,shift2,shift3) %>%
      modelr::add_predictions(fit) %>%
      dplyr::mutate(pred1=fit$coefficients["(Intercept)"] +
                      fit$coefficients["I(post)FALSE:I(shift)"]*shift +
                      fit$coefficients["I(post)FALSE:I(shift2)"]*shift2 +
                      fit$coefficients["I(post)FALSE:I(shift3)"]*shift3,
                    se_fit=predict(fit, type = "link", se.fit = TRUE)$se.fit,
                    pred_low=ilink(pred - (2*se_fit)),
                    pred_high=ilink(pred + (2*se_fit)),
                    pred=ilink(pred),
                    pred1=ilink(pred1))

    out <- list(fit=fit,
                pred=preds_out,
                stats=broom::glance(fit))
  } else {

    out <- broom::glance(fit)

  }

  return(out)
}

fit_cp_quad <- function(data,x,return_all=FALSE){

  new_data <- data %>%
    dplyr::mutate(post=t>x,
                  shift=t-x,
                  shift2=shift^2)

  fit <- glm(Y~I(post):I(shift)+I(post):I(shift2),data=new_data)

  ilink <- family(fit)$linkinv

  if (return_all==TRUE){

    preds_out <- new_data %>%
      dplyr::select(period,Y,t,post,shift,shift2) %>%
      modelr::add_predictions(fit) %>%
      dplyr::mutate(pred1=fit$coefficients["(Intercept)"] +
                      fit$coefficients["I(post)FALSE:I(shift)"]*shift +
                      fit$coefficients["I(post)FALSE:I(shift2)"]*shift2,
                    se_fit=predict(fit, type = "link", se.fit = TRUE)$se.fit,
                    pred_low=ilink(pred - (2*se_fit)),
                    pred_high=ilink(pred + (2*se_fit)),
                    pred=ilink(pred),
                    pred1=ilink(pred1))

    out <- list(fit=fit,
                pred=preds_out,
                stats=broom::glance(fit))
  } else {

    out <- broom::glance(fit)

  }

  return(out)
}

fit_cp_exp <- function(data,x,return_all=FALSE){

  fit <- glm(Y~I((t-x))+I((t-x)):I(t>x),family = gaussian("log"),data=data)

  ilink <- family(fit)$linkinv

  if (return_all==TRUE){

    preds_out <- data %>%
      dplyr::select(period,Y,t) %>%
      modelr::add_predictions(fit) %>%
      dplyr::mutate(pred1=(fit$coefficients[1] + fit$coefficients[2]*(t-x)),
                    se_fit=predict(fit, type = "link", se.fit = TRUE)$se.fit,
                    pred_low=ilink(pred - (2*se_fit)),
                    pred_high=ilink(pred + (2*se_fit)),
                    pred=ilink(pred),
                    pred1=ilink(pred1))

    out <- list(fit=fit,
                pred=preds_out,
                stats=broom::glance(fit))
  } else {

    out <- broom::glance(fit)

  }

  return(out)
}

fit_cp_lm <- function(data,x,return_all=FALSE){

  fit <- glm(Y~I(t-x)+I(t-x):I(t>x),data=data)

  ilink <- family(fit)$linkinv

  if (return_all==TRUE){

    preds_out <- data %>%
      dplyr::select(period,Y,t) %>%
      modelr::add_predictions(fit) %>%
      dplyr::mutate(pred1=fit$coefficients[1] + fit$coefficients[2]*(t-x),
                    se_fit=predict(fit, type = "link", se.fit = TRUE)$se.fit,
                    pred_low=ilink(pred - (2*se_fit)),
                    pred_high=ilink(pred + (2*se_fit)),
                    pred=ilink(pred),
                    pred1=ilink(pred1))

    out <- list(fit=fit,
                pred=preds_out,
                stats=broom::glance(fit))
  } else {

    out <- broom::glance(fit)

  }

  return(out)
}

fit_cp_lm_quad <- function(data,x,return_all=FALSE){

  # shift time
  new_data <- data %>%
    dplyr::mutate(t2=t>x,
                  shift=t-x,
                  shift2=shift^2*t2)

  fit <- glm(Y~shift+shift2,data=new_data)

  ilink <- family(fit)$linkinv

  if (return_all==TRUE){

    preds <- new_data %>%
      dplyr::select(period,Y,t,shift,shift2) %>%
      modelr::add_predictions(fit) %>%
      dplyr::mutate(pred1=fit$coefficients[1] + fit$coefficients[2]*shift,
                    se_fit=predict(fit, type = "link", se.fit = TRUE)$se.fit,
                    pred_low=ilink(pred - (2*se_fit)),
                    pred_high=ilink(pred + (2*se_fit)),
                    pred=ilink(pred),
                    pred1=ilink(pred1))

    out <- list(fit=fit,
                pred=preds,
                stats=broom::glance(fit))

  } else {

    out <- broom::glance(fit)

  }

  return(out)
}

fit_cp_lm_cube <- function(data,x,return_all=FALSE){

  # shift time
  new_data <- data %>%
    dplyr::mutate(t2=t>x,
                  shift=t-x,
                  shift2=shift^2*t2,
                  shift3=shift^3*t2)

  fit <- glm(Y~shift+shift2+shift3,data=new_data)
  ilink <- family(fit)$linkinv

  if (return_all==TRUE){

    preds <- new_data %>%
      dplyr::select(period,Y,t,shift,shift2,shift3) %>%
      modelr::add_predictions(fit) %>%
      dplyr::mutate(pred1=fit$coefficients[1] + fit$coefficients[2]*shift,
                    se_fit=predict(fit, type = "link", se.fit = TRUE)$se.fit,
                    pred_low=ilink(pred - (2*se_fit)),
                    pred_high=ilink(pred + (2*se_fit)),
                    pred=ilink(pred),
                    pred1=ilink(pred1))

    out <- list(fit=fit,
                pred=preds,
                stats=broom::glance(fit))

  } else {

    out <- broom::glance(fit)

  }

  return(out)
}

#' Compute simulation trial visit table
#'
#'
#' @export
compute_sim_trial_n_visit_table <- function(sim_miss_num_data,total_patients){
  ## Compute bins for number of missed visits ##
  visit <- tibble::tibble(n_vis_groups = 1:5) %>%
    dplyr::mutate(n=purrr::map(n_vis_groups, ~ dplyr::filter(sim_miss_num_data, n_vis >= .) %>% nrow())) %>%
    tidyr::unnest(n) %>%
    dplyr::mutate(percent = n/total_patients*100)

  ## Aggregate number of missed visits table ##
  n_miss <- visit %>%
    rbind(visit %>%
            dplyr::filter(n_vis_groups==1) %>%
            dplyr::mutate(n_zero=total_patients-n, percent_zero = (n_zero/total_patients)*100) %>%
            dplyr::mutate(n_vis_groups = 0)  %>%
            dplyr::select(n_vis_groups, n = n_zero, percent = percent_zero)) %>%
    dplyr::arrange(n_vis_groups) %>%
    dplyr::mutate(n_vis_groups=ifelse(n_vis_groups !=0, paste0(">= ", n_vis_groups), as.character(n_vis_groups))) %>%
    rbind(sim_miss_num_data %>%
            dplyr::mutate(n_vis_groups=ifelse(n_vis>=mean(sim_miss_num_data$n_vis),
                                              ">= Mean", "Greater that or equal to mean")) %>%
            dplyr::group_by(n_vis_groups) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            dplyr::mutate(percent=n/total_patients*100) %>%
            dplyr::filter(n_vis_groups==">= Mean")) %>%
    rbind(sim_miss_num_data %>%
            dplyr::mutate(n_vis_groups=ifelse(n_vis>=median(sim_miss_num_data$n_vis),
                                              ">= Median", "Greater that or equal to median")) %>%
            dplyr::group_by(n_vis_groups) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            dplyr::mutate(percent=n/total_patients*100) %>%
            dplyr::filter(n_vis_groups==">= Median"))

  return(n_miss)
}

#' Compute simulation trial duration table
#'
#'
#' @export
compute_sim_trial_duration_table <- function(sim_miss_num_data, upper_bound){
  ## compute range of miss durations ##
  # use change point to define upper bound of range
  x <-  cut(0:upper_bound, 10)

  range <- c(0)
  for (i in levels(x)){
    y <- str_split(i, ",")
    y <- y[[1]][2]
    z <- floor(as.numeric(str_remove(y, "]")))
    range <- c(range, z)
  }

  ## compute table of miss durations ##
  duration_miss <- tibble::tibble(bins= range) %>%
    dplyr::mutate(n=purrr::map(bins, ~dplyr::filter(sim_miss_num_data, dur>= .) %>%
                                 nrow())) %>%
    tidyr::unnest(n) %>%
    dplyr::mutate(percent=n/nrow(sim_miss_num_data)*100) %>%
    dplyr::mutate(bins = paste0(">= ", bins)) %>%
    rbind(tibble::tibble(bins = ">= Mean",
                         n =dplyr::filter(sim_miss_num_data,dur >= mean(sim_miss_num_data$dur)) %>%
                           nrow(),
                         percent = n/nrow(sim_miss_num_data)*100)) %>%
    rbind(tibble::tibble(bins = ">= Median",
                         n =filter(sim_miss_num_data, dur >= median(sim_miss_num_data$dur)) %>%
                           nrow(),
                         percent = n/nrow(sim_miss_num_data)*100))

  return(duration_miss)
}

# a helper function for drawing enrollees for the second simulation
draw_enrollees <- function(enrolid_list1,enrolid_list2,num_draw,new_samp_prob=NA){

  # whole number of cases to draw
  whole_draw <- num_draw%/%1+as.integer(runif(1)<num_draw%%1)

  # size list 1
  nl1 <- length(enrolid_list1)
  nl2 <- length(enrolid_list2)

  if (is.na(new_samp_prob)){
    if (nl1>=whole_draw){

      out_list <- sample(x = enrolid_list1,
                         size = whole_draw,
                         replace = FALSE)
    } else {
      out_list1 <- enrolid_list1
      out_list2 <- sample(enrolid_list2,whole_draw-nl1,replace = FALSE)
      out_list <- c(out_list1,out_list2)
    }
  } else {
    # if new_samp_prob is supplied then sample from the two groups based on probability values

    # determine inital number to draw from each group
    g1_draw_count <- sum(sample(x = c(1,2), size = whole_draw,
                                prob = c(1-new_samp_prob,new_samp_prob),
                                replace=TRUE)==1)

    g2_draw_count <- whole_draw-g1_draw_count

    if (g1_draw_count<= nl1 & g2_draw_count<= nl2){
      # all is good draw according to these values
      out_list1 <- sample(x = enrolid_list1, size = g1_draw_count, replace = FALSE)

      out_list2 <- sample(x = enrolid_list2, size = g2_draw_count, replace = FALSE)

      out_list <- c(out_list1,out_list2)

    } else if (g1_draw_count> nl1) {
      # draw from group 1 is larger than available

      # draw all of group 1
      out_list1 <- enrolid_list1

      # draw remaining from group 2
      out_list2 <- sample(x = enrolid_list2, size = whole_draw-nl1, replace = FALSE)

      out_list <- c(out_list1,out_list2)


    } else {
      # draw from group 2 is larger than available

      # draw all of group 1
      out_list2 <- enrolid_list2

      # draw remaining from group 2
      out_list1 <- sample(x = enrolid_list1, size = whole_draw-nl2, replace = FALSE)

      out_list <- c(out_list1,out_list2)

    }

  }



  return(out_list)
}


