####################
# GENERAL STAN SPECS
####################
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

###################
# GENERAL FUNCTIONS
###################
read_file <- function(...) {
  read.csv(..., header = TRUE, stringsAsFactors = FALSE)
}

make_cn_data <- function(data) {
  data %>%
    dplyr::mutate(C_molar = C_ug * 1e-6 / 12.0107,
                  N_molar = N_ug * 1e-6 / 14.0067,
                  CN_ratio = C_molar / N_molar)
}

make_cn_pond_stats <- function(data) {
  data %>%
    dplyr::mutate(shape = ifelse(sample == "phytoplankton", 21, 22),
                  sample = Hmisc::capitalize(sample)) %>%
    dplyr::group_by(sample, treatment, pond, shape) %>%
    dplyr::summarise(mean = mean(CN_ratio))
}

quantile_hdi <- function(x, probs) {
  out <- HDInterval::hdi(x, credMass = max(probs) - min(probs))
  names(out) <- paste0(probs * 100, "%")
  attr(out, "credMass") <- NULL
  out
}

extract_em <- function(model) {
  suppressWarnings(emmeans::emtrends(model,
                                     pairwise ~ treatment,
                                     var = "day")$emtrends) %>%
    data.frame(row.names = "treatment") %>%
    dplyr::select("Estimate" = "day.trend",
                  "Q2.5" = "lower.HPD",
                  "Q97.5" = "upper.HPD")
}

#######
# STATS
#######
create_new_inits <- function(seed, type) {
  nms <- c("hyper_sd_ln_ka", "hyper_sd_logit_ke", "hyper_sd_logit_phi",
           "ln_ka_amb", "logit_ke_amb", "logit_phi_amb", "r_ln_ka_amb",
           "r_logit_ke_amb", "r_ln_phi_amb", "ln_ka_war", "logit_ke_war",
           "logit_phi_war", "r_ln_ka_war", "r_logit_ke_war", "r_ln_phi_war",
           "sigma")
  if (type == "zooplankton") {
    nms <- gsub("ln_phi", "logit_phi", nms)
  }
  set.seed(seed)
  hyper_sd_ln_ka <- rgamma(1, 2, 0.1)
  hyper_sd_logit_ke <- rgamma(1, 2, 0.1)
  hyper_sd_logit_phi <- rgamma(1, 2, 0.1)
  sigma <- rgamma(1, 2, 0.1)
  ln_ka <- rnorm(1, 0, 1)
  logit_ke <- rnorm(1, 0, 1)
  logit_phi <- rnorm(1, 0, 1)
  r_ln_ka <- rnorm(8, 0, hyper_sd_ln_ka)
  r_logit_ke <- rnorm(8, 0, hyper_sd_logit_ke)
  r_ln_phi <- rnorm(8, 0, hyper_sd_logit_phi)
  out <- list(hyper_sd_ln_ka, hyper_sd_logit_ke, hyper_sd_logit_phi,
              ln_ka, logit_ke, logit_phi, r_ln_ka, r_logit_ke, r_ln_phi,
              ln_ka, logit_ke, logit_phi, r_ln_ka, r_logit_ke, r_ln_phi,
              sigma)
  names(out) <- nms
  out
}

run_eqn1_stan <- function(data, ...) {
  plyr::dlply(data, .(sample), function(x, stan_list) {
    stan_data <- list(N = nrow(x),
                      Y = x$add15N_perc,
                      Time_days = x$day,
                      NJ = length(unique(x$pond)),
                      half_NJ = length(unique(x$pond)) / 2,
                      J = as.integer(as.factor(x$pond)))
    type <- unique(x$sample)
    model_code <- stan_list[[type]]
    init_list <- list(create_new_inits(seed = 10, type),
                      create_new_inits(seed = 23, type),
                      create_new_inits(seed = 31, type),
                      create_new_inits(seed = 40, type))
    set.seed(15)
    model <- rstan::stan(model_code = model_code,
                         data = stan_data,
                         chains = 4,
                         cores = 4,
                         iter = 3e4,
                         warmup = 25e3,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 20),
                         init = init_list)
    list("data" = x, "model" = model)
  }, ...)
}

calc_eqn_1 <- function(day, ka, ke, phi) {
  phi * ka * ke * (exp(-ke * day) - exp(-ka * day)) / (ka - ke)
}

calc_eqn_2 <- function(day, ka, ke, phi) {
  chi <- calc_eqn_1(day, ka, ke, phi)
  # dose / volume, i.e. initial concentration
  x <- phi * ke
  chi / (x * (1 - exp(-ka * day)))
}

calc_eqn_3 <- function(day = 54, ka, ke, phi) {
  # 54 is duration of experiment
  integrate(calc_eqn_2,
            lower = 0,
            upper = day,
            ka = ka,
            ke = ke,
            phi = phi)$value / day
}

extract_pp_par <- function(stan_output, ...) {
  plyr::ldply(stan_output, function(x, param) {
    dat <- as.data.frame(x$model)
    post_par <- matrix(0, nrow(dat), 2)
    for (i in seq_len(ncol(post_par))) {
      post_par[, i] <- dat[, paste0(param, ifelse(i == 1, "amb", "war"))]
    }
    data.frame(sample = unique(x$data$sample),
               iter = rep(seq_len(nrow(dat)), 2),
               treatment = rep(c("A", "H"), each = nrow(dat)),
               param = c(post_par[, 1], post_par[, 2]),
               stringsAsFactors = FALSE) %>%
      dplyr::arrange(iter, treatment)
  }, ...)
}

extract_pp_eff <- function(stan_output) {
  plyr::ldply(stan_output, function(x) {
    dat <- as.data.frame(x$model)
    effs <- matrix(0, nrow(dat), 2)
    for (i in seq_len(ncol(effs))) {
      pars <- paste0(c("ka_", "ke_", "phi_"), ifelse(i == 1, "amb", "war"))
      ka <- dat[, pars[1]]
      ke <- dat[, pars[2]]
      phi <- dat[, pars[3]]
      for (j in seq_along(ka)) {
        effs[j, i] <- calc_eqn_3(day = 54, ka[j],
                                 ke[j], phi[j])
      }
    }
    data.frame(sample = unique(x$data$sample),
               iter = rep(seq_len(nrow(dat)), 2),
               treatment = rep(c("A", "H"), each = nrow(dat)),
               param = c(effs[, 1], effs[, 2]),
               stringsAsFactors = FALSE) %>%
      dplyr::arrange(iter, treatment)
  })
}

make_pond_stats <- function(data) {
  data %>%
    dplyr::mutate(C_ug_L = C_ug / sample_vol_L,
                  N_ug_L = N_ug / sample_vol_L) %>%
    dplyr::group_by(sample, pond, treatment, day) %>%
    dplyr::summarise(av_N_ug_L = mean(N_ug_L),
                     av_C_ug_L = mean(C_ug_L),
                     ln_av_N_ug_L = log(av_N_ug_L),
                     ln_av_C_ug_L = log(av_C_ug_L))
}

make_polygon_data <- function(data) {
  data.frame(x = c(data$x, rev(data$x)),
             y_cred = c(data$Q2.5, rev(data$Q97.5)),
             stringsAsFactors = FALSE)
}

extract_pp_means <- function(stan_output, ...) {
  data <- plyr::ldply(stan_output, function(model_list, ...) {
    dat_pos <- as.data.frame(model_list$model)
    treats <- list("amb" = "amb", "war" = "war")
    plyr::ldply(treats, function(x, dat_pos, x_seq) {
      name_phi <- paste0("phi_", x)
      name_ka <- paste0("ka_", x)
      name_ke <- paste0("ke_", x)
      preds <- matrix(0, nrow(dat_pos), length(x_seq))
      for (j in seq_len(nrow(preds))) {
        preds[j, ] <- calc_eqn_1(day = x_seq,
                                 ka = dat_pos[[name_ka]][j],
                                 ke = dat_pos[[name_ke]][j],
                                 phi = dat_pos[[name_phi]][j])
      }
      quantiles <- apply(preds, 2, quantile, probs = c(0.025, 0.975))
      data.frame("x" = x_seq,
                 "mean" = colMeans(preds),
                 "Q2.5" = quantiles["2.5%", ],
                 "Q97.5" = quantiles["97.5%", ],
                 stringsAsFactors = FALSE)
    }, dat_pos = dat_pos, .id = "treatment", ...)
  }, ...)
  list(lines = data %>%
         dplyr::select(-c(Q2.5, Q97.5)),
       polygons = data %>%
         plyr::ddply(.(sample, treatment), make_polygon_data))
}

extract_pp_ponds <- function(data, stan_output, x_seq) {
  ponds <- data %>%
    dplyr::distinct(pond, treatment) %>%
    dplyr::arrange(treatment, pond)
  mat_phy <- as.data.frame(stan_output$phytoplankton$model)
  mat_zoo <- as.data.frame(stan_output$zooplankton$model)
  preds <- vector(mode = "list", length = nrow(ponds))
  for (i in seq_len(nrow(ponds))) {
    name_phi <- paste0("r_phi[", i, "]")
    name_ka <- paste0("r_ka[", i, "]")
    name_ke <- paste0("r_ke[", i, "]")
    predictions_phy <- matrix(0, nrow(mat_phy), length(x_seq))
    predictions_zoo <- predictions_phy
    for (j in seq_len(nrow(predictions_phy))) {
      predictions_phy[j, ] <- calc_eqn_1(day = x_seq,
                                         ka = mat_phy[[name_ka]][j],
                                         ke = mat_phy[[name_ke]][j],
                                         phi = mat_phy[[name_phi]][j])
      predictions_zoo[j, ] <- calc_eqn_1(day = x_seq,
                                         ka = mat_zoo[[name_ka]][j],
                                         ke = mat_zoo[[name_ke]][j],
                                         phi = mat_zoo[[name_phi]][j])
    }
    quantiles_phy <- predictions_phy %>%
      apply(2, quantile, probs = c(0.025, 0.975))
    mod_pred_phy <- data.frame(mean = colMeans(predictions_phy),
                               Q2.5 = quantiles_phy["2.5%", ],
                               Q97.5 = quantiles_phy["97.5%", ])
    quantiles_zoo <- predictions_zoo %>%
      apply(2, quantile, probs = c(0.025, 0.975))
    mod_pred_zoo <- data.frame(mean = colMeans(predictions_zoo),
                               Q2.5 = quantiles_zoo["2.5%", ],
                               Q97.5 = quantiles_zoo["97.5%", ])

    preds[[i]] <- data.frame(sample = rep(c("phytoplankton", "zooplankton"),
                                          each = nrow(mod_pred_phy) * 2),
                             x = c(x_seq, rev(x_seq), x_seq, rev(x_seq)),
                             y_conf = c(mod_pred_phy$Q2.5,
                                        rev(mod_pred_phy$Q97.5),
                                        mod_pred_zoo$Q2.5,
                                        rev(mod_pred_zoo$Q97.5)),
                             y_mean = c(mod_pred_phy$mean,
                                        rep(NA, nrow(mod_pred_phy)),
                                        mod_pred_zoo$mean,
                                        rep(NA, nrow(mod_pred_zoo))),
                             stringsAsFactors = FALSE)
  }
  list(preds = preds,
       ponds = ponds)
}

calculate_diffs <- function(data, decline = TRUE) {
  if (decline) {
    fct <- function(x) 1 - (x[2] / x[1])
  } else {
    fct <- function(x) (x[2] / x[1]) - 1
  }
  # differences are calculated as
  # 1 - (H / A), so positive numbers
  # mean decline with warming (H), neg
  # numbers mean increase with warming
  tapply(data$param,
         list(data$iter,
              data$sample),
         fct) %>%
    plyr::adply(2, function(x) {
      qts <- quantile(x, probs = c(0.05, 1))
      round(c(mean = mean(x),
              median = median(x),
              "5%" = qts[["5%"]],
              "100%" = qts[["100%"]]) * 1e2, 1)
    }, .id = "sample")
}

run_biomass_model <- function(data) {
  type <- unique(data$sample)
  if (type == "phytoplankton") {
    data <- data %>%
      dplyr::filter(pond != 14)
  }
  brms::brm(ln_av_C_ug_L ~ treatment + (1 + treatment | pond),
            data = data,
            chains = 4,
            cores = 4,
            iter = 3e4,
            warmup = 25e3,
            control = list(adapt_delta = 0.99,
                           max_treedepth = 20))
}

calc_biomass_diff <- function(type, model_list) {
  post_model <- brms::posterior_samples(model_list[[type]])
  data.frame(sample = type,
             iter = seq_len(nrow(post_model)),
             diff = 1 - exp(post_model[, "b_treatmentH"]),
             stringsAsFactors = FALSE)
}

rounded_percent <- function(x) {
  LoLinR::rounded(x * 100, 1)
}

calc_biomass_diff_summary <- function(data) {
  intervals <- quantile_hdi(data$diff, probs = c(0.025, 0.975))
  data.frame(median = rounded_percent(median(data$diff)),
             lower = rounded_percent(intervals[["2.5%"]]),
             upper = rounded_percent(intervals[["97.5%"]]),
             stringsAsFactors = FALSE)
}

run_cn_model <- function(data) {
  mod_phy <- brms::brm(CN_ratio ~ 1 + day * treatment + (1 + day | pond),
                       data = dplyr::filter(data, sample == "phytoplankton"),
                       chains = 4,
                       cores = 4,
                       iter = 3e4,
                       warmup = 25e3)
  mod_zoo <- update(mod_phy,
                    newdata = dplyr::filter(data, sample == "zooplankton"))
  list(mod_phy = mod_phy,
       mod_zoo = mod_zoo)
}

run_baci_co2 <- function(data) {
  brms::brm(influx ~ period * treatment + (1 | pond),
            data = data, chains = 4, cores = 4,
            iter = 3e4, warmup = 25e3,
            control = list(adapt_delta = 0.99,
                           max_treedepth = 20))
}

run_permanova_community <- function(data) {
  treat_rest <- data$treatment
  data <- data %>%
    dplyr::select(-pond, -treatment)
  bc_all <- vegan::vegdist(data, method = "bray")
  list(out = vegan::adonis(bc_all ~ treat_rest, permutations = 1e3))
}

run_baci_nutrients <- function(data) {
  data <- data %>%
    dplyr::filter(sample_date != as.Date("2013-07-16") & treatment != "C") %>%
    dplyr::mutate(period = ifelse(sample_date >= as.Date("2013-07-09") &
                                    sample_date < as.Date("2013-07-16"),
                                  "Before", "After"),
                  y = 1)
  chosen_vars <- c("NO2_uM", "NO3_uM", "NH3_uM")
  model <- brms::brm(y ~ period * treatment + (1 | pond),
                     data = data, chains = 0)
  baci_models <- plyr::llply(chosen_vars, function(x, data, model) {
    data$y <- data[[x]]
    data <- data %>%
      dplyr::select(y, period, treatment, pond) %>%
      tidyr::drop_na()
    update(model, newdata = data,
           chains = 4, cores = 4,
           iter = 3e4, warmup = 25e3,
           control = list(adapt_delta = 0.99,
                          max_treedepth = 20))
  }, data = data, model = model)
  names(baci_models) <- chosen_vars
  baci_models
}

make_params_summary <- function(data) {
  data %>%
    plyr::ddply(.(sample, treatment), function(data) {
        x <- quantile_hdi(data$param, probs = c(0.025, 0.975))
        data.frame(mean = LoLinR::rounded(mean(data$param), 2),
                   median = LoLinR::rounded(median(data$param), 2),
                   lower = LoLinR::rounded(x[["2.5%"]], 2),
                   upper = LoLinR::rounded(x[["97.5%"]], 2))
    })
}

extract_pp_matrices <- function(data, stan_output) {
  mat_phy <- as.data.frame(stan_output$phytoplankton$model)
  mat_zoo <- as.data.frame(stan_output$zooplankton$model)
  resids <- matrix(0, nrow(mat_phy), nrow(data))
  preds <- resids
  name_phi <- paste0("r_phi[", data$pond, "]")
  name_ka <- paste0("r_ka[", data$pond, "]")
  name_ke <- paste0("r_ke[", data$pond, "]")
  for (i in seq_len(nrow(preds))) {
    mat_phy_ka_i <- mat_phy[i, name_ka]
    mat_zoo_ka_i <- mat_zoo[i, name_ka]
    ka_i <- unlist(ifelse(data$sample == "phytoplankton",
                          mat_phy_ka_i, mat_zoo_ka_i))
    mat_phy_ke_i <- mat_phy[i, name_ke]
    mat_zoo_ke_i <- mat_zoo[i, name_ke]
    ke_i <- unlist(ifelse(data$sample == "phytoplankton",
                          mat_phy_ke_i, mat_zoo_ke_i))
    mat_zoo_phi_i <- mat_zoo[i, name_phi]
    mat_phy_phi_i <- mat_phy[i, name_phi]
    phi_i <- unlist(ifelse(data$sample == "phytoplankton",
                          mat_phy_phi_i, mat_zoo_phi_i))
    preds[i, ] <- calc_eqn_1(day = data$day, ka = ka_i,
                             ke = ke_i, phi = phi_i)
    resids[i, ] <- data$add15N_perc - preds[i, ]
  }
  list(preds = preds,
       resids = resids)
}

run_residual_model <- function(data, nutrients, stan_output) {
  pp_matrices <- extract_pp_matrices(data, stan_output)
  data <- data %>%
    dplyr::mutate(preds = colMeans(pp_matrices$preds),
                  resids = colMeans(pp_matrices$resids),
                  sample_date = date %>%
                    gsub("/13$", "/2013", .) %>%
                    as.Date(format = "%d/%m/%Y"))
  nutrients <- nutrients %>%
    dplyr::mutate(pond = as.numeric(gsub("^P", "", pond)))
  data <- plyr::ddply(data,
                      .(dorset_pond, sample_date),
                      function(df, nutrients) {
    nuts <- nutrients %>%
      dplyr::filter(sample_date == unique(df$sample_date),
                    pond == unique(df$dorset_pond)) %>%
      dplyr::select(NO2_uM, NO3_uM, NH3_uM) %>%
      colMeans(na.rm = TRUE)
    df %>%
      dplyr::mutate(NO2_uM = nuts[["NO2_uM"]],
                    NO3_uM = nuts[["NO3_uM"]],
                    NH3_uM = nuts[["NH3_uM"]])
  }, nutrients) %>%
    dplyr::select(sample, pond, treatment, day,
                  preds, resids, NO2_uM, NO3_uM, NH3_uM) %>%
    dplyr::rename(predicted_15N_perc = preds,
                  residual_15N_perc = resids)
  plyr::dlply(data, .(sample), function(df) {
    brms::brm(residual_15N_perc ~ NO2_uM + NO3_uM + NH3_uM +
                (1 + NO2_uM + NO3_uM + NH3_uM | pond),
              data = df,
              chains = 4,
              cores = 4,
              iter = 3e4,
              warmup = 25e3,
              control = list(adapt_delta = 0.99,
                             max_treedepth = 20))
  })
}
