organise_fits <- function(brmsmodel) {
  emmeans::emmeans(brmsmodel, pairwise ~ treatment | period)$emmeans %>%
    data.frame() %>%
    dplyr::arrange(treatment, desc(period))
}

export_csv <- function(object, tab_out_folder, out_file, verbose = TRUE) {
  out_path <- file.path(tab_out_folder, out_file)
  if (verbose) {
    message("Saving ", out_path, " to file")
  }
  write.csv(object, out_path, row.names = FALSE)
  object
}

make_ed_table_1 <- function(stan_output, ...) {
  group_tables <- plyr::llply(names(stan_output), function(x, model) {
    out_names <- c("ln_ka_amb", "logit_ke_amb",
                   "logit_phi_amb", "ln_ka_war",
                   "logit_ke_war", "logit_phi_war",
                   "sigma", "hyper_sd_ln_ka",
                   "hyper_sd_logit_ke",
                   "hyper_sd_ln_phi")
    if (x == "zooplankton") {
      out_names <- gsub("hyper_sd_ln_phi", "hyper_sd_logit_phi", out_names)
    }
    y <- x
    x <- model[[x]]
    out <- summary(x$model)$summary[out_names,
                                      c("mean", "2.5%", "97.5%",
                                        "n_eff", "Rhat")] %>%
      as.data.frame %>%
      round(2) %>%
      dplyr::mutate(n_eff = round(n_eff))
    names(out) <- paste0(y, ".", names(out))
    if (unique(x$data$sample) == "phytoplankton") {
      out <- rbind(out, out[1, ])
      out[11, ] <- rep("-", 5)
    } else {
      out <- rbind(out[-10, ], out[1, ], out[10, ])
      out[10, ] <- rep("-", 5)
    }
    out
  }, model = stan_output)
  out <- do.call("cbind.data.frame", group_tables)
  row.names(out) <- c("ln_k_a (amb)",
                      "logit_ke (amb)",
                      "logit_phi (amb)",
                      "ln_k_a (war)",
                      "logit_ke (war)",
                      "logit_phi (war)",
                      "sigma$",
                      "sigma_ln_Delta_k_a$",
                      "sigma_Delta_logit_ke$",
                      "sigma_Delta_ln_phi$",
                      "sigma_Delta_logit_phi")
  out %>%
    dplyr::mutate(Parameter = row.names(.)) %>%
    dplyr::select(Parameter, tidyselect::everything()) %>%
    export_csv(...)
}

make_proto_sp_tab_1 <- function(brmsmodel, sgnf = 2, title) {
  cols <- c("Estimate", "l-95% CI", "u-95% CI", "Bulk_ESS", "Rhat")
  random <- summary(brmsmodel)$random$pond[, cols]
  fixed <- cbind(organise_fits(brmsmodel)[, 3:5],
                 summary(brmsmodel)$fixed[c(2, 1, 4, 3), cols[4:5]])
  colnames(fixed) <- cols
  sp_tab <- rbind(random, fixed)
  sp_tab[, cols[4:5]] <- round(sp_tab[, cols[4:5]])
  sp_tab[, cols[1:3]] <- round(sp_tab[, cols[1:3]], sgnf)
  empty <- matrix(rep("", 5), 1, 5)
  colnames(empty) <- cols
  sp_tab <- rbind(empty,
                  sp_tab) %>%
    as.matrix()
  rownames(sp_tab) <- c(title,
                        "Pond-level (SD)",
                        c("Before, Ambient",
                          "After, Ambient",
                          "Before, Warmed",
                          "After, Warmed"))
  sp_tab
}

make_sp_table_1 <- function(ba_co2, ba_nutrients, ...) {
  rbind(make_proto_sp_tab_1(ba_nutrients$NO2_uM, title = "NO2-"),
        make_proto_sp_tab_1(ba_nutrients$NO3_uM, title = "NO3-"),
        make_proto_sp_tab_1(ba_nutrients$NH3_uM, title = "NH4+"),
        make_proto_sp_tab_1(ba_co2, sgnf = 0, "CO_2 influx")) %>%
    export_csv(...)
}

make_sp_table_2 <- function(resid_brm_model, ...) {
  rounded <- function(value, precision = 1) {
    sprintf(paste0("%.", precision, "f"), round(value, precision))
  }

  plyr::ldply(resid_brm_model, function(x) {
    y <- data.frame(brms::fixef(x))
    for (i in seq_len(ncol(y))) {
      y[, i] <- rounded(y[, i], 2)
    }
    rownames(y) <- c("Intercept",
                     "NO2- slope",
                     "NO3- slope",
                     "NH4+ slope")
    y <- tibble::rownames_to_column(y, "Parameters")
    names(y) <- c("Parameters", "Estimate", "",
                  "2.5% C.I.", "97.5% C.I.")
    y[, c(1:2, 4:5)]
  }) %>%
    dplyr::rename(Taxon = sample) %>%
    dplyr::mutate(Taxon = c("Phytoplankton",
                            "", "", "",
                            "Zooplankton",
                            "", "", "")) %>%
    export_csv(...)
}
