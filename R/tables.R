organise_fits <- function(brmsmodel) {
  emmeans::emmeans(brmsmodel, pairwise ~ treatment | period)$emmeans %>%
    data.frame() %>%
    dplyr::arrange(treatment, desc(period))
}

make_proto_ed_tab_1 <- function(brmsmodel, sgnf = 2, title) {
  cols <- c("Estimate", "l-95% CI", "u-95% CI", "Bulk_ESS", "Rhat")
  random <- summary(brmsmodel)$random$pond[, cols]
  fixed <- cbind(organise_fits(brmsmodel)[, 3:5],
                 summary(brmsmodel)$fixed[c(2, 1, 4, 3), cols[4:5]])
  colnames(fixed) <- cols
  ed_tab <- rbind(random, fixed)
  ed_tab[, cols[4:5]] <- round(ed_tab[, cols[4:5]])
  ed_tab[, cols[1:3]] <- round(ed_tab[, cols[1:3]], sgnf)
  empty <- matrix(rep("", 5), 1, 5)
  colnames(empty) <- cols
  ed_tab <- rbind(empty,
                  ed_tab) %>%
    as.matrix()
  colnames(ed_tab) <- paste0("\\textbf{",
                             c("Mean", "2.5\\%", "97.5\\%",
                               "Eff", "$\\widehat{R}$"),
                             "}")
  rownames(ed_tab) <- c(title,
                        "\\textbf{Pond-level (SD)}",
                        paste0("\\textbf{", c("Before, Ambient",
                                              "After, Ambient",
                                              "Before, Warmed",
                                              "After, Warmed"),
                               "}"))
  ed_tab
}

make_ed_table_1 <- function(baci_co2, baci_nutrients) {
ed_tab_1 <- rbind(make_proto_ed_tab_1(baci_nutrients$NO2_uM, title = "\\textit{\\textbf{\\ce{NO2^-}}}"),
                  make_proto_ed_tab_1(baci_nutrients$NO3_uM, title = "\\textit{\\textbf{\\ce{NO3^-}}}"),
                  make_proto_ed_tab_1(baci_nutrients$NH3_uM, title = "\\textit{\\textbf{\\ce{NH4^+}}}"),
                  make_proto_ed_tab_1(baci_co2, sgnf = 0, "\\textit{\\textbf{CO\\textsubscript{2} influx}}"))

ed_tab_1 <- ed_tab_1 %>%
    knitr::kable(format = "latex", escape = FALSE)
  ed_tab_1[[1]] <- gsub("l|l|l|l|l|l", "llllll",
                          ed_tab_1[[1]], fixed = TRUE)
  ed_tab_1[[1]] <- gsub("1\\\\\n\\hline\n &", "1\\\\\n &",
                          ed_tab_1[[1]], fixed = TRUE)
  ed_tab_1[[1]] <- gsub("1\\\\\n\\hline\n\\textbf{", "1\\\\\n\\textbf{",
                          ed_tab_1[[1]], fixed = TRUE)
  ed_tab_1[[1]] <- gsub("& \\\\\n\\hline\n\\textbf{", "& \\\\\n\\textbf{",
                          ed_tab_1[[1]], fixed = TRUE)
  ed_tab_1[[1]] <- gsub("& \\\\\n\\hline\n\\textit{", "& \\\\\n\\textit{",
                          ed_tab_1[[1]], fixed = TRUE)
  ed_tab_1[[1]] <- gsub("& \\\\\n\\hline\n\\textbf{", "& \\\\\n\\textbf{",
                          ed_tab_1[[1]], fixed = TRUE)
  ed_tab_1
}

make_ed_table_2 <- function(stan_output) {
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
  row.names(out) <- paste0("\\textbf{",
                           c("$\\overline{\\textrm{ln}{\\kappa}_a}$ (amb)",
                             "$\\overline{{\\kappa}_e\'}$ (amb)",
                             "$\\overline{\\phi\'}$ (amb)",
                             "$\\overline{\\textrm{ln}{\\kappa}_a}$ (war)",
                             "$\\overline{{\\kappa}_e\'}$ (war)",
                             "$\\overline{\\phi\'}$ (war)",
                             "$\\sigma$",
                             "${\\sigma}_{\\textrm{ln}\\Delta{\\kappa_a}_j}$",
                             "${\\sigma}_{\\Delta{\\kappa_e\'}_j}$",
                             "${\\sigma}_{\\Delta\\textrm{ln}\\phi_j}$",
                             "${\\sigma}_{\\Delta{\\phi\'}_j}$"), "}")
  ed_tab_2 <- out %>%
    dplyr::mutate(Parameter = row.names(.)) %>%
    dplyr::select(Parameter, tidyselect::everything()) %>%
    knitr::kable(format = "latex", escape = FALSE) %>%
    kableExtra::column_spec(1, width = "2cm") %>%
    kableExtra::add_header_above(c(" ", "Phytoplankton" = 5, "Zooplankton" = 5))
  ed_tab_2[[1]] <- sub("Parameter & phytoplankton.mean & phytoplankton.2.5% & phytoplankton.97.5% & phytoplankton.n_eff & phytoplankton.Rhat & zooplankton.mean & zooplankton.2.5% & zooplankton.97.5% & zooplankton.n_eff & zooplankton.Rhat",
                         paste0("\\textbf{",
                                c("Parameter",
                                  rep(c("Mean",
                                        "2.5\\%",
                                        "97.5\\%",
                                        "Eff",
                                        "$\\widehat{R}$"),
                                      2)),
                                "}",
                                collapse = " & "),
                         ed_tab_2[[1]], fixed = TRUE)
  ed_tab_2[[1]] <- sub("Phytoplankton", "\\textbf{Phytoplankton}",
                         ed_tab_2[[1]], fixed = TRUE)
  ed_tab_2[[1]] <- sub("Zooplankton", "\\textbf{Zooplankton}",
                         ed_tab_2[[1]], fixed = TRUE)
  ed_tab_2[[1]] <- gsub("1\\\\\n\\hline\n\\textbf{$", "1\\\\\n\\textbf{$",
                          ed_tab_2[[1]], fixed = TRUE)
  ed_tab_2[[1]] <- gsub("-\\\\\n\\hline\n\\textbf{$", "-\\\\\n\\textbf{$",
                          ed_tab_2[[1]], fixed = TRUE)
  ed_tab_2[[1]] <- gsub("|l|l|l|l|l|l|l|l|l|l", "llllllllll",
                          ed_tab_2[[1]], fixed = TRUE)
  ed_tab_2[[1]] <- gsub("{c|}", "{c}", ed_tab_2[[1]], fixed = TRUE)
  ed_tab_2
}

make_sp_table_1 <- function(resid_brm_model) {
  rounded <- function(value, precision = 1) {
    sprintf(paste0("%.", precision, "f"), round(value, precision))
  }

  s_tab_1 <- plyr::ldply(resid_brm_model, function(x) {
    y <- data.frame(brms::fixef(x))
    for (i in seq_len(ncol(y))) {
      y[, i] <- rounded(y[, i], 2)
    }
    rownames(y) <- c("Intercept",
                     "\\ce{NO2^-} slope",
                     "\\ce{NO3^-} slope",
                     "\\ce{NH4^+} slope")
    y <- tibble::rownames_to_column(y, "Parameters")
    names(y) <- paste0("\\textbf{",
                       c("Parameters", "Estimate", "",
                         "2.5\\% C.I.", "97.5\\% C.I."),
                       "}")
    y[, c(1:2, 4:5)]
  }) %>%
    dplyr::rename(`\\textbf{Taxon}` = sample) %>%
    dplyr::mutate(`\\textbf{Taxon}` = c("\\textbf{Phytoplankton}",
                                        "", "", "",
                                        "\\textbf{Zooplankton}",
                                        "", "", "")) %>%
    knitr::kable(format = "latex", escape = FALSE) %>%
    kableExtra::column_spec(1, width = "3cm")
  s_tab_1[[1]] <- gsub("\\\\\n\\hline\n & N", "\\\\\n & N",
                       s_tab_1[[1]], fixed = TRUE)
  s_tab_1[[1]] <- gsub("p{3cm}|l|l|l|l",
                       "L{3cm}C{3cm}C{2cm}C{2cm}C{2cm}",
                       s_tab_1[[1]], fixed = TRUE)
  s_tab_1
}
