######################
# AUXILLIARY FUNCTIONS
######################
generate_png <- function(origin_file_name, output_file_name) {
  system(paste0("sips -s formatOptions best -s format png ",
                origin_file_name, " --out ", output_file_name))
  file.info(output_file_name)
}

trim_png_read <- function(file_path) {
  png_file <- png::readPNG(file_path)
  trim_png_blanks(png_file)
}

are_all_one <- function(x) {
  all(x == 1)
}

trim_png_blanks <- function(png_mat) {
  chosen_rows <- !apply(png_mat[, , 1], 1, are_all_one)
  chosen_cols <- !apply(png_mat[, , 1], 2, are_all_one)
  png_mat[chosen_rows, chosen_cols, ]
}

make_grob_png <- function(...) {
  grid::rasterGrob(trim_png_read(...), interpolate = TRUE)
}

# from stackoverflow
# question 22488563
# ggplot2-annotate-layer-position-in-r
annotate_textp <- function(label, x, y, facets = NULL, hjust = 0,
                           vjust = 0, color = "black", alpha = NA,
                           family = thm$text$family,
                           size = thm$text$size, fontface = 1,
                           line_height = 1.0,
                           box_just = ifelse(c(x, y) < 0.5, 0, 1),
                           margin = grid::unit(size / 2, "pt"),
                           thm = theme_get()) {
  x <- scales::squish_infinite(x)
  y <- scales::squish_infinite(y)

  tg <- grid::textGrob(label, x = 0, y = 0, hjust = hjust, vjust = vjust,
                       gp = grid::gpar(col = alpha(color, alpha),
                                       fontsize = size, fontfamily = family,
                                       fontface = fontface,
                                       lineheight = line_height))
  ts <- grid::unit.c(grid::grobWidth(tg), grid::grobHeight(tg))
  vp <- grid::viewport(x = x, y = y, width = ts[1], height = ts[2],
                       just = box_just)
  tg <- grid::editGrob(tg, x = ts[1] * hjust, y = ts[2] * vjust, vp = vp)
  unt <- grid::unit(1, "npc") - margin * 2
  inr <- grid::grobTree(tg, vp = grid::viewport(width = unt, height = unt))

  layer(data = NULL, stat = StatIdentity, position = PositionIdentity,
        geom = GeomCustomAnn, inherit.aes = TRUE,
        params = list(grob = grid::grobTree(inr),
                      xmin = -Inf,
                      xmax = Inf,
                      ymin = -Inf,
                      ymax = Inf))
}

clip_vec_hdi <- function(data, probs = c(0.005, 0.995)) {
  lim_x <- quantile_hdi(data$param, probs = probs)
  data %>%
    dplyr::filter(param >= lim_x[[1]] & param <= lim_x[[2]])
}

empty_panel_w_txt <- function(color, label) {
  grid::grobTree(grid::rectGrob(width = grid::unit(0.915, "npc"),
                                height = grid::unit(0.9, "npc"),
                                just = 0.45,
                                gp = grid::gpar(fill = color)),
                 grid::textGrob(label,
                                gp = grid::gpar(fontsize = 15,
                                                col = "black"),
                                hjust = 0.4))
}

sim_pp_mean <- function(data, model, type) {
  posteriors <- as.data.frame(model)
  data <- dplyr::filter(data, sample == tidyselect::all_of(type))
  y_rep <- matrix(0, nrow(posteriors), nrow(data))
  for (j in seq_len(ncol(y_rep))) {
    target_phi <- paste0("r_phi[", data$pond[j], "]")
    target_ka <- paste0("r_ka[", data$pond[j], "]")
    target_ke <- paste0("r_ke[", data$pond[j], "]")
    phi <- posteriors[, target_phi]
    ka <- posteriors[, target_ka]
    ke <- posteriors[, target_ke]
    sg <- posteriors[, "sigma"]
    mu <- calc_eqn_1(data$day[j], ka, ke, phi)
    y_rep[, j] <- rnorm(rep(1, nrow(y_rep)), mu, sg)
  }
  y_rep
}

###############
# PAPER FIGURES
###############
make_fig_1 <- function(dest, fig_out_folder, ...) {
  pdf(dest, width = 5.819004, height = 2.950226)
  p <- fig_1(...)
  print(p)
  grid::grid.text("a", x = grid::unit(0.11, "npc"), y = grid::unit(0.82, "npc"),
                  gp = grid::gpar("fontsize" = 13, "fontface" = "bold"))
  grid::grid.text("b", x = grid::unit(0.58, "npc"), y = grid::unit(0.82, "npc"),
                  gp = grid::gpar("fontsize" = 13, "fontface" = "bold"))
  dev.off()
}

fig_1 <- function(post_pred_means, my_cols_treatment, phy_png, zoo_png) {
  polygons <- post_pred_means$polygons
  linesd <- post_pred_means$lines
  tp <- polygons %>%
    dplyr::distinct(sample, treatment) %>%
    dplyr::mutate(x = 1)

  ggplot(data = polygons, mapping = aes(x = x, y = y_cred)) +
    geom_rect(data = tp, mapping = aes(fill = sample),
              xmin = -Inf, xmax = Inf, ymin = -Inf,
              ymax = Inf, inherit.aes = FALSE, show.legend = FALSE) +
    scale_fill_manual(values = c("#CCECE6", "#F6F1EB")) +
    ggnewscale::new_scale("fill") +
    geom_polygon(data = polygons,
                 mapping = aes(x = x, y = y_cred, fill = treatment)) +
    geom_line(data = linesd,
              mapping = aes(x = x, y = mean, group = treatment),
              col = "black", lty = 1, size = 0.5) +
    geom_polygon(data = polygons,
                 mapping = aes(x = x, y = y_cred, fill = treatment),
                 alpha = 0.7,
                 inherit.aes = FALSE) +
    scale_fill_manual(labels = c("Ambient", "Warmed"),
                      values = rev(my_cols_treatment)) +
    facet_wrap(~sample, scales = "free", ncol = 2) +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.4, 0.1, 0.1, 0.2), "in"),
          strip.background = element_blank(),
          strip.text = element_text(color = "transparent"),
          panel.spacing.x = grid::unit(1.2, "lines"),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          legend.position = c(0.5, 1.2),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    ylab(substitute("Excess "^15 * "N"["%"] * ", " * chi)) +
    xlab("Time (days)") +
    layer(data = polygons %>%
            dplyr::filter(sample == "phytoplankton"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = phy_png,
                        ymin = 0.5,
                        ymax = 0.65,
                        xmin = 43,
                        xmax = 63)) +
    layer(data = polygons %>%
            dplyr::filter(sample == "zooplankton"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = zoo_png,
                        ymin = 0.3,
                        ymax = 0.45,
                        xmin = 45,
                        xmax = 65))
}

make_fig_2 <- function(dest, fig_out_folder, ...) {
  p <- fig_2(...)
  pdf(dest, width = 5.737556, height = 6.950226)
  print(p)
  grid.text(substitute(kappa["a"] * " (d"^-1 * ")"),
            x = grid::unit(0.56, "npc"),
            y = grid::unit(0.775, "npc"),
            gp = grid::gpar("fontsize" = 10))
  grid.text(substitute(kappa["e"] * " (d"^-1 * ")"),
            x = grid::unit(0.56, "npc"),
            y = grid::unit(0.54, "npc"),
            gp = grid::gpar("fontsize" = 10))
  grid.text(substitute(italic(phi) * " (% d)"),
            x = grid::unit(0.565, "npc"),
            y = grid::unit(0.31, "npc"),
            gp = grid::gpar("fontsize" = 10))
  grid.text(substitute("Efficiency of N transfer (" * bar(epsilon) * ")"),
            x = grid::unit(0.54, "npc"),
            y = grid::unit(0.075, "npc"),
            gp = grid::gpar("fontsize" = 10))
  # legends
  grid.text("a",
            x = grid::unit(0.13, "npc"),
            y = grid::unit(0.985, "npc"),
            gp = grid::gpar("fontsize" = 11, "fontface" = "bold"))
  grid.text("b",
            x = grid::unit(0.13, "npc"),
            y = grid::unit(0.76, "npc"),
            gp = grid::gpar("fontsize" = 11, "fontface" = "bold"))
  grid.text("c",
            x = grid::unit(0.13, "npc"),
            y = grid::unit(0.525, "npc"),
            gp = grid::gpar("fontsize" = 11, "fontface" = "bold"))
  grid.text("d",
            x = grid::unit(0.13, "npc"),
            y = grid::unit(0.295, "npc"),
            gp = grid::gpar("fontsize" = 11, "fontface" = "bold"))
  dev.off()
}

fig_2 <- function(data_eff, data_ka, data_ke, data_phi,
                  my_cols_treatment, phy_png, zoo_png) {
  nreps <- nrow(data_eff)
  plot_data <- do.call("rbind.data.frame",
                       list(data_ka, data_ke, data_phi, data_eff)) %>%
    dplyr::mutate(name = rep(c("a_kas", "b_kes", "c_phi", "d_eff"),
                             each = nreps)) %>%
    plyr::ddply(.(name, sample, treatment), clip_vec_hdi)
  tp <- plot_data %>%
    dplyr::distinct(sample, name) %>%
    dplyr::mutate(param = 1)

  ggplot(data = plot_data, mapping = aes(x = param)) +
    geom_rect(data = tp, mapping = aes(fill = sample),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              inherit.aes = FALSE, show.legend = FALSE) +
    scale_fill_manual(values = c("#CCECE6", "#F6F1EB")) +
    ggnewscale::new_scale("fill") +
    geom_density(data = plot_data, adjust = 2, trim = TRUE,
                 mapping = aes(x = param, fill = treatment),
                 colour = NA) +
    geom_density(adjust = 2, alpha = 0.7, trim = TRUE,
                 mapping = aes(fill = treatment, colour = treatment),
                 show.legend = FALSE) +
    scale_fill_manual(values = rev(my_cols_treatment),
                      labels = c("Ambient", "Warmed")) +
    facet_wrap(name~sample, scales = "free", ncol = 2) +
    theme_bw() +
    theme(plot.margin = grid::unit(c(-0.3, 0.1, 0.6, 0.2), "in"),
          strip.background = element_blank(),
          strip.text = element_text(color = "transparent"),
          panel.spacing.y = grid::unit(-1.2, "lines"),
          panel.spacing.x = grid::unit(1.2, "lines"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14),
          legend.position = c(0.5, -0.1),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    ylab("Posterior density (99% C.I.)\n") +
    scale_color_manual(values = rev(my_cols_treatment)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(clip = "off") +
    layer(data = plot_data %>%
            dplyr::filter(sample == "phytoplankton" & name == "a_kas"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = phy_png,
                        ymin = 2.2,
                        ymax = 3,
                        xmin = 1,
                        xmax = 1.25)) +
    layer(data = plot_data %>%
            dplyr::filter(sample == "zooplankton" & name == "a_kas"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = zoo_png,
                        ymin = 6,
                        ymax = 10.5,
                        xmin = 0.62,
                        xmax = 0.74))
}

make_fig_3 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, fig_3(...), device = "pdf", width = 3.87,
                  height = 5.46, units = "in", onefile = FALSE)
}

fig_3 <- function(data, phy_png, zoo_png, my_cols_treatment) {
  cols <- rep(c(my_cols_treatment[2], my_cols_treatment[1]), 2)
  data <- data %>%
    dplyr::group_by(sample, pond, treatment) %>%
    dplyr::summarise(av_C_ug_L = mean(av_C_ug_L), ln_av_C_ug_L = log(av_C_ug_L))

  a <- ggplot(data = data, mapping = aes(y = ln_av_C_ug_L, x = treatment)) +
    geom_rect(mapping = aes(xmin = -Inf, xmax = Inf,
                            ymin = 1.8, ymax = 1.8 + (10.7 - 1.8) / 2),
              fill = "#F6F1EB") +
    geom_rect(mapping = aes(xmin = -Inf, xmax = Inf,
                            ymin = 1.8 + (10.7 - 1.8) / 2, ymax = 10.7),
              fill = "#CCECE6") +
    geom_boxplot(mapping = aes(colour = interaction(treatment, sample)),
                 position = position_dodge(0),
                 fill = cols,
                 show.legend = FALSE,
                 width = 1,
                 size = 0.3) +
    geom_point(mapping = aes(fill = interaction(treatment, sample),
                             shape = sample),
               colour = "black",
               position = position_jitterdodge(jitter.width = 0.4,
                                               dodge.width = 0,
                                               seed = 1),
               size = 2.5,
               show.legend = FALSE) +
    scale_colour_manual(values = rep("black", 4)) +
    scale_fill_manual(values = cols) +
    scale_shape_manual(values = c(21, 22)) +
    labs(x = "Treatment",
         y = substitute("Total Biomass (" * mu * "g C L"^-1 * ")")) +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.2, 0.3, 0.2, 0.4), "in"),
          axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
          axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
          axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
          axis.text.y = element_text(size = 10),
          panel.grid = element_blank()) +
    scale_x_discrete(expand = c(1, -0.5), labels = c("A", "W")) +
    scale_y_continuous(limits = c(1.8, 10.7), expand = c(0, 0),
                       breaks = c(log(8), log(50), log(400),
                                  log(3e3), log(22e3)),
                       labels = c(8, 50, 400, 3e3, 22e3))

  a$coordinates$clip <- "off"

  a +
    layer(data = data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE,
          params = list(grob = phy_png,
                        ymin = 6.6,
                        ymax = 7.4,
                        xmin = 0.3,
                        xmax = 1.3)) +
    layer(data = data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE,
          params = list(grob = zoo_png,
                        ymin = 4.5,
                        ymax = 6,
                        xmin = 2,
                        xmax = 2.5))
}

#######################
# EXTENDED DATA FIGURES
#######################
make_internal_ed_fig_1d <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, internal_ed_fig_1d(...), device = "pdf", width = 8.5,
                  height = 3.5, units = "in", onefile = FALSE)
}

internal_ed_fig_1d <- function(physchem_data, my_cols_treatment) {
  make_bef_aft_data <- function(data, vrb) {
    data$y_var <- data[[vrb]]
    data <- data %>%
      dplyr::select(Date, pairs, treatment, pond, y_var) %>%
      tidyr::drop_na()
    bef <- data %>%
      dplyr::filter(Date < as.Date("2013-07-16")) %>%
      dplyr::group_by(pairs, treatment, pond) %>%
      dplyr::summarise(mean = mean(y_var)) %>%
      dplyr::mutate(period = "before") %>%
      data.frame()
    aft <- data %>%
      dplyr::filter(Date > as.Date("2013-07-16") &
                      Date < as.Date("2013-07-20")) %>%
      dplyr::group_by(pairs, treatment, pond) %>%
      dplyr::summarise(mean = mean(y_var)) %>%
      dplyr::mutate(period = "after") %>%
      data.frame()

    rbind(bef, aft)
  }

  plot_bef_aft <- function(data, vrb, ylab, my_cols_treatment) {
    data$vrb <- data[[vrb]]
    ggplot(data = data, mapping = aes(y = vrb, x = period,
                                      fill = treatment,
                                      shape = treatment)) +
      geom_boxplot(colour = "black",
                   show.legend = FALSE,
                   width = 0.6,
                   size = 0.3,
                   outlier.shape = NA) +
      geom_point(position = position_dodge(0.4),
                 size = 2.5) +
      labs(x = "Treatment",
           y = ylab) +
      theme_bw() +
      theme(plot.margin = grid::unit(c(0.2, 0.3, 0.2, 0.4), "in"),
            axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
            axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
            axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
            axis.text.y = element_text(size = 10),
            panel.grid = element_blank()) +
      scale_x_discrete(labels = c("Before", "After")) +
      scale_fill_manual(name = "Treatment",
                        labels = c("Ambient", "Warmed"),
                        values = rev(my_cols_treatment)) +
      scale_shape_manual(name = "Treatment",
                         labels = c("Ambient", "Warmed"),
                         values = c(21, 22))
  }

  pdosat <- make_bef_aft_data(physchem_data, "DOsat") %>%
    plot_bef_aft("mean", "DO saturation (%)", my_cols_treatment) +
    scale_y_continuous(trans = "log10") +
    theme(legend.position = "none")

  pph <- make_bef_aft_data(physchem_data, "pH") %>%
    plot_bef_aft("mean", "pH", my_cols_treatment) +
    theme(legend.position = "none")
  gridExtra::grid.arrange(pdosat, pph, ncol = 2)
}

make_ed_fig_2 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_2(...), device = "pdf", width = 12,
                  height = 4, units = "in", onefile = FALSE)
}

ed_fig_2 <- function(data, my_cols_treatment) {
  my_theme <- function(leg = FALSE) {
    theme_bw() +
      theme(axis.title.y = element_text(size = 14,
                                        margin = margin(t = 0,
                                                        r = 10,
                                                        b = 0,
                                                        l = 10)),
            axis.title.x = element_text(size = 14,
                                        margin = margin(t = 10,
                                                        r = 0,
                                                        b = 10,
                                                        l = 0)),
            axis.text.x = element_text(size = 12,
                                       margin = margin(t = 4,
                                                       r = 0,
                                                       b = 0,
                                                       l = 0)),
            axis.text.y = element_text(size = 12,
                                       margin = margin(t = 0,
                                                       r = 4,
                                                       b = 0,
                                                       l = 0)),
            axis.ticks.length = grid::unit(5, "pt"),
            strip.background = element_blank(),
            legend.position = if (leg) c(0.8, 0.8) else "none")
  }

  unique_ggplot <- function(data, vrb, my_cols_treatment,
                            my_ylab, leg = FALSE) {
    data <- data %>%
      dplyr::mutate(sample_date = as.Date(sample_date,
                                          format = "%Y-%m-%d")) %>%
      dplyr::select(vrb = tidyselect::all_of(vrb),
                    "sample_date", "treatment") %>%
      tidyr::drop_na() %>%
      dplyr::group_by(sample_date, treatment) %>%
      dplyr::summarise(mean = mean(vrb),
                       sd = sd(vrb),
                       se = sd / sqrt(n()),
                       conf = se * 1.96)
    ggplot(data = data, mapping = aes(x = sample_date,
                                      y = mean,
                                      fill = treatment)) +
      geom_errorbar(mapping = aes(ymax = mean + conf,
                                  ymin = ifelse(mean - conf < 0,
                                                0,
                                                mean - conf)),
                    width = 0.2,
                    color = "black",
                    lwd = 0.3,
                    position = position_dodge(width = 1)) +
      geom_point(shape = 21,
                 size = 2,
                 position = position_dodge(width = 1)) +
      geom_vline(mapping = aes(xintercept = as.Date("2013-07-16")),
                 color = "black",
                 size = 0.5,
                 lty = 2) +
      scale_fill_manual(values = my_cols_treatment,
                        labels = c("Ambient", "Control", "Warmed"),
                        name = "Treatment") +
      ylab(my_ylab) +
      xlab("Sampling date (2013)") +
      my_theme(leg)
  }
  my_cols_treatment <- c(my_cols_treatment, "white")
  names(my_cols_treatment) <- c("H", "A", "C")

  no2 <- unique_ggplot(data, "NO2_uM", my_cols_treatment,
                       substitute(mu * "mol " * "NO"[2^"-"] * " L"^-1),
                       leg = TRUE)
  no3 <- unique_ggplot(data, "NO3_uM", my_cols_treatment,
                       substitute(mu * "mol " * "NO"[3^"-"] * " L"^-1))
  nh4 <- unique_ggplot(data, "NH3_uM", my_cols_treatment,
                       substitute(mu * "mol " * "NH"[4^"+"] * " L"^-1))
  gridExtra::grid.arrange(no2, no3, nh4, ncol = 3)
}

make_ed_fig_3 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_3(...), device = "pdf", width = 7,
                  height = 6, units = "in", onefile = FALSE)
}

ed_fig_3 <- function(co2_data, my_cols_treatment) {
  ylims <- co2_data %>%
    dplyr::group_by(treatment, period) %>%
    dplyr::summarise(lims = boxplot.stats(influx)$stats[c(1, 5)]) %>%
    data.frame()
  ylims <- range(ylims$lims)

  ###################
  ## OULIERS EXCLUDED
  ###################
  ggplot(data = co2_data, mapping = aes(y = influx,
                                        x = period,
                                        fill = treatment,
                                        shape = treatment)) +
    geom_boxplot(colour = "black",
                 show.legend = FALSE,
                 width = 0.6,
                 size = 0.3,
                 outlier.shape = NA) +
    geom_point(position = position_jitterdodge(dodge.width = 0.6,
                                               jitter.width = 0.1),
               size = 2.5) +
    labs(x = "Period",
         y = substitute("Daytime CO"[2] *
                          " influx (" *
                          mu *
                          "mol m"^-2 * " d"^-1 * ")")) +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.2, 0.3, 0.2, 0.4), "in"),
          axis.title.x = element_text(size = 20, vjust = -1, hjust = 0.5),
          axis.title.y = element_text(size = 20, vjust = 4, hjust = 0.5),
          axis.text.x = element_text(size = 17, vjust = -1, hjust = 0.5),
          axis.text.y = element_text(size = 17),
          panel.grid = element_blank(),
          legend.position = c(0.85, 0.12),
          legend.background = element_blank(),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 16)) +
    scale_x_discrete(labels = c("Before", "After"),
                     limits = rev(sort(unique(co2_data$period)))) +
    scale_fill_manual(name = "Treatment",
                      labels = c("Ambient", "Warmed"),
                      values = rev(my_cols_treatment)) +
    scale_shape_manual(name = "Treatment",
                       labels = c("Ambient", "Warmed"),
                       values = c(24, 25)) +
    coord_cartesian(ylim = ylims * 1.03)
}

make_ed_fig_5 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_5(...), device = "pdf", width = 10.5,
                  height = 7.86, units = "in", onefile = FALSE)
}

ed_fig_5 <- function(data, posterior_predictions,
                     my_cols_treatment, my_cols_group) {
  empty_void <- function() {
    ggplot() +
      theme_void()
  }
  ambient_panel <- empty_panel_w_txt(color = my_cols_treatment[2],
                                     label = "Ambient")
  warmed_panel <- empty_panel_w_txt(color = my_cols_treatment[1],
                                    label = "Warmed")
  ponds <- posterior_predictions$ponds
  preds <- posterior_predictions$preds
  out <- vector(mode = "list", length = nrow(ponds) + 4)
  out[[1]] <- ambient_panel
  out[[2]] <- warmed_panel
  out[[19]] <- empty_void()
  out[[20]] <- empty_void()
  for (i in ponds$pond) {
    grob1 <- grid::grobTree(grid::textGrob(paste0("Pond #", ponds$pond[i]),
                                           x = 0.95, y = 0.9, hjust = 1,
                                           gp = grid::gpar(col = "grey30",
                                                           fontsize = 10)))
    p <- ggplot(data = preds[[i]], mapping = aes(x = x, y = y_conf,
                                                 fill = sample)) +
      geom_polygon(alpha = 0.2) +
      labs(x = "", y = "") +
      scale_color_manual(values = c(my_cols_group[1], my_cols_group[2]),
                         aesthetics = "fill") +
      theme_bw() +
      theme(plot.margin = margin(-0.2, 0, 0, 0, "in"),
            axis.text.y = element_text(size = 9),
            legend.position = "none") +
      geom_line(mapping = aes(x = x, y = y_mean, colour = sample),
                data = preds[[i]][complete.cases(preds[[i]]), ],
                linetype = 2,
                size = 0.6,
                inherit.aes = FALSE) +
      scale_color_manual(values = c("seagreen4", my_cols_group[2]),
                         aesthetics = "colour") +
      geom_point(mapping = aes(x = day, y = add15N_perc,
                               shape = sample, fill = sample),
                 size = 2, data = data[data$pond == ponds$pond[i], ],
                 inherit.aes = FALSE) +
      scale_shape_manual(values = c(21, 22)) +
      scale_fill_manual(values = c(my_cols_group[1], my_cols_group[2])) +
      annotation_custom(grob1)
    if (i %in% seq(4, 16, 4)) {
      out[[i + 2]] <- p +
        theme(axis.text.x = element_text(size = 9))
    } else {
      out[[i + 2]] <- p +
        theme(axis.text.x = element_text(size = 9)) +
        scale_x_continuous(labels = rep("", 4), breaks = seq(0, 60, 20))
    }
    if (i == 8) {
      out[[i + 2]] <- out[[i + 2]] +
        scale_y_continuous(labels = seq(0, 0.2, 0.1), breaks = seq(0, 0.2, 0.1))
    }
  }

  lay_mat <- rbind(matrix(c(1, 1, 2, 2), 1, 4, byrow = TRUE),
                   matrix(NA, 1, 4, byrow = TRUE),
                   matrix(rep(3:18, each = 4), 16, 4),
                   matrix(c(19, 19, 20, 20), 1, 4, byrow = TRUE))
  x <- gridExtra::arrangeGrob(grobs = out, layout_matrix = lay_mat)
  ggpubr::annotate_figure(x,
                          bottom = ggpubr::text_grob("Time (days)",
                                                     hjust = 0.5,
                                                     vjust = -1,
                                                     size = 25,
                                                     lineheight = grid::unit(2, "in")),
                          left = ggpubr::text_grob(substitute("Excess "^15 *
                                                                "N"["%"] *
                                                                ", " * chi),
                                                   hjust = 0.25,
                                                   vjust = 0.5,
                                                   size = 25,
                                                   rot = 90)) +
    annotation_custom(grid::grid.points(x = grid::unit(1.4, "in"),
                                        y = grid::unit(0.7, "in"),
                                        size = grid::unit(0.15, "in"),
                                        pch = 21,
                                        gp = grid::gpar(col = "black",
                                                        fill = my_cols_group[1]))) +
    annotation_custom(grid::grid.text("Phytoplankton",
                                      x = grid::unit(1.5, "in"),
                                      y = grid::unit(0.7, "in"),
                                      hjust = 0,
                                      vjust = 0.5,
                                      gp = grid::gpar(cex = 1.2))) +
    annotation_custom(grid::grid.points(x = grid::unit(1.4, "in"),
                                        y = grid::unit(0.4, "in"),
                                        size = grid::unit(0.15, "in"),
                                        pch = 22,
                                        gp = grid::gpar(col = "black",
                                                        fill = my_cols_group[2]))) +
    annotation_custom(grid::grid.text("Zooplankton",
                                      x = grid::unit(1.5, "in"),
                                      y = grid::unit(0.4, "in"),
                                      hjust = 0,
                                      vjust = 0.5,
                                      gp = grid::gpar(cex = 1.2)))
}

make_ed_fig_6 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_6(...), device = "pdf", width = 8.2,
                  height = 3.8, units = "in", onefile = FALSE)
}

ed_fig_6 <- function(post_effs, diff_biomass, phy_png, zoo_png) {
  diff_effs <- tapply(post_effs$param,
                      list(post_effs$iter,
                           post_effs$sample),
                      function(x) 1 - (x[2] / x[1])) %>%
    as.data.frame %>%
    dplyr::mutate(iter = seq_len(nrow(.))) %>%
    tidyr::pivot_longer(c(phytoplankton, zooplankton),
                        names_to = "sample",
                        values_to = "diff") %>%
    dplyr::mutate(variable = "efficiency") %>%
    dplyr::arrange(sample, iter) %>%
    dplyr::select(sample, iter, diff, variable) %>%
    as.data.frame

  plot_data <- rbind(diff_effs, diff_biomass) %>%
    dplyr::rename(param = diff) %>%
    dplyr::mutate(param = param * 1e2) %>%
    plyr::ddply(c("sample", "variable"), clip_vec_hdi)

  tp <- plot_data %>%
    dplyr::distinct(sample) %>%
    dplyr::mutate(param = 1)

  ggplot(data = plot_data, mapping = aes(x = param)) +
    geom_rect(data = tp, mapping = aes(fill = sample),
              xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              inherit.aes = FALSE, show.legend = FALSE) +
    scale_fill_manual(values = c("#CCECE6", "#F6F1EB")) +
    ggnewscale::new_scale("fill") +
    geom_density(data = plot_data, adjust = 2, trim = TRUE,
                 mapping = aes(x = param, fill = variable),
                 colour = NA) +
    geom_density(adjust = 2, alpha = 0.7, trim = TRUE,
                 mapping = aes(fill = variable, colour = variable),
                 show.legend = FALSE) +
    scale_fill_manual(values = c("black", "grey60"),
                      labels = c("Biomass", "Efficiency")) +
    facet_wrap(~sample, scales = "free", ncol = 2) +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.3, 0.1, 0.3, 0.2), "in"),
          strip.background = element_blank(),
          strip.text = element_text(color = "transparent"),
          panel.spacing.x = grid::unit(1.2, "lines"),
          axis.title.x = element_text(size = 14,
                                      margin = grid::unit(c(0.15,
                                                      0,
                                                      0,
                                                      0),
                                                    "in")),
          axis.title.y = element_text(size = 14),
          legend.position = c(0.5, 1.1),
          legend.direction = "horizontal",
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    ylab("Posterior density (99% C.I.)\n") +
    xlab("Percentage decline relative to ambient") +
    scale_color_manual(values = c("black", "grey60")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(clip = "off") +
    layer(data = plot_data %>%
            dplyr::filter(sample == "phytoplankton"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = phy_png,
                        ymin = 0.028,
                        ymax = 0.033,
                        xmin = -10,
                        xmax = 10)) +
    layer(data = plot_data %>%
            dplyr::filter(sample == "zooplankton"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = zoo_png,
                        ymin = 0.017,
                        ymax = 0.024,
                        xmin = -40,
                        xmax = -15))
}

make_ed_fig_7 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_7(...), device = "pdf", width = 7,
                  height = 7, units = "in", onefile = FALSE)
}

ed_fig_7 <- function(community_data_2012, community_data_2016,
                     size_data_2012, size_data_2016,
                     my_cols_treatment) {
  ed_fig_7_bxpl <- function(data, my_cols_treatment, title) {
    cols <- rev(my_cols_treatment)
    ggplot(data = data, mapping = aes(y = log_av_C_ug, x = treatment)) +
      geom_boxplot(mapping = aes(colour = treatment),
                   position = position_dodge(0),
                   fill = cols,
                   show.legend = FALSE,
                   width = 0.7,
                   size = 0.3,
                   outlier.shape = NA) +
      geom_point(mapping = aes(fill = treatment,
                               shape = treatment),
                 colour = "black",
                 position = position_jitterdodge(jitter.width = 0.4,
                                                 dodge.width = 0,
                                                 seed = 1),
                 size = 2.5,
                 show.legend = FALSE) +
      scale_colour_manual(values = rep("black", 4)) +
      scale_fill_manual(values = cols) +
      scale_shape_manual(values = c(21, 22)) +
      labs(x = "Treatment",
           y = expression(log[10]~Organism~mass~(µg~C))) +
      theme_classic() +
      theme(plot.title = element_text(face = "bold")) +
      ggtitle(title)
  }

  ed_fig_7_nmds <- function(data, my_cols_treatment, tlab) {
    treat_rest <- data$treatment
    data <- data %>%
      dplyr::select(-pond, -treatment)
    bc_mds <- vegan::metaMDS(data, distance = "bray",
                             trace = FALSE, autotransform = FALSE,
                             k = 2)
    ponds <- data.frame(bc_mds$points) %>%
      dplyr::mutate(treat = treat_rest,
                    shape = ifelse(treat == "H", 25, 24))
    species <- data.frame(bc_mds$species) %>%
      dplyr::arrange(MDS1) %>%
      tidyr::drop_na()
    spp_h <- head(species, 4)
    spp_a <- tail(species, 4)
    spp <- rbind(spp_h, spp_a) %>%
      dplyr::mutate(treat = rep(c("H", "A"), each = 4),
                    shape = rep(c(25, 24), each = 4))
    treat_a <- ponds[ponds$treat == "A", ][chull(ponds[ponds$treat == "A",
                                                       c("MDS1", "MDS2")]), ]
    treat_h <- ponds[ponds$treat == "H", ][chull(ponds[ponds$treat == "H",
                                                       c("MDS1", "MDS2")]), ]
    hull_data <- rbind(treat_a, treat_h)

    ggplot() +
      geom_point(data = ponds,
                 mapping = aes(x = MDS1,
                               y = MDS2,
                               fill = treat),
                 size = 3,
                 shape = ponds$shape) +
      geom_point(data = species,
                 mapping = aes(x = MDS1,
                               y = MDS2,
                               alpha = 0.001),
                 size = 1.5) +
      geom_point(data = spp,
                 mapping = aes(x = MDS1,
                               y = MDS2,
                               alpha = 0.001),
                 size = 5,
                 shape = spp$shape) +
      geom_polygon(data = hull_data,
                   mapping = aes(x = MDS1,
                                 y = MDS2,
                                 fill = treat,
                                 group = treat),
                   alpha = 0.30) +
      scale_colour_manual(values = rev(my_cols_treatment)) +
      scale_fill_manual(values = rev(my_cols_treatment)) +
      scale_shape_manual(values = c(21, 21)) +
      labs(x = "NMDS1", y = "NMDS2") +
      theme_classic() +
      ggtitle(tlab) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"))
  }
  nmds_2012 <- ed_fig_7_nmds(community_data_2012,
                             my_cols_treatment,
                             "a (2012)") +
      geom_text(mapping = aes(x = -1, y = -1,
                label = "Stress = 0.12",
                size = 5), hjust = 0) +
    scale_y_continuous(limits = c(-1, 1),
                       breaks = c(-1, -0.5, 0, 0.5, 1))
  nmds_2016 <- ed_fig_7_nmds(community_data_2016,
                             my_cols_treatment,
                             "b (2016)") +
    geom_text(mapping = aes(x = -1.1, y = -1.8,
              label = "Stress = 0.05",
              size = 5), hjust = 0) +
    scale_y_continuous(limits = c(-1.8, 2),
                       breaks = c(-1.8, -0.9, 0, 0.9, 1.8))
  bxpl_2012 <- ed_fig_7_bxpl(size_data_2012, my_cols_treatment, "c (2012)") +
    scale_y_continuous(limits = c(-4.8, -2.4),
                       breaks = c(-4.8, -3.8, -2.8))
  bxpl_2016 <- ed_fig_7_bxpl(size_data_2016, my_cols_treatment, "d (2016)") +
    scale_y_continuous(limits = c(-4.8, -3.5),
                       breaks = c(-4.8, -4.2, -3.8))
  gridExtra::grid.arrange(nmds_2012, nmds_2016,
                          bxpl_2012, bxpl_2016, ncol = 2)
}

make_ed_fig_8 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_8(...), device = "pdf", width = 6.3,
                  height = 3.76, units = "in", onefile = FALSE)
}

ed_fig_8 <- function(data, my_cols_treatment) {
  data <- data %>%
    dplyr::mutate(shape = ifelse(treat == "H", 25, 24))
  ggplot(data = data) +
    geom_line(mapping = aes(x = doy,
                            y = ratio_fits,
                            colour = treat,
                            linetype = treat)) +
    scale_colour_manual(values = rev(my_cols_treatment)) +
    scale_fill_manual(values = rev(my_cols_treatment)) +
    geom_point(mapping = aes(x = doy,
                             y = ER.GPP,
                             colour = treat,
                             fill = treat),
               alpha = 0.3,
               shape = data$shape) +
    cowplot::theme_cowplot(font_size = 12) +
    theme(legend.position = "none") +
    facet_wrap(~ year, scales = "fixed") +
    theme(strip.background = element_blank()) +
    xlab("Day of the Year") +
    ylab(expression(R[eco] / GPP))
}

make_ed_fig_9 <- function(dest, fig_out_folder, ...) {
  pdf(dest, width = 9, height = 2.8)
  ed_fig_9(...)
  dev.off()
}

ed_fig_9 <- function(control_data, my_cols_group) {
  ponds <- unique(control_data$pond)
  par(mfrow = c(1, 3),
      omi = c(0, 0.5, 0, 0),
      mai = c(1.02, 0.72, 0.2, 0.1),
      cex = 1,
      cex.lab = 1.2,
      cex.axis = 0.8,
      cex.main = 1.2,
      xpd = NA)
  for (i in seq_along(ponds)) {
    x <- control_data %>%
      dplyr::filter(pond == ponds[i])
    if (i == 1) {
      ylab <- substitute(""^15 * "N"["%"])
    } else {
      ylab <- ""
    }
    plot(NA, xlim = c(0, 60), ylim = c(0, 0.1),
         xlab = ifelse(i == 2, "Time (days)", ""),
         ylab = ylab, las = 1)
    x %>%
      dplyr::filter(sample == "phytoplankton") %>%
      {
        points(.$day, .$add15N_perc, pch = 21, bg = my_cols_group[1], cex = 1.3)
      }
    x %>%
      dplyr::filter(sample == "zooplankton") %>%
      {
        points(.$day, .$add15N_perc, pch = 22, bg = my_cols_group[2], cex = 1.3)
      }
    LoLinR::proportionalLabel(0.95, 0.9, paste0("Control pond ", i),
                              adj = c(1, 0.5), cex = 0.9)
  }
}

make_ed_fig_10 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_10(...), device = "pdf", width = 3.87,
                  height = 5.46, units = "in", onefile = FALSE)
}

ed_fig_10 <- function(data, phy_png, zoo_png, my_cols_treatment) {
  cols <- rep(c(my_cols_treatment[2], my_cols_treatment[1]), 2)
  data <- data %>%
    dplyr::group_by(sample, pond, treatment) %>%
    dplyr::summarise(av_N_ug_L = mean(av_N_ug_L), ln_av_N_ug_L = log(av_N_ug_L))

  a <- ggplot(data = data, mapping = aes(y = ln_av_N_ug_L, x = treatment)) +
    geom_rect(mapping = aes(xmin = -Inf, xmax = Inf,
                            ymin = 0.5, ymax = 0.5 + (8.5 - 0.5) / 2),
              fill = "#F6F1EB") +
    geom_rect(mapping = aes(xmin = -Inf, xmax = Inf,
                            ymin = 0.5 + (8.5 - 0.5) / 2, ymax = 8.5),
              fill = "#CCECE6") +
    geom_boxplot(aes(colour = interaction(treatment, sample)),
                 position = position_dodge(0),
                 fill = cols,
                 show.legend = FALSE,
                 width = 1,
                 size = 0.3,
                 outlier.shape = NA) +
    geom_point(mapping = aes(fill = interaction(treatment, sample),
                             shape = sample),
               colour = "black",
               position = position_jitterdodge(jitter.width = 0.4,
                                               dodge.width = 0,
                                               seed = 1),
               size = 2.5,
               show.legend = FALSE) +
    scale_colour_manual(values = rep("black", 4)) +
    scale_fill_manual(values = cols) +
    scale_shape_manual(values = c(21, 22)) +
    labs(x = "Treatment",
         y = substitute("Total Biomass (" * mu * "g N L"^-1 * ")")) +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.2, 0.3, 0.2, 0.4), "in"),
          axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
          axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
          axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
          axis.text.y = element_text(size = 10),
          panel.grid = element_blank()) +
    scale_x_discrete(expand = c(1, -0.5), labels = c("A", "W")) +
    scale_y_continuous(limits = c(0.5, 8.5), expand = c(0, 0),
                       breaks = c(log(8), log(50), log(400),
                                  log(3e3)),
                       labels = c(8, 50, 400, 3e3))

  a$coordinates$clip <- "off"

  a +
    layer(data = data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE,
          params = list(grob = phy_png,
                        ymin = 7.4,
                        ymax = 8.2,
                        xmin = 0.3,
                        xmax = 1.3)) +
    layer(data = data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE,
          params = list(grob = zoo_png,
                        ymin = 3,
                        ymax = 4.3,
                        xmin = 2,
                        xmax = 2.5))
}

make_ed_fig_11 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, ed_fig_11(...), device = "pdf", width = 3.87,
                  height = 5.46, units = "in", onefile = FALSE)
}

ed_fig_11 <- function(data, phy_png, zoo_png, my_cols_treatment) {
  cols <- rep(c(my_cols_treatment[2], my_cols_treatment[1]), 2)
  a <- ggplot(data = data, mapping = aes(y = mean, x = treatment)) +
    geom_rect(mapping = aes(xmin = -Inf, xmax = Inf,
                            ymin = 3, ymax = 7.5),
              fill = "#F6F1EB") +
    geom_rect(mapping = aes(xmin = -Inf, xmax = Inf,
                            ymin = 7.5, ymax = 25),
              fill = "#CCECE6") +
    geom_boxplot(mapping = aes(colour = interaction(treatment, sample)),
                 position = position_dodge(0),
                 fill = cols,
                 show.legend = FALSE,
                 width = 1,
                 size = 0.3,
                 outlier.shape = NA) +
    geom_point(mapping = aes(fill = interaction(treatment, sample), shape = sample),
               colour = "black",
               position = position_jitterdodge(jitter.width = 0.4,
                                               dodge.width = 0,
                                               seed = 1),
               size = 2.5,
               show.legend = FALSE) +
    scale_colour_manual(values = rep("black", 4)) +
    scale_fill_manual(values = cols) +
    scale_shape_manual(values = c(21, 22)) +
    labs(x = "Treatment",
         y = "C:N ratio") +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.2, 0.3, 0.2, 0.4), "in"),
          axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
          axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
          axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
          axis.text.y = element_text(size = 10),
          panel.grid = element_blank()) +
    scale_y_continuous(trans = "log10", limits = c(3, 25), expand = c(0, 0)) +
    scale_x_discrete(expand = c(1, -0.5), labels = c("A", "W"))

  a$coordinates$clip <- "off"

  a +
    layer(data = data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE,
          params = list(grob = phy_png,
                        ymin = log10(20),
                        ymax = log10(24),
                        xmin = 2,
                        xmax = 2.5)) +
    layer(data = data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE,
          params = list(grob = zoo_png,
                        ymin = log10(3.2),
                        ymax = log10(4.8),
                        xmin = 0.5,
                        xmax = 1))
}

#######################
# SUPPLEMENTARY FIGURES
#######################
make_sp_fig_inorganic <- function(dest, fig_out_folder, model_list, vrb, ...) {
  mod <- model_list[[vrb]]
  ggplot2::ggsave(dest, make_baci_plots(mod, ...), device = "pdf", width = 14,
                  height = 7, units = "in", onefile = FALSE)
}

make_sp_fig_4 <- function(dest, fig_out_folder, mod, ...) {
  names(mod$data)[1] <- "y"
  ggplot2::ggsave(dest, make_baci_plots(mod, ...), device = "pdf", width = 14,
                  height = 7, units = "in", onefile = FALSE)
}

make_baci_plots <- function(mod, x_lab, my_cols_treatment) {
  a <- mod$data %>%
    dplyr::select(period, treatment) %>%
    dplyr::distinct() %>%
    tidybayes::add_fitted_draws(mod, dpar = c("mu", "sigma"),
                                re_formula = ~ 0) %>%
    tidybayes::sample_draws(200) %>%
    ggplot(aes(y = interaction(period, treatment))) +
    ggdist::stat_dist_slab(mapping = aes(dist = "norm",
                                         arg1 = mu,
                                         arg2 = sigma),
                           slab_color = "gray65",
                           alpha = 5e-2,
                           fill = NA,
                           size = 0.2) +
    geom_point(data = mod$data,
               inherit.aes = FALSE,
               mapping = aes(x = y,
                             y = interaction(period, treatment),
                             fill = treatment,
                             colour = treatment,
                             shape = treatment),
               size = 3,
               alpha = 0.3) +
    labs(x = x_lab,
         y = "") +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.2, 0.3, 0.2, 0.4), "in"),
          axis.title.x = element_text(size = 20, vjust = -1, hjust = 0.5),
          axis.title.y = element_text(size = 20, vjust = 4, hjust = 0.5),
          axis.text.x = element_text(size = 17, vjust = -1, hjust = 0.5),
          axis.text.y = element_text(size = 17),
          panel.grid = element_blank(),
          legend.position = c(0.85, 0.9),
          legend.background = element_blank(),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 16)) +
    scale_y_discrete(labels = c("After", "Before", "After", "Before")) +
    scale_colour_manual(name = "Treatment",
                        labels = c("Ambient", "Warmed"),
                        values = rev(my_cols_treatment)) +
    scale_fill_manual(name = "Treatment",
                      labels = c("Ambient", "Warmed"),
                      values = rev(my_cols_treatment)) +
    scale_shape_manual(name = "Treatment",
                       labels = c("Ambient", "Warmed"),
                       values = c(24, 25))

  b <- plot(mod, N = 6, plot = FALSE)
  gridExtra::grid.arrange(a, b[[1]], ncol = 2)
}

make_sp_fig_5 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, sp_fig_5(...), device = "pdf", width = 10.2,
                  height = 4.8, units = "in", onefile = FALSE)
}

sp_fig_5 <- function(cn_ratios, phy_png, zoo_png, my_cols_treatment) {
  targets <- names(cn_ratios)
  both <- plyr::llply(targets, function(x, models, my_cols_treatment) {
    my_cs <- my_cols_treatment
    my_shape <- c("phytoplankton" = 21, "zooplankton" = 22)
    data <- models[[x]]$data
    names(my_cs) <- c("H", "A")
    type <- c("mod_phy" = "phytoplankton", "mod_zoo" = "zooplankton")
    s_dat <- data.frame(y = data$CN_ratio,
                        y_rep = colMeans(brms::posterior_epred(models[[x]])),
                        group = data$treatment,
                        stringsAsFactors = FALSE) %>%
      dplyr::mutate(cols = my_cs[group])

    my_theme <- function() {
      theme_bw() +
        theme(plot.margin = grid::unit(c(0.2, 0.1, 0.4, 0.2), "in"),
              panel.grid = element_blank(),
              axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
              axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
              axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
              axis.text.y = element_text(size = 10))
    }

    a <- ggplot(data = s_dat, mapping = aes(x = y_rep, y = y, fill = group)) +
      geom_point(shape = my_shape[type[x]], size = 2, data = s_dat) +
      geom_smooth(mapping = aes(x = y_rep, y = y),
                  method = "lm",
                  se = FALSE,
                  lty = 2,
                  colour = "grey60",
                  size = 0.5,
                  inherit.aes = FALSE) +
      geom_smooth(mapping = aes(x = y, y = y),
                  method = "lm",
                  se = FALSE,
                  lty = 1,
                  colour = "black",
                  size = 0.5,
                  inherit.aes = FALSE) +
      scale_fill_manual(values = my_cs) +
      labs(x = "Predicted", y = "Observed") +
      my_theme() +
      theme(legend.position = "none")
    list(data = s_dat, plot = a)
  }, models = cn_ratios, my_cols_treatment)

  a <- both[[1]]$plot +
    layer(data = both[[1]]$data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = phy_png,
                        ymin = 27,
                        ymax = 30,
                        xmin = 6,
                        xmax = 11))
  b <- both[[2]]$plot +
    layer(data = both[[2]]$data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = zoo_png,
                        ymin = 9,
                        ymax = 12,
                        xmin = 2,
                        xmax = 4))
  gridExtra::grid.arrange(a, b, ncol = 2)
}

make_sp_fig_6 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, sp_fig_6(...), device = "pdf", width = 14,
                  height = 6, units = "in", onefile = FALSE)
}

sp_fig_6 <- function(...) {
  pp_checks_set <- function(type, data,
                            model_list,
                            my_cols_treatment,
                            my_shape) {
    fit <- model_list[[type]]$model
    y_rep <- sim_pp_mean(data, fit, type)
    sdat <- data %>%
      dplyr::filter(sample == type)
    y <- sdat$add15N_perc
    x <- sdat$day

    bayesplot::color_scheme_set("gray")

    # preamble
    my_theme <- function() {
      theme_bw() +
        theme(plot.margin = grid::unit(c(0.2, 0.1, 0.4, 0.2), "in"),
              panel.grid = element_blank(),
              axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
              axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
              axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
              axis.text.y = element_text(size = 10))
    }

    mean_y <- function(data, group) {
      mean(data %>%
             dplyr::filter(treatment == group) %>%
             dplyr::select(add15N_perc) %>%
             unlist)
    }

    # for p_b
    h_dat <- plyr::adply(y_rep, 1, function(x, treat) {
      data.frame(group = c("A", "H"),
                 y_rep = tapply(x, treat, mean),
                 stringsAsFactors = FALSE)
    }, treat = sdat$treatment)
    
    # for p_c
    my_cs <- my_cols_treatment
    names(my_cs) <- c("H", "A")
    s_dat <- data.frame(y = y,
                        y_rep = colMeans(y_rep),
                        group = sdat$treatment,
                        cols = my_cs[sdat$treatment],
                        stringsAsFactors = FALSE)
    
    p_a <- bayesplot::ppc_dens_overlay(y, y_rep[1:200, ]) +
      labs(x = substitute("Excess "^15 * "N"["%"] * ", " * chi),
           y = "Density") +
      my_theme() +
      theme(legend.text = element_blank(),
            legend.position = c(0.58, 0.79),
            legend.background = element_blank()) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      annotate_textp("Observed", x = 0.9, y = 0.85, hjust = 0) +
      annotate_textp("Predicted", x = 0.9, y = 0.74, hjust = 0)

    p_b <- ggplot(h_dat, aes(x = y_rep)) +
      geom_histogram(data = dplyr::filter(h_dat, group == "A"),
                     colour = "black",
                     fill = my_cols_treatment[2],
                     size = 0.2) +
      geom_vline(mapping = aes(xintercept = mean_y(sdat, "A")),
                 color = "black",
                 size = 0.5,
                 lty = 2) +
      geom_histogram(data = dplyr::filter(h_dat, group == "H"),
                     colour = "black",
                     fill = my_cols_treatment[1],
                     size = 0.2) +
      geom_vline(mapping = aes(xintercept = mean_y(sdat, "H")),
                 color = "black",
                 size = 0.5,
                 lty = 2) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                         breaks = seq(0, 6000, 2000),
                         labels = c("0.0", "2.0", "4.0", "6.0")) +
      labs(x = substitute("Mean " * chi * " (%)"),
           y = "Frequency (thousands)") +
      my_theme() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 18, hjust = 0),
            legend.position = c(0.8, 0.8),
            legend.background = element_blank())

    p_c <- ggplot(data = s_dat, mapping = aes(x = y_rep, y = y,
                                              fill = group)) +
      geom_point(shape = my_shape[type], size = 2, data = s_dat) +
      geom_smooth(mapping = aes(x = y_rep, y = y),
                  method = "lm",
                  se = FALSE,
                  lty = 2,
                  colour = "grey60",
                  size = 0.5,
                  inherit.aes = FALSE) +
      geom_smooth(mapping = aes(x = y, y = y),
                  method = "lm",
                  se = FALSE,
                  lty = 1,
                  colour = "black",
                  size = 0.5,
                  inherit.aes = FALSE) +
      scale_fill_manual(values = my_cs) +
      labs(x = "Predicted", y = "Observed") +
      my_theme() +
      theme(legend.position = "none")

    p_d <- bayesplot::ppc_intervals(y = y, yrep = y_rep, x = x, prob = 0.5) +
      labs(x = "Time (days)",
           y = substitute("Excess "^15 * "N"["%"] * ", " * chi)) +
      my_theme() +
      theme(legend.text = element_blank(),
            legend.position = c(0.58, 0.79),
            legend.background = element_blank()) +
      annotate_textp("Observed", x = 0.92, y = 0.85, hjust = 0) +
      annotate_textp("Predicted", x = 0.92, y = 0.74, hjust = 0)
    list(p_a = p_a, p_b = p_b, p_c = p_c, p_d = p_d)
  }
  out <- vector(mode = "list", length = 2)
  names(out) <- c("phy", "zoo")
  my_shape <- c("phytoplankton" = 21, "zooplankton" = 22)
  for (i in seq_along(out)) {
    out[[i]] <- pp_checks_set(names(my_shape)[i],
                              my_shape = my_shape,
                              ...)
    names(out[[i]]) <- paste0(names(out)[i], "_", names(out[[i]]))
  }
  p <- append(out[["phy"]], out[["zoo"]])
  p <- gridExtra::grid.arrange(p$phy_p_a, p$phy_p_b,
                               p$phy_p_c, p$phy_p_d,
                               p$zoo_p_a, p$zoo_p_b,
                               p$zoo_p_c, p$zoo_p_d,
                               ncol = 4)
  ggpubr::annotate_figure(p,
                          left = ggpubr::text_grob("",
                                                   hjust = 0.2,
                                                   vjust = 0.4,
                                                   size = 40,
                                                   rot = 90)) +
    annotation_custom(grid::grid.text("Phytoplankton",
                                      x = grid::unit(0.015, "npc"),
                                      y = grid::unit(0.8, "npc"),
                                      gp = grid::gpar("fontsize" = 20),
                                      hjust = 0.5,
                                      vjust = 0.5,
                                      rot = 90)) +
    annotation_custom(grid::grid.text("Zooplankton",
                                      x = grid::unit(0.015, "npc"),
                                      y = grid::unit(0.3, "npc"),
                                      gp = grid::gpar("fontsize" = 20),
                                      hjust = 0.5,
                                      vjust = 0.5,
                                      rot = 90))
}

make_sp_fig_7 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, sp_fig_7(...), device = "pdf", width = 10.2,
                  height = 4.8, units = "in", onefile = FALSE)
}

sp_fig_7 <- function(hierarchical_biomass, phy_png,
                     zoo_png, my_cols_treatment) {
  targets <- names(hierarchical_biomass)
  both <- plyr::llply(targets, function(x, models, my_cols_treatment) {
    my_cs <- my_cols_treatment
    my_shape <- c("phytoplankton" = 21, "zooplankton" = 22)
    names(my_cs) <- c("H", "A")
    data <- models[[x]]$data
    s_dat <- data.frame(y = data$ln_av_C_ug_L,
                        y_rep = colMeans(brms::posterior_epred(models[[x]])),
                        group = data$treatment,
                        stringsAsFactors = FALSE) %>%
      dplyr::mutate(cols = my_cs[group])

    my_theme <- function() {
      theme_bw() +
        theme(plot.margin = grid::unit(c(0.2, 0.1, 0.4, 0.2), "in"),
              panel.grid = element_blank(),
              axis.title.x = element_text(size = 15, vjust = -1, hjust = 0.5),
              axis.title.y = element_text(size = 15, vjust = 4, hjust = 0.5),
              axis.text.x = element_text(size = 10, vjust = -1, hjust = 0.5),
              axis.text.y = element_text(size = 10))
    }

    a <- ggplot(data = s_dat, mapping = aes(x = y_rep, y = y,
                                            fill = group)) +
      geom_point(shape = my_shape[x], size = 2, data = s_dat) +
      geom_smooth(mapping = aes(x = y_rep, y = y),
                  method = "lm",
                  se = FALSE,
                  lty = 2,
                  colour = "grey60",
                  size = 0.5,
                  inherit.aes = FALSE) +
      geom_smooth(mapping = aes(x = y, y = y),
                  method = "lm",
                  se = FALSE,
                  lty = 1,
                  colour = "black",
                  size = 0.5,
                  inherit.aes = FALSE) +
      scale_fill_manual(values = my_cs) +
      labs(x = "Predicted", y = "Observed") +
      my_theme() +
      theme(legend.position = "none")
    list(data = s_dat, plot = a)
  }, models = hierarchical_biomass, my_cols_treatment)

  a <- both[[1]]$plot +
    layer(data = both[[1]]$data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = phy_png,
                        ymin = 10,
                        ymax = 10.6,
                        xmin = 6.2,
                        xmax = 7))
  b <- both[[2]]$plot +
    layer(data = both[[2]]$data,
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = zoo_png,
                        ymin = 4.5,
                        ymax = 6.5,
                        xmin = -1.3,
                        xmax = 0.2))
  gridExtra::grid.arrange(a, b, ncol = 2)
}

make_sp_fig_8 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, sp_fig_8(...), device = "pdf", width = 5.82,
                  height = 2.95, units = "in", onefile = FALSE)
}

sp_fig_8 <- function(pp_means, phy_png, zoo_png, my_cols_treatment) {
  make_curve <- function(days, ln_ka, logit_ke, logit_phi) {
    ka <- exp(ln_ka)
    ke <- 1 / (1 + exp(-logit_ke))
    phi <- (1 / ke) / (1 + exp(-logit_phi))
    calc_eqn_1(days, ka, ke, phi)
  }

  curve_df <- function(data, days) {
    c_amb <- make_curve(days, data$ln_ka_amb,
                        data$logit_ke_amb,
                        data$logit_phi_amb)
    c_war <- make_curve(days, data$ln_ka_war,
                        data$logit_ke_war,
                        data$logit_phi_war)
    # cap samples that yield predictions
    # that are within the reasonable range of
    # data
    if (max(c_amb) < 1 && max(c_war) < 1) {
      data.frame(sample = data$sample,
                 days = days,
                 values = c(c_amb, c_war),
                 treatment = rep(c("amb", "war"),
                                 each = length(c_amb)))
    }
  }

  make_prior_lines <- function(ln_ka_amb, ln_ka_war, logit_ke_amb,
                               logit_ke_war, logit_phi_amb, logit_phi_war,
                               sample, iter) {
    plot_data <- data.frame(iter = seq_len(iter)) %>%
      dplyr::mutate(ln_ka_amb = rnorm(iter, ln_ka_amb, 1),
                    logit_ke_amb = rnorm(iter, logit_ke_amb, 1),
                    logit_phi_amb = rnorm(iter, logit_phi_amb, 1),
                    ln_ka_war = rnorm(iter, ln_ka_war, 1),
                    logit_ke_war = rnorm(iter, logit_ke_war, 1),
                    logit_phi_war = rnorm(iter, logit_phi_war, 1),
                    sample = sample)
    days <- seq(0.01, 60, length.out = 60)
    plyr::ddply(plot_data, .(iter), curve_df, days)
  }

  prior_bkg <- data.frame(sample = c("phytoplankton", "zooplankton")) %>%
    dplyr::mutate(ln_ka_amb = c(0, 0),
                  ln_ka_war = c(0, 0),
                  logit_ke_amb = c(0, 0),
                  logit_ke_war = c(0, 0),
                  logit_phi_amb = c(0, 0),
                  logit_phi_war = c(0, 0))

  set.seed(1)
  prior_lines <- plyr::ddply(prior_bkg, .(sample), function(data) {
    make_prior_lines(data$ln_ka_amb, data$ln_ka_war, data$logit_ke_amb,
                     data$logit_ke_war, data$logit_phi_amb, data$logit_phi_war,
                     data$sample, iter = 1000)
  })

  chosen <- dplyr::distinct(prior_lines, iter, sample) %>%
    dplyr::group_by(sample) %>%
    dplyr::summarise(chosen = sample(iter, 300)) %>%
    data.frame()

  prior_lines <- prior_lines %>%
    dplyr::group_by(sample) %>%
    dplyr::filter(iter %in% chosen$chosen[chosen$sample %in% unique(sample)]) %>%
    data.frame()

  prior_means <- plyr::ddply(prior_bkg, .(sample), curve_df,
                             days = seq(0.01, 60, length.out = 60)) %>%
    dplyr::filter(treatment == "amb") %>%
    dplyr::select(sample, days, values)

  linesd <- pp_means$lines %>%
    dplyr::mutate(col = ifelse(treatment == "amb",
                               my_cols_treatment[2],
                               my_cols_treatment[1]))
  tp <- linesd %>%
    dplyr::distinct(sample, treatment) %>%
    dplyr::mutate(x = 1)

  ggplot() +
    geom_rect(data = tp, mapping = aes(fill = sample),
              xmin = -Inf, xmax = Inf, ymin = -Inf,
              ymax = Inf, inherit.aes = FALSE, show.legend = FALSE) +
    scale_fill_manual(values = c("#CCECE6", "#F6F1EB")) +
    ggnewscale::new_scale("fill") +
    geom_line(data = prior_lines,
              mapping = aes(x = days, y = values,
                            group = paste(iter, treatment)),
              colour = "grey40", lty = 1, size = 0.5, alpha = 0.07,
              show.legend = FALSE) +
    geom_line(data = linesd,
              mapping = aes(x = x, y = mean,
                            group = treatment,
                            linetype = treatment),
              col = "black", size = 1, show.legend = FALSE) +
    geom_line(data = linesd,
              mapping = aes(x = x, y = mean,
                            group = treatment,
                            linetype = treatment,
                            colour = treatment),
              size = 1) +
    scale_colour_manual(labels = c("Ambient", "Warmed"),
                        values = rev(my_cols_treatment)) +
    scale_linetype_manual(labels = c("Ambient", "Warmed"),
                        values = c(1, 3)) +
    facet_wrap(~sample, scales = "free", ncol = 2) +
    theme_bw() +
    theme(plot.margin = grid::unit(c(0.4, 0.1, 0.1, 0.2), "in"),
          strip.background = element_blank(),
          strip.text = element_text(color = "transparent"),
          panel.spacing.x = grid::unit(1.2, "lines"),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          legend.position = c(0.5, 1.2),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    ylab(substitute("Excess "^15 * "N"["%"] * ", " * chi)) +
    xlab("Time (days)") +
    layer(data = linesd %>%
            dplyr::filter(sample == "phytoplankton"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = phy_png,
                        ymin = 0.55,
                        ymax = 0.75,
                        xmin = 50,
                        xmax = 60)) +
    layer(data = linesd %>%
            dplyr::filter(sample == "zooplankton"),
          stat = StatIdentity,
          position = PositionIdentity,
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = FALSE,
          params = list(grob = zoo_png,
                        ymin = 0.45,
                        ymax = 0.65,
                        xmin = 50,
                        xmax = 63))
}

make_sp_fig_9_11 <- function(dest, fig_out_folder, ...) {
  ggplot2::ggsave(dest, supp_resid_figure(...), device = "pdf", width = 10.5,
                  height = 7.86, units = "in", onefile = FALSE)
}

supp_resid_figure <- function(data, pred_tag, my_cols_group, lab) {
  names(my_cols_group) <- c("phytoplankton", "zooplankton")
  data <- data %>%
    dplyr::filter(predictor == pred_tag)
  ggplot(data = data) +
    geom_polygon(mapping = aes(x = x_conf, y = y_conf,
                               fill = sample),
                 alpha = 0.5,
                 show.legend = FALSE) +
    geom_line(mapping = aes(x = x, y = y,
                            linetype = sample),
              colour = "black",
              show.legend = FALSE) +
    geom_point(mapping = aes(x = raw_x,
                             y = raw_y,
                             shape = sample,
                             fill = sample),
               show.legend = FALSE,
               size = 2) +
    labs(x = lab,
         y = "Model residuals") +
    scale_shape_manual(values = c("phytoplankton" = 21,
                                  "zooplankton" = 22)) +
    scale_fill_manual(values = my_cols_group) +
    theme_bw() +
    facet_wrap(~ pond, scales = "free", dir = "v") +
    theme(axis.title = element_text(size = 15))
}

make_resid_plot_data <- function(resid_brm_model, data) {
  plot_data <- plyr::ldply(resid_brm_model, function(model) {
    df <- model$data %>%
      tidyr::pivot_longer(cols = c(NO2_uM, NO3_uM, NH3_uM),
                          names_to = "predictor",
                          values_to = "value")
    plyr::ddply(df, .(pond), function(df_i, ...) {
      means <- tapply(df_i$value, df_i$predictor, mean)
      plyr::ddply(df_i, .(predictor), function(dd, model, means) {
        pred_name <- unique(dd$predictor)
        means <- means[names(means) != pred_name]
        nd <- data.frame(x = seq(min(dd$value),
                                 max(dd$value),
                                 length.out = 50),
                         pond = unique(dd$pond),
                         pred_1 = means[[1]],
                         pred_2 = means[[2]])
        names(nd) <- c(pred_name, "pond", names(means))
        fits <- fitted(model, newdata = nd) %>%
          cbind(nd)
        fits %>% {
          data.frame(y = NA,
                     x = NA,
                     x_conf = c(.[[pred_name]], rev(.[[pred_name]])),
                     y_conf = c(.$Q2.5, rev(.$Q97.5)),
                     raw_x = NA,
                     raw_y = NA)
        } %>%
          rbind(fits %>% {
            data.frame(y = .$Estimate,
                       x = .[[pred_name]],
                       x_conf = NA,
                       y_conf = NA,
                       raw_x = NA,
                       raw_y = NA)
          }) %>%
          rbind(dd %>% {
            data.frame(y = NA,
                       x = NA,
                       x_conf = NA,
                       y_conf = NA,
                       raw_x = .$value,
                       raw_y = .$residual_15N_perc)
          })
      }, means = means, ...)
    }, model = model)
  })
  plot_data$treatment <- data$treatment[match(plot_data$pond, data$pond)]
  plot_data$pond <- paste0("Pond = ", formatC(plot_data$pond, width = 2,
                                            format = "d", flag = "0"),
                           "; Treatment = ",
                           ifelse(plot_data$treatment == "H",
                                  "W", plot_data$treatment))
  plot_data
}
