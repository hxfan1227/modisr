#' @import ggplot2
#' @import scales
NULL
#' Plot Taylor Diagram with ggplot2
#' @export
#' @param mods List. A named list which contain the modelled values. Model names are
#' extracted using the list names. Observations for each model should be the same in length.
#' @param obs Vector. A numeric vector which contain the observed values and the length of
#' which should be the same with that of the models.
#' @param normalize Logical. If TRUE, the standard deviation of the observations will be set
#' to 1 and standard deviation of the models will be normalized using the actural standard
#' deviation of the observations. Default is TRUE
#' @param ... Extra arguments. Currently not implemented.
#' @return A ggplot2 object. If not assigned, it will plot in the Plots panel.
#' @examples
#' \dontrun{
#' set.seed(1234)
#' obs <- rnorm(100)
#' mods <- list(mod1 = obs +rnorm(100)*0.1,
#'              mod2 = obs +rnorm(100)*0.1,
#'              mod3 = obs+rnorm(100)/2,
#'              mod4 = obs+rnorm(100)/2)
#' gg_taylor(mods = mods, obs = obs, normalize = T)
#' }

gg_taylor <- function(mods, obs, normalize = T, ...){
  # Calculate the statistics
  model_cors <- plyr::laply(mods, .fun = cor, y = obs)
  model_stds <- plyr::laply(mods, .fun = sd)
  if (normalize){
    obs_std <- 1
    model_stds <- model_stds/sd(obs, na.rm = T)
  } else {
    obs_std <- sd(obs, na.rm = T)
  }
  model_points_df <- data.frame(Model = names(mods), Cor = model_cors, Std = model_stds)
  std_max <- ceiling((max(c(obs_std, model_stds))))
  std_major <- seq(0, std_max, length.out = 5)
  model_rmses <- plyr::laply(mods, .fun = JBTools::RMSE, observation = obs)
  rmse_max <- ceiling(max(model_rmses))
  scale_facor_range <- seq(1, 5, by = 0.1)
  scale_facor_df <- plyr::aaply(scale_facor_range, 1,
                                .fun = function(x){length( which((x * rmse_max * cos(seq(0,pi,pi/1000)) + obs_std)^2 + (x * rmse_max * sin(seq(0,pi,pi/1000)))^2 < max(std_major)^2))})
  scale_facor <- scale_facor_range[sum(scale_facor_df > 0)]
  rmse_major <- round(seq(0, scale_facor * rmse_max, length.out = 10), 1)

  semicircle_df <- plyr::adply(std_major, 1, .fun = function(x){data.frame(x = x * cos(seq(0,pi,pi/1000)),
                                                                           y = x * sin(seq(0,pi,pi/1000)),
                                                                           label = x)})
  cor_major <- c(seq(-1, 1, 0.1),-0.95, 0.95, -0.99, 0.99)
  lines_df <- plyr::adply(cor_major, 1, .fun = function(x){data.frame(xend = 1*max(std_major) * cos(acos(x)),
                                                                      yend = 1*max(std_major) * sin(acos(x)),
                                                                      label = x)})
  get_rmse_semicircle <- function(rms){
    inds <- which((rms * cos(seq(0,pi,pi/1000)) + obs_std)^2 + (rms * sin(seq(0,pi,pi/1000)))^2 < max(std_major)^2)
    x = rms*cos(seq(0,pi,pi/1000))[inds]+obs_std
    y = rms*sin(seq(0,pi,pi/1000))[inds]
    label = rms
    data.frame(x, y, label)
  }
  rmse_semicircle_df <- plyr::adply(rmse_major[-1], 1, .fun = get_rmse_semicircle)
  rmse_text <- data.frame(x = -1 * rmse_major[-1] * cos(pi * rmse_major[-1]/40) + obs_std,
                          y = rmse_major[-1] * sin(pi * rmse_major[-1]/40),
                          label = rmse_major[-1])
  base_plot <- ggplot() +
    coord_equal()+
    ggthemes::theme_base() +
    # geom_segment(aes(x = 0, y = 0, xend = 0, yend = 5)) +
    geom_line(data = semicircle_df, aes(x = x, y = y, group = label), linetype = "solid", colour = "black", size = 0.6) +
    # scale_x_continuous(limits = c(-4.5, 4.5), breaks = seq(-5, 5, by = 1), expand = c(0, 0)) +
    # scale_y_continuous(expand = c(0, 0), limits = c(-0.1, 4.5)) +
    geom_segment(data = lines_df, aes(x = 0, y = 0, xend = xend, yend = yend), linetype = "dashed", colour = "blue") +
    geom_segment(data = lines_df, aes(x = min(xend), y = 0, xend = max(xend), yend = 0)) +
    geom_text(data = lines_df, aes(x = 1.05 * xend, y = 1.05 * yend, label = label), vjust = 0, size = 6, colour = "blue") +
    geom_line(data = rmse_semicircle_df, aes(x = x, y = y, group = label), colour = "darkgreen", size = 0.8, linetype = "dotted") +
    geom_text(data = rmse_text, aes(x = x, y = y, label = label), size = 6, colour = "darkgreen") +
    theme_taylor(base_size = 20)
  base_plot + geom_point(data = model_points_df, aes(x = Std * cos(acos(Cor)), y = Std * sin(acos(Cor)), colour = Model), size = 3) +
    ggsci::scale_colour_npg() +
    theme(legend.position = c(0.9, 0.9))
}
NULL
#' Themes for taylor diagram.
#' @param base_size Numeric. Parameter pass to \code{\link[ggplot2]{theme_bw}}.
#' @param base_family Character. Parameter pass to \code{\link[ggplot2]{theme_bw}}.
theme_taylor <- function(base_size = 11, base_family = "serif"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_blank(),
          panel.grid = element_blank())
}
