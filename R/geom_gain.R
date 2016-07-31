StatGain <- ggproto("StatGain", Stat,

  setup_data = function(data, params) {
    data$y <- as.factor(data$y)
    data$group <- 1
    data
  },


  compute_group = function(data, scales, formula = y ~ x) {
    lift_curve <- caret::lift(formula, data = data)$data
    data.frame(x = lift_curve$CumTestedPct, y = lift_curve$CumEventPct)
  },

  required_aes = c("x", "y")
)

stat_gain <- function(mapping = NULL, data = NULL, geom = "GeomGain",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(stat = StatGain, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm)
    )
}


GeomGain <- ggplot2::ggproto("GeomGain", Geom,
                    required_aes = c("x", "y"),
                    default_aes = aes(shape = 19, colour = "black", hjust = 10, angle = 90),
                    draw_key = draw_key_point,

                    draw_panel = function(data, panel_scales, coord) {
                      coords <- coord$transform(data, panel_scales)
                      grid::pointsGrob(
                        coords$x, coords$y,
                        pch = coords$shape,

                        gp = grid::gpar(col = coords$colour)
                      )
                    }
)

#' Geom for lift gain curve
#'
#' @import ggplot2
#'
#' @param data
#' @param scales
#' @param formula
#'
#' @return
#' @export
#'
#'
#' @examples
#' fit <- glm(Class ~ Cell.size + Cell.shape, data = BreastCancer, family = "binomial")
#' fitted <- predict(fit, BreastCancer)
#' my_data <- data.frame(x = - fitted, y = BreastCancer$Class)

geom_gain <- function(mapping = NULL, data = NULL, stat = "gain",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    geom = GeomGain, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# my_data %>% ggplot(aes(x = x, y = y)) + geom_gain()
