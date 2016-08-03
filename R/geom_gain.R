# library(mlbench)
# data("BreastCancer")
# my_data <- BreastCancer %>%
#   mutate(Cell.size = as.numeric(Cell.size)) %>%
#   select(Cell.size, Class)
#
# fit <- glm(Class ~ ., data = my_data, family = "binomial")
# my_data$pred <- predict(fit, newdata = my_data)




StatGain <- ggproto("StatGain", Stat,

  setup_data = function(data, params) {
    data$class <- as.factor(data$class)
    data$group <- 1
    data
  },


  compute_group = function(data, scales, formula = class ~ score,
                           positive_class = NULL) {
    lift_curve <- caret::lift(formula, data = data, class = positive_class)$data
    data.frame(cum_tested_pct = lift_curve$CumTestedPct,
               cum_event_pct = lift_curve$CumEventPct)
  },

  required_aes = c("score", "class"),
  default_aes = aes(x = ..cum_tested_pct.., y = ..cum_event_pct..)
)

stat_gain <- function(mapping = NULL, data = NULL, geom = "GeomGain",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      positive_class, inherit.aes = TRUE, ...) {
  layer(stat = StatGain, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, positive_class = positive_class)
    )
}


GeomGain <- ggproto("GeomGain", Geom,
                    required_aes = c("x", "y"),
                    draw_key = draw_key_point,
                    default_aes = aes(shape = 20, colour = "black"),

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


# my_data %>% ggplot(aes(score = pred, class = Class)) + geom_gain(positive_class = "malignant")
