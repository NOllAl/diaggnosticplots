#' ggproto class for stat_lift()
#'
#' @export

StatLift <- ggproto("StatLift", Stat,

                    setup_data = function(data, params) {
                      data$class <- as.factor(data$class)
                      data$group <- 1
                      data
                    },


                    compute_group = function(data, scales, formula = class ~ score,
                                             positive_class = NULL) {
                      lift_curve <- caret::lift(formula, data = data, class = positive_class)

                      data.frame(cut = lift_curve$data$cuts,
                                 lift = lift_curve$data$lift)
                    },

                    required_aes = c("score", "class"),
                    default_aes = aes(x = ..cut.., y = ..lift..)
)

#' Geom for lift curve
#'
#' @description Plots the lift curve for evaluating a trained classification
#' model.
#'
#' @import ggplot2
#'
#' @param data The data to be displayer in this layer.
#' @param mapping Set of aestetic mappings. Required aesthetics are `score` and
#'        `class`.
#' @param positive_class character indicating the "positive" class of interest.
#'
#' @return ggplot
#' @export
#'
#'
#' @examples
#' library(dplyr)
#' library(mlbench)
#' data("BreastCancer")
#' my_data <- BreastCancer %>%
#'   mutate(Cell.size = as.numeric(Cell.size)) %>%
#'     select(Cell.size, Class)
#'
#' fit <- glm(Class ~ ., data = my_data, family = "binomial")
#' my_data$pred <- predict(fit, newdata = my_data)
#' my_data %>%
#'   ggplot(aes(score = pred, class = Class)) +
#'   geom_lift(positive_class = "malignant")

stat_lift <- function(mapping = NULL, data = NULL, geom = "Lift",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      positive_class, inherit.aes = TRUE, ...) {
  layer(stat = Lift, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, positive_class = positive_class)
  )
}

#' ggproto class for geom_lift()
#'
#' @export

GeomLift <- ggproto("GeomLift", Geom,
                    required_aes = c("x", "y"),
                    draw_key = draw_key_point,
                    default_aes = aes(colour = "black"),

                    draw_panel = function(data, panel_scales, coord) {
                      # Reorder points
                      data <- data[order(data[["x"]]), ]

                      coords <- coord$transform(data, panel_scales)

                      grid::linesGrob(
                          coords$x, coords$y,
                          gp = grid::gpar(col = coords$colour)
                          )
                    }
)

#' Geom for lift curve
#'
#' @description Plots the lift curve for evaluating a trained classification
#' model.
#'
#' @import ggplot2
#'
#' @param data The data to be displayer in this layer.
#' @param mapping Set of aestetic mappings. Required aesthetics are `score` and
#'        `class`.
#' @param positive_class character indicating the "positive" class of interest.
#'
#' @return ggplot
#' @export
#'
#'
#' @examples
#' library(dplyr)
#' library(mlbench)
#' data("BreastCancer")
#' my_data <- BreastCancer %>%
#'   mutate(Cell.size = as.numeric(Cell.size)) %>%
#'     select(Cell.size, Class)
#'
#' fit <- glm(Class ~ ., data = my_data, family = "binomial")
#' my_data$pred <- predict(fit, newdata = my_data)
#' my_data %>%
#'   ggplot(aes(score = pred, class = Class)) +
#'   geom_lift(positive_class = "malignant")

geom_lift <- function(mapping = NULL, data = NULL, stat = "lift",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    geom = GeomLift, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
