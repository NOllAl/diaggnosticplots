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
    lift_curve <- caret::lift(formula, data = data, class = positive_class)

    data.frame(cum_tested_pct = lift_curve$data$CumTestedPct,
               cum_event_pct = lift_curve$data$CumEventPct,
               pct = lift_curve$pct)
  },

  required_aes = c("score", "class"),
  default_aes = aes(x = ..cum_tested_pct.., y = ..cum_event_pct..)
)

stat_gain <- function(mapping = NULL, data = NULL, geom = "Gain",
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
                    default_aes = aes(colour = "black",
                                      fill_color = "pink",
                                      fill_alpha = .3,
                                      width = 1),

                    draw_panel = function(data, panel_scales, coord) {
                      # Reorder points
                      data <- data[order(data[["x"]]), ]

                      coords <- coord$transform(data, panel_scales)

                      n <- nrow(coords)
                      lm_x <- lm(x ~ cum_tested_pct, data = coords) %>%
                        predict(data.frame(cum_tested_pct = data$pct)) %>%
                        unique()
                      lm_y <- lm(y ~ cum_event_pct, data = coords) %>%
                        predict(data.frame(cum_event_pct = 100)) %>%
                        unique()

                      grid::gList(
                        grid::polygonGrob(
                          c(coords$x[c(1, n)], lm_x, coords$x[1]),
                          c(coords$y[c(1, n)], lm_y, coords$y[1]),
                          gp = grid::gpar(alpha = coords$fill_alpha,
                                          fill = coords$fill_color)
                        ),
                        grid::linesGrob(
                          coords$x, coords$y,
                          gp = grid::gpar(col = coords$colour,
                                          lwd = coords$width))

                        )
                    }
)

#' Geom for gain curve
#'
#' @import ggplot2
#'
#' @param data
#' @param scales
#' @param mapping Set of aestetic mappings.
#' @param positive_class character indicating the "positive" class.
#'
#' @return
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
#'   geom_gain(positive_class = "malignant")

geom_gain <- function(mapping = NULL, data = NULL, stat = "gain",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    geom = GeomGain, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# my_data %>% ggplot(aes(score = pred, class = Class)) +
#   geom_gain(positive_class = "malignant", fill_color = "blue", fill_alpha = 0.1, width = 1)
