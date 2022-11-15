#' @export
sd_pop <- function(x) {
  sd(x) * sqrt((length(x) - 1) / length(x))
}

