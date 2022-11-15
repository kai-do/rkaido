#' @export
sd <- function(x, dof = 0) {
  sd(x) * sqrt((length(x) - dof) / length(x))
}
