open_plot_window <- function(width = 7, height = 7, ...) {
  if (.Platform$OS.type == "unix") {
    quartz(width = width, height = height); plot(1:10)
  }  else { windows(width = width, height = height) }
}
