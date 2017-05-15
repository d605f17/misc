plotResults <- function(data) {
  plot <- barplot(data,
                  beside = TRUE,
                  col = c("red", "blue", "green"),
                  space = c(0, 0.4),
                  width = 0.1,
                  xlim = c(0, 2),
                  ylim = c(0, 1.1),
                  legend = c("KNN", "MF1", "MF2"),
                  args.legend = list(x = 2.7, y = 1.2),
                  yaxt = 'n',
                  xpd = TRUE,
                  srt = 90
          )
  text(x = plot,
       y = c(max(data[, 1]) + 0.05, max(data[, 1]) + 0.05, max(data[, 1]) + 0.05,
             max(data[, 2]) + 0.05, max(data[, 2]) + 0.05, max(data[, 2]) + 0.05,
             max(data[, 3]) + 0.05, max(data[, 3]) + 0.05, max(data[, 3]) + 0.05,
             max(data[, 4]) + 0.05, max(data[, 4]) + 0.05, max(data[, 4]) + 0.05,
             max(data[, 5]) + 0.05, max(data[, 5]) + 0.05, max(data[, 5]) + 0.05,
             max(data[, 6]) + 0.05, max(data[, 6]) + 0.05, max(data[, 6]) + 0.05,
             max(data[, 7]) + 0.05, max(data[, 7]) + 0.05, max(data[, 7]) + 0.05,
             max(data[, 8]) + 0.05, max(data[, 8]) + 0.05, max(data[, 8]) + 0.05),
       labels = as.character(round(data, digits = 2)),
       srt = 90,
       xpd = TRUE)
}
