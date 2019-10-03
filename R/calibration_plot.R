#' @title Draws a calibration plot
#' @description Returns a ggplot2 object of calibration plot
#' @param y_true Vector of know labels on the test set
#' @param y_prob Vector of probability predictions on the test set
#' @export

calibration_plot <- function(y_true, y_prob, normalize=FALSE, smooths = F, conf = T){
  if (!(is.vector(y_true)&is.vector(y_prob)))
    stop('a')
  if (normalize){
    y_prob = (y_prob - min(y_prob)) / (max(y_prob) - min(y_prob))

  }else if(min(y_prob)<0 | max(y_prob) > 1)
    stop('......')
  bins = 10
  if(length(bins) == 1) {
    num_bins <- bins
    bins <- (0:num_bins)/num_bins
  } else {
    bins <- unique(c(0, bins, 1))
    num_bins <- length(bins)
  }

  binData <- data.frame(prob = y_prob,
                        bin = cut(y_prob, bins, include.lowest = TRUE),
                        class = y_true)

  dataPoints <- ddply(binData,
                      .(bin),
                      function(x) {
                        if(nrow(x) > 0) {
                          tmp <- binom.test(x = sum(x$class == 1), n = nrow(x))
                          out <- c(Percent = mean(x$class == 1),
                                   Lower  = tmp$conf.int[1],
                                   Upper  = tmp$conf.int[2],
                                   Count = sum(x$class == 1))
                        } else out <- c(Percent = NA, Lower  = NA,
                                        Upper  = NA, Count = 0)
                        out
                      },
                      .drop = FALSE)

  dataPoints$midpoint <- NA
  for(i in 2:length(bins))
    dataPoints$midpoint[i-1] <- .5*(bins[i] + bins[i-1])
  dataPoints$Percent <- ifelse(dataPoints$Count == 0, 0, dataPoints$Percent)


  p <- ggplot(dataPoints, aes(x = midpoint, y = Percent)) +
    geom_abline(slope = 1, intercept = 0, col = "red", lty = 2, alpha = .6) +
    theme(panel.background = element_blank(),
          aspect.ratio = 0.7,
          plot.margin = margin(30,0,30,0),
          panel.grid.major.y = element_line(colour = "grey", linetype = 2, size = 0.1),
          axis.line = element_line(color = "black"),
          axis.title.x = element_text(vjust = 0.1),
          axis.title.y = element_text(vjust = 1)) +
    scale_x_continuous(name="Predicted probability", limits=c(0,1), breaks = seq(0,1,0.1), expand = expand_scale(add = 0)) +
    scale_y_continuous(name = "Observed proportion", limits = c(0,1), breaks = seq(0,1,0.1), expand = expand_scale(add = 0))

  if(smooths == T ){
    p <- p + geom_smooth(method = "loess", span = 0.9, se = conf)
  }else if(conf == T){
    p <- p + geom_point() +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width=0.025, colour = "black", size=0.8)
  }else{
    p <- p + geom_point()
  }
  return(p)
}
