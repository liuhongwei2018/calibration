#' @title Draws a calibration plot
#' @description Returns a \code{ggplot2} object of calibration plot
#' 
#' @param x Vector of know labels on the test set
#' @param y Vector of probability predictions on the test set
#' @param bins If a single number this indicates the number of splits of the data are used to create the
#' plot. By default, \code{bins} = 10. If a vector, these are actual cuts that will be used.
#' @param smooths set this to TRUE if you want to draw a smoothing line using \code{\link[stats]{loess}},
#' or \code{FALSE} to draw a point graph. By default it is \code{FALSE}.
#' @param conf whether to show 95\% confidence bands, by default it is \code{TRUE}.
#' @param aline whether to connect these points when \code{smooths = FALSE}, by default this is \code{TRUE}
#' @param point_shape the shape of point, see \code{aes_linetype_size_shape}, by default 16
#' @param point_size the size of point, by default it is 3
#' @param span the parameter which controls the degree of smoothing
#' @param errorbar_width the width of the whiskers, by default 0.025
#' @param errorbar_size the size of the whiskers, by default 0.8  
#' @return \code{ggplot2 object}
#' @export

calibration_plot <- function(x, y, bins = 10, smooths = F, conf = T, abline = F,
                     point_shape = 16, point_size = 3, span = 0.9, errorbar_width = 0.025, 
                     errorbar_size = 0.8){
  #加判断  1.判断输入数据类型
  #       2.判断输入数据是否符合要求     
  
  if(length(bins) == 1) {
    num_bins <- bins
    bins <- (0:num_bins)/num_bins
  } else {
    bins <- unique(c(0, bins, 1))  
    num_bins <- length(bins)
  }
  x <- data.frame(x)
  # x must be a data.frame
  binData <- data.frame(prob = x,
                        bin = apply(x, 2,cut, breaks = bins, include.lowest = TRUE),
                        class = y)
  # draw a empty diagram
  p <- ggplot() +
    geom_abline(slope = 1, intercept = 0, col = "black", lty = 2, alpha = .6) +
    theme(panel.background = element_blank(),
          aspect.ratio = 0.7,
          plot.margin = margin(30,0,30,0),
          panel.grid.major.y = element_line(colour = "grey", linetype = 2, size = 0.1), 
          axis.line = element_line(color = "black"),
          axis.title.x = element_text(vjust = 0.1),
          axis.title.y = element_text(vjust = 1),
          legend.title=element_blank()) +
    scale_x_continuous(name="Predicted probability", limits=c(0,1.05), breaks = seq(0,1,0.1), expand = expand_scale(add = 0)) + 
    scale_y_continuous(name = "Observed proportion", limits = c(0,1.05), breaks = seq(0,1,0.1), expand = expand_scale(add = 0))
  
  # add element 
  model_num <- ncol(x)
  
  for (i in 1:model_num) {
    data_tmp <- binData[,c(i,i + model_num,ncol(binData))]
    colnames(data_tmp) <- c("prob","bin","class")
    dataPoints_tmp <- ddply(data_tmp,
                            .(bin),
                            function(x) {
                              if(nrow(x) > 0) {
                                tmp <- binom.test(x = sum(x$class == 1), n = nrow(x))
                                out <- c(Observed = mean(x$class == 1),
                                         Lower  = tmp$conf.int[1],
                                         Upper  = tmp$conf.int[2],
                                         Estimated = mean(x$prob))
                              } else out <- c(Observed = NA, Lower  = NA,
                                              Upper  = NA, Estimated = 0)
                              out
                            },
                            .drop = FALSE)
    
    if(smooths == T ){
      p <- p + geom_smooth(method = "loess", span = span, se = conf,
                           aes(x = Estimated, y = Observed), data = dataPoints_tmp)
    }else if(conf == T){
      if (abline == T){
        p <- p + geom_point(shape = point_shape, size = point_size,
                            aes(x = Estimated, y = Observed),color = i, data = dataPoints_tmp) + 
          geom_errorbar(aes(ymin = Lower, ymax = Upper, x = Estimated), 
                        data = dataPoints_tmp, width= errorbar_width, colour = i, 
                        size= errorbar_size) + 
          geom_line(linetype = 1, size = 1, colour = i, aes(x = Estimated, y = Observed),
                    data = dataPoints_tmp)
      }else{
        p <- p + geom_point(shape = point_shape, size = point_size, color = i,
                            aes(x = Estimated, y = Observed), data = dataPoints_tmp) + 
          geom_errorbar(aes(ymin = Lower, ymax = Upper, x = Estimated), 
                        data = dataPoints_tmp, width= errorbar_width, colour = i, 
                        size= errorbar_size) 
      }
    }else if (abline == T){
      p <- p + geom_point(shape = point_shape, size = point_size, color = i,
                          aes(x = Estimated, y = Observed), data = dataPoints_tmp) + 
        geom_line(linetype = 1, colour = i, size = 1, aes(x = Estimated, y = Observed),
                  data = dataPoints_tmp)
    }else {
      p <- p + geom_point(shape = point_shape, size = point_size, color = i,
                          aes(x = Estimated, y = Observed), data = dataPoints_tmp)
    }
  }
  return(p)
}
