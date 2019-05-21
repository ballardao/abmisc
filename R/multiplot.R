#' Multiplot
#'
#' Plot multiple ggplot objects in the same well-formatted space.
#' @param ... Plot objects to be plotted, separated by commas.
#' @param plotlist An alternate way to specify plot order, as a list. Defaults to \code{NULL}.
#' @param cols The number of columns in which the plots will be arranged. Defaults to 1.
#' @param layout An optional matrix of positions for each plot
#' @return The plots in \code{...} or \code{plotlist} will be plotted in a clean manner. To further specify relative size, \code{windows()} can be called before \code{multiplot()}.

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
