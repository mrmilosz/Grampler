heatgraph <- function(data, xlabels=1:length(data[,1]), ylabels=1:length(data[1,])) {
  data.min <- min(data)
  data.max <- max(data)
  image(1:length(xlabels), 1:length(ylabels), data, zlim=c(data.min, data.max), axes=FALSE, xlab="time", ylab="frequency")
  axis(1, at=0:length(xlabels)+0.5, labels=c(0,xlabels), padj=-0.4)
  axis(2, at=0:length(ylabels)+0.5, labels=c(1,ylabels), las=1, cex.axis=0.7)
}
