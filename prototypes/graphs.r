source('spectrum.r')
source('wavebank.r')

oscillograph <- function(soundwave, sampleRate=44100) {
  par(mar=c(5, 5, 5, 5))
  plot(round(1:length(soundwave) / sampleRate, 4), soundwave, type='l', las=1, xlab='time', ylab='voltage')
  title(main='oscillograph')
}

spectrograph <- function(soundwave, ...) {
  par(mar=c(5, 5, 5, 5))
  data <- spectrum(soundwave, ...)
  xlabels <- round(data$times, 3)
  ylabels <- round(data$frequencies, 2)
  image(1:length(xlabels), 1:length(ylabels), data$energies, zlim=range(data$energies), axes=FALSE, xlab="time", ylab="frequency")
  axis(1, at=0:length(xlabels)+0.5, labels=c(0,xlabels), padj=-0.4)
  axis(2, at=0:length(ylabels)+0.5, labels=c(0,ylabels), las=1, cex.axis=0.7)
  title(main='spectrograph')
}
