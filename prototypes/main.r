source('wavemanip.r')
source('wavebank.r')
source('bandmatrix.r')
source('heatgraph.r')

soundwave <- NULL

graph <- function(...) {
  par(mfrow=c(2, 1))
  plot(round(1:length(soundwave) / 44100, 4), soundwave, type='l', las=1, xlab='time', ylab='voltage')
  title(main='oscillograph')
  data <- bandMatrix(soundwave, ...)
  par(mar=c(5, 5, 5, 5))
  heatgraph(data$energies, round(data$times, 3), round(data$frequencies, 2))
  title(main='spectrograph', xlab='time', ylab='frequency')
}

soundwave <- wave.snare()
