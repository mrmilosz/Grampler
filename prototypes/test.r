source('bandmatrix.r');
source('wavebank.r');
source('myImagePlot.R');

soundwave <- NULL

oscillo <- function() {
  plot(round(1:length(soundwave) / 44100, 4), soundwave, type='l', xlab='time', ylab='voltage')
}

spectro <- function(...) {
  data <- bandmatrix(soundwave, ...)
  myImagePlot(t(data$energies), xLabels=round(data$times, 4), yLabels=round(data$frequencies, 2), title='spectrograph')
}

soundwave <- wave.snare()
