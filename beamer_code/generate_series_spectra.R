###############################################################################
# Script to generate examples of spectra
# Author: Filippo Palomba
# Last Modified: 10 Feb 2022
###############################################################################
pacman::p_load(ggplot2, latex2exp)
theme_set(theme_bw())

setwd("D:\\Dropbox\\Research\\Projects\\fourier_fe\\beamer_code")

spectrumAR <- function(w,phi,sigma2_e){ 
  spectrum <- (1/(2*pi))*sigma2_e/( 1 - 2*phi*cos(w) + phi^2 )
  return( spectrum )
}

sigma2 <- 1.0
TT <- 100
set.seed(8894)
eps <- rnorm(TT, mean = 0, sd = sqrt(sigma2))

j <- 0
for (phi in c(0.9,0,-0.9)) {
  j <- j + 1
  grid_spec <- seq(from = 0, to = pi, length.out = 200)
  spec_y    <- spectrumAR(grid_spec, phi, sigma2)
  
  df <- data.frame(cbind(grid_spec,spec_y))
  
  ggplot(data=df, aes(x=grid_spec, y=spec_y)) +
    geom_line(size = 1.2, color = "#FC4E07") + xlab(TeX('$w$')) + ylab(TeX('$f(w)$'))
  
  ggsave(paste0("spectrum_",j,".png"), height = 4, width = 5, dpi = 1000)
  
  y <-  matrix(NA, TT, 1)
  y[1,1] <- eps[1]
  
  for (t in seq(2, TT, by = 1)) {
    y[t,1] = y[t-1,1]*phi + eps[t]
  }
  
  grid_time <- seq(from = 1, to = TT, by = 1)
  df <- data.frame(cbind(grid_time, y))
  
  ggplot(data=df, aes(x=grid_time, y=y)) +
    geom_line(size = 1.2, color = "#FC4E07") + xlab(TeX('$t$')) + ylab(TeX('$y_t$'))
  ggsave(paste0("series_",j,".png"), height = 4, width = 5, dpi = 1000)
}

