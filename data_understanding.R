# Mostra i risultati medi relativi alle misurazioni di ogni canale
visualize_data <- function(data) {
  
  channels <- c(1:NUM_CHANNELS)
  x <- seq(1:SAMPLE_POINTS)
  
  par(mar=c(1,1,1,1))
  par(mfrow=c(3,3))
  
  m <- data.frame()
  
  for (i in channels - 1) {
    start <- (i * SAMPLE_POINTS) + 1
    end <- SAMPLE_POINTS * (i + 1)
    target_data <- data[data$label == 1, c(start:end)]
    nontarget_data <- data[data$label == -1, c(start:end)]
    
    target_avg <- apply(target_data, 2, mean)
    nontarget_avg <- apply(nontarget_data, 2, mean)
    matplot(x, target_avg, type="l", col="red")
    lines(x, nontarget_avg, type="l", col="blue")
    legend(0, max(target_avg), legend=c("Target", "NoTarget"),
           col=c( "red", "blue"), lty=1:1, cex=0.5)
    m <- rbind(m, target_avg)
  }
  average <- apply(m, 2, mean)
  matplot(x, average, type="l", col="red")
  legend(0, max(average), legend=c("Avg Targer"),
         col=c("red"), lty=1:1, cex=0.7)
}