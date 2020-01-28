visualize_data <- function(data) {
  
  data <- data[, -1]
  channels <- c(1:8)
  x <- seq(1:204)
  
  par(mar=c(1,1,1,1))
  par(mfrow=c(3,3))
  
  for (i in channels - 1) {
    start <- (i * 204) + 1
    end <- 204 * (i + 1)
    target_data <- data[data$label == 1, c(start:end)]
    nontarget_data <- data[data$label == -1, c(start:end)]
    
    target_avg <- apply(target_data, 2, mean)
    nontarget_avg <- apply(nontarget_data, 2, mean)
    matplot(x, target_avg, type="l", col="red") #red=no target ->-1
    lines(x, target_avg - nontarget_avg, type="l", col="blue") #red=no target ->-1
    #lines(x, nontarget_avg, type="l",col="green") #green= target ->1
  }
  #v<-summary(((df[1,])))
}