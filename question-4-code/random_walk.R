#install.packages("ggplot2")
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

random_walk  <- function (n_steps) {
  
  df <- data.frame(x = rep(NA, n_steps), y = rep(NA, n_steps), time = 1:n_steps)
  
  set.seed(1) # seed is set to 1 making this reproducible
  
  df[1,] <- c(0,0,1) #initial values x, y, time
  
  for (i in 2:n_steps) {
    
    h <- 0.25 #step size
    
    angle <- runif(1, min = 0, max = 2*pi)
    
    df[i,1] <- df[i-1,1] + rnorm(1, mean = 0, sd = h) #new x postion
    
    df[i,2] <- df[i-1,2] + rnorm(1, mean = 0, sd = h) #new y position
    
    df[i,3] <- i #time is set to step number
    
  }
  
  return(df)
  
}

data1 <- random_walk(500)

plot1 <- ggplot(aes(x = x, y = y), data = data1) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

data2 <- random_walk(500)

plot2 <- ggplot(aes(x = x, y = y), data = data2) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

grid.arrange(plot1, plot2, ncol=2)


### Saving image

library(ragg)

agg_png("Graphs/random_walks.png", 
        width = 20, 
        height = 10, 
        units = "cm", 
        res = 300, 
        scaling = 1)
grid.arrange(plot1, plot2, ncol=2)
dev.off()
