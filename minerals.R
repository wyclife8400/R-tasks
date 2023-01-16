#loading the data
library(readr)
minerals <-read_csv("minerals.csv")



#loading libraries for the plot
library(ggplot2)

#creating a plot
ggplot(minerals,
       aes(x=Antimony,
           y=Gold)) + 
  geom_point(size = 5, alpha =0.5) +
  xlab("Antimony(kjl)")
  ylab("Gold (tones)")
  ggtitle("scatter plot of Gold and Antimony")
  
  #linear regression
  
  #removing missing values
  minerals <- na.omit(minerals)
  
  #fitting the simple linear regression model
  m <-lm(Antimony ~ Gold, data = minerals)
  
  #print model summary
  summary(m)
  
  #plotting the fitted model
  library(ggplot2)
  minerals$predictions <- predict(m)
  ggplot(minerals, aes(Antimony,Gold)) + geom_point() + 
    geom_line(aes( y = predictions), colour = "blue")
  
  
  #more complicated linear regression model.
  
  
  #model diagnostics
  install.packages("ggfortify")
  library(ggfortify)
  autoplot(m)
  
  
  #Bootstrap p-values for non- normal data
  install.packages("boot.pval")
  library(boot.pval)
boot_summary(m)  




