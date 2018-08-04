#-------------------------------------------------------------------------------------------------------------------------------
# Reading data into dataframes from csv
#-------------------------------------------------------------------------------------------------------------------------------
df_ibm = read.csv('IBMStock.csv')
df_boing = read.csv('BoeingStock.csv')
df_ge = read.csv('GEStock.csv')
df_cocacola = read.csv('CocaColaStock.csv')
df_proctorgamble = read.csv('ProcterGambleStock.csv')

#-------------------------------------------------------------------------------------------------------------------------------
# Converting dates into R format (Problem 1.1)
#-------------------------------------------------------------------------------------------------------------------------------
# The first argument to the as.Date function is the variable we want to convert,
# and the second argument is the format of the Date variable.
# We can just overwrite the original Date variable values with the output of this function.
# Now, answer the following questions using the str and summary functions.
#-------------------------------------------------------------------------------------------------------------------------------
df_ibm$Date = as.Date(df_ibm$Date,"%m/%d/%y")
df_boing$Date = as.Date(df_boing$Date,"%m/%d/%y")
df_ge$Date = as.Date(df_ge$Date,"%m/%d/%y")
df_cocacola$Date = as.Date(df_cocacola$Date,"%m/%d/%y")
df_proctorgamble$Date = as.Date(df_proctorgamble$Date,"%m/%d/%y")

#=============================(Problem 1.1)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Total number of observations (i.e. sum of all observations ) each data set has 480 observations. 
# We have monthly data for 40 years, so there are 12*40 = 480 observations. 
#-------------------------------------------------------------------------------------------------------------------------------
total_observations = nrow(df_ibm) + nrow(df_boing) + nrow(df_ge) + nrow(df_cocacola) + nrow(df_proctorgamble)
cat("Total number of observations are:",  total_observations,'\n')

#=============================(Problem 1.2,1.3)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Earliest and Latest Year in the Dataset
#-------------------------------------------------------------------------------------------------------------------------------
cat("Earliest date: ", min(df_ibm$Date),'\n')
cat("Latest date: ", max(df_ibm$Date),'\n')

#=============================(Problem 1.4)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Mean stock price
#-------------------------------------------------------------------------------------------------------------------------------
#cat("Mean stock price of IBM stock is: ",format(round(mean(df_ibm$StockPrice),2),nsmall = 2),'\n')
cat("Mean stock price of IBM stock is: ",specify_decimal(mean(df_ibm$StockPrice),2),'\n')

#=============================(Problem 1.5)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Min stock price
#-------------------------------------------------------------------------------------------------------------------------------
cat("Min stock price of GE stock is: ",specify_decimal(min(df_ge$StockPrice),2),'\n')

#=============================(Problem 1.6)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Max stock price
#-------------------------------------------------------------------------------------------------------------------------------
cat("Max stock price of CocaCola stock is: ",specify_decimal(max(df_cocacola$StockPrice),2),'\n')

#=============================(Problem 1.7)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Median stock price
#-------------------------------------------------------------------------------------------------------------------------------
cat("Median stock price of Boeing stock is: ",specify_decimal(median(df_boing$StockPrice),2),'\n')

#=============================(Problem 1.8)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Standard Deviation stock price
#-------------------------------------------------------------------------------------------------------------------------------
cat("Standard Deviation of stock price of ProctorGamble stock is: ",specify_decimal(sd(df_proctorgamble$StockPrice),2),'\n')

#=============================(Problem 2.1 - 2.3)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Plotting (Problem 2.1 - 2.3)
#-------------------------------------------------------------------------------------------------------------------------------
plot(df_cocacola$Date,df_cocacola$StockPrice, type = "l",col = "red")
lines(df_proctorgamble$Date,df_proctorgamble$StockPrice,col = "blue",lty = 2) # Adds line plot over the previous plot lty=2 is dotted line

# Next argument generates a vertical line at the date March 1, 2000. 
# The argument lwd=2 makes the line a little thicker. You can change the date in this command to generate the vertical line in different locations.
abline(v=as.Date(c("1983-01-01")), lwd=1)
abline(v=as.Date(c("1983-12-31")), lwd=1)

#=============================(Problem 3.1 - 3.4)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Plotting (Problem 3.1 - 3.4)
#-------------------------------------------------------------------------------------------------------------------------------
#par(bg="darkblue",fg = "white") #sets background colour
plot(df_cocacola$Date[301:432],df_cocacola$StockPrice[301:432],type = "l",col = "red",ylim = c(0,210),xlab = 'Date', ylab = 'StockPrice')
lines(df_boing$Date[301:432],df_boing$StockPrice[301:432],type = "l",col = "blue",ylim = c(0,210))
lines(df_ge$Date[301:432],df_ge$StockPrice[301:432],type = "l",col = "green",ylim = c(0,210))
lines(df_proctorgamble$Date[301:432],df_proctorgamble$StockPrice[301:432],type = "l",col = "black",ylim = c(0,210))
lines(df_ibm$Date[301:432],df_ibm$StockPrice[301:432],type = "l",col = "pink",ylim = c(0,210))
# Add a legend to the plot
legend(x = "topleft",y = "topleft", legend=c("CocaCola", "Boing","GE","ProctorGamble","IBM")
       ,col=c("red", "blue","green","black","pink")
       ,lty=1,cex = 0.7, text.font= 9, bg='white')
abline(v=as.Date(c("2000-03-01")), lwd=1)#march 2000
abline(v=as.Date(c("1997-09-01")), lwd=1)#sept 1997
#abline(v=as.Date(c("1997-10-01")), lwd=1,lty = 2)#oct 1997
abline(v=as.Date(c("1997-11-01")), lwd=1)#nov1997



#=============================(Problem 4.1)===============================
#-------------------------------------------------------------------------------------------------------------------------------
# Problem 4.1
#-------------------------------------------------------------------------------------------------------------------------------
print("IBM")
print(tapply(df_ibm$StockPrice,months(df_ibm$Date),mean))
print("Boing")
print(tapply(df_boing$StockPrice, months(df_boing$Date), mean))
print("GE")
print(tapply(df_ge$StockPrice, months(df_ge$Date), mean))
print("CocaCola")
print(tapply(df_cocacola$StockPrice, months(df_cocacola$Date), mean))
print("ProctorGamble")
print(tapply(df_proctorgamble$StockPrice, months(df_proctorgamble$Date), mean))



#-------------------------------------------------------------------------------------------------------------------------------
#============== My Functions ================
#-------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------------
# Print formatting dotted line seperator
#-------------------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------------------
# Specify decimal points.
# x is the number and k is the number of decimals to show. 
# trimws removes any leading white space.
#-------------------------------------------------------------------------------------------------------------------------------
specify_decimal <- function(x, k) {
  trimws(format(round(x, k), nsmall=k))
}
  
