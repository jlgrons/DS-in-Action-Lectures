# ---------------------------------------------------------------------------- # 
# ------------ Linear Regression --------------------------------------------- #
# ---------------------------------------------------------------------------- #  

# Uncomment the line below to install the package that contains fread() if 
# you haven't installed it before. 
# install.packages('data.table') 
library(data.table) # Loads the package that contains fread()
ice_cream_data <- fread('~/Desktop/ice_cream_dataset_2021.csv')

# Take a peak at the data
head(ice_cream_data)
str(ice_cream_data)

summary(ice_cream_data)

# Fit the linear regression model
my_fit <- lm(ice_cream_sales~temperature, data = ice_cream_data)
summary(my_fit)

# Plot the results
# Uncomment the line below to install ggplot2 if you haven't installed it before. 
# install.packages('ggplot2') 
library(ggplot2)
ggplot(ice_cream_data, aes(y = ice_cream_sales,
                           x = temperature)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(y = 'Ice Cream Sales', x = 'Daily Temperature') +
  ggtitle('2021 Summer Ice Cream Sales') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 78, 
            color = "blue", size=0.5)

# Get the least squares estimates from scratch
get_LS_estimates <- function(x, y){
  ls_slope <-  (y-mean(y))%*%(x - mean(x))/((x-mean(x))%*%(x - mean(x)))
  ls_intercept <- mean(y) - ls_slope*mean(x)
  return(list(ls_intercept = ls_intercept,
              ls_slope = ls_slope))
}

my_LS_estimates <- get_LS_estimates(temperature, ice_cream_sales)
my_LS_estimates
my_fit$coefficients

# ---------------------------------------------------------------------------- # 
# ------------ Logistic Regression ------------------------------------------- #
# ---------------------------------------------------------------------------- #  
 
# Create variable to indicate if $400 was made
ice_cream_data$made_400 <- I(ice_cream_data$ice_cream_sales >= 400)*1

head(ice_cream_data)
str(ice_cream_data)

# Fit the logistic model
my_logistic_fit <- glm(made_400~temperature, data = ice_cream_data,
                      family = 'binomial')
summary(my_logistic_fit)

# Plot the logistic model
ggplot(ice_cream_data, aes(y=made_400, x = temperature)) +
  geom_point() +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=FALSE,
              fullrange=TRUE) +
  labs(x="Daily Temperature", y="Probability of Making $400") +
  ggtitle('2021 Summer Ice Cream Sales') +
  theme(plot.title = element_text(hjust = 0.5)) 
  



