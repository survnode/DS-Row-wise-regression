#Import libraries
library(stringr)
library(dplyr)
library(purrr)

#load data 
iv<-read.csv("iv.csv")
dv<-read.csv("dv.csv")


#Transpose data 
iv.t <- iv[-1] %>% t() %>% as.data.frame() %>% setNames(iv[,1])
dv.t <- dv[-1] %>% t() %>% as.data.frame() %>% setNames(dv[,1])

# Declare correlation dataframe and add
# variable names
corr_df <- as.data.frame(iv[,c(1)])

# Run correlation for similar column names
L <- Map(cor, iv.t[sort(names(iv.t))], dv.t[sort(names(dv.t))])

# Store correlation coefficients in dataframe
corr_df[,2] <- map_df(L, ~as.data.frame(t(.)))

# Rename columns
names(corr_df) <- c("Variable", "Correlation")

# Drop first column containing row names
iv.t<-iv.t[,-1]
dv.t<-dv.t[,-1]

# Declare and initialize dataframe
# to store regression results
reg_df <- data.frame(NULL)              

# Sort variables numerically
str_sort(iv.t, numeric = TRUE)
str_sort(dv.t, numeric = TRUE)

## Apply Quasi regression (takes appx 20 mins to run)
#--------------------------------#
# 'family' changable to [binomial (logistic), gaussian,Gamma, 
#inverse.gaussian, quasi, quasibinomial (logit) and quasipoisson]

# Loop through each column in the iv and dv dataframes and 
# store the main regression statistics

for (i in 1:ncol(iv.t)) {
  model <- summary(glm(dv.t[,i] ~ iv.t[,i], family = "quasi"))  #model
  reg_df[i, 1] <- names(iv.t)[i]           # variable name
  reg_df[i, 2] <- model$coefficients[1,1]   # Regression intercept
  reg_df[i, 3] <- model$coefficients[2,1]   # Variable coefficient
  reg_df[i, 4] <- model$coefficients[2,4]    # Variable Pvalue
}

# Rename columns
names(reg_df) <- c("Variable", "Intercept", "Coefficient","P-value")

# Combine the regression and correlation dataframes to 
# a new dataframes based on the variable name
results <- left_join(corr_df, reg_df, by=c("Variable"))

#Export the data to csv
write.csv(results, "results.csv",row.names = FALSE)
