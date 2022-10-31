# Install
install.packages("tidyverse")
library(tidyverse)
install.packages(c("rmarkdown", "bookdown", "knitr", "forcats", "htmltools", "readxl", "dslabs", "palmerpenguins", "car", "ggplot2", "ggpubr", "here", "remotes", "nycflights13", "pheatmap", "tidymodels", "embed", "cluster", "gridExtra", "caret", "webchem"))

#install.packages("gitcreds")
#library(gitcreds)
#gitcreds_set()
###

#Lists
list_4.1 <-list(1:360, LETTERS, 2*pi/1:360, 2^(1:20), list("Hello,", "World", log10(1:100)))
list_4.1[[5]][[3]][99]
# Create a list with named elements "a", "b", "c", "d" and "e"
list_3 <- list(a = 2, b = "ernie", c = c(11:20), d = FALSE, e = list_1) 
# Extract an element by refering to the name of the element
list_3[["a"]] 
# Check if elements of a list have names
names(list_3)
# Change the name of the 4th element of list_3 to "pino"
names(list_3)[4] <- "pino" 
names(list_3)

#Dataframe
df1 <-data.frame(var1 = 1:5, var2 = c("a", "b", "c", "d", "e"), var3 = c(rep("bert", 3) , rep("ernie", 2)))
df1


### !!! To make dataframes compatible with the tidyverse package, we have to change an R base dataframe to a tibble
### A tibble is also a dataframe but with different behavior

library(tidyverse) 
mtcars %>% ggplot(aes(x = hp, y = mpg)) +   
  geom_point() +  
  geom_smooth(se = FALSE)

tibble_1 <- tibble(number = 1:10, letters = letters[1:10], random = runif(10))  
is_tibble(tibble_1)
class(mtcars)  
tibble_2 <- as_tibble(mtcars)  
class(tibble_2)
mtcars
tibble_2
#With non-syntatic names use ''
weird_column_names <- tibble(`0` = 1:10, `@` = seq(102, 120, 2), `&*(` = letters[1:10])
colnames(weird_column_names)

###

library(forcats)
flu_month <- tibble(patient = paste0(c("patient"), 1:16),
                    month = c("jan", "jam", "mar", "nov", "nov", "dec", "feb", 
                              "apr", "jun", "sept", "dec", "feb", "aug", "may", 
                              "jul", "sep"))
flu_month
flu_month %>% count(month)
# Search for data values that are not in the level list -->
# Create a vector containing the level definition
month_levels <- c("jan", "feb", "mar", "apr", "may" ,"jun", 
                  "jul", "aug", "sep", "oct", "nov", "dec") 
parse_factor(flu_month$month, levels = month_levels)
# Correct the misspellings
flu_month$month[flu_month$month=="jam"] <- c("jan") 
flu_month$month[flu_month$month=="sept"] <- c("sep") 
#We can now convert the character variable “month” to a factor variable
flu_month$month <- factor(flu_month$month, levels = month_levels) 
flu_month
levels(flu_month$month)
#count() summaraizes data
flu_month %>% count(month)

###

library(drc)
#This package contains functions and data for the analysis of dose-response curves
ryegrass
#We use the function drm() (Dose Response Model), which takes in this case 3 inputs
model<- drm(formula = rootl~conc, # we are interested in rootlength as a function of (~) concentration
            data=ryegrass, # input
            fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50"))) #LL.4 means 4-parameter log-logistic
summary(model)
plot(model, type="all")
?ED
ED(model, c(10,20,50), interval="delta")

toxdata <- ryegrass
# W2.3: a weibull type 2 function with 4 parameters
model.W24 <-  drm(rootl~conc, data=toxdata, fct=W2.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
# a Log logistic model with 3 parameters
# (3 parameters means fixing the lower limit at zero. Which makes sense as rootlength can't be negative)
model.LL3 <-  drm(rootl~conc, data=ryegrass, fct=LL.3(names = c("Slope", "Upper Limit", "ED50")))
plot(model.LL3, broken = TRUE, xlab="Concentration", ylab="Percent Response", type='all',lty=1, lwd=2)
plot(model.W24, add=TRUE,col="orange",lty=1, lwd=2)

#You can calculate the Akaike’s information criterion and the residual variance (smaller = better) with the mselect() function, to make an informed decision between different models:
mselect(model.LL3, fctList = list(W2.4()), linreg=TRUE) 
