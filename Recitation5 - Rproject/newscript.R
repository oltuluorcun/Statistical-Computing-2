getwd()

set.seed(292)
x <- rnorm(50, 0, 1)

A <- matrix(x, ncol = 5, dimnames = list(paste("row.", 1:10, sep=""),
                                         paste("col.", 1:5, sep="")))
A

write.csv(A, file = "MatrixA2.csv")
# buraya geri don !! 
# apply(A, 2, mean)
# write(colMeans(A), file = "Colmeans2.txt")

source("Functions3.R")
myfunction(A)

x <- 10
y <- list(1:10,letters[1:10])
z <- c("mon","tue","wed","thu","fri","sat","sun")

B <- matrix(rnorm(80,100,2), nrow = 20, ncol = 4,
            dimnames = list(paste("row.", 1:20, sep=""),
                            paste("col.", 1:4, sep="")))

save(x,y,z,A,B, file = "mydata3.RData")

rm(list=ls())

load("mydata3.RData")
ls()


library(tidyverse)
library(magrittr)
library(dplyr)
library(ISLR)

####################################################################
# ex.2.
## Part A

as.character(round(log(5:12),2))
5:12 %>% log %>% round(2) %>% as.character

# ctrl + shift + m

A %>% abs %>% round(3) %>% t %>% "%*%" (A)
# multiply_by_matrix(.,A)

set.seed(292)
Y <- sample(10:30, 30, replace = T)
Y_hat <- rnorm(30,3,2) + Y

Y %>% "-"(Y_hat) %>% "^"(2) %>% 
  sum() %>% "/"(length(Y)) %>% sqrt

## 
Y %>% "-" (Y_hat) %>% "^"(2) %>% mean() %>% sqrt
Y-Y_hat %>% "^"(2) %>% mean() %>% sqrt
(Y-Y_hat) %>% "^"(2) %>% mean() %>% sqrt
##
sqrt(mean((Y-Y_hat)^2))

### 
# part D 

data(iris)
head(iris)

iris %>% aggregate( . ~ Species, ., mean)

iris %>% subset(Petal.Length > 3) %>% 
  aggregate(cbind(Sepal.Length,Sepal.Width) ~ Species,., mean)

iris %>% select(Sepal.Length, Petal.Length) %T>%
  plot(main="ScatterPlot",col="Dark Blue", pch=4) %>% 
  cor
  
iris %>% select(Species) %>% table()
iris %$% table(Species) 

### ex 3

iris %>% select(Sepal.Length, Species) %>% head(10)

iris %>% select(Petal.Length, Petal.Width) %>% head(10)

iris %>% select(starts_with("Petal")) %>% head(10)

iris %>% select_if(is.numeric) %>% head(10)
iris %>% select(where(is.numeric)) %>% head(10)

#head(iris[,sapply(iris, is.numeric)],10)

iris %>% 
  filter(Species == "setosa", Sepal.Length >= 4.4 , Sepal.Length <= 4.8)

iris %>% 
  filter(Species == "setosa", between(Sepal.Length,4.4,4.8))

iris %>% 
  group_by(Species) %>% 
  summarise(mean(Petal.Length))

iris %<>% 
  mutate(Proportion = Sepal.Length/Sepal.Width)

iris %<>% 
  mutate(Type = ifelse(Proportion < 1.8, "A","B"))
  
iris %>% group_by(Species) %>% 
  filter(Type == "B") %>% 
  count

################# 

set.seed(292)
smp1 <- runif(5000,10,20)
smp2 <- rnorm(5000,15,2)
smp3 <- rnorm(5000)

mydf <- cbind(smp1, smp2, smp3)

system.time({
means <- numeric()
for(i in 1:nrow(mydf)){
  means[i] <- mean(mydf[i,])
}
})
system.time(apply(mydf, 1, mean))
system.time(rowMeans(mydf))



system.time({
  vec2 <- numeric()
  for(i in 1:100){
    vec <- rnorm(5000)
    vec2[i] <- shapiro.test(vec)$p.val
  }
})

system.time(
rnorm(5000*100) %>%
  matrix(.,ncol=100, byrow = TRUE) %>%
  apply(.,2,function(x) shapiro.test(x)$p.val)
)


data <- read.table("bmi_data.txt")
BMI <- numeric()
Status <- character()

system.time({
  for(i in 1:nrow(data)){
    BMI[i] <- data$Weight[i] / data$Height[i]^2
    if(BMI[i] <= 18.5){
      Status[i] <- "underweight"
    } else if(BMI[i] > 18.5 & BMI[i] < 24.9){
      Status[i] <- "normal"
    } else{
      Status[i] <- "overweight"
    }
  }
  data <- cbind(data, BMI, Status)
})


data <- read.table("bmi_data.txt")
system.time({
  data %<>%
    mutate(Status = ifelse(Weight/Height^2 <= 18.5,
                           "underweight",
                           ifelse(Weight/Heightˆ2 >= 24.9,
                                  "overweight",
                                  "normal")))
})





data %<>%
  mutate(BMI = Weight / Height^2) %>% 
  mutate(Status = ifelse(BMI < 18.5, "under",
                         ifelse(BMI > 24.9, "over", "normal")))

data
























