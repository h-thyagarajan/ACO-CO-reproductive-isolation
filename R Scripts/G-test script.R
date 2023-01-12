######## G-test script ##########

library(dplyr)
library(DescTools)

# same method used for female, male and group choice assay

################################################################
## example given for female mate choice assay 

dat<-read.csv(file.choose()) #choose "female choice- compiled data.csv"

## start with A1 females- determine the number of A and C males females mated with in each trial

dat1<- dat %>% filter(female.id== "A1")
table(dat1$female.id, dat1$mated.male.pop, dat1$trial.id)

## step 1:  Individual G-value- fill in observed values (observed # A's mated, observed # C's mated) and expected proportions (0.5, 0.5) under null hypothesis
# repeat this for each trial (1-4)

#example for A1 female trial 1

observed =c(21, 26)
expected =c(0.5,0.5)
GTest(x=observed, p=expected, correct="none") #output gives individual G value, df, p-value (testing for deviation from expected proportion)

#repeat this for A1 female trials 2, 3 and 4. 

## step 2: G-test of independence- fill in observed values across all four trial for homotypic and heterotypic matings

Input =("
 Trial   Homo   Hetero       
  1       21     26
  2       30     18
  3       22     21
  4       29     20
        ")

Matriz = as.matrix(read.table(textConnection(Input),
                              header=TRUE, 
                              row.names=1)) ; Matriz

GTest(Matriz,
      correct="none") # output gives heterogeneity G value, df, p-value (testing for significant heterogeneity between trials)

## step 3: Pooled G-value- fill in values pooled across replicates and expected proportions under null hypothesis

observed =c(102, 85)
expected =c(0.5,0.5)
GTest(x=observed, p=expected, correct="none") # output gives pooled G value, df, p-value (testing for deviation from null hypothesis when observations pooled across trials)

## step 4: Total G-value (calculated in excel)

# G-value= Heterogeneity G + Pooled G
# df= df (heterogeneity) + df (pooled)
# p-value calculated using CHIDIST function in excel, filling in total g-value and df
   # this p-value tests for overall deviation from null hypothesis across replicates

###########################################################################

# Steps 1-4 then repeated for A3, A5, C1, C3, C5 females. 

###########################################################################

# Steps 1-4 then repeated for overall A and C populations 
## in this case pooled values for each 1,3,5 replicate used in step 1
## Step 2 tests for heterogeneity among overall replicates 
## Step 3 pools observations across overall A and C populations 
## Step 4 calculates overall G-value for overall A and C populations


######## this method then be repeated for the male choice 'male choice- compiled data.csv' and group choice 'group choice- compiled data.csv' analysis ########### 



