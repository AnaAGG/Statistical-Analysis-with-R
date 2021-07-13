## Table of contents 
- [**1. Types of response variable distributions**](#1-types-of-response-variable-distributions)
    - [**1.1 Gaussian or Normal distribution**](#11-gaussian-or-normal-distribution)
    - [**1.2 Poisson distribution**](#12-poisson-distribution)
    - [**1.3. Binomial Distribution**](#13-binomial-distribution)
    - [**1.4 Negative Binomial distribution**](#14-negative-binomial-distribution)
- [2. Types predictors](#2-types-predictors)
    - [**2.1 Continous factors**](#21-continous-factors)
    - [**2.2 Offsets**](#22-offsets)
    - [**2.3 Within- and between-groups factors**](#23-within--and-between-groups-factors)
    - [**2.4 Fixed and random factors**](#24-fixed-and-random-factors)
    - [**2.5 Nested factors**](#25-nested-factors)
- [3. Basic n-way ANOVA models](#3-basic-n-way-anova-models)
- [4. Tables and types of contrasts](#4-tables-and-types-of-contrasts)
- [5. Check the assumptions for the "good" model](#5-check-the-assumptions-for-the-good-model)
    - [**5.1 Normality (residuals)**](#51-normality-residuals)
    - [**5.2 Homocedasticity (residuals)**](#52-homocedasticity-residuals)
    - [**5.3. Influent and lost points**](#53-influent-and-lost-points)
    - [**5.4. Variance homogeneity**](#54-variance-homogeneity)
    - [**5.5. Independence between predictor variables-factors - collinearity**](#55-independence-between-predictor-variables-factors---collinearity)


# **1. Types of response variable distributions**

In statistic we can find four main distributions: 

### **1.1 Gaussian or Normal distribution**
   
![distribution](https://github.com/AnaAGG/Statistical-Analysis-with-R/blob/main/Images/normal_disrtibution.jpeg)

Is a type of continuous probability distribution. Is defined by two statistics: 

- Mean ($\mu$) =  0

- Standard deviation ($\sigma$) = 1. It is a measure how spread out our date are. 
  
The parameter $\mu$  is the mean of the distribution, while the parameter $\sigma$  is its standard deviation. The variance of the distribution is $\sigma^2$. A random variable with a Gaussian distribution is said to be normally distributed, and is called a normal deviate.
  
Key points:
- A normal distribution is the proper term for a probability bell curve.

- Normal distributions are symmetrical, but not all symmetrical distributions are normal.

- In normal distribution:
  
    > $\mu$  = median = mode
    
### **1.2 Poisson distribution**
   This distriburtion has the following characteristics:
   - Created by integers (usually counts)
   - It can not have negative values
   - Its distribution is described by only one parameter, lambda ($\lambda$), so that the mean is equal to the variance of the distribution
  
        > $\lambda$ **-->** $\mu$  = $\sigma^2$

⚠️ Poisson distributions with high $\lambda$ (i.e. far from zero) can resemble a Gaussian
### **1.3. Binomial Distribution**

### **1.4 Negative Binomial distribution**

More spread out distributions than a Poisson (variance > mean). Their characteristics are:
- Distribution of integers (usually counts)
- It can not have negative values
- Are described by two parameters:
  - Mean ($\mu$)
  - Inflated variance (called `size` in R)

The variance is defines by: 

$$
\mu \ + \mu^2 / size
$$

When size is very large `1/size` tend to 0 and the variance ends up looking by $\mu$

Depending on the nature of our response variable, we will work with different types of models: 
|Linear General Models|Linear Generalised Linear Models|
|:-------:|:-------:|
|Gaussian Distribution|Poisson Distribution|
||Binomial Distribution|
||Negative Binomial Distribution|

⚠️ If the distribution of the response variable is neither Poisson, Negative Binomial or Binomial, but it does not exhibit Gaussian characteristics eirther then:

    TRANSFORM THE RESPONSE VARAIBALE AND APPLY GENERAL LINEAR MODELS

# 2. Types predictors

### **2.1 Continous factors**
### **2.2 Offsets**
### **2.3 Within- and between-groups factors**
### **2.4 Fixed and random factors**
### **2.5 Nested factors**

# 3. Basic n-way ANOVA models
# 4. Tables and types of contrasts
# 5. Check the assumptions for the "good" model
### **5.1 Normality (residuals)**
In statistics, normality tests are used to determine if a data set is well-modeled by a normal distribution and to compute how likely it is for a random variable underlying the data set to be normally distributed.

The residuals of the model should fit a normal distribution. Should look something like this: 

METER UNA FOTO DE LA DISTRIBUCION NORMLA

How we can explore the normality in our residuals?
- Visually:
  - **Histogram**

  <p align="center">
  <img src="https://github.com/AnaAGG/Statistical-Analysis-with-R/blob/main/Images/Histogram.png" />
  </p>

  - **Q-Q plot (normal probability plot)**
  <p align="center">
  <img src="https://github.com/AnaAGG/Statistical-Analysis-with-R/blob/main/Images/QQPlot.png" />
  </p>
- Analytically / Statistically:
  
  - **Test the Shapiro-Wilk**
  
    The `null hypothesis` of this test is that the population is normally distributed. Thus: 
    - If p-value  < 0.05 ==> Data **NOT NORMALLY** distributed
    - If p-value  > 0.05 ==> Data **NORMALLY** distributed
  
    More info about Shapiro test [here](https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test)
  
  - **Kurtosis (K)**
  
    Kurtosis tell us the height and sharpness of the central peak, relative to that of a standard bell curve.  The `null hypothesis` of this test is that the population is normally distributed.

    - If K > 0 (leptokurtosis) ==> greater type II error, accept null hypothesis when is false
    - If K < 0 (platikurtosis) ==> greated type I error, 
      <p align="center">
      <img src="https://github.com/AnaAGG/Statistical-Analysis-with-R/blob/main/Images/kurtosis.png" />
      </p>
    

    
  - **Skew**
### **5.2 Homocedasticity (residuals)**
- The variance of the residuals should be similar across model predictions.
  
- A random scatter pattern of points should appear, without drawing any geometric pattern (e.g., like many randomly distributed balls on a billard table) 
 <p align="center">
      <img src="https://github.com/AnaAGG/Statistical-Analysis-with-R/blob/main/Images/homocedasticity.png" />
      </p>
⚠️ SITUATION WE SHOULD NOT HAVE ⚠️ violation of the homoscedasticity assumption as there is a triangular pattern indicating that tehre is heterogeneity in the variance of the residuals across the model predictions. There is greater variance at higer preficted values. 

### **5.3. Influent and lost points**
### **5.4. Variance homogeneity**

### **5.5. Independence between predictor variables-factors - collinearity**

The predictor variables were for a long time called `independent`. This was an explicit recognition that they shoud be uncorrelated with each other. 

If there is dependence between the predictor variables, we have **COLLINEARITY**

The collinearity could be the result of:
- By definition the predictor variables are correlated ( e.g. altitude and temperature)
  
- Because there is not homogeneity in the sample size along the different factors levels. 

The problems that we can find when the variables are correlated:
- Variables cancel each other out
- significance estimates are altered 
- Effect sizes are altered 
- No convergence between type I and type II sum of squares (SS) results. 

Exploring collinearity between predictor variables: 


    AFTER ALL THESE EXPLORATION OF THE CANONICAL ASSUMPTIONS OF THE MODELS...

    ⚠️ WE CAN NOW PROCEED TO ASSESS THEIR RESULTS ⚠️

