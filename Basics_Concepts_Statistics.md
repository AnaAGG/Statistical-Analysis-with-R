## Table of contents 
- [**1. Types of response variable distributions**](#1-types-of-response-variable-distributions)
    - [**1.1 Gaussian or Normal distribution**](#11-gaussian-or-normal-distribution)
    - [**1.2 Poisson distribution**](#12-poisson-distribution)
    - [**1.3 Continous distribution**](#13-continous-distribution)
    - [**1.4 Negative Binomial distribution**](#14-negative-binomial-distribution)
- [2. Types of predictors](#2-types-of-predictors)
- [3. Basic n-way ANOVA models](#3-basic-n-way-anova-models)
- [4. Tables and types of contrasts](#4-tables-and-types-of-contrasts)
- [5. Check the assumptions for the "good" model](#5-check-the-assumptions-for-the-good-model)
    - [**5.1 Normality (residuals)**](#51-normality-residuals)
    - [**5.2 Heterocedasticity (residuals)**](#52-heterocedasticity-residuals)
    - [**5.3. Influent and lost points**](#53-influent-and-lost-points)
    - [**5.4. Variance homogeneity**](#54-variance-homogeneity)
    - [**5.5. Independence between predictor variables-factors - collinearity**](#55-independence-between-predictor-variables-factors---collinearity)
- [6. Other concepts to incorporate (In progress)](#6-other-concepts-to-incorporate-in-progress)
  - [dispersion](#dispersion)
  - [When use AIC or AICc)](#when-use-aic-or-aicc)
  - [post-hoc](#post-hoc)


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

### **1.3 Continous distribution**
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

<img src="https://render.githubusercontent.com/render/math?math=e^{i \pi} = -1">


When size is very large `1/size` tend to 0 and the variance ends up looking by $\mu$
  
  

# 2. Types of predictors 
# 3. Basic n-way ANOVA models
# 4. Tables and types of contrasts
# 5. Check the assumptions for the "good" model
### **5.1 Normality (residuals)**
In statistics, normality tests are used to determine if a data set is well-modeled by a normal distribution and to compute how likely it is for a random variable underlying the data set to be normally distributed.
### **5.2 Heterocedasticity (residuals)**
### **5.3. Influent and lost points**
### **5.4. Variance homogeneity**
### **5.5. Independence between predictor variables-factors - collinearity**



# 6. Other concepts to incorporate (In progress)
## dispersion
## When use AIC or AICc)
Acording Burnham & Anderson (2002) recommended not to use AIC without the bias correction unless 

N/K < 40

K = total number of parameters of the likelihood
N = sample size


## post-hoc

Post hoc analysis consists of statistical analyses that were specified after the data were seen. This typically creates a multiple testing problem because each potential analysis is effectively a statistical test. Multiple testing procedures are sometimes used to compensate, but that is often difficult or impossible to do precisely. Post hoc analysis that is conducted and interpreted without adequate consideration of this problem is sometimes called data dredging by critics because the statistical associations that it finds are often spurious.


