Homework Assignment \#6 for STAT 547
================

Comparing a function for linear regression vs robust linear regression of life expectancy on GDP per capita in a nested dataframe
---------------------------------------------------------------------------------------------------------------------------------

-   Overview
-   Loading required packages
-   Creating a filtered uni-country test dataset and plotting
-   Creating a regular and robust linear regression model for our filtered data
-   Comparing robust and regular linear regression
-   Expanding our model to fit multi-country data
-   Working with nested dataframe

#### Overview

To fufill prompt \#2, we are going to build a function that can take in data on multiple countries and create a linear regression model for predicting life Expectancy based on GDP per capita. Then for prompt \#6, we will apply these methods to a nested dataframe

#### Loading required packages

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(DMwR))
```

#### Creating a filtered uni-country dataset and plotting

We will choose Canada for this example, and use dplyr methods to extract this information from the gapminder dataset. Additionally, we can see the rational for looking at life expectancy and GDP per capita, they are positively correlated.

``` r
can_gap <- gapminder %>%
  filter(country == "Canada") #filtered dataset for Canada

ggplot(can_gap, aes(year, gdpPercap)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + #linear model to fit scatterplot of  year vs gdpPercap 
  labs(title = "Canadian GDP per capita vs year", x="Year", y="GDP per capita") +
  theme_bw()
```

![](hw06-zhamadeh_files/figure-markdown_github/unnamed-chunk-2-1.png)

#### Creating a robust and regular linear regression model for our filtered data

Here, we create our models as functions. Firstly, our linear regression model takes in data and has a starting year (1945 in this case). We create a variable, `fit`, to create a linear model of life expectancy and GDP per capita beginning in the starting year. Then we simply set the names of our linear model values to intercept and slope. Next, repeat this but using the robust linear model function.

``` r
lin_reg_mod <- function(df) {
  fit <- lm(gdpPercap ~ I(year - 1945), data = df)
  setNames(data.frame(t(coef(fit))), c("intercept","slope"))
}

rob_lin_mod <- function(df) {
  fit <- rlm(gdpPercap ~ I(year - 1945), data = df)
  setNames(data.frame(t(coef(fit))), c("intercept","slope"))
}

knitr::kable(lin_reg_mod(can_gap))
```

|  intercept|     slope|
|----------:|---------:|
|   6835.607|  451.4533|

``` r
knitr::kable(rob_lin_mod(can_gap))
```

|  intercept|     slope|
|----------:|---------:|
|   6831.438|  451.8441|

#### Comparing robust and regular linear regression

First off, lets use the `regr.eval()` method from the DMwR package to calculate the absolute error (mae), mean squared error (mse), root mean squared error (rmse) and other relative measures of error (mape)

``` r
regr.eval(can_gap$lifeExp, lin_reg_mod(can_gap))
```

    ##          mae          mse         rmse         mape 
    ## 5.956958e+02 3.827991e+06 1.956525e+03 8.656669e+00

``` r
regr.eval(can_gap$lifeExp, rob_lin_mod(can_gap))
```

    ##          mae          mse         rmse         mape 
    ## 5.953810e+02 3.823316e+06 1.955330e+03 8.652082e+00

This output tells us there is very little effect on all measures of error between linear regression models. But this could be due to the test data so lets try this test again with another test data set from Rwanda.

``` r
test_rwanda <- gapminder %>%
  filter(country == "Rwanda")

regr.eval(test_rwanda$lifeExp, lin_reg_mod(test_rwanda))
```

    ##          mae          mse         rmse         mape 
    ## 3.918298e+01 1.584043e+04 1.258588e+02 9.768884e-01

``` r
regr.eval(test_rwanda$lifeExp, rob_lin_mod(test_rwanda))
```

    ##          mae          mse         rmse         mape 
    ## 3.792499e+01 1.478530e+04 1.215948e+02 9.454614e-01

Here, we see the results we expected. Robust linear regression has lower measures of error because it weighs down outlier observations from skewing the model too much. Knowing Rwanda has some outliers in life expectancy seen below in the plot of life expectancy vs gdp per capita, robust linear was effective. Now lets extrapolate this model to take multi-country datasets.

``` r
ggplot(test_rwanda, aes(year, gdpPercap)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Rwandan GDP per capita vs year", x="Year", y="GDP per capita") +
  theme_bw()
```

![](hw06-zhamadeh_files/figure-markdown_github/unnamed-chunk-6-1.png)

#### Expanding our model to fit multi-country data

Here, lets use the `do()` method in combination with the `group_by()` method that will group input data by country. The `do()` method will apply our previously defined robust linear regression function to all observations within a given country and repeat this for every country. We can see from the first few observations that we know have the slope and intercept of our linear regression model for every country.

``` r
rob_lin_all <- function(input_data) {
  input_data %>%
  group_by(country) %>%
  do(lin_reg_mod(.))
}
knitr::kable(head(rob_lin_all(gapminder), n=10))
```

| country     |  intercept|        slope|
|:------------|----------:|------------:|
| Afghanistan |   817.8748|   -0.4405854|
| Albania     |  1376.5683|   54.4579214|
| Algeria     |  2208.3315|   64.2810005|
| Angola      |  4409.2758|  -23.2514578|
| Argentina   |  5960.5514|   86.8116645|
| Australia   |  5254.2054|  426.8518919|
| Austria     |  1969.1715|  534.5723120|
| Bahrain     |  8434.7619|  279.5044084|
| Bangladesh  |   455.1799|   10.5037373|
| Belgium     |  3897.4020|  463.8653945|

#### Working with nested dataframe

First we must group gapminder by country and continent and then we use the `nest()` function to create a nested dataframe that stores all remaining columns for each country in a tibble known as data. Lets also recreate our robust linear model function.

``` r
#create nested dataframe
nest_gap <- gapminder %>%
  group_by(continent, country) %>%
  nest()

#recreate robust linear regression model
rob_lin_mod <- function(df) {
  fit <- rlm(gdpPercap ~ I(year - 1945), data = df)
}
```

Then we will add a new column called fit that will apply our linear regression model to each country using the `map()` function. Then we will create a tidy column that applies the tidy function to the output of the fit column. Finally, we selected only the continent, country and tidy column and unnest the tidy column. From here, we can rename our two columns and select only from continent to estimate. Then we spread out the term column into a intercept and slope column and we have our final product: robust linear regression on gapminder data.

``` r
#create mutated nest dataframe that adds columns for linear regression analysis
gap_lm_coef <- nest_gap %>% 
  mutate(fit = map(data, rob_lin_mod),
         tidy = map(fit, tidy)) %>% 
  dplyr::select(continent, country, tidy) %>% 
  unnest(tidy) %>% #unnest tidy column
  mutate(term = recode(term,
                        `(Intercept)` = "intercept",
                        `I(year - 1945)` = "slope")) %>%
  dplyr::select(continent:estimate) %>% 
  spread(key = term, value = estimate)

knitr::kable(head(gap_lm_coef,n=20))
```

| continent | country                  |   intercept|        slope|
|:----------|:-------------------------|-----------:|------------:|
| Africa    | Algeria                  |   2192.2555|   64.0911274|
| Africa    | Angola                   |   4389.0749|  -26.4279587|
| Africa    | Benin                    |    881.8422|    7.9248772|
| Africa    | Botswana                 |  -2710.9907|  224.2959306|
| Africa    | Burkina Faso             |    515.0541|    9.5188884|
| Africa    | Burundi                  |    376.9516|    2.7452585|
| Africa    | Cameroon                 |   1203.4992|   14.0290237|
| Africa    | Central African Republic |   1329.6079|  -10.5725420|
| Africa    | Chad                     |   1252.5841|   -3.3974685|
| Africa    | Comoros                  |   1416.2368|   -4.7481796|
| Africa    | Congo, Dem. Rep.         |   1123.8413|  -13.5707870|
| Africa    | Congo, Rep.              |   2145.3885|   30.4350036|
| Africa    | Cote d'Ivoire            |   1889.9138|    0.5679539|
| Africa    | Djibouti                 |   3346.8431|  -19.5258823|
| Africa    | Egypt                    |    445.1385|   76.2156270|
| Africa    | Equatorial Guinea        |  -1974.5413|  119.7034141|
| Africa    | Eritrea                  |    279.5323|    7.0726491|
| Africa    | Ethiopia                 |    382.2571|    3.7361535|
| Africa    | Gabon                    |   4820.6217|  175.2960436|
| Africa    | Gambia                   |    585.4777|    2.6496936|
