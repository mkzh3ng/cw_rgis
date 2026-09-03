library(tidyverse)

##ctrl + shift + r = label
# point figure ------------------------------------------------------------

iris %>% 
  ggplot(
    aes(x = Sepal.Length,
        y = Sepal.Width)
  ) +
  geom_point()

iris %>% 
  ggplot(
    aes(x = Sepal.Length,
        y = Sepal.Width,
        color = Species)
  ) + 
  geom_point()

iris %>% 
  ggplot(
    aes(x = Sepal.Length,
        y = Sepal.Width,)
  ) + 
  geom_point(color = "darkgreen")


# Line Figure -------------------------------------------------------------

df_x <- tibble(x=1:50,
               y=2*x)

df_x %>% 
  ggplot(
    aes(x = x,
        y = y)
  ) + 
  geom_line()


# histogram ---------------------------------------------------------------

iris %>% 
  ggplot(
    aes(x = Sepal.Length,
        fill = Species)
  ) +
  geom_histogram()


# box plot ----------------------------------------------------------------

iris %>% 
  ggplot(
    aes(x = Species,
        y = Sepal.Length)
  ) +
  geom_boxplot()

## change color border
iris %>% 
  ggplot(
    aes(x = Species,
        y = Sepal.Length,
        color = Species)
  ) +
  geom_boxplot()

## change color inside box
iris %>% 
  ggplot(
    aes(x = Species,
        y = Sepal.Length,
        fill = Species,
        color = Species)
  ) +
  geom_boxplot()


# Exercise ----------------------------------------------------------------

#Q1 Using iris data, identify the longest Sepal.Length using arrange() function 
arrange(iris, desc(Sepal.Length))

#longest is 7.9 

#Q2 Using iris data, filter individuals with Sepal.Width greater than 3
iris %>% 
  filter(Sepal.Width>3)

#Q3 Using iris data, select the column "Petal.Length" and "Petal.Width", and  
#arrange the order of rows by "Petal.Length", assign the result as "df_petal)
df_petal <- iris %>% 
  select(Petal.Length, Petal.Width) %>% 
  arrange(desc(Petal.Width))

df_petal

#Q4 calculate mean Sepal.Width by species; assign the result to df_mean
df_mean <- iris %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Sepal.Width))

df_mean

#Q5 Create a point figure of Petal.Width (y-axis) and Sepal.Width (x axis)
#with colors distinguishing species
iris %>% 
  ggplot(
    aes(x = Sepal.Width,
        y = Petal.Length,
        color= Species)
  ) +
  geom_point()

