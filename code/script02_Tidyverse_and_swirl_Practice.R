library (tidyverse)

set.seed(123)

iris_sub<-as_tibble(iris) |> 
  group_by(Species) |> 
  sample_n(3) |> 
  ungroup()

print(iris_sub)

filter(iris_sub, Species == "virginica")
filter(iris_sub, Species %in% c("virginica", "versicolor"))
filter(iris_sub, Species != "setosa")

# "|" = "or"
filter(iris_sub, Petal.Length > 5 | Sepal.Length > 5)

# for either, but not both, us "," or "&"
filter(iris_sub, Petal.Length >5 &
         Sepal.Length>5)


## Arranging order
arrange(iris_sub, desc(Sepal.Width))

##Exercises
# "alt-" is shortcut for "<-"
iris_3<-filter(iris_sub, Sepal.Width > 3)
iris_3

iris_setosa<-filter(iris_sub, Species == "setosa")
iris_setosa

iris_3_setosa<-filter(iris_sub,Sepal.Width > 3,
                      Species == "setosa")
iris_3_setosa

##Column Manipulation "select()"
select(iris_sub,
       Sepal.Length,
       Sepal.Width)

select(iris_sub,
       -Sepal.Length,
       -Sepal.Width)

select(iris_sub,
       starts_with("Sepal"))

##Mutating 
x_max <- nrow(iris_sub)
x <- 1:x_max

mutate(iris_sub, row_id = x)

mutate(iris_sub, mu_sl = mean(Sepal.Length))
mutate(iris_sub, sep.area = Sepal.Length *Sepal.Width/2)

mutate(group_by(iris_sub, Species), mu_sl = mean(Sepal.Length))

##Piping 
iris_sub |> 
  filter(Species == "virginica")

iris_sub |> 
  group_by (Species) |> 
  mutate (mu_sl = mean(Sepal.Length)) |> 
  ungroup()

##Exercises 2
iris_pipe <- iris_sub |> 
  filter(Species == "setosa") |> 
  mutate(pw_two_times = Petal.Width*2)

iris_pipe

## Summarizing
iris_sub |> 
  group_by(Species) |> 
  summarize(mu_sl = mean(Sepal.Length),
            sum_sl = sum(Sepal.Length))
