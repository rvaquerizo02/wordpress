data(iris)
summary(iris)

library(dplyr)
two.columns <- iris %>%
  select(Sepal.Length,Sepal.Width)

columns = c("Sepal.Length","Sepal.Width")
two.columns <- iris %>%
  select(columns)


setosa <- iris %>%
  filter(Species=="setosa")

species_to_select = c("setosa","virginica")
species <- iris %>%
  filter(Species %in% species_to_select)
table(species$Species)


iris2 <- iris %>%
  mutate(Sepal.Length.6 = ifelse(Sepal.Length >=6, "GE 6", "LT 6")) %>%
  mutate(Sepal.Length.rela = Sepal.Length/mean(Sepal.Length))


iris %>% group_by(Species) %>% 
  summarize(mean.Sepal.Length = mean(Sepal.Length),
            sd.Sepal.Length = sd(Sepal.Length),
            rows = n())

order1 <- iris %>%
  arrange(Sepal.Length)

order2 <- iris %>%
  arrange(desc(Sepal.Length))

iris %>% group_by(Species) %>% 
  summarize(mean.Sepal.Length = mean(Sepal.Length),
            sd.Sepal.Length = sd(Sepal.Length),
            rows = n()) %>%
  arrange(mean.Sepal.Length)

iris2 <- iris %>%
  mutate(id = row_number())

iris3 <- iris2 %>%
  filter(Species=="setosa") %>% 
  mutate(Sepal.Length.6 = ifelse(Sepal.Length >=6, "GE 6", "LT 6")) %>%
  mutate(Sepal.Length.rela = Sepal.Length/mean(Sepal.Length)) %>%
  select(id,Sepal.Length.6,Sepal.Length.rela)

iris4 <- iris2 %>% inner_join(iris3, by=c("id"))

iris5 <- iris2 %>% left_join(iris3, by=c("id"))

iris6 <- iris2 %>% anti_join(iris3)


