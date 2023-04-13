# Instala os pacotes necessários
install.packages(c("zoo", "dplyr", "ggplot2"))

# Carrega os pacotes necessários
library(dplyr)
library(zoo)
library(ggplot2)

# Lê o arquivo csv
df <- read.csv("C:/Users/patri/Downloads/us_state_vaccinations.csv", header = TRUE, sep = ",")

# Exibe as primeiras 6 linhas do dataframe
head(df)

# Preenche valores faltantes com a observação anterior
df <- na.locf(df, na.rm = FALSE)

# Preenche valores faltantes com 0
df[is.na(df)] <- 0

# Exibe as primeiras 6 linhas do dataframe
head(df)

# Agrupa os dados por data e calcula a soma das pessoas totalmente vacinadas
df_sum <- df %>%
  group_by(date) %>%
  summarize(total_fully_vaccinated = sum(people_fully_vaccinated, na.rm = TRUE))

# Cria um gráfico de linha mostrando a evolução da quantidade de pessoas totalmente vacinadas
ggplot(df_sum, aes(x = date, y = total_fully_vaccinated, group = 1)) +
  geom_line() +
  labs(title = "Progresso da vacinação nos EUA",
       x = "Data",
       y = "Pessoas totalmente vacinadas") +
  theme_minimal()

# Agrupa os dados por data e calcula a soma das pessoas vacinadas com pelo menos uma dose
df_sum2 <- df %>%
  group_by(date) %>%
  summarize(total_vaccinated = sum(people_vaccinated, na.rm = TRUE))

# Cria um gráfico de linha mostrando a evolução da quantidade de pessoas vacinadas com pelo menos uma dose
ggplot(df_sum2, aes(x = date, y = total_vaccinated,group = 1)) +
  geom_line() +
  labs(title = "Progresso da vacinação nos EUA",
       x = "Data",
       y = "Pessoas vacinadas com pelo menos uma dose") +
  theme_minimal()

# Seleciona a última observação de cada estado e a coluna de pessoas totalmente vacinadas por 100 pessoas
df_last <- df %>%
  group_by(location) %>%
  slice_tail(n = 1) %>%
  select(location, people_fully_vaccinated_per_hundred)

# Cria um gráfico de barras mostrando os 10 estados com maior porcentagem de pessoas totalmente vacinadas
ggplot(head(arrange(df_last, desc(people_fully_vaccinated_per_hundred)), 10),
       aes(x = people_fully_vaccinated_per_hundred, y = reorder(location, people_fully_vaccinated_per_hundred))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Estados com maior porcentagem de pessoas totalmente vacinadas",
       x = "Pessoas totalmente vacinadas por 100 pessoas",
       y = "Estado") +
  theme_minimal()

# Seleciona a última observação de cada estado e a coluna de pessoas vacinadas com pelo menos uma dose por 100 pessoas
df_last2 <- df %>%
  group_by(location) %>%
  slice_tail(n = 1) %>%
  select(location, people_vaccinated_per_hundred)

#Cria um gráfico de barras mostrando os 10 estados com maior porcentagem de pessoas vacinadas com pelo menos uma dose
ggplot(head(arrange(df_last2, desc(people_vaccinated_per_hundred)), 10),
       aes(x = people_vaccinated_per_hundred, y = reorder(location, people_vaccinated_per_hundred))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Estados com maior porcentagem de pessoas totalmente vacinadas",
       x = "Pessoas vacinadas com pelo menos uma dose por 100 pessoas",
       y = "Estado") +
  theme_minimal()
 
