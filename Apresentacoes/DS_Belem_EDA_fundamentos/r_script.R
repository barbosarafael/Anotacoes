#--- 1. Importando os pacotes

require(tidyverse)


#--- 2. Lendo o arquivo


dados <- read_csv(file = "https://raw.githubusercontent.com/barbosarafael/Anotacoes/master/Apresentacoes/DS_Belem_EDA_fundamentos/penguins.csv")


#--- 3. Estrutura dos dados (Variáveis)


dados %>%
  head


dados %>%
  glimpse


#--- 4. Tópicos especiais

# 4.1. Arredondamento

numero_arred = 17.3452

round(x = numero_arred, digits = 2)



# 4.2. Missing data

colSums(is.na(dados))


# 4.3. Recodificação


# Nova variável:
# < 4500: Leve
# >= 4500: Pesada


dados %>%
  mutate(categoria_pinguim = ifelse(body_mass_g < 4500, yes = "Leve",
                                    no = "Pesada")) %>%
  count(categoria_pinguim)


#--- 5. Gráficos

# 5.1. Barras

dados %>%
  count(sex) %>%
  ggplot(data = .) +
  geom_bar(aes(x = sex, y = n), stat = "identity", colour = "black",
           fill = "orangered") +
  theme_minimal(14) +
  labs(title = "Percentual de gênero no banco de dados")


# 5.2. Barras

dados %>%
  ggplot(data = .) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, colour = sex)) +
  theme_minimal(14) +
  labs(title = "bill_length_mm x bill_depth_mm")


# 5.3. Histograma

g1 <- dados %>%
  ggplot(data = .) +
  geom_histogram(aes(x = body_mass_g), bins = 12, color = "black",
                 fill = "orangered") +
  theme_minimal(14) +
  labs(title = "Distribuição de frequência do peso em g")


# 5.4. Boxplot


g2 <-
  dados %>%
  ggplot(data = .) +
  geom_boxplot(aes(x = sex, y = body_mass_g), fill = "orangered") +
  theme_minimal(14) +
  labs(title = "Distribuição do peso (em g) pelo sexo")



ggpubr::ggarrange(dados %>%
                    ggplot(data = .) +
                    geom_boxplot(aes(x = sex, y = body_mass_g), fill = "orangered") +
                    theme_minimal(14) +
                    labs(title = "Distribuição do peso (em g) pelo sexo"),
                  dados %>%
                    ggplot(data = .) +
                    geom_histogram(aes(x = body_mass_g), bins = 12, color = "black",
                                   fill = "orangered") +
                    theme_minimal(14) +
                    labs(title = "Distribuição de frequência do peso em g")
                  )



#--- 6. Medidas resumo

# 6.1. Tendência central

# Média

mean(dados$body_mass_g, na.rm = T)


# Média

mean(dados$body_mass_g, na.rm = T)

# Moda

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(dados$body_mass_g)


# 6.2. Medidas de dispersão

var(dados$body_mass_g, na.rm = T)

sd(dados$body_mass_g, na.rm = T)

sd(dados$body_mass_g, na.rm = T) * sd(dados$body_mass_g, na.rm = T)


# 6.3. Medidas separatrizes

# Quartis

quantile(x = dados$body_mass_g, probs = c(0.25, 0.5, 0.75), na.rm = T)
