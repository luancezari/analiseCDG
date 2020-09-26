##### CARREGA PACOTE #####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
library(zoo)
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
library(scales)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(kableExtra)
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
library(knitr)

##### GRAFICOS BASICOS #####
# Plota indices de liquidez
LIQUIDEZ %>% mutate(Data = as.yearqtr(Data)) %>%
  pivot_longer(c('Corrente', 'Seca', 'Imediata',), names_to = 'key') %>%
  ggplot(aes(Data, value, col = key)) +
  geom_line() +
  geom_point() +
  scale_x_yearqtr() +
  labs(title = 'Índices de liquidez',
       x = 'Trimestre',
       y = '',
       color = NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

# Plota CGL
CGPM %>%
  mutate(Data = as.yearqtr(Data),
         CGL = CGL/10^6) %>% 
  ggplot(aes(Data, CGL)) +
  geom_line() +
  geom_point() +
  labs(title = 'Capital de Giro Líquido',
       x = 'Trimestre',
       y = 'CGL em milhões de reais') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_yearqtr() +
  scale_y_continuous(labels = dollar_format(prefix = 'R$')) +
  theme_light()

# Plota ciclos
CICLOS %>%
  mutate(Data = as.yearqtr(Data)) %>% 
  pivot_longer(c('Operacional Total', 'Financeiro'), names_to = 'key', values_to ='value') %>%
  ggplot(aes(Data, value, col = key)) +
  geom_line() +
  geom_point() +
  scale_x_yearqtr() +
  labs(title = 'Ciclos',
       x = 'Trimestre',
       y = 'Ciclos em Dias',
       color = NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

# Plota prazos medios
CGPM %>%
  mutate(Data = as.yearqtr(Data)) %>%
  select(-CGL) %>% 
  pivot_longer(c('PMC', 'PMPF', 'PME'), names_to = 'key', values_to ='value') %>%
  ggplot(aes(Data, value, col = key)) +
  geom_line() +
  geom_point() +
  scale_x_yearqtr() +
  labs(title = 'Prazos Operacionais Médios',
       x = 'Trimestre',
       y = 'Prazos em Dias',
       color = NULL) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

# Plota variáveis do modelo dinâmico de Fleuriet
FLEURIET %>% mutate(Data = as.yearqtr(Data)) %>%
  pivot_longer(c('CDG', 'NCG', 'T'), names_to = 'key') %>%
  ggplot(aes(Data, value, col = key)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_x_yearqtr() +
  labs(title = 'Indicadores - Modelo de Fleuriet',
       x = 'Trimestre',
       y = '',
       color = NULL) +
  theme_light() +
  scale_y_continuous(labels = dollar_format(prefix = 'R$')) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom')

# Calcula e plota termômetro de liquidez
FLEURIET %>% mutate(Data = as.yearqtr(Data),
                    `Termômetro de Liquidez` = `T`/abs(NCG)) %>%
  ggplot(aes(Data, `Termômetro de Liquidez`)) +
  geom_line() +
  geom_point() +
  scale_x_yearqtr() +
  scale_y_log10() +
  labs(title = 'Termômetro de Liquidez (log 10)',
       x = 'Trimestre',
       y = '') +
  theme_light()