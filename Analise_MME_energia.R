# ------------ Carregando Pacotes
library(basedosdados)
library(ggplot2)
library(tidyr)
library(dplyr)
library(bigrquery)
library(DBI)
library(scales)
library(plotly)
options(scipen=999)
#------ Coletando os dados da base dos dados
con = dbConnect(bigquery(),
                billing = 'analise-1746',
                project = 'basedosdados')

query = 'SELECT * FROM `basedosdados.br_mme_consumo_energia_eletrica.uf`'
df = dbGetQuery(con, query)

# Crriando base de consumo por ano e criando grafico ---------
consumo_por_ano = df %>% filter(tipo_consumo == "Total") %>%  select(ano, consumo) %>% group_by(ano) %>% summarise(total_ano = sum(consumo))

# criando grafico
ggplot(data = consumo_por_ano, aes(ano, total_ano))+
  geom_line(col = '#02907d')+
  labs(x = 'Ano', y = 'Total Consumido en MWh',
       title = 'Total de Energia Consumida no Brasil entre 2004 e 2021 em MWh',
       subtitle = 'Fonte - Ministério de Minas e Energia',
       caption = 'Produzido por Christian Basilio')+
  geom_point()+
  geom_text(aes(label = round(total_ano/1000000,0)), vjust = 2, hjust = 1)+
  scale_y_continuous(labels  = 
                       label_number(scale = 0.000001, suffix = "M", accuracy = 1))+
  theme_classic()


# Analise por Estado do sudeste por ano -------
estado_ano = df %>%filter(tipo_consumo == "Total", sigla_uf %in% c('RJ', 'SP', 'ES', 'MG')) %>% 
  select(ano, sigla_uf,consumo) %>%
  group_by(ano, sigla_uf) %>% 
  summarize(total_ano = sum(consumo))

# Grafico 
ggplot(data = estado_ano, aes(ano, total_ano, col = sigla_uf))+
  geom_line()+
  theme_minimal()


# Observações sobre os dados
df$tipo_consumo %>% unique()


# Criando Grafico de Consumo total nos Ultimos ano ------
tipos_consumo = df %>%filter(tipo_consumo != 'Total') %>% 
  select(tipo_consumo, consumo) %>% 
  group_by(tipo_consumo) %>% 
  summarise(total_consumo = sum(consumo))

# Plotando grafico de pizza interativo

plot2 = plot_ly(tipos_consumo, labels = ~tipo_consumo, values = ~total_consumo, type = 'pie')

plot2 = plot2 %>% layout(title = 'Tipos de Consumo de Energia em Mwh entre 2004 e 2021 no Brasil')

plot2




