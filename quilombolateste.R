
################################### Construcao do Mapa de casos de Covid estratificados por idade ##################################################

###Instalando os pecotes 
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, psych, descr,data.table,lubridate,e1071, dplyr, tidyverse,openxlsx, geobr, raster, ggspatial, rio,flextable,  
               fields, sf,gt, janitor, readxl, MASS, knitr, ggrepel,udunits2, rstatix,extrafont,here,officer,splitstackshap,sjmisc, 
               hablar,ggsn, surveillance, gridExtra, grid, ggpubr, httr,gtsummary, rvest, readxl, gganimate,hrbrthemes,hablar,
               gghighlight,ggflags,kableExtra,vtable, magrittr,formattable, hrbrthemes, tmap )


###Instalando do github o pacote 
devtools::install_github("mtennekes/tmap", force = TRUE)


#####  Continue carregando o banco de dados em Rdada
load ("C:/Users/audencio.victor/OneDrive - Ministério da Saúde/e-SUS_Plinio/Dados_14_04_24/Dados_completo_14_04_24-Suspeitos_e_final_com_reinfeccao.Rdata")

#Arquivo separadoonde estao os meus dados 
setwd("C:/Users/audencio.victor/OneDrive - Ministério da Saúde/Audencio/Mapas")


####################################################### BANCO GERAL ########################################################

## Construir o banco  banco com casos de Covid por faixa etaria
## Passo 1: Organização dos Dados de Casos de COVID-19

## Adicionar (Renomear) as regioes e os estados no dataset

dados <- df_final %>% mutate( regiao = case_when( estadoIBGE %in% c(11:17) ~ "Norte",
                                                  estadoIBGE %in% c(21:29) ~ "Nordeste",
                                                  estadoIBGE %in% c(31, 32, 33, 35) ~ "Sudeste",
                                                  estadoIBGE %in% c(41:43) ~ "Sul",
                                                  estadoIBGE %in% c(50:53) ~ "Centro-Oeste"),
                              siglaEstado = case_when( estadoIBGE == 11 ~ "RO", estadoIBGE == 12 ~ "AC", estadoIBGE == 13 ~ "AM", estadoIBGE == 14 ~ "RR",
                                                       estadoIBGE == 15 ~ "PA", estadoIBGE == 16 ~ "AP", estadoIBGE == 17 ~ "TO", estadoIBGE == 21 ~ "MA",
                                                       estadoIBGE == 22 ~ "PI", estadoIBGE == 23 ~ "CE", estadoIBGE == 24 ~ "RN", estadoIBGE == 25 ~ "PB",
                                                       estadoIBGE == 26 ~ "PE", estadoIBGE == 27 ~ "AL", estadoIBGE == 28 ~ "SE", estadoIBGE == 29 ~ "BA",
                                                       estadoIBGE == 31 ~ "MG", estadoIBGE == 32 ~ "ES", estadoIBGE == 33 ~ "RJ", estadoIBGE == 35 ~ "SP",
                                                       estadoIBGE == 41 ~ "PR", estadoIBGE == 42 ~ "SC", estadoIBGE == 43 ~ "RS", estadoIBGE == 50 ~ "MS",
                                                       estadoIBGE == 51 ~ "MT", estadoIBGE == 52 ~ "GO", estadoIBGE == 53 ~ "DF" ),
                              nomeEstado = case_when( estadoIBGE == 11 ~ "Rondônia", estadoIBGE == 12 ~ "Acre", estadoIBGE == 13 ~ "Amazonas", estadoIBGE == 14 ~ "Roraima",
                                                      estadoIBGE == 15 ~ "Pará", estadoIBGE == 16 ~ "Amapá", estadoIBGE == 17 ~ "Tocantins", estadoIBGE == 21 ~ "Maranhão",
                                                      estadoIBGE == 22 ~ "Piauí", estadoIBGE == 23 ~ "Ceará", estadoIBGE == 24 ~ "Rio Grande do Norte", estadoIBGE == 25 ~ "Paraíba",
                                                      estadoIBGE == 26 ~ "Pernambuco", estadoIBGE == 27 ~ "Alagoas", estadoIBGE == 28 ~ "Sergipe", estadoIBGE == 29 ~ "Bahia",
                                                      estadoIBGE == 31 ~ "Minas Gerais", estadoIBGE == 32 ~ "Espírito Santo", estadoIBGE == 33 ~ "Rio de Janeiro", 
                                                      estadoIBGE == 35 ~ "São Paulo", estadoIBGE == 41 ~ "Paraná", estadoIBGE == 42 ~ "Santa Catarina", 
                                                      estadoIBGE == 43 ~ "Rio Grande do Sul", estadoIBGE == 50 ~ "Mato Grosso do Sul", estadoIBGE == 51 ~ "Mato Grosso", 
                                                      estadoIBGE == 52 ~ "Goiás", estadoIBGE == 53 ~ "Distrito Federal")) %>%
  select(estadoIBGE, siglaEstado, nomeEstado, dataInicioSintomas, comunidadeTradicional, classificacaoFinal) %>%
  
  dados1 <- dados  %>% filter( dataInicioSintomas > "2023-01-01")%>%
  filter(classificacaoFinal == 1 | classificacaoFinal == 2) %>%
  filter(comunidadeTradicional ==29)
 



## Exportar o banco de dados 
write.csv(dados, file = "dados.csv", row.names = FALSE) 



