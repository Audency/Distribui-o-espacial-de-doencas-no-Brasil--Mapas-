
################################## Construcao do Mapa de casos de Covid estratificados por idade ##################################################
###Instalando os pecotes 
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, psych, descr,data.table,lubridate,e1071, dplyr, tidyverse,openxlsx, geobr, raster, ggspatial, rio,flextable,  
               fields, sf,gt, janitor, readxl, MASS, knitr, ggrepel,udunits2, rstatix,extrafont,here,officer,splitstackshap,sjmisc, 
               hablar,ggsn, surveillance, gridExtra, grid, ggpubr, httr,gtsummary, rvest, readxl, gganimate,hrbrthemes,hablar,
               gghighlight,ggflags,kableExtra,vtable, magrittr,formattable, hrbrthemes )

pacman::p_load("ggplot2", "psych", "descr", "data.table", "lubridate", "dplyr", "tidyverse", "geobr", "openxlsx",
               "readxl", "MASS", "knitr", "vtable","ids","splitstackshape", "ggrepel", "gtsummary", "naniar", 
               "readr", "vroom", "janitor", "sjmisc", "summarytools", "abjutils", "mice","kableExtra","DataExplorer")



###Instalando do github o pacote 
remotes::install_github('r-tmap/tmap')

install.packages("devtools")
devtools::install_github("mtennekes/tmap")
devtools::install_github("mtennekes/tmap", force = TRUE)


library('tmap')
#####  Continue carregando o banco de dados em Rdada
load ("C:/Users/audencio.victor/OneDrive - Ministério da Saúde/e-SUS_Plinio/Dados_12_05_24/bancos/Dados_completo_12_05_24-Suspeitos_e_final_com_reinfeccao.Rdata")

#Arquivo separadoonde estao os meus dados 
setwd("C:/Users/audencio.victor/OneDrive - Ministério da Saúde/Audencio/Mapas")
#setwd("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/Audencio/Mapas")

######################################################## BANCO GERAL ########################################################
dados_rs <- df_final %>%
  mutate(
    regiao = case_when(
      estadoIBGE %in% c(11:17) ~ "Norte",
      estadoIBGE %in% c(21:29) ~ "Nordeste",
      estadoIBGE %in% c(31, 32, 33, 35) ~ "Sudeste",
      estadoIBGE %in% c(41:43) ~ "Sul",
      estadoIBGE %in% c(50:53) ~ "Centro-Oeste" ),
    siglaEstado = case_when(estadoIBGE == 11 ~ "RO", estadoIBGE == 12 ~ "AC", estadoIBGE == 13 ~ "AM", estadoIBGE == 14 ~ "RR",
      estadoIBGE == 15 ~ "PA", estadoIBGE == 16 ~ "AP", estadoIBGE == 17 ~ "TO", estadoIBGE == 21 ~ "MA",
      estadoIBGE == 22 ~ "PI", estadoIBGE == 23 ~ "CE", estadoIBGE == 24 ~ "RN", estadoIBGE == 25 ~ "PB",
      estadoIBGE == 26 ~ "PE", estadoIBGE == 27 ~ "AL", estadoIBGE == 28 ~ "SE", estadoIBGE == 29 ~ "BA",
      estadoIBGE == 31 ~ "MG", estadoIBGE == 32 ~ "ES", estadoIBGE == 33 ~ "RJ", estadoIBGE == 35 ~ "SP",
      estadoIBGE == 41 ~ "PR", estadoIBGE == 42 ~ "SC", estadoIBGE == 43 ~ "RS", estadoIBGE == 50 ~ "MS",
      estadoIBGE == 51 ~ "MT", estadoIBGE == 52 ~ "GO", estadoIBGE == 53 ~ "DF"
    ),
    nomeEstado = case_when(
      estadoIBGE == 11 ~ "Rondônia", estadoIBGE == 12 ~ "Acre", estadoIBGE == 13 ~ "Amazonas", estadoIBGE == 14 ~ "Roraima",
      estadoIBGE == 15 ~ "Pará", estadoIBGE == 16 ~ "Amapá", estadoIBGE == 17 ~ "Tocantins", estadoIBGE == 21 ~ "Maranhão",
      estadoIBGE == 22 ~ "Piauí", estadoIBGE == 23 ~ "Ceará", estadoIBGE == 24 ~ "Rio Grande do Norte", estadoIBGE == 25 ~ "Paraíba",
      estadoIBGE == 26 ~ "Pernambuco", estadoIBGE == 27 ~ "Alagoas", estadoIBGE == 28 ~ "Sergipe", estadoIBGE == 29 ~ "Bahia",
      estadoIBGE == 31 ~ "Minas Gerais", estadoIBGE == 32 ~ "Espírito Santo", estadoIBGE == 33 ~ "Rio de Janeiro", 
      estadoIBGE == 35 ~ "São Paulo", estadoIBGE == 41 ~ "Paraná", estadoIBGE == 42 ~ "Santa Catarina", 
      estadoIBGE == 43 ~ "Rio Grande do Sul", estadoIBGE == 50 ~ "Mato Grosso do Sul", estadoIBGE == 51 ~ "Mato Grosso", 
      estadoIBGE == 52 ~ "Goiás", estadoIBGE == 53 ~ "Distrito Federal" ) ) %>%
  select(estadoIBGE, siglaEstado, nomeEstado, dataInicioSintomas, classificacaoFinal, municipioIBGE) %>%
  filter( estadoIBGE == 43,
    dataInicioSintomas >= "2024-04-14" & dataInicioSintomas <= "2024-05-11",
    classificacaoFinal == 1 | classificacaoFinal == 2 ) %>%
  group_by(municipioIBGE) %>%
  summarise(total_casos = n())

dados_rs$municipioIBGE <- substr(dados_rs$municipioIBGE, 1, nchar(dados_rs$municipioIBGE) - 1)

#####Abrindo  o banco de dados  da populacao  por municipios de rs 
populacao <- read_excel("dados.xlsx")

### Merge dos bancos dos casos e da populacao para o calculo da incidencia 
dados_unidos <- merge(dados_rs, populacao, by="municipioIBGE") %>%
  rename(estadoIBGE = estadoIBGE.x) %>%
  select(-estadoIBGE.y) %>%
  replace_na(list(across(everything(), 0)))


## Calculo da incidencia por faixa etaria 
dados <- dados_unidos %>%
  mutate( tx_inc = (total_casos /pop) * 100000)
          
########################################################################################################################
############################################## MAPA DO RIO GRANDE DO SUL -2024 #########################################
########################################################################################################################
  
###### Construir os Mapas com geobr apenas com municipios de RS
muni_br <- read_municipality (code_muni = "all",  year=2020) %>%
mutate (muniIBGE = code_muni) %>%
filter(abbrev_state == "RS")

muni_br$muniIBGE <- as.character(muni_br$muniIBGE)
muni_br$muniIBGE <- substr(muni_br$muniIBGE, 1, nchar(muni_br$muniIBGE) - 1)

###### Inserindo os dados com as referencia geoespacia e os casos 
dados$muniIBGE <- as.character(dados$muniIBGE)
dados_mapa <- muni_br %>% full_join(dados, by = c("muniIBGE"))

dados_mapa$tx_inc[is.na(dados_mapa$tx_inc)] <- 0
  

#### Dados de estado 
dados_estados <- read_state(code_state = "all", year = 2020) %>%
mutate (estadoIBGE = code_state) %>%
select(-"code_state")%>%
filter(abbrev_state == "RS")
  
  
####O Mapa no pacote Municipal 
tm_shape( dados_rs) + tm_borders()
  
  ####O Mapa no pacote Estaduais 
tm_shape(dados_estados) + tm_borders()
  
# Ver paletas legais
  library("RColorBrewer")
  display.brewer.all()
  
  
  
################################################ Mapas PF #####################################
############################### Mapa de Incluindo maap do mundo ################################
  
  tm_shape(dados_mapa) +
    tm_fill(col = "tx_inc",  # Substitua 'Total' pela sua variável de incidência se necessário
            title = "Taxa de Incidência de casos de Covid-19 nas ultimas 4 semanas no RS, BR, 2024",
            #breaks = c(-Inf, 1, Inf),  # Definindo intervalos
            n = 3,  
            palette = "Blues", # Define uma paleta de cores
            legend.hist = TRUE) +
    tm_legend(legend.outside = TRUE, legend.outside.position="right") +
    tm_shape(dados_mapa) +
    tm_text("", size=0.5, scale=0.7)+
    tm_borders(col = 'black', lwd = 0.3) +
    tm_layout(frame = FALSE)
  tm_layout(inner.margins = c(.03,.03,.03,.03),
            #main.title = "Distribuição espacial da taxa de incidência de covid-19 por faixa etária por UF no Brasil em 2024",
            main.title.position = 'center', title.size = 0.3)+
    tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Inserindo escala no mapa
    tm_compass(type ="8star", size = 3, position = c("right", "top"))  # Inserindo rosa dos ventos no mapa
  

  
  

  
  tm_shape(dados_mapa) +
    tm_fill(col = '12 a 17 anos',  # Substitua 'alguma_variavel' pela variável de incidência
            title = 'Taxa de Incidência 12 a 17 anos', # Título da legenda de preenchimento
            #breaks = c(-Inf,1, 30,50,100,Inf),
            n = 5,  
            palette = "Blues", # Define uma paleta de cores
            legend.hist = TRUE) +
    tm_legend(legend.outside = TRUE, legend.outside.position="right") +
    tm_shape(dados_mapa) +
    tm_text("abbrev_state", size=0.5, scale=0.7)+
    tm_borders(col = 'grey', lwd = 0.3) +
    tm_layout(frame = FALSE)
  tm_layout(inner.margins = c(.03,.03,.03,.03),
            #main.title = "Distribuição espacial da taxa de incidência de covid-19 por faixa etária por UF no Brasil em 2024",
            main.title.position = 'center', title.size = 0.3)+
    tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Inserindo escala no mapa
    tm_compass(type ="8star", size = 3, position = c("right", "top"))  # Inserindo rosa dos ventos no mapa
  
  #############################################################################################################
  ################################################### Dados de 2023 ##########################################
  #############################################################################################################
  #############################################################################################################
  dados_rs <- df_final %>%
    mutate(
      regiao = case_when(
        estadoIBGE %in% c(11:17) ~ "Norte",
        estadoIBGE %in% c(21:29) ~ "Nordeste",
        estadoIBGE %in% c(31, 32, 33, 35) ~ "Sudeste",
        estadoIBGE %in% c(41:43) ~ "Sul",
        estadoIBGE %in% c(50:53) ~ "Centro-Oeste" ),
      siglaEstado = case_when(estadoIBGE == 11 ~ "RO", estadoIBGE == 12 ~ "AC", estadoIBGE == 13 ~ "AM", estadoIBGE == 14 ~ "RR",
                              estadoIBGE == 15 ~ "PA", estadoIBGE == 16 ~ "AP", estadoIBGE == 17 ~ "TO", estadoIBGE == 21 ~ "MA",
                              estadoIBGE == 22 ~ "PI", estadoIBGE == 23 ~ "CE", estadoIBGE == 24 ~ "RN", estadoIBGE == 25 ~ "PB",
                              estadoIBGE == 26 ~ "PE", estadoIBGE == 27 ~ "AL", estadoIBGE == 28 ~ "SE", estadoIBGE == 29 ~ "BA",
                              estadoIBGE == 31 ~ "MG", estadoIBGE == 32 ~ "ES", estadoIBGE == 33 ~ "RJ", estadoIBGE == 35 ~ "SP",
                              estadoIBGE == 41 ~ "PR", estadoIBGE == 42 ~ "SC", estadoIBGE == 43 ~ "RS", estadoIBGE == 50 ~ "MS",
                              estadoIBGE == 51 ~ "MT", estadoIBGE == 52 ~ "GO", estadoIBGE == 53 ~ "DF"
      ),
      nomeEstado = case_when(
        estadoIBGE == 11 ~ "Rondônia", estadoIBGE == 12 ~ "Acre", estadoIBGE == 13 ~ "Amazonas", estadoIBGE == 14 ~ "Roraima",
        estadoIBGE == 15 ~ "Pará", estadoIBGE == 16 ~ "Amapá", estadoIBGE == 17 ~ "Tocantins", estadoIBGE == 21 ~ "Maranhão",
        estadoIBGE == 22 ~ "Piauí", estadoIBGE == 23 ~ "Ceará", estadoIBGE == 24 ~ "Rio Grande do Norte", estadoIBGE == 25 ~ "Paraíba",
        estadoIBGE == 26 ~ "Pernambuco", estadoIBGE == 27 ~ "Alagoas", estadoIBGE == 28 ~ "Sergipe", estadoIBGE == 29 ~ "Bahia",
        estadoIBGE == 31 ~ "Minas Gerais", estadoIBGE == 32 ~ "Espírito Santo", estadoIBGE == 33 ~ "Rio de Janeiro", 
        estadoIBGE == 35 ~ "São Paulo", estadoIBGE == 41 ~ "Paraná", estadoIBGE == 42 ~ "Santa Catarina", 
        estadoIBGE == 43 ~ "Rio Grande do Sul", estadoIBGE == 50 ~ "Mato Grosso do Sul", estadoIBGE == 51 ~ "Mato Grosso", 
        estadoIBGE == 52 ~ "Goiás", estadoIBGE == 53 ~ "Distrito Federal" ) ) %>%
    select(estadoIBGE, siglaEstado, nomeEstado, dataInicioSintomas, classificacaoFinal, municipioIBGE) %>%
    filter( estadoIBGE == 43,
            dataInicioSintomas >= "2023-04-16" & dataInicioSintomas <= "2023-05-13",
            classificacaoFinal == 1 | classificacaoFinal == 2 ) %>%
    group_by(municipioIBGE) %>%
    summarise(total_casos = n())
  
  dados_rs$municipioIBGE <- substr(dados_rs$municipioIBGE, 1, nchar(dados_rs$municipioIBGE) - 1)
  
  #####Abrindo  o banco de dados  da populacao  por municipios de rs 
  populacao <- read_excel("dados.xlsx")
  
  ### Merge dos bancos dos casos e da populacao para o calculo da incidencia 
  dados_unidos <- merge(dados_rs, populacao, by="municipioIBGE") %>%
    rename(estadoIBGE = estadoIBGE.x) %>%
    select(-estadoIBGE.y) %>%
    replace_na(list(across(everything(), 0)))
  
  
  ## Calculo da incidencia por faixa etaria 
  dados <- dados_unidos %>%
    mutate( tx_inc = (total_casos /pop) * 100000)
  
  ########################################################################################################################
  ############################################## MAPA DO RIO GRANDE DO SUL -2024 #########################################
  ########################################################################################################################
  
  ###### Construir os Mapas com geobr apenas com municipios de RS
  muni_br <- read_municipality (code_muni = "all",  year=2020) %>%
    mutate (muniIBGE = code_muni) %>%
    filter(abbrev_state == "RS")
  
  muni_br$muniIBGE <- as.character(muni_br$muniIBGE)
  muni_br$muniIBGE <- substr(muni_br$muniIBGE, 1, nchar(muni_br$muniIBGE) - 1)
  
  ###### Inserindo os dados com as referencia geoespacia e os casos 
  dados$muniIBGE <- as.character(dados$muniIBGE)
  dados_mapa <- muni_br %>% full_join(dados, by = c("muniIBGE"))
  
  dados_mapa$tx_inc[is.na(dados_mapa$tx_inc)] <- 0
  
  
  #### Dados de estado 
  dados_estados <- read_state(code_state = "all", year = 2020) %>%
    mutate (estadoIBGE = code_state) %>%
    select(-"code_state")%>%
    filter(abbrev_state == "RS")
  
  
  ####O Mapa no pacote Municipal 
  tm_shape( dados_rs) + tm_borders()
  
  ####O Mapa no pacote Estaduais 
  tm_shape(dados_estados) + tm_borders()
  
  # Ver paletas legais
  library("RColorBrewer")
  display.brewer.all()
  
  
  
  ################################################ Mapas PF #####################################
  ############################### Mapa de Incluindo maap do mundo ################################
  
  tm_shape(dados_mapa) +
    tm_fill(col = "tx_inc",  # Substitua 'Total' pela sua variável de incidência se necessário
            title = "Taxa de Incidência de casos de Covid-19 nas ultimas 4 semanas no RS, BR, 2023",
            #breaks = c(-Inf, 1, Inf),  # Definindo intervalos
            n = 3,  
            palette = "Blues", # Define uma paleta de cores
            legend.hist = TRUE) +
    tm_legend(legend.outside = TRUE, legend.outside.position="right") +
    tm_shape(dados_mapa) +
    tm_text("", size=0.5, scale=0.7)+
    tm_borders(col = 'black', lwd = 0.3) +
    tm_layout(frame = FALSE)
  tm_layout(inner.margins = c(.03,.03,.03,.03),
            #main.title = "Distribuição espacial da taxa de incidência de covid-19 por faixa etária por UF no Brasil em 2024",
            main.title.position = 'center', title.size = 0.3)+
    tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Inserindo escala no mapa
    tm_compass(type ="8star", size = 3, position = c("right", "top"))  # Inserindo rosa dos ventos no mapa
  

