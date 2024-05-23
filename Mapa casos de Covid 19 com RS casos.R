################################### Construcao do Mapa de casos de Covid estratificados por idade ##################################################

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
load ("C:/Users/audencio.victor/OneDrive - Ministério da Saúde/e-SUS_Plinio/Dados_05_05_24/Dados_completo_05_05_24-Suspeitos_e_final_com_reinfeccao.Rdata")
#load ("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/e-SUS_Plinio/Dados_28_04_24/Dados_completo_28_04_24-Suspeitos_e_final_com_reinfeccao.Rdata")

#Arquivo separadoonde estao os meus dados 
setwd("C:/Users/audencio.victor/OneDrive - Ministério da Saúde/Audencio/Mapas")
#setwd("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/Audencio/Mapas")

######################################################## BANCO GERAL ########################################################

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
  select(estadoIBGE, siglaEstado, nomeEstado, dataInicioSintomas, classificacaoFinal, idade) %>%
  filter( dataInicioSintomas > "2023-12-31")%>%
  filter(classificacaoFinal == 1 | classificacaoFinal == 2) %>%
  mutate(faixet = case_when(idade <= 2 ~ "<= 2 anos",
    idade > 2 & idade <= 4 ~ "3 a 4 anos",
    idade > 4 & idade <= 11 ~ "5 a 11 anos",
    idade > 11 & idade <= 17 ~ "12 a 17 anos",
    idade > 17 & idade <= 39 ~ "18 a 39 anos",
    idade > 39 ~ ">= 40 anos",TRUE ~ NA_character_ )) %>%
  group_by(estadoIBGE, nomeEstado) %>%
  count(faixet) %>%
  pivot_wider(names_from = faixet, values_from = n)
  
###removendo a coluna NA no banco de dados "dados"  
dados <- dados %>% select(-`NA`)


## Exportar o banco de dados 
write.csv(dados, file = "dados.csv", row.names = FALSE) 


## Ler o banco recem exportado 
dados <- read.csv("dados.csv", encoding = "UTF-8")

dados <-  dados %>%
  mutate(siglaEstado = case_when(estadoIBGE == 11 ~ "RO", estadoIBGE == 12 ~ "AC", estadoIBGE == 13 ~ "AM", estadoIBGE == 14 ~ "RR",
                                  estadoIBGE == 15 ~ "PA", estadoIBGE == 16 ~ "AP", estadoIBGE == 17 ~ "TO", estadoIBGE == 21 ~ "MA",
                                  estadoIBGE == 22 ~ "PI", estadoIBGE == 23 ~ "CE", estadoIBGE == 24 ~ "RN", estadoIBGE == 25 ~ "PB",
                                  estadoIBGE == 26 ~ "PE", estadoIBGE == 27 ~ "AL", estadoIBGE == 28 ~ "SE", estadoIBGE == 29 ~ "BA",
                                  estadoIBGE == 31 ~ "MG", estadoIBGE == 32 ~ "ES", estadoIBGE == 33 ~ "RJ", estadoIBGE == 35 ~ "SP",
                                  estadoIBGE == 41 ~ "PR", estadoIBGE == 42 ~ "SC", estadoIBGE == 43 ~ "RS", estadoIBGE == 50 ~ "MS",
                                  estadoIBGE == 51 ~ "MT", estadoIBGE == 52 ~ "GO", estadoIBGE == 53 ~ "DF" ))

### renomear as variaveis  
# Renomear colunas para torná-las mais legíveis
dados <- rename(dados, `12 a 17 anos` = X12.a.17.anos, `18 a 39 anos` = X18.a.39.anos,
                `3 a 4 anos` = X3.a.4.anos, `5 a 11 anos`= X5.a.11.anos,
                `<= 2 anos` = X...2.anos, `>= 40 anos` = X...40.anos)

##Passo 2: Unir os Dados de Casos com os Dados de População
dadospop <- read_excel("dadospop2.xlt")

# Criar o novo banco de dados com as faixas etárias 
populacao <- dadospop %>%
  mutate(`<= 2anos` = `Menos que 1 ano de idade` + `1  ano` + `2  anos`,
         `3 a 4anos` = `3  anos` + `4  anos`,
         `5 a 11anos` = `5  anos` + `6  anos` + `7  anos` + `8  anos` + `9  anos` + `10 anos` + `11 anos`,
         `12 a 17anos` = `12 anos` + `13 anos` + `14 anos` + `15 anos` + `16 anos` + `17 anos`,
         `18 a 39anos` = `18 anos` + `19 anos` + `20 anos` + `21 anos` + `22 anos` + `23 anos` + `24 anos` + 
          `25 anos` + `26 anos` + `27 anos` + `28 anos` + `29 anos` + `30 anos` + `31 anos` + `32 anos` + `33 anos` + 
           `34 anos` + `35 anos` + `36 anos` + `37 anos` + `38 anos` + `39 anos`,
         `>= 40anos` = rowSums(select(., `40 anos`:`80 anos e mais`))) %>%
        mutate (nomeEstado = `Unidade da Federação`) %>%
       select(estadoIBGE,nomeEstado, `<= 2anos`, `3 a 4anos`, `5 a 11anos`, `12 a 17anos`, `18 a 39anos`, `>= 40anos`)


### Merge dos bancos dos casos e da populacao para o calculo da incidencia 
dados_unidos <- merge(dados, populacao, by=c("nomeEstado"))%>%
  rename(estadoIBGE = estadoIBGE.x) %>%
  select(-estadoIBGE.y)

names(dados_unidos)
## Calculo da incidencia por faixa etaria 
incidencia <- dados_unidos %>%
   mutate( `<= 2 anos` = (`<= 2 anos`/ `<= 2anos`) * 100000, 
          `3 a 4 anos`= (`3 a 4 anos`/`3 a 4anos`)* 100000,
          `5 a 11 anos` = (`5 a 11 anos`/`5 a 11anos`)* 100000, 
          `12 a 17 anos` = (`12 a 17 anos`/`12 a 17anos`)* 100000,
          `18 a 39 anos` = (`18 a 39 anos`/`18 a 39anos`)* 100000,
          `>= 40 anos` = (`>= 40 anos`/`>= 40anos`)* 100000) %>%
select(estadoIBGE,nomeEstado, `<= 2anos`, `3 a 4anos`, `5 a 11anos`, `12 a 17anos`, `18 a 39anos`, `>= 40anos`, 
       `<= 2 anos`, `3 a 4 anos`, `5 a 11 anos`, `12 a 17 anos`, `18 a 39 anos`, `>= 40 anos`) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) 


# Substituindo NA por 0 na variável de incidência
incidencia[is.na(incidencia)] <- 0

## Passo 3: Construir os Mapas com geobr
estados_br <- read_state(code_state = "all", year = 2020) %>%
  mutate (estadoIBGE = code_state) %>%
  select(-"code_state")

####### Inserindo os dados com as referencia geoespacia e os casos 
dados_mapa <- estados_br %>%full_join(incidencia, by = c("estadoIBGE"))


####O Mapa no pacote TM
tm_shape(dados_mapa) +
  tm_borders()


  # Ver paletas legais
library("RColorBrewer")
display.brewer.all()


####### Mapa faixa etaria em <= de 2 anos ######################################
um <- tm_shape(dados_mapa) +
  tm_fill(col = '<= 2 anos',  # Substitua 'alguma_variavel' pela variável de incidência
          title = 'Taxa de Incidência em <= 2 anos', # Título da legenda de preenchimento
          #breaks = c(-Inf,1, 30,50,100,Inf),
          #labels = c("0-2", "2-6", "6-10","10-14", ">14"), # Rótulos personalizados para os intervalos
          n = 5,  
          lwd = 0.8,
          palette = "Blues", # Define uma paleta de cores
          legend.hist = TRUE) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_shape(dados_mapa) +
  tm_text("abbrev_state", size=0.5, scale=0.3)+
  tm_borders(col = 'grey', lwd = 0.3) +
  tm_layout(frame = FALSE)
  tm_layout(inner.margins = c(.03,.03,.03,.03),
  #main.title = "Distribuição espacial da taxa de incidência de covid-19 por faixa etária por UF no Brasil em 2024",
  main.title.position = 'center', title.size = 0.3) +
  tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Inserindo escala no mapa
  tm_compass(type ="8star", size = 3, position = c("right", "top"))  # Inserindo rosa dos ventos no mapa

  # Salvar o mapa como PNG
  tmap_save(um , "um.png")
  
##################### Mapa para   faixa etaria 3 a 4anos ######################################
  dois <- tm_shape(dados_mapa) +
  tm_fill(col = '3 a 4 anos',  # Substitua 'alguma_variavel' pela variável de incidência
          title = 'Taxa de Incidência 3 a 4 anos', # Título da legenda de preenchimento
          #breaks = c(-Inf,1, 30,50,100,Inf),
          n = 5,  
          palette = "Blues", # Define uma paleta de cores
          legend.hist = TRUE) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_shape(dados_mapa) +
  tm_text("abbrev_state",size=0.5,  scale=0.7)+
  tm_borders(col = 'grey', lwd = 0.3) +
  tm_layout(frame = FALSE)
  tm_layout(inner.margins = c(.03,.03,.03,.03),
            #main.title = "Distribuição espacial da taxa de incidência de covid-19 por faixa etária por UF no Brasil em 2024",
            main.title.position = 'center', title.size = 0.3)+
  tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Inserindo escala no mapa
  tm_compass(type ="8star", size = 3, position = c("right", "top"))  # Inserindo rosa dos ventos no mapa
  
  # Salvar o mapa como PNG
  tmap_save(dois , "dois.png")
##################### Mapa para   faixa etaria 5 a 11 anos ######################################
tres <- tm_shape(dados_mapa) +
  tm_fill(col = '5 a 11 anos',  # Substitua 'alguma_variavel' pela variável de incidência
          title = 'Taxa de Incidência 5 a 11 anos', # Título da legenda de preenchimento
          #breaks = c(-Inf,1, 30,50,100,Inf),
          n = 5,  
          palette = "Blues", # Define uma paleta de cores
          legend.hist = TRUE) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_shape(dados_mapa) +
  tm_text("abbrev_state", size=0.5,scale=0.7)+
  tm_borders(col = 'grey', lwd = 0.3) +
  tm_layout(frame = FALSE)
  tm_layout(inner.margins = c(.03,.03,.03,.03),
            #main.title = "Distribuição espacial da taxa de incidência de covid-19 por faixa etária por UF no Brasil em 2024",
            main.title.position = 'center', title.size = 0.3)+
  tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Inserindo escala no mapa
  tm_compass(type ="8star", size = 3, position = c("right", "top"))  # Inserindo rosa dos ventos no mapa
  
  # Salvar o mapa como PNG
  tmap_save(tres, "tres.png" )

##################### Mapa para faixa etaria 12 a 17 anos ######################################
quatro <- tm_shape(dados_mapa) +
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

  # Salvar o mapa como PNG
  tmap_save(quatro , "quatro.png")
##################### Mapa para   faixa etaria 18 a 39 anos ######################################
  cinco <- tm_shape(dados_mapa) +
  tm_fill(col = '18 a 39 anos',  # Substitua 'alguma_variavel' pela variável de incidência
          title = 'Taxa de Incidência 18 a 39 anos', # Título da legenda de preenchimento
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

  # Salvar o mapa como PNG
  tmap_save(cinco , "cinco.png")
##################### Mapa para   faixa etaria  >= 40 anos ######################################
  seis <- tm_shape(dados_mapa) +
  tm_fill(col = '>= 40 anos',  # Substitua 'alguma_variavel' pela variável de incidência
          title = 'Taxa de Incidência >= 40 anos', # Título da legenda de preenchimento
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
  
  # Salvar o mapa como PNG
  tmap_save(seis , "seis.png")

######################################################  FIM  ############################################################
#########################################################################################################################
  

########################################################################################################################
#################################################### MAPA DO RIO GRANDE DO SUL #########################################
########################################################################################################################
  
##Leitura do banco  de dados de obitos 
  
dados<- read_excel(file_path, sheet = "casos 2023")
dados<- read_excel(file_path, sheet = "casos 2024")
dados <- read_excel(file_path, sheet = "obitos 2023")
dados <- read_excel(file_path, sheet = "obitos 2024")
  
###### Construir os Mapas com geobr apenas com municipios de RS
muni_br <- read_municipality (code_muni = "all",  year=2020) %>%
mutate (muniIBGE = code_muni) %>%
filter(abbrev_state == "RS")

muni_br$muniIBGE <- as.character(muni_br$muniIBGE)
muni_br$muniIBGE <- substr(muni_br$muniIBGE, 1, nchar(muni_br$muniIBGE) - 1)

###### Inserindo os dados com as referencia geoespacia e os casos 
  dados_mapa <- muni_br %>% full_join(dados, by = c("muniIBGE"))
  dados_mapa$Total[is.na(dados_mapa$Total)] <- 0
  
  colnames(muni_br)
  #### Dados de estado 
  dados_estados <- read_state(code_state = "all", year = 2020) %>%
    mutate (estadoIBGE = code_state) %>%
    select(-"code_state")%>%
    filter(abbrev_state == "RS")
  
  
  ####O Mapa no pacote Municipal 
  tm_shape( dados_rs) +
    tm_borders()
  
  ####O Mapa no pacote Estaduais 
  tm_shape(dados_estados) +
    tm_borders()
  
  
  # Ver paletas legais
  library("RColorBrewer")
  display.brewer.all()
  
  
  
  ##################### Mapas PF ######################################
  um <- tm_shape(dados_mapa) +
    tm_dots(col = 'Total',  # Substitua 'Total' pela sua variável de incidência
            title = 'Notificação-Paralisia Flácida', # Título da legenda
            breaks = c(-Inf,1,Inf),
            n=2,
            labels = c("Não", "Sim"), # Rótulos personalizados para os intervalos
            palette = "Blues", # Paleta de cores
            legend.hist = FALSE) + # Histograma da legenda desativado
    tm_legend(legend.outside = TRUE, legend.outside.position="right") +
    tm_borders(col = 'grey', lwd = 0.3) + # Fronteiras municipais
    tm_shape(dados_estados) +
    tm_borders(col = 'black', lwd = 0.5) + # Fronteiras estaduais
    tm_layout(frame = FALSE) +
    tm_layout(inner.margins = c(.03,.03,.03,.03),
              main.title = "Distribuição espacial de casos de paralisia flácida por município de residência no Brasil, 2023",
              main.title.position = 'center',
              title.size = 0.001) + # Tamanho do título principal
    tm_scale_bar(text.size = 0.5, width = 0.2, position = c("right", "bottom")) + # Barra de escala
    tm_bubbles(size = 'Total', col = '#1d4e82', border.col = 'black', 
               title.size = 'casos', 
               breaks = c(-Inf, 1, 10, 20, Inf), 
               labels = c("<1", "1-10", "10-20", "20-30", ">30")) + # Corrigido para refletir corretamente os dados
    tm_compass(type ="8star", size = 3, position = c("right", "top")) # Rosa dos ventos
  
  
  # Salvar o mapa como PNG
  tmap_save(um , "um.png")
  
  
  tm_shape(dados_mapa) +
    tm_fill(col = 'Total',  # Replace 'Total' with your incidence variable
            title = 'Notificação-Paralisia Flácida', # Legend title
            breaks = c(-Inf,1,Inf),
            labels = c("Não", "Sim"), # Add custom labels for breaks here
            palette = "Blues", # Define color palette
            legend.hist = FALSE) + # Optional: to show a histogram in the legend
    tm_legend(legend.outside = TRUE, legend.outside.position="right") +
    tm_borders(col = 'grey', lwd = 0.3) + # Adjust color and line width for state borders here
    tm_layout(frame = FALSE) +
    tm_layout(inner.margins = c(.03,.03,.03,.03),
              main.title = "Distribuição espacial de casos de paralisia flácida por município de residência no Brasil, 2023",
              main.title.position = 'center', title.size = 0.1) +
    tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Scale bar
    tm_compass(type ="8star", size = 3, position = c("right", "top")) # Compass
  
  
  ################################ Mapa de Incluindo maap do mundo ##########################################
  
  library(tmap) # Carregar a biblioteca tmap
  library(sf)
  
  # Criando o mapa
  mapa <- tm_shape(dados_mapa) +
    tm_fill(col = "Total",  # Substitua 'Total' pela sua variável de incidência se necessário
            title = "Notificação-Paralisia Flácida",
            breaks = c(-Inf, 1, Inf),  # Definindo intervalos
            labels = c("Não", "Sim"),  # Rótulos para os intervalos
            palette = "Blues",  # Paleta de cores
            legend.hist = FALSE) +  # Desabilitar histograma na legenda
    tm_legend(legend.outside = TRUE, legend.outside.position="right") +
    tm_borders(col = "grey", lwd = 0.3) +  # Fronteiras municipais
    tm_shape(dados_estados) +
    tm_borders(col = "black", lwd = 0.5) +  # Fronteiras estaduais
    tm_layout(frame = FALSE) +
    tm_layout(inner.margins = c(0.03, 0.03, 0.03, 0.03),
              main.title = "Distribuição espacial de casos de paralisia flácida por município de residência no Brasil, 2023",
              main.title.position = "center",
              title.size = 0.001) +  # Tamanho do título principal
    tm_scale_bar(text.size = 0.5, width = 0.2, position = c("right", "bottom")) +  # Barra de escala
    tm_compass(type = "8star", size = 3, position = c("right", "top"))  # Rosa dos ventos
  
  # Visualizar o mapa
  tmap_mode("view")
  print(mapa)
  
  
  
  
  
  
  
  
  
  