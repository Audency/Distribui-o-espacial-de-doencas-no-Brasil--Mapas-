library(tidyverse)
library(lubridate)
library(data.table)
library(openxlsx)
library(udunits2)
library(sf)
library(janitor)
library(sjmisc)  
library(splitstackshape)
library(tidyverse)
library(lubridate)
library(data.table)
library(openxlsx)
library(hablar)
library(extrafont)
library(hrbrthemes)
library(gghighlight)
library(udunits2)
library(sf)
library(tmap) #remotes::install_github('r-tmap/tmap')
library(ggsn)
library(ggflags)
library(ggrepel)
library(gridExtra)
library (kableExtra)
library(vtable)
library(gtsummary)
library(magrittr)
library(formattable)
library(gt)
library(dplyr)
library(hrbrthemes)
library(ggplot2)
library(openxlsx)
library(geobr)





load("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/CGGRIPE_EUCILENE/2023/bancoeSUS/confirmados2.Rdata")



#Filtro dos dados a partir da inclusão da variavel no banco
#em 01 de março de 2021

memory.limit (99999999999)

inclusaocomunidades <- confirmados2 %>%
  filter(dataInicioSintomas >= "2021-03-02") 

 #CONTINUA TODOS OS CRITÉRIOS 
dados_esus_antesNT14 <- inclusaocomunidades %>% 
   filter (dataInicioSintomas <= "2022-10-31")


memory.limit (99999999999)

# SOMENTE A CLASSIFICAÇÃO 1 E 2
dados_esus_depoisNT14 <- inclusaocomunidades %>% 
  filter (dataInicioSintomas >"2022-10-31") %>% 
  filter(classificacaoFinal %in% c("1","2"))



#JUNTAR BANCOS
confirmadosfinal <- bind_rows(dados_esus_antesNT14,dados_esus_depoisNT14 )


#verificar <-dados_esus_depoisNT14 %>% 
  #select(dataInicioSintomas,classificacaoFinal)





##count(dados_esus_depoisNT14,classificacaoFinal)

#count(dados_esus_antesNT14,classificacaoFinal)

#count(confirmadosfinal,classificacaoFinal)



#Filtra somente os quilombolas

dados_esus_quilombolas<-confirmadosfinal %>% 
  filter(comunidadeTradicional=="29")



save(dados_esus_quilombolas,file= "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/CGGRIPE_EUCILENE/2023/bancoeSUS/dados_esus_quilombolas.Rdata")



#População IBGE Quilombola
# https://agenciadenoticias.ibge.gov.br/agencia-noticias/2012-agencia-de-noticias/noticias/37464-brasil-tem-1-3-milhao-de-quilombolas-em-1-696-municipios

estados <- data.frame(estadoN = c("Rondônia", "Acre", "Amazonas","Roraima","Pará","Amapá", "Tocantins", "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas", "Sergipe","Bahia","Minas Gerais","Espírito Santo",
          "Rio de Janeiro","São Paulo","Paraná", "Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso",  "Goiás","Distrito Federal"))

pop <- data.frame(populacao = c(2926,0,2705,0,135033,12524,12881,269074,31686,23955,22384,16584,78827,37722,28124,397059,135310,15652,20344,10999,7113,4447,17496,2546,11719,30387,305))


pop_quilombola <- data.frame(estados, pop)




#https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Quilombolas_Primeiros_resultados_do_universo/Tabelas_selecionadas/


##ANÁLISE

glimpse(comunidadepositivo)

unique(dados_esus_quilombolas$estadoIBGE)

dados_esus_quilombolas <- dados_esus_quilombolas %>%
  mutate(estadoN = recode(estadoIBGE,
                          "11" = "Rondônia",    
                          "12" = "Acre", 
                          "13" = "Amazonas",
                          "14" = "Roraima",
                          "15" = "Pará",
                          "16" = "Amapá", 
                          "17" = "Tocantins",
                          "21" = "Maranhão",
                          "22" = "Piauí",
                          "23" = "Ceará",
                          "24" = "Rio Grande do Norte",
                          "25" = "Paraíba",
                          "26" = "Pernambuco",
                          "27" = "Alagoas", 
                          "28" = "Sergipe",
                          "29" = "Bahia", 
                          "31" = "Minas Gerais",
                          "32" = "Espírito Santo",
                          "33" = "Rio de Janeiro",
                          "35" = "São Paulo",
                          "41" = "Paraná",
                          "42" = "Santa Catarina",
                          "43" = "Rio Grande do Sul",
                          "50" = "Mato Grosso do Sul",
                          "51" = "Mato Grosso", 
                          "52" = "Goiás",
                          "53" = "Distrito Federal")
  )    





dados_esus_quilombolas <- dados_esus_quilombolas %>%
  mutate(regiao = recode(estadoN,
                           "Rondônia" = "Norte",    
                           "Acre" = "Norte",  
                          "Amazonas" = "Norte",  
                          "Roraima" = "Norte",  
                           "Pará"= "Norte",  
                          "Amapá" = "Norte",  
                           "Tocantins" = "Norte",  
                       "Maranhão"= "Nordeste", 
                        "Piauí"= "Nordeste", 
                        "Ceará"= "Nordeste", 
                          "Rio Grande do Norte"= "Nordeste", 
                      "Paraíba" = "Nordeste", 
                         "Pernambuco"= "Nordeste", 
                        "Alagoas" = "Nordeste", 
                         "Sergipe" = "Nordeste", 
                      "Bahia" = "Nordeste",  
                         "Minas Gerais" = "Sudeste", 
                          "Espírito Santo" = "Sudeste", 
                          "Rio de Janeiro"= "Sudeste", 
                       "São Paulo" = "Sudeste", 
                         "Paraná" = "Sul", 
                 "Santa Catarina"= "Sul", 
                          "Rio Grande do Sul" = "Sul", 
                         "Mato Grosso do Sul" = "Centro-Oeste ", 
                           "Mato Grosso"= "Centro-Oeste ",  
                        "Goiás"= "Centro-Oeste ", 
                         "Distrito Federal" = "Centro-Oeste " )
  )    

dados_esus_quilombolas <- dados_esus_quilombolas %>%
  mutate(sexoN = recode(sexo,
                         "1"=" Masculino",
                         "2" = "Feminino"))



#Criar faixa etária

dados_esus_quilombolas <- dados_esus_quilombolas %>% 
  mutate (faixet = case_when(idade >= 0 & idade <=4 ~ "0 a 04",
                             idade >= 5 & idade <=9 ~ "05 a 09",
                             idade >= 10 & idade <= 14 ~ "10 a 14",
                             idade >= 15 & idade <= 19 ~ "15 a 19",
                             idade >= 20 & idade <= 29 ~ "20 a 29",
                             idade >= 30 & idade <= 39 ~ "30 a 39",
                             idade >= 40 & idade <= 49 ~ "40 a 49",
                             idade >= 50 & idade <= 59 ~ "50 a 59",
                             idade >= 60 & idade <= 69 ~ "60 a 69",
                             idade >= 70 & idade <= 79 ~ "70 a 79",
                             idade >= 80 ~ "80 +"))





#Calcular incidência


estadocovd19 <- dados_esus_quilombolas %>%
  select(estadoN)  %>%
  count(estadoN)


juntar_incidencia <- full_join(estadocovd19,pop_quilombola, by= "estadoN")



incidencia <- juntar_incidencia   %>%
  mutate(incidencia = n/populacao*10000)



#TABELA GERAL comunidades


library(gtsummary)
library(flextable)

TABELA1 <-comunidadepositivo <- dados_esus_quilombolas %>%
  select(regiao,estadoN) %>%
  tbl_summary(
    type = all_categorical() ~ "categorical") %>% 
  modify_header(update =list(label  ~  "**Variáveis**"))%>% 
  bold_labels() #níveis variáveis em negrito
comunidadepositivo




save_as_docx(TABELA1, path = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/Documentos/teste/Tabela1.docx")

save_as_pptx(TABELA1, path = "C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/2023/QUILOMBOLA/tabelas/Tabela_estado_sexo.pptx")





#CRIAR ANO, MES, SEMEPI
dados_esus_quilombolas = dados_esus_quilombolas %>% 
  mutate(anoEpi = epiyear(dataInicioSintomas)) %>%
  mutate(semEpi = epiweek(dataInicioSintomas)) %>% 
  mutate(mes = month(dataInicioSintomas)) %>% 
  mutate(ano = year(dataInicioSintomas))




somaano<-dados_esus_quilombolas %>% 
  group_by(ano) %>% 
  summarise(n=n())

somaano_SE<-dados_esus_quilombolas %>% 
  group_by(ano,semEpi) %>% 
  summarise(n=n())

#(dados_esus_quilombolas_teste, file = "C:/Users/luiz.arroyo/OneDrive - Minist?rio da Sa?de/14. An?lises COE-COVID/OUTRAS DEMANDAS/TESTES_QUILOMBOLAS/Testes_quilombolas.xlsx")

########



comunidadefx_sexo <- dados_esus_quilombolas %>%
  select(sexoN,faixet) %>%
  tbl_summary(
    sort = list(faixet ~"frequency",
                sexoN ~"frequency"),#ordenar pela frequência
                type = all_categorical() ~ "categorical",      
    by = sexoN,
    percent = "row") %>%
  modify_header(update =list(label  ~  "**Variáveis**"))%>% 
  bold_labels() #níveis variáveis em negrito
comunidadefx_sexo





comunidadefx <- dados_esus_quilombolas %>%
  select(faixet) %>%
  tbl_summary(
    sort = list(faixet ~"frequency"), 
    type = all_categorical() ~ "categorical") %>% 
  modify_header(update =list(label  ~  "**Variáveis**"))%>% 
  bold_labels() #níveis variáveis em negrito
comunidadefx




count(dados_esus_quilombolas,evolucaoCaso)

obito <- dados_esus_quilombolas %>%
  filter(evolucaoCaso== 3)


somaobito_sem<-obito  %>% 
  group_by(ano,semEpi) %>% 
  summarise(n=n())

somaobito_UF<-obito  %>% 
  group_by(ano,estadoN) %>% 
  summarise(n=n())

#Graficos



grafico <- dados_esus_quilombolas %>%
  select(mes,anoEpi,dataInicioSintomas,semEpi)%>%
  group_by(anoEpi,mes,semEpi)%>%
  count(dataInicioSintomas)


ggplot(data = dados_esus_quilombolas) +  
  geom_histogram(
    mapping = aes(
      x = dataInicioSintomas,
      group = sexoN, 
     fill= sexoN))         # provide the pre-defined vector of breaks                    





ggplot(data = grafico)+
  geom_histogram(
    mapping = aes(x = semEpi, y = n),
    stat = "identity",
    width = 1,                    # count cases from start of breakpoint
    #color = "darkblue",
    #color = "black",
    fill ="lightblue")+ 
  # for daily counts, set width = 1 to avoid white space between bars
  labs(
    x = "Semana Epidemiológica -início dos sintomas", 
    y = "Número de casos",
    title = "Distribuição de casos de covid-19 por Semana Epidemiológica") +
  facet_wrap(~anoEpi)+
  theme_bw()


#vacinas


local_teste<-dados_esus_quilombolas %>% 
  group_by(anoEpi,localRealizacaoTestagem) %>% 
  summarise(n=n())


vacina <- dados_esus_quilombolas %>% 
  filter(recebeuVacina== "sim")%>% 
  group_by(ano,recebeuVacina) %>% 
  summarise(n=n())

library(tidyr)
vacina_fx <- dados_esus_quilombolas %>% 
  filter(recebeuVacina== "sim") %>% 
  tidyr::drop_na(faixet) %>% 
  group_by(anoEpi,faixet,recebeuVacina) %>% 
  summarise(n=n())
  
ggplot(vacina_fx, aes(x=factor(faixet),fill=factor(anoEpi))) + 
  geom_bar(position="fill") + 
  #geom_text(data=recebeuVacina, aes(y=n,label=ratio), position=position_fill(vjust=0.5))+
  xlab("Faixa etária") +
  ylab("Proporção de vacinados") + 
  scale_fill_manual(name="Ano", values = c("darkblue", "blue","lightblue"))


ggplot(vacina_fx ,aes(x=faixet,y=n,fill=anoEpi))+
  geom_col(color="black")+
  scale_y_continuous(expand = c(0, 1),
                     breaks = pretty(vacina_fx$n),
                     labels = abs(pretty(vacina_fx$n)))+                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
  theme_bw()+xlab("")+ylab("recebeu vacina(n)")





ggplot(vacina_fx)+ 
  geom_col(aes(x=faixet,y=n), fill="#076fa2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_bw()+xlab("")+ylab("Recebeu(n)")+   
  facet_wrap(~anoEpi)+
  coord_flip()





obito_fx <- dados_esus_quilombolas %>% 
  filter(evolucaoCaso== 3)%>% 
  group_by(anoEpi,faixet,evolucaoCaso) %>% 
  summarise(n=n())



  
  ggplot(obito_fx ,aes(x=faixet,y=n,fill ="red"))+
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_bw()+xlab("")+ylab("Óbito(n)") +
  facet_wrap(~anoEpi)+
  coord_flip()
  


ggplot(data = somaobito_sem, aes(x = semEpi, y=n, fill="darkred")) +
  geom_col(position = "identity") +
    theme_bw(base_size = 14) +
  labs(x = "Semana Epidemiológica", 
       y = "Frequência absoluta", fill = "ano") +
  facet_wrap(~ano)

#------------------------------ mapa POR  uf de residência

#Usar tabela 1



mapaUF <- read.xlsx("C:/Users/eucilene.santana/OneDrive - Ministério da Saúde/14. Análises COE-COVID/OUTRAS DEMANDAS/1_2023/QUILOMBOLA/BOLETIM QUILOMBOLA/mapa/tabela1.xlsx")




estados <- read_state(code_state = "all", year = 2020)



mapaQuilomb <- full_join(estados, mapaQuilomb, by="code_state")

tm_shape(mapaQuilomb) +
  tm_borders()

#ver paletas https://loading.io/color/feature/
tm_shape(mapaQuilomb) +
  tm_fill(col = 'Taxa', 
          title = 'Taxa de Incidência',
          breaks = c(-Inf,1, 30,50,100,Inf),
          n = 6,  
          palette = "-RdYlGn",
          legend.hist = TRUE) +
  tm_borders(col = 'grey', lwd = .5) +
  tm_legend(legend.outside = TRUE, legend.outside.position="right") +
  tm_shape(mapaQuilomb) +
  tm_text("abbrev_state.x", scale=0.45)+
  #tm_borders(col = 'grey', lwd = 0.3) +
  #tm_layout(frame = FALSE)
  #tm_layout(inner.margins = c(.03,.03,.03,.03),
            #main.title = "Taxa de incidência  covid-19 em comunidades Quilombolas",
            #main.title.position = 'center')+
  tm_scale_bar(text.size = 1, width = 0.2, position = c("right", "bottom")) + # Inserindo escala no mapa
  tm_compass(type ="8star", size = 4, position = c("right", "top"))  # Inserindo rosa dos ventos no mapa



#-------------------------- Segunda opção de mapa

final <- tm_shape(mapaQuilomb)+
  tm_fill("Taxa", palette = "YlGnBu", alpha=.5,
          title="Taxa de incidência", breaks = c(-Inf,1, 30,50,100,Inf))+
  #tm_format_Europe2()+
  #tm_style_classic()+
  tm_legend(position=c("left","bottom"))+
  tm_compass()+
  tm_scale_bar()+
  #tm_text("abbrev_state.x", scale=0.7, root=3,ymod=-.070)+
  tm_borders(alpha=.5)+
  tm_bubbles(size = 'Pop',col = '#1d4e82', title.size='População',  breaks = c(-Inf,50, 100,150,300,Inf)) +
  tm_legend(legend.format = list(text.separator= "a"))

final

#COPIAR e cola no PPT para ajustar a figura

#"C:\Users\eucilene.santana\OneDrive - Ministério da Saúde\14. Análises COE-COVID\OUTRAS DEMANDAS\1_2023\QUILOMBOLA\BOLETIM QUILOMBOLA\mapa\MAPA FINALIZADO.pptx"




