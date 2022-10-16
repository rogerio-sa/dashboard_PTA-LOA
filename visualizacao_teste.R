library(readxl)
library(ggplot2)
library(dplyr)
library(networkD3)
COFOG_GC <- read_excel("COFOG GC.xlsx", 
                       sheet = "despesa_funcao", col_types = c("text", 
                                                               "text", "skip", "skip", "skip", "skip", 
                                                               "skip", "skip", "skip", "skip", "skip"), 
                       skip = 2)


names(COFOG_GC)<- c("codigo_cofog","descricao_cofog")

Base_COFOG_2019_TT <- read_excel("Base COFOG 2019 TT.xlsx")

names(Base_COFOG_2019_TT)[13:14]<-c("valor","codigo_cofog")

dados_cofog_pai<-
  Base_COFOG_2019_TT %>%
  mutate(codigo_cofog = stringr::str_sub(codigo_cofog,1,3)) %>%
  inner_join( COFOG_GC) %>%
  mutate(descricao_cofog_pai = "Gastos com funções de governo") %>%
  group_by(descricao_cofog_pai, descricao_cofog) %>%
  summarise(
    total_gasto = sum(valor)
  )

dados_total<-
  dados_cofog_pai %>%
  ungroup() %>%
  mutate(descricao_cofog = descricao_cofog_pai) %>%
  mutate(descricao_cofog_pai = NA) %>%
  group_by(descricao_cofog_pai, descricao_cofog) %>%
  summarise(
    total_gasto = sum(total_gasto)
  )

dados_cofog_raiz<-  
  Base_COFOG_2019_TT %>%
  mutate(codigo_pai = stringr::str_sub(codigo_cofog,1,3)) %>%
  inner_join( COFOG_GC) %>%
  mutate(codigo_filho = codigo_cofog) %>%
  mutate(descricao_cofog_filho = descricao_cofog) %>%
  mutate(codigo_cofog = codigo_pai) %>%
  select(codigo_pai,codigo_cofog, descricao_cofog_filho, valor) %>%
  inner_join( COFOG_GC) %>%
  mutate(descricao_cofog_pai = descricao_cofog ) %>%
  mutate(descricao_cofog = descricao_cofog_filho) %>%
  group_by(descricao_cofog_pai, descricao_cofog) %>%
  summarise(
    total_gasto = sum(valor)
  )
dados_cofog_completo <-
  dados_cofog_pai %>%
  bind_rows(dados_cofog_raiz,
            dados_total)


dados_cofog_completo<-
  dados_cofog_completo%>%
  ungroup() %>%
  mutate (source = row_number() -1) 

pai<-
  (dados_cofog_completo %>%
     filter(!is.na(descricao_cofog_pai)) %>%
     distinct(descricao_cofog_pai))$descricao_cofog_pai

pos_pai <-
  dados_cofog_completo %>%
  filter(descricao_cofog %in% pai) %>%
  distinct(descricao_cofog, source) %>%
  mutate(descricao_cofog_pai =descricao_cofog ) %>%
  mutate(destination = source) %>%
  select(descricao_cofog_pai, destination)

dados_net<-
  dados_cofog_completo %>%
  inner_join(pos_pai)


nodes<- dados_cofog_completo %>%
  select(descricao_cofog)

dados_tree_view<-
  dados_cofog_raiz %>%
  mutate(raiz= "Gastos com funções de governo") %>%
  mutate(total_norm = (scale(total_gasto, center = FALSE))+1)

sum(dados_tree_view$total_gasto)

#########################################################################################


dados_grupo <- read_excel("Despesa.xlsx", sheet = 4)
head(dados_grupo)

dados_grupo <- dados_grupo[, c("DESCRICAO_RECURSO", "Grupo_DESCRICAO", "Teto","raiz")] 
names(dados_grupo)<- c("descricao_pai","descricao_filho", "total_teto", "raiz")



#names(dados_tree)<- c("Ano","descricao_cofog_pai","descricao_cofog", "total_gasto", "raiz")


library(collapsibleTree)

dados_grupo = dados_grupo[,c("descricao_pai","descricao_filho", "total_teto", "raiz")] %>% 
  group_by(descricao_pai, descricao_filho) %>%
  summarise(total = sum(total_teto))

dados_grupo%>%
  mutate(`Teto Orçamentário` = total /10^6) %>%
  collapsibleTreeSummary(
    hierarchy = c("descricao_pai", "descricao_filho"),
    root = paste0('Destinação dos Recursos no PLOA 2023'),
    width = 1280,# LARGURA
    height = 960,# ALTURA
    attribute = "Teto Orçamentário",
    fontSize = 16,
    #nodeSize = "total_norm",
    zoomable = TRUE
  )


#########################################################################################


dados_LOA <- read_excel("Despesa.xlsx", sheet = 2)
head(dados_LOA)

dados_LOA <- dados_LOA[, c("DESCRICAO_RECURSO",  "Prog. Governo","Ação (PAOE)","Cat.Econ","DESCRICAO_GRUPO","Valor PTA","raiz")] 
names(dados_LOA)<- c("descricao_pai","descricao_filho", "descricao_filha","descricao_neto", "descricao_bisneto","total_teto", "raiz")


library(collapsibleTree)

dados_LOA = dados_LOA[,c("descricao_pai","descricao_filho", "descricao_filha","descricao_neto", "descricao_bisneto","total_teto", "raiz")] %>% 
  group_by(descricao_pai, descricao_filho, descricao_filha,descricao_neto, descricao_bisneto) %>%
  summarise(total = sum(total_teto))

dados_LOA%>% filter(descricao_filho == "36 - Apoio administrativo") %>%
  mutate(`LOA-2023` = total /10^6) %>%
  collapsibleTreeSummary(
    hierarchy = c("descricao_pai","descricao_filho", "descricao_filha", "descricao_bisneto"),
    root = paste0('Orçamento LOA 2023'),
    width = 1280,# LARGURA
    height = 1280,# ALTURA
    attribute = "LOA-2023",
    fontSize = 14,
    #nodeSize = "total_norm",
    zoomable = TRUE
  )

###################################################################
###################################################################

dados_loa<- read.xlsx("Despesa.xlsx", sheet=2)

dados_loa <- dados_loa[,c("regiao","DESCRICAO_RECURSO","COD_GRUPO","Valor.PTA")] 

names(dados_loa) <- c("regiao","DESCRICAO_RECURSO","COD_GRUPO","valor_pta") 

dados_loa <- dados_loa%>% filter(COD_GRUPO == 4) %>% 
  group_by(regiao, DESCRICAO_RECURSO) %>%
  summarise(total = sum(valor_pta))

quantile(dados_loa$total)


C(0, 5435054,  20349211, 100236338, 900485930)

c('Até R$5.4 milhões','De R$5.4 a R$20.3 milhões',
  'De R$20.3 milhões a R$100 milhões','De R$100 milhões a 
  R$900 milhões')



###############################################################
library(flexdashboard)
library(networkD3)
library(shiny)
library(readxl)
library(ggplot2)
library(openxlsx)# pacote para ler arquivo de excel
library(dplyr)
library(networkD3)
library(plotly)
library(viridis)
library(stringr)
library(readr)
library(Cairo)
library(ggrepel)
library(colorspace)
library(cluster)
library(tidyr)
library(stringr)
library(collapsibleTree)
library(knitr)
library(tidyverse)
library(flexdashboard)
library(leaflet)
library(lubridate)
library(DT)
library(gridExtra)
library(leafpop)
library(sf)
library(DataExplorer)
#novas libraries
library(purrr)

LOA <- read_excel("Despesa.xlsx", sheet = 2)
fonte_mapa <- unique(LOA$DESCRICAO_RECURSO)

selectInput("fonte_mapa",label = h4("Fonte de Financiamento:"),
            choices = fonte_mapa, selected = fonte_mapa[[1]] )


mt_municipio <- read_sf('MT_Municipios_2019.shp')
dados_mt<- read.xlsx("Despesa.xlsx", sheet="Região")

regiao_mt <- mt_municipio %>% merge(dados_mt, by="CD_MUN")
dados_mt <- dados_mt %>% merge(mt_municipio, by="CD_MUN")

dados_loa<- read.xlsx("Despesa.xlsx", sheet=2)

dados_loa <- dados_loa[,c("regiao","DESCRICAO_RECURSO","COD_GRUPO","Valor.PTA")] 

names(dados_loa) <- c("regiao","DESCRICAO_RECURSO","COD_GRUPO","valor_pta") 

dados_loa <- dados_loa%>% filter(COD_GRUPO == 4) %>% 
  group_by(regiao) %>%
  summarise(total = sum(valor_pta))

quantile(dados_loa$total)
# Setting population density classes
dados_loa$FAIXA <- cut(dados_loa$total,breaks=c(0, 5435054,  20349211, 100236338, 900485930, Inf),
                       labels=c('Até R$5.4 milhões','De R$5.4 a R$20.3 milhões',
                                'De R$20.3 milhões a R$100 milhões',
                                'De R$100 milhões a R$900 milhões', 'Acima de R$900 milhões'))



mt_mun<- dados_loa  %>% merge(dados_mt, by = "regiao")

#pclr <- brewer.pal(8, "Dark2")
pclr = c("#1B9E77", "#D95F02", "#7570B3","#A50F15", "#67000D", "#66A61E","#9ECAE1", "#4292C6", "#08519C", "#08306B", "#2171B5", "#08306B")

# dfinir os nomes das microrregições 
points <- cbind(mt_mun, st_coordinates(st_centroid(mt_mun$geometry)))




renderPlotly({
  
  
  
mt_mun <- mt_mun%>% filter(DESCRICAO_RECURSO == fonte_mapa[[1]])
  
  # Plotting
  ggplot(data=mt_mun, aes(geometry = geometry))+
    # definir faixas e divisão administrativa dos municípios
    geom_sf(aes(fill=factor(FAIXA)),color=alpha("grey",0.4),data=mt_mun)+
    labs(x = NULL, y=NULL, fill= 'Faixa Investimento')+
    # faixa das regiões
    geom_sf(fill='transparent',color='black',data=mt_mun)+
    # nomes das municípios
    geom_text(data= points,aes(x=X, y=Y, label=nota),
              color = "Black", fontface = "bold", check_overlap = FALSE, size = 1.9)+
    
    # escala munual das cores  
    #scale_fill_manual(values = pclr) +
    # formação do título e das legendas
    ggtitle(" ", 
            subtitle = ' ')+
    labs(caption=c('Fonte: SEFAZ, 2023'))+
    # retira o sistema cartesiano: tipo de layout do mapa padrão
    theme(panel.grid = element_line(colour = "transparent"),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
})


#############################################################################
########################### FLUXO DE RECEITA E DESPESA ######################

library(readxl)
library(tidyverse)

rec <- read_excel("Receita.xlsx", sheet = 4, col_names = TRUE)

rec <- rec[,c("Especie", "FTE", "FTE_DESCRICAO", "LOA_2023")]

names(rec) <- c("nr", "fte", "fte_nome", "rec")

des <- read_excel("Despesa.xlsx", sheet = 2, col_names = TRUE)
des <- des[,c("FTE", "DESCRICAO_FTE", "agrupamento","Valor PTA")]
names(des) <- c("fte", "fte_nome", "nd", "des")

rec <- rec %>%
  group_by(nr) %>%
  mutate(rec = rec,  #/1000000
         subtot_rec = sum(rec),
         per_rec_fte = rec/sum(rec)) # calcula os % de distrib. de cada rec nas ftes

des <- des %>%
  group_by(fte) %>%
  mutate(des = des, #/1000000
         per_fte_des = des/sum(des)) # calcula os % de distrib. de fte nas desps



# tenho uma lista de fontes

matriz <- rec %>%
  full_join(des, by = "fte") # pelo código da fonte, pq vai que o nome da fonte muda...

# calcula o % de dist. de cada rec nas desps
# calcula então o tamanho de cada link, partir do subtotal de cada receita. ignorando se for menor que um bilhao
### a fazer: remover as linhas com um anti_join, para não contaminar a informação do hoover (outgoing, ingoing count)

matriz <- matriz %>% 
  mutate(p = per_rec_fte * per_fte_des, 
         # ramo = ifelse((round(subtot_rec * p,0)<1000000000),0,round(subtot_rec * p,0))) %>%
         ramo = round(subtot_rec * p,0)) %>% 
  group_by(nr,nd) %>%                        
  summarise(p = sum(p),
            ramo = sum(ramo)) %>%
  select(nr,nd,p,ramo)

# limpeza

tamanho_critico <- 1e1 
ramos_a_limpar <- matriz %>% filter(ramo<tamanho_critico) #fazer no shiny

matriz_original <- matriz

matriz <- matriz %>%
  anti_join(ramos_a_limpar)

# relacao unica dos rotulos de receita e despesa:
rotulos <- c(unique(matriz$nr),unique(matriz$nd))

# conta os nós e gera sequencia numerica a partir de zero
num_nos <- length(rotulos)
nos <- 0:(num_nos-1)

# cria tabelinha para numerar os nos
tab_aux <- data.frame(rotulos, nos)

# incorpora os números dos nodes na matriz, para a receita... e para a despesa.
matriz <- matriz %>%
  left_join(tab_aux, by = c("nr" = "rotulos")) %>%   
  left_join(tab_aux, by = c("nd" = "rotulos"), suffix = c("_rec","_desp"))



# DEFINIÇÕES DAS CORES
library(RColorBrewer)

azul_STN <- "#1f476a"
cor_divida_transluc <- "rgba(186, 57, 23, 0.7)"
cor_RPPS_transluc <- "rgba(31, 71, 106, 0.7)"
cor_divida <- "#BA3917"
cor_RPPS <- azul_STN
cor_Pessoal <- "#01665E"

nclr = 12
#pclr <- brewer.pal(nclr, "Greens")#[5:9]# " tons de cores començando em 4 até 9"
cor_padrao_despesas <- c(brewer.pal(nclr, "Paired"))



qde_rec_destacadas <- 1
qde_cores_extras <- length(unique(matriz$nr)) - qde_rec_destacadas

paleta_STN <- brewer.pal(qde_cores_extras, "Set3")
#paleta_STN <- c("#F8AC08","#CEC806","#96C11E","#028063","#149339","#4E857E","#6E287C")

paleta_STN_rgb <- col2rgb(paleta_STN)

# essa função gera uma matriz com os componentes RGB.
# precisamos fornecer as cores no formato "rgb(r,g,b)"
# além disso, para os ramos, é interessante que as cores sejam translúcidas, 
# então vamos acrescentar um componente de transparência

paleta_STN_transluc <- NULL
for (i in 1:dim(paleta_STN_rgb)[2] ) {
  paleta_STN_transluc <- c(paleta_STN_transluc, paste("rgba(", paleta_STN_rgb[1,i],", ",  paleta_STN_rgb[2,i],", ",  paleta_STN_rgb[3,i],", 0.7)", sep = ""))
}

# rotininha para povoar as cores dos ramos e dos nos, conforme as receitas

receitas <- unique(matriz$nr)

cores_ramos <- NULL
cores_nos <- NULL
k = 1

for (i in 1:length(receitas)) {
  if (receitas[i] == "Contribuições ao Plano Financeiro e Previdenciário do RPPS") {
    cores_ramos <- c(cores_ramos, cor_RPPS_transluc)
    cores_nos <- c(cores_nos, cor_RPPS)
  }
  else if (receitas[i] == "Contribuições Econômicas") {
    cores_ramos <- c(cores_ramos, cor_divida_transluc)
    cores_nos <- c(cores_nos, cor_divida)
  }
  else {
    cores_ramos <- c(cores_ramos, paleta_STN_transluc[k])
    cores_nos <- c(cores_nos, paleta_STN[k])
    k <- k + 1 # o certo seria fazer algo modular, caso sejam mais receitas que cores
  }
}

## povoando as cores das despesas... (com um método mais elegante do que um loop)
despesas <- unique(matriz$nd)
cores_despesas <- rep(cor_padrao_despesas, length(despesas))

cores_despesas[despesas == "Amortização da Dívida" | despesas == "Juros"] <- cor_divida

cores_despesas[despesas == "Previdência Social (RPPS)"] <- cor_RPPS

cores_despesas[despesas == "Pessoal e Encargos Sociais"] <- cor_Pessoal

## fechando a lista de cores dos nos

cores_nos <- c(cores_nos, cores_despesas)

## tabelinha para usar de referência no preenchimento da matriz

tabelinha_cores_ramos <- data.frame(receitas, cores_ramos)

## preenchendo as cores dos ramos na matriz a partir dessa tabelinha

matriz <- matriz %>%
  left_join(tabelinha_cores_ramos, by = c("nr" = "receitas"))

## sem dívida

# qual o numero do no de juros
no_juros <- unique(matriz$nos_desp[which(matriz$nd=="Juros")])

# qual o numero do no de amortização da dívida
no_amort <- unique(matriz$nos_desp[which(matriz$nd=="Amortização da Dívida")])

matriz_semdivida <- matriz %>%
  mutate(nd = replace(nd, nd=="Juros", "Dívida"),
         nd = replace(nd, nd=="Amortização da Dívida", "Dívida"),
         nos_desp = replace(nos_desp, nd=="Dívida", min(no_juros,no_amort))) %>%
  #filter(!(nr == "Emissões de títulos" & nd == "Dívida")) %>% # atenção a esse filtro!
  group_by(nr,nd,nos_rec,nos_desp,cores_ramos)%>%
  summarize(p = sum(p),
            ramo = sum(ramo))

rotulos_divida <- c(unique(matriz_semdivida$nr),unique(matriz_semdivida$nd))




# DIAGRAMA

library(plotly)

# os textos do hover estão hardcoded no plotly. Então, em plotly-latest.min.js,
# que está na instalação do R, R\win-library\3.4\plotly\htmlwidgets\lib\plotlyjs,
# substituí "Incoming Flow Count", "Outgoing Flow Count", "Source:" e "Target:"
# pelas expressões que eu quis.


p <- function(dados,nomes){
  plot_ly(
    type = "sankey",
    orientation = "h",
    width = 980,
    height = 720,
    opacity = 0.6, 
    # será q isso controla a opacidade dos hovers? # nope, é hardcoded,
    #valueformat = ">-.3~g",
    #valuesuffix = " bilhões de reais",
    
    textfont = list(
      family = "Open Sans",
      color = "#444444",
      size = 12
    ),
    
    node = list(
      label = nomes,
      color = cores_nos,
      pad = 10,
      thickness = 25,
      line = list(
        color = "",
        width = 0
      )
    ),
    
    hoverlabel = list(
      font = list(
        family = "Open Sans"
      )
    ),
    
    link = list(
      source = dados$nos_rec,
      target = dados$nos_desp,
      value =  c(dados$ramo,dados$p),
      color = dados$cores_ramos
      #color =  "rgba(255,213,0,0.4)" 
      # para deixar a cor translucida, é preciso usar rgba
      
    )
  ) %>% 
    layout(
      title = "",
      font = list(
        family = "Open Sans",
        size = 11,
        color = "#004a93"
      )
    )
}

# versão completa

p(matriz, rotulos)



# https://plotly-r.com/preface.html
# https://www.tesourotransparente.gov.br/historias/fluxo-de-receitas-e-despesas
# https://plotly.com/python/sankey-diagram/

# https://github.com/plotly/plotly.js/blob/d971001a04273334d858930957fa86ebb0952e71/lib/locales/pt-br.js
# https://stackoverflow.com/questions/72957392/edit-boxplot-tooltip-text-plotly


