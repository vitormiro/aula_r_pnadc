
## Indicadores de pobreza com a PNAD Cont�nua
## Apresenta��o em 30 de abril de 2020 - PARTE 1
## Vitor Hugo Miro

###--------------------------------------------------------------------------###

# limpar "�rea de trabalho" do R (mem�ria R)
rm(list = ls())

# Fixar um diret�rio de trabalho.
setwd("C:/Users/vitor/Desktop/aula_r_pnadc")

options(scipen = 999) # para evitar nota��o cient�fica.

###--------------------------------------------------------------------------###

# Carregar pacotes necess�rios
library(PNADcIBGE)
library(survey)

###--------------------------------------------------------------------------###
# Definir vari�veis utilizadas na an�lise
variaveis <- c('Ano', 'UF', 'Estrato', 'UPA', 'V1008', 'V1014', 'V1032', 
               'V1022', 'V2001', 'V2005', 'V2007', 'V2009', 
               'VD4001', 'VD4002', 'VD4016', 'VD4017', 
               'VD4019', 'VD4020', 'VD4022', 'VD4048', 'VD5008')

#UPA - Unidade Prim�ria de Amostragem (UPA)
#V1008 - N�mero de sele��o do domic�lio
#V1014 - Painel
#V1032 - Peso do domic�lio e das pessoas (com p�s estratifica��o)
#V1022 - Situa��o do domic�lio (urbano x rural)
#V2001 - N�mero de pessoas no domic�lio
#V2005 - Condi��o no domic�lio
#V2007 - sexo
#V2009 - idade do morador na data de refer�ncia
#VD4016 - Rendimento mensal habitual do trabalho principal
#VD4017 - Rendimento mensal efetivo do trabalho principal
#VD4019 - Rendimento mensal habitual de todos os trabalhos
#VD4020 - Rendimento mensal efetivo de todos os trabalhos
#VD4022 - Rendimento mensal efetivo de todas as fontes
#VD4048 - Rendimento efetivo recebido de outras fontes
#VD5008 - Rendimento domiciliar per capita

###--------------------------------------------------------------------------###
# Carrega dados da PNADC 
pnadc <- get_pnadc(year = 2019,
                   vars = variaveis,
                   interview = 1,
                   design = TRUE,
                   labels = TRUE,
                   deflator = TRUE,
                   defyear = 2019)

# estrutura dos dados
str(pnadc)

# Classe do objeto gerado
class(pnadc)  
# get_pnadc com design = TRUE, resulta em um objeto survey.design


## Dicas para salvar os dados em diret�rio local
# Salvar o arquivo em RDS
# saveRDS(pnadc, "pnadc19")

# Para carregar os dados da PNADC 
# pnadc <- readRDS(file="pnadc19")


###--------------------------------------------------------------------------###
## Indicadores com fun��es do pacote survey
# Totais - svytotal

popuf <- svytotal(~UF, pnadc, na.rm = TRUE)
popuf

sexo <- svytotal(~V2007, pnadc, na.rm = T)
sexo

# M�dias - svymean

rtrabp <- svymean(~VD4016, pnadc, na.rm = TRUE)
rtrabp

rdpc <- svymean(~VD5008, pnadc, na.rm = TRUE)
rdpc

# Propor��es - svymean

psexo <- svymean(~V2007, pnadc, na.rm = TRUE)
psexo

parea <- svymean(~V1022, pnadc, na.rm = TRUE)
parea

# Raz�es - svyratio

txdesocup <- svyratio(~VD4002 == "Pessoas desocupadas",
                      ~VD4001 == "Pessoas na for�a de trabalho", 
                      pnadc, 
                      na.rm = TRUE)
txdesocup

###--------------------------------------------------------------------------###
## Estima��o condicionada (em subconjuntos dos dados)

rdpc_ce <- svymean(~VD5008, subset(pnadc, UF == "Cear�"), na.rm = TRUE)
rdpc_ce

rdpc_ce_rural <- svymean(~VD5008, subset(pnadc, UF == "Cear�" & V1022 =="Rural"), 
                   na.rm = TRUE)
rdpc_ce_rural

rtrabp_m <- svymean(~VD4016, subset(pnadc, V2007 == "Mulher"), na.rm = TRUE)
rtrabp_m

rtrabp_m25 <- svymean(~VD4016, 
                    subset(pnadc, V2007 == "Mulher" & V2009>=25), 
                    na.rm = TRUE)
rtrabp_m25


###--------------------------------------------------------------------------###
# C�lculo de indicadores de pobreza e desigualdade com o Convey

# Carrega o pacote convey
library(convey)

# Transforma o objeto "survey.design" no objeto que o 
#convey utiliza para as estima��es "convey.design"
pnadc <- convey_prep(pnadc)

class(pnadc)


###--------------------------------------------------------------------------###
# Indicadores de pobreza com fun��es do pacote convey

# Pobreza
fgt0 <- svyfgt(~VD5008, pnadc, g=0, abs_thresh = 436, na.rm = TRUE)
fgt0

fgt0 <- coef(svyfgt(~VD5008, pnadc, g=0, abs_thresh = 436, na.rm = TRUE))*100
fgt0

# Gini
gini <- svygini(~VD5008, pnadc, na.rm = TRUE)
gini

gini_ce <- svygini(~VD5008, subset(pnadc, UF == "Cear�"), na.rm = TRUE)
gini_ce


###--------------------------------------------------------------------------###
