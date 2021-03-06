
## Indicadores de pobreza com a PNAD Cont�nua
## Apresenta��o em 30 de abril de 2020 - PARTE 2
## Vitor Hugo Miro

###--------------------------------------------------------------------------###
# limpar "�rea de trabalho" do R (mem�ria R)
rm(list = ls())

wd = "C:/Users/vitor/Desktop/aula_r_pnadc"

# Fixar um diret�rio de trabalho.
setwd(wd)

options(scipen = 999) # para evitar nota��o cient�fica.

###--------------------------------------------------------------------------###

# Pacotes
library(PNADcIBGE)
library(tidyverse)
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
                   design = FALSE,
                   labels = FALSE,
                   deflator = TRUE,
                   defyear = 2019)

# estrutura dos dados
str(pnadc)

# Classe do objeto gerado
class(pnadc)  
# get_pnadc com design = FALSE, resulta em um DataFrame

###--------------------------------------------------------------------------###

# Converter o DataFrame (pnadc) para "survey.design" (svypnadc)
# pnadc_design utiliza o pacote 'survey'

svypnadc <- pnadc_design(pnadc)


class(svypnadc)
# o resultado � um objeto "survey.design" para a an�lise com o pacote survey

###--------------------------------------------------------------------------###

# Com um objeto do tipo "survey.design" n�o podemos aplicar opera��es a linguagem
# tidyverse (que � bastante funcional)
# Para usar o tidyverse e seus pacotes na an�lise de complex survey como a PNADC
# podemos usar o pacote srvyr

# Carrega o pacote srvyr
library(srvyr)

# Transformar o objeto de survey com a fun��o 'as_survey' do pacote srvyr
svypnadc <- as_survey(svypnadc)

class(svypnadc)
# teremos um objeto "tbl_svy"

###--------------------------------------------------------------------------###

# Criar variaveis individuais
svypnadc <- svypnadc %>% 
    mutate(
        id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
        membro = ifelse (V2005 < 15, 1, 0),
        
        ### Contruir vari�veis de renda
        outras_fontes = ifelse (is.na (VD4048), 0, VD4048),
        trabalho = ifelse (is.na (VD4019), 0, VD4019),
        renda = trabalho + outras_fontes
    )

# Criar variaveis agregadas por domicilio
svypnadc <- svypnadc %>% 
    group_by(id_dom) %>% 
    mutate(n_dom = sum(membro),
           # Construir vari�vel de renda domiciliar total e rdpc
           outras_dom = ifelse(membro==1, sum(outras_fontes), 0),
           trabalho_dom = ifelse(membro==1, sum(trabalho), 0),
           renda_dom = ifelse(membro==1, sum(renda), 0),
           rdpc = renda_dom/n_dom )

###--------------------------------------------------------------------------###
# Renda domiciliar per capita m�dia

svymean(~rdpc, svypnadc, na.rm = TRUE)

###--------------------------------------------------------------------------###

# C�lculo de indicadores de pobreza e desigualdade com o Convey

# Carrega o pacote convey
library(convey)

svypnadc <- convey_prep(svypnadc)

class(svypnadc)

###--------------------------------------------------------------------------###

# Pobreza
svyfgt(~rdpc, svypnadc, g=0, abs_thresh = 436, na.rm = TRUE)

# Gini
svygini(~rdpc, svypnadc, na.rm = TRUE)


###--------------------------------------------------------------------------###

# Criar variaveis de pobreza e extrema pobreza

## Linhas de pobreza do Banco Mundial.
lpx <- 151  # US$ 1,90/dia ~ R$151/m�s
lp <- 436   # US$ 5,5/dia ~ R$436/m�s

svypnadc <- svypnadc %>% 
    mutate(poverty = ifelse (rdpc < lp, 1, 0),
           expoverty = ifelse (rdpc < lpx, 1, 0))

###--------------------------------------------------------------------------###

# Alguns indicadores poss�veis (exemplos)

pov <- svymean(~poverty, svypnadc, na.rm = TRUE)
pov

fgt0 <- svyfgt(~rdpc, svypnadc, g=0, abs_thresh = lp)
fgt0

xpov <- svymean(~expoverty, svypnadc, na.rm = TRUE)
xpov

xfgt0 <- svyfgt(~rdpc, svypnadc, g=0, abs_thresh = lpx)
xfgt0

###--------------------------------------------------------------------------###
## Indicadores em subconjuntos de dados (exemplos)

svypnadc <- svypnadc %>% 
    mutate(gidade = factor(case_when(V2009 %in% 0:14 ~ "0-14",
                                     V2009 %in% 15:29 ~ "15-29",
                                     V2009 %in% 30:59 ~ "30-59",
                                     V2009 >= 60 ~ "60+")),
           sexo = factor(case_when(V2007==1 ~ "homem",
                                   V2007==2 ~ "mulher")))

## Pobreza por sexo
svyby(~poverty, ~sexo , svypnadc, svymean, na.rm = TRUE)
svyby(~poverty, ~gidade , svypnadc, svymean, na.rm = TRUE)
svyby(~VD4019, ~sexo , svypnadc, svymean, na.rm = TRUE)


## Pobreza para um grupo especifico usando 'subset'
svyfgt(~rdpc, subset(svypnadc, UF == "23"), g=0, abs_thresh = lp)
