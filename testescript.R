##Limpar Memoria
rm(list=ls())

## Instalar Pacotes
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("DT")
#devtools::install_github("renkun-ken/formattable")

## Carregar Pacotes
library(devtools)
require(tidyverse)
require(DT)
require(formattable)

#Importacao dos dados
dadosteste <- read.csv("C:/Users/USER/OneDrive/Estuda/dados_teste.csv")
#Vizualiza??o
glimpse(dadosteste)

############

resumo1 = dadosteste %>%
  select(X,NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT)

  names(resumo1) = c("Id","Ciencias Humanas", "Ciencias da Natureza", 
                    "Linguagens e C?digos", "Matematica")
 
resumo1 = resumo1 %>%
  pivot_longer(!Id, names_to = "Area", values_to = "Nota") %>%
  group_by(Area) %>%
  summarise(M?dia = round(mean(Nota),2), 'Desvio Padr?o' = round(sd(Nota),2))

datatable(resumo1, options = list(dom = 't'), filter = "none" )

############

resumo2 = dadosteste %>%
    mutate(Idade = ifelse(NU_IDADE < 23 & NU_IDADE > 17,
                          "Maior que 17 e menor 23", "Caso Contrario")) %>%
    group_by(Idade) %>%
    summarise('N?mero'= n(),'Porcentagem'= paste0(n()/nrow(dadosteste)*100,
                                                   '%')) %>%
    arrange(desc(Idade))

resumo2 = dadosteste %>%
  mutate(Idade = ifelse(NU_IDADE < 23 & NU_IDADE > 17,
                        "Maior que 17 e menor 23", "Caso Contrario")) %>%
  group_by(Idade) %>%
  summarise(N?mero = n(), Porcentagem = percent(n()/nrow(dadosteste))) %>%
  arrange(desc(Idade))

datatable(resumo2, options = list(dom = 't'), filter = "none" )

############

resumo3 = dadosteste %>%
  select(Estado = SG_UF_PROVA) %>%
  group_by(Estado) %>%
  summarise(N?mero = n(), Porcentagem = percent(n()/nrow(dadosteste))) %>%
  arrange(desc(N?mero))

datatable(resumo3)
formattable(resumo3)

############

resumo4 = dadosteste %>%
  select(NU_IDADE, TP_ESCOLA) %>%
  mutate(Tipo_Escola = factor(TP_ESCOLA,
                       levels = c(1,2,3,4),
                       labels = c("N?o Respondeu", "Publica", "Privada", 
                                    "Exterior"))) %>%
  group_by(Tipo_Escola) %>%
  summarise(M?dia = round(mean(NU_IDADE),2), Minimo = min(NU_IDADE),
            Maximo = max(NU_IDADE))

datatable(resumo4, options = list(dom = 't'), filter = "none" )

############

resumo5 = dadosteste %>%
  mutate(TP_LINGUA = ifelse(TP_LINGUA == 0,'Ingl?s','Espanhol')) %>%
  group_by(Lingua = TP_LINGUA) %>%
  summarise(M?dia = round(mean(NU_NOTA_LC),2), 
            'Desvio Padr?o' = round(sd(NU_NOTA_LC),2))

datatable(resumo5, options = list(dom = 't'), filter = "none" )

############
dados = dadosteste
names(dados)[grep('SG_UF_PROVA', names(dados))] <- 'UF'

dados$Regiao[dados$UF %in% c('RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO')] <- 'Norte' 
dados$Regiao[dados$UF %in% c('MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA')] <- 'Nordeste' 
dados$Regiao[dados$UF %in% c('MG', 'ES', 'RJ', 'SP')] <- 'Sudeste'   
dados$Regiao[dados$UF %in% c('PR', 'SC', 'RS')] <- 'Sul'
dados$Regiao[dados$UF %in% c('MS', 'MT', 'GO', 'DF')] <- 'Centro Oeste'

resumo6 = dados %>%
  mutate(NU_IDADE = ifelse(NU_IDADE <= 21, "21 anos ou menos", "Mais de 21 anos")) %>%
  group_by(Idade = NU_IDADE, Regiao) %>% 
  summarise(N = n(), Porcentagem = percent(n()/nrow(dados)),
            M?dia = round(mean(NU_NOTA_MT),2))

datatable(resumo5, options = list(dom = 't'), filter = "none" )


#############
### Calculo da Taxa de acertos por quest?o e Area 

#Ciencias da Natureza

RESPOSTAS_CN = data.frame(1:45)
for (i in 1:2000) {
  RESPOSTAS_CN[i] = strsplit(dadosteste$TX_RESPOSTAS_CN[i], "")
} 
names(RESPOSTAS_CN)[1] <- 'V1'
RESPOSTAS_CN[,1:10]

GABARITO_CN = unlist(strsplit(dadosteste$TX_GABARITO_CN[1], ""))
 
MT_RESPOSTA_CN = matrix(rep(0), nrow = 45, ncol = 2000)

for(j in 1:45){
for(i in 1:2000) {
  MT_RESPOSTA_CN[j,i] = ifelse(RESPOSTAS_CN[j,i] == GABARITO_CN[j],1,0)
  }
    }

taxaAcertoCN = percent(rowMeans(MT_RESPOSTA_CN))

#Ciencias Humanas

RESPOSTAS_CH = data.frame(1:45)
for (i in 1:2000) {
  RESPOSTAS_CH[i] = strsplit(dadosteste$TX_RESPOSTAS_CH[i], "")
} 
names(RESPOSTAS_CH)[1] <- 'V1'

GABARITO_CH = unlist(strsplit(dadosteste$TX_GABARITO_CH[1], ""))

MT_RESPOSTA_CH = matrix(rep(0), nrow = 45, ncol = 2000)

for(j in 1:45){
  for(i in 1:2000) {
    MT_RESPOSTA_CH[j,i] = ifelse(RESPOSTAS_CH[j,i] == GABARITO_CH[j],1,0)
  }
}

taxaAcertoCH = percent(rowMeans(MT_RESPOSTA_CH))

#Linguagens Codigos

RESPOSTAS_LC = data.frame(1:45)
for (i in 1:2000) {
  RESPOSTAS_LC[i] = strsplit(dadosteste$TX_RESPOSTAS_LC[i], "")
} 
names(RESPOSTAS_LC)[1] <- 'V1'

GABARITO_LC = unlist(strsplit(dadosteste$TX_GABARITO_LC[1], ""))

MT_RESPOSTA_LC = matrix(rep(0), nrow = 45, ncol = 2000)

for(j in 1:45){
  for(i in 1:2000) {
    MT_RESPOSTA_LC[j,i] = ifelse(RESPOSTAS_LC[j,i] == GABARITO_LC[j],1,0)
  }
}

taxaAcertoLC = percent(rowMeans(MT_RESPOSTA_LC))

#Matematica

RESPOSTAS_MT = data.frame(1:45)
for (i in 1:2000) {
  RESPOSTAS_MT[i] = strsplit(dadosteste$TX_RESPOSTAS_MT[i], "")
} 
names(RESPOSTAS_MT)[1] <- 'V1'

GABARITO_MT = unlist(strsplit(dadosteste$TX_GABARITO_MT[1], ""))

MT_RESPOSTA_MT = matrix(rep(0), nrow = 45, ncol = 2000)

for(j in 1:45){
  for(i in 1:2000) {
    MT_RESPOSTA_MT[j,i] = ifelse(RESPOSTAS_MT[j,i] == GABARITO_MT[j],1,0)
  }
}

taxaAcertoMT = percent(rowMeans(MT_RESPOSTA_MT))

## Tabela Taxa de Acerto

taxaAcerto = tibble(1:45,taxaAcertoCH, taxaAcertoCN, 
                    taxaAcertoLC, taxaAcertoMT)
            
names(taxaAcerto) = c("Quest?o", "Ci?ncias Humanas", "Ci?ncias da Natureza", 
                      "Linguagens e C?digos", "Matem?tica")

formattable(taxaAcerto)

################
######GRAFICOS

dados1 <- dados %>%
    mutate(Idade = ifelse(NU_IDADE <= 21, 'Idade1' , 'Idade2'),
         Nota_Matematica = NU_NOTA_MT) %>%
    select(Regiao, Idade, Nota_Matematica) 

ggplot(dados1, aes(x = Regiao, y = Nota_Matematica, fill = Regiao)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Accent") +
  theme_gray() +
  facet_wrap(vars(Idade)) 

################################################################################

glimpse(dados)

x = colMeans(MT_RESPOSTA_CH)
y = dados$NU_NOTA_CH

plot(x,y)
plot(x, y, ylab = "Nota de Ci?ncias Humanas", xlab = "Score de Acertos")

abline(m)

summary(m)

cor(y,x)



