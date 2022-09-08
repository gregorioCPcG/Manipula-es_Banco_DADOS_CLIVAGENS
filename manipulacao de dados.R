
# tutorial exemplo latinobarometro 2006 
#primeiro baixar pacotes utilizados na manipulação de dados em 2006
library(haven)# pra baixar o arquivo do tipo .sav(se for de outro tipo tem outro pacote)
# caso nao tenha algum pacote deve fazer install.packages("nome do pacote")
library(memisc)#pacote para manipulação
library(car)# pacote aqui usado para manipulação
library(labelled)#operação util quando arquivo é .sav
options(scipen = 1000, digits=5) #para nao aparecer notação cientifica
library(tidyverse)# baixa sempre , útil pra c. (aqui usado para manipulação)

df2006 <- read_sav("Latinobarometro_2006_datos_esp_v2014_06_27.sav")# baixando o pacote, o nome df2006 é um nome dado para facilitar depois
#exemplo tem uma variável que queremos recodificar pq o formato dela não e conveniente
str(df2006$p30st)# para infos
# essa variável pe interessante que seja recodifica da de forma a opçao 2 se torne zero, pq noss sinal positivo é de direita e como 1 é de direita temos que deixar todas as issues com o mesmo sentido como no arquivo sentido questoes 2006.xlsx



df2006$p30st_recod <- memisc::recode(as.numeric(df2006$p30st),
                                              1 <- c (2), 2 <- c(1))#

df2006$p88n_recod <- memisc::recode(as.numeric(df2006$p88n),
                                             0 <- c (2,3), 1 <- c(1))## p88 é ed interesse que posição 1 seja destacad das demais e isso que o código opera

df2006$praticante <- memisc::recode(as.numeric(df2006$s3),
                                             1 <- c (4), 2 <- c(3),3<-c(2),4<-c(1))#

df2006$sexo <- as.factor(df2006$sexo)# transformar sexo em factor, vai ajudar nas analises posteriores

summary(df2006[,300:302])# pra confirmar  bom dizer q antes da virgula refere-se a linhas (casos) e depois à colunas (variáveis) - aqui nesse exemplo vemos as colunas 300 a 302 - que sao as tres novas que criamos acima


table(df2006$idenpa)# para ver os países, vai pelo arquivo do questionário pra achar do interesse

# selecionar só bra
bra <- df2006 %>%
  filter(idenpa == 76)# código do brasil no latinobarometro
#pra outro país basta alterar código - nome da nova brase = "bra"

bra <- remove_labels(bra)# operacção necessária dado que o arquivo vem do formato spps(.sav) e dá erro nas análises

bra <- subset(bra,select=c(p15st,
                           p16n,p22na.f,p17st,p24st.a,p24st.d,p25st,
                           p26st,p35st.d,p39st,p88n_recod,p30st_recod,
                           praticante,p47st, sexo,p13st.a, reeduc1,s7))
# a função subset é muito útil pq ela só puxa as variáveis que se quer para a base
# como eu sei que essas questões são as que eu quero?
# pq eu selecionei antes no questionário
# pronto bra é casos do brasil com 18 variáveis ()
summary(bra)
summary(bra[,1:12])#as doze primeiras colunas são as issues
# veja arquivo questoes2006_ISSUES para ver essas 12
# veja o arquivo variaveis demografic.2006 para ver as 6 sociodemografic
# lembrando que fiz essa seleção com base no questionário de 2006
# para cada onda e para cada banco tem q fazer ,
# em alguns casos há o arquivo merge (unidos)
# também é possivel unir os bancos posteriormente ()
# é possivel incluir mais issues e variaveis demograficas basta inserir no codebook
# mas por hoar vamos seguir nesse para fixar bem a operação de manipulação


rm(df2006)# deletei df2006 pq não é mais necessária aqui


# como trabalhamos com fatores, tenho por prática não renomear as issues, foco essa atenção nas variáveis independents, portanto as ultimas seis do data frame bra. Vamos olhar um pouco pra elas:

summary(bra[,13:18])#colunas 13 a 18
# note que já tem NA - ou seja as missings nada precisa ser feito, se nao teria que criar missings (se um dia precisarem me procurem)
# note também que -> se precisarem um dia um exemplo é esse BASET$Q405[BASET$Q405 == -1] <- NA - esse torna a -1 como missing

# praticante, note que praticante(COLUNA 13 de bra) já foi recodificado acima, os valores vão de 1 a 4 (sendo um pouco praticante e 4 muito praticante)
summary(bra$praticante)# vai de 1 a 4 como já era conhecido acima


# ideologia auto posicionamento (1 esq e 10 direita)# exemplo criar 3 categorias (direita, esquerda e centro)
summary(bra$p47st)# pra ver como é antes, sempre façam
bra$p47st<- memisc::recode(as.numeric(bra$p47st), 1 <- c (0,1,2,3), 2 <- c(4,5,6),3 <- c(7,8,9,10))# 1 passa a ser esquerda, 2 centro e 3 direita
summary(bra$p47st)# pra confirmar que deu certo , sempre confirmem!!!!
# agora vamos transformar em fatores - (pra comparar direita x esquerda, esquerda x centro, centro x direita)

bra <- within(bra, {
  ideologia <- Recode(p47st, '1 = "Esquerda"; 2 = "Centro"; 3 = "Direita"', as.factor=FALSE)

  })
# OBS se não roda aperta na vassourinha e roda tudo que tiver em cima de novo, resolve o erro do pacote em 99% das vezes

table(bra$p47st) #para conferir
table(bra$ideologia) # para conferir tem q ficar igual o de cima
# bateu

# classe social subjetiva (1 muy pobre e 10 muy rico)# manter como contínua
# basta alterar o nome , pois vamos manter contínua
bra$classe_social_subjetiva <- bra$p13st.a 

# educação
# o legal em eduação é comparar entre níveis, portanto vamos criar uma variavel que ao invés de ser numérica é fatorial, com 7 fatores que representam níveis educacionais do respodente
bra$nivel_educacional <- as.factor(bra$reeduc1)
summary(bra$nivel_educacional)
table(bra$reeduc1)# se as duas ficram igual deu certo

# idade
# operação bem simples só mudar o nome - já que vamos manter contínua
bra$idade <- bra$s7
summary(bra$idade)#ficou igual s7 e os valores vão de 16 a 93 anos.
# pronto idade chama idade

# está errado está mulher como 2 e homem como 1 vamos mudar?
summary(bra$sexo)# 628 mulheres
# Mulher
bra <- bra %>%
  mutate(Mulher = case_when(sexo == "2" ~ 1,
                            TRUE ~ 0))

table(bra$Mulher)# agora temos uma dicotômica correta, sendo 1 mulher e zero homem.





summary(bra)# como está agora
# se quiser pode excluir as variáveis subsituidas colunas 13,14,15,16 e 17
bra<-bra[,-c(14:18)]#
summary(bra)# vea como ficou limpo , sempre dá o summary pra conferir

# da pra fazer a mesma coisa que foi feita acima com a função subset (mais segura, pq se tem erro ela avisa)


# fim do tutorial - duvidas? manda pra mim, ou pergunta pro google (em inglês ou em portugues,sério ajuda muito!)tenta descrever teu problema e tu vai ver como lá encontras resposta

# extras####

# do 1 ao 12 é issue 
# do 13 ao 18 é variável independente

#- vou preceder algumas análises explortórias abaixo básicas pra vocês verem os dados em ação

table(bra$p25st) # isso nos diz que 481 brasileiros topariam a democracia sem partidos (assumidamente)
# como eu sei que é isso , eu olhei a lista de questões2006_Issues e depois olhei o Sentido das questões_2006... para saber

# vamo ver isso em porcentagem

prop.table(table(bra$p25st))# 46,92% , mas ficou feio, ficou 0.46
prop.table(table(bra$p25st))*100# agora deu 46,92%

#tabela cruzada gênero e aceitação democracia sem partidos

table(bra$p25st, bra$Mulher)# ficou estranho, melhor ver em percentagem

prop.table(table(bra$p25st, bra$Mulher),2)*100
# as colunas referem-se a genero (0 homem e 1 mulher)
# as linhas referem-se a aceitação democracia sem partidos (1 nao aceita, 2 aceita)
# 44,6 % das mulheres apoiam democracia sem partidos
# 49,21% dos homens
# olha que legal

# com essa info provisória da pra fazer duas coisas, testar se essa relação é estatisticamente significativa e construir uma bonita tabela

# 1. teste de relação
# temos uma tabela 2x2 então vamos ao teste de significação
# primeiro passo = criar um objeto
objeto1 <- prop.table(table(bra$p25st, bra$Mulher),2)*100 # objeto1 é um nome que eu dei poderia ser qualquer um(nunca dê espaço, se precisar use _)

chisq.test(objeto1)# não é sig, a diferença nao é estatisticamente significativa
library(vcdExtra)
GKgamma(objeto1) # cuidado o gamma superestima, e mesmo assim olher o CI toca no zero. 
# ou seja, Não há segurnaça estatística que genero explique p25st


#mesmo nao tendo , vamos fazer uma tabelinha bunita

# primeiro com o pacote plot o basicao que já vem instalado

# primeiro mudar os nomes das categorias
bratest <- subset(bra, select=c(p25st, Mulher))#selecionar só p25st e Mulher
bratest$p25st <- as.factor(bratest$p25st)#transformar em fatorial
bratest$Mulher <- as.factor(bratest$Mulher)#transformar em fatorial
levels(bratest$Mulher) <- c('MASC','FEM')#categorias de genero
levels(bratest$p25st) <- c('NÃO','SIM')#categorias de democr.
plot(bratest$Mulher,bratest$p25st, ylab="Democracia sem partidos?", xlab="Sexo", main="Democracia e Gênero")

# com ggpglot (já instalar ele quando baixaram o tidyverse)
# ver arquivo ggplot útil

# regressão
# por fim , uma regressãozinha

#vamo testar p25st frente a nossas seis variáveis independentes

# transformar p25st em variável factor para tornar apata
bra$p25st <- as.factor(bra$p25st)#transformar em fatorial
levels(bra$p25st) <- c('0','1')#categorias de democr.
# 1 é democracia sem partidos, então vamos ver o que determina essa atitude
bra$Mulher <- as.factor(bra$Mulher)
modelo1 <- glm(p25st ~ idade + Mulher + nivel_educacional+
                 ideologia+praticante+classe_social_subjetiva,
               data = bra, family=binomial(link=logit))
library(sjPlot)# pacote que mostra os modelos de forma amigável
tab_model(modelo1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#mulher passa a ter efeito significativo
#(OR-1)*100
(0.74-1)*100 # se mantido tudo constante, ser mulher reduz em 26% a chance de ser a favor de democracia sem partidos
(2.95-1)*100 # se mantido tudo constante, ter nivel 6 de escolaridade aumemta em 195% comparado ao nível 1 de escolaridade
library(jtools)# pacte maravilhosoooooo
plot1 <- plot_summs(modelo1, 
                         model.names = c("modelo 1"), legend.title = "",
                         inner_ci_level = .9,
                         point.shape = FALSE)

plot1
library(ggeffects)

p <- ggpredict(modelo1, c("Mulher"))
plot(p)
p

p2 <- ggpredict(modelo1, c("nivel_educacional"))
plot(p2)


p3 <- ggpredict(modelo1, c("nivel_educacional", "Mulher"))
plot(p3)

# finalizamos, na nossa pesquisa trabalhamos com análise fatorial e depois com regressoes, se quiserem os códigos e só me pedir
# gregoriosilva1986@gmail.com





