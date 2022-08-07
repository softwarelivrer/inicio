################################
## Regressão Logística Simples##
################################

# Instalando os pacotes necessários
install.packages(c("readr","ggplot2","mfx","caret","pROC","modEvA","foreign","stargazer","faraway"))


# Dados do exemplo:

# CHD: Coronary heart disease (Doença cardíaca coronária) - variável dependente - (0 não possui 1 possui)
# AGE: Idade - variável independente - (numérica)


# Carregando os dados
library(readr)
chd <- read_delim("https://github.com/Smolski/livroavancado/raw/master/cdh.csv", 
                  ";", escape_double = FALSE, col_types = cols(CHD = col_factor(levels = c())), 
                  trim_ws = TRUE)

summary(chd)
 
# Plotando o gráfico
library(ggplot2)

ggplot(chd, aes(x=AGE, y=CHD)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

# Gerando a equação
m1=glm(CHD~AGE, family = binomial(link="logit"), data = chd)

summary(m1)

library(stargazer)
stargazer(m1, title="Resultados",type = "text")

# Filtrando a idade dos indivíduos
IDADE<-chd[,1]  

# Criando campo de predição para cada idade dos indivíduos 
chd$PRED=predict(m1, newdata=IDADE, type="response")

# Plotando a probabilidade predita pelo modelo
ggplot(chd, aes(x=AGE, y=PRED)) + 
  geom_point()

## Estimando a razão de chances
library(mfx)
logitor(CHD~AGE,data = chd)

# Determinando o intervalo de confiança
exp(cbind(OR=coef(m1), confint(m1)))


## Predição de probabilidades

#Calculando a idade média
media = data.frame(AGE=mean(chd$AGE))
media

# Prevendo a probabiliade com a idade média
media$pred.prob = predict(m1, newdata=media, type="response")
media


## Matriz de Confusão

# Prevendo o modelo para os dados
library(caret)
chd$pdata <- as.factor(
  ifelse(
    chd$PRED>0.5,"1","0"))

# Criando a matriz de confusão
confusionMatrix(chd$pdata, chd$CHD, positive="1")

## Curva ROC

# Criando a curva ROC
library(pROC)
roc1 = plot.roc(chd$CHD,fitted(m1))

# plotando a curva ROC
plot(roc1,
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)

## O teste Hosmer e Lemeshow
library(ResourceSelection)
hl = hoslem.test(chd$CHD,fitted(m1),g=10)
hl

## Pseudo R$^2$


library(modEvA)
RsqGLM(m1)

##################################
## Regressão Logística Múltipla 1#
##################################

# Carregando os dados
library(haven)
mydata <- read_dta("http://dss.princeton.edu/training/Panel101.dta")
summary(mydata)

# Criando a regressão
logit=glm(y_bin~x1+x2+x3, data=mydata, family = binomial(link="logit"))
summary(logit)


# Apresentando os resultados
library(stargazer)
stargazer(logit, title="Resultados",type = "text")

# Razão de chances
library(mfx)
logitor(y_bin~x1+x2+x3,data=mydata)

# Exponenciando os coeficientes
exp(coef(logit))

# Juntando com os intervalos de confiança
exp(cbind(OR=coef(logit), confint(logit)))

# Data frame com as médias das variáveis independentes
allmean = data.frame(x1=mean(mydata$x1),
                     x2=mean(mydata$x2),
                     x3=mean(mydata$x3))

# Efetuando a previsão com o modelo
allmean$pred.prob = predict(logit, newdata=allmean, type="response")
allmean

## Método Stepwise para seleção de variáveis
# Obs: Quanto menor o AIC Melhor
step(logit, direction = 'both')


## VIF - Variance Inflation Factor: entendendo a multicolinearidade
library(faraway)
vif(logit)

# Modelo com somente 1 variável após Stepwise e VIF 
logit2=glm(y_bin~x3, data=mydata, family = binomial(link="logit"))
stargazer(logit, logit2, title="Resultados",type = "text")


##################################
## Regressão Logística Múltipla 2#
##################################

# admin: Variável dependente = 0 (não admitido) e 1 (admitido)
# Rank: Variável independente = ranking da escola de proveniência do candidato
# Gre: Variável independente = exames prévios do candidato.
# Gpa: Variável independente = exames prévios do candidato.




# Carregando o arquivo
library(readr)
binary <- read_csv("http://www.karlin.mff.cuni.cz/~pesta/prednasky/NMFM404/Data/binary.csv")

summary(binary)

# Transformando a variável rank em categórica
binary$rank <- factor(binary$rank)

# Determinando a regressão
mylogit <- glm(admit ~ gre + gpa + rank, data = binary, 
               family = binomial(link="logit"))

# Resultado
summary(mylogit)

# Teste anova - Análise de variância
anova(mylogit, test = "Chisq")

# Criação de novo modelo com base no anterior, sem a variável GRE
mylogit2=update(mylogit,~. - gre)

# Comparação das duas fórmulas 
anova(mylogit, mylogit2, test = "Chisq")

# Coeficientes exponenciados
exp(cbind(OR = coef(mylogit), confint(mylogit)))

## Predição das probabilidades

#Nova base
pred=data.frame(gre=700,
                gpa=3.67,
                rank=factor(1)
)

# Efetuando a prediçãoc om base no modelo
pred$prob=predict(mylogit, newdata=pred, type="response")

pred


# Comparando com aluno que estudou em uma escolal com ranking 4
pred=data.frame(gre=700,
                gpa=3.67,
                rank=factor(4)
)

# Aplicando o modelo
pred$prob=predict(mylogit, newdata=pred, type="response")

pred


## Predição comparando os rankings de escolas

# Criação da tabela
novosdados=with(binary,
                data.frame(gre=mean(gre),
                           gpa=mean(gpa),
                           rank=factor(1:4)))

novosdados

# Incluindo a predição dos valores
novosdados=cbind(novosdados,predict(mylogit,
                                    newdata=novosdados,
                                    type="response",
                                    se.fit=TRUE))

# Renomeando as variáveis
names(novosdados)[names(novosdados)=='fit']="prob"
names(novosdados)[names(novosdados)=='se.fit']="se.prob"

# Estimando os intervalos de confiança
novosdados$LL=novosdados$prob-1.96*novosdados$se.prob
novosdados$UL=novosdados$prob+1.96*novosdados$se.prob

# Vizualização dos dados
novosdados

library(ggplot2)
ggplot(novosdados, aes(x=rank, y=prob))+
  geom_errorbar(aes(ymin=LL, ymax=UL), width=0.2, lty=1, lwd=1, col="red")+
  geom_point(shape=18, size=5, fill="black")+
  scale_x_discrete(limits=c("1","2","3","4"))+
  labs(title="Probabilidades preditas", x="Ranking", y="Pr(y=1)")

