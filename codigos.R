prob=predict(fit,type="response")
croc=roc(lendario~prob)
plot(croc,xlab="Especificidade",ylab="Sensitividade")
polygon(c(croc$specificities,0,0),c(croc$sensitivities,0,1),
        col="aquamarine")
polygon(c(0,1,1,0),c(0,0,1,1))
abline(1,-1)
text(0.7,0.8,"Área sobre a\n curva: 0,9871")
```



## Critério de seleção

```{r class,include=F}
classificacao=function(prob){
  ifelse(fitted(fit)>prob,1,0)
}
class2=function(prob){
  sum(diag(table(classificacao(prob),lendario))/c(675,-46))
}
#uniroot(class2,c(0,0.2))
#xtable(table(classificacao(0.1461),lendario))
```

O critério de seleção para classificar um *Pokémon* como lendário foi de deixar a especificidade e a sensibilidade com valores iguais ou próximos, tendo como resultado a probabilidade 0,1461, ou seja, se um *Pokémon* tiver probabilidade estimada de ser lendário maior que 0,1461, ele será classificado lendário, caso contrário, ele seria classificado como comum. A Tabela \ref{tab:class} mostra a matriz de confusão da classificação pela raridade utilizando esse critério e os valores entre parênteses indicam a porcentagem dos *Pokémon* classificados por raridade. A probabilidade de acerto é de quase 96\%, indicando que esse critério possui uma taxa baixa de falsos positivos e falsos negativos. Apenas Entei (número 244) e Terrakion (número 639) foram os lendários classificados incorretamente.

\begin{table}[H]
\centering
\begin{tabular}{lccc}
\hline
& &   \multicolumn{2}{c}{Raridade} \\ \cmidrule{3-4}
&  & Comum & Lendário  \\ 
\hline
\multirow{2}{*}{Classificação}  & Comum & 647 (95,85\%) & 2 (4,35\%) \\ 
& Lendário & 28 (4,15\%) &  44 (95,65\%) \\ 
\hline
& Total & 675 (100\%) & 46 (100\%)\\
\hline
\end{tabular}
\caption{Classificação pela raridade.\label{tab:class}}
\end{table}

```{r extras}
Primarina=c(80,74,74,126,116,60)
Kommo=c(75,110,125,100,105,85)
Lunala=c(137,113,89,137,107,97)
dados2=as.data.frame(rbind(Primarina,Kommo,Lunala))
names(dados2)=c("Vida","Ataque","Defesa","atksp",
                "defsp","Veloc.")
res <- gam(lendario~Vida+Ataque+Defesa+atksp+
             defsp+Veloc.,family=binomial)
probs=predict(res, newdata=dados2,type="response")
class=c("Comum","Lendário","Lendário")
Raridade=c("Comum","Comum","Lendário")
# xtable(cbind(probs,class,Raridade))
# sum(probs[2]>fitted(fit)[lendario==1])
```

Foram escolhidos três *Pokémon* da sétima geração, portanto não estão presentes no banco de dados, para testar se o funcionamento da classificação, como mostra a Tabela \ref{tab:extras}. Primarina, que é comum, e Lunala, lendária, foram classificadas corretamente, entretanto Kommo-o, comum, foi classificado incorretamente, tendo sua probabilidade estimada bem mais alta que muitos lendários. Essa probabilidade estimada pode ser entendida também como um critério de escolha para o jogador, por exemplo, se ele tem uma vaga disponível em sua equipe e ele quer escolher entre Primarina ou Kommo-o, usando a regressão logística, ele poderia optar por Kommo-o já que, mesmo sendo comum, ele é tão forte quanto muitos lendários.

\begin{table}[H]
\centering
\begin{tabular}{lrrr}
\hline
Nome & Probabilidade & Classificação & Raridade \\ 
\hline
Primarina & 0,0250 & Comum & Comum \\ 
Kommo-o & 0,5314 & Lendário & Comum \\ 
Lunala & 0,9696 & Lendário & Lendário \\ 
\hline
\end{tabular}
\caption{Probabilidade estimada, classificação e a raridade verdadeira de três \textit{Pokémon} adicionais.\label{tab:extras}}
\end{table}





# Conclusão
Neste trabalho foi possível verificar as estatísticas que mais influenciam na probabilidade de um *Pokémon* ser lendário ou não a partir de uma regressão logística cuja finalidade é de estimar tal probabilidade, que foi igual a 0,1461, e também servir de critério de escolha para jogadores que estejam com dúvidas na escolha de sua equipe.

# Referências

\begin{itemize}

\item Notas de aula;\\

\item Link: \textcolor{blue}{https://web.stanford.edu/class/archive/stats/stats200/stats200.1172/Lecture26.pdf}, acesso em 03/12/2020;\\

\item Link: \textcolor{blue}{
\url{https://www.ime.usp.br/~giapaula/envel_bino}}, acesso em 03/12/2020.\\

\item Link: \textcolor{blue}{
\url{https://www.ime.usp.br/~giapaula/diag_bino_bino}}, acesso em 03/12/2020.\\

\item Link: \textcolor{blue}{https://www.kaggle.com/abcsds/pokemon}, acesso em 03/12/2020;\\

\item Link: \textcolor{blue}{https://bulbapedia.bulbagarden.net/wiki/Primarina}, acesso em 03/12/2020;\\

\item Link: \textcolor{blue}{https://bulbapedia.bulbagarden.net/wiki/Kommo-o}, acesso em 03/12/2020;\\

\item Link: \textcolor{blue}{https://bulbapedia.bulbagarden.net/wiki/Lunala}, acesso em 03/12/2020.
\end{itemize}


# Anexos

\begin{verbatim}

## Carregando os pacotes
library(corrplot)
library(dplyr)
library(xtable)
library(mgcv)
library(pROC)

## Carregando o banco de dados
pokemon=read.csv("Pokemon.csv")

## Escolhendo as variáveis
status=pokemon%>%
select(HP,Attack,Defense,Sp..Atk,Sp..Def,Speed)
status=apply(status,2,function(x){tapply(x,pokemon$X.,mean)})
lendario=ifelse(pokemon$Legendary=="True",1,0)
lendario=tapply(lendario,pokemon$X.,mean)
colnames(status)=c("Vida","Ataque","Defesa","Atq. Esp.",
"Def. Esp.","Veloc.")
status=as.data.frame(status)


## Gráficos de pares e correlação
names(status)=c("Vida","Ataque","Defesa","Atq. Esp.","Def. Esp.","Veloc.")
pairs(status,col=ifelse(lendario==0,"red","blue"),oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright",fill=c("red","blue"),legend=c("Comum","Lendário"))
cors=cor(status)
corrplot.mixed(cors)

## Boxplots
boxplot(status$Vida~lendario,xlab="Raridade",ylab="Vida",
names=c("Comum","Lendário"),ylim=c(0,255),col="red")
boxplot(status$Ataque~lendario,xlab="Raridade",ylab="Ataque",
names=c("Comum","Lendário"),ylim=c(0,255),col="darkorange2")
boxplot(status$Defesa~lendario,xlab="Raridade",ylab="Defesa",
names=c("Comum","Lendário"),ylim=c(0,255),col="gold1")
boxplot(status$`Atq. Esp.`~lendario,xlab="Raridade",ylab="Ataque Especial",
names=c("Comum","Lendário"),ylim=c(0,255),col="dodgerblue2")
boxplot(status$`Def. Esp.`~lendario,xlab="Raridade",ylab="Defesa Especial",
names=c("Comum","Lendário"),ylim=c(0,255),col="green3")
boxplot(status$Veloc.~lendario,xlab="Raridade",ylab="Velocidade",
names=c("Comum","Lendário"),ylim=c(0,255),col="deeppink1")

## Análises individuais
loginv=function(x){
exp(x)/(1+exp(x))
}
attach(status)
fitnulo=glm(lendario~1,family=binomial)

### Vida
lmt.cls <- c(min(status$Vida)-0.5,seq(min(status$Vida[lendario==1]),
max(status$Vida),len=5))
classes <- cut(status$Vida, breaks=lmt.cls)
nn <- tapply(lendario, classes, length)
pt.med <- tapply(status$Vida, classes, mean)
yy <- round(tapply(lendario, classes, sum), 2)
prop <- round(tapply(lendario, classes, mean), 2)
logito <- log((yy + 0.5)/(nn - yy + 0.5)) 

fitVida=glm(lendario~Vida,family=binomial)
xtable(summary(fitVida))
xtable(anova(fitnulo,fitVida,test="Chisq"))

plot(pt.med, logito, xlab="Vida", 
ylab="Logito da probabilidade de ser lendário", pch=16, cex=1.2)
res <- gam(lendario ~ Vida, family=binomial(link=logit),data=status)
xvals <- seq(min(status$Vida), 255, .1)
pvals <- predict(res, newdata=data.frame(Vida=xvals), type="link",se.fit=T)
lines(x=xvals, y=pvals$fit, lty=2)
lines(x=xvals, y=pvals$fit-1.96*pvals$se, col="blue")
lines(x=xvals, y=pvals$fit+1.96*pvals$se, col="blue")
legend("bottomright",legend=c("Estimativa Pontual",
"Limites (95% de confiança)"),
col=c("black","blue"),lty=2)

plot(pt.med, prop, xlab="Vida", 
ylab="Probabilidade de ser lendário", pch=16, cex=1.2, ylim=c(0,1))
lines(x=xvals, y=loginv(pvals$fit), lty=2)
lines(x=xvals, y=loginv(pvals$fit-1.96*pvals$se), lty=2, col="blue")
lines(x=xvals, y=loginv(pvals$fit+1.96*pvals$se), lty=2, col="blue")
legend("topleft",legend=c("Estimativa Pontual",
"Limites (95% de confiança)"), 
col=c("black","blue"),lty=2)

### Ataque
lmt.cls <- c(min(status$Ataque)-0.5,seq(min(status$Ataque[lendario==1]),max(status$Ataque),len=5))
classes <- cut(status$Ataque, breaks=lmt.cls)
nn <- tapply(lendario, classes, length)
pt.med <- tapply(status$Ataque, classes, mean)
yy <- round(tapply(lendario, classes, sum), 2)
prop <- round(tapply(lendario, classes, mean), 2)
logito <- log((yy + 0.5)/(nn - yy + 0.5))

fitataque=glm(lendario~Ataque,family=binomial)
xtable(summary(fitataque))
xtable(anova(fitnulo,fitataque,test="Chisq"))
plot(pt.med, logito, xlab="Ataque", 
     ylab="Logito da probabilidade de ser lendário", pch=16, cex=1.2)
res <- gam(lendario ~ Ataque, family=binomial(link=logit),data=status)
xvals <- seq(min(status$Ataque), 255, .1)
pvals <- predict(res, newdata=data.frame(Ataque=xvals), type="link",se.fit=T)
lines(x=xvals, y=pvals$fit, lty=2)
lines(x=xvals, y=pvals$fit-1.96*pvals$se, col="blue")
lines(x=xvals, y=pvals$fit+1.96*pvals$se, col="blue")
legend("bottomright",legend=c("Estimativa Pontual","Limites (95% de confiança)"), 
col=c("black","blue"),lty=2)

plot(pt.med, prop, xlab="Ataque", 
     ylab="Probabilidade de ser lendário", pch=16, cex=1.2, ylim=c(0,1))
lines(x=xvals, y=loginv(pvals$fit), lty=2)
lines(x=xvals, y=loginv(pvals$fit-1.96*pvals$se), lty=2, col="blue")
lines(x=xvals, y=loginv(pvals$fit+1.96*pvals$se), lty=2, col="blue")
legend("topleft",legend=c("Estimativa Pontual","Limites (95% de confiança)"), 
col=c("black","blue"),lty=2)

### Defesa
lmt.cls <- c(min(status$Defesa)-0.5,seq(min(status$Defesa[lendario==1]),max(status$Defesa),len=5))
classes <- cut(status$Defesa, breaks=lmt.cls)
nn <- tapply(lendario, classes, length)
pt.med <- tapply(status$Defesa, classes, mean)
yy <- round(tapply(lendario, classes, sum), 2)
prop <- round(tapply(lendario, classes, mean), 2)
logito <- log((yy + 0.5)/(nn - yy + 0.5)) 

fitdefesa=glm(lendario~Defesa,family=binomial)
xtable(summary(fitdefesa))
xtable(anova(fitnulo,fitdefesa,test="Chisq"))

plot(pt.med, logito, xlab="Defesa", 
     ylab="Logito da probabilidade de ser lendário", pch=16, cex=1.2)
res <- gam(lendario ~ Defesa, family=binomial(link=logit),data=status)
xvals <- seq(min(status$Defesa), 255, .1)
pvals <- predict(res, newdata=data.frame(Defesa=xvals), type="link",se.fit=T)
lines(x=xvals, y=pvals$fit, lty=2)
lines(x=xvals, y=pvals$fit-1.96*pvals$se, col="blue")
lines(x=xvals, y=pvals$fit+1.96*pvals$se, col="blue")
legend("bottomright",legend=c("Estimativa Pontual","Limites (95% de confiança)"), 
col=c("black","blue"),lty=2)

plot(pt.med, prop, xlab="Defesa", 
     ylab="Probabilidade de ser lendário", pch=16, cex=1.2, ylim=c(0,1))
lines(x=xvals, y=loginv(pvals$fit), lty=2)
lines(x=xvals, y=loginv(pvals$fit-1.96*pvals$se), lty=2, col="blue")
lines(x=xvals, y=loginv(pvals$fit+1.96*pvals$se), lty=2, col="blue")
legend("topleft",legend=c("Estimativa Pontual","Limites (95% de confiança)"), 
col=c("black","blue"),lty=2)

### Ataque Especial
lmt.cls <- c(min(status$`Atq. Esp.`)-0.5,
             seq(min(status$`Atq. Esp.`[lendario==1]),max(status$`Atq. Esp.`),len=5))
classes <- cut(status$`Atq. Esp.`, breaks=lmt.cls)
nn <- tapply(lendario, classes, length)
pt.med <- tapply(status$`Atq. Esp.`, classes, mean)
yy <- round(tapply(lendario, classes, sum), 2)
prop <- round(tapply(lendario, classes, mean), 2)
logito <- log((yy + 0.5)/(nn - yy + 0.5)) 

atksp=`Atq. Esp.`

fitatksp=glm(lendario~atksp,family=binomial)
xtable(summary(fitatksp))
xtable(anova(fitnulo,fitatksp,test="Chisq"))

plot(pt.med, logito, xlab="Ataque Especial", 
     ylab="Logito da probabilidade de ser lendário", pch=16, cex=1.2)
res <- gam(lendario ~ atksp, family=binomial(link=logit))
xvals <- seq(min(status$`Atq. Esp.`), 255, .1)
pvals <- predict(res, newdata=data.frame(atksp=xvals), type="link",se.fit=T)
lines(x=xvals, y=pvals$fit, lty=2)
lines(x=xvals, y=pvals$fit-1.96*pvals$se, col="blue")
lines(x=xvals, y=pvals$fit+1.96*pvals$se, col="blue")
legend("bottomright",legend=c("Estimativa Pontual","Limites (95% de confiança)"),
col=c("black","blue"),lty=2)

plot(pt.med, prop, xlab="Ataque Especial", 
     ylab="Probabilidade de ser lendário", pch=16, cex=1.2, ylim=c(0,1))
lines(x=xvals, y=loginv(pvals$fit), lty=2)
lines(x=xvals, y=loginv(pvals$fit-1.96*pvals$se), lty=2, col="blue")
lines(x=xvals, y=loginv(pvals$fit+1.96*pvals$se), lty=2, col="blue")
legend("topleft",legend=c("Estimativa Pontual","Limites (95% de confiança)"), 
col=c("black","blue"),lty=2)

### Defesa Especial
lmt.cls <- c(min(status$`Def. Esp.`)-0.5,
             seq(min(status$`Def. Esp.`[lendario==1]),max(status$`Def. Esp.`),len=5))
classes <- cut(status$`Def. Esp.`, breaks=lmt.cls)
nn <- tapply(lendario, classes, length)
pt.med <- tapply(status$`Def. Esp.`, classes, mean)
yy <- round(tapply(lendario, classes, sum), 2)
prop <- round(tapply(lendario, classes, mean), 2)
logito <- log((yy + 0.5)/(nn - yy + 0.5)) 


defsp=`Def. Esp.`
fitdefsp=glm(lendario~defsp,family=binomial)
xtable(summary(fitdefsp))
xtable(anova(fitnulo,fitdefsp,test="Chisq"))

plot(pt.med, logito, xlab="Defesa Especial", 
     ylab="Logito da probabilidade de ser lendário", pch=16, cex=1.2)
res <- gam(lendario ~ defsp, family=binomial(link=logit))
xvals <- seq(min(status$`Def. Esp.`), 255, .1)
pvals <- predict(res, newdata=data.frame(defsp=xvals), type="link",se.fit=T)
lines(x=xvals, y=pvals$fit, lty=2)
lines(x=xvals, y=pvals$fit-1.96*pvals$se, col="blue")
lines(x=xvals, y=pvals$fit+1.96*pvals$se, col="blue")
legend("bottomright",legend=c("Estimativa Pontual","Limites (95% de confiança
col=c("black","blue"),lty=2)

plot(pt.med, prop, xlab="Defesa Especial", 
     ylab="Probabilidade de ser lendário", pch=16, cex=1.2, ylim=c(0,1))
lines(x=xvals, y=loginv(pvals$fit), lty=2)
lines(x=xvals, y=loginv(pvals$fit-1.96*pvals$se), lty=2, col="blue")
lines(x=xvals, y=loginv(pvals$fit+1.96*pvals$se), lty=2, col="blue")
legend("topleft",legend=c("Estimativa Pontual","Limites (95% de confiança)"),
col=c("black","blue"),lty=2)

### Velocidade
lmt.cls <- c(min(status$Veloc.)-0.5,
             seq(min(status$Veloc.[lendario==1]),max(status$Veloc.),len=5))
classes <- cut(status$Veloc., breaks=lmt.cls)
nn <- tapply(lendario, classes, length)
pt.med <- tapply(status$Veloc., classes, mean)
yy <- round(tapply(lendario, classes, sum), 2)
prop <- round(tapply(lendario, classes, mean), 2)
logito <- log((yy + 0.5)/(nn - yy + 0.5)) 

fitveloc=glm(lendario~Veloc.,family=binomial)
xtable(summary(fitveloc))
xtable(anova(fitnulo,fitveloc,test="Chisq"))

plot(pt.med, logito, xlab="Velocidade", 
     ylab="Logito da probabilidade de ser lendário", pch=16, cex=1.2)
res <- gam(lendario ~ Veloc., family=binomial(link=logit))
xvals <- seq(min(status$Veloc.), 255, .1)
pvals <- predict(res, newdata=data.frame(Veloc.=xvals), type="link",se.fit=T)
lines(x=xvals, y=pvals$fit, lty=2)
lines(x=xvals, y=pvals$fit-1.96*pvals$se, col="blue")
lines(x=xvals, y=pvals$fit+1.96*pvals$se, col="blue")
legend("bottomright",legend=c("Estimativa Pontual","Limites (95% de confiança)"), 
col=c("black","blue"),lty=2)

plot(pt.med, prop, xlab="Velocidade", 
     ylab="Probabilidade de ser lendário", pch=16, cex=1.2, ylim=c(0,1))
lines(x=xvals, y=loginv(pvals$fit), lty=2)
lines(x=xvals, y=loginv(pvals$fit-1.96*pvals$se), lty=2, col="blue")
lines(x=xvals, y=loginv(pvals$fit+1.96*pvals$se), lty=2, col="blue")
legend("topleft",legend=c("Estimativa Pontual","Limites (95% de confiança)"),
col=c("black","blue"),lty=2)

## Modelo
fit=glm(lendario~.,family=binomial,data=status)
fit2=glm(lendario~.-Ataque,family=binomial,data=status)
summary(fit)
anova(fit2,fit,test="Chisq")
anova(fitnulo,fit,test="Chisq")
xtable(summary(fit))
xtable(anova(fit2,fit,test="Chisq"))
xtable(anova(fitnulo,fit,test="Chisq"))

## Análise de Resíduos
diag_bino=function(fit.model){
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  ts <- resid(fit.model,type="pearson")/sqrt(1-h)
  td <- resid(fit.model,type="deviance")/sqrt(1-h)
  di <- (h/(1-h))*(ts^2)
  a <- max(td)
  b <- min(td)
  par(mfrow=c(2,2))
  plot(fitted(fit.model),h,xlab="Valor Ajustado", 
       ylab="Medida h", pch=16)
  identify(fitted(fit.model), h, n=1)
  #
  plot(di,xlab="Índice", ylab="Distância de Cook",pch=16)
  #identify(di, n=1)
  #
  plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
       ylim=c(b-1,a+1), pch=16)
  abline(2,0,lty=2)
  abline(-2,0,lty=2)
  identify(td, n=4)
  #
  plot(fitted(fit.model), td,xlab="Valor Ajustado", 
       ylab="Resíduo Componente do Desvio", pch=16)
  #identify(fitted(fit.model), td, n=1)
  par(mfrow=c(1,1))
}
diag_bino(fit)

## Envelope
envel_bino=function(fit.model){
  par(mfrow=c(1,1))
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  td <- resid(fit.model,type="deviance")/sqrt(1-h)
  e <- matrix(0,n,100)
  #
  for(i in 1:100){
    dif <- runif(n) - fitted(fit.model)
    dif[dif >= 0 ] <- 0
    dif[dif<0] <- 1
    nresp <- dif
    fit <- glm(nresp ~ X, family=binomial)
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
  #
  e1 <- numeric(n)
  e2 <- numeric(n)
  #
  for(i in 1:n){
    eo <- sort(e[i,])
    e1[i] <- (eo[2]+eo[3])/2
    e2[i] <- (eo[97]+eo[98])/2}
  #
  med <- apply(e,1,mean)
  faixa <- range(td,e1,e2)
  par(pty="s")
  qqnorm(td,xlab="Percentil da N(0,1)",
         ylab="Componente do Desvio", ylim=faixa, main="")
  #
  par(new=TRUE)
  #
  qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="",col="blue")
  par(new=TRUE)
  qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="",col="blue")
  par(new=TRUE)
  qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
  
}
envel_bino(fit)

## Curva ROC
prob=predict(fit,type="response")
croc=roc(lendario~prob)
plot(croc,xlab="Especificidade",ylab="Sensitividade")
polygon(c(croc$specificities,0,0),c(croc$sensitivities,0,1),
        col="aquamarine")
polygon(c(0,1,1,0),c(0,0,1,1))
abline(1,-1)
text(0.7,0.8,"Área sobre a\n curva: 0,9871")

## Tabela para usar o critério de classificação
classificacao=function(prob){
ifelse(fitted(fit)>prob,1,0)
}

## Encontrar o valor que deixa as taxas de acertos iguais
class2=function(prob){
sum(diag(table(classificacao(prob),lendario))/c(735,-65))
}
uniroot(class2,c(0,0.2))
xtable(table(classificacao(0.1461),lendario))
