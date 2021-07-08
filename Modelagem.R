## Modelagem 
### Bibliotecas ----
library(glmm)
library(glmmTMB)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library("summarytools")
library(dplyr)
library(tidyr)
library(nlme)
library(lme4)
library(gamlss)
require(dglm)
require(MASS)
library(effects)
library(sjPlot)
library(lmtest)
require(pscl)

#remotes::install_github("nyiuab/NBZIMM")
#devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
#dependencies = T, build_vignettes = T)
library(DHARMa)


### Dados ----
dados<- read_excel("dados_organizados.xlsx")

dados<- data.frame(dados)
dados<-dados[, c(-3)]

dados$data<- as.Date(dados$data)



#### Criando a variavel de estacoes do ano ----
estacoes <- rep(c('outono', 'outono', 'outono', 'inverno', 'inverno', 'primavera', 
                  'primavera', 'verao', 'verao', 'verao', 'outono', 'inverno', 'inverno', 'primavera', 'primavera'), 6)
dados['estacoes']<-factor(estacoes)

View(dados)

est <- dados[, c("E2", "P4","Estagio.I","Estagio.II","Estagio.III", 
                 "Estagio.IV","data", "estacoes", "femea")]



##################################################################

### Modelagem -  Estagio 1 ----
#### Estagio 1 - Inverno de referencia ----
est$estacoes = relevel(est$estacoes, ref = "inverno") # define a casela de referencia
fit1_inverno <- glmer.nb(Estagio.I ~ estacoes+E2+ estacoes:E2 +(1|femea), data=na.omit(est))
summary(fit1_inverno)

getME(fit1_inverno, 'glmer.nb.theta')

#### Estagio 1 - Anova ----
car::Anova(fit1_inverno)

#### Estagio 1 - Residuos Dharma ----
simulationOutput <- simulateResiduals(fittedModel=fit1_inverno, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

#### Estagio 1 - Outono de referencia ----
est$estacoes = relevel(est$estacoes, ref = "outono")
fit1_outono <- glmer.nb(Estagio.I ~ estacoes+E2+ estacoes:E2 +(1|femea), data=na.omit(est))
summary(fit1_outono)
getME(fit1_outono, 'glmer.nb.theta')

#### Estagio 1 - Primavera de referencia ----
est$estacoes = relevel(est$estacoes, ref = "primavera")
fit1_primavera <- glmer.nb(Estagio.I ~ estacoes+E2+ estacoes:E2 +(1|femea), data=na.omit(est))
summary(fit1_primavera)

#### Estagio 1 - Verao de referencia ----
est$estacoes = relevel(est$estacoes, ref = "verao")
fit1_verao <- glmer.nb(Estagio.I ~ estacoes+E2+ estacoes:E2 +(1|femea), data=na.omit(est))
summary(fit1_verao)

#### Estagio 1 - exponecializa e transforma em tabela ----
#### inverno ----
tab_model(fit1_inverno, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

##### Outono ----
tab_model(fit1_outono, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

##### primavera ----
tab_model(fit1_primavera, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

##### verao ----
tab_model(fit1_verao, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

#######################################################
### Modelagem -  Estagio 2 ----
est$estacoes = relevel(est$estacoes, ref = "inverno")
fit2_inverno <- glmer.nb(Estagio.II ~ E2 + estacoes+  (1|femea), data=na.omit(est))
summary(fit2_inverno)
getME(fit2_inverno, 'glmer.nb.theta')

#### Estagio 2 - Anova ----
car::Anova(fit2_inverno)


#######################################################
#agrupando estagios III e IV

est['inv_prim'] = (ifelse(est$estacoes=='verao' | est$estacoes=='outono', 0, 1))

###### para Estagio III:######
est$estacoes = relevel(est$estacoes, ref = "verao")
fit3_completo <- glmmTMB(Estagio.III ~ estacoes+E2+P4 +(1 | femea),
                         data=na.omit(est), 
                         family=nbinom1(link="log"))
summary(fit3_completo)
confint(fit3_completo,parm="theta_")

est$estacoes = relevel(est$estacoes, ref = "inverno")
fit3_reduzido <- glmmTMB(Estagio.III ~ inv_prim+E2+P4 +(1 | femea),
                         data=na.omit(est), 
                         family=nbinom1(link="log"))
summary(fit3_reduzido)
confint(fit3_reduzido,parm="theta_")

#### Estagio 3 -  teste razao de verossimilhanca ----
lrtest( fit3_reduzido, fit3_completo)
## p valor alto -> usar o modelo reduzido

#### Estagio 3 - Residuos Dharma ----
simulationOutput <- simulateResiduals(fittedModel=fit3_reduzido, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)



#### Estagio 3 - exponecializa e transforma em tabela ----
# modelo completo
tab_model(fit3_completo, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

# modelo reduzido
tab_model(fit3_reduzido, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)


#########################################################################
#### para Estagio IV: #####

#### Estagio 4 - modelo completo ----

est$estacoes = relevel(est$estacoes, ref = "verao")
fit4_completo <- glmer.nb(Estagio.IV ~ estacoes+E2+P4 + (1 | femea),
                          data=na.omit(est))
summary(fit4_completo)
getME(fit4_completo, 'glmer.nb.theta')

#### Estagio 4 - modelo reduzido ----
est$estacoes = relevel(est$estacoes, ref = "inverno")
fit4_reduzido <- glmer.nb(Estagio.IV ~ inv_prim+E2+P4 +(1 | femea),
                          data=na.omit(est))
summary(fit4_reduzido)
getME(fit4_reduzido, 'glmer.nb.theta')

#### Estagio 4 - Teste razao de verossimilhanca ----
lrtest( fit4_reduzido, fit4_completo)

#### Estagio 4 - Residuos Dharma ----
simulationOutput <- simulateResiduals(fittedModel=fit4_reduzido, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)



#### Estagio 4 - exponecializa e transforma em tabela ----
tab_model(fit4_completo, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

tab_model(fit4_reduzido, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)



####################################################
# modelagem dos hormonios ----
# banco de dados

progesterona <- dados[, c("femea","P4", 'E2', "estacoes")]

progesterona <- progesterona %>% drop_na(P4)

#######################################################

### Modelagem -  Progesterona - Inverno de referencia ----
progesterona$estacoes = relevel(progesterona$estacoes, ref = "verao")
fit5_completo <- glmer(P4~ estacoes +(1|femea), data=na.omit(progesterona),
                       family=inverse.gaussian(link="log"))
summary(fit5_completo)

### Modelagem -  Progesterona - Outono ----
progesterona['outono'] = (ifelse(progesterona$estacoes=='outono', 1, 0))
fit5_reduzido <- glmer(P4~ outono +(1|femea), data=na.omit(progesterona),
                       family=inverse.gaussian(link="log"))
summary(fit5_reduzido)

### Progesterona - teste razao de verossimilhanca ----
lrtest(fit5_completo, fit5_reduzido)

### Estradiol - Residuos Dharma ----
simulationOutput <- simulateResiduals(fittedModel=fit5_reduzido, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)


### Estradiol - exponecializa e transforma em tabela ----
tab_model(fit5_completo, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

tab_model(fit5_reduzido, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)



#######################################################
### Estradiol ----

progesterona['inverno']<-(ifelse(progesterona$estacoes=='inverno', 1, 0))
### Estradiol - modelo completo ----
progesterona$estacoes = relevel(progesterona$estacoes, ref = "verao")
fit6_completo <- glmer((E2+1)~ estacoes +(1|femea), data=na.omit(progesterona),
                       family=inverse.gaussian(link="log"), control = glmerControl(tolPwrss=1e-6))

summary(fit6_completo)

### Estradiol - Modelo reduzido ----
fit6_reduzido <- glmer((E2+1)~ inverno +(1|femea), data=na.omit(progesterona),
                       family=inverse.gaussian(link="log"), control = glmerControl(tolPwrss=1e-6))

summary(fit6_reduzido)

### Estradiol - teste razao de verossimilhanca ----
lrtest(fit6_completo, fit6_reduzido)

### Estradiol - Residuos Dharma ----
simulationOutput <- simulateResiduals(fittedModel=fit6_reduzido, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

### Estradiol - exponecializa e transforma em tabela ----

tab_model(fit6_completo, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)

tab_model(fit6_reduzido, show.se = TRUE, string.est = "Estimativa",
          string.se = "Erro Padrao", string.ci = 'Intervalo 95%', 
          show.reflvl = TRUE, digits = 3)


########################################################

