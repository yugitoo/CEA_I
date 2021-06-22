require(robustbase)
require(MASS)
#ratosgee = read.table(dados_organizados.xlsx, header=TRUE)
dados<-read_excel("dados_organizados.xlsx")
dados$data = factor(dados$data)
#criar estacoes
dados['estacoes'] <- rep(c('outono', 'outono', 'outono', 'inverno', 'inverno', 'primavera', 
                           'primavera', 'verao', 'verao', 'verao', 'outono', 'inverno', 'inverno', 'primavera', 'primavera'), 6)

dados
#levels(dados$estacoes) = c("primavera","verao","outono","inverno")

attach(dados)
estacoes
require(ggplot2)
# = ggplot(ratosgee, aes(x=time, y=celulas, group=id))
#p + geom_line(colour="blue", size=1) + theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15)) 

ajuste1.dados = glm(`Estágio I` ~ estacoes+P4+E2  , family= poisson)
ajuste2.dados = glm(`Estágio II` ~ estacoes+P4+E2  , family=poisson)
ajuste3.dados = glm(`Estágio III` ~ estacoes+P4+E2  , family=poisson)
ajuste4.dados = glm(`Estágio IV` ~ estacoes+P4+E2  , family=poisson)
summary(ajuste1.dados)
summary(ajuste2.dados)
summary(ajuste3.dados)
summary(ajuste4.dados)
fit.model = ajuste1.dados
fit.model=
source("Código Envelope")
source("envel_pois")
source("diag_cook_pois")

require(gamlss)
ajuste1_dados = gamlss(`Estágio I` ~ estacoes+P4+E2  , family=PO)
summary(ajuste1_dados)
plot(ajuste1_dados)

ajuste2_dados = gamlss(`Estágio II` ~ estacoes+P4+E2  , family=PO)
summary(ajuste2_dados)
plot(ajuste2_dados)

ajuste3_dados = gamlss(`Estágio III` ~ estacoes+P4+E2  , family=PO)
summary(ajuste3_dados)
plot(ajuste3_dados)

ajuste4_dados = gamlss(`Estágio IV` ~ estacoes+P4+E2  , family=PO)
summary(ajuste4_dados)
plot(ajuste4_dados)
 
wp(ajuste1_dados)
wp(ajuste2_dados)
wp(ajuste3_dados)
wp(ajuste4_dados)
#estagio*p4+estagio*e2+p4*e2,stepAIC() 
aj1<-glm(`Estágio I`~P4+ E2 +E2*P4)
stepAIC(aj1)

aj2<-glm(`Estágio II`~P4+ E2 +E2*P4)
stepAIC(aj2)

aj3<-glm(`Estágio III`~P4+ E2 +E2*P4)
stepAIC(aj3)

aj4<-glm(`Estágio IV`~P4+ E2 +E2*P4)
stepAIC(aj4)

##################################################################################

ajust1.dado = glm.nb(`Estágio I` ~ estacoes+P4+E2 )
ajust11.dado=glm.nb(`Estágio II` ~ estacoes+P4+E2+P4*E2)
ajust2.dado = glm.nb(`Estágio II` ~ estacoes+P4+E2 )
ajust3.dado = glm.nb(`Estágio III` ~ estacoes+P4+E2 )
ajust4.dado = glm.nb(`Estágio IV` ~ estacoes+P4+E2)
summary(ajust1.dado)
summary(ajust2.dado)
summary(ajust3.dado)
summary(ajust4.dado)

ajubn1.dados=gamlss(`Estágio I` ~ estacoes+P4+E2,family = NBI)
summary(ajubn1.dados)
plot(ajubn1.dados)
wp(ajubn1.dados)

ajubn2.dados=gamlss(`Estágio II` ~ estacoes+P4+E2,family = NBI)
summary(ajubn2.dados)
plot(ajubn2.dados)
wp(ajubn2.dados)

ajubn3.dados=gamlss(`Estágio III` ~ estacoes+P4+E2,family = NBI)
summary(ajubn3.dados)
plot(ajubn3.dados)
wp(ajubn3.dados)


ajubn4.dados=gamlss(`Estágio IV` ~ estacoes+P4+E2,family = NBI)
summary(ajubn4.dados)
plot(ajubn4.dados)
wp(ajubn4.dados)


ajbn1.dados=gamlss(`Estágio I` ~ estacoes+P4+E2,family = NBI)
summary(ajubn4.dados)
plot(ajubn4.dados)
wp(ajubn4.dados)
