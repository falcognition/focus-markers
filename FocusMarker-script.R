# Context/request: 0ms-3500ms--T/C vs D1/D2 w/participant as rfact

df <- v2.data
attach(df)
modelov2nom <- lmer(Tiempo ~ Juntas + (1 | Nombre))
summary(modelov2nom)
anova(modelov2nom)
emmeans(modelov2nom, pairwise ~ Juntas)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
AIC(modelov2nom)
detach(df)


# Context/request: 0ms-3500ms--T/C vs D1/D2   w/participant and item as rfact

df <- v2.data
attach(df)
modelov2nomit <- lmer(Tiempo ~ Juntas + (1 | Nombre) + (1 | Item))
summary(modelov2nomit)
anova(modelov2nomit)
emmeans(modelov2nomit, pairwise ~ Juntas)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
AIC(modelov2nomit)
detach(df)

# Context/request: 0ms-3500ms--T/C vs D1/D2   w/participant + item + Age as rfact

df <- v2.data
attach(df)
modelov2nomited <- lmer(Tiempo ~ Juntas + (1 | Nombre) + (1 | Item) + (1| Edad))
summary(modelov2nomited)
anova(modelov2nomited)
emmeans(modelov2nomited, pairwise ~ Juntas)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
AIC(modelov2nomited)
detach(df)

# PARA CAMBIAR LA CATEGORÍA DE REFERENCIA
df <- v2.data
attach(df)
table(TCDD)
TCDD <- relevel(TCDD, ref = "T")
detach(df)


# Context/request: 0ms-3500ms----T-C-D1/D2   w/participant as rfact
df <- v2.data
attach(df)
modelov2Anom <- lmer(Tiempo ~ TCDD + (1 | Nombre))
summary(modelov2Anom)
anova(modelov2Anom)
emmeans(modelov2Anom, pairwise ~ TCDD)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ TCDD * MD)
AIC(modelov2Anom)
detach(df)

# Context/request: 0ms-3500ms----T-C-D1/D2   w/participant and item as rfact
df <- v2.data
attach(df)
modelov2Anomit <- lmer(Tiempo ~ TCDD + (1 | Nombre) + (1 | Item))
summary(modelov2Anomit)
anova(modelov2Anomit)
emmeans(modelov2Anomit, pairwise ~ TCDD)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ TCDD * MD)
AIC(modelov2Anomit)
detach(df)



# Yes: 3500ms-4000ms T--C--D1/D2
# PARA CAMBIAR LA CATEGORIA DE REFERENCIA
df <- v3.data
attach(df)
table(TCDD)
TCDD <- relevel(TCDD, ref = "c")
MD <- relevel(MD, ref = "s")


# Yes: 3500ms-4000ms T--C--D1/D2   w/particioant as rfact
df <- v3.data
attach(df)
modelov3DDnom <- lmer(Tiempo ~ TCDD * MD + (1 | Nombre))
summary(modelov3DDnom)
anova(modelov3DDnom)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov3DDnom, pairwise ~ TCDD * MD)
AIC(modelov3DDnom)
detach(df)

#Yes: 3500ms-4000ms T--C--D1/D2 -- w/participant and item as rfact
df <- v3.data
attach(df)
modelov3DDnomit <- lmer(Tiempo ~ TCDD * MD + (1 | Nombre) + (1 | Item))
summary(modelov3DDnomit)
anova(modelov3DDnomit)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov3DDnomit, pairwise ~ TCDD * MD)
AIC(modelov3DDnomit)
detach(df)



#Focus operator: 4000ms-4650ms T--C--D1/D2-- w/participant and item as rfact
df <- v4ti.data
attach(df)
modelov4ti <- lmer(Tiempo ~ MD * Imagen + (1 | Nombre) + (1 | Item))
summary(modelov4ti)
anova(modelov4ti)
boxplot(Tiempo ~ MD * Imagen)
boxplot(Tiempo ~ Imagen)
emmeans(modelov4ti, pairwise ~ MD * Imagen)
AIC(modelov4ti)
detach(df)

#Reply to request: 4650ms-6700ms T--C--D1/D2-- w/participant as rfact
df <- v5.data
attach(df)
modelov5nom <- lmer(Tiempo ~  MD * TCDD + (1 | Nombre))
summary(modelov5nom)
anova(modelov5)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov5nom, pairwise ~ TCDD * MD)
AIC(modelov5nom)


#Target named: 6700ms-9500ms T--C--D1/D2-- w/participant as rfact 
df <- v6.data
attach(df)
modelov6nom <- lmer(Tiempo ~  MD * TCDD + (1 | Nombre))
summary(modelov6nom)
anova(modelov6nom)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov6nom, pairwise ~ TCDD * MD)
AIC(modelov6nom)
detach(df)

