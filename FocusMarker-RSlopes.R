# WITH RANDOM SLOPES

# Context/request: 0ms-3500ms--T/C vs D1/D2   w/participant and item as rfact

df <- v2.data
attach(df)
modelov2nomit <- lmer(Tiempo ~ Juntas + (1 + Juntas | Nombre) + (1 + Juntas | Item))
summary(modelov2nomit)
anova(modelov2nomit)
emmeans(modelov2nomit, pairwise ~ Juntas)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
coef(modelov2nomit)
AIC(modelov2nomit)
detach(df)

# Context/request: 0ms-3500ms--T/C vs D1/D2 w/participant as rfact

df <- v2.data
attach(df)
modelov2nomRS <- lmer(Tiempo ~ Juntas + (1 + Juntas | Nombre))
summary(modelov2nomRS)
anova(modelov2nomRS)
emmeans(modelov2nomRS, pairwise ~ Juntas)
coef(modelov2nomRS)
AIC(modelov2nomRS)
detach(df)

# Context/request: 0ms-3500ms--T/C vs D1/D2   w/ item as rfact

df <- v2.data
attach(df)
modelov2it <- lmer(Tiempo ~ Juntas + (1 + Juntas | Item))
summary(modelov2it)
anova(modelov2it)
emmeans(modelov2nomit, pairwise ~ Juntas)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
coef(modelov2it)
AIC(modelov2it)
detach(df)





# PARA CAMBIAR LA CATEGORÍA DE REFERENCIA
df <- v2.data
attach(df)
table(TCDD)
TCDD <- relevel(TCDD, ref = "T")
detach(df)

# Context/request: 0ms-3500m--T-C-D1/D2   w/participant and item as rfact
df <- v2.data
attach(df)
modelov2Anomit <- lmer(Tiempo ~ TCDD + (1 + TCDD | Nombre) + (1 + TCDD | Item))
summary(modelov2Anomit)
anova(modelov2Anomit)
emmeans(modelov2Anomit, pairwise ~ TCDD)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ TCDD * MD)
coef(modelov2Anomit)
AIC(modelov2Anomit)
report(modelov2Anomit)
detach(df)


# Context/request: 0ms-3500ms----T-C-D1/D2   w/participant as rfact
df <- v2.data
attach(df)
modelov2Anom <- lmer(Tiempo ~ TCDD + (1 + TCDD | Nombre))
summary(modelov2Anom)
anova(modelov2Anom)
emmeans(modelov2Anom, pairwise ~ TCDD)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ TCDD * MD)
coef(modelov2Anom)
AIC(modelov2Anom)
detach(df)

# Context/request: 0ms-3500ms----T-C-D1/D2   w/item as rfact
df <- v2.data
attach(df)
modelov2Ait <- lmer(Tiempo ~ TCDD + (1 + TCDD | Item))
summary(modelov2Ait)
anova(modelov2Ait)
emmeans(modelov2Anomit, pairwise ~ TCDD)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ Juntas)
boxplot(Tiempo ~ TCDD)
boxplot(Tiempo ~ TCDD * MD)
coef(modelov2Ait)
AIC(modelov2Ait)
detach(df)


# Yes: 3500ms-4000ms T--C--D1/D2

# PARA CAMBIAR LA CATEGORIA DE REFERENCIA
df <- v3.data
attach(df)
table(TCDD)
TCDD <- relevel(TCDD, ref = "t")
MD <- relevel(MD, ref = "s")

# PARA CAMBIAR LA CATEGORIA DE REFERENCIA
df <- v3.data
attach(df)
table(TCDD)
TCDD <- relevel(TCDD, ref = "DD")
MD <- relevel(MD, ref = "n")

# PARA CAMBIAR LA CATEGORIA DE REFERENCIA
df <- v3.data
attach(df)
table(TCDD)
TCDD <- relevel(TCDD, ref = "DD")
MD <- relevel(MD, ref = "s")





# Yes: 3500ms-4000ms T--C--D1/D2   w/particioant and intem as rfact
df <- v3.data
attach(df)
modelov3DDnomit <- lmer(Tiempo ~ TCDD * MD + (1 + TCDD | Nombre) + (1 + MD | Nombre) + (1 + TCDD | Item) + (1 + MD | Item))
summary(modelov3DDnomit)
anova(modelov3DDnomit)
plotmo(modelov3DDnomit, type = "diag", show.values = TRUE)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov3DDnomit, pairwise ~ TCDD * MD)
coef(modelov3DDnomit)
AIC(modelov3DDnomit)
report (modelov3DDnomit)
detach(df)


# Yes: 3500ms-4000ms T--C--D1/D2   w/particioant as rfact
df <- v3.data
attach(df)
modelov3DDnom <- lmer(Tiempo ~ TCDD * MD + (1 + TCDD | Nombre) + (1 + MD | Nombre))
summary(modelov3DDnom)
anova(modelov3DDnom)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov3DDnom, pairwise ~ TCDD * MD)
coef(modelov3DDnom)
AIC(modelov3DDnom)
report (modelov3DDnom)
detach(df)



#Yes: 3500ms-4000ms T--C--D1/D2 -- w/item as rfact
df <- v3.data
attach(df)
modelov3DDit <- lmer(Tiempo ~ TCDD * MD + (1 + TCDD | Item) + (1 + MD | Item))
summary(modelov3DDit)
anova(modelov3DDit)
boxplot(Tiempo ~ Imagen * MD)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov3DDit, pairwise ~ TCDD * MD)
coef(modelov3DDit)
AIC(modelov3DDit)
detach(df)



# PARA CAMBIAR LA CATEGORÍA DE REFERENCIA
df <- v4.data
attach(df)
table(Imagen)
Imagen <- relevel(TCDD, ref = "c")
MD <- relevel(MD, ref = "h")
detach(df)

#Focus operator: 4000ms-4650ms T--C--D1/D2-- w/participant and item as rfact
df <- v4.data
attach(df)
modelov4nomit <- lmer(Tiempo ~ MD * TCDD + (1 + MD | Nombre) + (1 + TCDD | Nombre) + (1 + MD | Item) + (1 + TDDD | Item))
summary(modelov4nomit)
anova(modelov4nomit)
boxplot(Tiempo ~ MD * TCDD)
boxplot(Tiempo ~ Imagen)
emmeans(modelov4nomit, pairwise ~ MD * TCDD)
AIC(modelov4nomit)
coef(modelov4nomit)
detach(df)


#Focus operator: 4000ms-4650ms T--C--D1/D2-- w/participant
df <- v4.data
attach(df)
modelov4nom <- lmer(Tiempo ~ MD * TCDD + (1 + MD | Nombre) + (1 + TCDD | Nombre))
summary(modelov4nom)
anova(modelov4nom)
boxplot(Tiempo ~ MD * TCDD)
boxplot(Tiempo ~ Imagen)
emmeans(modelov4nom, pairwise ~ MD * TCDD)
coef(modelov4nom)
AIC(modelov4nom)
detach(df)

#Focus operator: 4000ms-4650ms T--C--D1/D2-- w/item
df <- v4.data
attach(df)
modelov4it <- lmer(Tiempo ~ MD * TDDD + (1 + MD | Item) + (1 + TDDD | Item))
summary(modelov4it)
anova(modelov4it)
boxplot(Tiempo ~ MD * TCDD)
boxplot(Tiempo ~ TCDD)
emmeans(modelov4ti, pairwise ~ MD * TCDD)
coef(modelov4it)
AIC(modelov4it)
detach(df)


# PARA CAMBIAR LA CATEGORÍA DE REFERENCIA
df <- v5.data
attach(df)
table(Imagen)
Imagen <- relevel(TCDD, ref = "c")
MD <- relevel(MD, ref = "s")
detach(df)

#Reply to request: 4650ms-6700ms T--C--D1/D2-- w/participant and item as rfact
df <- v5.data
attach(df)
modelov5nomit <- lmer(Tiempo ~  MD * TCDD + (1 + MD | Nombre) + (1 + MD | Item) + (1 + TCDD | Nombre) + (1 + TCDD | Item))
summary(modelov5nomit)
anova(modelov5nomit)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov5nomit, pairwise ~ TCDD * MD)
coef(modelov5nomit)
AIC(modelov5nomit)
detach(df)

#Reply to request: 4650ms-6700ms T--C--D1/D2-- w/participant as rfact
df <- v5.data
attach(df)
modelov5nom <- lmer(Tiempo ~  MD * TCDD + (1 + MD | Nombre)  + (1 + TCDD | Nombre))
summary(modelov5nom)
anova(modelov5nom)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov5nom, pairwise ~ TCDD * MD)
coef(modelov5nom)
AIC(modelov5nom)
detach(df)

#Reply to request: 4650ms-6700ms T--C--D1/D2-- w/item as rfact
df <- v5.data
attach(df)
modelov5it <- lmer(Tiempo ~  MD * TCDD  + (1 + MD | Item) + (1 + TCDD | Item))
summary(modelov5it)
anova(modelov5it)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov5it, pairwise ~ TCDD * MD)
coef(modelov5it)
AIC(modelov5it)
detach(df)



# PARA CAMBIAR LA CATEGORÍA DE REFERENCIA
df <- v6.data
attach(df)
table(Imagen)
Imagen <- relevel(TCDD, ref = "c")
MD <- relevel(MD, ref = "s")
detach(df)

#Target named: 6700ms-9500ms T--C--D1/D2-- w/participant and item as rfact 
df <- v6.data
attach(df)
modelov6nom <- lmer(Tiempo ~  MD * TCDD + (1 + MD | Nombre) + (1 + MD | Item) + (1 + TCDD | Nombre) + (1 + TCDD | Item))
summary(modelov6nom)
anova(modelov6nom)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov6nom, pairwise ~ TCDD * MD)
AIC(modelov6nom)
detach(df)


#Target named: 6700ms-9500ms T--C--D1/D2-- w/participant as rfact 
df <- v6.data
attach(df)
modelov6nom <- lmer(Tiempo ~  MD * TCDD + (1 + MD | Nombre)  + (1 + TCDD | Nombre))
summary(modelov6nom)
anova(modelov6nom)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov6nom, pairwise ~ TCDD * MD)
AIC(modelov6nom)
detach(df)

#Target named: 6700ms-9500ms T--C--D1/D2-- w/item as rfact 
df <- v6.data
attach(df)
modelov6nom <- lmer(Tiempo ~  MD * TCDD + (1 + MD | Item) + (1 + TCDD | Item))
summary(modelov6nom)
anova(modelov6nom)
boxplot(Tiempo ~ TCDD * MD)
emmeans(modelov6nom, pairwise ~ TCDD * MD)
AIC(modelov6nom)
detach(df)

