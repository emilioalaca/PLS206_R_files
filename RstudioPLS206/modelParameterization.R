# Different ways to parameterize a model

# Simulate simple data. Current options is contr.treatment
mydata <- expand.grid(group = c("A","B","C"), x = c(1, 2, 5, 6, 8))
mmat <- model.matrix(~ group*x, mydata)
myparms <- c(B01 = 10, B01.2 = 2, B01.3 = 4, B11 = 0.8, B11.2 = -0.3, B11.3 = 0.2)
mydata$y <- mmat %*% myparms + rnorm(dim(mmat)[1], 0, 0.7)

m.contr.treat <- lm(y ~ group*x, mydata)
summary(m.contr.treat)
(B02hat <- coef(m.contr.treat) %*% c(1, 1, 0, 0, 0, 0)) # close to parm value of 12
(B13hat <- coef(m.contr.treat) %*% c(0, 0, 0, 1, 0, 1)) # close to parm value of 1.0
sqrt(variance.of.B02hat <- (c(1, 1, 0, 0, 0, 0) %*% vcov(m.contr.treat)) %*% c(1, 1, 0, 0, 0, 0))
cbind(mydata, model.matrix(m.contr.treat))

m.contr.treat$contrasts # Given that meaning of parms depends on how the model matrix was
#                         constructed, the model has to include a record of that!

# Change options

options(contrasts=c("contr.sum", "contr.poly"))

# run same model and save with different name
m.contr.sum <- lm(y ~ group*x, mydata)
summary(m.contr.sum)

(B02hat <- coef(m.contr.sum) %*% c(1, 0, 1, 0, 0, 0)) # close to parm value of 12
(B13hat <- coef(m.contr.sum) %*% c(0, 0, 0, 1, -1, -1)) # close to parm value of 1.0