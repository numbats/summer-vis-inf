# Based on the hedonic housing prices and the demand of clean air
require(Ecdat)
require(ggplot2)
require(lme4)
require(nullabor)

data(Hedonic)
data <- Hedonic %>% mutate(chas = ifelse(chas == 'yes', 1, 0))
str(data)
data$townid <- as.factor(data$townid)

data$tax <- data$tax/10000
summary(data)
# checking correlation
cor(data)
# standard linear model via OLS
l1 <- lm(mv ~ crim+zn+indus+chas+nox+rm+age+dis+
           rad+tax+ptratio+blacks+lstat+townid, data = Hedonic)
summary(l1)
plot(l1)
qqnorm(resid(l1))
qqline(resid(l1))

ggplot(data, aes(crim, mv)) + geom_point() + stat_smooth(method = "lm")+ facet_wrap(~townid)

lmm1 <- lmer(mv ~ crim+zn+indus+chas+nox+rm+age+dis+
       rad+tax+ptratio+blacks+lstat+ (1|townid), data = data)
summary(lmm1)
fixef(lmm1) # fixed effect
ranef(lmm1) # random effect


##
data("sleepstudy")
str(sleepstudy)
sleepstudy$Days <- as.integer(sleepstudy$Days)
qplot(x = Days, y = Reaction, data = sleepstudy, geom = "smooth", group = Subject,
      se = F, method = 'lm', color = I("black"))

ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point() + stat_smooth(method = "lm") + facet_wrap(~Subject)
# slope and intercept are different
fm1 <- lme(Reaction ~ Days, random = ~ 1+Days|Subject, data = sleepstudy)
summary(fm1)
n <- length(getGroups(fm1$groups))
Zt <- model.matrix(fm1$modelStruct$reStruct, sleepstudy) # BLUPs?
grp.dim <- fm1$dims$ncol
Z <- matrix(0, n, 0)
X <- list()
X[[1]] <- matrix(1,n,1)
X[[2]] <- as.matrix(Zt[, 1:(1+grp.dim[1]-1)])
Z <- cbind(mgcv::tensor.prod.model.matrix(X),Z)

id <- sort(as.numeric(getGroups(fm1, level = 1)), index.return = TRUE)$x
mataux <- model.matrix(fm1$modelStruct$reStruct,sleepstudy)
mataux <- as.data.frame(cbind(mataux,id))
lZi <- list()
lgi <- list()

for (i in (as.numeric(unique(id)))) {
  lZi[[i]] <- as.matrix((subset(split(mataux,id == i,
                                      drop = T)$`TRUE`,select = -id)))
  lgi[[i]] <- getVarCov(fm1,type = "random.effects")
}
Z <- as.matrix(bdiag(lZi))
# for (i in 2:7) {
#   Z <- as.matrix(bdiag(Z,lZi[[i]]))
# }
g <- getVarCov(fm1,type = "random.effects")
q <- dim(g)[1]                                                           # Total number of random effects
Gam <- as.matrix(kronecker(diag(length(as.numeric(unique(id)))),g))



##### NULLABOR PACKAGE #####
d <- lineup(null_permute("mpg"), mtcars)
head(d)
attr(d, "pos") # position of actual data plot
ggplot(data = d, aes(x = mpg, y = wt)) + geom_point() + facet_wrap(~ .sample)

## null_dist: returns a function that given the data generates a null data set.
head(null_dist('mpg', dist = 'normal')(mtcars))
head(null_permute('mpg')(mtcars))
## The three built in methods are ‘rotate’, ‘pboot’ and ‘boot’ 
# defined by "resid_rotate", "resid_pboot" and "resid_boot" respectively. 
head(null_lm(wt~mpg, method = 'rotate')(mtcars))

require(MASS)
data(wasps)

wasps.lda <- lda(Group~., data = wasps[,-1])
wasps.la <- predict(wasps.lda,dimen = 2)$x
true <- data.frame(wasps.la, Group = wasps$Group)

wasp.sim <- data.frame(LD1 = NULL, LD2 = NULL, Group = NULL, .n = NULL)
for (i in 1:19) {
  x <- wasps
  x$Group <- sample(x$Group)
  x.lda <- lda(Group~., data = x[,-1])
  x.ld <- predict(x.lda, dimen = 2)$x
  sim <- data.frame(x.ld, Group = x$Group, .n = i)
  wasp.sim <- rbind(wasp.sim, sim)
}

pos <- sample(1:20, 1)
d <- lineup(true = true, samples = wasp.sim, pos = pos)

ggplot(d, aes(x = LD1, y = LD2, color = Group)) + 
  facet_wrap(~.sample, ncol = 5) + 
  geom_point() + theme(aspect.ratio = 1)
attr(d, "pos")


