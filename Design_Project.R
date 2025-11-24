exp_draws <- matrix(c(
  -1.0, -1.5, 0.5, 2.0, 2.0, -1.0, -2.0,
  -0.5, -1.2, 1.5, 1.2, 1.5, -1.0, -1.8,
  -0.4, -0.8, 0.8, 1.0, 0.4, -1.2, -1.9,
  +0.4, -1.5, 0.8, 1.5, 1.5, -0.9, -1.5,
  +0.3, -0.3, 0.7, 0.5, 1.5, -0.5, -1.0
  ), ncol=7, byrow=T)

mu <- apply(exp_draws, 2, mean)
var(exp_draws)
cor(exp_draws)


sigma <- diag(7)
sigma[7,6] <- sigma[6,7] <- 0.5


# Thirdly, we generate individual values of parameters according to the previously
# specified distributions, using the multivariate normal distribution 
library(MASS)
set.seed(8756)
draws <- mvrnorm(n=20, mu=mu, Sigma=sigma) # generate value for 20 individuals

library(idefix)

# Firstly, we need to create the full design that considers all possible profiles
# using the function Profiles()
attrib <- c(3, 2, 2, 2, 3) # specify 4 attributes with respectively 3, 3, 2 and 2 levels. 
dcode <- rep("D", length=5) # specify to treat attributes as dummy variables
prof.set <- Profiles(lvls=attrib, coding=dcode) # create the set of all possible profiles


optDes <- Modfed(cand.set=prof.set, n.sets=10, n.alts=3, par.draws=draws)

# Using the function Decode(), we can convert the choice set design from 
# a model matrix structure to a data frame format 
attrib <- list(spec = c("Assistente", "Codice", "Content"), #list of attributes with levels 
               vel = c("Lento", "Veloce"),
               qual = c("Sufficente", "Ottimale"),
               priv = c("Bassa", "Alta"),
               cost = c("15", "20", "25")) 
optDesDF <- Decode(des=optDes$BestDesign$design, n.alts=3, lvl.names=attrib, coding=dcode)$design 
names(optDesDF) <- c("spec", "vel", "qual", "priv", "cost")
optDesDF
