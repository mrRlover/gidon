
allRegData <- data.frame(sweep(coredata(r.xts), 1, coredata(rfr)), RM = coredata(jalsh) - coredata(rfr))[-1,]

betas <- matrix(NA, nrow = nrow(r.xts)-12, ncol = ncol(r.xts))

for (i in 1:ncol(betas)) {
  betas[,i] <- rollapply(zoo(allRegData[,c(i, 6)]),
                         width=12,
                         FUN = function(Z) 
                         { 
                           t = lm(formula=Z[,1]~RM, data = as.data.frame(Z), na.rm=T); 
                           return(t$coef) 
                         },
                         by.column=FALSE, align="right")[,2, drop = FALSE]
}

capm.test <- lm(r.xts[-c(1:12)] ~ betas)

test <- lapply(r.xts[-c(1:12)], function(x, d) lm(x ~ d), betas)



library(stargazer)

ylabs <- colnames(r.xts)

stargazer(capm.test, type = "html", out = paste0(getwd(), "/Output/results.html"), summary = F, rownames = T, dep.var.labels = ylabs, style = "qje")

stargazer(test$high, type = "html", out = paste0(getwd(), "/Output/results.html"), summary = F, rownames = T, style = "qje")



stargazer(broom::tidy(capm.test))

library(pander)

pander(summary(capm.test))
?pander
