---
title: "Corona China"
author: "Norberto Asensio"
date: "19/03/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

```{r cars}
setwd("~/Documents/R scripts and data/corona")
china.cor<-read.csv("chinadata.csv")
head(china.cor)
```

```{r}
par(mfrow=c(1,2))
plot(china.cor$day_count,china.cor$cases,ylab="new cases",
      xlab="days")
plot(china.cor$day_count,china.cor$cum,ylab="cum num. of cases",
     xlab="days")
```

```{r}
pred.frame <- seq(from=0, to=80, by=0.1)
model2<-glm(cases~poly(day_count,2),
            data=china.cor,family="poisson")
confint(model2, level=0.95)
prediction <- predict(model2, list(day_count = pred.frame),type="response",se.fit = T)
```

```{r}
plot(china.cor$day_count,china.cor$cases,ylab="new cases",
     xlab="days",main="China COVI19 first two months")
lines(pred.frame,prediction$fit,col="red")
lines(pred.frame,prediction$fit+1.96*prediction$se.fit,col="blue",lwd=0.5)
lines(pred.frame,prediction$fit-1.96*prediction$se.fit,col="blue",lwd=0.5)
abline(v=23,col="red",lty=2)
text(35,4500,"lockdown measures",col="red")
```
