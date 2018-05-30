

#clean <- na.omit(data)

watch_wd <- clean$watch_wd
watch_wd_sq <- clean$watch_wd_sq
watch_we <- clean$watch_we
watch_we_sq <- clean$watch_we_sq

play_wd <- clean$play_wd
play_wd_sq <- clean$play_wd_sq
play_we <- clean$play_we
play_we_sq <- clean$play_we_sq

sp_wd <- clean$sp_wd
sp_wd_sq <- clean$sp_wd_sq
sp_we <- clean$sp_we
sp_we_sq <- clean$sp_we_sq

comp_wd <- clean$comp_wd
comp_wd_sq <- clean$comp_wd_sq
comp_we <- clean$comp_we
comp_we_sq <- clean$comp_we_sq

#Linear and quadratic Models without correction

L <- list("watch_wd","watch_wd_sq","watch_we", "watch_we_sq", "play_wd", "play_wd_sq", "play_we", "play_we_sq", "sp_wd", "sp_wd_sq", "sp_we", "sp_wd_sq", "comp_wd", "comp_wd_sq","comp_we","comp_we_sq")

cnames<-colnames(clean)

uncontrolled.models <- list(NA)

for (i in 1:(length(L)/2)){
  
  c1 <- match(L[2*i-1],cnames)
  c2 <- match(L[2*i],cnames)
  
  lin.comp <- as.numeric(unlist(clean[,c1]))
  quad.comp <- as.numeric(unlist(clean[,c2]))
  
  uncontrolled.models[[i]] <- lm(clean$mwb ~  lin.comp + quad.comp)
}

#Linear and quadratic Models with correction for controlling variables

L <- list("watch_wd","watch_wd_sq","watch_we", "watch_we_sq", "play_wd", "play_wd_sq", "play_we", "play_we_sq", "sp_wd", "sp_wd_sq", "sp_we", "sp_wd_sq", "comp_wd", "comp_wd_sq","comp_we","comp_we_sq")

cnames<-colnames(clean)

controlled.models <- list(NA)

for (i in 1:(length(L)/2)){
  
  c3 <- match(L[2*i-1],cnames)
  c4 <- match(L[2*i],cnames)
  
  lin.comp <- as.numeric(unlist(clean[,c3]))
  quad.comp <- as.numeric(unlist(clean[,c4]))
  
  controlled.models[[i]] <- lm(clean$mwb ~  lin.comp + quad.comp + clean$Genderg + clean$Ethnicg + clean$IMD3)
  
  }
    #Creating Table for uncontrolled models
Table1 <- matrix(nrow = length(L), ncol = 6)

Columns <-c("b", "SE", "CI(2.5%)", "CI(97.5%)", "p", "d")
colnames(Table1) <- Columns
rownames(Table1) <- L


coefs <- c()
confints <- c()
effsizes <- c()

for (i in 1:(length(L)/2)){
  model <- uncontrolled.models[[i]]
  out <- summary(uncontrolled.models[[i]])
  coefs <- coef(uncontrolled.models[[i]])
  confints <- confint(uncontrolled.models[[i]])

  df.sqrt <- sqrt(model$df)
  cohens.d.lin <- 2*abs(summary(model)$coefficients[2,3]/df.sqrt)
  cohens.d.quad <- 2*abs(summary(model)$coefficients[3,3]/df.sqrt)
  
 # print(cohens.d.quad) 
  Table1[2*i-1,1] <- coefs[2]
  Table1[2*i,1] <- coefs[3]
  Table1[2*i-1, 2] <- out$coefficients[2,2]
  Table1[2*i,2] <- out$coefficients[3,2]
  Table1[2*i-1,3] <- confints[2,1] #2.5% part of linear model CI
  Table1[2*i,3] <- confints[3,1] #2,5% part of quadratic model CI
  Table1[2*i-1,4] <- confints[2,2] #97.5% part of linear model CI
  Table1[2*i,4] <- confints[3,2] #97.5% part of quadratic model CI
  Table1[2*i-1,5] <- out$coefficients[2,4] #p values, should be made nicer by replacing small values with "<.005)
  Table1[2*i,5] <- out$coefficients[3,4]
  Table1[2*i-1,6] <- cohens.d.lin
  Table1[2*i,6] <- cohens.d.quad
  #cohens d is still needed though, and might take a little time to get sorted
  }



    #Creating table for controlled models
Table2 <- matrix(nrow = length(L), ncol = 6)

colnames(Table2) <- Columns
rownames(Table2) <- L


for (i in 1:(length(L)/2)){
  out <- summary(controlled.models[[i]])
  coefs <- coef(controlled.models[[i]])
  confints <- confint(controlled.models[[i]])

  Table2[2*i-1,1] <- coefs[2]
  Table2[2*i,1] <- coefs[3]
  Table2[2*i-1, 2] <- out$coefficients[2,2]
  Table2[2*i,2] <- out$coefficients[3,2]
  Table2[2*i-1,3] <- confints[2,1] #2.5% part of linear model CI
  Table2[2*i,3] <- confints[3,1] #2,5% part of quadratic model CI
  Table2[2*i-1,4] <- confints[2,2] #97.5% part of linear model CI
  Table2[2*i,4] <- confints[3,2] #97.5% part of quadratic model CI
  Table2[2*i-1,5] <- out$coefficients[2,4] #p values, should be made nicer by replacing small values with "<.005)
  Table2[2*i,5] <- out$coefficients[3,4]
  #cohens d is still needed though, and might take a little time to get sorted
}

models.controlled <- Table2
models.uncontrolled <- Table1


View(models.uncontrolled)
View(models.controlled)
