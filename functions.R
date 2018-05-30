model.maker <- function(data, variable.name, control.vars=F){
  #  L <- list("watch_wd","watch_wd_sq","watch_we", "watch_we_sq", "play_wd", "play_wd_sq", "play_we", "play_we_sq", "sp_wd", "sp_wd_sq", "sp_we", "sp_wd_sq", "comp_wd", "comp_wd_sq","comp_we","comp_we_sq")
  cnames <- colnames(data)
  name.no1 <- match(variable.name,cnames)
  name.no2 <- match(paste(variable.name,"_sq", sep=""),cnames)
  lin.comp <- as.numeric(unlist(data[,name.no1]))
  quad.comp <- as.numeric(unlist(data[,name.no2]))
  
  model <- lm(data$mwbi ~ lin.comp + quad.comp)
  if (control.vars){model <- lm(data$mwbi ~ lin.comp + quad.comp + data$Genderg + data$Ethnicg + data$IMD3)}

    L <- list("watch_wd","watch_wd_sq","watch_we", "watch_we_sq", "play_wd", "play_wd_sq", "play_we", "play_we_sq", "sp_wd", "sp_wd_sq", "sp_we", "sp_wd_sq", "comp_wd", "comp_wd_sq","comp_we","comp_we_sq")

  return(model)
}

data <- c(1:12)
dimnames <- list(time=c("linear","quadratic"), name=c(1:6))
mat <- matrix(data, ncol=6, nrow=2, dimnames=dimnames)
as.data.frame((mat))

table.maker <- function (model){
  
  table <- matrix(nrow = 2, ncol = 6)
  
  Columns <-c("b", "SE", "CI(2.5%)", "CI(97.5%)", "p", "d")
  colnames(table) <- Columns
  rownames(table) <- c("linear","quadratic")
  
  out <- summary(model)
  coefs <- coef(model)
  confints <- confint(model)
  
  df.sqrt <- sqrt(model$df)
  cohens.d.lin <- 2*abs(summary(model)$coefficients[2,3]/df.sqrt)
  cohens.d.quad <- 2*abs(summary(model)$coefficients[3,3]/df.sqrt)
  
  table[1,1] <- coefs[2]
  table[2,1] <- coefs[3]
  table[1, 2] <- out$coefficients[2,2]
  table[2,2] <- out$coefficients[3,2]
  table[1,3] <- confints[2,1] #2.5% part of linear model CI
  table[2,3] <- confints[3,1] #2,5% part of quadratic model CI
  table[1,4] <- confints[2,2] #97.5% part of linear model CI
  table[2,4] <- confints[3,2] #97.5% part of quadratic model CI
  table[1,5] <- out$coefficients[2,4] #p values, should be made nicer by replacing small values with "<.005)
  table[2,5] <- out$coefficients[3,4]
  table[1,6] <- cohens.d.lin
  table[2,6] <- cohens.d.quad
  
  return(table)

}