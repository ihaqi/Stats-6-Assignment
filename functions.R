model.maker <- function(data, variable.name, add.gender=F, add.ethnic=F, add.ses=F, agg.vars=F){
  #  L <- list("watch_wd","watch_wd_sq","watch_we", "watch_we_sq", "play_wd", "play_wd_sq", "play_we", "play_we_sq", "sp_wd", "sp_wd_sq", "sp_we", "sp_wd_sq", "comp_wd", "comp_wd_sq","comp_we","comp_we_sq")
  cnames <- colnames(data)
  name.no1 <- match(variable.name,cnames)
  name.no2 <- match(paste(variable.name,"_sq", sep=""),cnames)
  lin.comp <- as.numeric(unlist(data[,name.no1]))
  quad.comp <- as.numeric(unlist(data[,name.no2]))
  

  gender.comp <- data$male
  ethnic.comp <- data$minority
  ses.comp <- data$deprived

  if(agg.vars){
    gender.comp <- data$Genderg
    ethnic.comp <- data$Ethnicg
    ses.comp <- data$IMD3
  }
  
  model <- lm(data$mwb ~ lin.comp + quad.comp)
  if (add.gender==T & add.ethnic==T & add.ses==T){
    model <- lm(data$mwb ~ lin.comp + quad.comp + gender.comp + ethnic.comp + ses.comp)}
  if (add.gender==T & add.ethnic==T & add.ses==F){
    model <- lm(data$mwb ~ lin.comp + quad.comp + gender.comp + ethnic.comp)}
  if (add.gender==T & add.ethnic==F & add.ses==T){
    model <- lm(data$mwb ~ lin.comp + quad.comp + gender.comp + ses.comp)}
  if (add.gender==T & add.ethnic==F & add.ses==F){
    model <- lm(data$mwb ~ lin.comp + quad.comp + gender.comp)}
  if (add.gender==F & add.ethnic==T & add.ses==T){
    model <- lm(data$mwb ~ lin.comp + quad.comp + ethnic.comp + ses.comp)}
  if (add.gender==F & add.ethnic==T & add.ses==F){
    model <- lm(data$mwb ~ lin.comp + quad.comp + ethnic.comp)}
  if (add.gender==F & add.ethnic==F & add.ses==T){
    model <- lm(data$mwb ~ lin.comp + quad.comp + ses.comp)}

  return(model)
}

data <- c(1:12)
dimnames <- list(time=c("linear","quadratic"), name=c(1:6))
mat <- matrix(data, ncol=6, nrow=2, dimnames=dimnames)
as.data.frame((mat))

row.maker <- function (model){
  
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

table.maker <- function(data, add.gender=F, add.ethnic=F, add.ses=F, agg.vars=F, digits = 2){
  
  table <- NULL
  rows.tmp <- NULL
  l <- list("watch_wd","watch_we", "play_wd", "play_we", "comp_wd", "comp_we", "sp_wd", "sp_we")
  activities <- list("Watch Weekday", "Watch Weekend", "Play Weekday", "Play Weekend", "Computer Weekday", "Computer Weekend", "Smatphone Weekday", "Smatphone Weekend")
  
  for(i in 1:length(l)){
    rows.tmp <- round(row.maker(model.maker(data,l[i], add.gender=add.gender, add.ethnic=add.ethnic, add.ses=add.ses, agg.vars = agg.vars)),digits)
    table <- rbind(table, rows.tmp)
  }
  
  for(i in 1:length(activities)){
    rownames(table)[2*i-1]<-paste(activities[i],"Linear")
    rownames(table)[2*i]<-paste(activities[i],"Quadratic")
  }
 
  return(table)
  
}

recoder <- function(data, minority.code = c(0,1,1,1,1), depr.code = c(0,1,1)){
  
  # re-coding ethnicity
  data$minority[data$Ethnicg==1] <- minority.code[1] # Whites
  data$minority[data$Ethnicg==2] <- minority.code[2] # Mixed / multiple ethnic groups
  data$minority[data$Ethnicg==3] <- minority.code[3] # Asian / Asian British
  data$minority[data$Ethnicg==4] <- minority.code[4] # Black / African / Caribbean / Black British
  data$minority[data$Ethnicg==5] <- minority.code[5] # Other ethnic group
  
  # re-coding deprivation
  data$deprived[data$IMD3==1] <- depr.code[1] # lower IMD3 quantiles
  data$deprived[data$IMD3==2] <- depr.code[2] # middle IMD3 quantiles
  data$deprived[data$IMD3==3] <- depr.code[3] # higher IMD3 quantiles
  
  return(data)
  
}

concise <- function (model){
  
  p.values <- summary(model)$coefficients[,4]
  effect.sizes <- 2*abs(summary(model)$coefficients[,3]/sqrt(model$df))

  return(data.frame(p.values,effect.sizes))
  
}

