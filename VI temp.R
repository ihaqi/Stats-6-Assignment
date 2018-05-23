

clean <- na.omit(data)

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
  
  uncontrolled.models[[i]] <- lm(clean$mwbi ~  lin.comp + quad.comp)
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
  
  controlled.models[[i]] <- lm(clean$mwbi ~  lin.comp + quad.comp + clean$Genderg + clean$Ethnicg + clean$IMD3)
  }

summary(controlled.models[[1]])

