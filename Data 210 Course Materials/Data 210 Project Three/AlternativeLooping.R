 # for (i in 1:num_records) {
 #      gsub(".","",dat2$State, fixed=TRUE)
 #      Needs_abbrev <- (dat2$State[i] %!in% state.abb)
 #          if (Needs_abbrev & dat2$State[i] != "DC") {
 #            dat2$State[i] <- state.abb[as.integer(match(dat2$State[i],state.name))]
 #          }
 #          if (!Needs_abbrev) {
 #            dat2$State[i] <- state.abb[as.integer(match(dat2$State[i], state.abb))]
 #          }
 # 
 #  }

df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)
ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y))

