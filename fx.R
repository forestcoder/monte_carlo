library(quantmod)
library(data.table)
library(ggplot2)

getSymbols("CAD/USD", src="oanda")

head(CADUSD)
plot(CADUSD$CAD.USD)

# generate return
ret <- as.numeric(CADUSD$CAD.USD/lag(CADUSD$CAD.USD))
hist(ret)
ret[1] <- 1

# monte carlo
set.seed(0)
ndays <- 252
numreplications <- 1000
paths <- replicate(n = numreplications,
                   expr = sample(ret, ndays,
                                 replace = TRUE))
hist(paths)
paths <- apply(paths, 2, cumprod)

# prepare
paths <- data.table(paths)
paths$days <- 1:nrow(paths)
paths <- melt(paths, id.vars = "days")

# Average gain (loss) over period
summary((paths$value[paths$days == ndays]))

# Plot simulations
ggplot(data = paths,
       aes(x = days, y = (value - 1) * 100
           , group = variable)) +
  geom_line(alpha=1/100, linewidth=2) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Days Invested") + ylab("Portfolio Return %")
