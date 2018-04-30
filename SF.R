library(tidyverse)
source('functions.R')

### SF1 correlation between slope and convexity
atm_skew: matrix(day, tau)
atm_convex: matrix(day, tau)

df.1 = get_df_k(0)
df.1 = df.1 %>% mutate(tauFactor = ifelse(df.1$tau > 14, 'tau > 14', 'tau <= 14'))
## calculate cor 
df.1.1 = filter(df.1, tauFactor == 'tau <= 14')
I1 = !is.na(df.1.1$slope) & !is.na(df.1.1$convexity)
cor(df.1.1$slope[I1], df.1.1$convexity[I1])
df.1.2 = filter(df.1, tauFactor == 'tau > 14')
I2 = !is.na(df.1.2$slope) & !is.na(df.1.2$convexity)
cor(df.1.2$slope[I2], df.1.2$convexity[I2])

## Linear regression for the three groups respectively
by(df.1, factor(df.1$tauFactor), function(x) summary(lm(convexity ~ slope, data = x)))


p1 = ggplot(df.1, aes(x = slope, y = convexity))
p1 = p1 + geom_point(size = 0.4)
p1 = p1 + 
    facet_wrap('tauFactor') + 
    ylab('convexity')
print(p1)
ggsave('p1.eps', p1)

###  SF2 flattening slope w.r.t. tau
df.2 = df.1
p2 = ggplot(df.2, aes(x = factor(tau), y = slope))
p2 = p2 + stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') +
    geom_point(size = 1)
print(p2)
ggsave('p2.eps', p2)

### SF3 correlation between level and slope
df.3 = df.1
p3 = ggplot(df.3, aes(x = level, y = slope))
p3 = p3 + geom_point(size = 0.4)
p3 = p3 + 
    facet_wrap('tauFactor') + 
ylab('slope')
print(p3)
ggsave('p3.eps', p3)

### SF4 time-scaled convexity
df.4 = df.1

#p4 = ggplot(df.4, aes(x = factor(tau), y = convexity))
p4 = ggplot(df.4, aes(x = factor(tau), y = sqrt(tau/252.0)*convexity))

p4 = p4 + stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') + geom_point(size = 0.8)
print(p4)
ggsave('p4.eps', p4)

### SF5 atm level and term-structure slope
level.mat = get_df_k_term_level_mat(0)
term_slope.mat = get_df_k_term_slope_mat(0)

df.5 = data.frame(level = as.vector(level.mat), term_slope = 252 * as.vector(term_slope.mat))
p5 = ggplot(df.5, aes(x = level, y = term_slope))
p5 = p5 + geom_point(size = 0.8) + geom_smooth()
print(p5)
ggsave('p5.eps', p5)


### SF6 log-moneyness k and term-structure slope at fixed tau

k_seq = seq(-0.15, 0.05, 0.02)
mat.6 = get_sf6(18, k_seq)

len_days = dim(mat.6)[1]
df.6 = data.frame(term_slope = as.vector(mat.6), k = rep(k_seq, each = len_days))
p6 = ggplot(df.6, aes(x = factor(k), y = 252*term_slope)) + stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') +geom_point() 
xlab("k")
print(p6)
ggsave('p6.eps', p6)


### SF7 k_star change w.r.t. tau
k_star: at fixed tau and date, k_star correspond to the minimum point of implied volatility smile

df.smile = get_df_smile()

p7 = ggplot(df.smile, aes(x = tau, y = k_min)) + stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') + geom_point(size = 0.2) 
print(p7)

