source('functions.R')

t = 18
k_seq = seq(-0.15, 0.05, 0.02)
mat.6 = get_sf6(t = t, k_seq)

len_days = dim(mat.6)[1]
a = 0.01
b = 0.99
df.6 = data.frame(tslope = as.vector(mat.6), k = rep(k_seq, each = len_days)) %>%
    filter(tslope > quantile(tslope, probs = a, na.rm = T), 
           tslope < quantile(tslope, probs = b, na.rm = T))

p6 = ggplot(df.6, aes(x = factor(k), y = tslope)) + 
    stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') + 
    geom_point(size = 0.8) + xlab("k") + ylab("term structure slope") +
    scale_x_discrete()
print(p6)
ggsave('SF6.eps', p6, width = 7, height = 6)