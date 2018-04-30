source('functions.R')

df.7 = get_sf7(tau_seq = get_tau())

p7 = ggplot(df.7, aes(x = tau, y = k_min)) + stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') + geom_point(size = 0.2) 
print(p7)
ggsave('SF7.eps', p7, width = 7, height = 6)