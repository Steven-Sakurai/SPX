source("functions.R")

df.2 = get_df_k(k = 0, method = "natural")
p2 = df.2 %>%
	ggplot(aes(x = factor(tau), y = slope)) +
	stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') +
    geom_point(size = 1) +
    xlab("tau")
print(p2)
ggsave('p2.eps', p2, width = 7, height = 6)