source('functions.R')

df.4 = get_df_k(k = 0, method = "natural")
df.4 = df.4 %>%
	group_by(tau) %>%
    #filter(convexity > quantile(convexity, probs = 0.05, na.rm = T),
    #       convexity < quantile(convexity, probs = 0.95, na.rm = T)) %>%
    mutate(time_scaled_convexity = sqrt(tau/252.0)*convexity)

p4_1 = df.4 %>%
    ggplot(aes(x = factor(tau), y = convexity)) +
	stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') +
    geom_point(size = 1) +
    xlab("tau")
print(p4_1)
ggsave('SF4_1.eps', p4_1, width = 7, height = 6)

p4_2 = df.4 %>%
    ggplot(aes(x = factor(tau), y = time_scaled_convexity)) +
	stat_summary(fun.data = by.vals, geom = 'boxplot', color = 'blue') +
    geom_point(size = 1) +
    xlab("tau")
print(p4_2)
ggsave('SF4_2.eps', p4_2, width = 7, height = 6)