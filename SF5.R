source('functions.R')

level_mat = get_df_k_term_level_mat(0)
tslope_mat = get_df_k_term_slope_mat(0)

level.vec = as.vector(t(level_mat))
tslope.vec = as.vector(t(tslope_mat))

n_group = 3
group_names = paste("group", 1:n_group)
pivot = (1:(n_group-1))*floor(n2/n_group)

levels = c(0, pivot, Inf)
labels = group_names 
group_names = paste("group", 1:n_group)
n3 = floor(n2 / n_group)
tau = rep(my_taus, n1)
tau = cut(tau, levels, labels = labels)

df.5 = data.frame(tau = tau, level = level.vec, tslope = tslope.vec)
df.5 %>%
    group_by(tau) %>%
    summarise(correlation = cor(level, tslope))

p5_1 = df.5 %>%
    ggplot(aes(x = level, y = tslope)) + 
    geom_point(size = 0.4) + 
    stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
    ylab('term-structure slope')
print(p5_1)
ggsave(filename = "SF5_1.eps", p5_1, width = 7, height = 6)

p5_2 = ggplot(df.5, aes(x = level, y = tslope)) +
    geom_point(size = 0.4) + scale_colour_manual(values = c(2, 4, 1)) +
    stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
    ylab('term-structure slope') + 
    facet_wrap(~ tau, scales = "free")
print(p5_2)
ggsave(filename = "SF5_2.eps", p5_2, width = 10, height = 4)