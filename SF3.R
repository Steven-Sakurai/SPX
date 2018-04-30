source('functions.R')

df.3 = get_df_k(k = 0, method = "natural")
df.3 = df.3 %>% 
	mutate(tau_group = "default")

my_days = get_date()
my_taus = get_tau()
n1 = length(my_days)
n2 = length(my_taus)

n_group = 3
group_names = paste("group", 1:n_group)
pivot = (1:(n_group-1))*floor(n2/n_group) 

i = 1
for(p in pivot) {
	if(i == 1) {
		q = 0
	}
	else {
		q = pivot[i-1]
	}
	df.3 = df.3 %>%
		mutate(tau_group = replace(tau_group, (tau > q & tau <= p), group_names[i]))
	i = i + 1
}
df.3 = df.3 %>% 
	mutate(tau_group = replace(tau_group, tau > my_taus[pivot[length(pivot)]], group_names[length(group_names)]))

p3 = df.3 %>% 
	filter(slope < 5, slope > -7.5) %>%
	ggplot(aes(x = level, y = slope)) +
	geom_point(size = 0.4) + 
	geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
	facet_wrap(~ tau_group, scales = "free") + 
	ylab('slope')
print(p3)
ggsave(filename = "SF3.eps", width = 10, height = 6)

