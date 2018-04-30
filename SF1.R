source('functions.R')
my_days = get_date()
my_taus = get_tau()
n1 = length(my_days)
n2 = length(my_taus)

atm.skew = get_atm_skew(k = 0, method = "natural")
atm.convex = get_atm_convex(k = 0, method = "natural")

skew.vec = as.vector(atm.skew)
convex.vec = as.vector(atm.convex)
# Remove outliers
q.sk = quantile(skew.vec, probs = c(0.05, 0.95), na.rm = T)
q.c = quantile(convex.vec, probs = c(0.05, 0.95), na.rm = T)

Index = skew.vec < q.sk[1] | skew.vec > q.sk[2] | convex.vec < q.c[1] | convex.vec > q.c[2]
skew.vec[Index] = NA
convex.vec[Index] = NA

################
# how many groups to have
n_group = 3
group_names = paste("group", 1:n_group)
n3 = floor(n2 / n_group)

tau = c(rep(group_names[-n_group], each = n1*n3), rep(group_names[n_group], n1*(n2 - n3*(n_group - 1))))
df.1 = data.frame(slope = skew.vec, convex = convex.vec, tau = tau)

df.1 = df.1[complete.cases(df.1), ]
## calculate cor 

df.1 %>% 
	group_by(tau) %>%
	summarize(correlation = cor(slope, convex))

## Linear regression for the three groups respectively
by(df.1, factor(df.1$tau), function(x) summary(lm(convex ~ slope, data = x)))



############################################
#  PLOT
p1 = df.1 %>%
	ggplot(aes(x = slope, y = convex))
p1 = p1 + geom_point(size = 0.8) +
	facet_wrap('tau') + ylab('convexity')
print(p1)
ggsave(filename = "SF1.eps", width = 10, height = 4)