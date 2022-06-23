
n_original = 100
n_boot = 2000

# generate random numbers
nums = rnorm(n = n_original, mean = 0, sd = 1)

# test on original data
t.test(nums, mu = 0)

# bootstrap resampling using 2000 samples
nums_resampled = sample(nums, n_boot, replace = T)

# test on bootstrapped data
t.test(nums_resampled, mu = 0)


sim_function = function(n_original, n_boot){
  # generate random numbers
  nums = rnorm(n = n_original, mean = 0, sd = 1)
  
  # test on original data
  p_original = t.test(nums, mu = 0)$p.value
  
  # bootstrap resampling using 2000 samples
  nums_resampled = sample(nums, n_boot, replace = T)
  
  # test on bootstrapped data
  p_boot = t.test(nums_resampled, mu = 0)$p.value
  
  return(round(c(p_original, p_boot),3))
  
}

iterations = 1000

sim_results = data.frame(t(replicate(n = iterations, sim_function(n_original = 100, n_boot = 2000))))
names(sim_results) = c("original", "boot")

sum(sim_results$original<0.05)/length(sim_results$original) # alpha error rate in original
sum(sim_results$boot<0.05)/length(sim_results$boot) # alpha error rate in bootstrapped resampled data
  