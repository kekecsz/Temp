
average_chance_of_success = 0.4
sd_of_success_chance = 0.1
number_of_receivers = 25
total_number_of_trials = 50



trial_results_byoneperson = function(average_chance_of_success, sd_of_success_chance, total_number_of_trials, number_of_receivers){
  personal_chance_of_success = rnorm(n = 1, mean = average_chance_of_success, sd = sd_of_success_chance)
  if(personal_chance_of_success < 0.00000000001){personal_chance_of_success = 0.00000000001}
  if(personal_chance_of_success > 0.99999999999){personal_chance_of_success = 0.99999999999}
  trial_result = replicate(total_number_of_trials/number_of_receivers,
                           sample(c(1,0,0,0), 1, prob = c(personal_chance_of_success, (1-personal_chance_of_success)/3, (1-personal_chance_of_success)/3, (1-personal_chance_of_success)/3)))
 return(trial_result) 
}

study_generator = function(average_chance_of_success, sd_of_success_chance, total_number_of_trials, number_of_receivers){
  data = as.vector(replicate(number_of_receivers,
                             trial_results_byoneperson(average_chance_of_success = average_chance_of_success, 
                                                       sd_of_success_chance = sd_of_success_chance, 
                                                       total_number_of_trials = total_number_of_trials, 
                                                       number_of_receivers = number_of_receivers)))
  
  p_value = binom.test(x = sum(data), n = length(data), p = 0.25, alternative = "greater")$p.value
  
  return(p_value)
}

iterations = 10000

### 50 separate receivers (1 trial each)

results = replicate(iterations,
          study_generator(average_chance_of_success = 0.43, 
                          sd_of_success_chance = 0.2, 
                          total_number_of_trials = 50, 
                          number_of_receivers = 50))

power = sum(results<0.05)/length(results)
power


### 5 separate receivers (10 trials each)

results = replicate(iterations,
                    study_generator(average_chance_of_success = 0.43, 
                                    sd_of_success_chance = 0.2, 
                                    total_number_of_trials = 50, 
                                    number_of_receivers = 5))

power = sum(results<0.05)/length(results)
power
