# Make some fake "maths pipeline data"

set.seed(33)
KS1 = sample(c('3+','2A','2B', '2C-'), 10000, replace = T, prob = c(.21,.28,.26,.25))
KS2 = sample(c('6','5+', '5-','4','3-'), 10000, replace = T, prob = c(0.03, 0.15, 0.22, 0.46, 0.13))
GCSE = sample(c('9','8', '7', '6-'), 10000, replace = T, prob = c(0.03, 0.07, 0.10, 0.80))
A_level =  sample(c('Maths + Further',  'Maths', 'No Maths'), 10000, replace = T, prob = c(0.02, 0.09, 0.89))
FSM = sample(c('Yes', 'No'), 10000, replace = T, prob = c(.238, 1-.238))
IDACI = sample(c('1st','2nd','3rd', '4th', '5th'), 10000, replace = T)
Gender = sample(c('Male', 'Female'), 10000, replace = T, prob = c(.4925, 1-.4925))
raw = data.frame(FSM = factor(FSM),
                 IDACI = factor(IDACI),
                 Gender = factor(Gender),
                 KS1 = factor(KS1, levels = c('3+','2A','2B', '2C-')),
                 KS2 = factor(KS2, levels = c('6','5+', '5-','4','3-')),
                 GCSE= factor(GCSE, levels = c('9','8', '7', '6-')),
                 A_level = factor(A_level, levels = c('Maths + Further',  'Maths', 'No Maths'))
)
fake_data = raw

usethis::use_data(fake_data)
