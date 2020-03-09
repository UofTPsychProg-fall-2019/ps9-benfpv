
library(tidyverse)

# load in the data
ipip <- read_csv('ipip50_sample.csv')

# This dataset includes measures of the Big 5 Inventory personality index, which
# measures traits of Agreeableness, Conscientiousness, Extroversion, 
# Neuroticism, and Openness, along with measures of age, BMI, and exercise 
# habits for 1000 participants. 
# 
# In the dataset, each trait has a set of associated survey items (e.g., 
# Agreeableness has A_1, A_2, A_3, ... A_10). The total number of  items 
# vary for the different traits (e.g., Agreeableness has 10, but Openness only 
# has 2). For each participant, there are measures for each of the items as well
# as the participant's age, BMI, gender, and exercise habits which are 
# categorically coded in terms of frequency.
#
# In this PS, we want to look at the relationship between the big 5 and age, 
# gender, BMI, and exercise habits. To do so will require some data wrangling...

# Calculate composites of the big 5 ---------------------------------------

# Composites for the big 5 are based on the average value of their multiple 
# items. For example, an Agreeableness composite would be the average of items
# A_1 through A_10. We want to calculate these averages for each trait 
# separately for each participant. Do this by filling in the steps below:

# The data is in wide format (i.e., each row is separate participant with
# columns for different measures) and we need it in long format. Convert
# to long format with a gather command on the trait items (A_1...O_10):
# **HINT: The long format data set should have 42000 rows**
ipip.l <- ipip %>% 
  gather(., "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "A_7", "A_8", "A_9", 'A_10', 
         'C_1', 'C_2', 'C_3', 'C_4', 'C_5', 'C_6', 'C_7', 'C_8', 'C_9', 'C_10', 
         'E_1', 'E_2', 'E_3', 'E_4', 'E_5', 'E_6', 'E_7', 'E_8', 'E_9', 'E_10',
         'N_1', 'N_2', 'N_3', 'N_4', 'N_5', 'N_6', 'N_7', 'N_8', 'N_9', 'N_10',
         'O_1', 'O_10', key=trait_value, value=score)

# We need a column that identifies rows as belonging to a specific trait,
# but the column you created based on the trait items includes both trait
# and item (e.g., A_1, but we want A in a separate column from item 1).
# Make this happen with a separate command:
ipip.l <- ipip.l %>% 
  separate(., trait_value, into=c('trait', 'item'))

# Calculate averages for each participant (coded as RID) and trait:
ipip.comp <- arrange(ipip.l, RID)
counter=0
# eplicate(length(unique(ipip.l$RID)),0)
A_mean = replicate(length(unique(ipip.l$RID)),0.0)
C_mean = replicate(length(unique(ipip.l$RID)),0.0)
E_mean = replicate(length(unique(ipip.l$RID)),0.0)
N_mean = replicate(length(unique(ipip.l$RID)),0.0)
O_mean = replicate(length(unique(ipip.l$RID)),0.0)

for (subj in unique(ipip.l$RID))
{
  counter=counter+1
  curr_row = filter(ipip.l, ipip.l$RID == subj, ipip.l$trait == 'A')
  A_mean[counter] = mean(curr_row$score)
  curr_row = filter(ipip.l, ipip.l$RID == subj, ipip.l$trait == 'C')
  C_mean[counter] = mean(curr_row$score)
  curr_row = filter(ipip.l, ipip.l$RID == subj, ipip.l$trait == 'E')
  E_mean[counter] = mean(curr_row$score)
  curr_row = filter(ipip.l, ipip.l$RID == subj, ipip.l$trait == 'N')
  N_mean[counter] = mean(curr_row$score)
  curr_row = filter(ipip.l, ipip.l$RID == subj, ipip.l$trait == 'O')
  O_mean[counter] = mean(curr_row$score)
}

sub_counter = 0
curr_sub = 0
curr_trait = 'A'
colsloop = seq(1,dim(ipip.comp)[1],1)
mean_col = seq(1,dim(ipip.comp)[1],1)

for (curr_row in colsloop)
{
  if (curr_sub != ipip.comp$RID[curr_row]){
    curr_sub = ipip.comp$RID[curr_row]
    sub_counter=sub_counter+1
  }
  curr_trait = ipip.comp$trait[curr_row]
  if (curr_trait == 'A'){
    mean_col[curr_row] = A_mean[sub_counter]
  }
  else if (curr_trait == 'C'){
    mean_col[curr_row] = C_mean[sub_counter]
  }
  else if (curr_trait == 'E'){
    mean_col[curr_row] = E_mean[sub_counter]
  }
  else if (curr_trait == 'N'){
    mean_col[curr_row] = N_mean[sub_counter]
  }
  else if (curr_trait == 'O'){
    mean_col[curr_row] = O_mean[sub_counter]
  }
}

ipip.comp$trait_mean = mean_col

# Cleaning up the other variables -----------------------------------------

# Depending on how you solved the above steps, your ipip.comp ttibble may or may
# not have the age, gender, exer, BMI variables that we want to compare to the big 5. If
# they are missing, let's add them in by joining the original ipip tibble with
# ipip.comp tibble:
# HINT: use a select call on ipip to only select the columns that you want to
# merge with ipip.comp
not_missing = 1 # STUDENT'S NOTE: NOT MISSING!

# One last thing, our exercise variable is all out of order. Because it was read
# in as a character string, it is in alphabetical order. Let's turn it into a 
# factor and reorder the levels according to increasing frequency. Do this by 
# using the factor command and its levels argument:
exer_lengths = seq(1,length(unique(ipip.comp$exer)))
for (ex in seq(1,length(unique(ipip.comp$exer)))) {
  exer_lengths[ex] = length(ipip.comp$exer[ipip.comp$exer==unique(ipip.comp$exer)[ex]])
}
rank(exer_lengths)
prelevels = seq(1,length(rank(exer_lengths)))
for (prelev in seq(1,length(rank(exer_lengths))))
  prelevels[prelev] = unique(ipip.comp$exer)[rank(exer_lengths)==prelev]
  
ipip.comp$exer <- factor(ipip.comp$exer,
                         levels=prelevels)


# Analyze the data! -------------------------------------------------------

# Summarise the trait values across the different levels of exercise habits. 
# Calculate both the mean (use the new variable name 'avg') and standard error
# of the mean (i.e., standard deviation divided by the square root of the 
# number of participants; use variable name 'sem'):
exer.avg <- ipip.comp %>% 
  group_by(exer,trait) %>% summarize(avg=mean(trait_mean), sd=sd(trait_mean), sem=sd/(sqrt(length(unique(ipip.comp$RID)))))

# If you properly created the exer.avg tibble above, the following code will 
# create a plot and save it as figures/exer.pdf. Check your figure with 
# figures/exer_answer.pdf to see if your data wrangling is correct!
dodge <- position_dodge(0.5)
ggplot(exer.avg,aes(x=trait,y=avg,colour=exer))+
  geom_pointrange(aes(ymin=avg-sem,ymax=avg+sem),
               position=dodge)+
  labs(x='big 5 trait',y='mean trait value',title='Big 5 and exercise')
ggsave('figures/exer.pdf',units='in',width=7,height=5)

# repeat the above summary commands for gender:
gender.avg <- ipip.comp %>% 
  group_by(gender,trait) %>% summarize(avg=mean(trait_mean), sd=sd(trait_mean), sem=sd/(sqrt(length(unique(ipip.comp$RID)))))

# create a gender plot and compare to the answer figure:
ggplot(gender.avg,aes(x=trait,y=avg,colour=gender))+
  geom_pointrange(aes(ymin=avg-sem,ymax=avg+sem),
                  position=dodge)+
  labs(x='big 5 trait',y='mean trait value',title='Big 5 and gender')
ggsave('figures/gender.pdf',units='in',width=5,height=5)

# For BMI, we need to recode the BMI continuous values into a categorical
# variable. Add a new BMI_cat variable to ipip.comp based on common definitions
# of BMI categories:
# <18.5=underweight, 18.5-25=healthy, 25-30=overweight, >30=obese
# HINT: check out the case_when function:
#     https://dplyr.tidyverse.org/reference/case_when.html
ipip.comp$BMI_cat <- 
  case_when(
    ipip.comp$BMI <18.5 ~ "underweight",
    ipip.comp$BMI >=18.5 & ipip.comp$BMI <= 25 ~ "healthy",
    ipip.comp$BMI >25 & ipip.comp$BMI <= 30 ~ "overweight",
    ipip.comp$BMI >30 ~ "obese",
    TRUE ~ as.character(ipip.comp$BMI)
  )

# turn BMI_cat into a factor and order it with levels
ipip.comp$BMI_cat <- factor(ipip.comp$BMI_cat, 
                            levels=c('underweight','healthy','overweight','obese'))

# summarise trait values by BMI categories  
bmi.avg <- ipip.comp %>% 
  group_by(BMI_cat,trait) %>% summarize(avg=mean(trait_mean), sd=sd(trait_mean), sem=sd/(sqrt(length(unique(ipip.comp$RID)))))

# create BMI plot and compare to the answer figure:
ggplot(bmi.avg,aes(x=trait,y=avg,colour=BMI_cat))+
  geom_pointrange(aes(ymin=avg-sem,ymax=avg+sem),
                  position=dodge)+
  labs(x='big 5 trait',y='mean trait value',title='Big 5 and BMI')
ggsave('figures/BMI.pdf',units='in',width=7,height=5)

# finally, use dplyr to calculate the correlation (use variable name 'corrcoef') 
# between age and the big 5
# NOTE: check out the cor() function by running ?cor in the console
age.avg <- ipip.comp %>% 
  group_by(age,trait,trait_mean) %>% summarize(avg=mean(trait_mean), corrcoef=cor(age,trait_mean)) # does not work; no sd
  
# create age plot and compare to the answer figure
ggplot(age.avg,aes(x=trait,y=corrcoef))+
  geom_hline(yintercept=0)+
  geom_point(size=3)+
  labs(x='big 5 trait',y='correlation between trait and age',title='Big 5 and age')
ggsave('figures/age.pdf',units='in',width=4,height=5) # does not work; no sd


