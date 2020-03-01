
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
  ...

# We need a column that identifies rows as belonging to a specific trait,
# but the column you created based on the trait items includes both trait
# and item (e.g., A_1, but we want A in a separate column from item 1).
# Make this happen with a separate command:
ipip.l <- ipip.l %>% 
  ...

# Calculate averages for each participant (coded as RID) and trait:
ipip.comp <- ipip.l %>% 
  ...


# Cleaning up the other variables -----------------------------------------

# Depending on how you solved the above steps, your ipip.comp ttibble may or may
# not have the age, gender, exer, BMI variables that we want to compare to the big 5. If
# they are missing, let's add them in by joining the original ipip tibble with
# ipip.comp tibble:
# HINT: use a select call on ipip to only select the columns that you want to
# merge with ipip.comp
ipip.comp <- ipip %>% 
  ...

# One last thing, our exercise variable is all out of order. Because it was read
# in as a character string, it is in alphabetical order. Let's turn it into a 
# factor and reorder the levels according to increasing frequency. Do this by 
# using the factor command and its levels argument:
ipip.comp$exer <- ...



# Analyze the data! -------------------------------------------------------

# Summarise the trait values across the different levels of exercise habits. 
# Calculate both the mean (use the new variable name 'avg') and standard error
# of the mean (i.e., standard deviation divided by the square root of the 
# number of participants; use variable name 'sem'):
exer.avg <- ipip.comp %>% 
  ...

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
  ...

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
ipip.comp <- ipip.comp %>% 
  ...
# turn BMI_cat into a factor and order it with levels
ipip.comp$BMI_cat <- ...

# summarise trait values by BMI categories  
bmi.avg <- ipip.comp %>% 
  ...  

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
  ...

# create age plot and compare to the answer figure
ggplot(age.avg,aes(x=trait,y=corrcoef))+
  geom_hline(yintercept=0)+
  geom_point(size=3)+
  labs(x='big 5 trait',y='correlation between trait and age',title='Big 5 and age')
ggsave('figures/age.pdf',units='in',width=4,height=5)




