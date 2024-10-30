#Read in the csv files
#Create a vector consisting of selected variables
variables <- c("SERIALNO", "REGION", "ST", "ADJINC",  "PWGTP", "AGEP", "CIT",
               "COW", "MAR", "SCHL","SEX", "ESR", "PINCP", "POVPIP", "RAC1P")
#Read in 'psam_pusa.csv'
a <- fread("psam_pusa.csv", header = TRUE, select = variables, data.table = FALSE,
           stringsAsFactors = FALSE)
#Read in 'psam_pusb.csv'
b <- fread("psam_pusb.csv", header = TRUE, select = variables, data.table = FALSE,
           stringsAsFactors = FALSE)
#Read in 'psam_pusc.csv'
c <- fread("psam_pusc.csv", header = TRUE, select = variables, data.table = FALSE,
           stringsAsFactors = FALSE)
#Read in 'psam_pusd.csv'
d <- fread("psam_pusd.csv", header = TRUE, select = variables, data.table = FALSE,
           stringsAsFactors = FALSE)
#Combine 4 files
df <- rbind(a, b, c, d)
#Save combined file
fwrite(df,'population.csv')
#Remove unnecessary files
rm(a, b, c, d, df)

# Read in the file used for analysis
population <- fread("population.csv", header = TRUE, data.table = FALSE, stringsAsFactors = FALSE)
#Change the columns' names
colnames(population) <- c('Serial.No', 'Region', 'States', 'Inflation', 'Weight', 'Age', 'Citizenship', 'Worker.Class', 'Marital.Status','Education', 'Sex','Employment','Total.Income', 'Inc.Pov.Rate', 'Race')
#Create levels and labels for `Region`
region.levels <- c(1,2,3,4,9)
region.labels <- c('Northeast', 'Midwest', 'South', 'West', 'Puerto Rico')
#Create levels and labels for `States`
state.levels <- c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                  26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46,
                  47, 48, 49, 50, 51, 53, 54, 55, 56, 72)
state.labels <- c('alabama', 'alaska', 'arizona', 'arkansas', 'california', 'colorado', 'connecticut', 'delaware', 'district of columbia', 'florida', 'georgia', 'hawaii', 'idaho', 'illinois', 'indiana', 'iowa', 'kansas', 'kentucky', 'louisiana', 'maine', 'maryland',  'massachusetts', 'michigan', 'minnesota', 'mississippi', 'missouri', 'montana', 'nebraska', 'nevada', 'new hampshire', 'new jersey', 'new mexico', 'new york', 'north carolina', 'north dakota', 'ohio', 'oklahoma', 'oregon', 'pennsylvania', 'rhode island', 'south carolina', 'south dakota', 'tennessee', 'texas', 'utah', 'vermont', 'virginia', 'washington', 'west virginia', 'wisconsin', 'wyoming', 'puerto rico')
# Create citizenship levels and labels
cit.levels <- c(1,2,3,4,5)
cit.labels <- c( 'US Born', 'PR.Guam.VI.NM Born', 'Abroad Born', 'Naturalized Citizen', 'Non US')
# Create class of workers' levels and labels
worker.levels <- c(1,2,3,4,5,6,7,8,9)
worker.labels <-c('Private, for Profit', 'Private, Non-Profit', 'Local','State','Federal',
                  'Self-employed, Not Incor','Self-employed, Incor', 
                  'Family work, No Payment','Unemployed')
#Create levels and labels for Marital Status
MS.levels <- c(1,2,3,4,5)
MS.labels <- c('Married','Widowed','Divorced','Separated','Never married/<15')
# Create levels and labels for Education
edu.levels <- c(1,2,3,4,5,6,7,8,9,10, 11,12,13,14,15,16,17,18,19,20,21,22,23,24)
edu.labels <- c( 'No schooling', 'Preschool', 'Kindergarten', 'Grade 1', 'Grade 2', 'Grade 3', 
                 'Grade 4', 'Grade 5', 'Grade 6', 'Grade 7', 'Grade 8', 'Grade 9', 'Grade 10',
                 'Grade 11', 'Grade 12, No Diploma','HS Diploma', 'GED/alternative credential',
                 'College, 1y less','College, 1y over',  'Associate', 'Bachelor','Master',
                 'Professional, over bachelor','Doctorate')
# Create levels and labels for 'Employment status'
emp.levels <- c(1,2,3,4,5,6)
emp.labels <- c( 'Civilian employed, at work', 'Civilian employed, not at work','Unemployed',
                 'Armed forces, at work','Armed forces, not at work', 'Not in labor force')
# Create levels and labels for 'Race'
race.levels <- c(1,2,3,4,5,6,7,8,9)
race.labels <- c('White', 'Black/African American', 'American Indian', 'Alaska Native', 
                 'American Indian and/or Alaska Native', 'Asian', 
                 'Native Hawaiian & Other Pacific Islander', 'Some Other Race',
                 'Two/More Races')
# Create factor variables and factor level names
factor.pop <- population %>%
  mutate(Region = factor(Region, levels = region.levels , labels = region.labels),
         States = factor(States, levels = state.levels, labels = state.labels),
         Citizenship = factor(Citizenship, levels = cit.levels, labels = cit.labels),
         Worker.Class = factor(Worker.Class, levels = worker.levels, labels = worker.labels),
         Marital.Status= factor(Marital.Status, levels = MS.levels, labels = MS.labels),
         Education = factor(Education, levels = edu.levels, labels = edu.labels),
         Sex = factor(Sex, levels = c(1,2), labels = c('Male', 'Female')),
         Employment = factor(Employment, levels = emp.levels, labels = emp.labels),
         Race = factor(Race, levels = race.levels, labels = race.labels))
# Save factorized data into a .rds file
write_rds(factor.pop, "population.rds")

# Load the data
factor.pop <-read_rds("population.rds")
# Adjust income value with inflation rate
adjust.pop <- factor.pop %>%
  mutate(Total.Income = (Inflation / 1000000) * Total.Income)
# Collapse some categorical variables into more meaningful groups
clean.pop <- adjust.pop %>%
  mutate(Citizenship = fct_collapse(Citizenship, 
                                    US = c('US Born', 'PR.Guam.VI.NM Born', 'Abroad Born', 'Naturalized Citizen'),
                                    Non.US = 'Non US'),
         Worker.Class = fct_collapse(Worker.Class,
                                     Private = c('Private, for Profit', 'Private, Non-Profit'),
                                     Government = c('Local', 'State', 'Federal'),
                                     Self.Employed = c('Self-employed, Not Incor','Self-employed, Incor', 
                                                       'Family work, No Payment'),
                                     Unemployed = 'Unemployed'),
         Education = fct_collapse(Education,
                                  No.school = 'No schooling',
                                  Pre.Primary = c('Preschool', 'Kindergarten'),
                                  Primary = c('Grade 1', 'Grade 2', 'Grade 3', 'Grade 4', 'Grade 5', 'Grade 6'),
                                  Secondary = c('Grade 7', 'Grade 8', 'Grade 9', 'Grade 10', 'Grade 11', 
                                                'Grade 12, No Diploma','HS Diploma', 'GED/alternative credential'),
                                  College = c('College, 1y less', 'College, 1y over'),
                                  Bachelor = c('Associate', 'Bachelor'),
                                  Master = c('Master', 'Professional, over bachelor'),
                                  PhD = 'Doctorate'),
         Employment = fct_collapse(Employment,
                                   Civil.employed = c('Civilian employed, at work', 'Civilian employed, not at work'),
                                   Unemployed = 'Unemployed',
                                   Arm.force = c('Armed forces, at work','Armed forces, not at work'),
                                   Not.Labor.Force = 'Not in labor force'),
         Race = fct_collapse(Race,
                             White = 'White',
                             Black = 'Black/African American',
                             Asian = 'Asian',
                             Others = c( 'American Indian', 'Alaska Native', 'American Indian and/or Alaska Native',
                                         'Native Hawaiian & Other Pacific Islander', 'Some Other Race','Two/More Races')),
         Age.Group = case_when(Age <= 15 ~ 'Children', 
                               Age >=16 & Age <=23 ~ "Late Gen Z",
                               Age >= 24 & Age <= 39 ~ 'Millennial', 
                               Age >=40 & Age <= 55 ~ 'Baby Boomer',
                               Age > 55 ~ 'Senior'),
         Income.Group = case_when (Total.Income < 20000 ~ 'Very Low',
                                   Total.Income >= 20000 & Total.Income < 44999 ~ 'Low',
                                   Total.Income >= 45000 & Total.Income < 139999 ~ 'Middle',
                                   Total.Income >= 140000 & Total.Income < 149999 ~ 'Upper Middle',
                                   Total.Income >= 150000 ~ 'High')
  )

# Create levels for Age Group and Income Group
age.levels <- c('Children','Late Gen Z', 'Millennial', 'Baby Boomer','Senior')
inc.levels <- c('Very Low', 'Low', 'Middle', 'Upper Middle','High')
clean.pop <- clean.pop %>%
  mutate(Age.Group = factor(Age.Group, levels = age.levels),
         Income.Group = factor(Income.Group, levels = inc.levels))
# Generate proportion of the population by States 
state.tbl <- clean.pop %>%
  count(States, wt = Weight) %>%
  mutate(region = States, # Create 'region' column to match with map data
         Percent = 100*round(n/sum(n), digits = 3)) %>%
  arrange(desc(n))

# Proportion of the population under 1%
s1 <- state.tbl %>%
  filter(Percent < 0.5) %>%
  ggplot(aes(x= reorder(States, n), y = Percent, fill = States)) +
  geom_col() +
  labs(x = 'States',title = "% of Population under 0.5%") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = 'none') +
  coord_flip()

s2 <- state.tbl %>%
  filter(Percent >= 0.5, Percent < 1) %>%
  ggplot(aes(x= reorder(States, n), y = Percent, fill = States)) +
  geom_col() +
  labs(x = 'States',title = " % of Population from 0.5% to under 1%") +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.position = 'none') +
  coord_flip()
# Proportion of the population from 1% to 2%
s3 <- state.tbl %>%
  filter(Percent >= 1, Percent <= 2) %>%
  ggplot(aes(x= reorder(States, n), y = Percent, fill = States)) +
  geom_bar(stat = "identity") +
  labs(x = 'States',title = "% of Population from 1% to 2 %") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = 'none') +
  coord_flip()

# Proportion of the population over 2%
s4 <- state.tbl %>%
  filter(Percent > 2) %>%
  ggplot(aes(x= reorder(States, n), y = Percent, fill = States)) +
  geom_bar(stat = "identity") +
  labs(x = 'States',title = "% of Population over 2 %") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = 'none') +
  coord_flip()

# Combine 3 plots
plot_grid(s1, s2, s3, s4)

# Create in-line code
n1 <- nrow(state.tbl%>%filter(Percent < 0.5))
n2 <- nrow(state.tbl%>%filter(Percent >= 0.5, Percent < 1))
n3 <- nrow(state.tbl%>%filter(Percent >= 1, Percent <= 2))
n4 <- nrow(state.tbl%>%filter(Percent >2))
min.st <- min(state.tbl$Percent)
lo.st <- as.vector(filter(state.tbl,Percent == min(state.tbl$Percent))$States)
avg.low <- floor(mean(filter(state.tbl,Percent == min(state.tbl$Percent))$n))
hi.st <- as.vector(filter(state.tbl,Percent == max(state.tbl$Percent))$States)
max.st <- max(state.tbl$Percent)
p <- filter(state.tbl,Percent == max(state.tbl$Percent))$n
options(scipen = 999)

# Create map plot for states
states_map <- map_data("state")
# Calculate average longitude and latitude
state.map <- state.pop %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat), group = group)
# Join 2 dataframe by region
state.pop <- left_join(states_map, state.tbl, by = 'region')
# Plot the % of population by state
ggplot(state.pop , aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Percent), color = "white")+
  geom_text(aes(label = region), data = state.map,  size = 3, hjust = 0.5) +
  scale_fill_viridis_c(option = "C") +
  labs( x = 'Longtitude', y = 'Latitude', title = "Percentage of Population by State") +
  theme(plot.title = element_text(hjust = 0.5))

pop.region <- clean.pop %>%
  count(Region, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3))
# Create in-line code
hi.reg <- max(pop.region$Percent)
lo.reg <- min(pop.region$Percent)
# Calculate the percentage of population by region
r1 <- clean.pop %>%
  group_by(Sex) %>%
  count(Region, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3)) %>%
  ggplot(aes(x= reorder(Region, n), y = Percent, fill = Sex)) +
  geom_col(position = "dodge") +
  labs(x = 'Region',title = "Percentage of Population by Region and Sex") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  coord_flip()
# Calculate the percentage of population by region and race
r2 <- clean.pop %>%
  group_by(Citizenship) %>%
  count(Region, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3)) %>%
  ggplot(aes(x= reorder(Region, n), y = Percent, fill = Citizenship)) +
  geom_col(position = "dodge") +
  labs(x = 'Region',title = "Percentage of Population by Region and Sex") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  coord_flip()
plot_grid(r1,r2)

# Generate the percentage of population by Class of Worker
class <- clean.pop %>%
  count(Worker.Class, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3)) %>%
  arrange(desc(Percent))

# Create in-line code
pri <- filter(class, Worker.Class == 'Private')$Percent
gov <- filter(class, Worker.Class == 'Government')$Percent
self <- filter(class, Worker.Class == 'Self.Employed')$Percent
unem <- filter(class, Worker.Class == 'Unemployed')$Percent
na.cla <- filter(class, is.na(Worker.Class))$Percent

# Plot the percentage of population by Class of Worker
class %>%
  ggplot(aes(x= reorder(Worker.Class, n), y = Percent, fill = Worker.Class)) +
  geom_bar(stat = "identity") +
  labs(x = 'Class of Worker',title = "Percentage of Population by Class of Worker") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none') +
  coord_flip()
# Generate the percentage of population by Education Attainment
edu <- clean.pop %>%
  count(Education, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3))

# Create in-line code
ba <- filter(edu, Education == 'Bachelor')$Percent
co <- filter(edu, Education == 'College')$Percent
dip <- filter(edu, Education == 'HS Diploma')$Percent
ma <- filter(edu, Education == 'Master')$Percent
PhD <- filter(edu, Education == 'PhD')$Percent
no <- filter(edu, Education == 'No.school')$Percent
sec <- filter(edu, Education == 'Secondary')$Percent
na.edu <- filter(edu, is.na(Education))$Percent

# Plot the percentage of population by Education Attainment
edu %>%
  ggplot(aes(fct_inorder(Education), Percent, fill = Education)) +
  geom_bar(stat = "identity") +
  labs(x = 'Education',title = "Percentage of Population by Education Attainment") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  coord_flip()
# Generate the percentage of population by Marital Status
marital <- clean.pop %>%
  count(Marital.Status, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3))

# Create in-line code
never <- filter(marital, Marital.Status == 'Never married/<15')$Percent
mar <- filter(marital, Marital.Status == 'Married')$Percent
div <- filter(marital, Marital.Status == 'Divorced')$Percent
sum <- filter(marital, Marital.Status == 'Separated')$Percent + 
  filter(marital, Marital.Status == 'Widowed')$Percent

# Plot the percentage of population by Marital Status
marital %>%
  ggplot(aes(x= reorder(Marital.Status, n), y = Percent, fill = Marital.Status)) +
  geom_bar(stat = "identity") +
  labs(x = 'Marital Status',title = "Percentage of Population by Marital Status") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  coord_flip()
# Generate the percentage of population by Employment status
emp <- clean.pop %>%
  count(Employment, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3))

# Create in-line code
cil <- filter(emp, Employment == 'Civil.employed')$Percent
not.lf <- filter(emp, Employment == 'Not.Labor.Force')$Percent
unempl <- filter(emp, Employment == 'Unemployed')$Percent
arm <- filter(emp, Employment == 'Arm.force')$Percent 

# Plot the percentage of population by Marital Status
emp %>%
  ggplot(aes(x= reorder(Employment, n), y = Percent, fill = Employment)) +
  geom_bar(stat = "identity") +
  labs(x = 'Employment',title = "Percentage of Population by Employment") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  coord_flip()
# Summary statistics of Age
mean.age <- round(wtd.mean(clean.pop$Age, weights= clean.pop$Weight, normwt = FALSE), digits = 0)
qtl.age <- wtd.quantile(clean.pop$Age,weights= clean.pop$Age, normwt = FALSE)
q1.age <- qtl.age[2]
q3.age <- qtl.age[4]
sd.age <- round(sqrt(wtd.var(clean.pop$Age, weights= clean.pop$Weight, normwt = FALSE)), digits = 0)

# Generate the percentage of population by Age Group
age.grp <- clean.pop %>%
  count(Age.Group, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3))

# Generate the percentage of population by Age Group and Sex
age.grp.sex <- clean.pop %>%
  group_by(Sex) %>%
  count(Age.Group, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3))

# Create in-line code
sen <- filter(age.grp, Age.Group == 'Senior')$Percent
f.sen <- filter(age.grp.sex, Age.Group == 'Senior', Sex == 'Female')$Percent
m.sen <- filter(age.grp.sex, Age.Group == 'Senior', Sex == 'Male')$Percent
f.chi <- filter(age.grp.sex, Age.Group == 'Children', Sex == 'Female')$Percent
m.chi <- filter(age.grp.sex, Age.Group == 'Children', Sex == 'Male')$Percent

# Plot the percentage of population by Age group
a1 <- age.grp %>%
  ggplot(aes(fct_inorder(Age.Group), Percent, fill = Age.Group)) +
  geom_bar(stat = "identity") +
  labs(x = 'Age Group',title = "Percent of Population by Age Group") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "none") +
  coord_flip()
# Plot the percentage of population by Age group and Sex
a2 <- age.grp.sex %>%
  ggplot(aes(fct_inorder(Age.Group), Percent, fill = Sex)) +
  geom_col(position = 'dodge') +
  labs(x = 'Age Group',title = "% of Population by Age Group and Sex") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  coord_flip()
# Combine 2 plots
plot_grid(a1,a2)

# Summary statistics of Income
mean.inc <- round(wtd.mean(clean.pop$Total.Income, weights= clean.pop$Weight, 
                           normwt = FALSE, na.rm = TRUE), digits = 0)
qtl.inc <- round(wtd.quantile(clean.pop$Total.Income,weights= clean.pop$Weight, 
                              normwt = FALSE, na.rm = TRUE), digits = 0)
q1.inc <- qtl.inc[2]
q3.inc <- qtl.inc[4]
sd.inc <- round(sqrt(wtd.var(clean.pop$Total.Income, weights= clean.pop$Weight,
                             normwt = FALSE, na.rm = TRUE)), digits = 0)
# Number of NA's
na.inc <- nrow(clean.pop %>% filter(is.na(Total.Income)))

# Generate the percentage of population by Age Group
inc.grp <- clean.pop %>%
  count(Income.Group, wt = Weight) %>%
  mutate(Percent = 100 * round(n/sum(n), digits = 3))

# Generate the percentage of population by Age Group and Sex
inc.grp.sex <- clean.pop %>%
  group_by(Sex) %>%
  count(Income.Group, wt = Weight) %>%
  mutate(Percent = 100*round(n/sum(n), digits = 3))

# Create in-line code
vl <- filter(inc.grp, Income.Group == 'Very Low')$Percent
up <- filter(inc.grp, Income.Group == 'Upper Middle')$Percent
hi <- filter(inc.grp, Income.Group == 'High')$Percent

# Plot the percentage of population by Income group
i1 <- inc.grp %>%
  ggplot(aes(fct_inorder(Income.Group), Percent, fill = Income.Group)) +
  geom_bar(stat = "identity") +
  labs(x = 'Income Group',title = "% of Population by Income Group") +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = "none") +
  coord_flip()

i2 <- inc.grp.sex %>%
  ggplot(aes(fct_inorder(Income.Group), Percent, fill = Sex)) +
  geom_col(position = 'dodge') +
  labs(x = 'Income Group',title = "% of Population by Income Group and Sex") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  coord_flip()
plot_grid(i1,i2)

# Summary statistics of Income to Poverty Ratio
mean.pov <- round(wtd.mean(clean.pop$Inc.Pov.Rate, weights= clean.pop$Weight, 
                           normwt = FALSE, na.rm = TRUE), digits = 0)
qtl.pov <- round(wtd.quantile(clean.pop$Inc.Pov.Rate,weights= clean.pop$Weight, 
                              normwt = FALSE, na.rm = TRUE), digits = 0)
q1.pov <- qtl.pov[2]
q3.pov <- qtl.pov[4]
sd.pov <- round(sqrt(wtd.var(clean.pop$Inc.Pov.Rate, weights= clean.pop$Weight,
                             normwt = FALSE, na.rm = TRUE)), digits = 0)
# Number of NA's
na.pov <- nrow(clean.pop %>% filter(is.na(Inc.Pov.Rate)))

avg.inc.pov <- clean.pop %>%
  group_by(States) %>%
  summarise(Average.Ratio = mean(Inc.Pov.Rate, na.rm = TRUE))

# Create map plot of all states in USA and change the name of `region` column into ` States`
states_map <- map_data("state") %>%
  mutate(States = region)

# Calculate average longitude and latitude
state.map <- state.unem %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat), group = group)

# Join two dataframe by state
state.unem <- left_join(states_map, avg.inc.pov, by = 'States')

# Plot Income to Poverty Ratio by State
ggplot(state.unem , aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Average.Ratio), color = "white")+
  geom_text(aes(label = region), data = state.map,  size = 3, hjust = 0.5) +
  scale_fill_viridis_c(option = "D") +
  labs( x = 'Longtitude', y = 'Latitude', title = "Income to Poverty Ratio by State") +
  theme(plot.title = element_text(hjust = 0.5))

# Create inline codes
min.pov <- round(min(avg.inc.pov$Average.Ratio), digits = 0)
lo.pov <- as.vector(filter(avg.inc.pov, Average.Ratio == min(avg.inc.pov$Average.Ratio))$States)
hi.pov <- as.vector(filter(avg.inc.pov, Average.Ratio == max(avg.inc.pov$Average.Ratio))$States)
max.pov <- round(max(avg.inc.pov$Average.Ratio), digits = 0)

# Identify outliers for Total Income
inc.outlier <- clean.pop %>%
  identify_outliers(Total.Income)
# Calculate percentage of total income outliers
p.inc.out <- round(length(inc.outlier$Total.Income) / length(clean.pop$Total.Income), digits = 3)*100
# Identify outliers for Poverty
pov.outlier <- clean.pop %>%
  identify_outliers(Inc.Pov.Rate) # No outliers
# Filter out outliers of Total Income, and people under 16 years old and NA values for Income and Inc.Pov.Rate, Education, Worker.Class and Employment
filter.pop <- clean.pop %>%
  filter(Age >= 16,
         !is.na(Total.Income),
         !is.na(Inc.Pov.Rate),
         !is.na(Education),
         !is.na(Worker.Class),
         !is.na(Employment)) %>%
  anti_join(inc.outlier)
# Unemployment rate between States, Region and Sex
Unem.state <- filter.pop %>%
  group_by(States, Region, Sex) %>%
  count(Employment, wt = Weight) %>%
  spread(Employment, value = n) %>%
  summarise(Unemploy.rate = round(Unemployed/Civil.employed, digits = 3)*100)

# Average Unemployment rate between Region
Unem.state %>%
  group_by(Region, Sex) %>%
  summarise(avg.unem = mean(Unemploy.rate)) %>%
  ggplot(aes(reorder(Region, avg.unem), avg.unem, fill = Sex)) +
  geom_col(position = 'dodge')+
  labs(y = 'Average Unemployment Rate (%)', x = 'States',
       title = 'Average Unemployment Rate by Region and Sex')+
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  coord_flip()

# Create inline code
Unem.reg <- Unem.state %>%
  group_by(Region) %>%
  summarise(avg.unem = mean(Unemploy.rate))
hi.S <- round(max(Unem.reg$avg.unem), digits = 1)
lo.MW <- round(min(Unem.reg$avg.unem), digits = 1)
bw <- round(Unem.reg$avg.unem[Unem.reg$Region == 'Northeast'], digits = 1)

# Create inline codes
## The lowest and highest % of unemployment rate in the Northeast
lo.NE <- min(filter(Unem.state, Region == 'Northeast')$Unemploy.rate)
hi.NE <- max(filter(Unem.state, Region == 'Northeast')$Unemploy.rate)
## The lowest and highest % of unemployment rate in the South
lo.S <- min(filter(Unem.state, Region == 'South')$Unemploy.rate)
hi.S <- max(filter(Unem.state, Region == 'South')$Unemploy.rate)
## The lowest and highest % of unemployment rate in the Midwest
lo.MW <- min(filter(Unem.state, Region == 'Midwest')$Unemploy.rate)
hi.MW <- max(filter(Unem.state, Region == 'Midwest')$Unemploy.rate)
## The lowest and highest % of unemployment rate in the West
lo.W <- min(filter(Unem.state, Region == 'West')$Unemploy.rate)
hi.W <- max(filter(Unem.state, Region == 'West')$Unemploy.rate)
## States' names with the lowest and highest % of unemployment rate in the USA
lo.st.name <- as.vector(filter(Unem.state,Unemploy.rate == min(Unem.state$Unemploy.rate))$States)
lo.st.unem <- min(Unem.state$Unemploy.rate) # States with lowest % of unemployment rate
hi.st.name <- as.vector(filter(Unem.state,Unemploy.rate == max(Unem.state$Unemploy.rate))$States)
hi.st.unem <- max(Unem.state$Unemploy.rate) # States with highest % of unemployment rate

# Unemployment rate between States in Northeast region
p1 <- Unem.state %>%
  filter(Region == 'Northeast') %>%
  ggplot(aes(reorder(States,Unemploy.rate), Unemploy.rate, fill = States)) +
  geom_col()+
  labs(y = 'Unemployment Rate (%)', x = 'States',
       title = 'Unemployment Rate in Northeast')+
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = 'none') +
  coord_flip()

# Unemployment rate between States in Northeast region
p2 <- Unem.state %>%
  filter(Region == 'South') %>%
  ggplot(aes(reorder(States,Unemploy.rate), Unemploy.rate, fill = States)) +
  geom_col()+
  labs(y = 'Unemployment Rate (%)', x = 'States',
       title = 'Unemployment Rate in South')+
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = 'none') +
  coord_flip()

# Unemployment rate between States in Midwest region
p3 <- Unem.state %>%
  filter(Region == 'Midwest') %>%
  ggplot(aes(reorder(States,Unemploy.rate), Unemploy.rate, fill = States)) +
  geom_col()+
  labs(y = 'Unemployment Rate (%)', x = 'States',
       title = 'Unemployment Rate in Midwest') +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = 'none') +
  coord_flip()

# Unemployment rate between States in West region
p4 <- Unem.state %>%
  filter(Region == 'West') %>%
  ggplot(aes(reorder(States,Unemploy.rate), Unemploy.rate, fill = States)) +
  geom_col()+
  labs(y = 'Unemployment Rate (%)', x = 'States',
       title = 'Unemployment Rate in West')+
  theme(plot.title = element_text(hjust = 0.5, size = 12),legend.position = 'none' ) +
  coord_flip()

# Combine 4 plots
plot_grid(p1,p2,p3, p4)

# Create map plot of all states in USA and change the name of `region` column into ` States`
states_map <- map_data("state") %>%
  mutate(States = region)
# Calculate average longitude and latitude
state.map <- state.unem %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat), group = group)

# Join two dataframe by state
state.unem <- left_join(states_map, Unem.state, by = 'States')

# Plot unemployment rate by State
ggplot(state.unem , aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Unemploy.rate), color = "white")+
  geom_text(aes(label = region), data = state.map,  size = 3, hjust = 0.5) +
  scale_fill_viridis_c(option = "C") +
  labs( x = 'Longtitude', y = 'Latitude', title = "Unemployment rate by State") +
  theme(plot.title = element_text(hjust = 0.5))

# Unemployment rate by Race
Unem.race <- filter.pop %>%
  group_by(Race) %>%
  count(Employment, wt = Weight) %>%
  spread(Employment, value = n) %>%
  summarise(Unemploy.rate = round(Unemployed/Civil.employed, digits = 3)*100)

# Unemployment rate by Race and Sex
Unem.race.sex <- filter.pop %>%
  group_by(Race, Sex) %>%
  count(Employment, wt = Weight) %>%
  spread(Employment, value = n) %>%
  summarise(Unemploy.rate = round(Unemployed/Civil.employed, digits = 3)*100)
#Unemployment rate by Race and Sex
rac.sex <- Unem.race.sex %>%
  ggplot(aes(fct_relevel(reorder(Race,Unemploy.rate), 'Others'), Unemploy.rate, fill =  Sex)) +
  geom_col(position = 'dodge') +
  labs(y = 'Unemployment Rate (%)', x = 'Race', title = 'Unemployment Rate by Race and Sex') +
  theme(plot.title = element_text(hjust = 0.5, size = 12), legend.position = 'none') +
  coord_flip()

# Unemployment rate by Citizenship
Unem.cit <- filter.pop %>%
  group_by(Citizenship) %>%
  count(Employment, wt = Weight) %>%
  spread(Employment, value = n) %>%
  summarise(Unemploy.rate = round(Unemployed/Civil.employed, digits = 3)*100)
# Unemployment rate by Citizenship and Sex
Unem.cit.sex <- filter.pop %>%
  group_by(Citizenship, Sex) %>%
  count(Employment, wt = Weight) %>%
  spread(Employment, value = n) %>%
  summarise(Unemploy.rate = round(Unemployed/Civil.employed, digits = 3)*100)
# Plot Unemployment rate by Citizenship and Sex
cit.sex <- Unem.cit.sex %>%
  ggplot(aes(reorder(Citizenship,Unemploy.rate), Unemploy.rate, fill =  Sex)) +
  geom_col(position = 'dodge')+
  labs(y = 'Unemployment Rate (%)', x = 'Citizenship', title = 'Unemployment Rate by Citizenship')+
  theme(plot.title = element_text(hjust = 0.5, size = 12))
#Combine 2 plots
plot_grid(cit.sex, rac.sex)

# Create inline codes for citizenship
US.unem <- filter(Unem.cit, Unem.cit$Citizenship == 'US')$Unemploy.rate
non.unem <- filter(Unem.cit, Unem.cit$Citizenship == 'Non.US')$Unemploy.rate
# Create inline codes for citizenship
unem.Blk <- filter(Unem.race, Unem.race$Race == 'Black')$Unemploy.rate
unem.Wt <- filter(Unem.race, Unem.race$Race == 'White')$Unemploy.rate
unem.Asi <- filter(Unem.race, Unem.race$Race == 'Asian')$Unemploy.rate

# Unemployment rate by Education
Unem.edu <- filter.pop %>%
  group_by(Education) %>%
  count(Employment, wt = Weight) %>%
  spread(Employment, value = n) %>%
  summarise(Unemploy.rate = round(Unemployed/Civil.employed, digits = 3)*100)

# Plot unemployment rate by Education
Unem.edu %>%  
  ggplot(aes(Education, Unemploy.rate, fill =  Education)) +
  geom_col(position = 'dodge')+
  labs(y = 'Unemployment Rate (%)', x = 'Education', title = 'Unemployment Rate by Education')+
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none') +
  coord_flip()

# Create inline codes
hi.edu <-max(Unem.edu$Unemploy.rate) # Highest % of unemployment for education attainment
lo.edu <-min(Unem.edu$Unemploy.rate) # Lowest % of unemployment for education attainment

# Calculate the variance of each group 
aggregate(Unemploy.rate ~ Sex, data=Unem.state, var)

# Test for normality of Unemployment rate distribution 
sha.test <- shapiro.test(Unem.state$Unemploy.rate)
sha.test
p.unem.s <- sha.test$p.value 

# Test for normality of Unemployment rate distribution grouped by Sex
sha.test.f <- shapiro.test(Unem.state$Unemploy.rate[Unem.state$Sex == "Female"]) # For Female
sha.test.f
p.unem.f <- sha.test.f$p.value # P-value for Female 
sha.test.m <- shapiro.test(Unem.state$Unemploy.rate[Unem.state$Sex == "Male"]) # For Male
sha.test.f
p.unem.m <-sha.test.m$p.value # P-value for Female 

# Visualize normality of Unemployment rate distribution
qqnorm(Unem.state$Unemploy.rate)

# Visualize normality of Unemployment rate distribution between group
ggplot(Unem.state, aes(x = Unemploy.rate, fill = Sex)) +
  geom_histogram(binwidth=.5, alpha=1/2) +
  labs(x = 'Unemployment Rate', y = 'Probability', title = 'Distribution of Unemployment Rate by Sex')

# Weighted Independent t-test
x.sex <- Unem.state$Unemploy.rate[Unem.state$Sex == "Female"]
y.sex <- Unem.state$Unemploy.rate[Unem.state$Sex == "Male"]
t.mdl <- wtd.t.test(x = x.sex, y = y.sex, weight = filter.pop$Weight, 
                    weighty = filter.pop$Weight, mean1 = FALSE)
t.mdl
# Create inline code
t.sex <-  round(t.mdl$coefficients[1], digits = 2)  # t statistics
f.mean <- round(t.mdl$additional[2], digits = 2)    # Average unemployment rate for female
m.mean <- round(t.mdl$additional[3], digits = 2)    # Average unemployment rate for male
se.unem <-round(t.mdl$additional[4],digits = 2)     # Standard error
df.unem <-round(t.mdl$coefficients[2], digits = 2)  # Degree of freedom
p.unem <- round(t.mdl$coefficients[3], digits = 2)  # p_value

# Unemployment rate by States and Citizenship
unem.st.cit <- filter.pop %>%
  group_by(States, Citizenship) %>%
  count(Employment, wt = Weight) %>%
  spread(Employment, value = n) %>%
  summarise(Unemploy.rate = round(Unemployed/Civil.employed, digits = 3)*100)

# Calculate the variance of each group 
aggregate(Unemploy.rate ~ Citizenship, data = unem.st.cit, var)

# Test normality of Unemployment rate distribution for Citizenship
sha.test.cit <-shapiro.test(unem.st.cit$Unemploy.rate)
sha.test.cit
p.unem.cit <- sha.test.cit$p.value 

# Test for normality of Unemployment rate distribution grouped by Citizenship
sha.test.us <- shapiro.test(unem.st.cit$Unemploy.rate[unem.st.cit$Citizenship == "US"])
sha.test.us
p.unem.us <- sha.test.us$p.value # P-value for US
sha.test.non <- shapiro.test(unem.st.cit$Unemploy.rate[unem.st.cit$Citizenship == "Non.US"])
sha.test.non
p.unem.non <- sha.test.non$p.value # P-value for US

# Check normality of distribution
qqnorm(unem.st.cit$Unemploy.rate)

# Visualize the difference between groups
ggplot(unem.st.cit, aes(x = Unemploy.rate, fill = Citizenship)) +
  geom_density(alpha=1/2)+
  labs(x = 'Unemployment Rate', y = 'Probability', 
       title = 'Distribution of Unemployment Rate by Citizenship')

# Independent t-test
x <- unem.st.cit$Unemploy.rate[unem.st.cit$Citizenship == "US"]
y <- unem.st.cit$Unemploy.rate[unem.st.cit$Citizenship == "Non.US"]
t.cit <- wtd.t.test(x = x, y = y, weight = filter.pop$Weight, weighty = filter.pop$Weight,
                    mean1 = FALSE)
t.cit
# Create inline code
t.ci <- round(t.cit$coefficients[1], digits = 2)    # t statistics
us.cit <-  round(t.cit$additional[2], digits = 2)   # Average unemployment rate for US
non.cit <-  round(t.cit$additional[3], digits = 2)  # Average unemployment rate for Non US
se.cit <- round(t.cit$additional[4], digits = 2)    # Standard error
df.cit <-  round(t.cit$coefficients[2], digits = 2) # Degree of freedom
p.cit <-  round(t.cit$coefficients[3], digits = 2)  # p_value

# Create boxplot for income to poverty ratio grouped by race
qplot(Race, Inc.Pov.Rate, geom = "boxplot", data = filter.pop)

# Create boxplot for income to poverty ratio grouped by Citizenship
qplot(Citizenship, Inc.Pov.Rate, geom = "boxplot", data = filter.pop)

# Multiple regression model
pov.lm <- lm(Inc.Pov.Rate ~ Race + Citizenship, data = filter.pop, weights = Weight)
#Summary of regression model
summary(pov.lm)
ti.lm <- tidy(pov.lm)
ti.lm
gla.lm <- glance(pov.lm)
gla.lm
# Create inline codes:
p.intercept <- ti.lm$p.value[1] # p value of White people
p.black <- ti.lm$p.value[2] # p value of Black people
p.others<- ti.lm$p.value[3] # p value of Other people
p.asian <- ti.lm$p.value[4] # p value of Asian people
p.non.us <- ti.lm$p.value[5] # p value of Non US people
p.value <- gla.lm$p.value # p value of the model
df1 <- floor(gla.lm$df) # degree of freedom between groups
F_stat <- floor(gla.lm$statistic) # F Statistics
df2 <- floor(gla.lm$df.residual) # degree of freedom within groups
R.squa.per <- 100*round(gla.lm$adj.r.squared, digits = 3) # Adjusted R squared (%)
R.squared <- round(gla.lm$adj.r.squared, digits = 3) # Adjusted R squared
sigma <- round(gla.lm$sigma,1) 
est.black <-  round(ti.lm$estimate[2], digits = 1) # Difference between White and Black
est.others <- round(ti.lm$estimate[3], digits = 1) # Difference between White and Others
est.asian <-  round(ti.lm$estimate[4], digits = 1) # Difference between White and Asian
est.non.us <- round(ti.lm$estimate[5], digits = 1) # Difference between US and Non US

# Using diagnostic Plots to examine the assumptions for the regression model
par(mfrow = c(2,2))
plot(lm(Inc.Pov.Rate ~ Race + Citizenship, data = filter.pop, weights = Weight))

# Fit the simpler model with race as the only predictor variable
pov.lm.simple <- lm(Inc.Pov.Rate ~ Race, data = filter.pop, weights = Weight)
summary(pov.lm.simple)

# Compare the simple regression to the more complex model (original model)
ano <- anova(pov.lm.simple, pov.lm)
ano
p.ano <- ano$`Pr(>F)`[2]

