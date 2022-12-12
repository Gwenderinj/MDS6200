#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
#install.packages('ggpubr')
#install.packages('packcircles')
#install.packages('circlize')
#install.packages('treemap')
#install.packages("plotly")
library(plotly)
library(ggthemes)
library(packcircles)
library(circlize)
library(tidyverse)
library(ggpubr)
library(treemap)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)

a18<-read.csv("data/2018.csv", header=T)
a19<-read.csv("data/2019.csv", header=T)
a20<-read.csv("data/2020.csv", header=T)
a21<-read.csv("data/2021.csv", header=T)
a22<-read.csv("data/2022.csv", header=T)


mymode <- function(x) {
  t <- table(x)
  names(t)[ which.max(t) ]
}



employed <- a22 %>% 
  filter(Q5=="No"& Q23!="Currently not employed")%>%
  dplyr::select(Q2, Q3, Q8, Q9, Q11, Q29, Q30, starts_with("Q31"), Q32, starts_with("Q42"),  
                Q4, starts_with("Q6"), starts_with("Q7"),starts_with("Q10") ,starts_with("Q12"),Q11, starts_with("Q13"), starts_with("Q14"), starts_with("Q15"), starts_with("Q17"), Q23, Q24, Q27, starts_with("Q19"), starts_with("Q20"),starts_with("Q28"), Q29, Q30,  starts_with("Q35"), starts_with("Q36"), starts_with("Q37"), starts_with("Q38"), starts_with("Q39"), starts_with("Q40"), starts_with("Q44"))
unique(employed$Q24)
##
###data for map
ma <- employed %>%
  dplyr::select(Q4, starts_with("Q10"))%>%
  pivot_longer(cols=starts_with("Q10"))%>%
  drop_na()

ma <- ma[ma$value!="",]
ma[ma == "Yes, the research made advances related to some novel machine learning method (theoretical research)"]= "Yes"
ma[ma == "Yes, the research made use of machine learning as a tool (applied research)"]= "Yes"

ma <- ma %>%
  count(value, Q4) %>%
  pivot_wider(names_from = value, values_from = n)

ma$hover <- with(ma, paste(Q4, '<br>', "No", No, "Yes", Yes))
ma$n <- ma$Yes+ma$No

m <- list(
  l = 10,
  r = 10,
  b = 10,
  t = 50,
  pad = 0
) #adjust the size of the map

##create all the data for first page
map22 <- a22 %>%
  filter(Q5=="No"& Q23!="Currently not employed")%>%
  dplyr::select(Q3, Q4, starts_with("Q10"), Q16, Q29)%>%
  pivot_longer(cols=starts_with("Q10"))%>%
  drop_na()
map22 <- map22[, -5]
map22 <- map22[map22$value!="",]
map22 <- map22[map22$Q16!="",]
map22 <- map22[map22$Q29!="",]
map22[map22 == "Yes, the research made advances related to some novel machine learning method (theoretical research)"]= "Yes"
map22[map22 == "Yes, the research made use of machine learning as a tool (applied research)"]= "Yes"

map21 <- a21 %>% 
  filter(Q5 != "Student"& Q5!="Currently not employed")%>%
  dplyr::select(Q2, Q3, Q15, Q23, Q25)%>%
  drop_na()
map20 <- a20 %>% 
  filter(Q5 != "Student"& Q5!="Currently not employed")%>%
  dplyr::select(Q2, Q3, Q15, Q22, Q24)%>%
  drop_na()
map19 <- a19 %>% 
  filter(Q5 != "Student"& Q5!="Not employed")%>%
  dplyr::select(Q2, Q3, Q8, Q23, Q10)%>%
  drop_na()
map18 <- a18 %>% 
  filter(Q5 != "Student"& Q5!="Not employed")%>%
  dplyr::select(Q1, Q3, Q9, Q25, Q10)%>%
  drop_na()

map21 <- map21[map21$Q15 != "",]
map21 <- map21[map21$Q23 != "",]
map21 <- map21[map21$Q25 != "",]

map20 <- map20[map20$Q15 != "",]
map20 <- map20[map20$Q22 != "",]
map20 <- map20[map20$Q24 != "",]

map19 <- map19[map19$Q8 != "",]
map19 <- map19[map19$Q10 != "",]
map19 <- map19[map19$Q23 != "",]

map18 <- map18[map18$Q9 != "",]
map18 <- map18[map18$Q10 != "",]
map18 <- map18[map18$Q25 != "",]


map21[map21 == "I do not use machine learning methods"]= 0
map21[map21 == "No (we do not use ML methods)"]= "No"
map21[map21 == "We use ML methods for generating insights (but do not put working models into production)"]= 'Yes'
map21[map21 == "I do not know"]= "No"
map21[map21 == "We are exploring ML methods (and may one day put a model into production)"]= 'Yes'
map21[map21 == "We have well established ML methods (i.e., models in production for more than 2 years)"]= "Yes"
map21[map21 == "We recently started using ML methods (i.e., models in production for less than 2 years)"]= 'Yes'
map21[map21 == "I do not know"]= "No"

map20[map20 == "I do not use machine learning methods"]= 0
map20[map20 == "No (we do not use ML methods)"]= "No"
map20[map20 == "We use ML methods for generating insights (but do not put working models into production)"]= 'Yes'
map20[map20 == "I do not know"]= "No"
map20[map20 == "We are exploring ML methods (and may one day put a model into production)"]= 'Yes'
map20[map20 == "We have well established ML methods (i.e., models in production for more than 2 years)"]= "Yes"
map20[map20 == "We recently started using ML methods (i.e., models in production for less than 2 years)"]= 'Yes'
map20[map20 == "I do not know"]= "No"

map19[map19 == "I do not use machine learning methods"]= 0
map19[map19 == "No (we do not use ML methods)"]= "No"
map19[map19 == "We use ML methods for generating insights (but do not put working models into production)"]= 'Yes'
map19[map19 == "I do not know"]= "No"
map19[map19 == "We are exploring ML methods (and may one day put a model into production)"]= 'Yes'
map19[map19 == "We have well established ML methods (i.e., models in production for more than 2 years)"]= "Yes"
map19[map19 == "We recently started using ML methods (i.e., models in production for less than 2 years)"]= 'Yes'
map19[map19 == "I do not know"]= "No"

map18[map18 == "I have never studied machine learning but plan to learn in the future"]= 0
map18[map18 == "No (we do not use ML methods)"]= "No"
map18[map18 == "We use ML methods for generating insights (but do not put working models into production)"]= 'Yes'
map18[map18 == "I do not know"]= "No"
map18[map18 == "We are exploring ML methods (and may one day put a model into production)"]= 'Yes'
map18[map18 == "We have well established ML methods (i.e., models in production for more than 2 years)"]= "Yes"
map18[map18 == "We recently started using ML methods (i.e., models in production for less than 2 years)"]= 'Yes'
map18[map18 == "I do not know"]= "No"
map18[map18 == "I do not wish to disclose my approximate yearly compensation"]= -1

map21 <- map21[-1, ]
map20 <- map20[-1, ]
map19 <- map19[-1, ]
map18 <- map18[-1, ]

map22$year <- rep(2022)
map21$year <- rep(2021)
map20$year <- rep(2020)
map19$year <- rep(2019)
map18$year <- rep(2018)

ma21 <- map21 %>%
  count(Q23, Q3) %>%
  pivot_wider(names_from = Q23, values_from = n)
ma20 <- map20 %>%
  count(Q22, Q3) %>%
  pivot_wider(names_from = Q22, values_from = n)
ma19 <- map19 %>%
  count(Q8, Q3) %>%
  pivot_wider(names_from = Q8, values_from = n)
ma18 <- map18 %>%
  count(Q10, Q3) %>%
  pivot_wider(names_from = Q10, values_from = n)

ma21$year <- rep(2021)
ma20$year <- rep(2020)
ma19$year <- rep(2019)
ma18$year <- rep(2018)

colnames(ma21)[1] <- 'country'
colnames(ma20)[1] <- 'country'
colnames(ma19)[1] <- 'country'
colnames(ma18)[1] <- 'country'
map_total <- rbind(ma21, ma20, ma19, ma18)
map_total$hover <- with(map_total, paste(country, '<br>', "No", No, "Yes", Yes))
map_total$n <- map_total$No + map_total$Yes
ma$year <- rep(2022)
colnames(ma)[1] <- 'country'
map_to <- rbind(map_total, ma)#data for map

#data for the rest of the first page
colnames(map22)[1:5] <- c('gender', 'country', 'MLusing', 'compensate', 'MLadopt')
colnames(map21)[1:5] <- c('gender', 'country', 'MLusing', 'MLadopt', 'compensate')
colnames(map20)[1:5] <- c('gender', 'country', 'MLusing', 'MLadopt', 'compensate')
colnames(map19)[1:5] <- c('gender', 'country', 'MLadopt', 'MLusing', 'compensate')
colnames(map18)[1:5] <- c('gender', 'country', 'compensate', 'MLusing', 'MLadopt')

page1 <- rbind(map22, map21, map20, map19, map18)
page1[page1 == 'Female'] = 'Woman'
page1[page1 == 'Male'] = 'Man'

##
page1[page1 == '< 1 years'| page1 == 'Under 1 year'] = '< 1 year'
page1[page1 == 'I have never studied machine learning and I do not plan to'| page1 == 'I do not use machine learning methods'] = '0'
page1[page1 == '20 or more years'] = '20+ years'
page1[page1 == '10-15 years'] = '10-20 years'
page1[page1 == 'Prefer not to say'| page1 == 'Prefer to self-describe'|page1 == 'Nonbinary'] = 'Not say'


page1_1 = page1 %>%
  filter(MLadopt!="No")%>%
  dplyr::select(gender, country, year, compensate)%>%
  filter(gender%in%c("Woman","Man"))%>%
  group_by(gender,country, year)%>%
  summarise(mode1=mymode(compensate))


page1_1[page1_1 == "$0-999"]= "0-999"
page1_1[page1_1 == "-1"]= "0-0"

page1_0 <- page1_1 %>% 
  separate(col = mode1, sep = "-", into = c("l", "r")) 

page1_0$l = as.numeric(gsub(",", "", page1_0$l))
page1_0$r = as.numeric(gsub(",", "", page1_0$r))
page1_0$ave_com <- (page1_0$l + page1_0$r)/2



######data for line


tmp2 = employed %>%
  filter(Q10_3!="No")%>%
  dplyr::select(Q3,Q4, Q29, Q30, Q11)%>%
  filter(Q3%in%c("Woman","Man"))%>%
  group_by(Q4,Q3,Q11)%>%
  summarise(mode1=mymode(Q29), mode2=mymode(Q30))

tmp2$Q3 = factor(tmp2$Q3, levels=c("Woman", "Man"))

lev_exp = c("I have never written code","< 1 years","1-3 years","3-5 years","5-10 years","10-20 years","20+ years")
tmp2$Q11 = factor(tmp2$Q11, levels=lev_exp)

ord_sal = order(as.numeric(sub(",", "", str_extract(unique(tmp2$mode1), "(?<=\\$)(.*?)(?=[-|+])"))))
lev_sal = unique(tmp2$mode1)[ord_sal]
tmp2$mode1 = factor(tmp2$mode1, levels=lev_sal)

ord_sal1 = order(as.numeric(sub(",", "", str_extract(unique(tmp2$mode2), "(?<=\\$)(.*?)(?=[-|+])"))))
lev_sal1 = unique(tmp2$mode2)[ord_sal1]
tmp2$mode2 = factor(tmp2$mode2, levels=lev_sal1)

####data for platform

dfm = employed %>%
  dplyr::select(starts_with("Q6"), Q8)%>%
  pivot_longer(starts_with("Q6"))%>%
  drop_na()%>%
  dplyr::select(-name)%>%
  count( value, Q8)

dfm <- dfm[dfm$value!="",]
dfm <- dfm[dfm$Q8!="",]
dfm[dfm == "Cloud-certification programs (direct from AWS, Azure, GCP, or similar)"]= "Cloud-certification programs"
dfm[dfm == "University Courses (resulting in a university degree)"]= "University Courses"



#
tmp2%>%ggplot(aes(x=Q11, y=mode, shape=Q3,color=Q3))+
  geom_point( size=5)+
  facet_grid(~Q4)+
  theme_minimal()+
  theme(panel.spacing = unit(2, "lines"), axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.9, size=13),
        axis.text.y = element_text(size=13), strip.text.x =element_text(size=14), title =element_text(size=14))+
  xlab("") +
  ylab("")+
  ggtitle("The most frequent range per country, experience and gender")
####data for page 2
page2 <- employed[employed$Q8!="",]
page2 <- page2[page2$Q9!="",]
page2 <- page2%>%
  dplyr::select(Q2, Q3, Q8, Q9, Q29)%>%
  group_by(Q2, Q3, Q8, Q9, Q29)%>%
  count()

page2[page2 == "$0-999"]= "0-999"
page2[page2 == ""]= "0-0"

page2 <- page2 %>% 
  separate(col = Q29, sep = "-", into = c("l", "r")) 

page2$l = as.numeric(gsub(",", "", page2$l))
page2$r = as.numeric(gsub(",", "", page2$r))
page2$ave_com <- (page2$l + page2$r)/2

page2 <- page2 %>%
  drop_na()


######data for density
tmp = employed%>%
  mutate(Q27=recode(Q27,"We recently started using ML methods (i.e., models in production for less than 2 years)"="use ML",
                    "We have well established ML methods (i.e., models in production for more than 2 years)"="use ML",
                    "We are exploring ML methods (and may one day put a model into production)"="use ML",
                    "I do not know"="don't use ML",
                    "We use ML methods for generating insights (but do not put working models into production)"="use ML",
                    "No (we do not use ML methods)"="don't use ML"))%>%
  dplyr::select(starts_with("Q27"), starts_with("Q24"))%>%
  drop_na()%>%
  group_by(Q24, Q27)%>%
  count()%>%
  group_by(Q24)%>%
  mutate(percent = n/sum(n))
tmp <- tmp[tmp$Q24!="",]
tmp <- tmp[tmp$Q27!="",]
#####data for radar chart

set.seed(32)
options(repr.plot.width=16, repr.plot.height=14,  repr.plot.res = 200)
tmp1 = dplyr::select(employed, union(starts_with("Q12"), starts_with("Q23")))%>%
  pivot_longer(starts_with("Q12"))%>%
  drop_na()%>%
  dplyr::select(-name)%>%
  count( value, Q23)
tmp1[tmp1 == "Data Analyst (Business, Marketing, Financial, Quantitative, etc)"]= "Data Analyst"
tmp1[tmp1 == "Data Administrator"]= "Data Admin"
tmp1[tmp1 == "Developer Advocate"]= "Developer"
tmp1[tmp1 == "Engineer (non-software)"]= "Engineer"
tmp1[tmp1 == "Machine Learning/ MLops Engineer"]= "ML/MLops Engineer"
tmp1[tmp1 == "Manager (Program, Project, Operations, Executive-level, etc)"]= "Manager"

tmp1 <- tmp1[tmp1$value!="",]
tmp1 <- tmp1[tmp1$Q23!="",]

##radar plot for tmp

fig <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) 
fig <- fig %>%
  add_trace(
    r = as.numeric(tmp1[tmp1$Q23=='Data Analyst',]$n),
    theta = unique(tmp1[tmp1$Q23=='Data Analyst',]$value),
    name = 'Data Analyst'
  ) 
fig <- fig %>%
  add_trace(
    r = as.numeric(tmp1[tmp1$Q23=='Data Admin',]$n),
    theta = unique(tmp1[tmp1$Q23=='Data Admin',]$value),
    name = 'Data Admin'
  ) 
fig <- fig %>%
  add_trace(
    r = as.numeric(tmp1[tmp1$Q23=='Developer',]$n),
    theta = unique(tmp1[tmp1$Q23=='Developer',]$value),
    name = 'Developer'
  )
fig <- fig %>%
  add_trace(
    r = as.numeric(tmp1[tmp1$Q23=='Engineer',]$n),
    theta = unique(tmp1[tmp1$Q23=='Engineer',]$value),
    name = 'Engineer'
  )
fig <- fig %>%
  add_trace(
    r = as.numeric(tmp1[tmp1$Q23=='ML/MLops Engineer',]$n),
    theta = unique(tmp1[tmp1$Q23=='ML/MLops Engineer',]$value),
    name = 'ML/MLops Engineer'
  )
fig <- fig %>%
  add_trace(
    r = as.numeric(tmp1[tmp1$Q23=='Manager',]$n),
    theta = unique(tmp1[tmp1$Q23=='Manager',]$value),
    name = 'Manager'
  )

# 定义侧边栏
sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
        menuItem(text = "Overview",  # item名字
                               tabName = "Overview", # 传递到tab的变量名称
                               icon = icon("chart-line")),
        menuItem(text = "Learning tools", 
                                tabName = "tools", # 传递到tab的变量名称
                                icon = icon("boxes")), # 徽章标签和颜色 
        # 增加百度搜索
        menuItem(text = "Work Force", 
                 tabName = "work",
                 icon = icon("images"))),
        
        
        conditionalPanel(condition = "input.menu1 == 'Overview'",
                         tags$hr(),
                         h4("Year"),
                         p("Select a year!"),
                         selectInput(
                           inputId = "Year",
                           label = "Year",
                           choices = c('2022', '2021', '2020', '2019', '2018'),
                           selected = '2022'
                         )           
        ),
    conditionalPanel(condition = "input.menu1 == 'Learning tools'",
                     tags$hr(),
                     h4("Industry"),
                     p("Select Industry!"),
                     checkboxGroupInput(
                       inputId = "scatterVars",
                       label = "Industry",
                       choices = 'All industries',
                       selected = 'All industries'
                     )           
    )
  )

tab1 <- fluidPage(box(plotlyOutput("plot1"),width = 12, title = 'Map of ML adoption for year'))



tab1_3 <- fluidRow(
  box(plotlyOutput('plot1_2'), 
      width = 8, height = 500, title = 'Distribution of ML experience for year'),
                          valueBoxOutput("femaleBox"),
                          valueBoxOutput("maleBox"))

tab1_4 <- fluidRow(box(plotlyOutput('plot1_3'), width = 8, title = 'Mode of the compensation'),
                   box(width = 4, 
                       br(),
                       selectInput(
                         inputId = "Country",
                         label = "Country",
                         choices = unique(page1$country),
                         selected = 'Canada'))
                   )


tab2 <- fluidRow(box(plotlyOutput("plot2"),width = 12, title = 'Learning platform in use'))

tab2_2 <- fluidRow(box(plotlyOutput('plot2_2'), width = 8,title = 'Paper publishment'),
                   box(width = 4,
                       br(),
                       selectInput(
                         inputId = "Age",
                         label = "Age",
                         choices = unique(page2$Q2),
                         selected = '25-29'),
                       selectInput(
                         inputId = "Gender",
                         label = "Gender",
                         choices = unique(page2$Q3),
                         selected = 'Male'),
                       selectInput(
                         inputId = "Degree",
                         label = "Degree",
                         choices = unique(page2$Q8),
                         selected = 'Master’s degree'),
                       selectInput(
                         inputId = "Ave_compensate",
                         label = "Ave_compensate",
                         choices = unique(page2$ave_com),
                         selected = '1499.5')
                       ))

tab3 <- fluidRow(box(plotlyOutput("plot3"),width = 12, title = 'The proportion of using ML in each industry'),
  box(plotlyOutput("plot4"), width = 12, title = 'radar plot for ML tools'))
  
  



# 定义主体
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "Overview",
                h2("Overview"),
                p("The Machine Learning & Data Science Survey data set is an industry-wide survey launched by Kaggle from 09/16/2022 to 10/16/2022, which is aimed to presents a comprehensive view of the state of data science and machine learning. The dataset have 23779 observations and 296 columns that responding to 43 questions as the responses to multiple choice questions are splited into multiple columns."),
                
                p("The provided data set mainly contains: "),
                p("*Basic information of survey participants such as gender, reigen, age, degree, etc;"),
                p("*Information of learning tools that the learning platform they began with and the recommendation;"),
                p("*Industry information about the machine learning adoption in their company, analitical tools and environment they use for their daily works;"),
                tags$br(),
                
                p(""),
                # 根据menuItem中的tabName进行联动
                p(''),
                tab1,
                tab1_3,
                tab1_4),
        
                
          
                #, # 插入选项卡内容 
        tabItem(tabName = "tools", 
                h2('Learning tools'),
                br(),
                p(),
                tab2,
                tab2_2
      ),
      tabItem(tabName = "work",
              h2('Work Force'),
              p(),
              tab3,
              p("Ethical problem:"),
              p("The authenticity of the data, the dashboard mainly shows the non-students and employed people, but some people may fill it in at will, and in terms of working age, people may not know how long they have been exposed to machine learning, resulting in data information and  Reality doesn' match.")
              )
      
              )
  )

# 组合在一起

ui <- dashboardPage(skin="yellow",
  dashboardHeader(title = "Machine Learning & Data Science Survey", titleWidth = 500),
  sidebar,
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # 定义反应表达式，产生数据
  
  
    
    output$plot1 <- renderPlotly({ # 内部可以插入计算代码
      df = data.frame(map_to)
      df.subset = reactive({ #subset data based on chosen year
        a = subset(df, year == input$Year) 
        return(data.frame(a))
      })
      plot_ly(df.subset(), type='choropleth', 
              locations=df.subset()$country, 
              locationmode="country names", z=df.subset()$n, 
              text=df.subset()$hover, sizes = c(200, 300), size=20, 
              colorscale="Yellow", width = 600)%>% 
        layout(margin = m, title=paste(input$Year))
      
    }
    )
    
    output$femaleBox <- renderValueBox({
      df = data.frame(page1[page1$MLadopt == 'Yes', ])
      df.subset = reactive({ #subset data based on chosen year
        a = subset(df, year == input$Year) 
        return(data.frame(a))
      })
      f <-  round(length(df.subset()$gender[df.subset()$gender == 'Woman'])/length(df.subset()$gender), 4)
      valueBox(subtitle = "Female(%)", value = f, icon = icon("person-dress"), color = 'black')
    })
    output$maleBox <- renderValueBox({
      df = data.frame(page1)
      df.subset = reactive({ #subset data based on chosen year
        a = subset(df, year == input$Year) 
        return(data.frame(a))
      })
      male <- round(length(df.subset()$gender[df.subset()$gender == 'Man'])/length(df.subset()$gender), 4)
      valueBox(subtitle = "Male(%)", value = male,icon = icon("person"), color = 'black')
    })
    
    output$plot1_2 <- renderPlotly({
      df = data.frame(page1)
      df.subset = reactive({ #subset data based on chosen year
        a = subset(df, year == input$Year) 
        return(data.frame(a))
      })
      
      plot_ly(x = ~df.subset()$MLusing, type = "histogram", 
              color = df.subset()$gender, colors =c('black', '#E69F00'), width = 400, )%>% 
        layout(autosize=F, title=paste(input$Year), xaxis = list(categoryorder = "array",
                                                     categoryarray = c("0", "< 1 year", "1-2 years", "2-3 years", "3-4 years", "4-5 years", "5-10 years", "10-20 years", "20+ years")))
      
    })
    
    output$plot1_3 <- renderPlotly({
      df <- page1_0
      df.subset = reactive({
        a <- df[df$year <= input$Year,]
        a = a[a$country == input$Country,]
        return(data.frame(a))
      })
      plot_ly(df.subset(), x = df.subset()$year, y = df.subset()$ave_com, color = df.subset()$gender,
              colors =c('black', '#E69F00'), type = 'scatter', mode = 'lines+markers')%>%
        layout(title = paste(input$Country))
      
    })
    
    output$plot2 <- renderPlotly({ # 内部可以插入计算代码
      ggplotly(ggdotchart(dfm, x = "value", y = "n",
                          color = "Q8",                                # Color by groups
                          palette = c('black', '#999999',  'darkgoldenrod4', 'darkgoldenrod3','darkgoldenrod1', '#E69F00', 'burlywood'), # Custom color palette
                          sorting = "descending",                       # Sort value in descending order
                          add = "segments",                             # Add segments from y = 0 to dots
                          rotate = TRUE,                                # Rotate vertically
                          size = "n",
                          ggtheme = theme_pubr()                        # ggplot2 theme
      ) + theme(axis.text = element_text(size = 7), legend.text = element_text(size = 5))
      
      
      )
    }
    )
    
    output$plot2_2 <- renderPlotly({
      df <- page2
      df.subset = reactive({
        a <- df[df$Q2 == input$Age,]
        a = a[a$Q3 == input$Gender,]
        a = a[a$Q8 == input$Degree,]
        a = a[a$ave_com == input$Ave_compensate,]
        return(data.frame(a))
      })
      plot_ly(df.subset(), labels = ~Q9, values = ~n,
              marker = list(colors = c('Yes' = '#FF7F0E',
                                       'No' = 'black')),
              hole = 0.6, type = 'pie')
      
    })
    
    output$plot3 <- renderPlotly({ # 内部可以插入计算代码
      ggplotly(ggplot(data = tmp, aes(x = Q24, y=percent, fill=Q27)) +
        geom_bar(stat="identity" ,alpha = 1, position="stack") +
        rotate()+
        theme_minimal()+ 
        theme(axis.text = element_text(size=12), legend.text =element_text(size=11))+
        scale_fill_manual(values = c("black","orange"))+
        xlab("Industry") +
        ylab("Ratio")+
        labs(fill = "")+ 
        theme(axis.text = element_text(size = 7))
       )
      }
      )
    output$plot4 <- renderPlotly({
      plot_ly(
        type = 'scatterpolar',
        fill = 'toself', size = 20
      ) %>%
        add_trace(
          r = as.numeric(tmp1[tmp1$Q23=='Data Analyst',]$n),
          theta = unique(tmp1[tmp1$Q23=='Data Analyst',]$value),
          name = 'Data Analyst'
        ) %>%
        add_trace(
          r = as.numeric(tmp1[tmp1$Q23=='Data Admin',]$n),
          theta = unique(tmp1[tmp1$Q23=='Data Admin',]$value),
          name = 'Data Admin'
        ) %>%
        add_trace(
          r = as.numeric(tmp1[tmp1$Q23=='Developer',]$n),
          theta = unique(tmp1[tmp1$Q23=='Developer',]$value),
          name = 'Developer'
        )%>%
        add_trace(
          r = as.numeric(tmp1[tmp1$Q23=='Engineer',]$n),
          theta = unique(tmp1[tmp1$Q23=='Engineer',]$value),
          name = 'Engineer'
        )%>%
        add_trace(
          r = as.numeric(tmp1[tmp1$Q23=='ML/MLops Engineer',]$n),
          theta = unique(tmp1[tmp1$Q23=='ML/MLops Engineer',]$value),
          name = 'ML/MLops Engineer'
        )%>%
        add_trace(
          r = as.numeric(tmp1[tmp1$Q23=='Manager',]$n),
          theta = unique(tmp1[tmp1$Q23=='Manager',]$value),
          name = 'Manager'
        )
      })
  
    
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
