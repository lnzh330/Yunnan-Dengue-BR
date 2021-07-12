####Load packages
packages<-c('openxlsx','ggplot2','dplyr','scales','ggtree','treeio','networkD3',
            'webshot','htmlwidgets')
lapply(packages, library, character.only = TRUE)

##########################################Fig1##########################################
####Fig1-A
fig1A<-read.xlsx('Figure.xlsx',sheet='1A')
fig1A$Year<-as.Date(fig1A$Year,origin='1900-01-01')
yni<-filter(fig1A,type=='import')
ynt<-filter(fig1A,type=='total')

ggplot()+geom_point(ynt,mapping=aes(Year,number,group=1),fill='#6F6F6F',size=5,shape=21)+
  geom_line(ynt,mapping=aes(Year,number,group=1),colour='#6F6F6F',lwd=1.2)+
  geom_point(yni,mapping=aes(Year,number,group=1),fill='#F66134',size=5,shape=21)+
  geom_line(yni,mapping=aes(Year,number,group=1),colour='#F66134',lwd=1.2)+
  theme(axis.text.x = element_text(vjust=1,hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x.bottom =element_text(size = 16 ,family = 'sans',hjust = - 0.5), 
        axis.text.y = element_text(size = '16',family = 'sans'),
        axis.title.y.right = element_text(angle=90),
        panel.background = element_blank(),
        legend.position = '',
        axis.title = element_text(family = 'sans',size = 16))+
  scale_x_date(breaks = '1 year',labels = date_format('%Y'),
               expand =c(0.025,0.025),date_minor_breaks = '1 year',
               limits = c(as.Date('2012-12-31'),as.Date('2020-12-30')))+
  scale_y_continuous(name = c('Number of Cases \n in Yunnan'),
                     expand = c(0.03,1))+
  xlab(label = '')

####Fig1-B
fig1B<-read_xlsx('Figure.xlsx',sheet='1B')
fig1B$Month<-as.Date(fig1B$Month,origin='1900-01-01')

ggplot(fig1B,aes(Month,Incidence,fill=Country)) + geom_line() + theme_bw() +
  geom_ribbon(mapping = aes(ymax=Incidence2,ymin=0,fill=Country),alpha=0.6)+
  theme(axis.text.x = element_text(vjust=1,hjust = 0.5),
        axis.line = element_line(colour = "black"),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x.bottom =element_text(size = 16,family = 'sans',hjust = -0.8), 
        axis.text.y = element_text(size = '16',family = 'sans'),
        axis.title.y.right = element_text(angle=90),
        panel.border = element_rect(size = 1.2),
        panel.background = element_blank(),legend.position = '',
        axis.title = element_text(family = 'sans',size = 16))+
  scale_x_date(breaks = '1 year',labels = date_format('%Y'),
               expand =c(0.025,0.025),date_minor_breaks = '1 year',
               limits = c(as.Date('2012-12-31'),as.Date('2020-12-31')))+
  scale_y_continuous(name = c('Incidence Rate \n (per 100,000)'),
                     expand = c(0.02,0.05),breaks = c(50,100,150,200,250))+
  scale_fill_manual(values = c('#AED4EB','#F8DC7B','#D6D2CE','#4A4A8F')) +
  xlab(label = '')

####Fig1-D
fig1D<-read.xlsx('Figure.xlsx',sheet = '1D')
fig1Dyn<-filter(fig1D,Type=='Import'| fig1D$Type=='Local')
fig1Dyn$Month<-factor(fig1Dyn$Month,levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
fig1Dca<-filter(fig1D,Type=='Monthly')
fig1Dca$Month<-factor(fig1Dca$Month,levels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))

ggplot()+
  geom_bar(data=fig1Dyn,mapping=aes(x=Month,y=Cases,fill=Type),
           stat = 'identity',position = 'fill',alpha=0.9)+
  geom_point(data=fig1Dca,mapping = aes(x=Month,y=Cases/590,group =1),size=3)+
  geom_line(data=fig1Dca,mapping=aes(x=Month,y=Cases/590,group=1),size=2) +
  theme_bw()+scale_fill_manual(values = c('#7C99C9','#ED9D85')) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text = element_text(family = 'sans',size = 16),
        axis.title = element_text(family = 'sans',size = 16),
        axis.title.y.right = element_text(angle = 90),legend.position = '') +
  scale_y_continuous(expand = c(0,0.01),
                     sec.axis = sec_axis(~.*590,name = 'Monthly Average Cases \n in Yunnan')) + 
  xlab('') + 
  ylab('Proportion of Imported and Local cases \n in Yunnan')

##########################################Fig2##########################################
c1= rgb(102,194,165, maxColorValue = 255)
c2= rgb(252,141,98, maxColorValue = 255)
c3= rgb(117,112,179, maxColorValue = 255)
####Fig2-A Markov jumps
Fig2A<-read.xlsx('Figure.xlsx',sheet='2A')
Fig2A$id2<-seq(1,length(Fig2A$height))
Fig2A$serotype<-as.character(Fig2A$serotype)

ggplot(Fig2A,aes(id2,height)) +
  geom_point(aes(color=serotype),size=2) +
  geom_errorbar(aes(ymin=min,ymax=max,color=serotype),width=0.5,size=0.3,linetype=8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = 'sans',size = 30),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(family = 'sans',size = 30),
        legend.position = '') +
  scale_y_continuous(breaks = seq(2013,2020,1),labels =seq(2013,2020,1)) +
  scale_x_continuous(expand = c(0,0.3)) +
  scale_color_manual(values = c(c1,c2,c3,'black')) +
  xlab('') + ylab('') +
  coord_flip()

####Fig2-B
DBDSKY<-read.xlsx('Figure.xlsx',sheet = '2B')
Rdata1<-filter(DBDSKY,Serotype=='DENV1')
Rdata2<-filter(DBDSKY,Serotype=='DENV2')
Rdata3<-filter(DBDSKY,Serotype=='DENV3')

ggplot()+geom_line(data=Rdata1,aes(Remeanname,Rmean),lwd=1.3,color= c1) +
  geom_line(data=Rdata3,aes(Remeanname,Rmean),lwd=1.3,color= c3) +
  geom_line(data=Rdata2,aes(Remeanname,Rmean),lwd=1.3,color= c2) +
  scale_y_continuous(expand = c(0,0.05),limits = c(0,3.1))+
  scale_x_continuous(breaks = seq(2013,2020,1))+
  xlab('')+ylab('Effectiv Reproductive Numbers (Re)')+
  geom_hline(yintercept = 1,linetype='dotted')+
  theme_bw()+
  theme(axis.text.x = element_text(family = 'sans',size = 22),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(family = 'sans',size = 22),
        panel.border = element_rect(size = 1.2),
        axis.line.y.left = element_line(colour = 'black',size = 1.2),
        axis.line.x.bottom = element_line(colour = 'black',size = 1.2),
        axis.title.y = element_text(size=22,family = 'sans'))

####Fig2-C
tmdata<-read.xlsx('Figure.xlsx',sheet = '2C')
tmdata$Month<-as.Date(tmdata$Month,origin = '1900-01-01')

ggplot()+geom_line(data=tmdata,aes(Month,Mean),lwd=1.3,color='black')+
  geom_line(data=tmdata,aes(Month,High),lwd=1.3,color='#D73027')+
  geom_line(data=tmdata,aes(Month,Low),lwd=1.3,color='#4575B4')+
  scale_y_continuous(expand = c(0,0.05),limits = c(0,35))+
  scale_x_date(breaks = '1 year',labels = date_format('%Y'),limits = as.Date(c('2012-12-03','2020-1-03')))+
  xlab('')+ylab('Mean Temperature') +
  theme_bw()+
  theme(axis.text.x = element_text(family = 'sans',size = 24),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(family = 'sans',size = 24),
        panel.border = element_rect(size = 1.5),
        axis.line.y.left = element_line(colour = 'black'),
        axis.line.x.bottom = element_line(colour = 'black'),
        axis.title.y = element_text(size=24,family = 'sans'))

####Fig2-D
Fig2D<-read.xlsx('Figure.xlsx',sheet = '2D')
Fig2D$Month<-as.Date(Fig2D$Month,origin = '1900-01-01')

ggplot(data=Fig2D)+geom_line(aes(Month,Hm),lwd=1.3,color= c3)+
  geom_histogram(aes(Month,Pr/10),color='grey',stat = 'identity') + 
  scale_y_continuous(expand = c(0,0.05),sec.axis = sec_axis(~.*10,name='Precipitation'))+
  scale_x_date(breaks = '1 year',labels = date_format('%Y'),limits = as.Date(c('2012-12-03','2020-1-03')))+
  xlab('')+ylab('Humidity (%)') +
  theme_bw()+
  theme(axis.text.x = element_text(family = 'sans',size = 24),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(family = 'sans',size = 24),
        panel.border = element_rect(size = 1.5),
        axis.line.y.left = element_line(colour = 'black'),
        axis.line.x.bottom = element_line(colour = 'black'),
        axis.title.y = element_text(size=24,family = 'sans'))

##########################################Fig3##########################################
####MCC tree
cols<-c('other'='#7C99C9','Yunnan'='#ED9D85','China-other'='#66C2A5','Laos'='#66C2A5','Myanmar'='#66C2A5','Thailand'='#66C2A5','VietNam'='#66C2A5')

tree<-read.beast('MCCtrees.txt')
groupfile1<-read.table('traits.txt',header=T,row.names=1)
groupInfo1 <- split(row.names(groupfile1), groupfile1$Location)
tree1 <- groupOTU(tree, groupInfo1)

ggtree(tree1,ladderize = TRUE,aes(color=Location),mrsd = '2019-11-01',size=1,alpha=1) + 
  theme_tree2() +
  theme(legend.position = "",
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(family = 'sans',size = 18),
        legend.text = element_text(family = 'sans',size = 12)) +
  geom_tippoint(aes(fill=group),size=1.5,shape=21,color='black')+
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = seq(1950,2020,10),limits = c(1950,2020)) +
  geom_rootedge(rootedge = 0.5) 

####Root-to-tip
rtt<-read.xlsx('Figure.xlsx',sheet='Root-to-tip')
rtt1<-filter(rtt,serotype=='1')
cols<-c('others'='#7C99C9','Yunnan'='#ED9D85','Boarder-areas'='#66C2A5')

rtt1y<-filter(rtt1,Location=='Yunnan')
ggplot()+
  geom_point(data=rtt1,mapping=aes(date,distance,fill=Location),size=3,shape=21) +
  geom_point(data=rtt1y,mapping=aes(date,distance,fill=Location),size=3,shape=21) +
  geom_smooth(data=rtt1,mapping=aes(date,distance),method='lm',formula = 'y ~ x',
              se = FALSE,size=1,color='black',linetype=2) +
  scale_fill_manual(values = cols) + 
  theme(axis.text.x = element_text(vjust=1,hjust = 0.5,family = 'sans',size = 16),
        axis.line = element_line(colour = "black",size = 1.3),
        panel.grid.minor  = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_text(size = 16,family = 'sans'),
        axis.title.y.right = element_text(angle=90),
        axis.ticks = element_line(size = 1.3),
        panel.background = element_blank(),
        legend.position = '',
        axis.title = element_text(family = 'sans',size = 16)) +
  scale_x_continuous(breaks = seq(1960,2020,10)) +
  ylab('Distance') + xlab('')

####Sankey plots
colourscale <- 'd3.scaleOrdinal() .domain(["China-other","Laos","Thailand","Viet Nam","Myanmar","Yunnan","Others"]) .range(["#66C2A5", "#66C2A5" , "#66C2A5", "#66C2A5", "#66C2A5", "#ED9D85", "#7C99C9"])'
datad1<-read.xlsx('figure.xlsx',sheet = 'D1SK')
migin1<-datad1$Source
migout1<-datad1$Target

nodes1<-data.frame(name1=c('Yunnan',migin1[migin1 != 'Yunnan'],migout1[migout1 != 'Yunnan']))
to<-length(migin1[migin1 != 'Yunnan'])                  
from<-length(migout1[migout1 != 'Yunnan'])

migin1[migin1 != 'Yunnan'] = seq(to)
migout1[migout1 != 'Yunnan'] = seq(from = to +1,length.out = from)

migin1[migin1 == 'Yunnan'] = 0
migout1[migout1 == 'Yunnan'] = 0

datad1$Source = as.numeric(migin1)
datad1$Target = as.numeric(migout1)

p1<-sankeyNetwork(
  Links = datad1,
  Nodes = nodes1,
  Source = 'Source',
  Target = 'Target',
  Value = 'Value',
  NodeID = 'name1',
  fontSize = 30,
  fontFamily = 'sans',
  colourScale = colourscale,
  nodeWidth = 100,
  nodePadding = 25
)
p1
saveWidget(p1, file="D1MJsankey.html")
webshot("D1MJsankey.html", "D1MJsankey.pdf")
