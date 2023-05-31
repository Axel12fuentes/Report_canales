library(shiny)
library(highcharter)
library(tidyverse)
library(shinythemes)
library(plotly)
library(DT)
library(lubridate)
library(plyr)
library(kableExtra)
library(formattable)
library(knitr)
library(viridisLite)
library(viridis)
library(openxlsx)
library(zoo)
library(tidyr)
library(car)


######################
# BDD
######################
bdd <- read.xlsx("D:\\Caja los Andes\\Canales\\Canales\\report Canales\\bdd_last.xlsx")
######################
# Limpieza de bdd
######################
bdd1 <- bdd

bdd1$date <- as.Date(bdd1$dFechaOpe, origin="1899-12-30")

bdd1$mes <- format(bdd1$date,'%m')
bdd1$ano <- format(bdd1$date,'%Y')
bdd1$dia <- format(bdd1$date,'%d')

canal <- bdd1 %>%
  filter(cTipoOperacion=="PAGO DE PRÉSTAMO")%>%
  select(cEOB_Kardex, total, cCanal, region, departamento, provincia,distrito,
         mes, dia,idCuenta,date,ofic_ases)

canal$cCanal[canal$cCanal=='BILLETERA ELECTRONICA MOVIL - BIM'] <- 'BIM'
canal$cCanal[canal$cCanal=="BANCO DE CREDITO DEL PERU"] <- "BCP"

bdd2 <- read.csv("D:\\Caja los Andes\\Canales\\Canales\\report Canales\\Operaciones por canalesS NOVIEMBRE DICIEMBRE 2022.csv")


bdd2$date <- as.Date(bdd2$dFechaOpe, format="%d/%m/%Y")

bdd2$mes <- format(bdd2$date,'%m')
bdd2$ano <- format(bdd2$date,'%Y')
bdd2$dia <- format(bdd2$date,'%d')

canal2 <- bdd2 %>%
  filter(cTipoOperacion=="PAGO DE PRÉSTAMO")%>%
  select(cEOB_Kardex, total, cCanal, region, departamento, provincia,distrito,
         mes, dia,idCuenta,date,ofic_ases)

canal2$cCanal[canal2$cCanal=='BILLETERA ELECTRONICA MOVIL - BIM'] <- 'BIM'
canal2$cCanal[canal2$cCanal=="BANCO DE CREDITO DEL PERU"] <- "BCP"

canal <-rbind(canal,canal2) 

canal$rango <-  ifelse(canal$total <= 500, 1,
                       ifelse(canal$total <= 1000, 2, 3))

canal$mesu <- format(as.Date(canal$date), "%b-%y")



####################################################################
# Analisis
###################################################################

### Ratio de Canales ALternativos

meses_ordenados <- factor(c("nov.-22","dic.-22", "ene.-23", "feb.-23", "mar.-23", 
                            "abr.-23","may.-23"),
                          levels = c("nov.-22","dic.-22", "ene.-23", "feb.-23",
                                     "mar.-23","abr.-23","may.-23"))


table(factor(canal$mesu,levels = meses_ordenados),canal$cCanal)


table(canal$cCanal,factor(canal$mesu,levels = meses_ordenados))

tab <- prop.table(table(factor(canal$mesu,levels = meses_ordenados),
                        canal$cCanal),1)

kpi <- 1-tab[,ncol(tab)]


canalr_1 <- canal %>%
  filter(rango=="1")

tabr1 <- prop.table(table(canalr_1$cCanal,factor(canalr_1$mesu,
                                                 levels = meses_ordenados)),2)

kpi_r1 <- 1-tabr1[nrow(tabr1),]


canalr_2 <- canal %>%
  filter(rango=="2")

tabr2 <- prop.table(table(canalr_2$cCanal,factor(canalr_2$mesu,
                                                 levels = meses_ordenados)),2)

kpi_r2 <- 1-tabr2[nrow(tabr2),]


canalr_12 <- canal %>%
  filter(rango%in%c("1","2"))

tabr1_2 <- prop.table(table(canalr_12$cCanal,factor(canalr_12$mesu,
                                                    levels = meses_ordenados)),2)

kpi_r12 <- 1-tabr1_2[nrow(tabr1_2),]


canalr_3 <- canal %>%
  filter(rango=="3")

tabr3 <- prop.table(table(canalr_3$cCanal,factor(canalr_3$mesu,
                                                 levels = meses_ordenados)),2)


kpi_r3 <- 1-tabr3[nrow(tabr3),]


kpiu <- cbind(kpi,kpi_r1,kpi_r2,kpi_r12,kpi_r3)

kpiu1 <-as.data.frame(kpiu)
kpiu1 <- round(kpiu1*100, 2)

kpina <- row.names(kpiu1)
rownames(kpiu1) <-NULL
kpiu1 <- cbind(kpina,kpiu1)
names(kpiu1)[1] <- "Meses"
names(kpiu1)[2] <- "Canal_Alt"
names(kpiu1)[3] <- "Canal_Alt_Rango1"
names(kpiu1)[4] <- "Canal_Alt_Rango2"
names(kpiu1)[5] <- "Canal_Alt_Rango1_2"
names(kpiu1)[6] <- "Canal_Alt_Rango3"

##### TABLAS NOMINAL

Can_mes <- table(canal$cCanal,factor(canal$mesu,levels = meses_ordenados))
Can_mes <-as.data.frame.array(Can_mes)

su_Can_mes <- colSums(Can_mes)
r_Can_mes <- row.names(Can_mes)
tot <- "Total"
r_Can_mes1 <- c(r_Can_mes,"Total")

sut_Can_mes <- rbind(Can_mes,su_Can_mes)
sum_Can_mes <- cbind(r_Can_mes1,sut_Can_mes)
rownames(sum_Can_mes) <- NULL 
names(sum_Can_mes)[1] <- "Canal"

per_mes <- as.data.frame.array(prop.table(table(canal$cCanal,
                                                factor(canal$mesu,
                                                       levels = meses_ordenados)),2)*100)

per_mes <-round(per_mes,2)

su_per_mes <- colSums(per_mes)
r_per_mes <- row.names(per_mes)
r_per_mes1 <- c(r_per_mes,"Total")

sut_per_mes <- rbind(per_mes,su_per_mes)
sum_per_mes <- cbind(r_per_mes1,sut_per_mes)
rownames(sum_per_mes) <- NULL 
names(sum_per_mes)[1] <- "Canal"


#################################################################
# ventanilla
#################################################################

evol_mes_ven<-canal %>%
  filter(cCanal=="VENTANILLA")


evol_mes_ven1 <-as.data.frame(table(evol_mes_ven$date,evol_mes_ven$cCanal))

evol_mes_ven1 <-select(evol_mes_ven1, Fecha=Var1,
                        Canal=Var2,Channel=Freq)

evol_mes_ven1$Fecha <-as.Date(evol_mes_ven1$Fecha)
evol_mes_ven1$Channel <- as.numeric(evol_mes_ven1$Channel)

# MIGRACIONES

### Migración de los de ventanilla abr-may

mig_abr_venta <- evol_mes_ven %>%
  filter(mes=="04", )%>%
  select(idCuenta,cEOB_Kardex, total,cCanal,departamento,provincia,
         distrito,rango)

mig_may_venta <- canal %>%
  filter(mes=="05")%>%
  select(idCuenta,cEOB_Kardex, total,cCanal,departamento,provincia,
         distrito,rango)


mig_cru_may_abr <- left_join(mig_abr_venta, mig_may_venta, by="idCuenta")

#quita los valores en blano

mig_cru_may_abr1 <-mig_cru_may_abr[!is.na(mig_cru_may_abr$cCanal.y),]


# uniendo las dos tablas, todas las transacciones de ventanilla de FEB 
# donde se movieron en marzo


migr_vent_total <- as.data.frame(cbind(table(mig_cru_may_abr1$cCanal.y),
                                       prop.table(table(mig_cru_may_abr1$cCanal.y))*100))
sum_migr_vent_total <-colSums(migr_vent_total) 
######################################################################

migr_vent_total1 <- rbind(migr_vent_total,sum_migr_vent_total)

names_migr_vent_total <- rownames(migr_vent_total)
names_migr_vent_total <- data.frame(names_migr_vent_total)
all_venta <- "TOTAL"

names_migr_vent_total1 <- rbind.data.frame(names_migr_vent_total,all_venta)
names(names_migr_vent_total1)[1] <- "Canal"


migr_vent_total1 <-cbind(names_migr_vent_total1,migr_vent_total1)
rownames(migr_vent_total1) <- NULL 


names(migr_vent_total1)[2] <- "#"
names(migr_vent_total1)[3] <- "Percent"

migr_vent_total1$Porcen <- round(migr_vent_total1$Percent,2)

migr_vent_total1$Porcent <- paste0(migr_vent_total1$Porcen, "%")

migr_vent_total1 <- select(migr_vent_total1,Canal,"#",Porcent)


## Oficina

ven_of_g<- as.data.frame(table(canal$ofic_ases))
ven_of_v<- as.data.frame(table(evol_mes_ven$ofic_ases))

ven_of_f <- left_join(ven_of_v, ven_of_g, by="Var1")
names(ven_of_f)[1] <- "Oficina"
names(ven_of_f)[2] <- "Ventanilla"
names(ven_of_f)[3] <- "Total"

ven_of_f$Ratio <-round(ven_of_f$Ventanilla/ven_of_f$Total,2)

ven_of_f <-ven_of_f %>%
  arrange(desc(ven_of_f$Ratio))


#################################################################
# BIM
#################################################################

evol_mes_bin<-canal %>%
  filter(cCanal=="BIM")


evol_mes_bin1 <-as.data.frame(table(evol_mes_bin$date,evol_mes_bin$cCanal))

evol_mes_bin1 <-select(evol_mes_bin1, Fecha=Var1,
                       Canal=Var2,Channel=Freq)

evol_mes_bin1$Fecha <-as.Date(evol_mes_bin1$Fecha)
evol_mes_bin1$Channel <- as.numeric(evol_mes_bin1$Channel)

# Migraciones


mig_abr_BIM<- evol_mes_bin %>%
  filter(mes=="04")%>%
  select(idCuenta,cEOB_Kardex, total,cCanal,departamento,provincia,
         distrito,rango)


mig_cru_may_BIM <- left_join(mig_abr_BIM, mig_may_venta, by="idCuenta")

mig_cru_may_BIM1 <-mig_cru_may_BIM[!is.na(mig_cru_may_BIM$cCanal.y),]



migr_BIM_total <- as.data.frame(cbind(table(mig_cru_may_BIM1$cCanal.y),
                                      prop.table(table(mig_cru_may_BIM1$cCanal.y))*100))

sum_migr_BIM_total <-colSums(migr_BIM_total) 
migr_BIM_total1 <- rbind(migr_BIM_total,sum_migr_BIM_total)

migr_BIM_total1$Porcent <- paste0(round(migr_BIM_total1$V2,2), "%")

names_migr_BIM_total <- rownames(migr_BIM_total)
names_migr_BIM_total <- data.frame(names_migr_BIM_total)
names_migr_BIM_total1 <- rbind(names_migr_BIM_total,all_venta)
names(names_migr_BIM_total1)[1] <- "Canal"

migr_BIM_total1 <-cbind(names_migr_BIM_total1,migr_BIM_total1)

rownames(migr_BIM_total1) <- NULL 
migr_BIM_total1 <- select(migr_BIM_total1, Canal,"#"=V1,Porcent)
migr_BIM_total1


#oficinas

#ven_of_g
ven_of_bi<- as.data.frame(table(evol_mes_bin$ofic_ases))

ven_of_bi_f <- left_join(ven_of_bi, ven_of_g, by="Var1")
names(ven_of_bi_f)[1] <- "Oficina"
names(ven_of_bi_f)[2] <- "BIM"
names(ven_of_bi_f)[3] <- "Total"

ven_of_bi_f$Ratio <-round(ven_of_bi_f$BIM/ven_of_bi_f$Total,2)

ven_of_bi_f <-ven_of_bi_f %>%
  arrange(desc(ven_of_bi_f$Ratio))
###############################
# BCP
###############################


evol_mes_BCP<-canal %>%
  filter(cCanal=="BCP")

evol_mes_BCP1 <-as.data.frame(table(evol_mes_BCP$date,evol_mes_BCP$cCanal))

evol_mes_BCP1 <-select(evol_mes_BCP1, Fecha=Var1,
                       Canal=Var2,Channel=Freq)

evol_mes_BCP1$Fecha <-as.Date(evol_mes_BCP1$Fecha)
evol_mes_BCP1$Channel <- as.numeric(evol_mes_BCP1$Channel)



# Migraciones

mig_ma_BCP<- evol_mes_BCP %>%
  filter(mes=="04")%>%
  select(idCuenta,cEOB_Kardex, total,cCanal,departamento,provincia,
         distrito,rango)


mig_cru_may_BCP <- left_join(mig_ma_BCP, mig_may_venta, by="idCuenta")

mig_cru_may_BCP1 <-mig_cru_may_BCP[!is.na(mig_cru_may_BCP$cCanal.y),]

migr_bcp_total <- as.data.frame(cbind(table(mig_cru_may_BCP1$cCanal.y),
                                      prop.table(table(mig_cru_may_BCP1$cCanal.y))*100))

sum_migr_bcp_total <-colSums(migr_bcp_total) 
migr_bcp_total1 <- rbind(migr_bcp_total,sum_migr_bcp_total)
migr_bcp_total1$Porcent <- paste0(round(migr_bcp_total1$V2,2), "%")

names_migr_bcp_total <- rownames(migr_bcp_total)
names_migr_bcp_total <- data.frame(names_migr_bcp_total)
names_migr_BCP_total1 <- rbind(names_migr_bcp_total,all_venta)

names(names_migr_BCP_total1)[1] <- "Canal"

migr_BCP_total1 <-cbind(names_migr_BCP_total1,migr_bcp_total1)
rownames(migr_BCP_total1) <- NULL 
migr_BCP_total1 <- select(migr_BCP_total1, Canal,"#"=V1,Porcent)
migr_BCP_total1


# oficinas

#ven_of_g
ven_of_bcp<- as.data.frame(table(evol_mes_BCP$ofic_ases))

ven_of_bcp_f <- left_join(ven_of_bcp, ven_of_g, by="Var1")
names(ven_of_bcp_f)[1] <- "Oficina"
names(ven_of_bcp_f)[2] <- "BCP"
names(ven_of_bcp_f)[3] <- "Total"

ven_of_bcp_f$Ratio <-round(ven_of_bcp_f$BCP/ven_of_bcp_f$Total,2)

ven_of_bcp_f <-ven_of_bcp_f %>%
  arrange(desc(ven_of_bcp_f$Ratio))


###############################
# KASNET
###############################
evol_mes_KAS<-canal %>%
  filter(cCanal=="KASNET")

evol_mes_KAS1 <-as.data.frame(table(evol_mes_KAS$date,evol_mes_KAS$cCanal))

evol_mes_KAS1 <-select(evol_mes_KAS1, Fecha=Var1,
                       Canal=Var2,Channel=Freq)

evol_mes_KAS1$Fecha <-as.Date(evol_mes_KAS1$Fecha)
evol_mes_KAS1$Channel <- as.numeric(evol_mes_KAS1$Channel)

# Migraciones

mig_ma_KAS<- evol_mes_KAS %>%
  filter(mes=="04")%>%
  select(idCuenta,cEOB_Kardex, total,cCanal,departamento,provincia,
         distrito,rango)


mig_cru_may_KAS <- left_join(mig_ma_KAS, mig_may_venta, by="idCuenta")

mig_cru_may_KAS1 <-mig_cru_may_KAS[!is.na(mig_cru_may_KAS$cCanal.y),]


migr_KAS_total <- as.data.frame(cbind(table(mig_cru_may_KAS1$cCanal.y),
                                      prop.table(table(mig_cru_may_KAS1$cCanal.y))*100))
sum_migr_KAS_total <-colSums(migr_KAS_total) 
migr_KAS_total1 <- rbind(migr_KAS_total,sum_migr_KAS_total)

migr_KAS_total1$Porcent <- paste0(round(migr_KAS_total1$V2,2), "%")

names_migr_KAS_total <- rownames(migr_KAS_total)
names_migr_KAS_total <- data.frame(names_migr_KAS_total)
names_migr_KAS_total1 <- rbind(names_migr_KAS_total,all_venta)
names(names_migr_KAS_total1)[1] <- "Canal"

migr_KAS_total1 <-cbind(names_migr_KAS_total1,migr_KAS_total1)

rownames(migr_KAS_total1) <- NULL 
migr_KAS_total1 <- select(migr_KAS_total1, Canal,"#"=V1,Porcent)
migr_KAS_total1

# OFICINAS

#ven_of_g
ven_of_KAS<- as.data.frame(table(evol_mes_KAS$ofic_ases))

ven_of_KAS_f <- left_join(ven_of_KAS, ven_of_g, by="Var1")
names(ven_of_KAS_f)[1] <- "Oficina"
names(ven_of_KAS_f)[2] <- "KASNET"
names(ven_of_KAS_f)[3] <- "Total"

ven_of_KAS_f$Ratio <-round(ven_of_KAS_f$KASNET/ven_of_KAS_f$Total,2)

ven_of_KAS_f <-ven_of_KAS_f %>%
  arrange(desc(ven_of_KAS_f$Ratio))







###############################
# APP
###############################

ui <- fluidPage(# Inicia la interfaz de usuario
  
  # Modifiquemos el tema :
  theme = shinytheme("readable"),
  
  # Titulo
  titlePanel("Dashboard Reporte de Canales"),
  
  # Agregar el componente para los input/output
  sidebarLayout(
    # Inicia el sidebarlayout
    
    # Construccion de los componentes de entrada : input 
    sidebarPanel(# Inicia el sidebarPanel 
      
      # Titulo al paquete de inputs
   
      selectInput(inputId = "y",
                  label = "Rango Canales Alternativos",
                  choices = c("Default"="Canal_Alt",
                              "Rango 1"="Canal_Alt_Rango1",
                              "Rango 2"="Canal_Alt_Rango2",
                              "Rango 1&2"="Canal_Alt_Rango1_2",
                              "Rango 3"="Canal_Alt_Rango3"),
                  selected = "Canal_Alt"),
      #Descripcion
      h5("Default - Son todos los Rangos"),
      h5("Rango 1 - Pagos entre S/0 a S/500"),
      h5("Rango 2 - Pagos entre S/500 a S/1,000"),
      h5("Rango 1&2 - Pagos entre S/0 a S/1,000"),
      h5("Rango 3 - Pagos entre S/1,000 a más"),
      br(), br(),
      
      #marketing
      
      br(), br(),
      h5("afuentesp - Dpto. Canales")
      
      
              ), # finaliza el sidebarpanel

mainPanel(# INICIO DEL PANEL PRINCIPAL
  tabsetPanel(#inicio del panel
    
    type = "tabs",
    #pestaña 1: GENERAL
    tabPanel(title="General",
      h3("Gráficos de Canales Alternos"),
     highchartOutput(outputId = "rangoid"),
      
     br(),
      h3("Evolutivo de Canales por meses"),
     tableOutput("tabla1"),
     
     
     br(), 
      h3("Evolutivo de la participación de Canales %"),
     tableOutput("tabla2"),
      br()
    ),
    #pestaña 2
    tabPanel(title="Ventanilla",
                 h3(" Evolutivo"),
             highchartOutput(outputId = "venid"),
             
             br(),
                 h3("Migraciones"),
             tableOutput("tabla3"),
                 h3("Oficinas"),
             tableOutput("tabla4")),
    #Pestaña3
    tabPanel(title="BIM",
                 h3(" Evolutivo"),
             highchartOutput(outputId = "binid"),
                 h3("Migraciones"),
            tableOutput("tabla5"),
                 h3("Oficinas"),
            tableOutput("tabla6"),
            ),
    #Pestaña4
    tabPanel(title="BCP",
                 h3(" Evolutivo"),
             highchartOutput(outputId = "bcpid"),
                 h3("Migraciones"),
             tableOutput("tabla7"),
                 h3("Oficinas"),
             tableOutput("tabla8"),
             ),
    #Pestaña5
    tabPanel(title="KASNET",
                 h3(" Evolutivo"),
             highchartOutput(outputId = "kasid"),
                 h3("Migraciones"),
             tableOutput("tabla9"),
                 h3("Oficinas"),
             tableOutput("tabla10"),
             )
    
  )#final del panel
  
  
)
)#finaliza el sidebarlayout

)


server <- function(input,output){
  
output$rangoid <- renderHighchart({
  

  
 p <-  highchart() %>%
   hc_chart(type = 'line') %>%
   hc_xAxis(categories = kpiu1$Meses) %>%
   hc_add_series(kpiu1$Canal_Alt, name = 'Canal_Alt',
                 dataLabels = list(enabled = TRUE),color="#ef476f") %>%
   hc_add_series(kpiu1$Canal_Alt_Rango1, name = 'Canal_Alt_Rango1',
                 dataLabels = list(enabled = TRUE),color="#ffd166")%>%
   hc_add_series(kpiu1$Canal_Alt_Rango2, name = 'Canal_Alt_Rango2',
                 dataLabels = list(enabled = TRUE),color="#06d6a0")%>%
   hc_add_series(kpiu1$Canal_Alt_Rango1_2, name = 'Canal_Alt_Rango1&2',
                 dataLabels = list(enabled = TRUE),color="#118ab2")%>%
   hc_add_series(kpiu1$Canal_Alt_Rango3, name = 'Canal_Alt_Rango3',
                 dataLabels = list(enabled = TRUE),color="#073b4c")%>%
   hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b> ({point.percentage:.1f}%)<br/>")
 
  p

  
  
})

output$tabla1 <- renderTable({
  
  sum_Can_mes
  
  
})
output$tabla2 <- renderTable({
  
  sum_per_mes
  
  
})



output$venid <- renderHighchart({

  hchart(evol_mes_ven1, "line", hcaes(x = Fecha, y = Channel, 
                                      group = Canal),regression = TRUE)%>%
    hc_add_dependency("plugins/highcharts-regression.js")%>%
  hc_colors("#00C8FF")
})
output$tabla3 <- renderTable({
  
  migr_vent_total1
  
  
  
})

output$tabla4 <- renderTable({
  
  ven_of_f
  
})
  
output$binid <- renderHighchart({
  
  hchart(evol_mes_bin1, "line", hcaes(x = Fecha, y = Channel, 
                                      group = Canal),regression = TRUE)%>%
    hc_add_dependency("plugins/highcharts-regression.js")%>%
    hc_colors("#37BEAD") 
})
output$tabla5 <- renderTable({
  
  migr_BIM_total1
  
  })

output$tabla6 <- renderTable({
  
  ven_of_bi_f
  
})


#pestaña bcp

output$bcpid <- renderHighchart({
  
  hchart(evol_mes_BCP1, "line", hcaes(x = Fecha, y = Channel, 
                                      group = Canal),regression = TRUE)%>%
    hc_add_dependency("plugins/highcharts-regression.js")%>%
    hc_colors("#FF7816")  
})
output$tabla7 <- renderTable({
  
  migr_BCP_total1
  
})

output$tabla8 <- renderTable({
  
  ven_of_bcp_f
  
})

#pestaña4

output$kasid <- renderHighchart({
  
  hchart(evol_mes_KAS1, "line", hcaes(x = Fecha, y = Channel, 
                                      group = Canal),regression = TRUE) %>% 
    hc_add_theme(hc_theme_google())%>%
    hc_add_dependency("plugins/highcharts-regression.js")%>%
    hc_colors('RGB(2,64,139)')  
  
})
output$tabla9 <- renderTable({
  
  migr_KAS_total1
  
})

output$tabla10 <- renderTable({
  
  ven_of_KAS_f
  
})


}

shinyApp(ui=ui, server=server)
