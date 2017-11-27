library(readxl)
library(plyr)
library(qpcR)
library(reshape2)
library(ggplot2)
library(e1071)

######## Drug A ###############
  R1 = c(0.21,
                  0.253333333,
                  0.66,
                  0.833333333,
                  1.323333333,
                  1.77,
                  1.89,
                  1.913333333)
  R1 <- as.data.frame(R1)
  
  R2 = c(0.243333333,
          0.29,
          0.536666667,
          0.66,
          0.97,
          1.253333333,
          NA,
          NA)
  R2 <- as.data.frame(R2)
  
  R3 = c(0,
          0.03,
          0.236666667,
          0.506666667,
          0.763333333,
          1.553333333,
          2.03,
          NA)
  R3 <- as.data.frame(R3)
  
  R4 = c(0.063333333,
          0.12,
          0.226666667,
          0.326666667,
          0.316666667,
          0.61,
          1.063333333,
          1.563333333)
  R4 <- as.data.frame(R4)
  
  R5 = c(-0.003333333,
         0.003333333,
         0,
         0.056666667,
         0.093333333,
         0.236666667,
         0.373333333,
         0.676666667)
  R5 <- as.data.frame(R5)
  
  R6 = c(-0.016666667,
         0.02,
         0.04,
         0.04,
         0.033333333,
         -0.016666667,
         0.05,
         0)
  R6 <- as.data.frame(R6)
  
  Molar =c(1*10^-9,
                        2*10^-9,
                        5*10^-9,
                        1*10^-8,
                        2*10^-8,
                        5*10^-8,
                        1*10^-7,
                        2*10^-7)

  Molar <- as.data.frame(Molar)

dfA <- cbind(R1,R2,R3,R4,R5,R6,Molar)

meltedA <- melt(dfA, id= 'Molar')

ggplot(meltedA, aes(x=Molar, y=value, colour = variable)) +
  geom_point() +
  geom_smooth(method = "loess", se= FALSE) +
  scale_x_log10() +
  geom_hline(yintercept = 0.574) +
  geom_text(aes(1*10^-9, 0.574 ,label = "0.574", vjust= -1))

svmModel <- svm(dfA$R1, dfA$Molar)
numberA = 0.574
predictionA <- predict(svmModel, numberA) ## 6.2497x 10^-9



####### Drug C ###############

CR1 = c(0.06,
        0.15,
        0.396666667,
        0.963333333,
        1.576666667,
        2.623333333,
        2.943333333,
        NA)

CR2 = c(0.133333333,
        0.163333333,
        0.363333333,
        0.723333333,
        1.253333333,
        2.563333333,
        2.96,
        NA)

CR3 = c(0.27,
        0.456666667,
        0.523333333,
        0.646666667,
        1.25,
        2.363333333,
        NA,NA)

CR4 = c(0.166666667,
        0.376666667,
        0.51,
        0.766666667,
        1.103333333,
        2.106666667,
        3.2,
        NA)

CR5 = c(0.133333333,
        0.356666667,
        0.663333333,
        1.016666667,
        1.67,
        2.566666667,
        3.45,
        NA)

CR6 = c(0.15,
        0.25,
        0.493333333,
        0.68,
        1.16,
        1.69,
        2.68,
        NA)

dfC <- as.data.frame(cbind(CR1,CR2,CR3,CR4,CR5,CR6,Molar))

meltedC <- melt(dfC, id= 'Molar')

ggplot(meltedC, aes(x=Molar, y=value, colour = variable)) +
  geom_point() +
  geom_smooth(method = "loess", se= FALSE)+
  scale_x_log10() +
  geom_hline(yintercept = numberC) +
  geom_text(aes(1*10^-9, numberC ,label = numberC, vjust= -1))

svmModelC <- svm(dfC$CR1, dfC$Molar)
numberC = 0.883
predictionC <- predict(svmModelC, numberC) ## 1.0413x 10^-8


###### Drug E ##########

CE1 = c(0.073333333,
        0.096666667,
        0.156666667,
        0.37,
        0.52,
        1.09,
        1.66,
        1.753333333,
        1.963333333,NA,NA,NA)

CE2 = c(0.106666667,
        0.166666667,
        0.206666667,
        0.363333333,
        0.57,
        1.386666667,
        1.863333333,NA,NA,NA,NA,NA)

CE3 = c(0.036666667,
        0.24,
        0.323333333,
        0.433333333,
        0.576666667,
        0.9,
        1.366666667,NA,NA,NA,NA,NA)

CE4 = c(0.03,
        0.05,
        0.153333333,
        0.193333333,
        0.293333333,
        0.49,
        0.956666667,NA,NA,NA,NA,NA)

CE5 = c(0.003333333,
        0.05,
        0.103333333,
        0.076666667,
        0.066666667,
        0.15,
        0.193333333,
        0.196666667,
        0.416666667,
        0.616666667,
        0.91,NA)

CE6 = c(-0.02,
        0.006666667,
        0.023333333,
        0.003333333,
        0.056666667,
        0.126666667,
        0.03,
        0.05,
        0.05,
        0.083333333,
        0.166666667,
        0.546666667)

MolarE =c(1*10^-9,
         2*10^-9,
         5*10^-9,
         1*10^-8,
         2*10^-8,
         5*10^-8,
         1*10^-7,
         2*10^-7,
         5*10^-7,
         1*10^-6,
         2*10^-6,
         5*10^-6)

dfE <- as.data.frame(cbind(CE1,CE2,CE3,CE4,CE5,CE6,MolarE))

meltedE <- melt(dfE, id= 'MolarE')

ggplot(meltedE, aes(x=MolarE, y=value, colour = variable)) +
  geom_point() +
  geom_smooth(method = "loess", se= FALSE)+
  scale_x_log10() +
  geom_hline(yintercept = numberE) +
  geom_text(aes(1*10^-9, numberE ,label = numberE, vjust= -1))

svmModelE <- svm(dfE$CE1, dfE$MolarE)
numberE = 0.589
predictionC <- predict(svmModelE, numberE) ## 3.0866x 10^-8


n <- 100
x <- seq(n)
y <- rnorm(n, 50 + 30 * x^(-0.2), 1)
Data <- data.frame(x, y)

plot(y ~ x, Data)
loess_fit <- loess(y ~ x, Data)
lines(Data$x, predict(loess_fit), col = "blue")



######## Potency ##############

P1 = c(0.883333333,
       0.846666667,
       0.803333333,
       0.733333333,
       0.613333333,
       0.01,
       -0.05,
       -0.11)

P2 = c(0.845,
       0.785,
       0.795,
       0.76,
       0.64,
       0.6,
       0.515,
       0.03)

P3 = c(1.083333333,
       0.963333333,
       0.99,
       0.936666667,
       0.85,
       0.45,
       0.27,
       -0.19)

Mols = c(0,
         1*10^-12,
         1*10^-11,
         1*10^-10,
         1*10^-9,
         1*10^-8,
         1*10^-7,
         1*10^-6)

dfPot <- as.data.frame(cbind(P1,P2,P3,Mols))


meltedPot <- melt(dfPot, id= 'Mols')

ggplot(meltedPot, aes(x=Mols, y=value, colour = variable)) +
  geom_point() +
  geom_smooth(method = "loess", se= FALSE) +
  scale_x_log10()

##########################

cbind.na<-function(df1, df2){
  
  #Collect all unique rownames
  total.rownames<-union(x = rownames(x = df1),y = rownames(x=df2))
  
  #Create a new dataframe with rownames
  df<-data.frame(row.names = total.rownames)
  
  #Get absent rownames for both of the dataframe
  absent.names.1<-setdiff(x = rownames(df1),y = rownames(df))
  absent.names.2<-setdiff(x = rownames(df2),y = rownames(df))
  
  #Fill absents with NAs
  df1.fixed<-data.frame(row.names = absent.names.1,matrix(data = NA,nrow = length(absent.names.1),ncol=ncol(df1)))
  colnames(df1.fixed)<-colnames(df1)
  df1<-rbind(df1,df1.fixed)
  
  df2.fixed<-data.frame(row.names = absent.names.2,matrix(data = NA,nrow = length(absent.names.2),ncol=ncol(df2)))
  colnames(df2.fixed)<-colnames(df2)
  df2<-rbind(df2,df2.fixed)
  
  #Finally cbind into new dataframe
  df<-cbind(df,df1[rownames(df),],df2[rownames(df),])
  return(df)
  
}
