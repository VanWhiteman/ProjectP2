##### PART 2

research_test <- function(data,question) {


  hypothesis <- function(data,question) {
    print("HYPOTHESIS:")

    if (question == 1){
      print("H0 : β = 0 against H1 : β ≠ 0. Where β is the slope parameter of the simple linear model Weight ~ Height")
    } else if (question == 2){
      print("H0 : mu_f = mu_m against H1 : mu_f ≠ mu_m. Where mu_f is average female height and mu_m is average male height")
    } else if (question ==3){
      print("H0 : No association between gender and physical activity against H1 : association between gender and physical activity exists.")
    }
  }


  assumptions <- function(data,question) {
    print("CHECK THE ASSUMPTIONS: look at plots!")
    library(ggplot2)
    library(gridExtra)

    if (question ==1) {
      model <- lm(weight~height,data)
      assum_data <-  data.frame(rstandard(model),fitted(model))
      names(assum_data) <- c("residuals","fitted")
      p1 <- ggplot(data,aes(x=height,y=weight))+geom_point()+labs(title="Weight vs Height")
      p2 <- ggplot(assum_data, aes(x=residuals))+geom_histogram(bins=20)+labs(title="regression of residuals")
      p3 <- ggplot(assum_data, aes(x=fitted,y=residuals))+geom_point()+geom_smooth(formula=y~x,method="loess")+labs(title="residuals vs fitted")
      grid.arrange(p1,p2,p3,nrow=1)
    } else if (question ==2) {
      qqnorm(data$weight)
    } else if (question==3){
      cont_tab <- table(data$phys,data$gender)
      print(ggplot(data, aes(y=phys, x=gender,fill=phys)) +geom_bar(position="dodge", stat="identity"))
    }
  }

  fit <- function(data,question) {
    if (question==1){
      model1 <- lm(weight~height,data=data)
      fit1 <- model1
    } else if (question ==2) {
      model2 <- t.test(weight~gender,data=data,var.equal=TRUE)
      fit1 <- model2
    } else if (question==3){
      cont_tab <- table(data$phys,data$gender)
      chisq <- chisq.test(cont_tab)
      fit1 <- chisq
    }
  }

  decision <- function(fit1,question) {
    print("PERFORM LINEAR REGRESSION:")

    if (question ==1){
      p_value <- summary(fit1)$coefficients[2,4]
      print(paste0("The estimated slope of the regression is ",(fit1$coefficients)[2],". It has p-value",p_value))
      print("DECISION:")
      if (p_value<0.05) {
        print("Reject the null hypothesis in favour of the alternative hypothesis at the 5% significance level.")
      } else {
        print("Do no reject the null hypothesis at the 5% significance level.")
      }

    } else if (question ==2){
      p_value <- fit1$p.value
      if (p_value< 0.05) {
        paste0("p-value is equal to: ", p_value,". p-value is less than 0.05, hence reject the null hypothesis in favour of the alternative hypothesis at the 5% significance level. The mean height of males and females are not equal. The mean height of males is ", round(mean((filter(data,gender=="Male"))$height),2),"cm and the mean height of females is ", round(mean((filter(data,gender=="Female"))$height),2), "cm.")
      } else {
        paste0("p-value is equal to: ",p_value, ". Hence do not reject the null hypothesis. In the provided data, the mean height of males and females are equal: ",mean(data$height),"cm.")
      }

    } else if (question ==3) {
      p_value <- fit1$p.value
      if (p_value < 0.05) {
        paste("the p-value is: ",p_value,". p-value is less than 0.05, hence reject the null hypothesis in favour of the alternative hypothesis at 5% signifcance level. There is association between gender and physical activity.",collapse ="\n")
      } else {
        paste("the p-value is: ",p_value,". p-value is not less than 0.05, hence do not reject the null hypothesis at the 5% signficance level. There is no association between gender and physical activity.",collpase="\n")
      }
    }
  }


  conclusion<- function(fit1,question) {
    print("CONCLUSION:")
    if (question ==1){
      p_value <- summary(fit1)$coefficients[2,4]

      if (p_value<0.05) {
        print(paste0("There is a linear relationship between weight and height which is",(fit1$coefficients)[2]))
      } else {
        print("There is no linear relationship between weight and height")
      }

    } else if (question ==2){
      p_value <- fit1$p.value
      if (p_value< 0.05) {
        paste0("The mean height of males and females are not equal. The mean height of males is ", round(mean((filter(data,gender=="Male"))$height),2),"cm and the mean height of females is ", round(mean((filter(data,gender=="Female"))$height),2), "cm.")
      } else {
        paste0("The mean height of males and females are equal: ",mean(data$height),"cm.")
      }

    } else if (question ==3) {
      p_value <- fit1$p.value
      if (p_value < 0.05) {
        paste("There is association between gender and physical activity.",collapse ="\n")
      } else {
        paste("There is no association between gender and physical activity.",collpase="\n")
      }
    }
  }


  # instead of running each function separately as in Distinction part, can run them all at once with below code inside mytest function
  hypothesis(data,question)
  assumptions(data,question)
  fit1 <- fit(data,question)
  decision(fit1,question)
  conclusion(fit1,question)
}

}





#### OWN FUNCTIONS


hypothesis <- function(data,question) {
  print("HYPOTHESIS:")

  if (question == 1){
    print("H0 : β = 0 against H1 : β ≠ 0. Where β is the slope parameter of the simple linear model Weight ~ Height")
  } else if (question == 2){
    print("H0 : mu_f = mu_m against H1 : mu_f ≠ mu_m. Where mu_f is average female height and mu_m is average male height")
  } else if (question ==3){
    print("H0 : No association between gender and physical activity against H1 : association between gender and physical activity exists.")
  }
}


assumptions <- function(data,question) {
  print("CHECK THE ASSUMPTIONS: look at plots!")
  library(ggplot2)
  library(gridExtra)

  if (question ==1) {
    model <- lm(weight~height,data)
    assum_data <-  data.frame(rstandard(model),fitted(model))
    names(assum_data) <- c("residuals","fitted")
    p1 <- ggplot(data,aes(x=height,y=weight))+geom_point()+labs(title="Weight vs Height")
    p2 <- ggplot(assum_data, aes(x=residuals))+geom_histogram(bins=20)+labs(title="regression of residuals")
    p3 <- ggplot(assum_data, aes(x=fitted,y=residuals))+geom_point()+geom_smooth(formula=y~x,method="loess")+labs(title="residuals vs fitted")
    grid.arrange(p1,p2,p3,nrow=1)
  } else if (question ==2) {
    qqnorm(data$weight)
  } else if (question==3){
    cont_tab <- table(data$phys,data$gender)
    print(ggplot(data, aes(y=phys, x=gender,fill=phys)) +geom_bar(position="dodge", stat="identity"))
  }
}

fit <- function(data,question) {
  if (question==1){
    model1 <- lm(weight~height,data=data)
    fit1 <- model1
  } else if (question ==2) {
    model2 <- t.test(weight~gender,data=data,var.equal=TRUE)
    fit1 <- model2
  } else if (question==3){
    cont_tab <- table(data$phys,data$gender)
    chisq <- chisq.test(cont_tab)
    fit1 <- chisq
  }
}

decision <- function(fit1,question) {
  print("PERFORM LINEAR REGRESSION:")

  if (question ==1){
    p_value <- summary(fit1)$coefficients[2,4]
    print(paste0("The estimated slope of the regression is ",(fit1$coefficients)[2],". It has p-value",p_value))
    print("DECISION:")
    if (p_value<0.05) {
      print("Reject the null hypothesis in favour of the alternative hypothesis at the 5% significance level.")
    } else {
      print("Do no reject the null hypothesis at the 5% significance level.")
    }

  } else if (question ==2){
    p_value <- fit1$p.value
    if (p_value< 0.05) {
      paste0("p-value is equal to: ", p_value,". p-value is less than 0.05, hence reject the null hypothesis in favour of the alternative hypothesis at the 5% significance level. The mean height of males and females are not equal. The mean height of males is ", round(mean((filter(data,gender=="Male"))$height),2),"cm and the mean height of females is ", round(mean((filter(data,gender=="Female"))$height),2), "cm.")
    } else {
      paste0("p-value is equal to: ",p_value, ". Hence do not reject the null hypothesis. In the provided data, the mean height of males and females are equal: ",mean(data$height),"cm.")
    }

  } else if (question ==3) {
    p_value <- fit1$p.value
    if (p_value < 0.05) {
      paste("the p-value is: ",p_value,". p-value is less than 0.05, hence reject the null hypothesis in favour of the alternative hypothesis at 5% signifcance level. There is association between gender and physical activity.",collapse ="\n")
    } else {
      paste("the p-value is: ",p_value,". p-value is not less than 0.05, hence do not reject the null hypothesis at the 5% signficance level. There is no association between gender and physical activity.",collpase="\n")
    }
  }
}


conclusion<- function(fit1,question) {
  print("CONCLUSION:")
  if (question ==1){
    p_value <- summary(fit1)$coefficients[2,4]

    if (p_value<0.05) {
      print(paste0("There is a linear relationship between weight and height which is",(fit1$coefficients)[2]))
    } else {
      print("There is no linear relationship between weight and height")
    }

  } else if (question ==2){
    p_value <- fit1$p.value
    if (p_value< 0.05) {
      paste0("The mean height of males and females are not equal. The mean height of males is ", round(mean((filter(data,gender=="Male"))$height),2),"cm and the mean height of females is ", round(mean((filter(data,gender=="Female"))$height),2), "cm.")
    } else {
      paste0("The mean height of males and females are equal: ",mean(data$height),"cm.")
    }

  } else if (question ==3) {
    p_value <- fit1$p.value
    if (p_value < 0.05) {
      paste("There is association between gender and physical activity.",collapse ="\n")
    } else {
      paste("There is no association between gender and physical activity.",collpase="\n")
    }
  }
}


