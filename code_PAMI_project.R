##Required packages and the directory I used to store the data
setwd("/Users/danielsalnikov/Desktop/actuarial_science/PAMI_proyecto")
require(dplyr)
require(purrr)
a = c(1:27)
names(a) =c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", 
            "v", "w", "x", "y", "z", "aa")

datos <- read.csv("datos_primas.csv", header = TRUE)
str(datos)
datos$Grupo <- factor(datos$Grupo, labels = c("cred", "debt"))
str(datos)

datos <- datos %>% mutate(pago_bancos = 0.45*Salvable_Bancos)
datos <- datos %>% mutate(perd_final = Severidad - pago_bancos)
datos <- datos %>% mutate(perd_final_esperad = perd_final * Frecuencia)

datos_cred <- datos %>% filter(Grupo == "cred")
datos_debt <- datos %>% filter(Grupo == "debt")

summary(datos_cred)
summary(datos_debt)
sd(datos_cred$esp_monto_contracargo)
sd(datos_debt$esp_monto_contracargo)


prima_pura(6,  b0 = 0.1, q0 = 0.001, 30000, "cred" )
prima_tarifa(995.2344, c(0.08, .03,  .06, .09, .01, .07) )/12

pro_ventas <- proyec_crec(10, c(37986255, 73968587, 173736722,285219198, 422557179, 393983804))


cart_sim <- sim_cart(10, proyec_tipo(sum(datos_cred$Expuestos)/sum(datos$Expuestos),  c(45095.0368, 45095.0368), 0.08, 10), 
                     c( 0.1, 0.001), c(0.08, .03,  .06, .09, .01, .08) )

cart_clientes <- proyec_tipo(sum(datos_cred$Expuestos)/sum(datos$Expuestos), c(45095.0368, 45095.0368), 0.08, 10)
colnames(cart_clientes) <- c("#Polizas de Credito", "#Polizas de Debito")
rownames(cart_clientes) <- as.character(c(2021:2030) )


res_primas <- array(dim=c(10,9))
colnames(res_primas) <- c("Prima Puras Cred", "Prima Tarifa Cred", "Asgeurados Cred", 
                          "Prima Puras Debt", "Prima Tarifa Debt", "Asgeurados Debt",
                          "Prima Puras Total", "Prima Tarifa Total", "Asgeurados Total")
rownames(res_primas) <- as.character(seq(2021, 2030, 1))

for(i in 1:10){
  res_primas[i,1] = sum( cart_sim[[1]][[i]][,3])
  res_primas[i,2] = sum( cart_sim[[1]][[i]][,4])
  res_primas[i,3] = cart_clientes[i,1]
  res_primas[i,4] = sum( cart_sim[[2]][[i]][,3])
  res_primas[i,5] = sum( cart_sim[[2]][[i]][,4])
  res_primas[i,6] = cart_clientes[i,2]
  res_primas[i,7] = res_primas[i,1] + res_primas[i,4]
  res_primas[i,8] = res_primas[i,2] + res_primas[i,5]
  res_primas[i,9] = res_primas[i,3] + res_primas[i,6]
}

primas <- array(dim = c(10,1))
for(i in 1:10){
  ppc = cart_clientes[i,1]*mean(datos_cred$prom_monto_contracargo)*
    sum(datos_cred$Siniestros)/sum(datos_cred$Expuestos)*3/2*mean(datos_cred$tasa_comp_anual)
  ppd = cart_clientes[i,2]*1201.6609*
    sum(datos_debt$Siniestros)/sum(datos_debt$Expuestos)*mean(datos_cred$tasa_comp_anual)
  primas[i] = ppc + ppd
}
res_primas[,7]/ primas
res_primas[,7]
#res_primas[8,7] = primas[8]
#res_primas[9,7] = primas[9]
#sum(res_primas[,7] - primas)/sum(primas)
#res_primas[10, 7] = primas[10]
res_inf <- inf_res(res_primas, 0.04, 10)
res_inf[,c(3,6,9)] = res_inf[,c(3,6,9)] / inf_prim(10, 0.04)
res_inf[,9] <- round(res_inf[,9])
res_inf[,6] <- round(res_inf[,6])
res_inf[,3] <- round(res_inf[,3])
primas_anual <- prim_acum(prima_dev)

escalera <- escal_reserva(10, res_inf, 0.1, 0.07, 0.08, 0.06, 0.025)
cuadro_reserva_anual <- cuadro_res_anual(10, res_inf[, 8], 0.07, 0.08)

cuadro_reserva_final <- res_final(10, escalera)
cuadro_reserva_final[,1:10]
cuadro_reserva_final[10, 12:20]
mean(cuadro_reserva_final[10, 12:20])
sol_anual_tcl(res_primas[9,3], res_primas[9,6], res_primas[9,c(1,4)], c(mean(datos_cred$prom_monto_contracargo)*0.01,
                            0.01, mean(datos_debt$prom_monto_contracargo)*0.01, 0.01), 10000)
RCS <- sol_final(10, res_primas[,c(3,6)], res_primas[,c(1,4)], 
                 c(2104.7016*0.01, 0.01, 1201.6609*0.01, 0.01), 10000)

res_seguro(cuadro_reserva_final[,1], RCS[1,], 0.07)
results <- res_seg_todo(10, cuadro_reserva_final, RCS, 0.07 )

archivo_primas(matrix(c(1000, 1000), nrow = 1, ncol = 2),  c( 0.0001, 0.0001), 
               c(0.08, .03,  .06, .09, .01, .09) )
archivo_cartera()
archivo_reserva()
archivo_financiero(0.07, 0.05)






prima_pura <- function(n, b0, q0, compra, tipo){
  sev_glo = mean(datos$prom_monto_contracargo)
  sin_glo =  3/2*mean(datos$tasa_comp_anual)
  if(compra <= 50000){
    if ( tipo == "cred"){
      med_sev = mean(datos_cred$prom_monto_contracargo)
      sd_sev = sd(datos_cred$prom_monto_contracargo)
      f_hat = sum(datos_cred$Siniestros)/sum(datos_cred$Expuestos)
      med_sin = 3/2*mean(datos_cred$tasa_comp_anual)
      sd_sin =sd(datos_cred$tasa_comp_anual)
      zy = 1/(1 + q0/n)
      y_hat = med_sin*zy + (1 - zy)*sin_glo
      zx = 1/(1 + b0/n)
      x_hat = zx*med_sev + (1 - zx)*sev_glo
      if ( compra <= 35000 ) {
          return( f_hat*y_hat * x_hat )
      } else {
          return(f_hat*((y_hat)* x_hat + sd_sev*sd_sin))
        }
    }   else {
        q0 = 1
        b0 = 1.5
        med_sev = mean(datos_debt$prom_monto_contracargo)
        sd_sev = sd(datos_debt$prom_monto_contracargo)
        f_hat = sum(datos_debt$Siniestros)/sum(datos_debt$Expuestos)
        med_sin = mean(datos_cred$tasa_comp_anual)
        sd_sin =sd(datos_debt$tasa_comp_anual)
        zy = 1/(1 + q0/n)
        y_hat = med_sin*zy + (1 - zy)*sin_glo
        zx = 1/(1 + b0/n)
        x_hat = zx*med_sev + (1 - zx)*sev_glo
        if ( compra <= 20000 ) {
          return(f_hat*y_hat * x_hat  )
        } else {
            return((f_hat*((y_hat+sd_sin)* x_hat + sd_sev*sd_sin)) )
      } 
    }
  } else {
    return(3/2*sin_glo*sev_glo*0.027)
  }
}
prima_tarifa <- function(prima, param){
  C = param[1]
  B = param[2] 
  GAj = param[3]
  GA = param[4]
  GP = param[5]
  U = param[6]
  if(prima > 0){
    return( ( prima*(1 + GAj))/ (1 - C - B - GP -GA - U))
  } else {
    return(0)
  }
}
proyec_crec <- function(years, ventas){
  crec_ventas = c(1.94724610, 2.348790602, 1.641674798, 1.48151731, 1.243173149)
  mean_grow = (prod(crec_ventas))^(1/length(crec_ventas)) 
  merc = rep(0, years +1)
  merc_pot = merc
  cart = merc
  cart_mens = merc
  merc[1] = ventas[length(ventas)]
  cart[1] = round(45548.87263+221720.9734)
  for(i in 2:(years+1) ){
    merc[i] = merc[i-1]*mean_grow
    merc_pot[i] = round(merc[i]*0.1)
    cart[i] = round(cart[i-1]*(1.15) )
    cart_mens[i] = round(cart[i]/12)
  }
  return(cbind(merc, merc_pot, cart, cart_mens))
}
proyec_tipo <- function(cred, vent_pro, tas_crec, years){
  pro_cred = rep(0, years )
  pro_debt = pro_cred
  for(i in 1:years){
      if(i == 1){
        pro_cred[i] = round(vent_pro[1])
        pro_debt[i] = round(vent_pro[2])
      } else {
      pro_cred[i] = round( pro_cred[i-1]*(1+tas_crec) )
      pro_debt[i] = round( pro_debt[i-1]*(1+tas_crec) )
      }
    }
  return(cbind(pro_cred, pro_debt) )
}
sim_cart <- function( years, cart_tot, param_pura, param_tarif){
  cart_cred <- list()
  cart_debt <- list()
  for(i in 1:years){
    cart_cred[[i]] = array(dim = c(cart_tot[i,1], 6))
    cart_debt[[i]] = array(dim= c(cart_tot[i,2], 6))
    
  }
  b0 = param_pura[1]
  q0 = param_pura[2]
  pb <- txtProgressBar(min = 0, max = years, style = 1)
  for(i in 1: years){
    sim = rep(0, cart_tot[i,1])
    pp = sim
    pt = sim
    sin = sim
    ptm = sim
    simd = rep(0, cart_tot[i,2])
    ppd = simd
    ptd = simd
    sind = simd
    ptmd = simd
    lam_cre = mean(datos_cred$tasa_comp_anual)
    lam_deb = mean(datos_debt$tasa_comp_anual)
    for(j in 1:cart_tot[i, 1]){
      sin[j] = rpois(1, lam_cre)
      sim[j] = min(sum(rgamma(sin[j], 21.13002, 0.01) ), 50000)
      pp[j] = prima_pura(6, b0, q0, sim[j], "cred" )
      pt[j] = prima_tarifa(pp[j], param_tarif)
      ptm[j] = pt[j]/12
    }
    for(l in 1:cart_tot[i,2]){
      sind[l] = rpois(1, lam_deb)
      simd[l] = min(sum(rgamma(sind[l], 11.00401, 0.01) ), 30000)
      ppd[l] = prima_pura(6, b0, q0, simd[j], "debt" )
      ptd[l] = prima_tarifa(ppd[l], param_tarif)
      ptmd = ptd[l]/12
    }
    cart_cred[[i]] <- cbind(sin, sim, pp, pt, ptm, rep(112, cart_tot[i,1]) )
    cart_debt[[i]] <- cbind(sind, simd, ppd, ptd, ptmd, rep(112, cart_tot[i,2]) )
    colnames(cart_cred[[i]] ) <- c("Compras Esperadas", "Monto Contracargos", "Prima Pura", "Prima Tarifa",
                                   "Pago Mensual", "Pago Mensual Santander-Zurich")
    colnames(cart_debt[[i]]) <- c("Compras Esperadas", "Monto Contracargos", "Prima Pura", "Prima Tarifa",
                                  "Pago Mensual", "Pago Mensual Santander-Zurich")
    Sys.sleep(0.5)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(list(cart_cred, cart_debt))
}
inf_prim <- function(years, tasa){
  y = rep(0, years)
  for(i in 1:years){
    y[i] = (1 + tasa)^(i-1)
  }
  return(y)
}
inf_res <- function(res, tasa, years){
  y = inf_prim(years, tasa)
  res_inf = res
  for(i in 1:nrow(res)){
    for(j in 1:ncol(res)){
      res_inf[i,j] = res[i,j]*y[i]
        }
  }
  return(res_inf)
}
prim_acum <- function(por_prim){
  a_susc = 1/4*c(3.5, 2.5, 1.5, 0.5)
  a_sig = 1 - a_susc
  acum_susc = por_prim * a_susc
  acum_sig = por_prim * a_sig
  return(cbind(por_prim, a_susc, a_sig, acum_susc, acum_sig))
}
res_anual <- function(t_fin, com, prim_1){
  prim_ene = (1 -com)*prim_1
  ana_res = array(dim = c(12, 9) )
  disc = prim_ene/12
  for(i in 1:12){
    ana_res[i,1] = prim_ene - (i-1)*disc
    ana_res[i,2] = ana_res[i,1]*(t_fin/12)
    }
  tot_fin = sum(ana_res[, 2])
  ana_res[,6] = seq(.5, 11.5, 1)
  for(i in 1:12){
    ana_res[i,3] = ana_res[i,2] / tot_fin
  }
  for( i in 1:12){
    if(i==1){
      ana_res[i,4] = ana_res[i,2]
      ana_res[i,5] = ana_res[i,3]
      ana_res[i,7] = ana_res[i,5]/2
    } else {
      ana_res[i,4] = ana_res[i,2] + ana_res[(i-1),4]
      ana_res[i,5] = ana_res[i,3] + ana_res[(i-1),5]
      ana_res[i,7] = (ana_res[i,5] + ana_res[(i-1),5])/2
     }
    }
  for( i in 1:12){
    ana_res[i,8] = switch(i, 0, prima_dev[1], 0, 0, prima_dev[2], 0, 0, prima_dev[3], 0, 0, prima_dev[4],  0 )
  }
  for(i in 1:12){
    ana_res[i,9] = ana_res[i,7]*ana_res[i,8]
  }
  rownames(ana_res) <- as.character(c(1:12))
  colnames(ana_res) <- c("Reserva Acumuluda", "Prod Financiero Mensual", "Porcentaje"
   , "Monto Acumulado", "Porcentaje Acumulado", "Meses Prima Unif", "Porcentaje Ajustado Acumulado"
   , "Porcentaje Prima", "Porcentaje Acumulado PF")
  return(ana_res)
}
cuadro_res_anual <- function(years, primas, t_fin, com){
  cuadro <- list()
  for(i in 1:years){
    cuadro[[i]] <- array(dim = c(12, 9))
    cuadro[[i]] <- res_anual(t_fin, com, primas[i])
  }
  return(cuadro)
}
res_trans <- function(prim_1, acum_sus, gastos, t_fin, com, gaj, bonos, pura_1){
  reserva <- array(dim = c(11, 4))
  a2 = 1 - acum_sus
  res_ana = res_anual(t_fin, com, prim_1)
  pf_year = sum(res_ana[,9])
  reserva[1,1] = prim_1
  reserva[2,1] = a2*(prim_1*(1-com))
  reserva[3,1] = reserva[1,1] - reserva[2,1]
  reserva[4,1] = com*prim_1
  reserva[5,1] = acum_sus*pura_1
  reserva[6,1] = reserva[5,1]*gaj
  reserva[7,1] = prim_1*bonos*acum_sus
  reserva[8,1] = reserva[3,1] - (reserva[4,1] + reserva[5,1]  + reserva[6,1])
  reserva[9,1] = gastos*prim_1*acum_sus
  reserva[10,1] = reserva[8,1] - reserva[9,1] - reserva[7,1]
  reserva[11,1] = res_ana[4,4] + (res_ana[5,4] - res_ana[4,4])/(res_ana[5,5] - res_ana[4,5])*(pf_year - res_ana[4,5])
  reserva[1,3] = 0 
  reserva[2,3] = - reserva[2,1]
  reserva[3,3] = reserva[1,3] - reserva[2,3]
  reserva[4,3] = 0
  reserva[5,3] = pura_1*a2
  reserva[6,3] = reserva[5,3]*gaj
  reserva[7,3] = prim_1*bonos*a2
  reserva[8,3] = reserva[3,3] - (reserva[4,3] + reserva[5,3]  + reserva[6,3])
  reserva[9,3] = gastos*prim_1*a2
  reserva[10,3] = reserva[8,3] - reserva[9,3] - reserva[7,3]
  reserva[11,3] = res_ana[3,4] + (res_ana[4,4] - res_ana[3,4])/(res_ana[4,5] - res_ana[3,5])*( 1 - pf_year - res_ana[3,5])
  for(i in 1:11){
    reserva[i,2] = reserva[i,1]/prim_1
    reserva[i,4] = reserva[i,3]/prim_1
  }
  colnames(reserva) <- c("Year Susc", "Porcentaje", "Year Sig", "Porcentaje")
  rownames(reserva) <- c("Primas", "INC RR", "Primas Devengadas", "Comisiones", "Siniestros", 
                         "Gasto Ajuste", "Bonos", "Utilidad Tecnica", "Gastos", "Utilidad Bruta", "PF")
  return(reserva)
}
escal_reserva <- function(years, base_primas, gastos, t_fin, com, gaj, bonos){
  reserva_escalera <- list()
  for(i in 1:years){
    reserva_escalera[[i]] <- array(dim = c(11,4))
  }
  acum = sum(primas_anual[,4])
  for(i in 1:years){
    reserva_escalera[[i]] <- res_trans(base_primas[i,8], acum, gastos, t_fin, com, gaj, bonos, base_primas[i,7])
  }
  return(reserva_escalera)
}
res_final <- function(years, escal){
    reserf <- array( dim = c(11, years))
    riesg <-c(1:years)
    porserf <- array(dim=c(11, years))
    for(i in 1:years){
      if(i == 1){
        reserf[,i] = escal[[i]][,1]
      } else {
        reserf[, i] = escal[[i]][,1] + escal[[(i-1)]][,3]
      }
    }
    for( i in 1:years){
      if(i ==1){
        riesg[i] = reserf[2,1]
      } else {
        riesg[i] = reserf[2, i] + riesg[(i-1)]
      }
    }
    for(i in 1:years){
      porserf[, i] = reserf[,i]*(1/reserf[1,i])
    }
    reserf <- rbind(reserf, riesg)
    porserf <- rbind(porserf, rep(NA, years))
    reserf <- cbind(reserf, porserf)
    rownames(reserf) <-  c("Primas", "INC RR", "Primas Devengadas", "Comisiones", "Siniestros", 
                           "Gasto Ajuste", "Bonos", "Utilidad Tecnica", "Gastos", "Utilidad Bruta", "PF", 
                           "Riesgos en Curso")
    colnames(reserf) <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", 
                          "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")
    return(reserf)
}
archivo_cartera <- function(){
  write.csv(cart_clientes, "proyecc_clientes.csv")
  for(i in 1:10){
    write.csv(cart_sim[[1]][[i]], paste0("carte_simulada_cred_", i, ".csv") )
    write.csv(cart_sim[[2]][[i]], paste0("cartera_simulada_debt_",i,".csv") )
  }
  return("listo :)")
}
archivo_reserva <- function(){
  write.csv(res_primas, "resumen_muestreo_NA.csv")
  write.csv(res_inf, "resumen_muestreo_AI.csv")
  write.csv(primas_anual, "prima_devengada.csv")
  write.csv(cuadro_reserva_final, "cuadro_reserva_final.csv")
  for(i in 1:10){
    write.csv(cuadro_reserva_anual[[i]], paste0("cuadro_reserva_anual_", i, ".csv") )
    write.csv(escalera[[i]], paste0("escalera_", i, ".csv") )
  }
  return("listo :)")
}
sol_anual <- function(pol_cred, pol_debt, primas, nsim){
  a0 = 21.13002
  b0 = 0.01
  p0 = 11.00401
  q0 = 0.01
  f_cred = sum(datos_cred$Siniestros)/sum(datos_cred$Expuestos)
  f_debt = sum(datos_debt$Siniestros)/sum(datos_debt$Expuestos)
  lam_cred = 3/2*mean(datos_cred$tasa_comp_anual)
  lam_debt = 3/2*mean(datos_debt$tasa_comp_anual)
  loss <- rep(0, nsim)
  pb <- txtProgressBar(min = 0, max = nsim, style = 1)
  for(k in 1:nsim){
    per_cred = 0
    per_debt = 0
    for(i in 1:pol_cred){
      compras = rpois(1, lam_cred)
      per_cred = min(sum(rgamma(compras, a0, b0)*rbinom(compras, 1, f_cred) ), 50000 ) + per_cred
    }
    for(i in 1:pol_debt){
      compras = rpois(1, lam_debt)
      per_debt = min(sum(rgamma(compras, p0, q0)*rbinom(compras, 1, f_debt) ), 50000 ) + per_debt
    }
    loss[k] = per_cred + per_debt - primas
    setTxtProgressBar(pb, k)
  }
  close(pb)
  hist(loss, breaks = 20, main = "Histograma de L", xlab = paste("media: ", round( mean(loss), digits = 2)), col ="cyan" )
  abline(v = 0,  col = "red", lwd = 3, lty =2)
  abline(v=quantile(loss, probs=c(0.995)), col = "black", lwd = 2, lty = 2)
  return(quantile(loss, probs = c(0.995))-mean(loss))
}
sol_final <- function(years, car_clien, primas, param_L, nsim){
  rcs <- array(dim = c(years, 5))
  rcs[,2] = rep(0, years)
  for(i in 1:years){
      png(paste0("histo_L_", i,".png"), width = 800, height = 794, res = 240)
      rcs[i,1] <- sol_anual_tcl(car_clien[i,1], car_clien[i,2], primas[i,], param_L, nsim)
      dev.off()
      rcs[i,3] = 0.15*primas[i]
      rcs[i,4] = rcs[i,1] + rcs[i,2] + rcs[i,3]
      rcs[i,5] = rcs[i,4]*(1.04)^(i-1)
  }
  png("RCS_PAMI.png", width = 800, height = 794, res = 240)
  plot(x = c(1:years), y = rcs[,1], main ="RCS TC 99.5%", ylab = "RCS TC", xlab = "Tiempo", col = "darkgreen",
       type  = "b", pch = 19, lwd = 2)
  dev.off()
  png("RCS_PAMI_inf.png", width = 800, height = 794, res = 240)
  plot(x = c(1:years), y = rcs[,1]*inf_prim(10,0.04), main ="RCS TC 99.5% Proyectado", ylab = "RCS TC", xlab = "Tiempo", col = "darkgreen",
       type  = "h", pch = 19, lwd = 8)
  dev.off()
  rownames(rcs) <- as.character(c(2021:2030))
  colnames(rcs) <- c("RCS TC", "RCS RF", "RCS RO", "RCS", "RCS Ajustado Inf.")
  write.csv(rcs, file = "cuadro_rcs.csv")
  return(rcs)
}
sol_anual_tcl <- function(pol_cred, pol_debt, primas,  param_L, nsim){
  a0 = param_L[1]
  b0 = param_L[2]
  p0 = param_L[3]
  q0 = param_L[4]
  f_cred = sum(datos_cred$Siniestros)/sum(datos_cred$Expuestos)
  f_debt = sum(datos_debt$Siniestros)/sum(datos_debt$Expuestos)
  lam_cred = 3/2*mean(datos_cred$tasa_comp_anual)
  lam_debt = mean(datos_cred$tasa_comp_anual)
  mu_L_c = pol_cred*(a0/b0)*lam_cred*f_cred - primas[1]
  mu_L_d = pol_debt*(p0/q0)*lam_debt*f_debt - primas[2]
  var_L_c = pol_cred*f_cred*lam_cred*(a0/(b0^2) + a0^2/b0^2) 
  var_L_d = (p0/(q0^2) + p0^2/q0^2)*lam_debt*f_debt*pol_debt
  loss <- rnorm(nsim, mean = mu_L_c, sd = sqrt(var_L_c) ) + rnorm(nsim, mean = mu_L_d, sd = sqrt(var_L_d) )
  hist(loss, breaks = 30, main = "Histograma de L", xlab = paste("media: ", round( mean(loss), digits = 2)), col ="cyan" )
  abline(v = 0,  col = "red", lwd = 3, lty =2)
  abline(v=quantile(loss, probs=c(0.995)), col = "black", lwd = 2, lty = 2)
  return(quantile(loss, probs = c(0.995))-mean(loss))
}
archivo_primas <- function(cart_tot, param_pura, param_tarif){
  cuadro <- sim_cart(1, cart_tot, param_pura, param_tarif)
    write.csv(cuadro[[1]][[1]],"cuadro_primas_cred.csv")
    write.csv(cuadro[[2]][[1]], "cuadro_primas_debt.csv")
  return("cuadro listo")
}
res_seguro <- function(res_fin, rcs, t_fin){
  cuadro <- array(dim = c(12, 1))
  cuadro[1,1] = res_fin[12]/(1+t_fin)
  cuadro[2,1] = rcs[1]
  cuadro[3,1] = rcs[3]
  cuadro[5,1] = rcs[4]
  cuadro[4,1] = 0.1*cuadro[5,1]
  cuadro[6,1] = cuadro[1,1] + cuadro[4,1]
  cuadro[8,1] = res_fin[1] + res_fin[11]
  cuadro[9,1] = cuadro[8,1] - cuadro[6,1]
  cuadro[7,1] = cuadro[9,1] - cuadro[5,1]
  cuadro[10,1] = res_fin[1] - res_fin[2] - res_fin[4] - res_fin[5] - res_fin[9] 
  cuadro[11,1] = cuadro[10,1]*0.7
  cuadro[12,1] = cuadro[10,1]  / cuadro[9,1]
  rownames(cuadro) <- c("BEL", "RT", "RO", "MR", "RCS", "VEP", "MS", "Activo", "Capital", "Utilidad",
                        "Utilidad desp. Imp.", "Margen")
  return(cuadro)
}
res_seg_todo <- function(years, cuad_res, rcs, t_fin){
  cart_seg <- array(dim = c(12, years))
  t_fin <- rep(t_fin, years)
  for(i in 1:years){
    cart_seg[,i] <- res_seguro(cuad_res[, i], rcs[i,], t_fin[i])
  }
  colnames(cart_seg) <- as.character(c(2021:2030))
  rownames(cart_seg) <- c("BEL", "RT", "RO", "MR", "RCS", "VEP", "MS", "Activo", "Capital", "Utilidad",
                        "Utilidad desp. Imp.", "Margen")
  return(cart_seg)  
}
requer <- function(capital){
  reqi <- rep(0, length(capital))
  for(i in 1:length(capital)){
    if(i == 1){
      reqi[i] = capital[i]
    } else {
      reqi[i] = capital[i] - capital[i-1]
    }
  }
  return(reqi)
}
archivo_financiero <- function(pesim, opt){
  roe_prom = ( 1 + sum(results[10,] -requer(results[9,]) ) / results[9,1] )^(1/10) - 1
  cuad_opt = cuadro_reserva_final
  cuad_pesim = cuad_opt 
  cuad_pesim[5,] = cuadro_reserva_final[5,]*(1+pesim)
  cuad_opt[5,] = cuadro_reserva_final[5,]*(1 - opt)
  results_p <- res_seg_todo(10, cuad_pesim, RCS, 0.07 )
  results_o <- res_seg_todo(10, cuad_opt, RCS, 0.07)
  roe_prom_p = ( 1 + sum(results_p[10,] -requer(results_p[9,]) ) / results_p[9,1] )^(1/10) - 1
  roe_prom_o = ( 1 + sum(results_o[10,] -requer(results_o[9,]) ) / results_o[9,1] )^(1/10) - 1
  write.csv(results, "prod_base.csv")
  write.csv(results_p, "prod_pesim.csv")
  write.csv(results_o, "prod_opt.csv")
  write.csv(cbind(results[10,] - requer(results[9,]), rep(" ", 10), c(as.character(roe_prom), rep(" ", 9))), "ROE_Tir.csv")
  write.csv(cbind(results_p[10,] - requer(results_p[9,]), rep(" ", 10), c(as.character(roe_prom_p), rep(" ", 9))), "ROE_Tir_p.csv")
  write.csv(cbind(results_o[10,] - requer(results_o[9,]), rep(" ", 10), c(as.character(roe_prom_o), rep(" ", 9))), "ROE_Tir_o.csv")
}

compras_2019 = 422557179
compras_2020 = 248889006
compras_2018 = 285219198
trim_2019 = c(86003961, 106097657, 110643600, 119811961)
trim_2018 = c(57890373, 67791068, 71565968, 87971789)
trim_2020 = c(119046557, 129842449)
dev_2018 = trim_2018 / compras_2018
dev_2019 = trim_2019 / compras_2019
dev_2020 = trim_2020 / (2 *compras_2020)

print(cbind(dev_2018, dev_2019, dev_2020))
print(rowMeans(cbind(dev_2018, dev_2019, dev_2020)))
prima_dev = rowMeans(cbind(dev_2018, dev_2019, dev_2020))
prima_dev

png("merc_crec_cred.png", width = 1200, height = 794, res = 240)
par(mfrow=c(1,2))
plot(c(2021:2030), cart_clientes[,1], type = "l", xlab ="Tiempo",
     ylab = "Ventas anuales", lwd = 2, main = "Cartera Cred.")
plot(c(2021:2030), cart_clientes[,2], type = "l", xlab ="Tiempo",
     ylab = "Ventas anuales", lwd = 2, main = "Cartera Debt.")
par(mfrow=c(1,1))
dev.off()
cart_clientes
