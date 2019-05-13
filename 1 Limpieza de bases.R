#########################
### Limpieza de bases ###
########################

setwd('C:/Users/usuario/Desktop/1er Reto DA/1 Bases originales')

#### Packages =============================================

#install.packages("foreign")
#install.packages("readstata13")
#install.packages("tidyr")
#install.packages("car")
#install.packages("dplyr")
library(foreign)
library(readstata13)
library(tidyr)
library(car)
library(dplyr)

#### Importado de bases ===================================

#vivienda <- read.dta("enaho01-2017-100_RetoDA.dta")
vivienda <- read.dta13("enaho01-2017-100_RetoDA.dta")
miembros <- read.dta13("enaho01-2017-200_RetoDA.dta")
educacion <- read.dta13("enaho01-2017-300_RetoDA.dta")
salud <- read.dta13("enaho01a-2017-400_RetoDA.dta")
activos <- read.dta13("enaho01-2017-612_RetoDA.dta")
ingresos <- read.dta13("sumaria-2017_RetoDA.dta")

#### Merge de bases =======================================

activos_long <- activos
activos_wide <- spread(activos_long, p612n, p612)

miembros_jh <- miembros[which(miembros$p203=='jefe/jefa'),]

total <- merge(miembros_jh,educacion,by=c("aÑo","mes","ubigeo","conglome","vivienda","codperso"), all.x = TRUE)
total <- merge(total,salud,by=c("aÑo","mes","ubigeo","conglome","vivienda","codperso"), all.x = TRUE)
total <- merge(total,vivienda,by=c("aÑo","mes","ubigeo","conglome","vivienda"), all.x = TRUE)
total <- merge(total,activos_wide,by=c("ubigeo","conglome","vivienda","hogar"), all.x = TRUE)
total <- merge(total,ingresos,by=c("ubigeo","conglome","vivienda","hogar"), all.x = TRUE)

##incluir hogar en bases

#### Recodificacion y limpieza ===========================

## Basica
total$hombre <- car::recode(total$p207, "'mujer' = 0; 'hombre' = 1")
total$edad <- total$p208a

## Educacion
total$castellano <- dplyr::recode(total$p300a, "castellano" = 1, .default = 0)
total$leng_nativa <- dplyr::recode(total$p300a, "quechua" = 1,"aymará" = 1,"otra lengua nativa" = 1, .default = 0)
total$leng_extran <- dplyr::recode(total$p300a, "portugués" = 1,"otra lengua extranjera" = 1, .default = 0)

total$educ <- dplyr::recode(total$p301a, 
                            "sin nivel" = 0,
                            "educación inicial" = 2, 
                            "primaria incompleta" = 5, 
                            "primaria completa" = 8, 
                            "secundaria incompleta" = 10.5, 
                            "secundaria completa" = 13, 
                            "superior no universitaria incompleta" = 14.5, 
                            "superior no universitaria completa" = 16, 
                            "superior universitaria incompleta" = 15.5, 
                            "superior universitaria completa" = 18, 
                            "postgrado universitario" = 20, 
                            .default = NULL)


## Salud

total$essalud <- car::recode(total$p4191, "'no' = 0; 'essalud' = 1")
total$seg_priv <- car::recode(total$p4192, "'no' = 0; 'seguro privado de salud' = 1")
total$eps <- car::recode(total$p4193, "'no' = 0; 'entidad prestadora de salud' = 1")
total$seg_ffaa <- car::recode(total$p4194, "'no' = 0; 'seguro ff.aa./policiales' = 1")
total$sis <- car::recode(total$p4195, "'no' = 0; 'seguro integral de salud (sis)' = 1")
total$seg_univ <- car::recode(total$p4196, "'no' = 0; 'seguro universitario' = 1")
total$seg_otr <- car::recode(total$p4198, "'no' = 0; 'otro' = 1")


## Activos

total$casa_ind <- dplyr::recode(total$p101, "casa independiente" = 1, .default = 0)
total$casa_dep <- dplyr::recode(total$p101, "departamento en edificio" = 1, .default = 0)
total$casa_qui <- dplyr::recode(total$p101, "vivienda en quinta" = 1, .default = 0)
total$casa_vec <- dplyr::recode(total$p101, "vivienda en casa de vecindad (callejón, solar o corralón)" = 1, .default = 0)
total$casa_cho <- dplyr::recode(total$p101, "choza o cabaña" = 1, .default = 0)
total$casa_otr <- dplyr::recode(total$p101, "vivienda improvisada" = 1, "local no destinado para habitación humana" = 1, .default = 0)

total$pared_lad <- dplyr::recode(total$p102, "ladrillo o bloque de cemento" = 1, .default = 0)
total$pared_cem <- dplyr::recode(total$p102, "piedra o sillar con cal o cemento" = 1, .default = 0)
total$pared_ado <- dplyr::recode(total$p102, "adobe" = 1, .default = 0)
total$pared_tap <- dplyr::recode(total$p102, "tapia" = 1, .default = 0)
total$pared_qui <- dplyr::recode(total$p102, "quincha (caña con barro)" = 1, .default = 0)
total$pared_pbar <- dplyr::recode(total$p102, "piedra con barro" = 1, .default = 0)
total$pared_mad <- dplyr::recode(total$p102, "madera" = 1, .default = 0)
total$pared_est <- dplyr::recode(total$p102, "estera" = 1, .default = 0)
total$pared_otr <- dplyr::recode(total$p102, "otro material" = 1, .default = 0)

total$piso_par <- dplyr::recode(total$p103, "parquet o madera pulida" = 1, .default = 0)
total$piso_lam <- dplyr::recode(total$p103, "láminas asfálticas, vinílicos o similares" = 1, .default = 0)
total$piso_los <- dplyr::recode(total$p103, "losetas, terrazos o similares" = 1, .default = 0)
total$piso_mad <- dplyr::recode(total$p103, "madera (entablados)" = 1, .default = 0)
total$piso_cem <- dplyr::recode(total$p103, "cemento" = 1, .default = 0)
total$piso_tie <- dplyr::recode(total$p103, "tierra" = 1, .default = 0)
total$piso_otr <- dplyr::recode(total$p103, "otro material" = 1, .default = 0)

total$techo_con <- dplyr::recode(total$p103a, "concreto armado" = 1, .default = 0)
total$techo_mad <- dplyr::recode(total$p103a, "madera" = 1, .default = 0)
total$techo_tej <- dplyr::recode(total$p103a, "tejas" = 1, .default = 0)
total$techo_cal <- dplyr::recode(total$p103a, "planchas de calamina, fibra de cemento o similares" = 1, .default = 0)
total$techo_cañ <- dplyr::recode(total$p103a, "caña o estera con torta de barro" = 1, .default = 0)
total$techo_est <- dplyr::recode(total$p103a, "estera" = 1, .default = 0)
total$techo_paj <- dplyr::recode(total$p103a, "paja, hojas de palmera" = 1, .default = 0)
total$techo_otr <- dplyr::recode(total$p103a, "otro material" = 1, .default = 0)

total$n_hab <- total$p104a

total$estado_alq <- dplyr::recode(total$p105a, "alquilada" = 1, .default = 0)
total$estado_pro_pag <- dplyr::recode(total$p105a, "propia, totalmente pagada" = 1, .default = 0)
total$estado_pro_inv <- dplyr::recode(total$p105a, "propia, por invasión" = 1, .default = 0)
total$estado_pro_plz <- dplyr::recode(total$p105a, "propia, comprándola a plazos" = 1, .default = 0)
total$estado_ced_tra <- dplyr::recode(total$p105a, "cedida por el centro de trabajo" = 1, .default = 0)
total$estado_ced_otr <- dplyr::recode(total$p105a, "cedida por otro hogar o institución" = 1, .default = 0)
total$estado_otr <- dplyr::recode(total$p105a, "otra forma" = 1, .default = 0)


total$agua <- dplyr::recode(total$p110c, "si" = 1, .default = 0)

total$desag <- dplyr::recode(total$p111a, "red pública de desagüe dentro de la vivienda" = 1, "red pública de desagüe fuera de la vivienda pero dentro del edificio" = 1,.default = 0)

total$ele <- dplyr::recode(total$p1131, "electricidad" = 1, .default = 0)
total$glp <- dplyr::recode(total$p1132, "gas (glp)" = 1, .default = 0)
total$gn <- dplyr::recode(total$p1133, "gas natural" = 1, .default = 0)
total$car <- dplyr::recode(total$p1135, "carbón" = 1, .default = 0)
total$leñ <- dplyr::recode(total$p1136, "leña" = 1, .default = 0)

total$tel_fij <- dplyr::recode(total$p1141, "teléfono(fijo)" = 1, .default = 0)
total$tel_mov <- dplyr::recode(total$p1142, "celular" = 1, .default = 0)

total$tv_cab <- dplyr::recode(total$p1143, "tv. cable" = 1, .default = 0)
total$internet <- dplyr::recode(total$p1144, "internet" = 1, .default = 0)

total$radio <- dplyr::recode(total$radio, "si" = 1, .default = 0)
total$tv <- dplyr::recode(total$`tv a color`, "si" = 1, .default = 0)
total$compu <- dplyr::recode(total$computadora, "si" = 1, .default = 0)
total$planc <- dplyr::recode(total$plancha, "si" = 1, .default = 0)
total$licua <- dplyr::recode(total$licuadora, "si" = 1, .default = 0)
total$cocin <- dplyr::recode(total$`cocina a gas`, "si" = 1, .default = 0)
total$refri <- dplyr::recode(total$`refrigeradora/congeladora`, "si" = 1, .default = 0)
total$lava <- dplyr::recode(total$lavadora, "si" = 1, .default = 0)
total$horno <- dplyr::recode(total$`horno microondas`, "si" = 1, .default = 0)
total$auto <- dplyr::recode(total$`auto, camioneta`, "si" = 1, .default = 0)
total$moto <- dplyr::recode(total$motocicleta, "si" = 1, .default = 0)


## Ingresos

total$n_per <- total$mieperho
total$ing <- total$ingmo1hd


total_limp <- total
select (total_limp,-c(mpg,cyl,wt))
