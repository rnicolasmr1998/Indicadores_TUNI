  cat("\014")
  '*********************************************************************************
  # BASE DE DATOS:	TUNI
  # PROYECTO: 		  INDICADORES EDUCATIVOS
  # TÍTULO:         MASTER DATA
  # AÑOS:				    2020-2022
  # AUTOR: 			    RENZO NICOLAS MARROQUIN RUBIO
  *********************************************************************************'

  # Outline: -----------------------------------------------------------------------

  {'
  1. Ruta de trabajo y globals
    1.1. Instalar paquetes requeridos
    1.2. Configurar usuarios
    1.3. Configurar carpetas
    1.4. Configurar ejecución
  2. Indicadores TUNI
  '}

  # ********************************************************************************
  # PART 1: Ruta de trabajo y globals ----------------------------------------------
  # ********************************************************************************

  rm(list = ls())               # Limpiar memoria
  options(scipen = 999)         # Deshabilitar la notación científica

  ## 1.1. Instalar librerias requeridas --------------------------------------------

  if (!require("pacman")) {install.packages("pacman")}
  pacman::p_load(data.table, dplyr, tidyr, magrittr, stringr, stringi, haven)
  pacman::p_load(ggplot2, RColorBrewer, ggcorrplot, gridExtra, ggthemes, hrbrthemes, extrafont, viridis)
  pacman::p_load(summarytools, openxlsx, expss, psych, gtsummary)

  ## 1.2. Configurar usuarios ------------------------------------------------------

  if (Sys.info()[["user"]] == "rnico")           {setwd("D:/NICOLAS")}
  if (Sys.info()[["user"]] == "renzomarroquin")  {setwd("D:/NICOLAS")}
  if (Sys.info()[["user"]] == "PIERO ALEJANDRO") {setwd("D:/NICOLAS")}

  ## 1.3. Configurar carpetas ------------------------------------------------------

  proyecto      <- paste(getwd(),  "TUNI"            ,  sep = "/")
  base_de_datos <- paste(proyecto, "01_Base_de_Datos",  sep = "/")
  codigo        <- paste(proyecto, "02_Codigo"       ,  sep = "/")
  input         <- paste(proyecto, "03_Input"        ,  sep = "/")
  resultados    <- paste(proyecto, "04_Resultados"   ,  sep = "/")
  documentacion <- paste(proyecto, "05_Reporte"      ,  sep = "/")
  alerta        <- paste(proyecto, "06_Alerta"       ,  sep = "/")

  ## 1.4. Configurar ejecución -----------------------------------------------------

  interrupcion_temprana   <- TRUE
  indicador_masculinidad  <- TRUE

  # ********************************************************************************
  # PART 2: Indicadores TUNI -------------------------------------------------------
  # ********************************************************************************

  if (interrupcion_temprana) {
    source(paste(codigo, "01_Interrupcion_temprana.R", sep = "/"))
  }
  
  if (indicador_masculinidad) {
    source(paste(codigo, "02_Indicador_de_masculinidad.R", sep = "/"))
  }
