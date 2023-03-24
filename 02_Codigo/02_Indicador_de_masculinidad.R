  cat("\014")
  '***********************************************************************************************************
  # BASE DE DATOS:	MATRICULA TUNI
  # PROYECTO: 		  INDICADORES EDUCATIVOS
  # TÍTULO:         ÍNDICE DE MASCULINIDAD POR GRUPOS DE CARRERAS UNIVERSITARIAS
  # AÑOS:				    2020-2022
  # AUTOR: 			    RENZO NICOLAS MARROQUIN RUBIO
  ***********************************************************************************************************'

  # Outline: -------------------------------------------------------------------------------------------------

  # **********************************************************************************************************
  # PART 1: Variables de inicio ------------------------------------------------------------------------------
  # **********************************************************************************************************

  matricula_total    <- data.table()
  ruta_matriculados  <- paste(base_de_datos, "Matriculados", sep = "/")

  # **********************************************************************************************************
  # PART 2: Importar base de datos ---------------------------------------------------------------------------
  # **********************************************************************************************************

  for (año in 2020:2022) {
  
    matricula_semestre_1 <- fread(paste0(ruta_matriculados, "/", "matriculado_", año, "_I", ".csv"),
                                  sep = "|", encoding = "Latin-1") %>% setnames(tolower)
  
    matricula_semestre_2 <- fread(paste0(ruta_matriculados, "/", "matriculado_", año, "_II", ".csv"),
                                  sep = "|", encoding = "Latin-1") %>% setnames(tolower)
    
  ## 2.1. Unir base de datos ---------------------------------------------------------------------------------  
    
    matricula_total      <- rbindlist(list(matricula_total, matricula_semestre_1, matricula_semestre_2), 
                                      fill = TRUE)
  
  ## 2.2. Limpieza de archivos -------------------------------------------------------------------------------    
    
    rm(list = c("matricula_semestre_1", "matricula_semestre_2"))
  }
  
  # **********************************************************************************************************
  # PART 2: Renombrar variables ------------------------------------------------------------------------------
  # **********************************************************************************************************
  
  setnames(x   = matricula_total,
           old = c("periodo", "nivel_academico", "tipo_constitucion", "sexo"),
           new = c("periodo_original", "matricula_nivel_academico", 
                   "matricula_tipo_constitucion", "matricula_sexo"))
  
  # **********************************************************************************************************
  # PART 3: Generar variables --------------------------------------------------------------------------------
  # **********************************************************************************************************
  
  ## 3.1. Periodo  -------------------------------------------------------------------------------------------
  
  matricula_total[, periodo := factor(periodo_estandarizado)]
  
  ## 3.2. Sexo  ----------------------------------------------------------------------------------------------
  
  matricula_total[, sexo := factor(matricula_sexo)]
  
  ## 3.3. Nivel académico  -----------------------------------------------------------------------------------
  
  matricula_total[, nivel_academico := factor(matricula_nivel_academico)]
  
  ## 3.4. Matricula  -----------------------------------------------------------------------------------------
  
  matricula_total[, matricula := 1]
  
  # **********************************************************************************************************
  # PART 4: Filtrar datos ------------------------------------------------------------------------------------
  # **********************************************************************************************************
  
  matricula_total <- matricula_total[licencia == "Licenciada",]
  
  # **********************************************************************************************************
  # PART 5: Construcción del indicador -----------------------------------------------------------------------
  # **********************************************************************************************************
  
  ## 5.1. Paso 1: Mostrar el indicador en cada periodo por nivel académico -----------------------------------
  
  matricula_total <- matricula_total[, .(matricula = sum(matricula)),
                                     by = .(periodo, codigo_grupo_3, nombre_grupo_3, sexo)]
  
  matricula_total <- dcast(data       = matricula_total,
                           formula    = periodo + codigo_grupo_3 + nombre_grupo_3 ~ sexo,
                           value.var  = "matricula")
  
  {
  # Reemplazar los NA por el valor de cero
  
  matricula_total[, c("Femenino", "Masculino") := lapply(.SD, function(x) replace(x, is.na(x), 0)), 
                  .SDcols = c("Femenino", "Masculino")]
  }
  
  ## 5.2. Paso 2: Crear variables -----------------------------------------------------------------------------
  
  matricula_total[, totales    := (Femenino + Masculino)]
  matricula_total[, indice_mas := round((Masculino/Femenino),2)]
  
  ## 5.3. Paso 3: Eliminar filas ------------------------------------------------------------------------------
  
  matricula_obs   <- matricula_total[(is.na(indice_mas) | is.na(codigo_grupo_3)),]
  matricula_total <- matricula_total[!(is.na(indice_mas) | is.na(codigo_grupo_3)),]
  
  ## 5.4. Paso 4: Transformar un conjunto de datos de formato largo a formato ancho ---------------------------
  
  matricula_total <- dcast(data      = matricula_total,
                           formula   = codigo_grupo_3 + nombre_grupo_3 ~ periodo,
                           value.var = c("Femenino", "Masculino", "totales", "indice_mas"))
  
  # **********************************************************************************************************
  # PART 6: Resultados ---------------------------------------------------------------------------------------
  # **********************************************************************************************************

  ## 6.1. Exportar base de datos -----------------------------------------------------------------------------
  
  write.xlsx(x = matricula_total, file = paste0(base_de_datos, "/", "Indice_de_masculinidad", ".xlsx"))
  write.xlsx(x = matricula_obs, file = paste0(base_de_datos, "/", "codigo_group_drop", ".xlsx"))
