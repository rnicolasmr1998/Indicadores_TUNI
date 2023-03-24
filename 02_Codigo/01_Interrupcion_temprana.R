  cat("\014")
  '***********************************************************************************************************
  # BASE DE DATOS:	MATRICULA TUNI
  # PROYECTO: 		  INDICADORES EDUCATIVOS
  # TÍTULO:         TASA DE INTERRUPCIÓN TEMPRANA DE ESTUDIOS
  # AÑOS:				    2020-2022
  # AUTOR: 			    RENZO NICOLAS MARROQUIN RUBIO
  ***********************************************************************************************************'

  # Outline: -------------------------------------------------------------------------------------------------
  
  # **********************************************************************************************************
  # PART 1: Variables de inicio ------------------------------------------------------------------------------
  # **********************************************************************************************************
  
  matricula_general  <- data.table()
  universidades_drop <- data.table()
  ruta_matriculados  <- paste(base_de_datos, "Matriculados", sep = "/")
  
  # **********************************************************************************************************
  # PART 2: Importar base de datos ---------------------------------------------------------------------------
  # **********************************************************************************************************
  
  for (año in 2020:2022) {
  
    matricula_periodo_1 <- fread(paste0(ruta_matriculados, "/", "matriculado_", año, "_I", ".csv"), 
                                 sep = "|", encoding = "Latin-1") %>% setnames(tolower)
    
    matricula_periodo_2 <- fread(paste0(ruta_matriculados, "/", "matriculado_", año, "_II", ".csv"),
                                 sep = "|", encoding = "Latin-1") %>% setnames(tolower)
    
  # **********************************************************************************************************
  # PART 3: Unir base de datos -------------------------------------------------------------------------------
  # **********************************************************************************************************
    
    matricula_periodo   <- rbindlist(list(matricula_periodo_1, matricula_periodo_2), fill = TRUE)
    
  # **********************************************************************************************************
  # PART 4: Renombrar variables ------------------------------------------------------------------------------
  # **********************************************************************************************************
    
    setnames(x   = matricula_periodo,
             old = c("periodo", "nivel_academico", "tipo_constitucion", "sexo"),
             new = c("periodo_original", "matricula_nivel_academico", 
                     "matricula_tipo_constitucion", "matricula_sexo"))
  
  # **********************************************************************************************************
  # PART 5: Generar variables --------------------------------------------------------------------------------
  # **********************************************************************************************************
  
  ## 5.1. Tipo de constitución -------------------------------------------------------------------------------
    
    matricula_periodo[, tipo_constitucion :=
                        factor(fcase(matricula_tipo_constitucion == "Pública", 1,
                                     str_detect(matricula_tipo_constitucion, "Asociativa"), 2,
                                     str_detect(matricula_tipo_constitucion, "Societaria"), 3),
                               labels = c("Pública", "Privada-Asociativa", "Privada-Societaria"))]
    
  ## 5.2. Periodo  -------------------------------------------------------------------------------------------
    
    matricula_periodo[, periodo := factor(periodo_estandarizado)]
  
  # **********************************************************************************************************
  # PART 6: Filtrar datos ------------------------------------------------------------------------------------
  # **********************************************************************************************************
    
    matricula_periodo   <- matricula_periodo[licencia == "Licenciada",]
  
  # **********************************************************************************************************
  # PART 7: Construcción del indicador -----------------------------------------------------------------------
  # **********************************************************************************************************
  
  ## 7.1. Paso 1: Reporte de matricula ambos periodos --------------------------------------------------------
    
    lista_universidades <- matricula_periodo[, matricula := 1]
    lista_universidades <- lista_universidades[,
                                               .(matricula = sum(matricula)),
                                               by = .(codigo_inei, nombre_entidad, periodo)]
    
    lista_universidades <- dcast(lista_universidades,
                                 codigo_inei + nombre_entidad ~ periodo, 
                                 value.var = "matricula", value.name = "matricula")
  {  
  # Universidades que no se usan por año 
    
    drop_universidades  <- lista_universidades[is.na(get(paste0(año, "-1"))) | is.na(get(paste0(año, "-2"))),]
    drop_universidades  <- drop_universidades[, c(paste0(año, "-1"), paste0(año, "-2")) := NULL]
    drop_universidades[, anio := c(año)]
    universidades_drop  <- rbindlist(list(universidades_drop, drop_universidades))
  }
    
    lista_universidades <- lista_universidades[!(is.na(get(paste0(año, "-1"))) | is.na(get(paste0(año, "-2")))),]
    lista_universidades <- lista_universidades[,c(paste0(año, "-1"), paste0(año, "-2")) := NULL]
    
    matricula_periodo   <- matricula_periodo[lista_universidades,
                                             nomatch = 0,
                                             on = intersect(names(matricula_periodo),
                                                            names(lista_universidades))]
  
  ## 7.2. Paso 2: Estudiantes matriculados en el segundo periodo ---------------------------------------------
      
    segundo_semestre  <- matricula_periodo[periodo == paste0(año, "-2"),]
    segundo_semestre  <- unique(segundo_semestre, by = "guid_persona")
    segundo_semestre  <- segundo_semestre[,
                                          c("edad", "periodo_original",
                                            "periodo_estandarizado", "matricula") := NULL]
    
  ## 7.3. Paso 3: Estudiantes semestrales en los primeros 2 años de matricula --------------------------------    
    
    matricula_periodo <- matricula_periodo[periodo == paste0(año, "-1") &
                                             (periodo_lectivo == "Semestral") &
                                             (anio_periodo_ingreso %in% c((año-2), (año-1), (año))),]
    
    
  # Nos quedamos por un registro de estudiante por universidad  
    
    setorder(matricula_periodo, codigo_inei, guid_persona) 
    matricula_periodo <- unique(matricula_periodo,
                                by = c("codigo_inei", "guid_persona"))
  
  # Quedarnos con solo un registro de estudiante en 1 solo tipo de constitucion    
    
    matricula_periodo <- unique(matricula_periodo,
                                by = c("guid_persona", "tipo_constitucion"))
  
  # Quedarnos con solo un registro de estudiante    
    
    setorder(matricula_periodo, guid_persona, tipo_constitucion)
    matricula_periodo <- unique(matricula_periodo,
                                by = "guid_persona")
  
  ## 7.4. Paso 4: Ver si siguen matriculados -----------------------------------------------------------------
    
    guid_persona      <- setDT(anti_join(matricula_periodo, segundo_semestre,
                                         by = "guid_persona")) %>%
      .[, interrupcion := 0]
    
    matricula_periodo <- guid_persona[matricula_periodo,
                                      on = intersect(names(matricula_periodo),
                                                     names(guid_persona))] %>%
      .[, interrupcion :=
          factor(fifelse(test = is.na(interrupcion),
                         yes  = 1,
                         no   = interrupcion),
                 labels = c("Dejo matricula", "Sigue matriculado"))] %>%
      .[, periodo := paste0(año, "-II")]
    
    matricula_general   <- rbindlist(list(matricula_general, matricula_periodo), 
                                     fill = TRUE)
  
  # Limpieza de archivos
    
    rm(list = c("matricula_periodo", "drop_universidades", "lista_universidades", "matricula_periodo_1", 
                "matricula_periodo_2", "guid_persona", "segundo_semestre"))
    }

  # **********************************************************************************************************
  # PART 8: Resultados ---------------------------------------------------------------------------------------
  # **********************************************************************************************************
  
  ## 8.1. Exportar base de datos -----------------------------------------------------------------------------
  
  matricula_resultados <- matricula_general[,
                                            .(matricula = sum(matricula)),
                                            by = .(periodo, tipo_constitucion, interrupcion)]
  
  matricula_resultados <- dcast(matricula_resultados,
                                periodo + tipo_constitucion ~ interrupcion,
                                value.var  = "matricula")
  
  setorder(matricula_resultados, periodo, tipo_constitucion)
  
  matricula_resultados[, totales := `Dejo matricula` + `Sigue matriculado`]
  
  matricula_resultados[, TI := round((`Dejo matricula`/ totales) * 100, 2)]
  
  write.xlsx(x = matricula_resultados, file = paste0(base_de_datos, "/", "Tasa_de_interrupcion", ".xlsx"))
  write.xlsx(x = universidades_drop, file = paste0(alerta, "/", "universidades_drop", ".xlsx"))
  
  ## 8.2. Generar gráfico ------------------------------------------------------------------------------------
  
  matricula_resultados %>% 
    ggplot(aes(x = periodo, y = TI, fill = tipo_constitucion)) +
    geom_bar(stat = "identity", width=0.6) +
    scale_fill_manual(values = c("#D6DCE5", "#8497B0", "#203555"))+
    geom_text(aes(label = paste0(TI, "%")), size = 4, position = position_stack(vjust = 0.5),
              family = 'Times New Roman') +
    geom_text(data = subset(matricula_resultados, tipo_constitucion == "Privada-Societaria"),
              aes(label = paste0(TI, "%")), colour = "white",
              size = 4, position = position_stack(vjust = 0.5),
              family = 'Times New Roman') +
    labs(title    = 'Tasa de Interrupción de estudios universitarios',
         subtitle = "(los dos primeros años)",
         caption  = 'Fuente: TUNI - Elaboración propia',
         fill     = 'Tipo de Constitución',
         x = 'Periodo académico', 
         y = NULL) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    theme(plot.background = element_rect(fill = "white"),
          plot.title    = element_text(size=20, face = "bold"),
          plot.subtitle = element_text(size = 15),
          plot.caption  = element_text(hjust = 0, margin = margin(r = 20)),
          panel.background = element_rect(fill = "white"),
          axis.title  = element_text(size = 10, face = "bold"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(margin = margin(t = -10), colour = "black"),
          axis.line.y = element_blank(),
          axis.ticks  = element_blank(),
          legend.title = element_text(face = "bold", colour = "black"),
          legend.justification = "center",
          text = element_text(color = 'black', family = 'Times New Roman'))
  
  ggsave(paste0(resultados, "/", "interrupcion_temprana.png"), 
         dpi = 1200, width = 8, height = 6, units = "in")
  
