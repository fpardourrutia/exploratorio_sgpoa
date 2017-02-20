library("plyr")
library("dplyr")
library("tidyr")
library("stringi")
library("ggplot2")

# Antes de conectarme con la base de datos, activar el tunel vía:
# tunel_coati="sudo ssh  -f -N -q -L 5433:localhost:5433 snmb@coati"

# conexion_bd <- src_postgres(dbname = "poa2015", host = "localhost", port = "5433",
#                             user = "postgres", password = "000999000")
# nombres_tablas <- src_tbls(conexion_bd)
# 
# glimpse_tablas <- llply(nombres_tablas, function(x, conexion_bd){
#   print(x)
#   tbl(conexion_bd, x) %>%
#     glimpse
#   }, conexion_bd
# )
# 
# tablas <- llply(nombres_tablas, function(x, conexion_bd){
#   tbl(conexion_bd, x) %>%
#   collect
#   }, conexion_bd)
# 
# names(tablas) <- nombres_tablas

# saveRDS(tablas, "insumos/tablas.rds")

tablas <- readRDS(file = "insumos/tablas.rds")

### Después de un análisis, se concluyó que se trabajará con las siguientes
### tablas y variables:

# Catálogo de ANP's
tb_direcciones <- tablas$tb_direcciones %>%
  select(
    id_dir,
    direccion,
    general
  )
    # No nos importan tanto los siguientes campos: Mercedes.
    #general, #si la que captura POA es dirección general o no 
    #complejo, #POA's que capturan más de una ANP
    #activa # si está activa la ANP

# Catálogo de objetivos
tb_objetivo <- tablas$tb_objetivo

# Catálogo de temas asociados a los objetivos
tb_temas <- tablas$tb_temas

# Catálogo de actividades asociadas a los temas
tb_actividades <- tablas$tb_actividades %>%
  select(
    id_act,
    actividad,
    id_tema
  )

# Catálogo de unidades de medida posibles para cada actividad
tb_unidades <- tablas$tb_unidades %>%
  select(
    id_uni,
    unidad,
    id_act,
    dato_part,
    poa_2016
  )

# Tabla de proyectos implementados por ANP, año y tema.
tb_proyecto <- tablas$tb_proyecto %>%
  select(
    id_proy,
    anyo_proy,
    id_tema,
    id_dir,
    obj
  )

# Tabla de actividades planeadas en cada uno de los proyectos anteriores
tb_actividad_poa <- tablas$tb_actividad_poa %>%
  select(
    id_act,
    id_cat_act,
    id_cat_unidad,
    des,
    tipo_meta,
    prog_prim,
    prog_seg,
    prog_ter,
    prog_cua,
    nueva_act,
    nueva_unidad,
    id_proy
  )

# Tabla de seguimiento a las actividades en tb_actividad_poa
tb_act_trim <- tablas$tb_act_trim %>%
  select(
    id_act_trim,
    id_actividad,
    act_1_realizado,
    act_1_programadas,
    act_1_alcanzadas,
    act_1_resultados,
    act_2_realizado,
    act_2_programadas,
    act_2_alcanzadas,
    act_2_resultados,
    act_3_realizado,
    act_3_programadas,
    act_3_alcanzadas,
    act_3_resultados,
    act_4_realizado,
    act_4_programadas,
    act_4_alcanzadas,
    act_4_resultados
  )

## Joins previos al análisis exploratorio

# Catálogo de objetivos, temas y actividades:

Catalogo_general_actividades <- tb_objetivo %>%
  right_join(tb_temas, by = "id_obj") %>%
  right_join(tb_actividades, by = "id_tema")

Catalogo_proyectos_anp_tema <- tb_proyecto %>%
  left_join(
    tb_temas, by = "id_tema"
  ) %>%
  left_join(
    tb_objetivo, by = "id_obj"
  ) %>%
  left_join(
    tb_direcciones, by = "id_dir"
  ) %>%
  select(
    -id_tema,
    -id_dir,
    -id_obj
  )

Actividad_poa <- tb_actividad_poa %>%
  left_join(Catalogo_general_actividades %>%
              select(
                id_act,
                actividad,
                # hay referencias circulares
                tema_cat_actividades = tema,
                objetivo_cat_actividades = objetivo
              ), by = c("id_cat_act" = "id_act")) %>%
  left_join(tb_unidades %>%
              select(
                id_uni,
                unidad,
                dato_part,
                poa_2016
              ), by = c("id_cat_unidad" = "id_uni")) %>%
  left_join(Catalogo_proyectos_anp_tema %>%
      rename(
        tema_cat_proyectos = tema,
        objetivo_cat_proyectos = objetivo
      ), by = "id_proy") %>%
  left_join(tb_act_trim, by = c("id_act" = "id_actividad")) %>%
  transmute(
    id = id_act,
    descripcion = des,
    acumulativa = ifelse(tipo_meta == 1, FALSE, TRUE),
    # revisar qué número corresponde a acumulativa / no acumulativa
    id_actividad = id_cat_act,
    actividad,
    unidad,
    
    realizada_1 = as.logical(act_1_realizado),
    alcanzadas_1 = act_1_alcanzadas,
    meta_1 = prog_prim,
    #meta_1_aux = act_1_programadas,
    
    realizada_2 = as.logical(act_2_realizado),
    alcanzadas_2 = act_2_alcanzadas,
    meta_2 = prog_seg,
    #meta_2_aux = act_2_programadas,
    
    realizada_3 = as.logical(act_3_realizado),
    alcanzadas_3 = act_3_alcanzadas,
    meta_3 = prog_ter,
    #meta_3_aux = act_3_programadas,
    
    realizada_4 = as.logical(act_4_realizado),
    alcanzadas_4 = act_4_alcanzadas,
    meta_4 = prog_cua,
    #meta_4_aux = act_4_programadas,
    
    id_proyecto = id_proy,
    anio = anyo_proy,
    tema = ifelse(!is.na(tema_cat_actividades), tema_cat_actividades, tema_cat_proyectos),
    objetivo = ifelse(!is.na(objetivo_cat_actividades), objetivo_cat_actividades, objetivo_cat_proyectos),
    direccion,
    general
  )

# Revisando que no haya incongruencias entre temas y objetivos obtenidos con ifelse:

unique(Actividad_poa$tema) # Hay NA's
unique(Actividad_poa$objetivo) # Hay NA's

identical(Actividad_poa %>% filter(is.na(tema)),
  Actividad_poa %>% filter(is.na(objetivo)))
# Perfecto: las actividades sin tema son las que no tienen objetivo

Actividad_poa %>%
  group_by(tema) %>%
  tally() %>%
  nrow() == Actividad_poa %>%
  group_by(tema, objetivo) %>%
  tally() %>%
  nrow()
# Perfecto: no hay actividades con temas iguales bajo distintos objetivos

### Análisis exploratorio

## Tabla Catalogo_proyectos_anp_tema
## Variables importantes: tema, ANP, año, número de proyectos

# Grafica de cantidad de proyectos por objetivo por año

Proyectos_objetivo_anio <- Catalogo_proyectos_anp_tema %>%
  group_by(objetivo, anyo_proy) %>%
  tally()

ggplot(data = Proyectos_objetivo_anio, aes(x = reorder(objetivo, n), y = n,
  fill = objetivo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~anyo_proy) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Gráfica de cantidad de proyectos por tema por año.

Proyectos_tema_anio <- Catalogo_proyectos_anp_tema %>%
  group_by(tema, anyo_proy, objetivo) %>%
  tally()

ggplot(data = Proyectos_tema_anio, aes(x = reorder(tema, n), y = n, fill = objetivo)) +
  geom_bar(stat = "identity") +
  facet_wrap(~anyo_proy) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogramas y diagramas de caja de número de proyectos realizados por dirección
# para cada año, dividiendo entre si son de ANP's o de direcciones regionales

Proyectos_anp_anio <- Catalogo_proyectos_anp_tema %>%
  group_by(direccion, anyo_proy) %>%
  summarise(
    n = n(),
    direccion_general = first(general)
  )

ggplot(data = Proyectos_anp_anio, aes(x = as.character(anyo_proy), y = n)) +
  geom_boxplot() +
  facet_wrap(~direccion_general)

# El histograma no se ve muy útil
ggplot(data = Proyectos_anp_anio, aes(x = n)) +
  geom_histogram(bins = 20) +
  facet_wrap(~anyo_proy + direccion_general)

# Diagrama de caja y brazos de número de proyectos realizados por ANP, desglosados
# por objetivo y año

Proyectos_anp_anio_objetivo <- Catalogo_proyectos_anp_tema %>%
  group_by(anyo_proy, objetivo, direccion) %>%
  summarise(
    n = n()
  )

ggplot(data = Proyectos_anp_anio_objetivo, aes(x = objetivo, y = n,
  colour = objetivo)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  facet_wrap(~anyo_proy) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Tabla Actividad_poa

# Actividades y unidades más realizadas en todo el POA: presentarla como una tabla
Actividades_unidades <- Actividad_poa %>%
  # Homologando actividades iguales con distintos id's
  group_by(actividad) %>%
    mutate(
      id_actividad = first(id_actividad)
    ) %>%
  ungroup %>%
  group_by(actividad, unidad) %>%
    summarise(
      # hay actividades iguales con distintos id's
      id_actividad = first(id_actividad),
      tema = first(tema),
      n = n()
    ) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  select(
    id_actividad,
    actividad,
    unidad,
    tema,
    n
  ) %>%
  arrange(tema, desc(n))

Actividades_unidades_presentable <- Actividades_unidades %>%
  select(
    actividad,
    unidad,
    n
  ) %>%
  arrange(desc(n))


# Como se observa en la tabla, muchas unidades son "Otra...", "Número de acciones
# realizadas", etc.. Estas unidades claramente no son comparables (pues no hay una
# medida estándar para, por ejemplo, número de acciones). Ejemplo: 3 tirolesas vs
# 1 complejo ecoturístico, por ello se eliminarán varias combinaciones de actividades
# + unidades

Actividades_unidades_comparables <- Actividades_unidades %>%
  filter(
    !stri_detect_regex(unidad, "acciones|\\.\\.\\.", case_insensitive=TRUE) |
    stri_detect_regex(unidad, "con|proyectos", case_insensitive=TRUE)
  ) %>%
  arrange(desc(n))

# Diagrama de caja y brazos de número de registros por actividad + unidad

ggplot(data = Actividades_unidades_comparables, aes(x = "1", y = n)) +
  geom_boxplot()
# El 75% de las actividades+unidad tiene menos de 30 datos.

# Para complementar esta gráfica, graficar qué porcentaje de las actividades
# realizadas pertenecen a actividades + unidades no comparables.

# Seleccionar actividades correspondientes a temas que nos interesan y hacer el mismo
# diagrama.

temas_prioritarios <- tb_temas %>%
  arrange(id_tema) %>%
  filter(id_tema %in% c(
    5,
    7,
    8,
    9,
    10,
    11,
    15,
    #16, 
    #20,
    22
    #24
  )) %>%
  '$'('tema')
# se comentaron algunos temas porque muy pocas de sus actividades son prioritarias
# (1 ó 2): "Turismo y áreas protegidas", "Desarrollo administrativo",
# "Identidad, comunicación y difusión"


Actividades_temas_prioritarios_unidades_comparables <- Actividades_unidades_comparables %>%
  filter(tema %in% temas_prioritarios)

Actividades_otros_temas_unidades_comparables <- Actividades_unidades_comparables %>%
  filter(
    id_actividad %in% c(93, 108, 63) | #infraestructura y señalética
      stri_detect_regex(actividad,
        "capacitación de productor|uso sustentable|visitante|uso público",
        case_insensitive=TRUE) # aprovechamiento sustentable consuntivo y no consuntivo
  )

Actividades_prioritarias_unidades_comparables <- Actividades_temas_prioritarios_unidades_comparables %>%
  union(Actividades_otros_temas_unidades_comparables) %>% #unión eliminando duplicados
  arrange(tema, desc(n))

# Histograma y diagrama de caja y brazos del número de unidades de medida
# distintas por actividad prioritaria:

Actividades_prioritarias_num_unidades <- Actividades_prioritarias_unidades_comparables %>%
  group_by(actividad) %>%
    summarise(
      num_unidades = n()
    ) %>%
  ungroup()

ggplot(data = Actividades_prioritarias_num_unidades, aes(x = num_unidades))+
  geom_bar() +
  geom_vline(xintercept = mean(Actividades_prioritarias_num_unidades$num_unidades), colour = "red")

ggplot(data = Actividades_prioritarias_num_unidades, aes(x = "1", y = num_unidades))+
  geom_boxplot()

# Gráfica del máximo de registros comparables por actividad
# prioritaria, y el máximo si hubiera un estándar de medida. Se plotea también la
# diferencia promedio.

Actividades_prioritarias_numero_registros_comparables <- Actividades_prioritarias_unidades_comparables %>%
  group_by(actividad) %>%
    summarise(
      id_actividad = first(id_actividad),
      max_registros_comparables = max(n),
      total_registros = sum(n),
      diferencia = total_registros - max_registros_comparables
    ) %>%
  ungroup() %>%
  arrange(diferencia) %>%
  mutate(
    indice = 1:nrow(.)
  ) %>%
  gather(key = "llave", value = "valor", max_registros_comparables, total_registros, diferencia)

ggplot(data = Actividades_prioritarias_numero_registros_comparables, aes(
  x = indice, y = valor, group = llave, colour = llave)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = mean(Actividades_prioritarias_numero_registros_comparables %>%
      filter(llave == "diferencia") %>%
      '$'(valor)))

# Tabla de unidades de actividades prioritarias utilizadas por el mayor número de ANP's.

Actividades_prioritarias_unidades_anps <- Actividad_poa %>%
  # Homologando actividades iguales con distintos id's
  group_by(actividad) %>%
    mutate(
      id_actividad = first(id_actividad)
    ) %>%
  ungroup() %>%
  group_by(actividad, unidad, direccion) %>%
    summarise(
      id_actividad = first(id_actividad),
      tema = first(tema),
      n = n()
  ) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  select(
    id_actividad,
    actividad,
    unidad,
    tema,
    direccion,
    n
  ) %>%
  # Quedándome con las unidades comparables
  filter(
    !stri_detect_regex(unidad, "acciones|\\.\\.\\.", case_insensitive=TRUE) |
      stri_detect_regex(unidad, "con|proyectos", case_insensitive=TRUE)
  ) %>%
  # Quedándome con actividades prioritarias
  filter(tema %in% temas_prioritarios |
           id_actividad %in% c(93, 108, 63) | #infraestructura y señalética
           stri_detect_regex(actividad,
                             "capacitación de productor|uso sustentable|visitante|uso público",
                             case_insensitive=TRUE) # aprovechamiento sustentable consuntivo y no consuntivo)
  ) %>%
  group_by(actividad, unidad) %>%
    summarise(
      id_actividad = first(id_actividad),
      tema = first(tema),
      num_anps = n()
    ) %>%
  ungroup() %>%
  arrange(tema, actividad, desc(num_anps))

Actividades_prioritarias_unidades_anps_presentable <- Actividades_prioritarias_unidades_anps %>%
  ddply(~tema, function(x){
    aux <- x %>%
      arrange(desc(num_anps)) %>%
      head(2)
    #print(aux)
    return(aux)
  })

# Gráfica de número de ANP's que ejecutan una determinada actividad + unidad, para
# actividades prioritarias.

ggplot(Actividades_prioritarias_unidades_anps, aes(x = tema, y = num_anps, colour = tema)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Seleccionando actividades cuyas metas son comparables y graficándolas.
# Las principales actividades + unidades comparables entre ANP's
# las tenemos en la tabla "Actividades_prioritarias_unidades_anps_presentable"
# el campo de acumulativo no es muy confiable, por lo que se omitió, al menos
# en principio.


# Tabla con los valores de actividad + unidad que nos interesan:
actividades_unidades_interes <- Actividades_prioritarias_unidades_anps_presentable %>%
  select(
    actividad,
    unidad
  ) %>%
  mutate(
    id_actividad_unidad = 1:nrow(.)
  )

# Gráfica de metas realizadas y alcanzadas vs tiempo para las actividades + unidades
# de interés, agregada por ANP.
Actividades_prioritarias_unidades_realizadas_anp <- Actividad_poa %>%
  filter(!is.na(anio)) %>%
  inner_join(actividades_unidades_interes, by = c("actividad", "unidad")) %>%
  select(
    -realizada_1,
    -realizada_2,
    -realizada_3,
    -realizada_4
    ) %>%
  gather(llave, valor, alcanzadas_1, meta_1, alcanzadas_2, meta_2, alcanzadas_3,
    meta_3, alcanzadas_4, meta_4, na.rm = TRUE) %>%
  separate(llave, c("tipo","trimestre")) %>%
  transmute(
    id_actividad_unidad = id_actividad_unidad,
    actividad = actividad,
    unidad = unidad,
    trimestre = paste0(anio, "_", trimestre),
    tipo = tipo,
    valor = valor
  ) %>%
  group_by(id_actividad_unidad, actividad, unidad, trimestre, tipo) %>%
    summarise(
      n = sum(valor)
    ) %>%
  ungroup %>%
  arrange(id_actividad_unidad, tipo, trimestre) %>%
  # Calculando frecuencias acumulativas
  group_by(id_actividad_unidad, tipo) %>%
    mutate(
      cum_n = cumsum(n)
    ) %>%
  ungroup()

# graficando el número de unidades alcanzadas por actividad por trimestre.
ggplot(data = Actividades_prioritarias_unidades_realizadas_anp,
  aes(x = trimestre, y = n, group = tipo, colour = tipo)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~id_actividad_unidad, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# y acumulativo:
ggplot(data = Actividades_prioritarias_unidades_realizadas_anp,
  aes(x = trimestre, y = cum_n, group = tipo, colour = tipo)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~id_actividad_unidad, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Gráficas para ANP's seleccionadas:

anps_prioritarias <- tb_direcciones %>%
  filter(
    id_dir %in% c(
      58, #Cuencas de los Ríos Valle de Bravo, Malacatepec, Tilostoc y Temascaltepec
      206, #Corredor Biológico Chichinautzin
      97, #Corredor Biológico Chichinautzin, El Tepozteco y Lagunas de Zempoala
      80, #Nevado de Toluca
      70 #Iztaccíhuatl - Popocatépetl
    )
  ) %>%
  '$'(direccion)

Catalogo_proyectos_anp_prioritaria_temas_prioritarios <- Catalogo_proyectos_anp_tema %>%
  filter(
    direccion %in% anps_prioritarias,
    tema %in% temas_prioritarios
  )

## Tabla de número de proyectos por tema prioritario desglosados cada ANP prioritaria:

Num_proyectos_anio_anp_prioritaria_tema_prioritario <- Catalogo_proyectos_anp_prioritaria_temas_prioritarios %>%
  group_by(direccion, tema, anyo_proy) %>%
  tally() %>%
  ungroup() %>%
  complete(direccion, tema, anyo_proy, fill = list(n = 0)) %>%
  spread(tema, n) %>%
  arrange(anyo_proy, direccion)

## Hacer la tabla de 22 gráficas por ANP, utilizando actividades y ANP's selectas.

# Reportar primero para todas las ANP's y luego para las 4 de interés.