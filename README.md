# Proyecto EPA — Lectura y consolidación de microdatos

## ¿Qué hace este proyecto?

Este proyecto recoge los ficheros de datos de la **Encuesta de Población Activa (EPA)** publicados por el INE, los unifica en una única tabla y la guarda lista para su análisis.

En la práctica, automatiza un trabajo que antes habría que hacer a mano: abrir decenas de ficheros de distintos años, homogeneizar sus columnas y pegarlos en uno solo.

---

## Estructura de carpetas

```
.
├── data/                        → Todos los datos del proyecto
│   ├── disenos_registro/        → Documentación técnica de los ficheros del INE
│   ├── enlaces/                 → Referencias y vínculos de descarga
│   ├── metadata/                → Información sobre las variables (qué significa cada columna)
│   └── microdatos/              → Ficheros de datos originales del INE
│       ├── csvs_hasta_20/       → Datos desde 2005 hasta 2020
│       ├── csvs_21_23/          → Datos de 2021 a 2023
│       └── csvs_desde_24/       → Datos de 2024 en adelante
│
├── documentos/                  → Informes y documentos de salida
│
└── scr/                         → Código del proyecto
    └── read_data/               → Scripts de lectura y procesamiento
        ├── read_data.R          → Script principal (punto de entrada)
        └── read_data_funproc.R  → Funciones auxiliares de lectura
```

---

## ¿Cómo se ejecuta?

Solo hay que abrir y ejecutar el fichero principal:

```
scr/read_data/read_data.R
```

El script hace todo automáticamente:

1. Lee los ficheros de cada período (hasta 2020, 2021–2023, desde 2024)
2. Ajusta los nombres de columnas para que todo sea coherente entre años
3. Combina todos los datos en una sola tabla
4. Guarda el resultado en dos formatos:
   - `.csv` — formato universal, abrible con Excel
   - `.parquet` — formato compacto y rápido para análisis de grandes volúmenes

Los ficheros de salida se guardan en `data/microdatos/`.

---

## Resultados que genera

| Fichero | Formato | Descripción |
|---|---|---|
| `microdatos_juntos.csv` | CSV | Tabla completa, compatible con Excel |
| `microdatos_juntos.parquet` | Parquet | Tabla completa, optimizada para análisis |

---

## Períodos de datos cubiertos

| Período | Tipo de fichero fuente |
|---|---|
| Hasta 2020 | `.csv` separado por tabulaciones |
| 2021 – 2023 | `.csv` principal + `.tab` de anexo con factor de ponderación |
| Desde 2024 | `.tab` separado por tabulaciones |

> El INE cambió el formato de entrega de los microdatos en varias ocasiones. El código gestiona estas diferencias de forma automática.

---

## Requisitos técnicos

El proyecto está desarrollado en **R**. Las librerías necesarias son:

`dplyr`, `tidyr`, `readr`, `data.table`, `openxlsx`, `magrittr`, `arrow`

> La librería `arrow` es la que permite leer y escribir ficheros `.parquet`.