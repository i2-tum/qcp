#!/usr/bin/env Rscript

# Load required packages
library(tidyverse)
library(sqldf)
library(hrbrthemes)
library(ggpubr)
library(ggridges)
library(viridis)

# ===== NOTE =====
# Make sure "Times New Roman" is available in postscript font database
# If this is not yet the case, make sure "Times New Roman" is installed on your machine
# and run the following code snippet
# library(extrafont)
# font_import(promt = FALSE)
# ================

# Load a clean theme and set font to "Times New Roman" as used in the lncs template
thm <- theme_ipsum(base_family = "Times New Roman")

# Get the location of the data, i.e. the "results.csv" files from the command line arguments
# and set the working directory to that directory
args = commandArgs(trailingOnly = TRUE)
setwd(dirname(args[1]))
# Load the data
data = read.csv(basename(args[1]))

# Calculate the total number of gates and controls for each gate
summary = sqldf(
  'select file,
    method,
    iif(total <> 0, time, null) as time,
    nullif(total, 0) as total,
    iif(total <> 0, controls, null) as controls
  from (
    select file,
      method,
      time,
      iif(time is not null, ccx + cp + cry + cswap + cu + cu1 + cu3 + cx + cz +
        ecr + h + p + rccx + rx + rxx + ry + ryy + rz + rzz + s + sdg + swap +
        sx + t + tdg + u + u1 + u2 + u3 + x + y + z, null) as total,
      iif(time is not null, 2 * ccx + cp + cry + cswap + cu + cu1 + cu3 + cx +
        cz, null) as controls
    from data)'
)

# Compare gate count reduction of QCP and RPO with Qiskit afterwards each
sqldf(
  'select sum(a.total - b.total) as qcprop1,
    sum(a.total - c.total) as qcprop2,
    sum(a.total - d.total) as qcprop4,
    sum(a.total - e.total) as qcprop8,
    sum(a.total - f.total) as qcprop16,
    sum(a.total - g.total) as qcprop32,
    sum(a.total - h.total) as qcprop64,
    sum(a.total - i.total) as qcprop128,
    sum(a.total - j.total) as qcprop256,
    sum(a.total - k.total) as qcprop512,
    sum(a.total - l.total) as qcprop1024
  from summary a
    join summary b using(file)
    join summary c using(file)
    join summary d using(file)
    join summary e using(file)
    join summary f using(file)
    join summary g using(file)
    join summary h using(file)
    join summary i using(file)
    join summary j using(file)
    join summary k using(file)
    join summary l using(file)
  where a.method = "qcprop0_rpo_qiskit"
    and b.method = "qcprop1_rpo_qiskit"
    and c.method = "qcprop2_rpo_qiskit"
    and d.method = "qcprop4_rpo_qiskit"
    and e.method = "qcprop8_rpo_qiskit"
    and f.method = "qcprop16_rpo_qiskit"
    and g.method = "qcprop32_rpo_qiskit"
    and h.method = "qcprop64_rpo_qiskit"
    and i.method = "qcprop128_rpo_qiskit"
    and j.method = "qcprop256_rpo_qiskit"
    and k.method = "qcprop512_rpo_qiskit"
    and l.method = "qcprop1024_rpo_qiskit"
    and b.total is not null
    and c.total is not null
    and d.total is not null
    and e.total is not null
    and f.total is not null
    and g.total is not null
    and h.total is not null
    and i.total is not null
    and j.total is not null
    and k.total is not null
    and l.total is not null'
) %>%
  pivot_longer(
    c(qcprop1,
      qcprop2,
      qcprop4,
      qcprop8,
      qcprop16,
      qcprop32,
      qcprop64,
      qcprop128,
      qcprop256,
      qcprop512,
      qcprop1024
    ),
    names_to = 'method',
    values_to =  'reduction'
  ) %>%
  ggplot(mapping = aes(x = factor(
    method,
    level = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ),
  y = reduction)) +
  geom_col(alpha = 0.8, fill = viridis(5)[2]) + 
  scale_x_discrete(
    labels = c(
      '1',
      '2',
      '4',
      '8',
      '16',
      '32',
      '64',
      '128',
      '256',
      '512',
      '1024'
    ),
    breaks = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ) +
  thm +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = -45,
      hjust = 0,
      vjust = 0.8
    ),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.x = element_blank(),
  ) +
  xlab('nmax') +
  ylab('gate count reduction')
ggsave(
  filename = "rpo_comp.eps",
  path = "./plots",
  device = cairo_ps,
  width = 6,
  height = 3
)

# Compare control count reduction of QCP and RPO with Qiskit afterwards each
sqldf(
  'select sum(a.controls - b.controls) as qcprop1,
    sum(a.controls - c.controls) as qcprop2,
    sum(a.controls - d.controls) as qcprop4,
    sum(a.controls - e.controls) as qcprop8,
    sum(a.controls - f.controls) as qcprop16,
    sum(a.controls - g.controls) as qcprop32,
    sum(a.controls - h.controls) as qcprop64,
    sum(a.controls - i.controls) as qcprop128,
    sum(a.controls - j.controls) as qcprop256,
    sum(a.controls - k.controls) as qcprop512,
    sum(a.controls - l.controls) as qcprop1024
  from summary a
    join summary b using(file)
    join summary c using(file)
    join summary d using(file)
    join summary e using(file)
    join summary f using(file)
    join summary g using(file)
    join summary h using(file)
    join summary i using(file)
    join summary j using(file)
    join summary k using(file)
    join summary l using(file)
  where a.method = "qcprop0_rpo_qiskit"
    and b.method = "qcprop1_rpo_qiskit"
    and c.method = "qcprop2_rpo_qiskit"
    and d.method = "qcprop4_rpo_qiskit"
    and e.method = "qcprop8_rpo_qiskit"
    and f.method = "qcprop16_rpo_qiskit"
    and g.method = "qcprop32_rpo_qiskit"
    and h.method = "qcprop64_rpo_qiskit"
    and i.method = "qcprop128_rpo_qiskit"
    and j.method = "qcprop256_rpo_qiskit"
    and k.method = "qcprop512_rpo_qiskit"
    and l.method = "qcprop1024_rpo_qiskit"
    and b.total is not null
    and c.total is not null
    and d.total is not null
    and e.total is not null
    and f.total is not null
    and g.total is not null
    and h.total is not null
    and i.total is not null
    and j.total is not null
    and k.total is not null
    and l.total is not null'
) %>%
  pivot_longer(
    c(qcprop1,
      qcprop2,
      qcprop4,
      qcprop8,
      qcprop16,
      qcprop32,
      qcprop64,
      qcprop128,
      qcprop256,
      qcprop512,
      qcprop1024
    ),
    names_to = 'method',
    values_to =  'reduction'
  ) %>%
  ggplot(mapping = aes(x = factor(
    method,
    level = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ),
  y = reduction)) +
  geom_col(alpha = 0.8, fill = viridis(5)[2]) +
  scale_x_discrete(
    labels = c(
      '1',
      '2',
      '4',
      '8',
      '16',
      '32',
      '64',
      '128',
      '256',
      '512',
      '1024'
    ),
    breaks = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ) +
  thm +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = -45,
      hjust = 0,
      vjust = 0.8
    ),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.x = element_blank(),
  ) +
  xlab('nmax') +
  ylab('control count reduction')
ggsave(
  filename = "ctrl_rpo_comp.eps",
  path = "./plots",
  device = cairo_ps,
  width = 6,
  height = 3
)

sqldf(
  'select sum(a.total - b.total) as qcprop1,
    sum(a.total - c.total) as qcprop2,
    sum(a.total - d.total) as qcprop4,
    sum(a.total - e.total) as qcprop8,
    sum(a.total - f.total) as qcprop16,
    sum(a.total - g.total) as qcprop32,
    sum(a.total - h.total) as qcprop64,
    sum(a.total - i.total) as qcprop128,
    sum(a.total - j.total) as qcprop256,
    sum(a.total - k.total) as qcprop512,
    sum(a.total - l.total) as qcprop1024
  from summary a
    join summary b using(file)
    join summary c using(file)
    join summary d using(file)
    join summary e using(file)
    join summary f using(file)
    join summary g using(file)
    join summary h using(file)
    join summary i using(file)
    join summary j using(file)
    join summary k using(file)
    join summary l using(file)
  where a.method = "qcprop0_qiskit"
    and b.method = "qcprop1_qiskit"
    and c.method = "qcprop2_qiskit"
    and d.method = "qcprop4_qiskit"
    and e.method = "qcprop8_qiskit"
    and f.method = "qcprop16_qiskit"
    and g.method = "qcprop32_qiskit"
    and h.method = "qcprop64_qiskit"
    and i.method = "qcprop128_qiskit"
    and j.method = "qcprop256_qiskit"
    and k.method = "qcprop512_qiskit"
    and l.method = "qcprop1024_qiskit"
    and b.total is not null
    and c.total is not null
    and d.total is not null
    and e.total is not null
    and f.total is not null
    and g.total is not null
    and h.total is not null
    and i.total is not null
    and j.total is not null
    and k.total is not null
    and l.total is not null'
)

# Compare gate count reduction of RPO and QCP with Qiskit afterwards each
sqldf(
  'select sum(a.total - b.total) as qcprop1,
    sum(a.total - c.total) as qcprop2,
    sum(a.total - d.total) as qcprop4,
    sum(a.total - e.total) as qcprop8,
    sum(a.total - f.total) as qcprop16,
    sum(a.total - g.total) as qcprop32,
    sum(a.total - h.total) as qcprop64,
    sum(a.total - i.total) as qcprop128,
    sum(a.total - j.total) as qcprop256,
    sum(a.total - k.total) as qcprop512,
    sum(a.total - l.total) as qcprop1024
  from summary a
    join summary b using(file)
    join summary c using(file)
    join summary d using(file)
    join summary e using(file)
    join summary f using(file)
    join summary g using(file)
    join summary h using(file)
    join summary i using(file)
    join summary j using(file)
    join summary k using(file)
    join summary l using(file)
  where a.method = "qcprop0_rpo_qiskit"
    and b.method = "rpo_qcprop1_qiskit"
    and c.method = "rpo_qcprop2_qiskit"
    and d.method = "rpo_qcprop4_qiskit"
    and e.method = "rpo_qcprop8_qiskit"
    and f.method = "rpo_qcprop16_qiskit"
    and g.method = "rpo_qcprop32_qiskit"
    and h.method = "rpo_qcprop64_qiskit"
    and i.method = "rpo_qcprop128_qiskit"
    and j.method = "rpo_qcprop256_qiskit"
    and k.method = "rpo_qcprop512_qiskit"
    and l.method = "rpo_qcprop1024_qiskit"
    and b.total is not null
    and c.total is not null
    and d.total is not null
    and e.total is not null
    and f.total is not null
    and g.total is not null
    and h.total is not null
    and i.total is not null
    and j.total is not null
    and k.total is not null
    and l.total is not null'
) %>%
  pivot_longer(
    c(qcprop1,
      qcprop2,
      qcprop4,
      qcprop8,
      qcprop16,
      qcprop32,
      qcprop64,
      qcprop128,
      qcprop256,
      qcprop512,
      qcprop1024
    ),
    names_to = 'method',
    values_to =  'reduction'
  ) %>%
  ggplot(mapping = aes(x = factor(
    method,
    level = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ),
  y = reduction)) +
  geom_col(alpha = 0.8, fill = viridis(5)[4]) +
  scale_x_discrete(
    labels = c(
      '1',
      '2',
      '4',
      '8',
      '16',
      '32',
      '64',
      '128',
      '256',
      '512',
      '1024'
    ),
    breaks = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ) +
  thm +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = -45,
      hjust = 0,
      vjust = 0.8
    ),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.x = element_blank(),
  ) +
  xlab('nmax') +
  ylab('gate count reduction')
ggsave(
  filename = "rpo_qcp_comp.eps",
  path = "./plots",
  device = cairo_ps,
  width = 6,
  height = 3
)

# Compare control count reduction of RPO and QCP with Qiskit afterwards each
sqldf(
  'select sum(a.controls - b.controls) as qcprop1,
    sum(a.controls - c.controls) as qcprop2,
    sum(a.controls - d.controls) as qcprop4,
    sum(a.controls - e.controls) as qcprop8,
    sum(a.controls - f.controls) as qcprop16,
    sum(a.controls - g.controls) as qcprop32,
    sum(a.controls - h.controls) as qcprop64,
    sum(a.controls - i.controls) as qcprop128,
    sum(a.controls - j.controls) as qcprop256,
    sum(a.controls - k.controls) as qcprop512,
    sum(a.controls - l.controls) as qcprop1024
  from summary a
    join summary b using(file)
    join summary c using(file)
    join summary d using(file)
    join summary e using(file)
    join summary f using(file)
    join summary g using(file)
    join summary h using(file)
    join summary i using(file)
    join summary j using(file)
    join summary k using(file)
    join summary l using(file)
  where a.method = "qcprop0_rpo_qiskit"
    and b.method = "rpo_qcprop1_qiskit"
    and c.method = "rpo_qcprop2_qiskit"
    and d.method = "rpo_qcprop4_qiskit"
    and e.method = "rpo_qcprop8_qiskit"
    and f.method = "rpo_qcprop16_qiskit"
    and g.method = "rpo_qcprop32_qiskit"
    and h.method = "rpo_qcprop64_qiskit"
    and i.method = "rpo_qcprop128_qiskit"
    and j.method = "rpo_qcprop256_qiskit"
    and k.method = "rpo_qcprop512_qiskit"
    and l.method = "rpo_qcprop1024_qiskit"
    and b.total is not null
    and c.total is not null
    and d.total is not null
    and e.total is not null
    and f.total is not null
    and g.total is not null
    and h.total is not null
    and i.total is not null
    and j.total is not null
    and k.total is not null
    and l.total is not null'
) %>%
  pivot_longer(
    c(qcprop1,
      qcprop2,
      qcprop4,
      qcprop8,
      qcprop16,
      qcprop32,
      qcprop64,
      qcprop128,
      qcprop256,
      qcprop512,
      qcprop1024
    ),
    names_to = 'method',
    values_to =  'reduction'
  ) %>%
  ggplot(mapping = aes(x = factor(
    method,
    level = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ),
  y = reduction)) +
  geom_col(alpha = 0.8, fill = viridis(5)[4]) +
  scale_x_discrete(
    labels = c(
      '1',
      '2',
      '4',
      '8',
      '16',
      '32',
      '64',
      '128',
      '256',
      '512',
      '1024'
    ),
    breaks = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ) +
  thm +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = -45,
      hjust = 0,
      vjust = 0.8
    ),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.x = element_blank(),
  ) +
  xlab('nmax') +
  ylab('control count reduction')
ggsave(
  filename = "ctrl_rpo_qcp_comp.eps",
  path = "./plots",
  device = cairo_ps,
  width = 6,
  height = 3
)

# Compare control count reduction of different optimizers after QCP
sqldf(
  'select substr(a.method, 9, length(a.method) - 8) as opt,
    sum(a.controls - b.controls) as qcprop1,
    sum(a.controls - c.controls) as qcprop2,
    sum(a.controls - d.controls) as qcprop4,
    sum(a.controls - e.controls) as qcprop8,
    sum(a.controls - f.controls) as qcprop16,
    sum(a.controls - g.controls) as qcprop32,
    sum(a.controls - h.controls) as qcprop64,
    sum(a.controls - i.controls) as qcprop128,
    sum(a.controls - j.controls) as qcprop256,
    sum(a.controls - k.controls) as qcprop512,
    sum(a.controls - l.controls) as qcprop1024
  from summary a
    join summary b using(file)
    join summary c using(file)
    join summary d using(file)
    join summary e using(file)
    join summary f using(file)
    join summary g using(file)
    join summary h using(file)
    join summary i using(file)
    join summary j using(file)
    join summary k using(file)
    join summary l using(file)
  where opt in ("pyzx", "qiskit", "tket")
    and a.method like "qcprop0\\_%" escape "\\"
    and b.method like "qcprop1\\_%" escape "\\"
    and c.method like "qcprop2\\_%" escape "\\"
    and d.method like "qcprop4\\_%" escape "\\"
    and e.method like "qcprop8\\_%" escape "\\"
    and f.method like "qcprop16\\_%" escape "\\"
    and g.method like "qcprop32\\_%" escape "\\"
    and h.method like "qcprop64\\_%" escape "\\"
    and i.method like "qcprop128\\_%" escape "\\"
    and j.method like "qcprop256\\_%" escape "\\"
    and k.method like "qcprop512\\_%" escape "\\"
    and l.method like "qcprop1024\\_%" escape "\\"
    and a.method not like "%rpo%"
    and b.method not like "%rpo%"
    and c.method not like "%rpo%"
    and d.method not like "%rpo%"
    and e.method not like "%rpo%"
    and f.method not like "%rpo%"
    and g.method not like "%rpo%"
    and h.method not like "%rpo%"
    and i.method not like "%rpo%"
    and j.method not like "%rpo%"
    and k.method not like "%rpo%"
    and l.method not like "%rpo%"
    and charindex(substr(a.method, 9, 4), b.method, 0) > 0
    and charindex(substr(a.method, 9, 4), c.method, 0) > 0
    and charindex(substr(a.method, 9, 4), d.method, 0) > 0
    and charindex(substr(a.method, 9, 4), e.method, 0) > 0
    and charindex(substr(a.method, 9, 4), f.method, 0) > 0
    and charindex(substr(a.method, 9, 4), g.method, 0) > 0
    and charindex(substr(a.method, 9, 4), h.method, 0) > 0
    and charindex(substr(a.method, 9, 4), i.method, 0) > 0
    and charindex(substr(a.method, 9, 4), j.method, 0) > 0
    and charindex(substr(a.method, 9, 4), k.method, 0) > 0
    and charindex(substr(a.method, 9, 4), l.method, 0) > 0
  group by opt'
) %>%
  pivot_longer(
    c(
      qcprop1,
      qcprop2,
      qcprop4,
      qcprop8,
      qcprop16,
      qcprop32,
      qcprop64,
      qcprop128,
      qcprop256,
      qcprop512,
      qcprop1024
    ),
    names_to = 'method',
    values_to =  'reduction'
  ) %>%
  ggplot(mapping = aes(
    x = factor(
      method,
      level = c(
        'qcprop1',
        'qcprop2',
        'qcprop4',
        'qcprop8',
        'qcprop16',
        'qcprop32',
        'qcprop64',
        'qcprop128',
        'qcprop256',
        'qcprop512',
        'qcprop1024'
      )
    ),
    y = reduction,
    fill = opt
  )) +
  geom_col(alpha = 0.8) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(
    labels = c('1', '2', '4', '8', '16', '32', '64', '128', '256', '512', '1024'),
    breaks = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ) +
  thm +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = -45,
      hjust = 0,
      vjust = 0.8
    ),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.x = element_blank(),
  ) +
  facet_wrap(vars(opt), nrow = 1, scales = 'free_y') +
  xlab('nmax') +
  ylab('control count reduction')
ggsave(
  filename = "ctrl_reduction.eps",
  path = "./plots",
  device = cairo_ps,
  width = 12.25,
  height = 3
)

# Compare gate count reduction of different optimizers after QCP
sqldf(
  'select substr(a.method, 9, length(a.method) - 8) as opt,
    sum(a.total - b.total) as qcprop1,
    sum(a.total - c.total) as qcprop2,
    sum(a.total - d.total) as qcprop4,
    sum(a.total - e.total) as qcprop8,
    sum(a.total - f.total) as qcprop16,
    sum(a.total - g.total) as qcprop32,
    sum(a.total - h.total) as qcprop64,
    sum(a.total - i.total) as qcprop128,
    sum(a.total - j.total) as qcprop256,
    sum(a.total - k.total) as qcprop512,
    sum(a.total - l.total) as qcprop1024
  from summary a
    join summary b using(file)
    join summary c using(file)
    join summary d using(file)
    join summary e using(file)
    join summary f using(file)
    join summary g using(file)
    join summary h using(file)
    join summary i using(file)
    join summary j using(file)
    join summary k using(file)
    join summary l using(file)
  where a.method like "qcprop0\\_%" escape "\\"
    and b.method like "qcprop1\\_%" escape "\\"
    and c.method like "qcprop2\\_%" escape "\\"
    and d.method like "qcprop4\\_%" escape "\\"
    and e.method like "qcprop8\\_%" escape "\\"
    and f.method like "qcprop16\\_%" escape "\\"
    and g.method like "qcprop32\\_%" escape "\\"
    and h.method like "qcprop64\\_%" escape "\\"
    and i.method like "qcprop128\\_%" escape "\\"
    and j.method like "qcprop256\\_%" escape "\\"
    and k.method like "qcprop512\\_%" escape "\\"
    and l.method like "qcprop1024\\_%" escape "\\"
    and a.method not like "%rpo%"
    and b.method not like "%rpo%"
    and c.method not like "%rpo%"
    and d.method not like "%rpo%"
    and e.method not like "%rpo%"
    and f.method not like "%rpo%"
    and g.method not like "%rpo%"
    and h.method not like "%rpo%"
    and i.method not like "%rpo%"
    and j.method not like "%rpo%"
    and k.method not like "%rpo%"
    and l.method not like "%rpo%"
    and charindex(substr(a.method, 9, 4), b.method, 0) > 0
    and charindex(substr(a.method, 9, 4), c.method, 0) > 0
    and charindex(substr(a.method, 9, 4), d.method, 0) > 0
    and charindex(substr(a.method, 9, 4), e.method, 0) > 0
    and charindex(substr(a.method, 9, 4), f.method, 0) > 0
    and charindex(substr(a.method, 9, 4), g.method, 0) > 0
    and charindex(substr(a.method, 9, 4), h.method, 0) > 0
    and charindex(substr(a.method, 9, 4), i.method, 0) > 0
    and charindex(substr(a.method, 9, 4), j.method, 0) > 0
    and charindex(substr(a.method, 9, 4), k.method, 0) > 0
    and charindex(substr(a.method, 9, 4), l.method, 0) > 0
  group by opt'
) %>%
  pivot_longer(
    c(
      qcprop1,
      qcprop2,
      qcprop4,
      qcprop8,
      qcprop16,
      qcprop32,
      qcprop64,
      qcprop128,
      qcprop256,
      qcprop512,
      qcprop1024
    ),
    names_to = 'method',
    values_to =  'reduction'
  ) %>%
  ggplot(mapping = aes(
    y = factor(
      method,
      level = c(
        'qcprop1',
        'qcprop2',
        'qcprop4',
        'qcprop8',
        'qcprop16',
        'qcprop32',
        'qcprop64',
        'qcprop128',
        'qcprop256',
        'qcprop512',
        'qcprop1024'
      )
    ),
    x = reduction,
    fill = opt
  )) +
  geom_col(alpha = 0.8) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_discrete(
    labels = c('1', '2', '4', '8', '16', '32', '64', '128', '256', '512', '1024'),
    breaks = c(
      'qcprop1',
      'qcprop2',
      'qcprop4',
      'qcprop8',
      'qcprop16',
      'qcprop32',
      'qcprop64',
      'qcprop128',
      'qcprop256',
      'qcprop512',
      'qcprop1024'
    )
  ) +
  thm +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = -45,
      hjust = 0,
      vjust = 0.8
    ),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.x = element_blank()
  ) +
  coord_flip() +
  facet_wrap(vars(opt), nrow = 1, scales = 'free_y') +
  ylab('nmax') +
  xlab('gate count reduction')
ggsave(
  filename = "gate_reduction.eps",
  path = "./plots",
  device = cairo_ps,
  width = 12.25,
  height = 3
)

# Show histogram over the improvement of single circuits with respect to gate count
sqldf(
  'select a.file as file,
    substr(a.method, 9, length(a.method) - 8) as opt,
    cast(a.total - b.total as float) / a.total as qcprop1024
  from summary a
    join summary b using(file)
  where a.method like "qcprop0\\_%" escape "\\"
    and b.method like "qcprop1024\\_%" escape "\\"
    and a.method not like "%rpo%"
    and b.method not like "%rpo%"
    and charindex(substr(a.method, 9, 4), b.method, 0) > 0
    and qcprop1024 is not null
    and qcprop1024 <> 0'
) %>%
  ggplot(mapping = aes(x = qcprop1024,
                       fill = opt)) +
  geom_histogram(alpha = 0.8,
                 binwidth = .02) +
  scale_fill_viridis(discrete = TRUE) +
  thm +
  theme(legend.position = "none",
        panel.spacing = unit(0.5, "lines")) +
  xlab("Relative gate count reduction (%)") +
  ylab("Changed circuits") +
  coord_flip() +
  facet_wrap(vars(opt), nrow = 1, scales = 'free_x')
ggsave(
  filename = "reduction_hist.eps",
  path = "./plots",
  device = cairo_ps,
  width = 12.25,
  height = 4
)

# Show histogram over the improvement of single circuits with respect to control count
sqldf(
  'select a.file as file,
    substr(a.method, 9, length(a.method) - 8) as opt,
    cast(a.controls - b.controls as float) / a.controls as qcprop1024
  from summary a
    join summary b using(file)
  where a.method like "qcprop0\\_%" escape "\\"
    and b.method like "qcprop1024\\_%" escape "\\"
    and a.method not like "%rpo%"
    and b.method not like "%rpo%"
    and charindex(substr(a.method, 9, 4), b.method, 0) > 0
    and qcprop1024 is not null
    and qcprop1024 <> 0'
) %>%
  ggplot(mapping = aes(x = qcprop1024,
                       fill = opt)) +
  geom_histogram(alpha = 0.8,
                 binwidth = .02) +
  scale_fill_viridis(discrete = TRUE) +
  thm +
  theme(legend.position = "none",
        panel.spacing = unit(0.5, "lines")) +
  xlab("Relative control count reduction (%)") +
  ylab("Changed circuits") +
  coord_flip() +
  facet_wrap(vars(opt), nrow = 1, scales = 'free_x')
ggsave(
  filename = "ctrl_reduction_hist.eps",
  path = "./plots",
  device = cairo_ps,
  width = 12.25,
  height = 4
)

# Show distribution of changed/unchanged/failed circuits over all optimizers
sqldf(
  'select a.opt as opt,
      failed,
      unchanged,
      changed
    from (
        select substr(a.method, 9, length(a.method) - 8) as opt,
          count(*) as failed
        from summary a 
          join summary b using(file)
        where a.method like "qcprop0\\_%" escape "\\"
          and b.method like "qcprop1024\\_%" escape "\\"
          and a.method not like "%rpo%"
          and b.method not like "%rpo%"
          and charindex(substr(a.method, 9, 4), b.method, 0) > 0
          and a.controls - b.controls is null
        group by opt) a
      full outer join (
        select substr(a.method, 9, length(a.method) - 8) as opt,
          count(*) as unchanged
        from summary a 
          join summary b using(file)
        where a.method like "qcprop0\\_%" escape "\\"
          and b.method like "qcprop1024\\_%" escape "\\"
          and a.method not like "%rpo%"
          and b.method not like "%rpo%"
          and charindex(substr(a.method, 9, 4), b.method, 0) > 0
          and a.controls - b.controls = 0
        group by opt) b
      on a.opt = b.opt full outer join (
        select substr(a.method, 9, length(a.method) - 8) as opt,
          count(*) as changed
        from summary a 
          join summary b using(file)
        where a.method like "qcprop0\\_%" escape "\\"
          and b.method like "qcprop1024\\_%" escape "\\"
          and a.method not like "%rpo%"
          and b.method not like "%rpo%"
          and charindex(substr(a.method, 9, 4), b.method, 0) > 0
          and a.controls - b.controls is not null
          and a.controls - b.controls <> 0
        group by opt) c
      on a.opt = c.opt'
) %>%
  pivot_longer(c(failed, unchanged, changed),
               names_to = 'status',
               values_to = 'count') %>%
  ggplot(mapping = aes(x = status, y = count, fill = opt)) +
  geom_col(alpha = 0.8, width = 1) +
  scale_fill_viridis(discrete = TRUE) +
  thm +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.y = element_blank()
  ) +
  coord_flip() +
  ylab('circuits') +
  facet_wrap(vars(opt), nrow = 1)
ggsave(
  filename = "status.eps",
  path = "./plots",
  device = cairo_ps,
  width = 12.25,
  height = 2
)

# Show running time of QCP for qft against number of qubits
sqldf(
  'select cast(substr(method, 7, 4) as int) as nmax,
    total,
    cast(substr(file, charindex("qiskit_", file, 0) + 7, length(file) - charindex("qiskit_", file, 0) - 11) as int) as qubits,
    time as time,
    substr(file, 0, 10) as alg
  from summary
  where method like "qcprop%"
    and method not like "qcprop%\\_%" escape "\\"
    and method <> "qcprop0"
    and time is not null
    and file like "qft\\_%" escape "\\"
    and nmax >= 16'
) %>%
  ggplot(mapping = aes(x = qubits, y = time)) +
  geom_point(alpha = .6, col = viridis(6)[2]) +
  thm +
  ylab('running time [s]') +
  facet_wrap(vars(nmax), dir = 'h', scales = 'free_y')
ggsave(
  filename = "qft_running_time.eps",
  path = "./plots",
  device = cairo_ps,
  width = 12.25,
  height = 8
)


