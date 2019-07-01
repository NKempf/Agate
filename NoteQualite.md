---
title: "Note Qualite"
author: "Baptiste Raimbaud"
date: "1 juillet 2019"
output: html_document
---



## Qualité des estimateur dans agate

La méthode d’estimation se base sur l’estimation d’un total dans le RP sur une communes entière par Lionnel Delta.

# Plan de Sondage :
Probabilité = 1 pour les adresses situées dans les petites communes. Et quelques adresses de grande communes.

Probabilité = 2,5 pour les adresses de grandes communes.

Les poids sont calés en utilisant le nombre de logements (Variable appelé X )
	
# Partie I : Estimation Simple d’un Total et Variance Associer.
L’estimation du Total TY ce fait par l’intermédiaire de  :
${\hat{T}}_Y = \sum  y_i  \times  IPOND$

## Including Plots

You can also embed plots, for example:

![plot of chunk pressure](figure/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
