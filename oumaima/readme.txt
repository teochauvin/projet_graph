DSATUR Backtrack (méthode exacte):
j'ai fait la traduction d'algo suivant :

    DSATUR_B&B(C)
        if LB >= UB -> Return
        if all_is_colored then
            if k < UB then UB = k
        else
            select a non_colored_vertex v 
            for every feasible color i in C plus a new one 
                Ĉ <- C, add v in Visited
                DSATUR_B&B (Ĉ)
            end for
        end if 


LB est initialisé au début de l'algorithme en utilisant la cardinalité de la clique maximale dans graph
UB est initialisé par le nombre des noeuds dans graph
k nbr de couleurs utlisés
