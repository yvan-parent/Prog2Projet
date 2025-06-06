
# Projet de Prog2

*LDD Magistère - Parent Yvan*

Le sujet portait sur l'implémentation d'un borrow checker sur un mini langage dérivé de Rust.

J'ai pu réaliser dans ce projet que très peu de tâches, notamment les tâches 1 et 2 en entières, et j'ai pu travailler sur la tâche 3 ensuite pendant la majorité du temps, mais sans succès en raisons des différentes explications ci-dessous:

Le sujet, bien que compréhensible dans l'idée globale, a été pour moi extrêment difficile à travailler dessus.

En effet, le principe même d'un borrow checker a été compris, il est question de repérer  à la compilation directement si les variables empreintées sont censées ou non pouvoir être utilisées, et move dans d'autres variables. 

Après avoir relus et relus les tâches à réaliser, j'ai également pu comprendre le but et l'intérêt unique de chaque tâche. Cependant, le fait que la structure de donnée n'était pas libre, et n'était pas créée par moi même, a rendu son utilisation bien trop complexe à mes yeux. J'ai essayé de comprendre le fonctionnement global à de mainte reprises, mais sans succès. Je comprend l'intérêt de passer par des zones de mémoire (les places), à la place des types de bases de minirust, mais son implémentation était pour moi très compliquée à comprendre. Notamment, lorsque je me mettais sur une tâche, et lorsque j'avais finalement compris son but et que j'avais imaginé une façon de procédé, je ne pouvais tout simplement pas la réaliser simplement étant donné qu'il fallait d'abord comprendre la façon dont le sujet voulait qu'on la code, et trouver dans les fichiers .ml déjà fournis les fonctions adéquates en devant d'abord les comprendres, puis voir si elles correspondent à notre cas. Le nombre énorme de fichiers n'ont également pas rendu ce travail plus simple car, même si je ne doute pas qu'ils sont bien organisés, c'était juste beaucoup trop compliqué de comprendre où trouver les informations dont j'avais besoin. J'ai également trouvé très brouillon l'utilisation de la bibliothèque Fix, trouvant cela comme une "formule magique" sans pouvoir le comprendre.

J'aurai de très loin préféré avoir à coder tout cela de zero, et de pouvoir avoir l'occasion d'implémenter le borrow checker de la façon dont je voulais. C'est d'ailleurs dommage que je n'ai pas eu l'occasion de réaliser des extensions puisque ces dernières semblaient permettre un codage plus libre et plus intéressant (c'est l'impression que j'ai eu en parlant avec certains étudiants qui ont réussi à implémenter des extensions).

Je tiens néanmoins à préciser le fait que le thème du sujet est très intéressant, et que je l'apprécie beaucoup mais que je n'ai probablement pas passé assez de temps dessus (même si j'ai passé de nombreuses et de nombreuses heures au fil des 2 ou 3 dernières semaines à essayer de comprendre à de multiple reprises le code et les tâches demandées).
Je suis également conscient que certaines séances de tp étaient réservées au projet, ou du moins étaient des heures où l'on pouvait poser des questions, mais cela s'averait être tombé durant la période d'examens du magistère (semaine durant laquelle je n'ai pas pu assister à ce cours, chose dont je suis pleinement responsable).

CEPENDANT, l'organisation même du magistère n'a pas du TOUT aidé sur la réalisation de ce projet : 
Un peu avant le début du projet, nous étions en pleine période de rendu de plusieurs projets notés qui nous demandait énormément de temps afin d'être réalisés. De plus, nous sommes rapidement rentrés en période d'examens et de tp notés chaque semaine, rendant le temps disponible sur prog2 très limité.
Lorsque nous avions terminé les examens, mon stage de fin d'année commençait également en parallèle, et j'ai dû m'adapter et travailler énormément chaque jour, avec beaucoup de temps de transport journalier pour m'y rendre.

J'ai bien conscience que ces raisons n'excusent en rien le rendu catastrophique de ce projet que je vous propose, mais j'espère qu'il vous permettra de comprendre les raisons qui m'ont conduit à rendre un projet que très peu developpé, malgré que le thème du projet me motivait.
