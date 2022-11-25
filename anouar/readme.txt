25/11 Ajout du module avec le foncteur.
Quelques remarques:
  Que ce soit après définition d'un module ou application du foncteur, si l'on veut agir sans souci sur la catégorie (module), il faut enlever l'abstraction de celle-ci (soit on parle de la categorie et on agit de maniere abstraite sur celle-ci c'est ce que je fais avec le foncteur, soit enfin je la met en application et dans ce cas je veux avoir accés à mes objets (définitions de types,exception etc..) et morphismes (fonctions du module), pour cela on "ouvre" la categorie avec un "#open Cat"  avec Cat le nom du module.
  
  Le switch entre Hashtable et Array n'est pas prioritaire à l'instant.
Des commentaires guides sont dans le code ,mais il ne devrait pas y avoir de difficultés de compréhension de celui-ci.

  On pensera lors de l'integration du code du foncteur à séparer le fichier avec un fichier entete comme indiqué en commentaire dans le code.
  
