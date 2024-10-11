# fonctions annexes pour intorroger Hubeau

#Ces fonctions servent à pouvoir "mapper" et optimiser l'appel à l'API
#exemple d'utilisation' pour la 1ere fonction (les autres dans le meme esprit)
# departement=c('22','29')
# Multi_indice_minv <- map_df(departement,f_get_minv_departement)


# recuperer métriques i2m2 par département
f_get_minv_departement <- function(codedep) {
  get_hydrobio_indices(code_departement=codedep,code_support=13)
}

#récupérer métriques i2m2 par station
f_get_minv_station <- function(cdstation){
  get_hydrobio_indices(code_station_hydrobio=cdstation,code_support=13)
}

# recuperer métriques ibd par département
f_get_ibd_departement <- function(codedep) {
  # si on passe par code support (=10) on obtient ibd et ips
  #ips non utile 
  get_hydrobio_indices(code_departement=codedep,code_support=10)
}

# recuperer liste taxonomique macro invertébrés par station
#impossible de faire une requete par département, on est limités
# par l'API
# nécessite d'avoir une liste de stations
# ou on peut aussi récupérer les stations par hubeau
f_get_liste_taxo_minv <- function(cdstation){ 
      get_hydrobio_taxons(code_station_hydrobio=cdstation,code_support=13) 
     }