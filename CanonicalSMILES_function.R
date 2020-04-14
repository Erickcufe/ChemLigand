library(jsonlite)
library(rlist)
library(tidyverse)
library(data.table)


Canonical_SMILES <- function(To_idenfier, Archivos_acumulados) { 
  # To_idenfier <- MMP12
  # Archivos_acumulados <- Lista_vacia
    for (i in 1:length(To_idenfier)){
    id <- To_idenfier[i]
    url_chem <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/", id, "/json")
    archivo <- fromJSON(url_chem)
    archivo_sections <- archivo$Record$Section$Section
    if (!is.null(archivo_sections[[2]][["Section"]][[1]][["Information"]][[4]])){
      archivo_smiles <- archivo_sections[[2]][["Section"]][[1]][["Information"]][[4]]
      Archivos_acumulados[[i]] <- archivo_smiles
      print(archivo_smiles)
      print(paste("id",To_idenfier[i],i))
      print("uno")
      
    }  else {
      if (!is.null(archivo_sections[[2]][["Section"]][[2]][["Information"]][[4]])) {
        archivo_smiles <- archivo_sections[[2]][["Section"]][[2]][["Information"]][[4]]
        Archivos_acumulados[[i]] <- archivo_smiles
        print(archivo_smiles)
        print(paste("id",To_idenfier[i],i))
        print("dos")
      } else {
        if (!is.null(archivo_sections[[3]][["Section"]][[2]][["Information"]][[4]])){
          archivo_smiles <- archivo_sections[[3]][["Section"]][[2]][["Information"]][[4]]
          Archivos_acumulados[[i]] <- archivo_smiles
          print(archivo_smiles)
          print(paste("id",To_idenfier[i],i))
          
        } else {
          if (!is.null(archivo_sections[[3]][["Section"]][[1]][["Information"]][[4]])) {
            archivo_smiles <- archivo_sections[[3]][["Section"]][[1]][["Information"]][[4]]
            Archivos_acumulados[[i]] <- archivo_smiles
            print(archivo_smiles)
            print(paste("id",To_idenfier[i],i))
            
          } else {
            if (!is.null(archivo_sections[[4]][["Section"]][[3]][["Information"]][[4]])) {
              archivo_smiles <- archivo_sections[[4]][["Section"]][[3]][["Information"]][[4]]
              Archivos_acumulados[[i]] <- archivo_smiles
              print(archivo_smiles)
              print(paste("id",To_idenfier[i],i))
              
            } else {
              if (!is.null(archivo_sections[[4]][["Section"]][[2]][["Information"]][[4]])) {
                archivo_smiles <- archivo_sections[[3]][["Section"]][[2]][["Information"]][[4]]
                Archivos_acumulados[[i]] <- archivo_smiles
                print(archivo_smiles)
                print(paste("id",To_idenfier[i],i))
                
              } else {
                if (!is.null(archivo_sections[[3]][["Section"]][[4]][["Information"]][[4]])) {
                  archivo_smiles <- archivo_sections[[3]][["Section"]][[4]][["Information"]][[4]]
                  Archivos_acumulados[[i]] <- archivo_smiles
                  print(archivo_smiles)
                  print(paste("id",To_idenfier[i],i))
                  
                } else {
                  if (!is.null(archivo_sections[[2]][["Section"]][[3]][["Information"]][[4]])){
                    archivo_smiles <- archivo_sections[[2]][["Section"]][[3]][["Information"]][[4]]
                    Archivos_acumulados[[i]] <- archivo_smiles
                    print(archivo_smiles)
                    print(paste("id",To_idenfier[i],i))
                    
                  } else {
                    if (!is.null(archivo_sections[[3]][["Section"]][[3]][["Information"]][[4]])) {
                      archivo_smiles <- archivo_sections[[3]][["Section"]][[3]][["Information"]][[4]]
                      Archivos_acumulados[[i]] <- archivo_smiles
                      print(archivo_smiles)
                      print(paste("id",To_idenfier[i],i))
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
    vector_select <- list()
    for (i in 1:length(Archivos_acumulados)){
      if (ncol(data.frame(Archivos_acumulados[i]))!=2){
        print(Archivos_acumulados[i])
        vector_select[i] <- 1
      } else {
        vector_select[i] <- 0
      }
    }
    
    vector_select_1 <- as.logical(unlist(vector_select))
    # print(vector_select_1)
    CID <- To_idenfier[vector_select_1]
    print(CID)
    Pum1 <- Archivos_acumulados[vector_select_1]
    Pum2 <- data.frame(unlist(Pum1))
    SMILES <- data.frame(Pum2[c(F,F,T),])
    print(SMILES)
    colnames(SMILES) <- "SMILES"
    smiles_ids <- data.frame(CID, SMILES)
    return(smiles_ids)
  
}  


# EJEMPLO DE USO

Tested_MMP12 <- read.csv("exports-5.csv")

# O PARTIENDO DE UNA COLUMNA CON EL ID DE LOS LIGANDOS
MMP12 <- Tested_MMP12$cid

# GENERAMOS COMO SEGUNDO ARGUMENTO DE LA FUNCION UNA LISTA VACIA
Lista_vacia <- list()

# PRE CARGAMOS LA FUNCION CANONICAL_SMILES Y LISTO!
MMP12_test <- Canonical_SMILES(MMP12, Lista_vacia)
