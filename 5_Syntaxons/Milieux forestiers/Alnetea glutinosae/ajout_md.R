# =============================================================================
# SCRIPT R : Ajout de sections aux .md depuis un fichier ODS (UNIQUEMENT ODS)
# Place ce script ET ton fichier "donnees_sections.ods" DANS LE MEME DOSSIER
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(readODS)  # UNIQUEMENT pour les fichiers .ods

# ======================
# ⚙️ CONFIGURATION
# ======================
ods_file_name <- "donnees_sections.ods"  # Nom du fichier ODS (doit être dans le même dossier que ce script)
format_style <- "titre"                  # "titre" (#### En-tête\n\ncontenu) ou "gras" (**En-tête** : contenu)

# ======================
# 📁 DÉTECTION AUTOMATIQUE DU DOSSIER DU SCRIPT (robuste : RStudio / Rscript / source())
# ======================
get_script_dir <- function() {
  # 1. Exécuté depuis RStudio (bouton "Source" ou Ctrl+Shift+S) : chemin du fichier ouvert dans l'éditeur
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) NULL)
    if (!is.null(path) && nzchar(path)) return(dirname(path))
  }
  
  # 2. Exécuté en ligne de commande via "Rscript script.R"
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  
  # 3. Exécuté via source("script.R") depuis une console R
  for (call in rev(sys.calls())) {
    call_fun <- tryCatch(as.character(call[[1]]), error = function(e) "")
    if (identical(call_fun, "source")) {
      src_path <- tryCatch(eval(call[[2]], envir = parent.frame()), error = function(e) NULL)
      if (!is.null(src_path) && nzchar(src_path)) return(dirname(normalizePath(src_path)))
    }
  }
  
  # 4. Repli : répertoire de travail courant
  message("⚠️ Impossible de détecter automatiquement le dossier du script, utilisation du répertoire de travail actuel : ", getwd())
  getwd()
}

script_dir <- get_script_dir()

ods_file <- file.path(script_dir, ods_file_name)  # Chemin complet vers le fichier ODS

# ======================
# 📥 LECTURE DU FICHIER ODS
# ======================
if (!file.exists(ods_file)) {
  stop(
    "❌ FICHIER ODS INTROUVABLE : ", ods_file, "\n",
    "→ Place '", ods_file_name, "' DANS LE MEME DOSSIER que ce script R !"
  )
}

data <- readODS::read_ods(ods_file)  # LECTURE UNIQUEMENT ODS
colnames(data) <- trimws(colnames(data))

if (ncol(data) < 2) {
  stop("❌ Le fichier ODS doit contenir AU MOINS 2 COLONNES (1 pour les noms de fichiers .md, 1+ pour les sections).")
}

md_names <- as.character(data[[1]])  # 1ère colonne = noms des fichiers .md
section_columns <- colnames(data)[-1]  # Autres colonnes = sections à ajouter

# ======================
# 🧹 FONCTIONS UTILITAIRES (version ultra-simple)
# ======================
clean_name <- function(name) {
  if (is.null(name) || is.na(name)) return("")
  name <- tolower(as.character(name))
  name <- gsub("[^a-z0-9._-]", "_", name)       # Remplace les caractères spéciaux
  name <- gsub("_+", "_", name)                # Supprime les _ multiples
  name <- gsub("^_|_$", "", name)               # Supprime les _ en début/fin
  name <- gsub("\\.md$", "", name)              # Supprime .md
  name
}

matches_ods_entry <- function(md_filename, ods_entry) {
  grepl(clean_name(ods_entry), clean_name(md_filename), fixed = TRUE)
}

format_section <- function(header, content, style) {
  if (is.na(content) || content == "") return(character(0))
  content <- trimws(as.character(content))
  if (style == "titre") paste0("#### ", header, "\n\n", content)
  else paste0("**", header, "** : ", content)
}

# ======================
# 🔍 TRAITEMENT DES FICHIERS .MD
# ======================
all_md_files <- list.files(
  path = script_dir,
  pattern = "\\.md$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(all_md_files) == 0) {
  stop("❌ AUCUN FICHIER .md TROUVÉ dans : ", script_dir)
}

cat("🔎 Traitement de", length(all_md_files), "fichiers .md dans :\n", script_dir, "\n")
cat("📌 Format des sections :", format_style, "\n\n")

files_processed <- 0
files_updated <- 0

for (full_path in all_md_files) {
  file_name <- basename(full_path)
  matching_indices <- which(sapply(md_names, function(x) matches_ods_entry(file_name, x)))
  if (length(matching_indices) == 0) next
  
  files_processed <- files_processed + 1
  current_content <- tryCatch(read_lines(full_path), error = function(e) { warning("⚠️ Lecture impossible : ", full_path); next })
  
  new_sections <- character(0)
  for (row_idx in matching_indices) {
    for (col_name in section_columns) {
      content <- data[[col_name]][row_idx]
      if (!is.na(content) && content != "") {
        new_sections <- c(new_sections, format_section(col_name, content, format_style))
      }
    }
    if (length(new_sections) > 0) new_sections <- c(new_sections, "")
  }
  
  if (length(new_sections) > 0 && new_sections[length(new_sections)] == "") {
    new_sections <- new_sections[-length(new_sections)]
  }
  
  if (length(new_sections) > 0) {
    updated_content <- c(current_content, "", new_sections)
    tryCatch(write_lines(updated_content, full_path), error = function(e) { warning("⚠️ Écriture impossible : ", full_path); next })
    files_updated <- files_updated + 1
    cat("✅ Mis à jour :", full_path, "\n")
  }
}

cat("\n🏁 TRAITEMENT TERMINÉ !\n")
cat("📂 Fichiers scannés :", length(all_md_files), "\n")
cat("🔄 Fichiers avec correspondances :", files_processed, "\n")
cat("✨ Fichiers modifiés :", files_updated, "\n")