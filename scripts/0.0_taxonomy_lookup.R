# =============================================================================
# TAXONOMY LOOKUP — Universal species reference table for mosquito surveillance
# =============================================================================
# Author    : Eleonora Longo
#             PhD Student in Agrifood and Environmental Science
#             Center of Agriculture, Food and Environment (C3A)
#             University of Trento
#             Via Edmund Mach, 1 - 38098 San Michele all'Adige (TN)
#             eleonora.longo@unitn.it
#
# Co-Authors: Blaha M., et al.
#
# Purpose   : Provides a single, shared taxonomy_lookup data frame mapping all
#             region-specific raw species name variants (uppercase, mixed case,
#             abbreviated, legacy genus names) to harmonised canonical names and
#             full taxonomic classification (kingdom → species).
#             Source this file at the top of each regional preprocessing script
#             to replace locally defined taxonomy_lookup blocks.
#
# Usage     : source("taxonomy_lookup.R")   # adjust path if needed
#
# Coverage  : All regional scripts
#
# Notes     : - Duplicate species_raw rows are intentional: different regions
#               encode the same species differently (e.g. "CULEX PIPIENS" vs
#               "Culex pipiens"). All variants map to the same Canonical_name.
#             - Veneto/Friuli joins on column "SPECIES" instead of "species_raw";
#               keep by = "SPECIES" in that script, or rename before joining.
#             - To add a new species variant, append a row and update the
#               parallel vectors (Canonical_name, genus, species, etc.).
# =============================================================================

taxonomy_lookup <- data.frame(
  species_raw = c(
    # ── AEDES ──────────────────────────────────────────────────────────────────
    "AEDES ALBOPICTUS", "Aedes albopictus",
    "AEDES CASPIUS",    "Aedes caspius",
    "AEDES DETRITUS",   "Aedes detritus",
    "AEDES SP",         "AEDES SP.",        "Aedes spp",        "Aedes spp.",
    "AEDES VEXANS",     "Aedes vexans",
    "Aedes berlandi",
    "Aedes cantans",
    "Aedes cinereus",   "Aedes cinereus/geminus", "AEDES.GEMINUS.CINEREUS",
    "Aedes flavescens",
    "Aedes geniculatus",
    "Aedes japonicus",
    "Aedes koreicus",
    "Aedes pulchritarsis",
    "Aedes rusticus",
    "Aedes sticticus",
    "Aedes vittatus",
    "Aedes annulipes",
    "Aedes cataphylla",
    "Aedes echinus",
    "Ochlerotatus caspius",  # taxonomic synonym of Aedes caspius (Reinert 2000; Wilkerson et al. 2015 reverted for most Aedes)
    # ── ANOPHELES ──────────────────────────────────────────────────────────────
    "ANOPHELES SP",      "Anopheles spp",    "Anopheles spp.",
    "Anopheles algeriensis",
    "Anopheles claviger", "Anopheles claviger/petragnani",
    "Anopheles maculipennis", "Anopheles maculipennis sl", "Anopheles maculipennis s.l.",
    "Anopheles plumbeus", "An. plumbeus", # <-- AGGIUNTO QUI
    # ── COQUILLETTIDIA ─────────────────────────────────────────────────────────
    "COQUILLETTIDIA RICHIARDII", "Coquillettidia richiardii",
    "Coquillettidia spp", "Coquilletidia", # <-- AGGIUNTO QUI (con errore di battitura originale)
    # ── CULEX ──────────────────────────────────────────────────────────────────
    "CULEX PIPIENS",    "Culex pipiens",
    "Culex Pipiens",    "CULEX PIPENS",    "Cuiex pipiens",   # spelling variants / typos (Liguria dataset)
    "CULEX SP",         "CULEX SP.",        "Culex spp",        "Culex spp.",
    "Culex sp.",   # mixed-case variant of "CULEX SP." (Liguria dataset),
    "Culex hortensis",
    "Culex impudicus",
    "Culex mimeticus",
    "Culex modestus",
    "Culex territans",
    "Culex theileri",
    "Culex univittatus",
    # ── CULICIDAE (unidentified to genus) ──────────────────────────────────────
    "CULICIDAE",
    # ── CULISETA ───────────────────────────────────────────────────────────────
    "CULISETA LONGIAREOLATA", "Culiseta longiareolata",
    "CULISETA SP",      "Culiseta spp",     "Culiseta spp.",
    "Culiseta annulata",
    "Culiseta litorea",
    "Culiseta morsitans",
    "Culiseta subochrea",
    # ── OTHER ──────────────────────────────────────────────────────────────────
    "Orthopodomyia pulcripalpis",
    "Uranotaenia unguiculata",
    "OTHER INSECTS",    "ALTRI INSETTI"
  ),
  
  Canonical_name = c(
    "Tiger mosquito",               "Tiger mosquito",
    "Floodwater mosquito",          "Floodwater mosquito",
    "Coastal floodwater mosquito",  "Coastal floodwater mosquito",
    "Aedes spp. (generic)",         "Aedes spp. (generic)",         "Aedes spp. (generic)", "Aedes spp. (generic)",
    "Inland floodwater mosquito",   "Inland floodwater mosquito",
    "Berland's mosquito",
    "Cantans mosquito",
    "Cinereous mosquito",           "Cinereous mosquito",           "Cinereous mosquito",
    "Flavescent mosquito",
    "Geniculatus mosquito",
    "Japanese bush mosquito",
    "Korean bush mosquito",
    "Pulchritarsis mosquito",
    "Rustic mosquito",
    "Sticticus mosquito",
    "Vittatus mosquito",
    "Annulipes mosquito",
    "Cataphylla mosquito",
    "Echinus mosquito",
    "Floodwater mosquito",          # Ochlerotatus caspius (synonym of Aedes caspius)
    "Anopheles spp.",               "Anopheles spp.",               "Anopheles spp.",
    "Algerian mosquito",
    "Claviger mosquito",            "Claviger-Petragnani mosquito",
    "Maculipennis complex",         "Maculipennis complex",         "Maculipennis complex",
    "Plumbeus mosquito",            "Plumbeus mosquito", 
    "Richiardi's mosquito",         "Richiardi's mosquito",
    "Coquillettidia spp.",          "Coquillettidia spp.",
    "Common house mosquito",        "Common house mosquito",
    "Common house mosquito",        "Common house mosquito",        "Common house mosquito",   # 3 spelling variants
    "Culex spp.",                   "Culex spp.",                   "Culex spp.",           "Culex spp.",
    "Culex spp.",                   # Culex sp. (mixed-case)
    "Hortensis mosquito",
    "Impudicus mosquito",
    "Mimetic mosquito",
    "Modest mosquito",
    "Territans mosquito",
    "Theileri mosquito",
    "Univittatus mosquito",
    "Mosquitoes",
    "Longiareolate mosquito",       "Longiareolate mosquito",
    "Culiseta spp.",                "Culiseta spp.",                "Culiseta spp.",
    "Annulate mosquito",
    "Litorea mosquito",
    "Morsitans mosquito",
    "Subochrea mosquito",
    "Pulcripalpis mosquito",
    "Unguiculate mosquito",
    "Other insects",                "Other insects"
  ),
  
  kingdom = "Animalia",
  phylum  = "Arthropoda",
  class   = "Insecta",
  
  order = c(
    rep("Diptera", 73),  
    NA, NA               
  ),
  
  family = c(
    rep("Culicidae", 73), 
    NA, NA
  ),
  
  genus = c(
    rep("Aedes", 29),
    rep("Anopheles", 11),       
    rep("Coquillettidia", 4),   
    rep("Culex", 17),          
    NA,                        
    rep("Culiseta", 9),        
    "Orthopodomyia",
    "Uranotaenia",
    NA, NA                     
  ),
  
  species = c(
    # Aedes (29)
    "albopictus", "albopictus",
    "caspius",    "caspius",
    "detritus",   "detritus",
    NA, NA, NA, NA,
    "vexans",     "vexans",
    "berlandi",
    "cantans",
    "cinereus",   "cinereus",   "cinereus",
    "flavescens",
    "geniculatus",
    "japonicus",
    "koreicus",
    "pulchritarsis",
    "rusticus",
    "sticticus",
    "vittatus",
    "annulipes",
    "cataphylla",
    "echinus",
    "caspius",    # Ochlerotatus caspius (synonym)
    # Anopheles (11)
    NA, NA, NA,
    "algeriensis",
    "claviger",   "claviger",
    "maculipennis", "maculipennis", "maculipennis",
    "plumbeus",   "plumbeus", 
    # Coquillettidia (4)
    "richiardii", "richiardii", NA, NA, 
    # Culex (17)
    "pipiens",    "pipiens",
    "pipiens",    "pipiens",    "pipiens",  # 3 spelling variants
    NA, NA, NA, NA,
    NA,   # Culex sp. (mixed-case)
    "hortensis",
    "impudicus",
    "mimeticus",
    "modestus",
    "territans",
    "theileri",
    "univittatus",
    # CULICIDAE (1)
    NA,
    # Culiseta (9)
    "longiareolata", "longiareolata",
    NA, NA, NA,
    "annulata",
    "litorea",
    "morsitans",
    "subochrea",
    # Other (4)
    "pulcripalpis",
    "unguiculata",
    NA, NA
  ),
  
  stringsAsFactors = FALSE
)
