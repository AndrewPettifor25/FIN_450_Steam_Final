library(shiny)
library(RSelenium)
library(jsonlite)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(promises)
library(future)
library(DT)
library(shinydashboard)  # Added for valueBox functionality

# Setup for async operations
plan(multisession)

#' Before running the app, scrape data first
#' 
#' This function stores data in a global variable that will be used by the app
#' 
#' @param use_real_data Logical, whether to scrape real data (TRUE) or use sample data (FALSE)
#' @param cases Character vector of case names to scrape
#' @return None, data is stored in global variables
cases <- c(
  "Chroma Case", "Chroma 2 Case", "Falchion Case", "Shadow Case",
  "Revolver Case", "Operation Wildfire Case", "Chroma 3 Case",
  "Gamma Case", "Gamma 2 Case", "Glove Case", "Spectrum Case",
  "Operation Hydra Case", "Spectrum 2 Case", "Clutch Case",
  "Horizon Case", "Danger Zone Case", "Prisma Case", "CS20 Case",
  "Shattered Web Case", "Prisma 2 Case", "Fracture Case",
  "Operation Broken Fang Case", "Snakebite Case",
  "Operation Riptide Case", "Dreams & Nightmares Case",
  "Recoil Case", "Revolution Case", "Kilowatt Case", "Gallery Case"
)
skin_conditions <- c(
  "Factory New",
  "Minimal Wear",
  "Field-Tested",
  "Well-Worn",
  "Battle-Scarred"
)

# Define skin categories
skin_categories <- c(
  "Normal",
  "StatTrak™",
  "Souvenir"
)

# Define mapping of cases to their skins
case_to_skins <- list(
  "Chroma Case" = c(
    "AWP | Man-o'-war",
    "Galil AR | Chatterbox",
    "P250 | Muertos",
    "AK-47 | Cartel",
    "M4A4 | 龍王 (Dragon King)",
    "Sawed-Off | Serenity",
    "Desert Eagle | Naga",
    "Dual Berettas | Urban Shock",
    "Glock-18 | Catacombs",
    "M249 | System Lock",
    "MP9 | Deadly Poison",
    "SCAR-20 | Grotto",
    "XM1014 | Quicksilver",
    "MAC-10 | Malachite"
  ),
  "Chroma 2 Case" = c(
    "M4A1-S | Hyper Beast",
    "MAC-10 | Neon Rider",
    "Five-SeveN | Monkey Business",
    "FAMAS | Djinn",
    "Galil AR | Eco",
    "AWP | Worm God",
    "MAG-7 | Heat",
    "UMP-45 | Grand Prix",
    "CZ75-Auto | Pole Position",
    "Negev | Man-o'-war",
    "P250 | Valence",
    "MP7 | Armor Core",
    "Sawed-Off | Origami",
    "Desert Eagle | Bronze Deco",
    "AK-47 | Elite Build"
  ),
  "Falchion Case" = c(
    "AWP | Hyper Beast",
    "AK-47 | Aquamarine Revenge",
    "CZ75-Auto | Yellow Jacket",
    "SG 553 | Cyrex",
    "MP7 | Nemesis",
    "Negev | Loudmouth",
    "P2000 | Handgun",
    "Nova | Ranger",
    "Glock-18 | Bunsen Burner",
    "UMP-45 | Riot",
    "M4A4 | Evil Daimyo",
    "FAMAS | Neural Net",
    "P90 | Elite Build",
    "MP9 | Ruby Poison Dart",
    "MAC-10 | Rangeen",
    "Galil AR | Rocket Pop"
  ),
  "Shadow Case" = c(
    "M4A1-S | Golden Coil",
    "USP-S | Kill Confirmed",
    "AK-47 | Frontside Misty",
    "G3SG1 | Flux",
    "SSG 08 | Big Iron",
    "P250 | Wingshot",
    "M249 | Nebula Crusader",
    "FAMAS | Survivor Z",
    "Glock-18 | Wraiths",
    "XM1014 | Scumbria",
    "MAG-7 | Cobalt Core",
    "MP7 | Special Delivery",
    "Desert Eagle | Corinthian",
    "SCAR-20 | Green Marine",
    "Negev | Loudmouth",
    "Sawed-Off | Yorick"
  ),
  "Revolver Case" = c(
    "R8 Revolver | Fade",
    "M4A4 | Royal Paladin",
    "AK-47 | Point Disarray",
    "G3SG1 | The Executioner",
    "P90 | Shapewood",
    "Negev | Power Loader",
    "XM1014 | Teclu Burner",
    "PP-Bizon | Fuel Rod",
    "SG 553 | Tiger Moth",
    "CZ75-Auto | Crimson Web",
    "P2000 | Imperial",
    "Desert Eagle | Corinthian",
    "Five-SeveN | Retrobution",
    "Tec-9 | Avalanche",
    "UMP-45 | Primal Saber",
    "Glock-18 | Royal Legion"
  ),
  "Operation Wildfire Case" = c(
    "AK-47 | Fuel Injector",
    "M4A4 | The Battlestar",
    "AWP | Elite Build",
    "Nova | Hyper Beast",
    "Desert Eagle | Kumicho Dragon",
    "MAG-7 | Praetorian",
    "Tec-9 | Jambiya",
    "UMP-45 | Primal Saber",
    "Glock-18 | Royal Legion",
    "FAMAS | Valence",
    "USP-S | Lead Conduit",
    "MP7 | Impire",
    "Sawed-Off | Limelight",
    "PP-Bizon | Photic Zone",
    "SSG 08 | Necropos"
  ),
  "Chroma 3 Case" = c(
    "M4A1-S | Chantico's Fire",
    "PP-Bizon | Judgement of Anubis",
    "P250 | Asiimov",
    "UMP-45 | Primal Saber",
    "AUG | Fleet Flock",
    "XM1014 | Black Tie",
    "Tec-9 | Re-Entry",
    "Galil AR | Firefight",
    "SSG 08 | Ghost Crusader",
    "CZ75-Auto | Red Astor",
    "Dual Berettas | Ventilators",
    "G3SG1 | Orange Crash",
    "M249 | Spectre",
    "MP9 | Bioleak",
    "P2000 | Oceanic",
    "Sawed-Off | Fubar",
    "SG 553 | Atlas"
  ),
  
  "Gamma Case" = c(
    "M4A1-S | Mecha Industries",
    "Glock-18 | Wasteland Rebel",
    "SCAR-20 | Bloodsport",
    "M4A4 | Desolate Space",
    "P2000 | Imperial Dragon",
    "R8 Revolver | Reboot",
    "AUG | Aristocrat",
    "P90 | Chopper",
    "Sawed-Off | Limelight",
    "Five-SeveN | Violent Daimyo",
    "Tec-9 | Ice Cap",
    "PP-Bizon | Harvester",
    "SG 553 | Aerial",
    "Nova | Exo",
    "Negev | Dazzle",
    "XM1014 | Slipstream"
  ),
  "Gamma 2 Case" = c(
    "AK-47 | Neon Revolution",
    "FAMAS | Roll Cage",
    "Tec-9 | Fuel Injector",
    "MP9 | Airlock",
    "AUG | Syd Mead",
    "Desert Eagle | Directive",
    "Glock-18 | Weasel",
    "SCAR-20 | Powercore",
    "SG 553 | Triarch",
    "MAG-7 | Petroglyph",
    "Negev | Dazzle",
    "CZ75-Auto | Imprint",
    "Five-SeveN | Scumbria",
    "P90 | Grim",
    "UMP-45 | Briefing",
    "XM1014 | Slipstream",
    "G3SG1 | Ventilator"
  ),
  "Glove Case" = c(
    "SSG 08 | Dragonfire",
    "M4A4 | Buzz Kill",
    "FAMAS | Mecha Industries",
    "P90 | Shallow Grave",
    "Sawed-Off | Wasteland Princess",
    "USP-S | Cyrex",
    "M4A1-S | Flashback",
    "Dual Berettas | Royal Consorts",
    "G3SG1 | Stinger",
    "Nova | Gila",
    "P2000 | Turf",
    "Galil AR | Black Sand",
    "MP7 | Cirrus",
    "MP9 | Sand Scale",
    "CZ75-Auto | Polymer",
    "Glock-18 | Ironwork",
    "MAG-7 | Sonar"
  ),
  "Spectrum Case" = c(
    "AK-47 | Bloodsport",
    "USP-S | Neo-Noir",
    "M4A1-S | Decimator",
    "CZ75-Auto | Xiangliu",
    "AWP | Fever Dream",
    "M249 | Emerald Poison Dart",
    "Galil AR | Crimson Tsunami",
    "MAC-10 | Last Dive",
    "UMP-45 | Scaffold",
    "XM1014 | Seasons",
    "Five-SeveN | Capillary",
    "SCAR-20 | Blueprint",
    "P250 | Ripple",
    "Sawed-Off | Zander",
    "MP7 | Akoben",
    "G3SG1 | Stinger",
    "SSG 08 | Ghost Crusader"
  ),
  "Operation Hydra Case" = c(
    "AWP | Oni Taiji",
    "Five-SeveN | Hyper Beast",
    "M4A4 | Hellfire",
    "Galil AR | Sugar Rush",
    "Dual Berettas | Cobra Strike",
    "AK-47 | Orbit Mk01",
    "P90 | Death Grip",
    "P2000 | Woodsman",
    "USP-S | Blueprint",
    "P250 | Red Rock",
    "M4A1-S | Briefing",
    "SSG 08 | Death's Head",
    "MAC-10 | Aloha",
    "FAMAS | Macabre",
    "UMP-45 | Metal Flowers",
    "Tec-9 | Cut Out",
    "MP7 | Cirrus"
  ),
  "Spectrum 2 Case" = c(
    "AK-47 | The Empress",
    "P250 | See Ya Later",
    "M4A1-S | Leaded Glass",
    "PP-Bizon | High Roller",
    "R8 Revolver | Llama Cannon",
    "MP9 | Goo",
    "SG 553 | Phantom",
    "CZ75-Auto | Tacticat",
    "UMP-45 | Exposure",
    "XM1014 | Ziggy",
    "Glock-18 | Off World",
    "Tec-9 | Cracked Opal",
    "AUG | Triqua",
    "G3SG1 | Hunter",
    "MAC-10 | Oceanic",
    "Negev | Dazzle",
    "P2000 | Turf"
  ),
  "Clutch Case" = c(
    "M4A4 | Neo-Noir",
    "MP7 | Bloodsport",
    "AWP | Mortis",
    "AUG | Stymphalian",
    "USP-S | Cortex",
    "Glock-18 | Moonrise",
    "MAG-7 | SWAG-7",
    "Negev | Lionfish",
    "SSG 08 | Dragonfire",
    "SG 553 | Aloha",
    "P2000 | Urban Hazard",
    "Five-SeveN | Flame Test",
    "MP9 | Black Sand",
    "R8 Revolver | Grip",
    "PP-Bizon | Night Riot",
    "Nova | Wild Six",
    "XM1014 | Oxide Blaze"
  ),
  "Horizon Case" = c(
    "AK-47 | Neon Rider",
    "Desert Eagle | Code Red",
    "M4A1-S | Nightmare",
    "FAMAS | Eye of Athena",
    "Sawed-Off | Devourer",
    "CZ75-Auto | Eco",
    "G3SG1 | High Seas",
    "Nova | Toy Soldier",
    "P90 | Traction",
    "R8 Revolver | Survivalist",
    "Tec-9 | Snek-9",
    "MP7 | Powercore",
    "Glock-18 | Warhawk",
    "AUG | Amber Slipstream",
    "M4A4 | Magnesium",
    "USP-S | Cortex",
    "AWP | PAW"
  ),
  "Danger Zone Case" = c(
    "AWP | Neo-Noir",
    "AK-47 | Asiimov",
    "Desert Eagle | Mecha Industries",
    "MP5-SD | Phosphor",
    "UMP-45 | Momentum",
    "P250 | Nevermore",
    "Galil AR | Signal",
    "G3SG1 | Scavenger",
    "Tec-9 | Fubar",
    "MAC-10 | Pipe Down",
    "MP9 | Modest Threat",
    "Glock-18 | Oxide Blaze",
    "Nova | Wood Fired",
    "M4A4 | Magnesium",
    "Sawed-Off | Black Sand",
    "SG 553 | Danger Close",
    "USP-S | Flashback"
  ),
  "Prisma Case" = c(
    "M4A4 | The Emperor",
    "Five-SeveN | Angry Mob",
    "XM1014 | Incinegator",
    "AUG | Momentum",
    "R8 Revolver | Skull Crusher",
    "AWP | Atheris",
    "Desert Eagle | Light Rail",
    "UMP-45 | Moonrise",
    "Tec-9 | Bamboozle",
    "MP5-SD | Gauss",
    "P90 | Off World",
    "FAMAS | Crypsis",
    "AK-47 | Uncharted",
    "Galil AR | Akoben",
    "P250 | Verdigris",
    "MAC-10 | Whitefish",
    "MP7 | Mischief"
  ),
  "CS20 Case" = c(
    "AWP | Wildfire",
    "FAMAS | Commemoration",
    "AUG | Death by Puppy",
    "P90 | Nostalgia",
    "MP9 | Hydra",
    "P250 | Inferno",
    "MP5-SD | Agent",
    "UMP-45 | Plastique",
    "Five-SeveN | Buddy",
    "Tec-9 | Flash Out",
    "SCAR-20 | Assault",
    "Dual Berettas | Elite 1.6",
    "M249 | Aztec",
    "MAC-10 | Classic Crate",
    "Glock-18 | Sacrifice",
    "MAG-7 | Popdog"
  ),
  "Shattered Web Case" = c(
    "AK-47 | Rat Rod",
    "MAC-10 | Stalker",
    "SG 553 | Colony IV",
    "SSG 08 | Bloodshot",
    "AUG | Arctic Wolf",
    "MP7 | Neon Ply",
    "R8 Revolver | Memento",
    "Glock-18 | Sacrifice",
    "Nova | Plume",
    "Dual Berettas | Balance",
    "UMP-45 | Plastique",
    "P2000 | Obsidian",
    "MP5-SD | Acid Wash",
    "Tec-9 | Decimator",
    "SCAR-20 | Torn"
  ),
  "Prisma 2 Case" = c(
    "M4A1-S | Player Two",
    "Glock-18 | Bullet Queen",
    "MAC-10 | Disco Tech",
    "SSG 08 | Fever Dream",
    "MAG-7 | Justice",
    "P2000 | Acid Etched",
    "CZ75-Auto | Distressed",
    "SCAR-20 | Enforcer",
    "UMP-45 | Momentum",
    "Negev | Prototype",
    "Sawed-Off | Apocalypto",
    "MP5-SD | Desert Strike",
    "AUG | Tom Cat",
    "AK-47 | Phantom Disruptor",
    "R8 Revolver | Bone Forged",
    "MP7 | Justice",
    "Tec-9 | Operator"
  ),
  "Fracture Case" = c(
    "AK-47 | Legion of Anubis",
    "Desert Eagle | Printstream",
    "Glock-18 | Vogue",
    "M4A4 | Tooth Fairy",
    "XM1014 | Entombed",
    "MAC-10 | Allure",
    "MAG-7 | Monster Call",
    "Galil AR | Connexion",
    "P250 | Cassette",
    "SG 553 | Ol' Rusty",
    "Negev | Ultralight",
    "Tec-9 | Brother",
    "MP5-SD | Kitbash",
    "P90 | Freight",
    "SSG 08 | Mainframe 001",
    "CZ75-Auto | Vendetta",
    "UMP-45 | Momentum"
  ),
  "Operation Broken Fang Case" = c(
    "M4A1-S | Printstream",
    "Glock-18 | Neo-Noir",
    "M4A4 | Cyber Security",
    "USP-S | Monster Mashup",
    "Five-SeveN | Fairy Tale",
    "AWP | Exoskeleton",
    "Nova | Clear Polymer",
    "Dual Berettas | Dezastre",
    "SSG 08 | Parallax",
    "UMP-45 | Gold Bismuth",
    "Galil AR | Vandal",
    "CZ75-Auto | Vendetta",
    "M249 | Deep Relief",
    "MP5-SD | Condition Zero",
    "P250 | Contaminant"
  ),
  "Snakebite Case" = c(
    "M4A4 | In Living Color",
    "USP-S | The Traitor",
    "Galil AR | Chromatic Aberration",
    "MP9 | Food Chain",
    "XM1014 | XOXO",
    "AK-47 | Slate",
    "Desert Eagle | Trigger Discipline",
    "P250 | Cyber Shell",
    "Negev | dev_texture",
    "MAC-10 | Button Masher",
    "Glock-18 | Clear Polymer",
    "Nova | Windblown",
    "SG 553 | Heavy Metal",
    "UMP-45 | Oscillator",
    "CZ75-Auto | Circaetus",
    "R8 Revolver | Junk Yard",
    "M249 | O.S.I.P.R."
  ),
  "Operation Riptide Case" = c(
    "AK-47 | Leet Museo",
    "Desert Eagle | Ocean Drive",
    "SSG 08 | Turbo Peek",
    "Glock-18 | Snack Attack",
    "MAC-10 | Toybox",
    "Five-SeveN | Boost Protocol",
    "MP9 | Mount Fuji",
    "M4A4 | Spider Lily",
    "FAMAS | ZX Spectron",
    "MAG-7 | BI83 Spectrum",
    "USP-S | Black Lotus",
    "G3SG1 | Keeping Tabs",
    "PP-Bizon | Lumen",
    "MP7 | Guerrilla",
    "P2000 | Gnarled",
    "SCAR-20 | Magna Carta"
  ),
  "Dreams & Nightmares Case" = c(
    "AK-47 | Nightwish",
    "MP9 | Starlight Protector",
    "FAMAS | Rapid Eye Movement",
    "Dual Berettas | Melondrama",
    "MP7 | Abyssal Apparition",
    "M4A1-S | Night Terror",
    "USP-S | Ticket to Hell",
    "XM1014 | Zombie Offensive",
    "PP-Bizon | Space Cat",
    "G3SG1 | Dream Glade",
    "Five-SeveN | Scrawl",
    "MAC-10 | Ensnared",
    "SCAR-20 | Poultrygeist"
  ),
  "Recoil Case" = c(
    "USP-S | Printstream",
    "AWP | Chromatic Aberration",
    "AK-47 | Ice Coaled",
    "P250 | Visions",
    "Sawed-Off | Kiss♥Love",
    "SG 553 | Dragon Tech",
    "M249 | Downtown",
    "Dual Berettas | Flora Carnivora",
    "R8 Revolver | Crazy 8",
    "P90 | Vent Rush",
    "M4A1-S | Leaded Glass",
    "MAC-10 | Monkeyflage",
    "Negev | Drop Me",
    "UMP-45 | Roadblock",
    "Glock-18 | Winterized",
    "FAMAS | Meow 36",
    "Galil AR | Destroyer"
  ),
  "Revolution Case" = c(
    "AK-47 | Head Shot",
    "M4A4 | Temukau",
    "UMP-45 | Wild Child",
    "P2000 | Wicked Sick",
    "MP9 | Featherweight",
    "MAC-10 | Sakkaku",
    "MP5-SD | Liquidation",
    "MAG-7 | Insomnia",
    "SCAR-20 | Fragments",
    "SSG 08 | Bloodshot",
    "Tec-9 | Rebel",
    "Glock-18 | Umbral Rabbit",
    "AUG | Midnight Lily",
    "P90 | Neoqueen",
    "R8 Revolver | Banana Cannon",
    "Five-SeveN | Hybrid",
    "SG 553 | Cyberforce"
  ),
  "Kilowatt Case" = c(
    "AK-47 | Inheritance",
    "USP-S | Jawbreaker",
    "AWP | Chrome Cannon",
    "M4A1-S | Black Lotus",
    "MAC-10 | Light Box",
    "Nova | Dark Sigil",
    "Glock-18 | Block-18",
    "MP5-SD | Liquid Camo",
    "XM1014 | Irezumi",
    "MP7 | Just Smile",
    "R8 Revolver | Extinguisher",
    "P90 | Futurisk",
    "SSG 08 | Dezastre",
    "M249 | Spectre",
    "FAMAS | Serotonin",
    "P250 | Re.built",
    "MAG-7 | Heat Sink"
  ),
  "Gallery Case" = c(
    "M4A1-S | Vaporwave",
    "Glock-18 | Gold Toof",
    "AK-47 | The Outsiders",
    "UMP-45 | Neo-Noir",
    "P250 | Epicenter",
    "Dual Berettas | Hydro Strike",
    "M4A4 | Turbine",
    "SSG 08 | Rapid Transit",
    "P90 | Randy Rush",
    "MAC-10 | Saibā Oni",
    "USP-S | 027",
    "SCAR-20 | Trail Blazer",
    "R8 Revolver | Tango",
    "MP5-SD | Statics",
    "M249 | Hypnosis",
    "Desert Eagle | Calligraffiti",
    "AUG | Luxe Trim"
  )
)

# Function to get USD to CAD exchange rate data
get_usd_cad_rates <- function() {
  # Get historical USD to CAD exchange rates using tidyquant
  message("Fetching USD to CAD exchange rates...")
  fx_usdcad <- tryCatch({
    tq_get("CAD=X", get = "stock.prices", from = "2010-01-01")
  }, error = function(e) {
    message("Error fetching exchange rates: ", e$message)
    # Return dummy data if real data fetch fails
    tibble(
      date = seq(as.Date("2010-01-01"), Sys.Date(), by = "day"),
      close = runif(length(seq(as.Date("2010-01-01"), Sys.Date(), by = "day")), 1.25, 1.35)
    )
  })
  
  # Clean the data to just date and rate
  fx_usdcad <- fx_usdcad %>%
    select(date, rate = close) %>%
    mutate(date = as.Date(date))
  
  return(fx_usdcad)
}

# Modified scraping function to handle cases, skins with conditions and categories
scrape_and_prepare_data <- function(use_real_data = FALSE, cases = c("Chroma Case", "Chroma 2 Case"), 
                                    selected_case = NULL, selected_skin = NULL, selected_condition = NULL,
                                    selected_category = NULL) {
  
  # Define scraping function for cases and skins (now with condition and category)
  scrape_case_chart_data <- function(item_name, remDr, is_skin = FALSE, condition = NULL, category = NULL, wait_time = 6) {
    # Process skin name with category and condition if provided
    if (is_skin) {
      # Add category prefix if not "Normal"
      if (!is.null(category) && category != "Normal") {
        item_name <- paste0(category, " ", item_name)
      }
      
      # Add condition suffix
      if (!is.null(condition)) {
        item_name <- paste0(item_name, " (", condition, ")")
      }
      
      message(paste("Scraping data for", item_name))
    } else {
      message(paste("Scraping data for", item_name))
    }
    
    # For skins, we need to prepend the weapon type
    url_name <- item_name
    
    encoded_item <- URLencode(url_name)
    url <- paste0("https://steamcommunity.com/market/listings/730/", encoded_item)
    
    remDr$navigate(url)
    Sys.sleep(wait_time)
    
    html <- remDr$getPageSource()[[1]]
    html_clean <- gsub("[\r\n\t]", "", html)
    
    pattern <- "var line1=\\[(\\[.*?\\])\\];"
    matches <- stringr::str_match(html_clean, pattern)
    
    if (is.na(matches[2])) {
      message(paste("❌ Could not extract line1 for:", item_name))
      return(NULL)
    }
    
    json_array <- paste0("[", matches[2], "]")
    
    chart_data <- tryCatch({
      fromJSON(json_array)
    }, error = function(e) {
      message(paste("❌ JSON parse error for", item_name, ":", e$message))
      return(NULL)
    })
    
    num_cols <- ncol(chart_data)
    
    df <- tibble(
      time = chart_data[, 1],
      price_usd = as.numeric(chart_data[, 2]),
      volume = if (num_cols >= 3) as.numeric(chart_data[, 3]) else NA_real_
    ) %>%
      mutate(
        time_clean = str_remove(time, " \\+0$"),
        date = parse_date_time(time_clean, orders = "b d Y H", tz = "UTC"),
        Item = item_name,
        Type = if(is_skin) "Skin" else "Case",
        Condition = if(is_skin && !is.null(condition)) condition else NA_character_,
        Category = if(is_skin && !is.null(category)) category else "Normal"
      ) %>%
      select(Item, Type, Category, Condition, date, price_usd, volume) %>%
      filter(!is.na(date))
    
    return(df)
  }
  
  # Get the data - either real or sample
  if (use_real_data) {
    message("Starting RSelenium... this may take a moment")
    rD <- rsDriver(browser = "firefox", chromever = NULL, verbose = FALSE)
    remDr <- rD$client
    
    # Create a list to store data for each item
    all_data_list <- list()
    
    # If specific case, skin, category, and condition are provided, scrape just those
    if (!is.null(selected_case) && !is.null(selected_skin) && !is.null(selected_condition) && 
        !is.null(selected_category) && selected_skin != "All") {
      # Scrape case
      case_data <- scrape_case_chart_data(selected_case, remDr, is_skin = FALSE)
      if (!is.null(case_data)) {
        all_data_list[[selected_case]] <- case_data
        message(paste("✓ Successfully scraped data for", selected_case))
      }
      
      # Scrape skin with category and condition
      skin_data <- scrape_case_chart_data(
        selected_skin, 
        remDr, 
        is_skin = TRUE, 
        condition = selected_condition,
        category = selected_category
      )
      
      if (!is.null(skin_data)) {
        # Build the full skin name based on category
        if (selected_category == "Normal") {
          full_skin_name <- paste0(selected_skin, " (", selected_condition, ")")
        } else {
          full_skin_name <- paste0(selected_category, " ", selected_skin, " (", selected_condition, ")")
        }
        all_data_list[[full_skin_name]] <- skin_data
        message(paste("✓ Successfully scraped data for", full_skin_name))
      }
    } 
    # If case and skin are selected but missing category or condition, use defaults
    else if (!is.null(selected_case) && !is.null(selected_skin) && selected_skin != "All") {
      # Scrape the case
      case_data <- scrape_case_chart_data(selected_case, remDr, is_skin = FALSE)
      if (!is.null(case_data)) {
        all_data_list[[selected_case]] <- case_data
        message(paste("✓ Successfully scraped data for", selected_case))
      }
      
      # Use defaults if not specified
      condition_to_use <- if(is.null(selected_condition)) "Factory New" else selected_condition
      category_to_use <- if(is.null(selected_category)) "Normal" else selected_category
      
      # Scrape skin with defaults
      skin_data <- scrape_case_chart_data(
        selected_skin, 
        remDr, 
        is_skin = TRUE, 
        condition = condition_to_use,
        category = category_to_use
      )
      
      if (!is.null(skin_data)) {
        # Build the full skin name based on category
        if (category_to_use == "Normal") {
          full_skin_name <- paste0(selected_skin, " (", condition_to_use, ")")
        } else {
          full_skin_name <- paste0(category_to_use, " ", selected_skin, " (", condition_to_use, ")")
        }
        all_data_list[[full_skin_name]] <- skin_data
        message(paste("✓ Successfully scraped data for", full_skin_name))
      }
    }
    # If only case is selected, scrape that case and sample skins
    else if (!is.null(selected_case)) {
      # Scrape the case
      case_data <- scrape_case_chart_data(selected_case, remDr, is_skin = FALSE)
      if (!is.null(case_data)) {
        all_data_list[[selected_case]] <- case_data
        message(paste("✓ Successfully scraped data for", selected_case))
      }
      
      # Scrape some skins for the case with Factory New condition and Normal category (defaults)
      for (i in 1:min(3, length(case_to_skins[[selected_case]]))) {
        skin <- case_to_skins[[selected_case]][i]
        skin_data <- scrape_case_chart_data(
          skin, 
          remDr, 
          is_skin = TRUE, 
          condition = "Factory New",
          category = "Normal"
        )
        if (!is.null(skin_data)) {
          full_skin_name <- paste0(skin, " (Factory New)")
          all_data_list[[full_skin_name]] <- skin_data
          message(paste("✓ Successfully scraped data for", full_skin_name))
        }
        Sys.sleep(2)  # Add a small delay between requests
      }
    }
    # Otherwise, scrape all selected cases
    else {
      for (case_name in cases) {
        case_data <- scrape_case_chart_data(case_name, remDr, is_skin = FALSE)
        if (!is.null(case_data)) {
          all_data_list[[case_name]] <- case_data
          message(paste("✓ Successfully scraped data for", case_name))
        }
        Sys.sleep(2)  # Add a small delay between requests
      }
    }
    
    # Close the Selenium session
    remDr$close()
    rD$server$stop()
    
    # Combine all data
    all_data <- bind_rows(all_data_list)
    
    # Get exchange rates and convert USD to CAD
    fx_rates <- get_usd_cad_rates()
    
    # Add CAD prices using the exchange rate data
    all_data <- all_data %>%
      mutate(date_only = as.Date(date)) %>%
      left_join(fx_rates, by = c("date_only" = "date")) %>%
      mutate(
        # Use the most recent available rate if data is missing
        rate = if_else(is.na(rate), last(na.omit(fx_rates$rate)), rate),
        price_cad = price_usd * rate
      ) %>%
      select(-date_only)
    
    return(all_data)
  } else {
    # Use sample data if not scraping real data
    # Create synthetic data for demonstration
    set.seed(123)
    
    sample_data <- tibble(
      Item = character(),
      Type = character(),
      Category = character(),
      Condition = character(),
      date = as.POSIXct(character()),
      price_usd = numeric(),
      volume = numeric()
    )
    
    # Generate sample data for each case
    for (case_name in cases) {
      dates <- seq(as.POSIXct("2020-01-01"), as.POSIXct("2023-04-01"), by = "day")
      
      # Case data
      case_data <- tibble(
        Item = case_name,
        Type = "Case",
        Category = "Normal",
        Condition = NA_character_,
        date = dates,
        price_usd = cumsum(rnorm(length(dates), 0, 0.01)) + 
          runif(1, 0.5, 3) + 
          sin(seq(0, 2*pi*3, length.out = length(dates)))*0.2,
        volume = round(abs(rnorm(length(dates), 1000, 300)))
      )
      
      sample_data <- bind_rows(sample_data, case_data)
      
      # Add a few skins for each case, with different conditions and categories
      if (!is.null(case_to_skins[[case_name]])) {
        for (i in 1:min(3, length(case_to_skins[[case_name]]))) {
          skin_name <- case_to_skins[[case_name]][i]
          
          # Generate data for different categories and conditions
          for (category in skin_categories) {
            # StatTrak and Souvenir are more expensive
            category_multiplier <- switch(category,
                                          "Normal" = 1.0,
                                          "StatTrak™" = 2.5,
                                          "Souvenir" = 3.0)
            
            for (condition in skin_conditions) {
              # Price multiplier based on condition (Factory New most expensive)
              condition_multiplier <- switch(condition,
                                             "Factory New" = 1.5,
                                             "Minimal Wear" = 1.2,
                                             "Field-Tested" = 1.0,
                                             "Well-Worn" = 0.8,
                                             "Battle-Scarred" = 0.6)
              
              # Build full skin name based on category
              if (category == "Normal") {
                full_skin_name <- paste0(skin_name, " (", condition, ")")
              } else {
                full_skin_name <- paste0(category, " ", skin_name, " (", condition, ")")
              }
              
              skin_data <- tibble(
                Item = full_skin_name,
                Type = "Skin",
                Category = category,
                Condition = condition,
                date = dates,
                price_usd = (cumsum(rnorm(length(dates), 0, 0.05)) + 
                               runif(1, 5, 50) + 
                               sin(seq(0, 2*pi*2, length.out = length(dates)))*2) * 
                  condition_multiplier * category_multiplier,
                volume = round(abs(rnorm(length(dates), 500, 150)) / category_multiplier) # Lower volume for rarer items
              )
              
              sample_data <- bind_rows(sample_data, skin_data)
            }
          }
        }
      }
    }
    
    # Ensure all values are positive
    sample_data <- sample_data %>%
      mutate(
        price_usd = pmax(0.03, price_usd),
        volume = pmax(1, volume)
      )
    
    # Get exchange rates and convert USD to CAD for sample data
    fx_rates <- get_usd_cad_rates()
    
    # Add CAD prices using the exchange rate data
    sample_data <- sample_data %>%
      mutate(date_only = as.Date(date)) %>%
      left_join(fx_rates, by = c("date_only" = "date")) %>%
      mutate(
        # Use the most recent available rate if data is missing
        rate = if_else(is.na(rate), 1.3, rate),  # Default rate of 1.3 if missing
        price_cad = price_usd * rate
      ) %>%
      select(-date_only)
    
    return(sample_data)
  }
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "CS:GO Market Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      checkboxInput("use_real_data", "Use Real Data (Slower)", FALSE),
      selectInput("case_select", "Select Case:", choices = cases),
      uiOutput("skin_select_ui"),
      # Add category and condition selection UI
      uiOutput("category_select_ui"),
      uiOutput("condition_select_ui"),
      actionButton("scrape_btn", "Scrape Data", class = "btn-primary"),
      hr(),
      dateRangeInput("date_range", "Date Range:",
                     start = "2022-01-01",
                     end = Sys.Date()),
      checkboxInput("show_volume", "Show Volume", TRUE),
      radioButtons("currency", "Currency:", 
                   choices = c("USD" = "usd", "CAD" = "cad"),
                   selected = "usd"),
      checkboxInput("show_ma", "Show Moving Averages", FALSE),
      conditionalPanel(
        condition = "input.show_ma == true",
        sliderInput("ma_periods", "MA Periods:", min = 5, max = 100, value = 30)
      ),
      hr(),
      checkboxInput("compare_mode", "Compare Items", FALSE),
      conditionalPanel(
        condition = "input.compare_mode == true",
        selectInput("compare_case", "Compare with Case:", choices = c("None", cases)),
        uiOutput("compare_skin_ui"),
        # Add compare category and condition UI
        uiOutput("compare_category_ui"),
        uiOutput("compare_condition_ui")
      )
    )
  ),
  
  dashboardBody(
    tabBox(
      width = 12,
      tabPanel("Price Chart", 
               plotOutput("price_chart"),
               plotOutput("volume_chart", height = "200px")),
      tabPanel("Price Analysis", 
               fluidRow(
                 valueBox(value = textOutput("max_price"), subtitle = "Maximum Price", width = 3, color = "blue"),
                 valueBox(value = textOutput("min_price"), subtitle = "Minimum Price", width = 3, color = "red"),
                 valueBox(value = textOutput("avg_price"), subtitle = "Average Price", width = 3, color = "purple"),
                 valueBox(value = textOutput("price_change"), subtitle = "Price Change", width = 3, color = "green")
               ),
               plotOutput("price_distribution")),
      tabPanel("Data Table", DTOutput("data_table")),
      tabPanel("Condition Comparison", 
               checkboxInput("show_all_conditions", "Show All Conditions for Selected Skin", TRUE),
               plotOutput("condition_comparison"))
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Store the data
  market_data <- reactiveVal(NULL)
  
  # Update skin selection based on case
  output$skin_select_ui <- renderUI({
    req(input$case_select)
    skins <- case_to_skins[[input$case_select]]
    selectInput("skin_select", "Select Skin:", choices = c("All", skins))
  })
  
  # Add category selection UI
  output$category_select_ui <- renderUI({
    req(input$skin_select)
    if(input$skin_select == "All") {
      return(NULL)
    } else {
      selectInput("category_select", "Select Category:", choices = skin_categories)
    }
  })
  
  # Add condition selection UI
  output$condition_select_ui <- renderUI({
    req(input$skin_select)
    if(input$skin_select == "All") {
      return(NULL)
    } else {
      selectInput("condition_select", "Select Condition:", choices = skin_conditions)
    }
  })
  
  # Update comparison skin selection
  output$compare_skin_ui <- renderUI({
    req(input$compare_case)
    if(input$compare_case == "None") {
      return(NULL)
    }
    skins <- case_to_skins[[input$compare_case]]
    selectInput("compare_skin", "Compare with Skin:", choices = c("None", skins))
  })
  
  # Add comparison category selection UI
  output$compare_category_ui <- renderUI({
    req(input$compare_skin, input$compare_case)
    if(input$compare_skin == "None") {
      return(NULL)
    }
    selectInput("compare_category", "Compare Category:", choices = skin_categories)
  })
  
  # Add comparison condition selection UI
  output$compare_condition_ui <- renderUI({
    req(input$compare_skin, input$compare_case)
    if(input$compare_skin == "None") {
      return(NULL)
    }
    selectInput("compare_condition", "Compare Condition:", choices = skin_conditions)
  })
  
  # Scrape data when button is clicked
  observeEvent(input$scrape_btn, {
    withProgress(message = 'Scraping data...', value = 0, {
      # Capture the current values of inputs BEFORE passing to future_promise
      use_real_data_val <- input$use_real_data
      selected_case <- input$case_select
      selected_skin <- if(input$skin_select == "All") NULL else input$skin_select
      selected_category <- if(!is.null(selected_skin) && selected_skin != "All") input$category_select else NULL
      selected_condition <- if(!is.null(selected_skin) && selected_skin != "All") input$condition_select else NULL
      
      # Scrape data asynchronously with the captured values
      future_promise({
        scrape_and_prepare_data(
          use_real_data = use_real_data_val,
          cases = cases,
          selected_case = selected_case,
          selected_skin = selected_skin,
          selected_condition = selected_condition,
          selected_category = selected_category
        )
      }) %...>% 
        (function(data) {
          market_data(data)
          showNotification("Data scraping completed!", type = "message")
        }) %...!% 
        (function(error) {
          showNotification(paste("Error:", error$message), type = "error")
        })
    })
  })
  
  # Filter data based on UI selections
  filtered_data <- reactive({
    req(market_data())
    
    data <- market_data() %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    # Filter for primary selection
    if (input$skin_select != "All") {
      # Create the full skin name with category and condition
      if (input$category_select == "Normal") {
        full_skin_name <- paste0(input$skin_select, " (", input$condition_select, ")")
      } else {
        full_skin_name <- paste0(input$category_select, " ", input$skin_select, " (", input$condition_select, ")")
      }
      primary_items <- c(input$case_select, full_skin_name)
    } else {
      # Select the case and all its skins
      primary_items <- c(input$case_select)
      # Add all skins from the case if "All" is selected
      for (skin in case_to_skins[[input$case_select]]) {
        for (category in skin_categories) {
          for (condition in skin_conditions) {
            # Build the full skin name based on category
            if (category == "Normal") {
              full_skin_name <- paste0(skin, " (", condition, ")")
            } else {
              full_skin_name <- paste0(category, " ", skin, " (", condition, ")")
            }
            
            if (full_skin_name %in% data$Item) {
              primary_items <- c(primary_items, full_skin_name)
            }
          }
        }
      }
    }
    
    # Add comparison items if in compare mode
    if (input$compare_mode && input$compare_case != "None") {
      comparison_items <- c(input$compare_case)
      if (!is.null(input$compare_skin) && input$compare_skin != "None") {
        # Build the comparison skin name based on selected category
        if (input$compare_category == "Normal") {
          full_compare_skin <- paste0(input$compare_skin, " (", input$compare_condition, ")")
        } else {
          full_compare_skin <- paste0(input$compare_category, " ", input$compare_skin, " (", input$compare_condition, ")")
        }
        comparison_items <- c(comparison_items, full_compare_skin)
      }
      selected_items <- c(primary_items, comparison_items)
    } else {
      selected_items <- primary_items
    }
    
    data %>% filter(Item %in% selected_items)
  })
  
  # Special filtered data for condition comparison
  condition_comparison_data <- reactive({
    req(market_data(), input$skin_select)
    req(input$skin_select != "All")
    
    data <- market_data() %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    # Get all conditions for the selected skin - base search on category
    if (input$category_select == "Normal") {
      base_skin_pattern <- paste0("^", input$skin_select, " \\(")
    } else {
      base_skin_pattern <- paste0("^", input$category_select, " ", input$skin_select, " \\(")
    }
    
    data %>% 
      filter(grepl(base_skin_pattern, Item)) %>%
      mutate(Condition = stringr::str_extract(Item, "\\([^)]+\\)") %>% 
               stringr::str_remove_all("\\(|\\)"))
  })
  
  # Price chart
  output$price_chart <- renderPlot({
    req(filtered_data())
    
    data <- filtered_data()
    
    # Select price column based on currency selection
    price_col <- if(input$currency == "usd") "price_usd" else "price_cad"
    currency_label <- if(input$currency == "usd") "USD" else "CAD"
    
    # Add moving averages if requested
    if (input$show_ma) {
      data <- data %>%
        group_by(Item) %>%
        arrange(date) %>%
        mutate(
          MA_price = rollmean(!!sym(price_col), k = input$ma_periods, fill = NA, align = "right")
        ) %>%
        ungroup()
    }
    
    p <- ggplot(data, aes(x = date, y = !!sym(price_col), color = Item)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = "Price History",
        x = "Date",
        y = paste0("Price (", currency_label, ")"),
        color = "Item"
      )
    
    if (input$show_ma) {
      p <- p + 
        geom_line(aes(y = MA_price, linetype = "Moving Average"), size = 1) +
        scale_linetype_manual(values = "dashed", name = "")
    }
    
    p
  })
  
  # Volume chart
  output$volume_chart <- renderPlot({
    req(filtered_data())
    req(input$show_volume)
    
    filtered_data() %>%
      ggplot(aes(x = date, y = volume, color = Item)) +
      geom_line(alpha = 0.7) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(
        title = "Volume History",
        x = "Date",
        y = "Volume"
      )
  })
  
  # Price distribution chart
  output$price_distribution <- renderPlot({
    req(filtered_data())
    
    # Select price column based on currency selection
    price_col <- if(input$currency == "usd") "price_usd" else "price_cad"
    currency_label <- if(input$currency == "usd") "USD" else "CAD"
    
    filtered_data()
    filtered_data() %>%
      ggplot(aes(x = !!sym(price_col), fill = Item)) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "Price Distribution",
        x = paste0("Price (", currency_label, ")"),
        y = "Density",
        fill = "Item"
      )
  })
  
  # Condition comparison chart
  output$condition_comparison <- renderPlot({
    req(condition_comparison_data())
    req(input$skin_select != "All")
    
    # Select price column based on currency selection
    price_col <- if(input$currency == "usd") "price_usd" else "price_cad"
    currency_label <- if(input$currency == "usd") "USD" else "CAD"
    
    if(input$show_all_conditions) {
      condition_comparison_data() %>%
        ggplot(aes(x = date, y = !!sym(price_col), color = Condition)) +
        geom_line() +
        theme_minimal() +
        labs(
          title = paste("Price Comparison by Condition:", input$skin_select),
          x = "Date",
          y = paste0("Price (", currency_label, ")"),
          color = "Condition"
        )
    } else {
      # Show just the selected condition if not showing all
      condition_comparison_data() %>%
        filter(Condition == input$condition_select) %>%
        ggplot(aes(x = date, y = !!sym(price_col))) +
        geom_line(color = "blue") +
        theme_minimal() +
        labs(
          title = paste("Price for", input$skin_select, "(", input$condition_select, ")"),
          x = "Date",
          y = paste0("Price (", currency_label, ")")
        )
    }
  })
  
  # Data table
  output$data_table <- renderDT({
    req(filtered_data())
    
    filtered_data() %>%
      arrange(desc(date)) %>%
      mutate(
        date = format(date, "%Y-%m-%d"),
        price_usd = round(price_usd, 2),
        price_cad = round(price_cad, 2),
        rate = round(rate, 4)
      ) %>%
      datatable(options = list(pageLength = 15))
  })
  
  # Value box outputs
  output$max_price <- renderText({
    req(filtered_data())
    # Select price column based on currency selection
    price_col <- if(input$currency == "usd") "price_usd" else "price_cad"
    currency_symbol <- if(input$currency == "usd") "$" else "C$"
    
    paste0(currency_symbol, round(max(filtered_data()[[price_col]], na.rm = TRUE), 2))
  })
  
  output$min_price <- renderText({
    req(filtered_data())
    # Select price column based on currency selection
    price_col <- if(input$currency == "usd") "price_usd" else "price_cad"
    currency_symbol <- if(input$currency == "usd") "$" else "C$"
    
    paste0(currency_symbol, round(min(filtered_data()[[price_col]], na.rm = TRUE), 2))
  })
  
  output$avg_price <- renderText({
    req(filtered_data())
    # Select price column based on currency selection
    price_col <- if(input$currency == "usd") "price_usd" else "price_cad"
    currency_symbol <- if(input$currency == "usd") "$" else "C$"
    
    paste0(currency_symbol, round(mean(filtered_data()[[price_col]], na.rm = TRUE), 2))
  })
  
  output$price_change <- renderText({
    req(filtered_data())
    # Select price column based on currency selection
    price_col <- if(input$currency == "usd") "price_usd" else "price_cad"
    
    data <- filtered_data() %>%
      arrange(date)
    
    first_price <- data[[price_col]][1]
    last_price <- data[[price_col]][nrow(data)]
    
    change_pct <- (last_price - first_price) / first_price * 100
    
    paste0(ifelse(change_pct >= 0, "+", ""), round(change_pct, 2), "%")
  })
}

# Run the application
shinyApp(ui = ui, server = server)