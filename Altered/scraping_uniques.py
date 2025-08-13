"""
Script to scrap altered unique after connexion to your account
Usage: python ./scraping_uniques.py -faction <faction to scrap> -name <name of uniques> -path <path to database of cards>
Author: G. Mulier
"""

# Libraries 

import pandas as pd
from argparse import ArgumentParser
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.common.exceptions import TimeoutException, WebDriverException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import re
from datetime import datetime

# Functions

## Retrieve the page
def AccederPage(url, compte_perso = False):
    Erreur = False
    try:
        OptionsNav = webdriver.chrome.options.Options() # Remove non fatal errors
        OptionsNav.add_argument("--log-level=3")
        driver = webdriver.Chrome(options = OptionsNav)
        driver.set_page_load_timeout(20)  # Timeout after 10 seconds
        driver.get(url)
    except TimeoutException:
        Erreur = True
        print("Error: too long loading.")
    except WebDriverException as e:
        Erreur = True
        print(f"Erreur Selenium: {e}")
    if not Erreur:
        time.sleep(2)
        RefuserCookies = driver.find_element(webdriver.common.by.By.ID, "didomi-notice-disagree-button")
        RefuserCookies.click()
        ## Set resolution
        driver.set_window_size(1500, 800)
        if compte_perso: # Account must be connected
            ReponseInput = ""
            BoutonConnect = driver.find_element(webdriver.common.by.By.XPATH, "//div[contains(@class, 'fixed') and contains(@class, 'top-2') and contains(@class, 'right-2')]")
            if BoutonConnect.text in ["SIGN IN", "SE CONNECTER"]:
                while ReponseInput != "OK":
                    ReponseInput = input("Connect your altered account on the open web browser, then type OK and press Enter.\n")
            else:
                while ReponseInput != "OK":
                    ReponseInput = input("If your account is " + BoutonConnect.text + " type 'OK' and Enter. Else, connect your altered account on the open web browser, then type OK and press Enter.\n")
            driver.refresh()
            time.sleep(2)
            BoutonConnect = driver.find_element(webdriver.common.by.By.XPATH, "//div[contains(@class, 'fixed') and contains(@class, 'top-2') and contains(@class, 'right-2')]")
            print(f"Page successfully loaded on account {BoutonConnect.text}.")
        else:
            print(f"Page successfully loaded.")
        return (True, driver)
    else:
        return(False)

def MiseEnForme(StringForme):
    ## Complete effects, rarity, etc.
    StringForme = BeautifulSoup(StringForme, "html.parser")
    for tag in StringForme.find_all("i", class_="fa-kit"):
        if "fa-altered-r" in tag.get("class", []):
            tag.replace_with("Effet de réserve : ")
        elif "fa-altered-h" in tag.get("class", []):
            tag.replace_with("Effet de main : ")
        elif "fa-altered-j" in tag.get("class", []):
            tag.replace_with("Effet flèche : ")
        elif "fa-altered-d" in tag.get("class", []):
            tag.replace_with("Effet de soutien : ")
        elif "fa-altered-t" in tag.get("class", []):
            tag.replace_with("Effet d'épuisement : ")
        elif "fa-altered-v" in tag.get("class", []):
            tag.replace_with(" forêt ")
        elif "fa-altered-o" in tag.get("class", []):
            tag.replace_with(" océan ")
        elif "fa-altered-m" in tag.get("class", []):
            tag.replace_with(" montagne ")
        elif "fa-altered-i" in tag.get("class", []):
            tag.replace_with("Effet continu de réserve : ")
        elif "fa-altered-1" in tag.get("class", []):
            tag.replace_with(" 1 ")
        elif "fa-altered-2" in tag.get("class", []):
            tag.replace_with(" 2 ")
        elif "fa-altered-3" in tag.get("class", []):
            tag.replace_with(" 3 ")
        elif "fa-altered-4" in tag.get("class", []):
            tag.replace_with(" 4 ")
        elif "fa-altered-5" in tag.get("class", []):
            tag.replace_with(" 5 ")
        elif "fa-altered-6" in tag.get("class", []):
            tag.replace_with(" 6 ")
        elif "fa-altered-7" in tag.get("class", []):
            tag.replace_with(" 7 ")
        elif "fa-altered-8" in tag.get("class", []):
            tag.replace_with(" 8 ")
        elif "fa-altered-9" in tag.get("class", []):
            tag.replace_with(" 9 ")
    return StringForme.get_text(separator = "\n", strip = True)

## Create the query
def CreerQuery(Site, Page, Faction, Carte):
    ListeFactions = {
        "axiom": "AX", 
        "bravos": "BR", 
        "lyra": "LY", 
        "muna": "MU", 
        "ordis": "OR", 
        "yzmir": "YZ"
    }
    return Site + "/market?page=" + str(Page) + "&factions[]=" + ListeFactions[Faction] + "&inSale=true&query=" + Carte + "&rarity[]=UNIQUE"

# Command arguments 

parser = ArgumentParser(description = "Scrap uniques from a faction")
parser.add_argument(
    "-faction",
    type = str,
    choices = ["axiom", "bravos", "lyra", "muna", "ordis", "yzmir"],
    required = False,
    default = "bravos",
    help = "Faction name of the uniques to scrap (must be one of: axiom [default], bravos, lyra, muna, ordis, yzmir)"
)
parser.add_argument(
    "-name",
    type = str,
    required = False,
    default = "",
    help = "Name of the unique card(s) to scrap. If not supplied, scrap all uniques in the excel file provided. If supplied, name of cards separated by ¤."
)
parser.add_argument(
    "-path",
    type = str,
    required = False,
    default = "./Altered/liste_cartes_20250617.xlsx",
    help = "Path for database of cards."
)
parser.add_argument(
    "-radical",
    type = str,
    required = False,
    default = "uniques",
    help = "Name for the output file (completed by the date)."
)
Arguments = parser.parse_args()

print(f"Scrapping of uniques from faction {Arguments.faction}!")
if Arguments.name == "":
    print("All uniques will be taken.")
else:
    print(f"Unique to scrap: {Arguments.name}")

# Global variables

print("Beginning of the script...")

Debut = time.time()
site = "https://www.altered.gg/fr-fr/cards"

## Database of cards if no name supplied
DonneesCartes = pd.read_excel(Arguments.path)
DonneesCartes = DonneesCartes[(DonneesCartes["Faction"].str.lower() == Arguments.faction) & 
                              (DonneesCartes["Type"] == "Personnage")]
DonneesCartes = DonneesCartes.sort_values(by = "Rarity", ascending = False)
DonneesCartes = DonneesCartes.drop_duplicates(subset = "Nom", keep = "first")

if Arguments.name == "":
    CartesToScrap = DonneesCartes["Nom"].unique().tolist()
else:
    CartesToScrap = Arguments.name.split("¤")
print(f"Number of different cards to search for: {str(len(CartesToScrap))}...")

## Structure of the data to retrieve 
Donnees = {
    "Nom": [],
    "Faction": [],
    "Type": [],
    "SubType": [],
    "HandCost": [],
    "ReserveCost": [],
    "Forest": [],
    "Mountain": [],
    "Lake": [],
    "Effect": [],
    "Price": [],
    "Reference": []
}
ListeCartes = []
ListePrix = []

# Scraping of cards

Accessible, Page = AccederPage(site, True)

print("Searching of the different adresses for the cards to scrap")

if Accessible:
    for carte in CartesToScrap:
        print(f"Scraping for {carte}: page ", end = "")
        cartejs = carte.replace("'", " ")
        for p in range(1, 31):
            print(str(p) + " - ", end = "")
            Adresse = CreerQuery(site, p, "bravos", cartejs)
            Page.execute_script(f"window.open('{Adresse}');")
            handles = Page.window_handles
            Page.switch_to.window(handles[-1])
            time.sleep(3)
            PageStatique = BeautifulSoup(Page.page_source, "html.parser")
            if re.search(r"0 Résultat trouvé", PageStatique.text) is not None:
                ## Page not found ==> no more uniques to scrap
                print("No result")
                Page.close()
                Page.switch_to.window(handles[0])
                break
            else:
                ## Get all prices
                PrixPage = [float(x.text.replace("\xa0", "").replace("À partir de ", "").replace("€", "").replace(",", ".")) for x in PageStatique.find_all("p", class_ = "w-fit")]
                ## Page found: find the adresses for the uniques
                xpath_expr = "//button[contains(., \"" + carte + "\")]"
                try:
                    btn = WebDriverWait(Page, 10).until(
                        EC.element_to_be_clickable((By.XPATH, xpath_expr))
                    )
                except TimeoutException:
                    print(f"\nNo card named {carte} found in the page. Closing this thread...")
                    Page.close()
                    Page.switch_to.window(handles[0])
                    break
                btn.click()
                time.sleep(.5)
                IndexPrix = 0
                ## Find the right arrow button
                xpath_expr = "//button[i[contains(@class, 'fa-arrow-right-long')]]"
                btn = Page.find_element(webdriver.common.by.By.XPATH, xpath_expr)
                PageCarte = BeautifulSoup(Page.page_source, "html.parser")
                ListeCartes.append(re.search(r"^.*cards/(.+)$", PageCarte.find("a", string = " Accéder au détail").get("href")).group(1))
                ListePrix.append(PrixPage[IndexPrix])
                IndexPrix += 1
                while btn.get_attribute("disabled") is None:
                    time.sleep(.5)
                    btn.click()
                    PageCarte = BeautifulSoup(Page.page_source, "html.parser")
                    ListeCartes.append(re.search(r"^.*cards/(.+)$", PageCarte.find("a", string = " Accéder au détail").get("href")).group(1))
                    ListePrix.append(PrixPage[IndexPrix])
                    IndexPrix += 1  
                    btn = Page.find_element(webdriver.common.by.By.XPATH, xpath_expr)
                ## End of that page
                Page.close()
                Page.switch_to.window(handles[0])  
        print(" ")

    TotalCartes = len(ListeCartes)
    print(f"{TotalCartes} unique cards to scrap.\nStarting now...")

    ## Retrieve the informations for each card
    for i, (carte, prix) in enumerate(zip(ListeCartes, ListePrix), start = 1):
        if i % 25 == 0: print(f"Card n°{i}/{TotalCartes}")
        Page.execute_script(f"window.open('{site}/{carte}');")
        handles = Page.window_handles
        Page.switch_to.window(handles[-1])
        time.sleep(1)
        PageStatique = BeautifulSoup(Page.page_source, "html.parser")
        TexteCarte = Page.find_element(webdriver.common.by.By.XPATH, "//div[contains(@class, 'rounded-sm') and contains(@class, 'bg-sand-100') and contains(@class, 'group')]")
        TexteCarte = MiseEnForme(TexteCarte.get_attribute("innerHTML"))
        TexteCarte = TexteCarte.replace("\n", " ")
        if re.search("Effet principal", TexteCarte):
            Regex = r"^(.+)(Type.+)(Sous-type.*)(Effet principal.*)$"
        elif re.search("Effet de soutien", TexteCarte):
            Regex = r"^(.+)(Type.+)(Sous-type.*)(Effet de soutien.*)$"
        else:
            Regex = r"^(.+)(Type.+)(Sous-type.*)()$"
        Processed = re.search(Regex, TexteCarte)
        NomCarte = Processed.group(1)
        TypeCarte = re.sub(r"^Type", "", Processed.group(2))
        SoustypeCarte = re.sub(r"^Sous-type", "", Processed.group(3))
        EffetsCarte = re.sub(r"^Effet principal", "", Processed.group(4))
        AttributsCarte = re.search(r"^.*Attributs([0-9 ]+).*$", SoustypeCarte).group(1)
        SoustypeCarte = re.search(r"^(.*)Attributs.+$", SoustypeCarte).group(1)
        CoutMain, CoutReserve, Foret, Montagne, Lac = AttributsCarte.strip().split(" ")
        ## Update dictionary to make the dataframe
        Donnees["Nom"].append(NomCarte.strip(" ,"))
        Donnees["Faction"].append(Arguments.faction)
        Donnees["Type"].append(TypeCarte.strip(" ,"))
        Donnees["SubType"].append(SoustypeCarte.strip(" ,"))
        Donnees["HandCost"].append(CoutMain.strip(" ,"))
        Donnees["ReserveCost"].append(CoutReserve.strip(" ,"))
        Donnees["Forest"].append(Foret.strip(" ,"))
        Donnees["Mountain"].append(Montagne.strip(" ,"))
        Donnees["Lake"].append(Lac.strip(" ,"))
        Donnees["Effect"].append(EffetsCarte.strip(" ,"))
        Donnees["Price"].append(prix)
        Donnees["Reference"].append(carte)
        Page.close()
        Page.switch_to.window(handles[0])

    # Export of database

    df = pd.DataFrame(Donnees)
    DonneesCartes = DonneesCartes[["Nom", "HandCost", "ReserveCost", "Forest", "Mountain", "Lake"]]
    DonneesCartes = DonneesCartes.rename(columns = {"Nom": "Nom", "HandCost": "DiffHandCost", "ReserveCost": "DiffReserveCost", "Forest": "DiffForest", "Mountain": "DiffMountain", "Lake": "DiffLake"})
    df = pd.merge(df, DonneesCartes, on = "Nom", how = "left")
    for colonne in ["HandCost", "ReserveCost", "Forest", "Mountain", "Lake"]:
        df["Diff" + colonne] = df[colonne].astype(float) - df["Diff" + colonne]
    df.to_excel("./Altered/Uniques/" + Arguments.radical + "_" + Arguments.faction + "_" + datetime.today().strftime("%Y%m%d") + ".xlsx", index = False, engine = "openpyxl")

# End of the program

Fin = time.time()
print(f"End of programm. Time spent: {round((Fin - Debut) / 60, 1)} minutes !")
