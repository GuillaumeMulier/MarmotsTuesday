from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.common.exceptions import TimeoutException, WebDriverException
import time
import re
import pandas as pd
from itertools import product
from argparse import ArgumentParser

# Scraping des cartes ou scraping des cartes possédées en plus
parser = ArgumentParser()
parser.add_argument("-personal", action = "store_true", help = "Connexion à son compte perso Altered")
args = parser.parse_args()

Debut = time.time()
site = "https://www.altered.gg/fr-fr/cards"

def AccederPage(url, compte_perso=False):
    Erreur = False
    try:
        OptionsNav = webdriver.chrome.options.Options() # Retirer les logs d'erreurs non fatales du navigateur
        OptionsNav.add_argument("--log-level=3")
        driver = webdriver.Chrome(options = OptionsNav)
        driver.set_page_load_timeout(10)  # Timeout après 10 secondes
        driver.get(url)
    except TimeoutException:
        Erreur = True
        print("Erreur : Le chargement a pris trop de temps.")
    except WebDriverException as e:
        Erreur = True
        print(f"Erreur Selenium : {e}")
    if not Erreur:
        time.sleep(2)
        RefuserCookies = driver.find_element(webdriver.common.by.By.ID, "didomi-notice-disagree-button")
        RefuserCookies.click()
        # Set resolution
        driver.set_window_size(1500, 800)
        if compte_perso: # Il faut que le compte soit connecté
            ReponseInput = ""
            BoutonConnect = driver.find_element(webdriver.common.by.By.XPATH, "//div[contains(@class, 'fixed') and contains(@class, 'top-2') and contains(@class, 'right-2')]")
            if BoutonConnect.text in ["SIGN IN", "SE CONNECTER"]:
                while ReponseInput != "OK":
                    ReponseInput = input("Connecte-toi à ton compte Altered sur la page ouverte, puis tape 'OK' en majuscule et valide.\n")
            else:
                while ReponseInput != "OK":
                    ReponseInput = input("Si ton compte est bien le compte " + BoutonConnect.text + " tape 'OK' en majuscule et valide. Sinon. connecte-toi à ton compte Altered sur la page ouverte, puis tape 'OK' en majuscule et valide.\n")
            driver.refresh()
            time.sleep(2)
            BoutonConnect = driver.find_element(webdriver.common.by.By.XPATH, "//div[contains(@class, 'fixed') and contains(@class, 'top-2') and contains(@class, 'right-2')]")
            print(f"Page chargée avec succès sur le compte {BoutonConnect.text}.")
        else:
            print(f"Page chargée avec succès")
        return (True, driver)
    else:
        return(False)

def MiseEnForme(StringForme):
    # Compléter les effets de réserve, de main, flèche
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
    return StringForme.get_text(separator = "\n", strip = True)

# Accéder au site (reselenium requis car génération de la page par JS)
Accessible, Page = AccederPage(site, args.personal)

if Accessible: # Le site est accessible

    Donnees = {
        "Nom": [],
        "Rarity": [],
        "Faction": [],
        "Edition": [],
        "Type": [],
        "SubType": [],
        "HandCost": [],
        "ReserveCost": [],
        "Forest": [],
        "Mountain": [],
        "Lake": [],
        "Effect": [],
        "Nb possédé (digital)": []
    }
    DicoFactions = {
        "ly": "Lyra",
        "mu": "Muna",
        "ax": "Axiom",
        "yz": "Yzmir",
        "br": "Bravos",
        "or": "Ordis"
    }

    Editions = ["CORE", "COREKS", "ALIZE", "BISE"]
    AltArt = ["A", "B"]
    FactionCarte = ["AX", "BR", "LY", "MU", "OR", "YZ"]
    NumCarte = [str(x + 1) if x > 8 else "0" + str(x + 1) for x in range(100)]

    NbACheck = len(Editions) * len(AltArt) * len(FactionCarte) * len(NumCarte)
    print(f"Potentiellement {NbACheck} cartes à chercher...")

    # On reconstruit les différentes adresses de cartes et on itère dessus
    for edi, art, fac, numero in product(Editions, AltArt, FactionCarte, NumCarte):
        # On va chercher les rares etc.
        for suf in ["C", "R1", "R2"]:
            Page.execute_script(f"window.open('{site}/ALT_{edi}_{art}_{fac}_{numero}_{suf}');")
            handles = Page.window_handles
            Page.switch_to.window(handles[-1])
            time.sleep(2)
            PageStatique = BeautifulSoup(Page.page_source, "html.parser")
            Titre = PageStatique.find("h1")
            if Titre.text == "Oups":
                # On ne trouve pas la carte, donc pas la peine de se faire tous les suffixes
                print(f"Carte non existante : ALT_{edi}_{art}_{fac}_{numero}_{suf}.")
                Page.close()
                Page.switch_to.window(handles[0])
                break
            else:
                print(f"Carte trouvée : ALT_{edi}_{art}_{fac}_{numero}_{suf} !")
                Faction = Page.find_element(webdriver.common.by.By.CSS_SELECTOR, ".fa-kit").get_attribute("class")
                Faction = re.sub(r"^fa-kit fa-(..).*$", r"\1", Faction)
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
                if suf == "C":
                    RareteCarte = "Commune"
                else:
                    RareteCarte = "Rare"
                TexteComplementaire = Page.find_elements(webdriver.common.by.By.XPATH, "//div[contains(@class, 'rounded-sm') and contains(@class, 'bg-sand-100') and contains(@class, 'group')]")[1].text
                if TypeCarte.strip() == "Héros":
                    CoutMain, CoutReserve, Foret, Montagne, Lac, Edition = "      "
                    NbDigi = "5"
                elif re.search("Jeton Personnage", TypeCarte.strip()):
                    AttributsCarte = re.search(r"^.*Attributs([0-9 ]+).*$", SoustypeCarte).group(1)
                    SoustypeCarte = re.search(r"^(.*)Attributs.+$", SoustypeCarte).group(1)
                    CoutMain, CoutReserve, Foret, Montagne, Lac = AttributsCarte.strip().split(" ")
                    Edition = " "
                    NbDigi = "5"                        
                elif re.search("Personnage", TypeCarte.strip()):
                    AttributsCarte = re.search(r"^.*Attributs([0-9 ]+).*$", SoustypeCarte).group(1)
                    SoustypeCarte = re.search(r"^(.*)Attributs.+$", SoustypeCarte).group(1)
                    CoutMain, CoutReserve, Foret, Montagne, Lac = AttributsCarte.strip().split(" ")
                    if re.search(r"\n([0-9]+)(/*[0-9]*)\n(DANS TA COLLECTION|CARTE\(S\) EXCLUSIVE\(S\) DANS MA COLLECTION)", TexteComplementaire):
                        Edition = re.search(r"ÉDITION\n(.+?)\n", TexteComplementaire).group(1)
                        NbDigi = re.search(r"\n([0-9]+)(/*[0-9]*)\n(DANS TA COLLECTION|CARTE\(S\) EXCLUSIVE\(S\) DANS MA COLLECTION)", TexteComplementaire).group(1)
                    else:
                        Edition = " "
                        NbDigi = "0"
                else:
                    AttributsCarte = re.search(r"^.*Attributs([0-9 ]+).*$", SoustypeCarte).group(1)
                    SoustypeCarte = re.search(r"^(.*)Attributs.+$", SoustypeCarte).group(1)
                    CoutMain, CoutReserve = AttributsCarte.strip().split(" ")
                    Foret, Montagne, Lac = "   "
                    if re.search(r"\n([0-9]+)(/*[0-9]*)\n(DANS TA COLLECTION|CARTE\(S\) EXCLUSIVE\(S\) DANS MA COLLECTION)", TexteComplementaire):
                        Edition = re.search(r"ÉDITION\n(.+?)\n", TexteComplementaire).group(1)
                        NbDigi = re.search(r"\n([0-9]+)(/*[0-9]*)\n(DANS TA COLLECTION|CARTE\(S\) EXCLUSIVE\(S\) DANS MA COLLECTION)", TexteComplementaire).group(1)
                    else:
                        Edition = " "
                        NbDigi = "0"
                Donnees["Nom"].append(NomCarte.strip(" ,"))
                Donnees["Faction"].append(DicoFactions[Faction])
                Donnees["Rarity"].append(RareteCarte.strip(" ,"))
                Donnees["Type"].append(TypeCarte.strip(" ,"))
                Donnees["SubType"].append(SoustypeCarte.strip(" ,"))
                Donnees["HandCost"].append(CoutMain.strip(" ,"))
                Donnees["ReserveCost"].append(CoutReserve.strip(" ,"))
                Donnees["Forest"].append(Foret.strip(" ,"))
                Donnees["Mountain"].append(Montagne.strip(" ,"))
                Donnees["Lake"].append(Lac.strip(" ,"))
                Donnees["Effect"].append(EffetsCarte.strip(" ,"))
                Donnees["Edition"].append(Edition.strip(" ,"))
                Donnees["Nb possédé (digital)"].append(int(NbDigi.strip(" ,")))
                Page.close()
                Page.switch_to.window(handles[0])
                
    Page.quit()

    # Export sous Excel
    print(f"Export sous Excel des résultats.")
    df = pd.DataFrame(Donnees)
    ColonnesPremier = ["Type", "SubType", "HandCost", "ReserveCost", "Forest", "Mountain", "Lake", "Effect"]
    DictOperations = {"Nb possédé (digital)": "sum"}
    DictOperations.update({col: "first" for col in ColonnesPremier})
    df["Edition"] = df["Edition"].str.replace(" - Édition KS", "")
    df = df.groupby(['Nom', 'Rarity', 'Faction', 'Edition']).agg(DictOperations).reset_index()
    df.to_excel("C:/Users/DRY12/Documents/Github/MarmotsTuesday/Altered/liste_cartes_20250617.xlsx", index=False, engine='openpyxl')

Fin = time.time()
print(f"Fin du programme en {round((Fin - Debut) / 60, 1)} minutes !")





