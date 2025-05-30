from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.common.exceptions import TimeoutException, WebDriverException
import time
import re
import pandas as pd

Debut = time.time()
site = "https://www.altered.gg/cards"

def AccederPage(url):
    Erreur = False
    try:
        driver = webdriver.Chrome()
        driver.set_page_load_timeout(10)  # Timeout après 10 secondes
        driver.get(url)
        print(f"Page chargée avec succès")
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
        return (True, driver)
    else:
        return(False)

# Accéder au site (reselenium requis car génération de la page par JS)
Accessible, Page = AccederPage(site)

if Accessible: # Le site est accessible

    # Set resolution
    Page.set_window_size(1500, 800)

    # Nombre de pages à scraper
    SoupeSite = BeautifulSoup(Page.page_source, "html.parser")
    MainElement = SoupeSite.find("main")
    NbPages = 0
    for l in MainElement.find_all("ul"):
        RecherchePages = re.search(r"(\d+)Next", l.text)
        if RecherchePages:
            NbPages = int(RecherchePages.group(1))

    # On a trouvé le nombre de pages à scrapper donc on peut les faire une par une
    if NbPages == 0:
        print("Pas de nombre de page trouvées, vérifier que la page s'ouvre bien en petit avec 1/x affiché en nombre de pages.")
    else:
        print(f"{NbPages} pages trouvées, scrapping de ces pages en cours.")
        Images = []

        for n in range(NbPages):
            print(f"Scraping de la page n°{n + 1}...")

            # Charger la page
            Page.execute_script("window.open('" + site + "?page=" + str(n + 1) + "');")
            handles = Page.window_handles
            Page.switch_to.window(handles[-1])
            time.sleep(2)

            # Trouver les cartes de la page
            SoupeSite = BeautifulSoup(Page.page_source, "html.parser")
            Grid = SoupeSite.find_all("img")

            # Ajouter dans la liste de cartes à visiter
            for it in Grid:
                if re.search("CARD", it["src"]):
                    Images.append(re.search(r"CARDS/(ALT[^/]*)/JPG", it["src"]).group(1))

            # On a toutes cartes donc on peut fermer l'onglet
            Page.close()
            Page.switch_to.window(handles[0])

        Images = set(Images)
        Donnees = {
            "Nom": [],
            "Rarity": [],
            "Faction":[],
            "Type": [],
            "SubType": [],
            "HandCost": [],
            "ReserveCost": [],
            "Forest": [],
            "Mountain": [],
            "Lake": [],
            "Effect": []
        }
        DicoFactions = {
            "ly": "Lyra",
            "mu": "Muna",
            "ax": "Axiom",
            "yz": "Yzmir",
            "br": "Bravos",
            "or": "Ordis"
        }
        print(f"{len(Images) * 3} potentielles cartes trouvées.")

        for Carte in Images:
            if re.search("_NE_", Carte):
                print(f"Carte générique sans faction : {Carte} !")
            else :
                for suf in ["_C", "_R1", "_R2"]:
                    Page.execute_script("window.open('" + site + "/" + Carte + suf + "');")
                    handles = Page.window_handles
                    Page.switch_to.window(handles[-1])
                    time.sleep(2)
                    PageStatique = BeautifulSoup(Page.page_source, "html.parser")
                    Titre = PageStatique.find("h1")
                    if Titre.text == "Oops":
                        print(f"Carte non existante : {Carte + suf}.")
                    else:
                        print(f"Carte trouvée : {Carte + suf} !")
                        Faction = Page.find_element(webdriver.common.by.By.CSS_SELECTOR, ".fa-kit").get_attribute("class")
                        Faction = re.sub(r"^fa-kit fa-(..).*$", r"\1", Faction)
                        TexteCarte = Page.find_elements(webdriver.common.by.By.TAG_NAME, "div")[25].text
                        TexteCarte = TexteCarte.replace("\n", "")
                        if re.search("MAIN EFFECT", TexteCarte):
                            Regex = r"^(.+)(TYPE.+)(SUBTYPE.*)(MAIN EFFECT.*)$"
                        elif re.search("Discard me from Reserve to do this", TexteCarte):
                            Regex = r"^(.+)(TYPE.+)(SUBTYPE.*)(:.*)$"
                        else:
                            Regex = r"^(.+)(TYPE.+)(SUBTYPE.*)()$"
                        Processed = re.search(Regex, TexteCarte)
                        NomCarte = Processed.group(1)
                        TypeCarte = re.sub(r"^TYPE", "", Processed.group(2))
                        SoustypeCarte = re.sub(r"^SUBTYPE", "", Processed.group(3))
                        EffetsCarte = re.sub(r"^MAIN EFFECT", "", Processed.group(4))
                        if suf == "_C":
                            RareteCarte = "Commune"
                        else:
                            RareteCarte = "Rare"
                        if TypeCarte == "Hero":
                            CoutMain, CoutReserve, Foret, Montagne, Lac = "     "
                        elif re.search("Character", TypeCarte):
                            AttributsCarte = re.search(r"^.*ATTRIBUTES(.+)$", SoustypeCarte).group(1)
                            SoustypeCarte = re.search(r"^(.*)ATTRIBUTES.+$", SoustypeCarte).group(1)
                            CoutMain, CoutReserve, Foret, Montagne, Lac = AttributsCarte
                        else:
                            AttributsCarte = re.search(r"^.*ATTRIBUTES(.+)$", SoustypeCarte).group(1)
                            SoustypeCarte = re.search(r"^(.*)ATTRIBUTES.+$", SoustypeCarte).group(1)
                            CoutMain, CoutReserve = AttributsCarte
                            Foret, Montagne, Lac = "   "
                        EffetsCarte = re.sub(r"(:.* \(Discard me from Reserve to do this.\))",
                                             r"\nSoutien\1", EffetsCarte)
                        Donnees["Nom"].append(NomCarte)
                        Donnees["Faction"].append(DicoFactions[Faction])
                        Donnees["Rarity"].append(RareteCarte)
                        Donnees["Type"].append(TypeCarte)
                        Donnees["SubType"].append(SoustypeCarte)
                        Donnees["HandCost"].append(CoutMain)
                        Donnees["ReserveCost"].append(CoutReserve)
                        Donnees["Forest"].append(Foret)
                        Donnees["Mountain"].append(Montagne)
                        Donnees["Lake"].append(Lac)
                        Donnees["Effect"].append(EffetsCarte)
                    Page.close()
                    Page.switch_to.window(handles[0])

    Page.quit()

    # Export sous Excel
    print(f"Export sous Excel des résultats.")
    df = pd.DataFrame(Donnees)
    df = df.drop_duplicates()
    df.to_excel("C:/Users/DRY12/Documents/Github/MarmotsTuesday/Altered/liste_cartes.xlsx", index=False, engine='openpyxl')

Fin = time.time()
print(f"Fin du programme en {round((Fin - Debut) / 60, 1)} minutes !")


