from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.common.exceptions import TimeoutException, WebDriverException
import time
import re
import pandas as pd

Debut = time.time()
site = "https://39cards.com"
LimiteTop = {32: 4, 64: 8, 256: 16, 1024: 32}
DateMin = pd.to_datetime("2025-01-31")
DateMax = pd.to_datetime("2025-06-01")

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
        return (True, driver)
    else:
        return(False)

def convertir_date_fr_en(date_str):
    mois_fr_en = {
        "janvier": "January", "février": "February", "mars": "March",
        "avril": "April", "mai": "May", "juin": "June",
        "juillet": "July", "août": "August", "septembre": "September",
        "octobre": "October", "novembre": "November", "décembre": "December"
    }
    for fr, en in mois_fr_en.items():
        if fr in date_str.lower():
            return date_str.lower().replace(fr, en).capitalize()
    return date_str

def TrouverFaction(Hero):
    if Hero in ["Sigismar & Wingspan", "Waru & Mack", "Gulrang & Tocsin"]:
        return "Ordis"
    elif Hero in ["Akesha & Taru", "Lindiwe & Maw", "Afanas & Senka"]:
        return "Yzmir"
    elif Hero in ["Nevenka & Blotch", "Auraq & Kibble", "Fen & Crowbar"]:
        return "Lyra"
    elif Hero in ["Teija & Nauraa", "Arjun & Spike", "Rin & Orchid"]:
        return "Muna"
    elif Hero in ["Sierra & Oddball", "Treyst & Rossum", "Subhash & Marmo"]:
        return "Axiom"
    elif Hero in ["Kojo & Booda", "Atsadi & Surge", "Basira & Kaizaimon"]:
        return "Bravos"
    else:
        return "Inconnu"

# Accéder au site (reselenium requis car génération de la page par JS)
Accessible, Page = AccederPage(site)

if Accessible:

    # Set resolution
    Page.set_window_size(1500, 800)

    # Nombre de pages à scraper
    print(f"Scrapping du tableau...")
    SoupeSite = BeautifulSoup(Page.page_source, "html.parser")
    TableauDonnees = SoupeSite.find("table")
    Lignes = TableauDonnees.find_all("tr")
    DF = []
    for row in Lignes:
        cells = row.find_all(["td", "th"])
        ligne = []
        lien = ""
        for cell in cells:
            a_tag = cell.find("a")
            if a_tag and a_tag.has_attr("href"):
                lien = a_tag["href"]
            ligne.append(cell.get_text(strip=True))
        ligne.append(lien)
        DF.append(ligne)
    DF[0][4] = "Lien"
    DF = pd.DataFrame(DF[1:], columns = DF[0])

    # Mise en forme des colonnes
    DF["Date"] = DF["Date"].apply(lambda x: re.sub(r"^(\d+.*?)(\d+.*)$", r"\2", x))
    DF["Date"] = DF["Date"].apply(convertir_date_fr_en)
    DF["Date"] = pd.to_datetime(DF["Date"], format = "%d %B %Y", dayfirst = True)
    print(f"{DF.shape[0]} lignes récupérées.")

    # On filtre avec les dates de tournoi désirées
    DF = DF[(DF["Date"] >= DateMin) & (DF["Date"] <= DateMax)]
    print(f"Après filtre, il nous reste {DF.shape[0]} lignes correspondant à des tournois entre le {DateMin.strftime('%d-%m-%Y')} et le {DateMax.strftime('%d-%m-%Y')} !")

    # Début du tableau de stockage des différentes cartes
    ListeCartes = []

    # On boucle sur les tournois pour récupérer les decks
    for index, ligne in DF.iterrows():
        # On ouvre la page du tournoi
        Page.execute_script("window.open('" + site + ligne["Lien"] + "');")
        handles = Page.window_handles
        Page.switch_to.window(handles[-1])
        time.sleep(1)
        PageStatique = BeautifulSoup(Page.page_source, "html.parser")

        # Extraction du tableau
        if re.search(r"^NUC", ligne["Event"]) == None:
            TypeTournoi = "No unique"
        else:
            TypeTournoi = "Classique"
        TableauTournoi = PageStatique.find("table")
        LignesTournoi = TableauTournoi.find_all("tr")
        NbARecup = LimiteTop[sorted(k for k in LimiteTop if k > int(ligne["PlayersPl."]))[0]]
        if NbARecup > (len(LignesTournoi) - 1): NbARecup = len(LignesTournoi) - 1
        print(f"Tournoi n°{index + 1} : {ligne['Event']}. {ligne['PlayersPl.']} participants donc on cherche le top {NbARecup} !")

        for lig in range(NbARecup):
            # Infos générales pour le joueur
            Hero = LignesTournoi[lig + 1].find("span", class_ = "desktop-only").get_text()
            Place = lig + 1
            # On va chercher son deck
            Page.execute_script("window.open('" + site + LignesTournoi[lig + 1].find("a")["href"] + "');")
            handles = Page.window_handles
            Page.switch_to.window(handles[-1])
            time.sleep(1)
            PageJoueur = BeautifulSoup(Page.page_source, "html.parser")

            for carte in PageJoueur.find_all("div", attrs = {"data-card-id": True}):
                Spans = carte.find_all("span")
                if "bg-rarityRare/15" in carte.get("class"):
                    Rarete = "Rare"
                elif "bg-rarityUnique/10" in carte.get("class"):
                    Rarete = "Unique"
                else:
                    Rarete = "Commune"
                if Rarete == "Commune" :
                    Manas = Spans[4].find_all("i")
                    Carte = Spans[3].get_text()
                else:
                    Manas = Spans[5].find_all("i")
                    Carte = Spans[4].get_text()
                DonneesCartes = {
                    "Tournoi": ligne["Event"],
                    "TypeTournoi": TypeTournoi,
                    "NbJoueurs": ligne["PlayersPl."],
                    "Place": Place,
                    "Joueur": LignesTournoi[lig + 1].find_all("td")[2].get_text(),
                    "Hero": Hero,
                    "Faction": TrouverFaction(Hero),
                    "Carte": Carte,
                    "Rarete": Rarete,
                    "Exemplaires": Spans[0].get_text(),
                    "CoutMain": re.sub(r"^mana-(\d+)$", r"\1", Manas[0].get("class")[1]),
                    "CoutReserve": re.sub(r"^mana-(\d+)$", r"\1", Manas[1].get("class")[1])
                }
                ListeCartes.append(DonneesCartes)

            Page.close()
            handles = Page.window_handles
            Page.switch_to.window(handles[-1])

        Page.close()
        Page.switch_to.window(handles[0])

    # Construction de la base de données
    DF = pd.DataFrame(ListeCartes)
    DF.to_excel("C:/Users/DRY12/Documents/Github/MarmotsTuesday/Altered/scraping_tournois.xlsx", index = False)


Fin = time.time()
print(f"Fin du programme en {round((Fin - Debut) / 60, 1)} minutes !")