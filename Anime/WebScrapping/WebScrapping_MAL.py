###########################################
#   Data Feast                            #
#   analisisyvisualziacion@gmail.com      #
#   Web scrapping My Anime List           #
###########################################

# Packages
from bs4 import BeautifulSoup
from tqdm import tqdm
import sys
import time
import requests
import pandas as pd

# Extracting links
num_page = 1
status = 202
anime_links = []
genre = "Madhouse"
while status != 404:
    url_page = "https://myanimelist.net/anime/producer/11/Madhouse?page=" + \
        str(num_page)
    page = requests.get(url_page)
    status = page.status_code
    num_page += 1
    print(f"This link {url_page} has a {status} status code")
    if status == 404:
        break
    else:
        soup = BeautifulSoup(page.content, "html.parser")
        links = soup.find_all("a", attrs={'class': 'link-title'})

        for link in links:
            anime_links.append(link['href'])


# Working for each anime link
# Anime Info
title_english = []
title_japanese = []
episodes = []
source = []
ratings = []
season = []
type = []
studio = []
# Anime Statistics
score = []
scored_by = []
ranked = []
popularity = []
members = []
genres = []

for i in tqdm(range(len(anime_links))):
    time.sleep(1)
    #print(f"Extrancting Anime Data from: {anime_links[i]}")
    page = requests.get(anime_links[i])
    soup = BeautifulSoup(page.content, "html.parser")
    #
    anime_details = soup.find(
        "div", attrs={'class': 'anime-detail-header-stats'})
    anime_card = soup.find("tr")
    anime_info = anime_card.find("td")
    # Extract anime info
    try:
        anime_english = anime_info.find(
            "span", text="English:").next_sibling.strip()
    except AttributeError as error:
        anime_english = "-"
    try:
        anime_japanese = anime_info.find(
            "span", text="Japanese:").next_sibling.strip()
    except AttributeError as error:
        anime_japanese = "-"
    try:
        anime_episodes = anime_info.find(
            "span", text="Episodes:").next_sibling.strip()
    except AttributeError as error:
        anime_episodes = "-"
    try:
        anime_source = anime_info.find(
            "span", text="Source:").next_sibling.strip()
    except AttributeError as error:
        anime_source = "-"
    try:
        anime_ratings = anime_info.find(
            "span", text="Rating:").next_sibling.strip()
    except AttributeError as error:
        anime_ratings = "-"
    try:
        anime_info_genres = anime_info.find_all(
            "span", attrs={'itemprop': 'genre'})
        anime_genres = ";".join([g.text for g in anime_info_genres])
    except AttributeError as error:
        anime_genres = "-"
    # Anime statistics
    try:
        anime_score = anime_details.find(
            "div", attrs={"class": "score-label"}).text
    except AttributeError as error:
        anime_score = "-"
    try:
        anime_scoreby = anime_details.find("div", attrs={'class': 'score'})[
            'data-user'].replace("users", "").replace(",", "")
    except AttributeError as error:
        anime_scoreby = "-"
    try:
        anime_rank = anime_details.find(
            "span", attrs={'class': 'numbers ranked'}).text
    except AttributeError as error:
        anime_rank = "-"
    try:
        anime_popular = anime_details.find(
            "span", attrs={'class': 'numbers popularity'}).text
    except AttributeError as error:
        anime_popular = "-"
    try:
        anime_members = anime_details.find("span",  attrs={
            'class': 'numbers members'}).text.replace("Members ", "").replace(",", "")
    except AttributeError as error:
        anime_members = "-"
    try:
        anime_season = anime_details.find(
            "span", attrs={'class': 'information season'}).text
    except AttributeError:
        anime_season = "-"
    try:
        anime_type = anime_details.find(
            "span", attrs={'class': 'information type'}).text
    except AttributeError as error:
        anime_type = "-"
    try:
        anime_studio = anime_details.find(
            "span", attrs={'class': 'information studio author'}).text
    except AttributeError as error:
        anime_studio = "-"

    # Append information
    title_english.append(anime_english)
    title_japanese.append(anime_japanese)
    episodes.append(anime_episodes)
    source.append(anime_source)
    ratings.append(anime_ratings)
    season.append(anime_season)
    type.append(anime_type)
    studio.append(anime_studio)
    score.append(anime_score)
    scored_by.append(anime_scoreby)
    ranked.append(anime_rank)
    popularity.append(anime_popular)
    members.append(anime_members)
    genres.append(anime_genres)

# Making a Dataset

data = pd.DataFrame(data={
    "Title_english": title_english,
    "Title_japanese": title_japanese,
    "Anime_episodes": episodes,
    "Anime_source": source,
    "Ratings": ratings,
    "Season": season,
    "Type": type,
    "Studio": studio,
    "score": score,
    "scored_by": scored_by,
    "ranked": ranked,
    "popularity": popularity,
    "members": members,
    "links": anime_links,
    "Genres": genres,
    "genre": genre
})

data.to_csv(f"dataset_anime_{genre}.csv", index=False)
