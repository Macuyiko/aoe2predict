import dataset
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin
import re

# This script scrapes information of past matches from Voobly

# Set the database location and your Voobly cookie information below:

db = dataset.connect('sqlite:///voobly.db')

storage = './recordings/'
session = requests.Session()
session.cookies.update({
    '__cfduid': '',
    'vbly_GMT_bias': '',
    'vbly_test': '',
    'vbly_session7': '',
    'vbly_username': '',
    'vbly_session1': '',
    'vbly_password': ''
    })

# ----------------------------------------------------------

url = 'http://www.voobly.com/match/view/{match}'
player_url = 'http://www.voobly.com/profile/view/{player}/Matches/games/matches/user/{player}/0/{page}'

game_name = 'Age of Empires II: The Conquerors'

def get_row(table, name, toint=False):
    value = table.find('td', text=name).find_next_sibling('td').text.strip()
    return value if not toint else int(value)

def scrape_player_page(player_id, page):
    purl = player_url.format(player=player_id, page=page)
    r = session.get(purl)
    soup = BeautifulSoup(r.text, 'html5lib')
    matches = []
    for row in soup.find_all('tr'):
        tds = row.find_all('td')
        if not tds or len(tds) != 7:
            continue
        if game_name not in tds[1].text:
            continue
        matches.append(int(tds[5].text.replace('#', '').strip()))
    return matches

def scrape_match(match_id):
    murl = url.format(match=match_id)
    r = session.get(murl)
    soup = BeautifulSoup(r.text, 'html5lib')
    if game_name not in soup.find('h3').text:
    	return None
    table = soup.find('td', text='Match Details').find_parent('table')
    details = {'match_id': match_id}
    details['match_players'] = get_row(table, 'Players:', True)
    # Make sure this is a 1v1 match
    if details['match_players'] != 2:
        return None
    details['match_rating'] = get_row(table, 'Match Rating:', True)
    details['match_map'] = get_row(table, 'Map:')
    details['match_duration'] = get_row(table, 'Duration:')
    details['match_mod'] = get_row(table, 'Game Mod:')
    results = [span for span in soup.find_all('span') if 'New Rating' in span.text]
    for index in range(len(results)):
        td = results[index].find_parent('td')
        profile = td.find('a', href=re.compile('/profile/view'))
        prefix = 'match_player' + str(index + 1)
        details[prefix+'_id'] = int(re.search('/(\d+)', profile.get('href')).group(1))
        details[prefix+'_name'] = profile.text.strip()
        details[prefix+'_civid'] = td.find_parent('table').find('img', src=re.compile('/res/games/AOC/civs')).get('src')
        points = int(re.search('Points: (-?\d+)', td.text).group(1))
        new = int(re.search('New Rating: (-?\d+)', td.text).group(1))
        details[prefix+'_rating_after'] = new
        details[prefix+'_rating_before'] = new - points
        if points > 0:
            details['match_winner'] = index + 1
    recordings = [a for a in soup.find_all('a') if 'Download Rec.' in a.text]
    for index in range(len(recordings)):
        href = recordings[index].get('href')
        name = recordings[index].find('b').text.strip()
        print (name,href)
        fr = session.get(urljoin(url, href))
        filename = '{}_{}.zip'.format(match_id, name)
        with open(storage + filename, 'wb') as f:
            f.write(fr.content)
        details['recording'+str(index)] = filename
    return details

def scrape_downwards(match, amount=1000):
    while amount > 0:
        print('----------',match,'------------')
        try:
            details = scrape_match(match)
        except Exception as e:
            print(e)
            details = None
        if details:
            db['matches'].upsert(details, ['match_id'])
            print(details)
        match -= 1
        amount -= 1

def scrape_player(player_id, amount=10):
    page = 0
    matches = []
    while amount > 0:
        while matches:
            match = matches.pop()
            print('----------',match,'------------')
            try:
                details = scrape_match(match)
            except Exception as e:
                print(e)
                details = None
            if details:
                db['matches'].upsert(details, ['match_id'])
                print(details)
            amount -= 1
        matches = scrape_player_page(player_id, page)
        if not matches:
            break
        page += 1

#scrape_player(123211439)
scrape_player(123397463)
scrape_player(123211439)
scrape_player(123999216)
scrape_player(124474647)
scrape_player(124147336)
scrape_player(123497926)
