import dataset
import zipfile
import os, shutil
import pyautogui
import time
import ctypes
from pywinauto import Desktop
from pywinauto.findwindows import find_elements
import pyautogui
import win32gui
import websocket
import subprocess
import sys
import os
from glob import glob

# This script plays recorded games through Voobly, make sure a Voonly game window is open

# Set / check the following fields, you'll have to enter your Voobly username in "game_lobby"

database = 'sqlite:///voobly.db'
game_title = '^Age of Empires II Expansion'
game_lobby = '^Game \d+: Macuyiko\'s Game'
game_settings = '^Game Settings'
game_messenger = '^Voobly Messenger'
game_browser = '^Game Browser'
game_mainlobby = "^New Player Lobby \(Age of Empires II: The Conquerors\)"

waitforinput = False
targets = {
	'v1.5 Beta R6 | WololoKingdoms': 'D:\\Age of Empires II\\Voobly Mods\\AOC\\Data Mods\\WololoKingdoms\\Savegame',
	'v1.5 Beta R5 | WololoKingdoms': 'D:\\Age of Empires II\\Voobly Mods\\AOC\\Data Mods\\WololoKingdoms\\Savegame',
	'v1.5 Beta R5': 'D:\\Age of Empires II\\SaveGame',
	'1.4 RC': 'D:\\Age of Empires II\\SaveGame',
	}
versions_y = {'1.0' : 20, '1.4' : 35, '1.5 Beta R5' : 50, '1.5 Beta R6' : 65}
mods_y = {'Allied' : 20, 'WololoKingdoms AK' : 50, 'WololoKingdoms FE' : 65, 'WololoKingdoms' : 35}

# ----------------------------------------------------------


def getrect(handle):
	return win32gui.GetWindowRect(handle)
	
def focus_and_click(title, x, y, tries=3, double=False):
	while tries > 0:
		try:
			dlg = Desktop(backend="win32").window(title_re=title)
			dlg.restore()
			dlg.set_focus()
			time.sleep(0.1)
			dlg.click_input(coords=(int(x), int(y)), double=double)
			return True
		except Exception as e:
			print('[!] Trouble focusclicking')
			print(e)
			tries -= 1
	return False

def focus_and_click_image(title, image, tries=3):
	while tries > 0:
		try:
			dlg = Desktop(backend="win32").window(title_re=title)
			dlg.restore()
			dlg.set_focus()
			time.sleep(0.1)
			x, y = pyautogui.locateCenterOnScreen(image)
			pyautogui.click(int(x), int(y))
			return True
		except Exception as e:
			print('[!] Trouble imageclicking')
			print(e)
			tries -= 1
	return False

def is_window_pixel_white(title, x, y):
	dlg = Desktop(backend="win32").window(title_re=title)
	dlg.set_focus()
	left, top, right, bot = getrect(dlg.handle)
	im = pyautogui.screenshot(region=(left, top, right-left, bot-top))
	return im.getpixel((int(x), int(y))) == (255, 255, 255)
	
def get_match_id(recording):
	return int(recording.replace('\\', '/').rpartition('/')[-1].partition('_')[0])

def cleanup_recs():
	for target in targets.values():
		for the_file in os.listdir(target):
			file_path = os.path.join(target, the_file)
			if os.path.isfile(file_path) and (file_path.lower().endswith('.mgz') or file_path.lower().endswith('.mgx')):
				print('[i] Cleaning:', file_path)
				os.unlink(file_path)

def move_recording(match_id, recording, match_mod):
	if match_mod not in targets:
		return False
	with zipfile.ZipFile(recording, "r") as zip_ref:
		for name in zip_ref.namelist():
			if name.lower().endswith('.mgz') or name.lower().endswith('.mgx'):
				print('[i] Extracting:', name, 'to:', targets[match_mod])
				zip_ref.extract(name, targets[match_mod])
				return match_mod
		return False

def setup_voobly(match_mod):
	print('[i] Opening settings')
	if waitforinput: input('[*] Ready to open settings?')
	focus_and_click_image(game_lobby, 'settings.png')
	time.sleep(1)
	if waitforinput: input('[*] Ready to set patch?')
	focus_and_click(game_settings, 315, 295)
	for key, val in versions_y.items():
		if key in match_mod:
			focus_and_click(game_settings, 315, 295+val)
			break
	else:
		focus_and_click(game_settings, 460, 445)
		return False
	time.sleep(1)
	if waitforinput: input('[*] Ready to set mod?')
	current_mod = is_window_pixel_white(game_settings, 203, 376)
	desired_mod = any([ x in match_mod for x in mods_y.keys() ])
	if current_mod != desired_mod:
		focus_and_click(game_settings, 203, 375)
	if desired_mod:
		focus_and_click(game_settings, 315, 375)
		for key, val in mods_y.items():
			if key in match_mod:
				focus_and_click(game_settings, 315, 375+val)
				break
		else:
			focus_and_click(game_settings, 460, 445)
			return False
	time.sleep(1)
	if waitforinput: input('[*] Ready to set watch game?')
	focus_and_click(game_settings, 40, 95)
	time.sleep(0.5)
	current_watch = is_window_pixel_white(game_settings, 203, 73)
	if not current_watch:
		focus_and_click(game_settings, 203, 73)
	if waitforinput: input('[*] Ready to set close settings?')
	focus_and_click(game_settings, 460, 445)
	return True

def voobly_launch():
	focus_and_click_image(game_lobby, 'launch.png')
	
def speedup_aoe():
	for i in range(10):
		focus_and_click(game_title, 224, 1034, 1)

def close_aoe():
	dlg = Desktop(backend="win32").window(title_re=game_title)
	left, top, right, bot = getrect(dlg.handle)
	focus_and_click(game_title, right - left - 20, 10)
	time.sleep(1)
	focus_and_click(game_title, (750/1680) * (right-left), (580/1050) * (bot-top-20))


if __name__ == '__main__':
	if not ctypes.windll.shell32.IsUserAnAdmin():
		exit('You need to be an administrator to run this')

	db = dataset.connect(database)
	match_ids = [row['match_id'] for row in db.query('SELECT DISTINCT match_id FROM matches')]
	replay_ids = [row['match_id'] for row in db.query('SELECT DISTINCT match_id FROM replay')]
	this_ids = []
	db = None

	for recording in glob('./recordings/*.zip'):
		print('====================================')
		print(recording)
		match_id = get_match_id(recording)
		if match_id in replay_ids or match_id in this_ids:
			print('[i] Already done')
			continue
		if match_id not in match_ids:
			print('[w] Unknown match')
			continue
		db = dataset.connect('sqlite:///voobly3.db')
		match = db['matches'].find_one(match_id=match_id)
		db = None
		match_mod = match['match_mod']
		print(match_mod)
		cleanup_recs()
		if not move_recording(match_id, recording, match_mod):
			print('[w] Unknown target dest')
			continue
		time.sleep(2)
		if not setup_voobly(match_mod):
			print('[!] Unknown target dest')
			time.sleep(5)
			continue
		print('[i] Ready to launch')
		time.sleep(1)
		if waitforinput: input('Ready to launch?')
		voobly_launch()
		print('[i] Tracking game')
		proc = subprocess.Popen([sys.executable, '_wscapture.py', str(match_id)], 
			creationflags=subprocess.CREATE_NEW_CONSOLE)
		time.sleep(20)
		try:
			print('[i] Speeding up game')
			speedup_aoe()
		except:
			print('[w] Speedup failed')
		print('[i] Waiting for close')
		poll = proc.poll()
		while poll is None:
			time.sleep(3)
			poll = proc.poll()
		try:
			print('[i] Closing game')
			close_aoe()
		except:
			print('[!] Close failed')
		this_ids.append(match_id)
		time.sleep(15)
