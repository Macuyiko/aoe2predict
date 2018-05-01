import dataset
import websocket
import time
import json
import sys
import threading
import requests

connected = False
ratings = False
busy = False
last_seen = time.time()

def flattenit(pyobj, keystring=''):
	if type(pyobj) is dict:
		keystring = keystring + "_" if keystring else keystring
		for k in pyobj:
			yield from flattenit(pyobj[k], keystring + k)
	elif type(pyobj) is list:
		keystring = keystring + "_" if keystring else keystring
		for i in range(len(pyobj)):
			yield from flattenit(pyobj[i], keystring + str(i))
	else:
		yield keystring, pyobj

def on_error(ws, error):
	print(error)

def on_close(ws):
	print("*** Closed")

def on_open(ws):
	global connected, last_seen
	print("*** Connected")
	last_seen = time.time()
	t = threading.Thread(target=check_timeout, args=(ws,))
	t.start()
	connected = True

def on_message(ws, message):
	global last_seen, ratings, busy
	data = json.loads(message)
	flattened = {k:v for k,v in flattenit(data)}
	flattened['data_players_0_rating'] = ratings[0]
	flattened['data_players_1_rating'] = ratings[1]
	last_seen = time.time()
	if not busy:
		busy = True
		r = requests.post('http://localhost:8000/predict', json={"instance": flattened})
		busy = False
		print(flattened['data_players_0_name'], '@', flattened['current_time'], r.text)

try:
	from exceptions import WinError
except ImportError:
	class WinError(OSError): pass

def check_timeout(ws):
	while ws.keep_running:
		tim = time.time()
		if tim > last_seen + 5:
			print('closing!')
			ws.close()
		time.sleep(3)

if __name__ == "__main__":
	tries = 1
	if not ratings:
		r = input('Enter ratings seperated by commas: ')
		ratings = r.split(',')
	while not connected and tries < 20:
		print('Trying to connect...', tries)
		tries += 1
		ws = websocket.WebSocketApp("ws://localhost:9002/",
								on_message=on_message,
								on_error=on_error,
								on_close=on_close)
		ws.on_open = on_open
		ws.run_forever()


