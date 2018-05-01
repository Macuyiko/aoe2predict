import dataset
import websocket
import time
import json
import sys
import threading

# This script is not ran directly, but spawned by voobly_playback.py

match_id = int(sys.argv[1])
db = dataset.connect('sqlite:///voobly.db')
connected = False
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
	global last_seen
	data = json.loads(message)
	flattened = {k:v for k,v in flattenit(data)}
	flattened['match_id'] = match_id
	db['replay'].upsert(flattened, ['match_id', 'current_time', 'type'])
	last_seen = time.time()

try:
	from exceptions import WinError
except ImportError:
	class WinError(OSError): pass

def check_timeout(ws):
	while ws.keep_running:
		print('last_seen:', last_seen)
		tim = time.time()
		if tim > last_seen + 5:
			print('closing!')
			ws.close()
		time.sleep(3)

if __name__ == "__main__":
	tries = 1
	while not connected and tries < 20:
		print('Trying to connect...', tries)
		tries += 1
		ws = websocket.WebSocketApp("ws://localhost:9002/",
								on_message=on_message,
								on_error=on_error,
								on_close=on_close)
		ws.on_open = on_open
		ws.run_forever()


