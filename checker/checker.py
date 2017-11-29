#!/usr/bin/env python3

import sys
from socket import create_connection as cr_conn
import string
import random
import traceback

from keygen import newkey, getkey

OK, CORRUPT, MUMBLE, DOWN, CHECKER_ERROR = 101, 102, 103, 104, 110
PORT = 6666

def randname():
	length = 32
	letters = string.ascii_letters + string.digits
	name = "".join(random.choice(letters) for i in range(length))
	return name.encode("utf-8")


def recvtil(sock, s):
	r = ""
	l = len(s)
	while r[-l:] != s:
		t = sock.recv(100500).decode('utf-8')
		r += t
	return r

def save_db(data):
	with open('db.txt', 'w') as f:
		f.write(json.dumps(data))

def load_db():
	with open('db.txt', 'r') as f:
		return json.loads(f.read())

def trace(message):
	print(message, file=sys.stderr)

def verdict(exit_code, public="", private=""):
	if private:
		print(private, file=sys.stderr)
	sys.exit(exit_code)


def check(args):
	if len(args) != 1:
		verdict(CHECKER_ERROR, "Wrong args count","Wrong args count for check()")
	host = args[0]
	trace("checking %s"%host)

	s = ""
	try:
		s = cr_conn((host, PORT))
	except Exception as e:
		verdict(DOWN, "Down", "Exception: %s" % traceback.format_exc())

	try:
		recvtil(s, ": \n")
		#here it asks for name
		myname = randname()
		s.send(myname + b"\n")
		recvtil(s, "> ")
		#see the list of users
		s.send(b"list\n")
		users = recvtil(s, "> ")
		if myname.decode("utf-8") not in users:
			verdict(MUMBLE, "Mumble", "Incorrect user list")
		s.close()
		verdict(OK)
	except Exception as e:
		verdict(MUMBLE, "Mumble", "Exception: %s" % traceback.format_exc())


def put(args):
	if len(args) != 4:
		verdict(CHECKER_ERROR, "Wrong args count", "Wrong args count for put()")
	host, flag_id, flag_data, vuln = args
	trace("put(%s, %s, %s, %s)" % (host, flag_id, flag_data, vuln))

	s = ""
	try:
		s = cr_conn((host, PORT))
	except Exception as e:
		verdict(DOWN, "Down", "Exception: %s" % traceback.format_exc())
	try:
		recvtil(s, ": \n")
		#here it asks for name
		myname = flag_id.encode("utf-8")
		s.send(myname + b"\n")
		recvtil(s, "> ")

		#premiumize
		key = newkey(myname, host).encode("utf-8")
#		trace("claim %s"%key)
		s.send(b"claim " + key + b"\n")
		str = recvtil(s, "> ")
		#check if reply was correct
		if "Successfully claimed the key" not in str:
			verdict(MUMBLE, "Mumble", "could not claim the key")

		#put flag
		s.send(b"proclaim " + flag_data.encode("utf-8") + b"\n")
		str = recvtil(s, "> ")
		#check if reply was correct
		if "proclaim" not in str:
			verdict(MUMBLE, "Mumble", "could not proclaim the flag")

		s.send(b"quit\n")
		s.close()
		verdict(OK)
	except Exception as e:
		verdict(MUMBLE, "Mumble", "Exception: %s" % traceback.format_exc())

def get(args):
	if len(args) != 4:
		verdict(CHECKER_ERROR, "Wrong args count", "Wrong args count for put()")
	host, flag_id, flag_data, vuln = args
	trace("put(%s, %s, %s, %s)" % (host, flag_id, flag_data, vuln))

	s = ""
	try:
		s = cr_conn((host, PORT))
	except Exception as e:
		verdict(DOWN, "Down", "Exception: %s" % traceback.format_exc())
	try:
		recvtil(s, ": \n")
		#here it asks for name
		myname = flag_id.encode("utf-8")
		s.send(myname + b"\n")
		recvtil(s, "key\n")

		#authorize
		key = getkey(myname, host).encode("utf-8")
		s.send(key + b"\n")
		str = recvtil(s, "> ")
		#check if reply was correct
		if "Successfully" not in str:
			verdict(MUMBLE, "Mumble", "could reclaim the key")

		#get flag
		s.send(b"see\n")
		str = recvtil(s, "> ")
		if flag_data not in str:
			verdict(CORRUPT, "Corrupt", "incorrect flag or no at all")

		s.send(b"quit\n")
		s.close()
		verdict(OK)
	except Exception as e:
		verdict(MUMBLE, "Mumble", "Exception: %s" % traceback.format_exc())
	


def main(args):
	if len(args) == 0:
		verdict(CHECKER_ERROR, "No args")
	try:
		if args[0] == "info":
			info()
		elif args[0] == "check":
			check(args[1:])
		elif args[0] == "put":
			put(args[1:])
		elif args[0] == "get":
			get(args[1:])
		else:
			verdict(CHECKER_ERROR, "Checker error", "Wrong action: " + args[0])
	except Exception as e:
		verdict(DOWN, "Down", "Exception: %s" % traceback.format_exc())


if __name__ == "__main__":
	try:
		main(sys.argv[1:])
		verdict(CHECKER_ERROR, "Checker error (NV)", "No verdict")
	except Exception as e:
		verdict(CHECKER_ERROR, "Checker error (CE)", "Exception: %s" % e)
