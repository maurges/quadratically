#!/usr/bin/env python3
import sys
import json

OK, CORRUPT, MUMBLE, DOWN, CHECKER_ERROR = 101, 102, 103, 104, 110

E1 = ['b', 'o', 'B', 'O']
E = {
		2: ['c', 'p', 'C', 'P'],
		4: ['e', 'r', 'E', 'R', '1'],
		8: ['i', 'v', 'I', 'V', '5'],
		12: ['m', 'z', 'M', 'Z', '9']
	}
m = 1000000007

def verdict(exit_code, public="", private=""):
	if public:
		print(public)
	if private:
		print(private, file=sys.stderr)
	sys.exit(exit_code)

def normalize(x):
	return (ord(x.lower()) - ord('a')) % 13

def den1(i, a):
	if a == 0:
		return "a"
	rest = den1((i+1)%len(E1), a-1)
	return E1[i] + rest

def den(n, i, a):
	if n == 1:
		return den1(i, a)
	if a < n:
		return den(n/2, 0, a)
	rest = den(n, (i+1)%len(E[n]), a-n)
	return E[n][i] + rest

def denmax(a):
	i = 0
	ret = ""
	while a > 12:
		a -= 12
		ret += E[12][i]
		i = (i + 1) % len(E[12])
	return ret + den(8, 0, a)

def denorm_from(c, a):
	next_a = (a - normalize(c))%m
	rest = denmax(next_a)
	return c + rest

def koefs_from_seeds(a, b, c, s1, s2):
	return (c, c*(s1+s2), c*s1*s2)

def gen(c1, c2, c3, seed1, seed2):
	c1_ = normalize(c1)
	c2_ = normalize(c2)
	c3_ = normalize(c3)
	a, b, c = koefs_from_seeds(c1_, c2_, c3_, seed1, seed2)
	part1 = denorm_from(c3, a)
	part2 = denorm_from(c2, b)
	part3 = denorm_from(c1, c)
	return part1 + "-" + part2 + "-" + part3

def gen_from_name(name, seed1, seed2):
	if len(name) < 3:
		verdict(CHECKER_ERROR, "","Incorrect flagid")
	a, b, c = name[0], name[1], name[2]
	if normalize(c) == 0:
		verdict(CHECKER_ERROR, "","Incorrect flagid: zero koef")
	return gen(a, b, c, seed1, seed2)

DBNAME = 'quadratic_db.db'

def save_db(data):
	with open(DBNAME, 'w') as f:
		f.write(json.dumps(data))

def load_db():
	with open(DBNAME, 'r') as f:
		return json.loads(f.read())

def init_host(db, host):
	db[host] = {'keys':{}, 'seeds':{}}
	return db

def simplify(name):
	return name[:3]

def init_name(db, host, name):
	simple = simplify(name)
	db[host]['seeds'][simple] = (1,10)
	return db

def newkey(name, host):
	name = name.decode('utf-8')
	db = load_db()
	if host not in db:
		db = init_host(db, host)

	simple = simplify(name)

	if simple not in db[host]['seeds']:
		db = init_name(db, host, name)

	s1, s2 = db[host]['seeds'][simple]

	result = gen_from_name(name, s1, s2)
	db[host]['keys'][name] = result
	db[host]['seeds'][simple] = (s1, s2+1)

	save_db(db)
	return result

def getkey(name, host):
	name = name.decode('utf-8')
	db = load_db()
	#just throw if anything goes wrong
	simple = simplify(name)

	return db[host]['keys'][name]
