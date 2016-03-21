#!/usr/bin/env python2.5

# Extract a few fields from the concatenation-of-json format

import simplejson,sys,os,datetime,re,time
#from collections import defaultdict
#import util;util.fix_stdio(shutup=False)  # anyall.org/util.py
class default_dict(dict):
    def __missing__(self, key):
        self[key] = value = self.__class__()
        return value
keys = sys.argv[1:]
if not keys:
  keys = ('id','created_at', 'user.name','user.location','user.statuses_count','user.description','user.friends_count','user.following_count','user.followers_count','user.listed_count','user.favourites_count','user.screen_name','retweeted','retweet_count','text')
header = "\t".join(keys)


msgs = default_dict()

def log(key, msg):
  msgs[key].append(msg)
  #print>>sys.stderr,msg

BAD = re.compile("[\r\n\t]")
def clean_cell(x):
  if x is None: return ""
  return BAD.sub(" ", unicode(x))
def lookup(json, k):
  # return json[k]
  if '.' in k:
    # jpath path
    ks = k.split('.')
    v = json
    for k in ks: v = v.get(k,{})
    return v or ""
  return json.get(k,"")

def nicedate(twitter_date):
  return (datetime.datetime(*t[:7]) - datetime.timedelta(seconds=time.altzone)).strftime("%a %b %d %I:%M")


attempts=0
nonid=0
success=0

#for line in util.counter(sys.stdin):
#def protected_stdin():
#  line = sys.stdin.next()
#  while True:
#    yield line
#    try:
#      line = sys.stdin.next()
#    except UnicodeDecodeError, e:
#      log(str(type(e)), e.message)

outfile = open("newAsthma.txt", 'w')
outfile.write(header + "\n")
for line in (sys.stdin):
  attempts += 1
  if True:
    try:
      json = simplejson.loads(line)
      if 'id' not in json: 
        nonid+=1
        continue
     #print sorted(json.keys())
      d = {}
      for k in keys:
        d[k] = lookup(json,k)
      dt = time.strptime(d['created_at'], "%a %b %d %H:%M:%S +0000 %Y")
      d['created_at'] = time.strftime("%Y-%m-%dT%H:%M:%S",dt)
      currentline = "\t".join(clean_cell(d[k]) for k in keys)
   # print currentline
      outfile.write(currentline + "\n")
      success += 1
    except IOError, e:
      break
    except KeyboardInterrupt,e:
      break
    except Exception,e:
    #raise e
    #log(str(type(e)), str(type(d))+" "+e.message)
      continue
outfile.close()
for key in msgs:
  print>>sys.stderr, "Error report"
  print>>sys.stderr, key, len(key)
  #for m in msgs[key]: print>>sys.stderr, msgs[key]
print>>sys.stderr, "%d input lines" % attempts
print>>sys.stderr, "%d non-id legit json objects" % nonid
print>>sys.stderr, "%d records successfully processed" % success
