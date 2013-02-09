# It would be awesome if someone can port this to Elisp.

import urllib2
import re
from hashlib import md5
from Levenshtein import ratio
import sys

def htmlDecode(string):
    entities = {'&apos;':'\'','&quot;':'"','&gt;':'>','&lt;':'<','&amp;':'&'}
    for i in entities:
        string = string.replace(i,entities[i])
    return string

def decryptResultXML(value):
    magickey = ord(value[1])
    neomagic = ''
    for i in range(20, len(value)):
        neomagic += chr(ord(value[i]) ^ magickey)
    return neomagic

def get_lyrics():
    xml ="<?xml version=\"1.0\" encoding='utf-8'?>\r\n"
    xml+="<search filetype=\"lyrics\" artist=\"%s\" title=\"%s\" " % (artist, song)
    xml+="ClientCharEncoding=\"utf-8\"/>\r\n"
    md5hash = md5(xml+"Mlv1clt4.0").digest()
    request = "\x02\x00\x04\x00\x00\x00%s%s" % (md5hash, xml)
    del md5hash,xml
    
    url = "http://www.viewlyrics.com:1212/searchlyrics.htm"
    req = urllib2.Request(url,request)
    req.add_header("User-Agent", "MiniLyrics")
    proxy = urllib2.ProxyHandler({'http': '10.3.100.212:8080'})
    opener = urllib2.build_opener(proxy)
    response = opener.open(req).read()
    print sorted(miniLyricsParser(response), cmp=compare_results)
    
def miniLyricsParser(response):
    text = decryptResultXML(response)
    lines = text.splitlines()
    ret = []
    for line in lines:
        if line.strip().startswith("<fileinfo filetype=\"lyrics\" "):
            loc = []
            loc.append(htmlDecode(re.search('link=\"([^\"]*)\"',line).group(1)))
            if not loc[0].lower().endswith(".lrc"):
                continue
            if(re.search('artist=\"([^\"]*)\"',line)):
                loc.insert(0,htmlDecode(re.search('artist=\"([^\"]*)\"',line).group(1)))
            else:
                loc.insert(0,' ')
            if(re.search('title=\"([^\"]*)\"',line)):
                loc.insert(1,htmlDecode(re.search('title=\"([^\"]*)\"',line).group(1)))
            else:
                loc.insert(1,' ')
            ret.append(loc)
    return ret

def compare_results(x, y):
    ar = ratio(artist, y[0]) - ratio(artist, x[0])
    if ar == 0:
        return int(round(ratio(song, y[1]) - ratio(song, x[1])))
    else:
        return int(round(ar))

artist = sys.argv[1]
song = sys.argv[2]
get_lyrics()
