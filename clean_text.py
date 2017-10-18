## -*- coding: utf-8 -*-
import re
from unidecode import unidecode
import fileinput
import HTMLParser

import warnings
warnings.filterwarnings("ignore")

import sys
#reload(sys)  
#sys.setdefaultencoding('utf8')

#__init__.py importa la funcion print (pruebas de tensorflow)
#  cuando VERBOSE=T y salida a consola en ocasiones sale todo OK
#  pero cuando la salida redir (>) a archivo puede haber errores en print
VERBOSE = False

def clean_text(s,downcase=False,remove_single_chars=False):
    #Limpiar tags HTML
    so = re.sub(r"<[^>]*>"," ",s)
    #Lo que parezca horas o dinero dejarlo sin el separador
    so = re.sub(r"(\d+)[,:](\d+)",r"\1\2",so)
    #Eliminar caracteres de separacion
    so = re.sub(r'[.\(\),:!\"\-\'\+\/\*]'," ",so)
    #Eliminar espacios breaking o normales duplicados 
    so = re.sub(r'[ \xc2\xa0]+',' ',so)
    if downcase:
        so = so.lower()
    return so.strip()

def try_to_decode(txt):
    try:
        txt = unidecode(txt) 
        if VERBOSE: print"Unidecoded"
    except UnicodeDecodeError:
        if VERBOSE: print(">>>UnidecodeError")
        try:
            txt = unidecode(txt.decode('utf-8'))
            if VERBOSE: print("Decoded UTF-8") 
        except UnicodeDecodeError:
            if VERBOSE: print(">>>UnidecodeError2")
            try:
                txt = unidecode(txt.decode('iso-8859-1'))
                if VERBOSE: print("Decoded iso-8859-1")
            except:
                if VERBOSE: print(">>>UnidecodeError3")
                try:
                    txt = unidecode(txt.decode('cp1252'))
                    if VERBOSE: print("Decoded cp1252")
                except UnicodeDecodeError:
                    #que mas se puede hacer aqui?
                    if VERBOSE: print("Couldn't decode")
                    pass
    return txt


h = HTMLParser.HTMLParser()
count = 0
for line in fileinput.input():
    txt = line.strip()
    if VERBOSE:
        print("Line "+str(count))
        print("==============")
        print(txt)
    txt = try_to_decode(txt)
    txt = h.unescape(txt)
    if VERBOSE:
        print(txt)
    txt = clean_text(txt,downcase=True)
    if VERBOSE:
        print("---------")
    print(txt.encode("utf-8"))
    count+=1
