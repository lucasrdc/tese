#-*- coding: utf-8 -*-
#!/usr/bin/python3

import time
import csv
from geopy import geocoders
from geopy.exc import GeocoderQuotaExceeded
import unicodedata

g = geocoders.GoogleV3(api_key='OMITIDO',timeout=10)

with open('liraa2018.csv', 'r', encoding='ISO-8859-1') as csvfile1:
    with open('saida2018.csv', 'w', encoding='utf-8') as csvfile2:
        cep = csv.reader(csvfile1, delimiter=';')
        writer = csv.writer(csvfile2, delimiter=';')
        for row in cep:
            rua = unicodedata.normalize('NFKD', row[1]).encode('ASCII', 'ignore').decode('utf-8').lower()
            numero=row[2]
            endereco = "Rua "+rua+", "+numero+" - Pedro Leopoldo, Minas Gerais, Brasil"
            certo = False

            while not certo:
                try:
                    location = g.geocode(endereco)                    
                    if location:
                        lat=location.latitude
                        lon=location.longitude
                    else:
                        lat = None
                        lon = None
                    certo = True
                except GeocoderQuotaExceeded as e:
                    time.sleep(86400) # espera 24 horas
                    pass
                except GeocoderTimedOut as e:
                    time.sleep(50)
                    pass
                except:
                    pass
                else:
                    linha = [row[0],
                             row[12],
                        lat,
                        lon
                        ]
                    writer.writerow(linha)


with open('liraa2015.csv', 'r', encoding='ISO-8859-1') as csvfile1:
    with open('saida2015.csv', 'w', encoding='utf-8') as csvfile2:
        cep = csv.reader(csvfile1, delimiter=';')
        writer = csv.writer(csvfile2, delimiter=';')
        for row in cep:
            rua = unicodedata.normalize('NFKD', row[1]).encode('ASCII', 'ignore').decode('utf-8').lower()
            numero=row[2]
            endereco = "Rua "+rua+", "+numero+" - Pedro Leopoldo, Minas Gerais, Brasil"
            certo = False

            while not certo:
                try:
                    location = g.geocode(endereco)                    
                    if location:
                        lat=location.latitude
                        lon=location.longitude
                    else:
                        lat = None
                        lon = None
                    certo = True
                except GeocoderQuotaExceeded as e:
                    time.sleep(86400) # espera 24 horas
                    pass
                except GeocoderTimedOut as e:
                    time.sleep(50)
                    pass
                except:
                    pass
                else:
                    linha = [row[0],
                             row[12],
                        lat,
                        lon
                        ]
                    writer.writerow(linha)

with open('liraa2016.csv', 'r', encoding='ISO-8859-1') as csvfile1:
    with open('saida2016.csv', 'w', encoding='utf-8') as csvfile2:
        cep = csv.reader(csvfile1, delimiter=';')
        writer = csv.writer(csvfile2, delimiter=';')
        for row in cep:
            rua = unicodedata.normalize('NFKD', row[1]).encode('ASCII', 'ignore').decode('utf-8').lower()
            numero=row[2]
            endereco = "Rua "+rua+", "+numero+" - Pedro Leopoldo, Minas Gerais, Brasil"
            certo = False

            while not certo:
                try:
                    location = g.geocode(endereco)                    
                    if location:
                        lat=location.latitude
                        lon=location.longitude
                    else:
                        lat = None
                        lon = None
                    certo = True
                except GeocoderQuotaExceeded as e:
                    time.sleep(86400) # espera 24 horas
                    pass
                except GeocoderTimedOut as e:
                    time.sleep(50)
                    pass
                except:
                    pass
                else:
                    linha = [row[0],
                             row[12],
                        lat,
                        lon
                        ]
                    writer.writerow(linha)

with open('liraa2017.csv', 'r', encoding='ISO-8859-1') as csvfile1:
    with open('saida2017.csv', 'w', encoding='utf-8') as csvfile2:
        cep = csv.reader(csvfile1, delimiter=';')
        writer = csv.writer(csvfile2, delimiter=';')
        for row in cep:
            rua = unicodedata.normalize('NFKD', row[1]).encode('ASCII', 'ignore').decode('utf-8').lower()
            numero=row[2]
            endereco = "Rua "+rua+", "+numero+" - Pedro Leopoldo, Minas Gerais, Brasil"
            certo = False

            while not certo:
                try:
                    location = g.geocode(endereco)                    
                    if location:
                        lat=location.latitude
                        lon=location.longitude
                    else:
                        lat = None
                        lon = None
                    certo = True
                except GeocoderQuotaExceeded as e:
                    time.sleep(86400) # espera 24 horas
                    pass
                except GeocoderTimedOut as e:
                    time.sleep(50)
                    pass
                except:
                    pass
                else:
                    linha = [row[0],
                             row[12],
                        lat,
                        lon
                        ]
                    writer.writerow(linha)


