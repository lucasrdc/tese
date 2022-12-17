#!/usr/bin/python3

import time
import csv
from geopy import geocoders
from geopy.exc import GeocoderQuotaExceeded

g = geocoders.GoogleV3(api_key='OMITIDO',timeout=10)

with open('enderecos.csv', 'r', encoding='ISO-8859-1') as csvfile1:
    with open('saida.csv', 'w', encoding='utf-8') as csvfile2:
        cep = csv.reader(csvfile1, delimiter=';')
        writer = csv.writer(csvfile2, delimiter=';')
        for row in cep:
            endereco = "Rua "+row[1]+" "+row[2]+" - Pedro Leopoldo, Minas Gerais, Brasil"
            certo = False
            location = g.geocode(endereco)

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
                        lat,
                        lon
                        ]
                    print(row[0],lat,lon)
                    writer.writerow(linha)
