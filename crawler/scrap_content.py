import csv
import os
from urllib.request import urlopen

from bs4 import BeautifulSoup
from nltk.corpus import stopwords
from nltk.tokenize import wordpunct_tokenize


__author__ = 'ada'


def parse_links_from_csv(filename='pracuj.csv'):
    with open(filename, mode='r') as infile:
        reader = csv.reader(infile)
        try:
            parse_links = {rows[0]: rows[5] for rows in reader}
        except:
            print('error')
    return parse_links


def read_polish_stopwords(filename='stop_words.csv'):
    with open(filename, mode='r', encoding='utf-8') as infile:
        reader = csv.reader(infile)
        try:
            for row in reader:
                print(row)
        except:
            print('error')
        return stopwords


def load(base_address='http://www.pracuj.pl/', offer_link='praca/inzynier-big-data-warszawa,oferta,4382201'):
    address = base_address + str(offer_link)
    return BeautifulSoup(urlopen(address).read(), 'lxml')


def scrap_content(soup):
    job_content = soup.find('div', id='offCont') \
        .find('div', id='main')
    job_title = job_content.find('div', id='jobTitle').get_text().strip()
    job_info = job_content.find('div', id='info').get_text().strip()
    job_description = job_content.find('div', id='description').get_text().strip()
    return job_description


def write_to_file_by_id(parse_links):
    directory = 'job_content'
    if not os.path.exists(directory):
        os.makedirs(directory)
    for id, link in parse_links.items():
        soap = load(offer_link=link)
        try:
            job_content = scrap_content(soap)
            file_dir = directory + '/' + id + '.txt'
            if not os.path.exists(file_dir):
                file = open(file_dir, "w")
                file.write(job_content)
                file.close()
        except:
            print("Inactive Offer: " + link)


def remove_english_stopwords(text):
    set(stopwords.words('english'))
    stopwords.update(
        ['.', ',', '"', "'", '?', '!', ':', ';', '(', ')', '[', ']', '{', '}'])  # remove it if you need punctuation
    list_of_words = [i.lower() for i in wordpunct_tokenize(text) if i.lower() not in stopwords]
    return list_of_words


if __name__ == "__main__":
    print("Processing...")
    parse_links = parse_links_from_csv()
    write_to_file_by_id(parse_links)