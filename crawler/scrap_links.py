# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
from urllib.request import urlopen
from tqdm import tqdm
import csv
import os.path


# Load pracuj page
def load(page=1, base_address='http://www.pracuj.pl/praca'):
    address = base_address + r'?pn=' + str(page)
    return BeautifulSoup(urlopen(address).read(), 'lxml')


# Get number of pages in pracuj.pl/praca
def get_max_pages():
    soup = load()
    return int(soup.find('ul', class_='desktopPagin clearfix').find_all('li')[-2].get_text().strip())


# Check if current page is the last one
def is_last(current):
    last_li = current.find('ul', class_='desktopPagin clearfix').find_all('li')[-1].get_text().strip()
    return False if last_li == 'Następna' else True


def id_list(filename='pracuj.csv'):
    with open(filename) as f:
        ids = [row['id'] for row in csv.DictReader(f)]
    return ids


# This function creates database or update existing one.
def update_csv(filename='pracuj.csv'):
    exist = os.path.isfile(filename)
    # If it's not a new file, create list of id
    if exist:
        ids = id_list(filename)

    with open(filename, 'a+', newline='') as csvfile:
        fieldnames = ['id', 'position', 'employer', 'date', 'location', 'href']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        # Setting initial values
        last_page = is_last(load(page=1))       # Checking if  starting page is the first one
        unique = True                           # Unique is True as long as we haven't encounter duplicate id
        i = 0                                   # page counter
        new_rows = 0                            # number of added rows
        pbar = tqdm(total=get_max_pages())      # progress bar. Maximum number of iterations is equal max_pages.
        if not exist:                           # write header if it's new file
            writer.writeheader()

        # Iterate over pages until it's the last one.
        while not last_page and unique:
            i += 1
            soup = load(page=i)
            job_listing = soup.find('div', class_='content')\
                .find('div', class_='clearfix block resultsCnt')\
                .find('section', class_='offer rightCol', id='returnUrl')\
                .find('ul', class_='offer__list', id='mainOfferList')\
                .find_all('li', class_='offer__list_item', itemscope="", recursive=False)

            for job in job_listing:
                one_location = job.h2.a
                loc_link = []
                if one_location is None:
                    position = job.h2.span.get_text().strip()
                    date = job.find('p', class_='offer__list_item_regions clearfix')\
                        .find('span', class_='offer__list_item_desc_date').get_text().strip()
                    locations = job.find('ul', class_='acc_cnt offer__list--inner').find_all('li')

                    for region in locations:
                        location = region.h2.a.get_text().strip()
                        link = region.h2.a['href']
                        id_job = region.h2['data-applied-offer-id']
                        loc_link.append((location, link, id_job))
                else:
                    id_job = job.h2['data-applied-offer-id']
                    position = one_location.get_text().strip()
                    date = job.p.find('span', class_='offer__list_item_desc_date').get_text().strip()
                    location = job.p.span.find('span', class_='offer__list_item_desc_location_name_text')\
                        .get_text().strip()
                    link = job.h2.a['href']
                    loc_link.append((location, link, id_job))
                employer = job.h3.a.get_text().strip()

                # Check if there is any duplicate.
                if exist:
                    if any(item[2] in ids for item in loc_link):
                        unique = False
                        break
                try:
                    # Dump output to csv file
                    for loc, lnk, idn in loc_link:
                        new_rows += 1
                        writer.writerow({'id': idn,
                                         'position': position,
                                         'employer': employer,
                                         'date': date,
                                         'location': loc,
                                         'href': lnk})
                except UnicodeEncodeError:
                    for loc, lnk, idn in loc_link:
                        new_rows += 1
                        writer.writerow({'id': idn,
                                         'position': 'UnicodeEncodeError',
                                         'employer': 'UnicodeEncodeError',
                                         'date': 'UnicodeEncodeError',
                                         'location': 'UnicodeEncodeError',
                                         'href': lnk})

            # Check if there is next page
            last_page = is_last(soup)
            pbar.update(1)  # increment progress bar
    pbar.close()
    csvfile.close()
    print('\n Successful parsing. Successfully added ', new_rows, ' rows.')
