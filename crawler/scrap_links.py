from bs4 import BeautifulSoup
from urllib.request import urlopen
from tqdm import tqdm
import csv

# Extract number of job offers pages.
base_address = 'http://www.pracuj.pl/praca'
html = urlopen(base_address).read()
soup = BeautifulSoup(html, 'lxml')
no_of_pages = soup.find('ul', class_='desktopPagin clearfix').find_all('li')[-2].get_text().strip()
no_of_pages = int(no_of_pages)
last_page = False

with open('jobs.csv', 'w') as csvfile:
    fieldnames = ['id', 'position', 'employer', 'date', 'location', 'href']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

    id = 0  # unique offer counter
    i = 0  # page counter
    pbar = tqdm(total=no_of_pages) # progress bar

    # Iterate over pages until it's the last one.
    writer.writeheader()
    while not last_page:
        i += 1
        address = base_address + r'?pn=' + str(i)
        html = urlopen(address).read()
        soup = BeautifulSoup(html, 'lxml')
        job_listing = soup.find('div', class_='content')\
            .find('div', class_='clearfix block resultsCnt')\
            .find('section', class_='offer rightCol', id='returnUrl')\
            .find('ul', class_='offer__list', id='mainOfferList')\
            .find_all('li', class_='offer__list_item', itemscope="", recursive=False)

        for job in job_listing:
            id += 1

            position = job.h2.a
            loc_link = []
            if position is None:
                position = job.h2.span.get_text().strip()
                date = job.find('p', class_='offer__list_item_regions clearfix')\
                    .find('span', class_='offer__list_item_desc_date').get_text().strip()
                locations = job.find('ul', class_='acc_cnt offer__list--inner').find_all('li')

                for region in locations:
                    location = region.h2.a.get_text().strip()
                    link = region.h2.a['href']
                    loc_link.append((location, link))
            else:
                position = position.get_text().strip()
                date = job.p.find('span', class_='offer__list_item_desc_date').get_text().strip()
                location = job.p.span.find('span', class_='offer__list_item_desc_location_name_text').get_text().strip()
                link = job.h2.a['href']
                loc_link.append((location, link))
            employer = job.h3.a.get_text().strip()

            # Dump output to csv file
            for loc, lnk in loc_link:
                writer.writerow({'id': i,
                                 'position': position.encode('ascii', 'ignore'),
                                 'employer': employer.encode('ascii', 'ignore'),
                                 'date': date.encode('ascii', 'ignore'),
                                 'location': loc.encode('ascii', 'ignore'),
                                 'href': lnk})

        # Check if there is next page
        last_li = soup.find('ul', class_='desktopPagin clearfix').find_all('li')[-1].get_text().strip()
        last_page = False if last_li == 'Następna' else True
        pbar.update(1)  # increment progress bar

pbar.close()
print('\n Successful parsing \n')