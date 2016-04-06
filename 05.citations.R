# 5. Add TRY citations into BETY

# a. Get unique citation list from TRY.
# b. Get data for each citation via rcrossref (author, year, title, journal, volume, pg, url, doi)
# c. Loop over DOI's:
#         i. Check if DOI already in BETY. If yes, record citation_id
#         ii. If no, INSERT INTO citations(author, year, title, journal, vol, pg, url, doi) VALUES(...)
#         iii. Get last citation ID.
# d. Merge citation_id back into base TRY data.