#  An automated web crawler was implemented to conduct a systematic survey of published trait values for plants #common to tundra ecosystems. Given a list of species names, species, and parameter search terms, params, the #crawl function searches the Google Scholar search engine for all possible combinations of species and search #terms. Results are compiled and ordered by the frequency at which they occur throughout searches. The top most #frequent results are automatically opened in the user's default web browser. The limit parameter indicates the #maximum number of tabs to open, which was set to 50 for the purpose of our survey. Code extends the xgoogle #library through the GoogleScholarSearch and PubSearchResult classes, which retrieve search pages and represent #search results, respectively.

from xgoogle import BeautifulSoup , SearchResult
import webbrowser

def crawl(species, params, limit=-1): 
    occurences = {} 
    for sp in species: 
        for param in params: 
            searcher = GoogleScholarSearch(['"'+sp+'" ' + param]) 
            results = searcher.search() 
            if len(results) > 1: 
                for result in results: 
                    if result not in occurences: 
                        occurences[result] = 0 
                    occurences[result]+=1 
                break 
    results = sorted(occurences, key=lambda article: occurences[article]) 
    for result in results[:limit]: 
        if result.url: 
            webbrowser.open_new_tab(result.url) 
            print occurences[result], result 



class PubSearchResult(SearchResult): 
    def __init__(self, title, url, desc=None, 
                 authors=[], journal=None, year=None, abstract=None, citationnum=0): 
        SearchResult.__init__(self, title, url, desc) 
    def __hash__(self): 
        return self.url.__hash__() 
    def __str__(self): 
        return self.title 
class GoogleScholarSearch: 
    """ 
    @brief This class searches Google Scholar (http://scholar.google.com) 
    Search for articles and publications containing terms of interest. 
    """ 
    SEARCH_HOST = "scholar.google.com" 
    SEARCH_BASE_URL = "/scholar" 
    def __init__(self,terms,limit = 10,browser='Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)'): 
        self.params= urllib.urlencode({'q': "+".join(terms), 'num': limit}) 
        self.baseurl = GoogleScholarSearch.SEARCH_BASE_URL+"?"+self.params 
        self.url =  GoogleScholarSearch.SEARCH_HOST+self.baseurl 
        self.browser = browser 
    def search(self): 
        headers = {'User-Agent': self.browser} 
        conn = httplib.HTTPConnection(GoogleScholarSearch.SEARCH_HOST) 
        conn.request("GET", self.url, {}, headers) 
        resp = conn.getresponse() 
        if resp.status==200: 
            html = resp.read() 
            results = [] 
            html = html.decode('ascii', 'ignore') 
                        
            # Screen-scrape the result to obtain the publication information 
            soup = BeautifulSoup(html) 
            for record in soup.findAll('div', {'class': 'gs_r'}): 
                # Includeds error checking 
                topPart = record.first('div', {'class': 'gs_rt'})                                
                
                pubTitle = '' 
                pubUrl = None 
                if topPart.a: 
                    pubUrl = topPart.a['href'] 
                    # Clean up the URL, make sure it does not contain '\' but '/' instead 
                    pubUrl = pubUrl.replace('\\', '/') 
                    for part in topPart.a.contents: 
                        pubTitle += str(part) 
                authorPart = record.first('span', {'class': 'gs_a'}) 
                authorPart = ''.join(map(lambda tag: str(tag), authorPart.contents)) 
                    
                        
                #num = authorPart.count(" - ") 
                # Assume that the fields are delimited by ' - ', the first entry will be the 
                # list of authors, the last entry is the journal URL, anything in between 
                # should be the journal year 
                idx_start = authorPart.find(' - ') 
                idx_end = authorPart.rfind(' - ') 
                pubAuthors = authorPart[:idx_start]                
                pubJournalYear = authorPart[idx_start + 3:idx_end] 
                # If (only one ' - ' is found) and (the end bit contains '\d\d\d\d') 
                # then the last bit is journal year instead of journal URL 
                if pubJournalYear=='' and re.search('\d\d\d\d', pubJournalURL)!=None: 
                    pubJournalYear = pubJournalURL 
                    pubJournalURL = '' 
                match = re.search("Cited by ([^<]*)", str(record)) 
                pubCitation = '' 
                if match != None: 
                    pubCitation = match.group(1) 
                results.append(PubSearchResult(pubTitle, pubUrl, pubAuthors, 
                                    year=pubJournalYear, citationnum=pubCitation)) 
            return results 
        else: 
            print "ERROR: ", 
            print resp.status, resp.reason 
            return []