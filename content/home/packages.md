+++
widget = "pages"
headless = true
active = true
weight = 35

title = "<a href='/package'>Packages</a>"


[content]
  # Page type to display. E.g. post, talk, or publication.
  page_type = "package"
  
  # Choose how much pages you would like to display (0 = all pages)
  count = 0
  
  # Choose how many pages you would like to offset by
  offset = 0

  # Page order. Descending (desc) or ascending (asc) date.
  order = "desc"

  # Filter posts by a taxonomy term.
  [content.filters]
    tag = ""
    category = ""
    publication_type = ""
    author = ""
    exclude_featured = false
  
[design]
  view = 2
  
[advanced]
 # Custom CSS. 
 css_style = ""
 
 # CSS class.
 css_class = "home-section"
+++