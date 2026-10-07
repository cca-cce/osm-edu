# SPEC: Word cloud (single-page web app)

## 1. Purpose
Paste any English text and see its most important words as a word cloud,
in the browser, without Python.

## 2. Files and libraries
- Exactly one file, wordcloud.html, with the CSS and JavaScript inside it.
  No build step, no server.
- Load the libraries as ES modules from the jsdelivr CDN, with pinned major versions:
  wink-nlp@2 with wink-eng-lite-web-model@1 (NLP), d3@7 and d3-cloud@1 (word cloud).
- Must work when wordcloud.html is opened directly from the folder (a file:// address)
  in current Chrome, Firefox, Edge and Safari. An internet connection is needed for
  the libraries.

## 3. Layout
- Wide screens (700 px and wider): two columns. Left: a labelled text area and a
  Submit button. Right: the word cloud.
- Narrow screens (below 700 px): one column, text area and button on top, word cloud
  below.
- The text area starts with a short sample paragraph, so Submit works right away.

## 4. Processing (when Submit is clicked)
1. Read: take the text from the text area; if it is empty, show a short message
   instead of a cloud.
2. Clean: lowercase; remove web addresses, numbers and extra whitespace.
3. Tokenise with wink-nlp (readDoc).
4. Lemmatise: replace each word with its lemma (its.lemma), e.g. "running" -> "run".
5. Filter: keep words of three or more letters (a-z); drop stop words
   (its.stopWordFlag) and punctuation.
6. Count how often each lemma occurs and keep the 100 most frequent.

## 5. Word cloud
- Layout with d3-cloud, drawn as SVG; font size scaled to frequency, the most frequent
  lemma largest; words horizontal.
- Hovering a word shows the lemma and its count.
- A line under the cloud: number of tokens and number of distinct lemmas.
- Clicking Submit again replaces the old cloud.

## 6. Acceptance checks
- A1 Opening wordcloud.html from the folder shows the page with no errors in the
  browser console.
- A2 Paste a text, click Submit: a word cloud appears within a few seconds.
- A3 The cloud contains no stop words such as "the", "and" or "was".
- A4 The verbs "runs", "ran" and "running" all count as one lemma, "run".
- A5 At 400 px width the cloud is below the text area; on a desktop it is to the right.
- A6 Submitting a new text replaces the cloud; an empty text shows the message.
