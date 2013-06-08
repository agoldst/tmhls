readme.txt

Code and data for the second round of a Jstor collaboration between Andrew Goldstone and Ted Underwood.

tmhls = "topic-modeling and the history of literary scholarship."

THE STOPLIST

I've recreated the stopword list from sources. Right now it's at an intermediate stage. There are a lot of ambiguous cases in two special categories below (inGoldstone and inUnderwood). We may want to frame a general policy about what to include before I try to sort those out manually.

Currently stoplist.tsv is a list with two columns

stopword	label

The label tells you why the stopword is present. Within each label-category, the words are sorted by frequency (in a dictionary of mostly 19c books, so take it with a grain of salt, but it might usefully foreground some patterns).

The categories are:

alphabet -- I find that words of one letter are always bad mojo.

englishfunction -- A list of common function words I produced manually some time ago.

inGoldstone -- words that were in your multilingual stoplist, but not in any of the other sources below

inUnderwood -- words that were in my stoplist, but not in any of the other sources below

(I'm going to suggest that we go through both of these sets manually.)

french, german, latin, italian, spanish -- Common words from these languages. I have not tried to rigorously identify *only* function words, because that would have been tiring. Just took the most common 200 or so for each language.

romannumeral -- all roman numerals up to 499

name -- 

Most of the male and female names come from

http://names.mongabay.com/female_names_alpha.htm

I supplemented this with a list of first names I noticed in my own data (largely 18c ones that aren't currently common), plus some names from Matt Jockers' list.

Then this list was filtered to remove names that are actual dictionary words (like June and Prudence).

I can't guarantee that this list will include names common in other languages; that's a place where we might need to do some adding.

STOPLISTCREATOR.PY

The Python script I used is also included, as a kind of documentation, though it's really ugly.
