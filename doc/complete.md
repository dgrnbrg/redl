A fuzzy completion engine for Clojure and Java based on clojure-complete. Its functionality is mostly a superset of clojure-complete's prefix-matching, with the following additions:

# Matching algorithm:
- This engine allows for any characters to appear between characters in a form (so that "clj" matches "clojure").
- Some characters (i.e. ".", "$", and "/") are treated as special delimiters. The first letter in each of the sections separated by delimiters must be the first letter in the corresponding match. For example, "clj.c" matches "clojure.core", but "ojure.c" does NOT match "clojure.core".
- If there's an empty section between delimiters, that will match anything. For example, "clj./join" matches "clojure.string/join". For example, "./join" matches "clojure.string/join". For example, "../join" does NOT match "clojure.string/join" (there are 3 namespace segments in the pattern).

# Scoring algorithm:
- All matches are first assigned a raw score. This is a vector of the number of characters that filled each fuzzy gap to make the match succeed. For instance, with the pattern "clj", "clojure" would be given the score [0 0 1 3], since we can look at the pattern as if it was "*c*l*j*" and count the number of characters in each "*".
- To convert the raw score vector into a single number, we raise every element to the Xth power, and then sum all but the last of them up. By ignoring the last element, we don't unfairly penalize mostly prefix matches. Varying X allows us to control the extent to which a few large gaps are penalized vs. many small gaps (higher X penalizes larger gaps more). This is a common technique in regressions, and it appears to give very good results.
- Finally, we sort the matches by their scores, using alphabetical order as a tiebreaker.

# Additional metadata:
- For every match, we also collect docstrings and arglists if available. For Java methods, we include all overloads, and for Java classes, we include all constructors.
- I have a utility function that converts the match results into a form similar to what Vim's completion requires--similar adapters could be written for Emacs, Eclipse, etc.
