import requests


def is_valid(raw_word):
    return "s" not in raw_word and len(raw_word) >= 4 and len(set(raw_word)) <= 7

url = "https://norvig.com/ngrams/enable1.txt"
raw_words = filter(is_valid, requests.get(url).text.splitlines())

def score(raw_word):
    length = len(raw_word)
    n_chars = len(set(raw_word))
    return (1 if length == 4 else length) + (7 if n_chars == 7 else 0)

words = [{"chars": frozenset(raw), "score": score(raw)} for raw in raw_words]

pangram_sets = {word["chars"] for word in words if len(word["chars"]) == 7}

honeycombs = []
words_for_chars = {}
for chars in pangram_sets:
    words_for_chars[chars] = [w for w in words if w["chars"] <= chars]
    honeycombs.extend({"center": c, "chars": chars} for c in chars)

def max_score(honeycomb):
    score = 0
    for word in words_for_chars[honeycomb["chars"]]:
        if honeycomb["center"] in word["chars"]:
            score += word["score"]
    return score

best_honeycomb = max(honeycombs, key=max_score)
print(best_honeycomb)
print("Score:", max_score(best_honeycomb))
