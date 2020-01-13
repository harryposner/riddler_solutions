MAX_TO_CHECK = 1_000_000

TWO_DIGIT_WORDS = {
    0: "",
    1: "one",
    2: "two",
    3: "three",
    4: "four",
    5: "five",
    6: "six",
    7: "seven",
    8: "eight",
    9: "nine",
    10: "ten",
    11: "eleven",
    12: "twelve",
    13: "thirteen",
    14: "fourteen",
    15: "fifteen",
    16: "sixteen",
    17: "seventeen",
    18: "eighteen",
    19: "nineteen",
    20: "twenty",
    30: "thirty",
    40: "forty",
    50: "fifty",
    60: "sixty",
    70: "seventy",
    80: "eighty",
    90: "ninety",
}

POWERS_OF_10 = {
    9: "billion",
    6: "million",
    3: "thousand",
    0: "",
}


def digits(n, place, ending_place=None):
    if ending_place is None:
        ending_place = place + 1
    return (n % (10 ** ending_place)) // (10 ** place)


def three_digits_as_words(n):
    hundreds = TWO_DIGIT_WORDS[digits(n, 2)] + " hundred" if n >= 100 else ""
    if 10 <= n % 100 < 20:
        tens = TWO_DIGIT_WORDS[digits(n, 0, 2)]
        ones = ""
    else:
        tens = TWO_DIGIT_WORDS[digits(n, 1) * 10]
        ones = TWO_DIGIT_WORDS[digits(n, 0)]
    return " ".join([hundreds, tens, ones])

def digits_as_words(n):
    word_parts = []
    for expt, size_word in POWERS_OF_10.items():
        if n >= 10 ** expt:
            word_part = three_digits_as_words(digits(n, expt, expt + 3))
            if word_part:
                word_parts.extend([word_part, size_word])
    return " ".join(word_parts).strip().replace("  ", " ")


ALPHABET = " abcdefghijklmnopqrstuvwxyz"
def gematria(a_string):
    return sum(ALPHABET.find(char) for char in a_string.lower())


print(max(n for n in range(MAX_TO_CHECK) if n < gematria(digits_as_words(n))))
