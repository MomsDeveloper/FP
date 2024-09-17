def int2word(n: int) -> int:
    ones = ["", "one", "two", "three", "four",
            "five", "six", "seven", "eight", "nine"]
    teens = ["ten", "eleven", "twelve", "thirteen", "fourteen",
             "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    tens = ["", "", "twenty", "thirty", "forty",
            "fifty", "sixty", "seventy", "eighty", "ninety"]
    if n == 1000:
        return "onethousand"
    elif n >= 100:
        if n % 100 == 0:
            return ones[n // 100] + "hundred"
        else:
            return ones[n // 100] + "hundredand" + int2word(n % 100)
    elif n >= 20:
        return tens[n // 10] + ones[n % 10]
    elif n >= 10:
        return teens[n % 10]
    else:
        return ones[n]


def euler_17(n: int) -> int:
    return sum(len(int2word(i)) for i in range(1, n + 1))


print(euler_17(1000))  # 21124
