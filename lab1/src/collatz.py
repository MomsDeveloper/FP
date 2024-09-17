def gen_len(n: int) -> int:
    if n == 1:
        return 1
    if n % 2 == 0:
        return 1 + gen_len(n // 2)
    return 1 + gen_len(3 * n + 1)


def euler_14(n: int) -> int:
    if n == 1:
        return 1
    max_len = 0
    max_num = 0
    for i in range(1, n):
        length = gen_len(i)
        if length > max_len:
            max_len = length
            max_num = i
    return max_num


print(euler_14(1))  # 871
