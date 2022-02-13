from collections import defaultdict
import sys
def group_diff1(points):
    while True:
        groups = defaultdict(list)
        for point in points:
            for i in range(4):
                l = list(point)
                removed = l.pop(i)
                groups[(i, tuple(l))].append(removed)
        def score(entry):
            (_, values) = entry
            return len(values)
        ((index, rest), values) = max(groups.items(), key=score)
        if len(values) <= 1:
            break
        replacement = list(rest)
        replacement.insert(index, '({})'.format('|'.join(sorted(values))))
        points.add(tuple(replacement))
        previous = list(rest)
        previous.insert(index, 0)
        for value in values:
            previous[index] = value
            points.remove(tuple(previous))

data = set(
    tuple(line.strip().split(','))
    for line in sys.stdin.readlines()
)
group_diff1(data)
for point in sorted(data):
    print(','.join(point))
