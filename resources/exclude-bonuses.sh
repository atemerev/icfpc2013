#!/bin/bash
client unsolved myproblems.json | jq 'map(select(.operators | contains(["bonus"]) | not))' > unsolved-no-bonuses.json
