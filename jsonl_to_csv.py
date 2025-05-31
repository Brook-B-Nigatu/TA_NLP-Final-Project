import json
import csv

def jsonl_to_csv(jsonl_file, csv_file):
    """Convert a JSONL file to a CSV file."""
    with open(jsonl_file, 'r', encoding='utf-8') as infile, open(csv_file, 'w', newline='', encoding='utf-8') as outfile:
        writer = None
        for line in infile:
            data = json.loads(line)
            if writer is None:
                # Initialize CSV writer with fieldnames from the first JSON object
                writer = csv.DictWriter(outfile, fieldnames=data.keys())
                writer.writeheader()
            writer.writerow(data)

# Convert submissions and comments JSONL files to CSV
jsonl_to_csv('futurology_submissions.jsonl', 'futurology_submissions.csv')
jsonl_to_csv('futurology_comments.jsonl', 'futurology_comments.csv')
