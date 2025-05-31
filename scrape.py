import praw
import os
import json
import time

# Read credentials from credentials.txt
creds = {}
with open('credentials.txt') as f:
    for line in f:
        if ':' in line:
            key, value = line.strip().split(':', 1)
            creds[key.strip()] = value.strip()

reddit = praw.Reddit(
    client_id=creds['client_id'],
    client_secret=creds['client_secret'],
    user_agent='FuturologyScraper/0.1 by YourUsername'
)

subreddit = reddit.subreddit('Futurology')

def scrape():
    # Load existing submission and comment IDs to avoid duplicates
    sub_ids = set()
    if os.path.exists('futurology_submissions.jsonl'):
        with open('futurology_submissions.jsonl', 'r', encoding='utf-8') as f:
            for line in f:
                try:
                    obj = json.loads(line)
                    sub_ids.add(obj['id'])
                except Exception:
                    print(f"Error parsing line: {line}")
                    continue

    com_ids = set()
    if os.path.exists('futurology_comments.jsonl'):
        with open('futurology_comments.jsonl', 'r', encoding='utf-8') as f:
            for line in f:
                try:
                    obj = json.loads(line)
                    com_ids.add(obj['id'])
                except Exception:
                    print(f"Error parsing line: {line}")
                    continue

    # Download new posts (submissions)
    print('Fetching new submissions...')
    submissions = list(subreddit.new(limit=None))
    print(f"Downloaded {len(submissions)} new submissions.")

    # Download new comments
    print('Fetching new comments...')
    comments = list(subreddit.comments(limit=None))
    print(f"Downloaded {len(comments)} new comments.")

    # Append new submissions with body, avoid duplicates
    new_subs = 0
    with open('futurology_submissions.jsonl', 'a', encoding='utf-8') as f:
        for submission in submissions:
            if submission.id not in sub_ids:
                obj = {
                    'id': submission.id,
                    'title': submission.title,
                    'author': str(submission.author),
                    'created_utc': submission.created_utc,
                    'body': submission.selftext.replace('\t', ' ').replace('\n', ' ')
                }
                f.write(json.dumps(obj, ensure_ascii=False) + '\n')
                new_subs += 1

    # Append new comments with submission id, avoid duplicates
    new_coms = 0
    with open('futurology_comments.jsonl', 'a', encoding='utf-8') as f:
        for comment in comments:
            if comment.id not in com_ids:
                obj = {
                    'id': comment.id,
                    'author': str(comment.author),
                    'body': comment.body.replace('\t', ' ').replace('\n', ' '),
                    'created_utc': comment.created_utc,
                    'submission_id': comment.submission.id
                }
                f.write(json.dumps(obj, ensure_ascii=False) + '\n')
                new_coms += 1

    print(f"Appended {new_subs} new submissions and {new_coms} new comments.")

if __name__ == "__main__":
    while True:
        scrape()
        print("###############################################################################################")
        time.sleep(600)  