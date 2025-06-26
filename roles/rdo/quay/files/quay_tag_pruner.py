#!/usr/bin/env python3
#   Copyright Red Hat, Inc. All Rights Reserved.
#
#   Licensed under the Apache License, Version 2.0 (the "License"); you may
#   not use this file except in compliance with the License. You may obtain
#   a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
#   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
#   License for the specific language governing permissions and limitations
#   under the License.

import argparse
import logging
import logging.config
import json
import os
import requests
import sys
import time
import urllib.parse
import yaml

from datetime import datetime
from datetime import timedelta


def setup_logging(level):
    log_config = """
    ---
    version: 1
    formatters:
        console:
            format: '%(asctime)s %(levelname)s %(name)s: %(message)s'
    handlers:
        console:
            class: logging.StreamHandler
            formatter: console
            level: {level}
            stream: ext://sys.stdout
    loggers:
        {name}:
            handlers:
                - console
            level: {level}
            propagate: 0
    root:
      handlers:
        - console
      level: {level}
    """.format(name=os.path.basename(__file__), level=level).lstrip()
    logging.config.dictConfig(yaml.safe_load(log_config))


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--debug', help='Enable debug logging',
                        action='store_true')
    parser.add_argument('--confirm', help='Confirm the deletion of the tags'
                                          ' (Dry-run by default)',
                        action='store_true')
    parser.add_argument('--days', help='Delete tags older than N days',
                        default=7, type=int)
    parser.add_argument('--keeplist', help='Comma-separated list of tags to'
                                           ' keep, even if older.',
                        default=None)
    parser.add_argument('--allow-empty-keeplist',
                        help='Whether if it is valid passing'
                             ' an empty keep-list.',
                        default=False, type=bool)
    # We will be using the QUAY API endpoints from
    # https://docs.quay.io/api/swagger/
    parser.add_argument('--api-url', help='Registry API url.',
                        default='https://quay.rdoproject.org/api/v1')
    parser.add_argument('--token', help='Access token with permissions on the'
                                        ' namespace.', required=True)
    parser.add_argument('namespace', help='Namespace to prune old tags in')
    args = parser.parse_args()
    return args


def get_repos_by_namespace(log, namespace, api_url):
    done = False
    next_page = None
    return_repos = []

    # We need to consider pagination in this request, but it is done
    # differently when compared to get_tags_by_repo :?
    while not done:
        url = urllib.parse.urljoin(
            api_url + '/',
            'repository?namespace=%s&public=true' % namespace)
        if next_page:
            url = url + '&next_page=%s' % next_page

        r = requests.get(url)
        if r.status_code >= 400:
            log.critical("Couldn't get repositories %s: %s", namespace, r.text)
        r.raise_for_status()
        repos = json.loads(r.text)
        if 'repositories' in repos:
            return_repos.extend(repos['repositories'])
            if 'next_page' in repos:
                next_page = repos['next_page']
            else:
                done = True
        else:
            done = True

    return return_repos


def get_tags_by_repo(log, namespace, repo, api_url):
    done = False
    page = 1
    return_tags = []

    # We need to consider pagination in this request
    while not done:
        url = urllib.parse.urljoin(
            api_url + '/',
            'repository/%s/%s/tag/?onlyActiveTags=true&page=%s' %
            (namespace, repo, page))
        r = requests.get(url)
        if r.status_code >= 400:
            log.critical("Couldn't get tags for %s/%s: %s", namespace, repo, r.text)
        r.raise_for_status()
        tags = json.loads(r.text)

        if 'tags' in tags:
            return_tags.extend(tags['tags'])
            if tags['has_additional']:
                page += 1
            else:
                done = True
        else:
            done = True

    return return_tags


def expire_tag(namespace, repo, tag, exp_ts, token, api_url):
    url = urllib.parse.urljoin(
        api_url + '/',
        'repository/%s/%s/tag/%s' % (namespace, repo, tag))
    r = requests.put(url,
                     headers={'content-type': 'application/json',
                              'Authorization': 'Bearer %s' % token},
                     data='{"expiration": %s}' % exp_ts)
    r.raise_for_status()


def main():
    args = get_args()
    level = "DEBUG" if args.debug else "INFO"
    setup_logging(level)
    log = logging.getLogger(os.path.basename(__file__))
    log.debug("Arguments: %s" % json.dumps(args.__dict__))

    if not args.confirm:
        log.warning("{0} RUNNING IN DRY RUN MODE {0}".format("*" * 10))
    else:
        log.warning("{0} CONFIRMED DELETION OF TAGS {0}".format("*" * 10))
        time.sleep(5)

    MAX_AGE = 86400 * args.days
    NOW = datetime.utcnow()
    TOMORROW = NOW + timedelta(days=1)

    keeplist = set()
    if args.keeplist is not None:
        if len(args.keeplist) != 0:
            keeplist = set(args.keeplist.split(','))
        elif not args.allow_empty_keeplist:
            log.critical('An empty keeplist has been provided.'
                         ' Consider using --allow-empty-keeplist=True')
            sys.exit(1)
    else:
        # If there are no keeplisted tags, we can remove more than we want
        log.critical("No keeplist has been provided.")
        sys.exit(1)

    log.info("Deleting tags from %s older than %s days" % (args.namespace,
                                                           args.days))

    repos = get_repos_by_namespace(log, args.namespace, args.api_url)
    for repo in repos:
        log.debug("Found repo: %s" % repo['name'])
        try:
            tags = get_tags_by_repo(log, args.namespace, repo['name'], args.api_url)
        except Exception:
            tags = []
        for tag in tags:
            name = tag['name']
            if name not in keeplist:
                start = datetime.fromtimestamp(tag['start_ts'])
                if 'end_ts' in tag:
                    end = tag['end_ts']
                else:
                    end = None
                log.debug("Found tag name: %s, start: %s, end: %s" % (
                    name, start, end))

                delta = NOW - start
                # Uncomment the line below if you want extra debug
                # log.debug("Delta: %s, max_age: %s" %
                #           (delta.total_seconds(), MAX_AGE))
                # We expire a tag if it is too old
                # AND it does not have an expiry date already
                if delta.total_seconds() > MAX_AGE and not end:
                    try:
                        log.info("%s:%s to be deleted: %s" % (repo['name'],
                                 name, delta))
                        if args.confirm:
                            expire_tag(args.namespace, repo['name'],
                                       name, int(TOMORROW.timestamp()),
                                       args.token, args.api_url)
                    except Exception as e:
                        log.error("Failed to expire tag %s:%s: %s" %
                                  (repo['name'], name, e))

    log.info("Finished.")


if __name__ == "__main__":
    main()
