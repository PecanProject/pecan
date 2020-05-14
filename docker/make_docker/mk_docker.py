#!/usr/bin/env python3
"""
Author : Ken Youens-Clark <kyclark@arizona.edu>
Date   : 2020-05-13
Purpose: Create Docker containers
"""

import argparse
import itertools
import json
import os
import sys
import docker


# --------------------------------------------------
def get_args():
    """Get command-line arguments"""

    parser = argparse.ArgumentParser(
        description='Create Docker containers',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('-c',
                        '--config',
                        help='Config JSON',
                        metavar='FILE',
                        type=argparse.FileType('rt'),
                        default='docker.json')

    return parser.parse_args()


# --------------------------------------------------
def main():
    """Make a jazz noise here"""

    args = get_args()
    config = json.load(args.config)
    client = docker.from_env()

    if 'builds' not in config:
        sys.exit(f'"{args.config.name}" missing "builds"')

    images = client.images.list()
    tags = itertools.chain.from_iterable(map(lambda img: img.tags, images))
    for build in config['builds']:
        tag = build['tag']
        if tag in tags:
            print(f'Already built "{tag}"')
            continue

        print(f'Building "{tag}"')


# --------------------------------------------------
if __name__ == '__main__':
    main()
