#!/usr/bin/env python3

import argparse
import daemon
import lockfile
import gear
import json
import os
import requests
import sys
import time
import urllib
import yaml


file_to_check = ['job-output.txt.gz', 'job-output.txt',
                 'postci.txt', 'postci.txt.gz',
                 'var/log/extra/logstash.txt',
                 'var/log/extra/logstash.txt.gz',
                 'var/log/extra/errors.txt',
                 'var/log/extra/errors.txt.gz']

config = '''
files:
  - name: job-output.txt
    tags:
      - console
      - console.html
  # tripleo logs
  - name: postci.txt
    tags:
      - console
      - postci
  - name: var/log/extra/logstash.txt
    tags:
      - console
      - postci
  - name: var/log/extra/errors.txt
    tags:
      - console
      - errors
'''


def get_arguments():
    parser = argparse.ArgumentParser(
        description="Fetch and push last Zuul CI job logs into gearman."
                    "Example: \n\tzuul-job-scrapper.py --tenant openstack\n\n"
                    "or more advanced version that scrap specified job name:"
                    "\n\tzuul-job-scrapper.py --job-name "
                    "tripleo-ci-centos-8-containers-multinode "
                    "--gearman-port 4732 --gearman-server elk.rdoproject.org "
                    "--zuul-api-url https://zuul.opendev.org/api/tenant/ "
                    "--checkpoint-file /tmp/results-checkpoint.txt "
                    "--tenant openstack --foreground",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument(
        "-u", "--zuul-api-url",
        help="URL for Zuul API. Defaults to: "
             "https://softwarefactory-project.io/zuul/api/tenant",
        default="https://softwarefactory-project.io/zuul/api/tenant/")
    parser.add_argument("-t", "--tenant",
                        help="Tenant on which CI job is running."
                             "Defaults to: local",
                        default="local")
    parser.add_argument("--job-name",
                        help="CI job name. If not set it would scrap "
                             "every latest builds")
    parser.add_argument('-s', '--gearman-server', help='Gearman host addresss')
    parser.add_argument('-p', '--gearman-port', help='Gearman listen port. '
                        'Defaults to 4731.', default=4731)
    parser.add_argument('--sleep', help='Time before checking new builds. '
                        'Defaults to 120', default=120)
    parser.add_argument("--insecure",
                        action="store_false",
                        help="Skip validating SSL cert")
    parser.add_argument('--checkpoint-file', help='File that will keep'
                        'information about last uuid timestamp for a job.'
                        'Defaults to /tmp/results-checkpoint.txt',
                        default='/usr/local/zuul-scrapper-checkpoint.txt')
    parser.add_argument('--ignore-checkpoint', help='Ignore last job uuid '
                        'that is set in --checkpoint-file',
                        action="store_true")
    parser.add_argument("--foreground", action='store_true',
                        help="Run in the foreground.")
    parser.add_argument("--pidfile",
                        default="/var/run/zuul-scrapper/scrapper.pid",
                        help="PID file to lock during daemonization.")
    args = parser.parse_args()
    return args


def get_last_job_results(zuul_url, tenant, job_name, insecure):
    if job_name:
        url = "%s/%s/builds?job_name=%s" % (zuul_url, tenant, job_name)
    else:
        url = "%s/%s/builds" % (zuul_url, tenant)
    jobs_result = requests.get(url, verify=insecure)
    if jobs_result.status_code != 200:
        print("Provided URL is wrong or Zuul API is down")
        sys.exit(1)
    return jobs_result.json()


def sort_results(job_results):
    dump_datetime = {}
    sorted_results = []
    for i, v in enumerate(job_results):
        # script should not raise an error when job is ongoing or
        # job fails on post job, so log_url value is not present.
        if not v["end_time"] or not v["log_url"]:
            continue
        dump_datetime[i] = time.mktime(time.strptime(v['end_time'],
                                                     "%Y-%m-%dT%H:%M:%S"))
    sorted_datetime = {k: v for k, v in sorted(dump_datetime.items(),
                                               key=lambda item: item[1])}
    for k, v in sorted_datetime.items():
        sorted_results.append(job_results[k])
    return sorted_results


def check_if_uuid_in_job_result(job_results, last_uuid):
    return filter(lambda result: result['uuid'] == last_uuid, job_results)


def get_last_result_uuid(job_name, checkpoint_file, ignore_checkpoint):
    if not os.path.isfile(checkpoint_file) or ignore_checkpoint:
        return

    with open(checkpoint_file) as f:
        for line in f:
            key, val = line.split()
            # FIXME: add checking uuid if no job_name specified
            if (job_name and key == job_name) or (not job_name and
                                                  key == "builds"):
                return val


def write_last_job_uuid(job_name, checkpoint_file, job_uuid):
    job_in_file = False
    checkpoint_text = []
    # Assume, that no --job-name specified, so write "builds"
    # as a key to set checkpoint.
    if not job_name:
        job_name = "builds"

    try:
        if not os.path.isfile(checkpoint_file):
            open(checkpoint_file, "a").close()

        with open(checkpoint_file, 'r') as f:
            checkpoint_text = f.readlines()
            with open(checkpoint_file, 'w') as f_out:
                for line in checkpoint_text:
                    key, val = line.split()
                    if key == job_name:
                        checkpoint_text = [jobuuid.replace(val, job_uuid)
                                           for jobuuid in checkpoint_text]
                        f_out.writelines(checkpoint_text)
                        job_in_file = True
                        break

        if not job_in_file:
            with open(checkpoint_file, "a") as f:
                # if job-name or builds key was not found, write old content
                f.writelines(checkpoint_text)
                f.write("%s %s" % (job_name, job_uuid))
    except Exception as e:
        raise("Can not write status to the checkpoint file %s" % e)


def cleanup_results(job_results, job_uuid):
    if not job_uuid or not check_if_uuid_in_job_result(job_results, job_uuid):
        return job_results
    else:
        for result in job_results:
            if result['uuid'] == job_uuid:
                return job_results
            else:
                job_results.pop(job_results.index(result))


def check_specified_files(job_result):
    available_files = []
    for f in file_to_check:
        if not job_result['log_url']:
            continue
        response = requests.get("%s%s" % (job_result['log_url'], f))
        if response.status_code == 200:
            available_files.append(f)
    return available_files


class LogMatcher(object):
    def __init__(self, server, port, success, log_url, host_vars):
        self.client = gear.Client()
        self.client.addServer(server, port)
        self.hosts = host_vars
        self.success = success
        self.log_url = log_url

    def submitJobs(self, jobname, files, result):
        self.client.waitForServer(90)
        ret = []
        for f in files:
            output = self.makeOutput(f, result)
            output = json.dumps(output).encode('utf8')
            job = gear.TextJob(jobname, output)
            self.client.submitJob(job, background=True)
            ret.append(dict(handle=job.handle,
                            arguments=output))
        return ret

    def makeOutput(self, file_object, result):
        output = {}
        output['retry'] = False
        output['event'] = self.makeEvent(file_object, result)
        output['source_url'] = output['event']['fields']['log_url']
        return output

    def makeEvent(self, file_object, result):
        out_event = {}
        tags = []
        out_event["fields"] = self.makeFields(file_object, result)
        config_files = yaml.safe_load(config)
        for f in config_files['files']:
            if (file_object in f['name'] or file_object.replace('.gz', '')
                    in f['name']):
                tags = f['tags']
                break

        out_event["tags"] = [file_object] + tags
        return out_event

    def makeFields(self, filename, result):
        fields = {}
        fields["build_node"] = 'zuul-executor'
        fields["filename"] = filename
        fields["build_name"] = result['job_name']
        fields["build_status"] = 'SUCCESS' if result['result'] else 'FAILURE'
        fields["project"] = result['project']
        fields["voting"] = int(result['voting'])
        fields["build_set"] = result["buildset"]
        fields["build_queue"] = result['pipeline']
        fields["build_ref"] = result['ref']
        fields["build_branch"] = result.get('branch', 'UNKNOWN')
        fields["build_zuul_url"] = "N/A"

        if 'change' in result:
            fields["build_change"] = result['change']
            fields["build_patchset"] = result['patchset']
        elif 'newrev' in result:
            fields["build_newrev"] = result.get('newrev', 'UNKNOWN')

        fields["node_provider"] = 'local'
        log_url = urllib.parse.urljoin(result['log_url'], filename)
        fields["log_url"] = log_url
        fields["tenant"] = result["tenant"]

        if 'executor' in result and 'hostname' in result['executor']:
            fields["zuul_executor"] = result['executor']['hostname']

        fields["build_uuid"] = result['buildset']['uuid']

        return fields


def main(args):
    while True:
        job_results = get_last_job_results(
            args.zuul_api_url, args.tenant, args.job_name, args.insecure
        )
        sorted_results = sort_results(job_results)
        last_result_uuid = get_last_result_uuid(args.job_name,
                                                args.checkpoint_file,
                                                args.ignore_checkpoint)
        cleaned_results = cleanup_results(sorted_results, last_result_uuid)

        if cleaned_results:
            for job_result in cleaned_results:
                results = dict(files=[], jobs=[], invocation={})

                # add missing informations
                job_result['tenant'] = args.tenant

                lmc = LogMatcher(args.gearman_server, args.gearman_port,
                                 job_result['result'], job_result['log_url'],
                                 {})
                results['files'] = check_specified_files(job_result)

                for handle in lmc.submitJobs("push-log", results['files'],
                                             job_result):
                    results['jobs'].append(handle)

            write_last_job_uuid(args.job_name, args.checkpoint_file,
                                sorted_results[-1]['uuid'])
        else:
            print("Nothing to do!")

        if args.foreground:
            break
        else:
            time.sleep(args.sleep)


if __name__ == "__main__":
    args = get_arguments()
    if args.foreground:
        main(args)
    else:
        with daemon.DaemonContext(stdout=sys.stdout, stderr=sys.stderr,
                                  pidfile=lockfile.FileLock(args.pidfile)):
            main(args)
