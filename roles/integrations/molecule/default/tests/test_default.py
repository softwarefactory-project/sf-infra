import os

import testinfra.utils.ansible_runner

testinfra_hosts = testinfra.utils.ansible_runner.AnsibleRunner(
    os.environ['MOLECULE_INVENTORY_FILE']
).get_hosts('all')


test_cfg = """
logging:
  level: DEBUG
broker:
  url: softwarefactory-project.io
  port: 1883
projects:
  - DLRN:
      TaigaIO:
        project: morucci-software-factory
        auth:
          user: sfbot0
          password: xxxxx
  - config:
      TaigaIO:
        project: morucci-software-factory
        auth:
          user: sfbot0
          password: xxxxx
  - software-factory:
      TaigaIO:
        project: morucci-software-factory
        auth:
          user: sfbot0
          password: xxxxx
  - repoxplorer:
      TaigaIO:
        project: morucci-software-factory
        auth:
          user: sfbot0
          password: xxxxx
  - logreduce:
      TaigaIO:
        project: morucci-software-factory
        auth:
          user: sfbot0
          password: xxxxx
  - rpms:
      TaigaIO:
        project: morucci-software-factory
        auth:
          user: sfbot0
          password: xxxxx"""


def test_defaults_config(host):
    f = host.file('/etc/firehooks/default.yaml')

    assert f.exists
    assert f.user == 'firehook'
    assert f.group == 'firehooks'

    default = f.content_string
    assert default == test_cfg


def test_service_running(host):
    f = host.service('firehooks')
    assert f.is_enabled
    assert f.is_running
