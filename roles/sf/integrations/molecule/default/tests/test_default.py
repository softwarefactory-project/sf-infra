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
hooks:
  SFTaigaIO:
    - project: DLRN|config|software-factory|repoxplorer|logreduce|rpms
      auth:
        username: sfbot0
        password: xxxxx
      taiga_project: morucci-software-factory
"""


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
