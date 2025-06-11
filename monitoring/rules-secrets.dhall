let Secret = ../tools/secret-age.dhall

in  Secret.renderSecretAlerts
      [ Secret::{
        , match = "k1s_.*"
        , description =
            ''
            To update the secret:

            - k1s_token: Use `uuidgen`.
            - k1s_key, k1s_crt and k1s_chain: Create a new certifcate.

            After merging the sf-infra change, make sure to restart nodepool-launcher service to update the client.
            ''
        }
      , Secret::{
        , match = "grafana_pass"
        , description =
            ''
            To update the secret use `uuidgen`.
            ''
        }
      , Secret::{
        , match = "zuul_pagure_token_.*"
        , description =
            ''
            You need to connect as the zuul user on both pages (see password in bitwarden) and renew the API keys there.
            Restart Zuul with the updated config once the keys have been regenerated.
            ''
        , -- The token are valid for 180 days
          expiry = Secret.day * 180
        }
      , Secret::{
        , match = "zuul_gitlab_com_connection_api_token"
        , description =
            ''
            You need to connect as softwarefactoryio-zuul-bot on gitlab.com (see password in bitwarden) and renew a personal access token.
            Make sure you choose an expiry date of a year when creating the token.
            ''
        , -- The token are valid for 365 days (if the expiry date is properly set)
          expiry = Secret.day * 365
        }
      , Secret::{
        , match =
            "cloud_softwarefactory_service_account_token|tenant_builds_zuul_api_url_es_password|tenant_builds_zuul_api_url_es_password|internal_users_admin_password|internal_users_kibanaserver_password|users_admin_password|users_logstash_password|users_curator_password|users_zuul_password|users_admin_password|users_logstash_password|users_curator_password|users_zuul_password|clair_psk_key|postgresql_user|postgresql_password|postgresql_admin_password|redis_password|database_secret_key|secret_key|quay_users_admin_password|quay_users_admin_token|quay_users_sf_password|quay_users_tripleo_password|quay_users_tripleo_token|quay_users_zuul_password|quay_organizations_tripleo_tripleomastercentos9_token|quay_organizations_tripleo_tripleowallabycentos9_token|quay_organizations_tripleo_tripleozedcentos9_token|quay_organizations_tripleo_openstack-k8s-operators_token|quay_organizations_tripleo_podified-main-centos9_token|quay_organizations_tripleo_podified-master-centos9_token|quay_organizations_tripleo_podified-zed-centos9_token|quay_organizations_tripleo_podified-antelope-centos9_token|quay_organizations_tripleo_podified-master-centos10_token|install_server_sf_key|install_server_rdo_key|rhel_activationkey|rhel_org_id|rhel_pool_ids|loki_auth_config_gateway_password|sf_grafana_password|vexxhost_prometheus_password|vexxhost_prometheus_url|vexxhost_prometheus_user|nextgen_openshift_prometheus_url|vexxhost_ceph_s3_access|vexxhost_ceph_s3_secret|gateway_password|vexxhost_nodepool_rdo_password|ibm_bm3_password|ibm_bm3_url|ibm_bm4_password|ibm_bm4_url|zuul_github_com_connection_webhook_token|zuul_github_com_connection_app_key|zuul_github_com_connection_api_token|zuul_gitlab_com_connection_webhook_token|centOSInfra_zuul_ssh_priv"
        , description =
            ''
            Unknown secret expired. Please rotate and add instructions to monitoring/rules-secrets.dhall
            ''
        }
      ]
