# Delete ibm triple-standalone cloud

0. remove nodepool config for nodepool launcher and nodepool-builder first

1. remove bm and instances from ./vars/infra-rdo/ibm-baremetal.dhall

`
diff --git a/vars/infra-rdo/ibm-baremetal.dhall b/vars/infra-rdo/ibm-baremetal.dhall
index 09979e1..d58b689 100644
--- a/vars/infra-rdo/ibm-baremetal.dhall
+++ b/vars/infra-rdo/ibm-baremetal.dhall
@@ -75,22 +75,6 @@ let mk_cloud =
             cloud.baremetal_name
         ]

-let baremetal02 =
-      let prefix = "ibm-"
-
-      in  Cloud::{
-          , baremetal_name = "baremetal02." ++ rdo_domain
-          , baremetal_ip = "169.60.49.233"
-          , mirror_ip = "192.168.25.35"
-          , launcher_name = prefix ++ "nodepool-launcher"
-          , launcher_ip = "192.168.25.195"
-          , executor_name = prefix ++ "ze"
-          , executor_ip = "192.168.25.127"
-          , fingergw_name = prefix ++ "zfgw"
-          , fingergw_ip = "192.168.25.80"
-          , domain = "ibm-bm2-nodepool"
-          }
-
 let baremetal03 =
       let prefix = "ibm-bm3-"

@@ -107,4 +91,4 @@ let baremetal03 =
           , domain = prefix ++ "nodepool"
           }

-in  mk_cloud baremetal02 # mk_cloud baremetal03
+in  mk_cloud baremetal03
`

2. Remove Proxy jump for each ip (needed by ansible) on vars/directory-tree.dhall

`
diff --git a/vars/directory-tree.dhall b/vars/directory-tree.dhall
index 8bf4751..10e6169 100644
--- a/vars/directory-tree.dhall
+++ b/vars/directory-tree.dhall
@@ -58,14 +58,6 @@ let sshconfig =
                   ControlPath ~/.ssh/control-%r@%h:%p
                   PubkeyAcceptedKeyTypes +ssh-rsa

-              Host 192.168.25.35
-                  ProxyJump baremetal02.rdoproject.org
-              Host 192.168.25.195
-                  ProxyJump baremetal02.rdoproject.org
-              Host 192.168.25.127
-                  ProxyJump baremetal02.rdoproject.org
-              Host 192.168.25.80
-                  ProxyJump baremetal02.rdoproject.org
               Host 192.168.25.10
                   ProxyJump baremetal03.rdoproject.org
               Host 192.168.25.11
`

3. remove zookeeper sec rules from ./vars/infra-sf/networking.dhall

`
diff --git a/vars/infra-sf/networking.dhall b/vars/infra-sf/networking.dhall
index 0f49102..44a6034 100644
--- a/vars/infra-sf/networking.dhall
+++ b/vars/infra-sf/networking.dhall
@@ -62,10 +62,6 @@ let security_groups =
         , { name = "zookeeper"
           , rules =
             [ Infra.Rule::{
-              , port = +2281
-              , remote_ip_prefix = Some "{{ ibm_bm2_ip }}/32"
-              }
-            , Infra.Rule::{
               , port = +2281
               , remote_ip_prefix = Some "{{ ibm_bm3_ip }}/32"
               }
`

4. remove host from ./playbooks/zuul/configure-private-clouds.yaml

5. remove ssh config from ./roles/infra/ssh/tasks/main.yaml

6. remove hostvars file if it exists for bm and afs instances

7. remove deployment from ./playbooks/zuul/host_vars/bridge.softwarefactory-project.io.yaml

`
diff --git a/playbooks/zuul/host_vars/bridge.softwarefactory-project.io.yaml b/playbooks/zuul/host_vars/bridge.softwarefactory-project.io.yaml
index 5c5b3a9..39bbfc8 100644
--- a/playbooks/zuul/host_vars/bridge.softwarefactory-project.io.yaml
+++ b/playbooks/zuul/host_vars/bridge.softwarefactory-project.io.yaml
@@ -3,22 +3,6 @@ cloud_image: "CentOS-7-x86_64-GenericCloud-2111"
 cloud_image_url: "https://cloud.centos.org/centos/7/images/{{ cloud_image }}.qcow2"
 keypair: "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHj3R7JGtLkSD9h+E6JNHGdZ+SStJx+wyQsE7DK41t+Au7mfTJb12zjxBO4OwzAJbVnudN86BPYI0aHuY7ZpZ8+yitcFOBxQdczcGNistIaOHNb7bp0aPpVTYkHEYo3IlsDgPYcMVsvwOpIIDfi8wSQo3p4FNTtOc9>
 standalone_deployments:
-  - baremetal_name: ibm-bm2
-    baremetal_ip: 169.60.49.233
-    cloud: ibm-bm2-nodepool
-    password: "{{ ibm_bm2_nodepool.password }}"
-    tripleo_repos_branch: train
-    namespace: quay.io/tripleotraincentos8
-    name_prefix: centos-binary-
-    servers:
-      - name: mirror.regionone.ibm-bm2-nodepool.rdoproject.org
-        flavor: afs
-      - name: ibm-nodepool-launcher.softwarefactory-project.io
-        flavor: m1.medium
-      - name: ibm-ze.softwarefactory-project.io
-        flavor: m1.large
-      - name: ibm-zfgw.softwarefactory-project.io
-        flavor: m1.small
   - baremetal_name: ibm-bm3
     baremetal_ip: 169.60.49.226
     cloud: ibm-bm3-nodepool
`

8. remove entry from ./playbooks/zuul/templates/clouds.yaml.j2

9. remove zuul secret for the bm instance from zuul.d/secrets.yaml and zuul/jobs.yaml

10. remove cert on ./playbooks/group_vars/ibm-baremetal-nodepool.yaml

11. Update configuration and commit

`
podman run --rm -it --volume $PWD:/workspace/sf-infra/:Z --volume ~/.cache:/workspace/.cache:Z quay.io/software-factory/zuul-worker-dhall /bin/bash -c "cd /workspace/sf-infra && make"
`
