let Infra = ../../Infra/package.dhall

let Configs = Infra.Instance.getBackups ../../vars/instances.dhall

let indexed = (../../Infra/Prelude.dhall).List.indexed

in  { servers =
        Infra.Backup.mkServers
          (Infra.Backup.mkCron (indexed Infra.Backup.Type Configs))
    , devices = [ "/dev/nvme1n1" ]
    , lvm = True
    , vg_name = "data"
    , lv_name = "backup"
    , mountpoint = "/mnt/{{ vg_name }}_{{ lv_name }}"
    , fqdn = "backup.rdoproject.org"
    , email = "softwarefactory-operations-team@redhat.com"
    , bind_mounts =
      [ { source = "{{ mountpoint }}/backup", dest = "/var/lib/backup" } ]
    }
