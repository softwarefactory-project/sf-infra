# podman-pruner : a role to install image pruning cron

## Variables

- `pruning_time`: How often to run the pruning
  :default: daily

- `user`: On which user to install the cron
  :default: zuul

- `extra_attributes`: Extra attributes to pass to the podman image prune command
  :default: ""
