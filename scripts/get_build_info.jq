# Copyright (C) 2022 Red Hat
# SPDX-License-Identifier: Apache-2.0
#
# A `jq` script to Process a zuul status page to extract worker information.
# This produce a list of command ready to be executed on the scheduler.
#
def get_inf(inf):
    (if inf.id == null then ("--ref " + inf.ref) else ("--change " + inf.id) end)
  + " --tenant " + $tenant
  + " --pipeline " + inf.pipeline
  + " --project " + inf.project
  + " # " + inf.worker.job + " running on " + inf.worker.host;

def get_worker(finger_url):
  if (finger_url|type) == "string" then
    (finger_url | split("finger://") | .[1] | split(":") | .[0])
  else
    "unknown"
  end;

# Get pipelines
.pipelines[] |
  # Keep the pipeline name
  .name as $pipeline |
  # Dig in the change queues
  .change_queues[] | .heads[][] |
  # Make the inf record:
    {
      pipeline: $pipeline,
      ref: .ref,
      id: .id,
      project: .project_canonical,
      worker: (.jobs[] | { result: .result, job: .name, host: get_worker(.finger_url) })
    } as $inf |
  if $inf.worker.result == null then
  [
    # Output the dequeue command
    "dequeue " + get_inf($inf),
    # And the enqueue command
    (if $inf.id == null then "enqueue-ref " else "enqueue " end) + get_inf($inf)
  ] |
  # Make a flat list of commands
  join("\n")
  else "" end
