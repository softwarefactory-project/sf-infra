# Copyright (C) 2022 Red Hat
# SPDX-License-Identifier: Apache-2.0
#
# A `jq` script to Process a zuul status page to extract hang build information.
#
# Run like this:
#
# curl -s https://softwarefactory-project.io/zuul/api/tenant/rdoproject.org/status | jq --arg tenant rdoproject.org --arg now $(date "+%s") --arg max_age_hour 10 -r -f ./get_hang_build.jq | tee restart-hang-build.sh


def get_inf(inf):
    (if inf.id == null then ("--ref " + inf.ref) else ("--change " + inf.id) end)
  + " --tenant " + $tenant
  + " --pipeline " + inf.pipeline
  + " --project " + inf.project
  + " # age: " + (inf.age|tostring)
  ;

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
      age: ((($now|tonumber) - (.enqueue_time / 1000)) / 3600),
    } as $inf |
  if $inf.age > ($max_age_hour|tonumber) then
    ["zuul-client dequeue " + get_inf($inf),
     "zuul-client " + (if $inf.id == null then "enqueue-ref " else "enqueue " end) + get_inf($inf)
    ] | join("\n")
  else "" end
