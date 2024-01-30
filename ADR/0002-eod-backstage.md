---
status: proposed
date: 2024-01-30
---

# Engineer On Duty (EOD) backstage dashboard definition

## Context and Problem Statement

As the operator of the sf-infra powered services, I would like to know the operational status of the platform.
At a glance, I need an overview of the high level metrics that matters the most to our users.

Most of the information is already present in several data-sources, but they are not readily available in a clear visualization.
This ADR is not concerned with the actual metrics being used, this is purely about how to display them.

## Considered Options

The purpose of this ADR is to pick a tool to define the dashboard.

* Grafana dashboard editor
* Grafonnet
* Cue with TypeScript
* Dhall

## Decision Outcome

Chosen option: "Dhall", because it comes out best (see below).


### Consequences

* Good, because we have a formal tool to define and extend dashboards.
* Bad, because we need to maintain a dashboard using a rigorous tool, as opposed to creating ad hoc visualization with the editor UI.


## Pros and Cons of the Options

### Grafana dashboard editor

Doc: https://grafana.com/docs/grafana/latest/panels-visualizations/panel-editor-overview/

* Good, because it is available by default.
* Good, because it is interactive.
* Bad, because it is not maintainable through code review, the generated JSON is not exactly human readable and it contains lots of repetition.
* Bad, because it is tedious to make repetitive panels. For example, to align all the axis so that the timestamp match, you need to set the 'axisWidth' on every individual panels.

### Grafonnet

Doc: https://grafana.github.io/grafonnet/index.html

* Good, because it is the recommended tool.
* Good, because it brings a programmatic approach to dashboard lifecycle management.
* Neutral, because we don't have tooling in Zuul for it.
* Bad, because it does not have type safety.

### Cue with TypeScript

Doc: https://github.com/grafana/grafana/issues/60590

* Good, because Cue is the source of truth for Grafana data type, see: https://github.com/grafana/grafana/blob/main/kinds/dashboard/dashboard_kind.cue
* Good, because it is statically typed.
* Good, because it is usable from TypeScript.
* Bad, because we don't have tooling or expertise with TypeScript.

### Dhall

Doc: https://dhall-lang.org/

* Good, because it is a mature and general purpose language.
* Good, because we already use it for Prometheus, nodepool and sf-infra instances.
* Bad, because it is slow for complex type like dhall-grafana.
* Bad, because using native JSON is not ergonomic (see: https://github.com/dhall-lang/dhall-lang/issues/1027). This means that new kind of panels needs to be converted to a dhall type and added to the PType union.

Here is an example implementation: https://github.com/softwarefactory-project/sf-infra/blob/master/roles/sf/grafana/files/infra/Panels.dhall
