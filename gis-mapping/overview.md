# GIS & Mapping Overview

## Tools

- PostGIS
    - https://postgis.net/
    - postgis can host tiles? openstreet map using it for tile server
        - RDS includes it in all recent versions
- pgRouting
    - https://pgrouting.org/
    - a collection of routing algorithms:
        1. All Pairs Shortest Path, Johnson's Algorithm
        1. All Pairs Shortest Path, Floyd-Warshall Algorithm
        1. Shortest Path A\*
        1. Bi-directional Dijkstra Shortest Path
        1. Bi-directional A\* Shortest Path
        1. Shortest Path Dijkstra
        1. Driving Distance
        1. K-Shortest Path, Multiple Alternative Paths
        1. K-Dijkstra, One to Many Shortest Path
        1. Traveling Sales Person
        1. Turn Restriction Shortest Path (TRSP)
        1. PostGIS raster
        - RDS includes it in PG 16+
- QGIS
    - https://www.qgis.org/en/site/
    - QGIS is a free old-school GIS tool based on PostGIS
- pgAdmin
    - pgAdmin has a geometry viewer
- books
    - PostGIS in Action
