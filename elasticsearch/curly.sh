# Zsh helper for querying elasticsearch
#
# Usage:
#   # load this function
#   $ source ./elasticurl.sh
#
#   # use it
#   $ elasticurl /
#   $ elasticurl /_search
#
func elasticurl() {
  curl --silent --user "elastic:changeme" "http://localhost:9200${1}" | jq --color-output .
}
