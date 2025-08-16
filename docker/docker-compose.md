GOTCHA: depends_on will not wait for db and redis to be “ready” before starting
web - only until they have been started

docker-compose up docker-compose exec --user postgres db psql docker-compose run
web bundle exec rake db:setup
