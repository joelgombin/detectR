mysql -P 3307 -h 127.0.0.1 -u rstudio -p piwik -e "SELECT visit_entry_idaction_url, visit_last_action_time, location_latitude, location_longitude, config_device_type, config_device_model FROM log_visit" -B | gzip > m.tsv.gz