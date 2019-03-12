# pour récupérer les IP en clair
SELECT inet_ntoa(conv(hex(location_ip), 16, 10)) as ip FROM piwik_log_visit;