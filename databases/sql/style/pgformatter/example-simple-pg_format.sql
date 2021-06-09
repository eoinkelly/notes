SELECT
  decorated_post_tags.real_submission_id AS submission_id,
  decorated_post_tags.id AS tag_id,
  coalesce(decorated_posts.resolved_filename, '') AS submission_filename,
  coalesce(decorated_post_tags.survey_question_token, '') AS survey_question_token,
  coalesce(tags.name, '') AS tag_name,
  coalesce(tags.full_number, '') AS tag_number,
  coalesce(decorated_post_tags.text, '') AS "quote",
  decorated_post_tags.start_char AS start_char,
  decorated_post_tags.end_char AS end_char,
  CASE WHEN users.email IS NOT NULL THEN
    users.email
  WHEN decorated_post_tags.auto_tag_job_id IS NOT NULL THEN
    format('ML-%s', decorated_post_tags.auto_tag_job_id)
  ELSE
    ''
  END AS tagger,
  to_char(decorated_post_tags.created_at, 'YYYY-MM-DD" "HH24:MI:SS "UTC"') AS tagtime
FROM
  decorated_post_tags
  LEFT OUTER JOIN decorated_posts ON decorated_post_tags.real_submission_id = decorated_posts.id
  LEFT OUTER JOIN tags ON decorated_post_tags.tag_id = tags.id
  LEFT OUTER JOIN users ON decorated_post_tags.tagger_id = users.id
WHERE (decorated_posts.consultation_id = 1122
  AND decorated_posts.state != 'archived'
ORDER BY
  decorated_post_tags.id;
