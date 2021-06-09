select
  decorated_post_tags.real_submission_id as submission_id,
  decorated_post_tags.id as tag_id,
  coalesce(decorated_posts.resolved_filename, '') as submission_filename,
  coalesce(decorated_post_tags.survey_question_token, '') as survey_question_token,
  coalesce(tags.name, '') as tag_name,
  coalesce(tags.full_number, '') as tag_number,
  coalesce(decorated_post_tags.text, '') as "quote",
  decorated_post_tags.start_char as start_char,
  decorated_post_tags.end_char as end_char,
  case
    when users.email is not null then
      users.email
    when decorated_post_tags.auto_tag_job_id is not null then
      format('ML-%s', decorated_post_tags.auto_tag_job_id)
    else
      ''
  end as tagger,
  to_char(decorated_post_tags.created_at, 'YYYY-MM-DD" "HH24:MI:SS "UTC"') as tagtime
from decorated_post_tags
left outer join decorated_posts
  on decorated_post_tags.real_submission_id = decorated_posts.id
left outer join tags
  on decorated_post_tags.tag_id = tags.id
left outer join users
        on decorated_post_tags.tagger_id = users.id

      where (decorated_posts.consultation_id = 1122
          and decorated_posts.state != 'archived'

      order by decorated_post_tags.id;
