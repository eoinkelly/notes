copy (
  with
  decorated_post_tags as (
    -- Create a "decorated" version of the "post_tags" table which has some
    -- extra columns which are useful for our exported report.
    --
    -- A SubmissionTag is associated with a Submission either directly (when
    -- taggable_type='Submission, taggable_id=123) or indirectly (when
    -- taggable_type='SurveyAnswer, taggable_id=123 then the SurveyAnswer id=123
    -- is associated to the Submission). This table resolves those paths into the
    -- `real_submission_id` column.
    --
    --
    --     |      Column           |              Type              |
    --     |-----------------------+--------------------------------+
    --     | id                    | bigint                         |
    --     | tag_id                | bigint                         |
    --     | start_char            | integer                        |
    --     | end_char              | integer                        |
    --     | created_at            | timestamp(6) without time zone |
    --     | updated_at            | timestamp(6) without time zone |
    --     | text                  | character varying              |
    --     | tagger_id             | bigint                         |
    --     | taggable_type         | character varying              |
    --     | taggable_id           | bigint                         |
    --     | confidence_level      | double precision               |
    --     | auto_tag_job_id       | bigint                         |
    --     | survey_question_token | bigint                         | (extra)
    --     | real_submission_id    | bigint                         | (extra)
    --
    select
      post_tags.*,
      survey_questions.token as survey_question_token,
      case
        when post_tags.taggable_type = 'Submission' then post_tags.taggable_id
        when post_tags.taggable_type = 'SurveyAnswer' then survey_answers.submission_id
      end
      as real_submission_id
    from post_tags
      left outer join survey_answers
        on post_tags.taggable_type = 'SurveyAnswer' and post_tags.taggable_id = survey_answers.id
      left outer join survey_questions
        on survey_answers.survey_question_id = survey_questions.id
  ),

  decorated_posts as (
    -- Create a "decorated" version of the "posts" table which has some
    -- extra columns which are useful for our exported report.
    --
    --     |           Column           |              Type              |
    --     +----------------------------+--------------------------------+
    --     | id                         | bigint                         |
    --     | consultation_id            | bigint                         |
    --     | created_at                 | timestamp(6) without time zone |
    --     | updated_at                 | timestamp(6) without time zone |
    --     | text                       | text                           |
    --     | description                | text                           |
    --     | state                      | character varying              |
    --     | submitted_at               | timestamp without time zone    |
    --     | channel                    | character varying              |
    --     | source                     | character varying              |
    --     | name                       | character varying              |
    --     | email_address              | character varying              |
    --     | address                    | character varying              |
    --     | phone_number               | character varying              |
    --     | query_type                 | character varying              |
    --     | anonymise                  | character varying              |
    --     | submitter_type             | character varying              |
    --     | exemplar                   | boolean                        |
    --     | maori_perspective          | boolean                        |
    --     | pacific_perspective        | boolean                        |
    --     | high_impact_stakeholder    | boolean                        |
    --     | high_relevance_stakeholder | boolean                        |
    --     | age_bracket                | character varying              |
    --     | ethnicity                  | character varying              |
    --     | gender                     | character varying              |
    --     | file_hash                  | character varying              |
    --     | survey_id                  | bigint                         |
    --     | resolved_filename          | character varying              | (extra)
    --
    select
      posts.*,
      -- coalesce() is a PostgreSQL function which returns the first arg which is
      -- not null. This allows us to set `resolved_filename` to the
      -- Survey#original_file if it exists and otherwise fallback to
      -- Submission#file
      coalesce(asb_survey_file.filename, asb_sub_file.filename) as resolved_filename
    from posts
      -- join posts --> active_storage_attachements --> active_storage_blobs,
      -- filtering to get only attachments attached via `Submission#file`
      left outer join active_storage_attachments as asa_sub_file
              on asa_sub_file.record_type = 'Submission'
                and asa_sub_file.name = 'file'
                and asa_sub_file.record_id = posts.id
      left outer join active_storage_blobs as asb_sub_file
              on asa_sub_file.blob_id = asb_sub_file.id
      -- join posts -- surveys --> active_storage_attachements --> -- active_storage_blobs
      -- filtering to get only attachments attached to -- `Survey#original_file`
      left outer join surveys
              on posts.survey_id = surveys.id
      left outer join active_storage_attachments as asa_survey_file
              on asa_survey_file.record_type = 'Survey'
                and asa_survey_file.name = 'original_file'
                and asa_survey_file.record_id = surveys.id
      left outer join active_storage_blobs as asb_survey_file
              on asa_survey_file.blob_id = asb_survey_file.id
  )

  -- This is our "main" query which uses the temporary tables defined above in the
  -- `WITH ...` clause. The goal of this query is to return results which can be
  -- **directly** added to the CSV without any further processing.
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

  where decorated_posts.consultation_id = 1122
      and decorated_posts.state != 'archived'

  order by decorated_post_tags.id
) to stdout with (
  format csv,
  header true,
  encoding 'UTF8',
  force_quote *
);
