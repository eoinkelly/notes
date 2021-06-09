COPY ( WITH decorated_post_tags AS (
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
    SELECT
      post_tags.*,
      survey_questions.token AS survey_question_token,
      CASE WHEN post_tags.taggable_type = 'Submission' THEN
        post_tags.taggable_id
      WHEN post_tags.taggable_type = 'SurveyAnswer' THEN
        survey_answers.submission_id
      END AS real_submission_id
    FROM
      post_tags
    LEFT OUTER JOIN survey_answers ON post_tags.taggable_type = 'SurveyAnswer'
    AND post_tags.taggable_id = survey_answers.id
  LEFT OUTER JOIN survey_questions ON survey_answers.survey_question_id = survey_questions.id),
decorated_posts AS (
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
  SELECT
    posts.*,
    -- coalesce() is a PostgreSQL function which returns the first arg which is
    -- not null. This allows us to set `resolved_filename` to the
    -- Survey#original_file if it exists and otherwise fallback to
    -- Submission#file
    coalesce(asb_survey_file.filename, asb_sub_file.filename) AS resolved_filename
  FROM
    posts
    -- join posts --> active_storage_attachements --> active_storage_blobs,
    -- filtering to get only attachments attached via `Submission#file`
    LEFT OUTER JOIN active_storage_attachments AS asa_sub_file ON asa_sub_file.record_type = 'Submission'
      AND asa_sub_file.name = 'file'
      AND asa_sub_file.record_id = posts.id
    LEFT OUTER JOIN active_storage_blobs AS asb_sub_file ON asa_sub_file.blob_id = asb_sub_file.id
  -- join posts -- surveys --> active_storage_attachements --> -- active_storage_blobs
  -- filtering to get only attachments attached to -- `Survey#original_file`
    LEFT OUTER JOIN surveys ON posts.survey_id = surveys.id
    LEFT OUTER JOIN active_storage_attachments AS asa_survey_file ON asa_survey_file.record_type = 'Survey'
      AND asa_survey_file.name = 'original_file'
      AND asa_survey_file.record_id = surveys.id
    LEFT OUTER JOIN active_storage_blobs AS asb_survey_file ON asa_survey_file.blob_id = asb_survey_file.id)
-- This is our "main" query which uses the temporary tables defined above in the
-- `WITH ...` clause. The goal of this query is to return results which can be
-- **directly** added to the CSV without any further processing.
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
WHERE
  decorated_posts.consultation_id = 1122
  AND decorated_posts.state != 'archived'
ORDER BY
  decorated_post_tags.id)
  TO stdout WITH (
    format csv,
    header TRUE,
    ENCODING 'UTF8',
    force_quote *);
