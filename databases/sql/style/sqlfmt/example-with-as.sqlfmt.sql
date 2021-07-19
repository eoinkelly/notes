WITH
    decorated_post_tags
        AS (
            SELECT
                post_tags.*,
                survey_questions.token AS survey_question_token,
                CASE
                WHEN post_tags.taggable_type = 'Submission'
                THEN post_tags.taggable_id
                WHEN post_tags.taggable_type = 'SurveyAnswer'
                THEN survey_answers.submission_id
                END
                    AS real_submission_id
            FROM
                post_tags
                LEFT JOIN survey_answers ON
                        post_tags.taggable_type = 'SurveyAnswer'
                        AND post_tags.taggable_id = survey_answers.id
                LEFT JOIN survey_questions ON
                        survey_answers.survey_question_id = survey_questions.id
        ),
    decorated_posts
        AS (
            SELECT
                posts.*,
                COALESCE(asb_survey_file.filename, asb_sub_file.filename)
                    AS resolved_filename
            FROM
                posts
                LEFT JOIN active_storage_attachments AS asa_sub_file ON
                        asa_sub_file.record_type = 'Submission'
                        AND asa_sub_file.name = 'file'
                        AND asa_sub_file.record_id = posts.id
                LEFT JOIN active_storage_blobs AS asb_sub_file ON
                        asa_sub_file.blob_id = asb_sub_file.id
                LEFT JOIN surveys ON posts.survey_id = surveys.id
                LEFT JOIN active_storage_attachments AS asa_survey_file ON
                        asa_survey_file.record_type = 'Survey'
                        AND asa_survey_file.name = 'original_file'
                        AND asa_survey_file.record_id = surveys.id
                LEFT JOIN active_storage_blobs AS asb_survey_file ON
                        asa_survey_file.blob_id = asb_survey_file.id
        )
SELECT
    decorated_post_tags.real_submission_id AS submission_id,
    decorated_post_tags.id AS tag_id,
    COALESCE(decorated_posts.resolved_filename, '') AS submission_filename,
    COALESCE(decorated_post_tags.survey_question_token, '')
        AS survey_question_token,
    COALESCE(tags.name, '') AS tag_name,
    COALESCE(tags.full_number, '') AS tag_number,
    COALESCE(decorated_post_tags.text, '') AS quote,
    decorated_post_tags.start_char AS start_char,
    decorated_post_tags.end_char AS end_char,
    CASE
    WHEN users.email IS NOT NULL THEN users.email
    WHEN decorated_post_tags.auto_tag_job_id IS NOT NULL
    THEN format('ML-%s', decorated_post_tags.auto_tag_job_id)
    ELSE ''
    END
        AS tagger,
    to_char(decorated_post_tags.created_at, 'YYYY-MM-DD" "HH24:MI:SS "UTC"')
        AS tagtime
FROM
    decorated_post_tags
    LEFT JOIN decorated_posts ON
            decorated_post_tags.real_submission_id = decorated_posts.id
    LEFT JOIN tags ON decorated_post_tags.tag_id = tags.id
    LEFT JOIN users ON decorated_post_tags.tagger_id = users.id
WHERE
    decorated_posts.consultation_id = 1122
    AND decorated_posts.state != 'archived'
ORDER BY
    decorated_post_tags.id;
