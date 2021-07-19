SELECT
    *
FROM
    (
        SELECT
            b.starttime AS starttime,
            concat_ws(' ', m.firstname, m.surname) AS member,
            f.name AS facility,
            CASE
            WHEN m.memid = 0 THEN f.guestcost * b.slots
            ELSE f.membercost * b.slots
            END
                AS cost
        FROM
            cd.members AS m
            JOIN cd.bookings AS b ON m.memid = b.memid
            JOIN cd.facilities AS f ON b.facid = f.facid
    )
        AS real_costs
WHERE
    CAST(starttime AS DATE) = '2012-09-14'::DATE AND cost > 30
ORDER BY
    cost DESC;
