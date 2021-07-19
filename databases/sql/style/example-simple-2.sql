select *
from (

    select
      b.starttime as starttime,
      concat_ws(' ', m.firstname, m.surname) as member,
      f.name as facility,
      case
        when m.memid = 0 then
          f.guestcost * b.slots
        else
          f.membercost * b.slots
      end as cost
    from cd.members as m
    join cd.bookings as b
      on m.memid = b.memid
    join cd.facilities as f
      on b.facid = f.facid

) as real_costs
where cast(starttime as date) = '2012-09-14'::date
  and cost > 30
order by cost desc
    ;

