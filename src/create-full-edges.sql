drop table full;

create table a as select * from vertices;
create table b as select * from vertices;

create table full as
   select b.degree_id, a.school_name, a.school_country, b.school_name, b.school_country, edges.degree_year
      from a
      left join edges on 
         edges.advisor_id = a.person_id
      left join b on
         edges.person_id = b.person_id
      where a.degree_id<>'' and b.degree_id<>'';

drop table a;
drop table b;
