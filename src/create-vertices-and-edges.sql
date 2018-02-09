drop table edges;

create table edges (
   advisor_id INT,
   person_id INT,
   degree_id INT,
   degree_year INT
);

insert into edges
   select degree_advisor.person_id, degree.person_id, degree.id, degree.year
      from degree_advisor
      left join degree on
         degree_advisor.degree_id = degree.id
      where 
         degree.year<>'';

drop table vertices;

create table vertices (
   person_id INT,
   degree_id INT,
   degree_year INT,
   school_id INT, 
   school_name TEXT,
   school_country TEXT
);

insert into vertices 
   select degree.person_id, degree.id, degree.year, degree_school.school_id, school.name, school.country
      from degree
      left join degree_school on
        degree_school.degree_id = degree.id
      left join school on
        school.id = degree_school.school_id
      where
        degree.year<>'' and school.name<>'' and school.id<>'';
        
