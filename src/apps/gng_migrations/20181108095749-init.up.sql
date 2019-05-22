create type gdr as enum ('Male', 'Female', 'Unknow')

--;;

create table folk (
  id serial primary key,
  name varchar(40),
  gender gdr,
  birthday date,
  password varchar(40),
  register_date timestamp

)

--;;

-- canot support money failed to parse to Double
create table item (
  id serial primary key,
  name varchar(40),
  detail varchar,
  product_date timestamp,
  bought_date timestamp,
  public_date timestamp,
  img_urls varchar(300) array,
  price numeric(10, 3),
  location point,
  owner_id integer references folk(id)

)



--;;

