-- :name insert-item :! :n
-- :doc insert a item return affected row count
insert into item (name, detail)
values (:name, :detail)


-- :name insert-items :! :n
-- :doc insert mul items with :tuple* parameter type
insert into item (name, detail)
values :tuple*:items


-- :name insert-folk :! :n
-- :doc insert a folk
insert into folk (name, password)
values (:name, :password)


-- :name folk-by-name :? :1
-- :doc get folk by name
select * from folk
where name = :name


-- :name item-by-id :? :1
-- :doc get item by id
select * from item
where id = :id


-- :name items-by-ids :? :*
-- :doc get items by ids
select * from item
where id in (:v*:ids)


-- :name items :? :*
-- :doc get items via conditions
select * from item

