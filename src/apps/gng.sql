-- :name insert-item :! :n
-- :doc insert a item return affected row count
insert into item (name, detail)
values (:name, :detail)


-- :name insert-items :! :n
-- :doc insert mul items with :tuple* parameter type
insert into item (name, detail)
values :tuple*:items


-- :name item-by-id :? :1
-- :doc get item by id
select * from item
where id = :id


-- :name items-by-ids :? :*
-- :doc get items by ids
select * from item
where id in (:v*:ids)



