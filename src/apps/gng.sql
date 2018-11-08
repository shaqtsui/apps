-- :name insert-item :! :n
-- :doc insert a item return affected row count
insert into items (name, detail)
values (:name, :detail)


-- :name insert-items :! :n
-- :doc insert mul items with :tuple* parameter type
insert into items (name, detail)
values :tuple*:items


-- :name item-by-id :? :1
-- :doc get item by id
select * from items
where id = :id


-- :name items-by-ids :? :*
-- :doc get items by ids
select * from items
where id in (:v*:ids)



