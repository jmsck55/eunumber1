-- Copyright (c) 2016-2021 James Cook

-- Class file

-- class data

integer baseId = 0
object free_list = {}
export object privateData = {}

export function getNewId()
	integer id
	if length(free_list) then
		id = free_list[1]
		free_list = free_list[2..$]
		return id
	end if
	privateData = append(privateData, {})
	baseId += 1
	return baseId
end function

--export function find_object(object data, integer start = 1)
--	integer f
--	f = find(data, privateData, start)
--	return f
--end function

export procedure replace_object(integer id, object data)
	if id > 0 then
		privateData[id] = data
	end if
end procedure

public function new_object_from_data(object data)
	object id = getNewId()
	replace_object(id, data)
	return id
end function

public procedure delete_object(integer id)
	if id > 0 then
		privateData[id] = {}
		free_list = append(free_list, id)
	end if
end procedure

export function get_data_from_object(integer id, object default = {})
	if id > 0 then
		return privateData[id]
	else
		return default
	end if
end function

public procedure store_object(integer id_dst, integer id_src)
	replace_object(id_dst, get_data_from_object(id_src))
end procedure

public function clone_object(object id)
	object ret_id = getNewId()
	store_object(ret_id, id)
	return ret_id
end function

