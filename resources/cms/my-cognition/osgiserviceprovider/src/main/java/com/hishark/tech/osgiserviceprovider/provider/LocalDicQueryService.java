package com.hishark.tech.osgiserviceprovider.provider;

import java.util.HashMap;

import com.hishark.tech.osgiservice.service.DicQueryService;

public class LocalDicQueryService implements DicQueryService {
	static HashMap<String, String> dic = new HashMap<String, String>();
	{
		dic.put("shark", "A fish in ocean");
	}

	public String queryWord(String word) {
		return dic.get(word);
	}

}
