package com.hishark.tech.jpa;


import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;

public class DataHandler {

	/**
	 * @param args
	 */
	public static void main(String[] args) {

		EntityManagerFactory f = Persistence.createEntityManagerFactory("BizData");
		EntityManager em = f.createEntityManager();

		UserInfo shark = new UserInfo();
		shark.setEmail("fu.cheng.xu@hishark.com");
		shark.setName("Shark");
		shark.setBirthDay(new Date());
		System.out.println(shark.getId());

		AddressInfo addr = new AddressInfo();
		addr.setAddressDetails("aaaaaaaaaaaa");
		List l = new ArrayList();
		l.add(addr);
		AddressInfo addr1 = new AddressInfo();
		addr1.setAddressDetails("bbbbbbbbb");
		l.add(addr1);
		shark.setAddr(l);
		em.getTransaction().begin();
		em.persist(addr);
		em.persist(addr1);
		em.persist(shark);
		System.out.println(shark.getId());
		em.getTransaction().commit();

		Query query = em.createQuery("select ui from UserInfo ui where ui.name='Shark' and ui.birthDay > {ts '2014-12-05 00:00:00.000'}");
		List result = query.getResultList();
		System.out.println(result);

	}

}
