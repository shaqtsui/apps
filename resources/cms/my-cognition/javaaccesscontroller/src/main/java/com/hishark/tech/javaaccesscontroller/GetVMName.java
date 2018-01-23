package com.hishark.tech.javaaccesscontroller;

import java.security.AccessController;

public class GetVMName {

	/**
	 * @param args
	 */
	public void getName() {
		AccessController.doPrivileged(new GetVMNamePrivilegedAction());
		String name = System.getProperty("java.vm.name");
		System.out.println("VM NAME out of Privileged Code:" + name);
	}

}
