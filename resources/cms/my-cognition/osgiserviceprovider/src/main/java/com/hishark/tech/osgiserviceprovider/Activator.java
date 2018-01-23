package com.hishark.tech.osgiserviceprovider;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.hishark.tech.osgiservice.service.DicQueryService;
import com.hishark.tech.osgiserviceprovider.provider.LocalDicQueryService;


public class Activator implements BundleActivator {

	private static BundleContext context;

	static BundleContext getContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
	 * )
	 */
	public void start(BundleContext bundleContext) throws Exception {
		Activator.context = bundleContext;
		System.out.println("com.hishark.osgi.localdicquery.Activator.start()");
		context.registerService(DicQueryService.class.getName(),
				new LocalDicQueryService(), null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		System.out.println("com.hishark.osgi.localdicquery.Activator.stop()");

		Activator.context = null;

	}

}
