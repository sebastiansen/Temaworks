package temaworks.handling;
// MOTHERFUCKER!!!
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.util.GenericForwardComposer;

import clojure.lang.RT;

public class JavaInterop extends GenericForwardComposer {
	@Override
	public void doAfterCompose(Component comp) throws Exception {
		super.doAfterCompose(comp);
		RT.loadResourceScript("temaworks/handling/management.clj");
		RT.var("temaworks.handling.management", "init").invoke(this, comp, desktop, page);
	}
	public void sendAlert(String s){
		alert(s);
	//(new org.zkoss.zul.Longbox()).setva
		//(new java.util.List()).ge
	}
}
