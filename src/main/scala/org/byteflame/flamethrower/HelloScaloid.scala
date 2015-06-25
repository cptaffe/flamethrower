package org.byteflame.flamethrower

import android.{content, os, app}

// Background Service
// Checks if default device is set, if not sends notification
// Checks if bluetooth is on, else waits until turned on
// Waits until the default device is seen then attempts to connect
// Lazily polls for current gasoline level
// On gasoline level < some percentage, sends notification
// On notification click, opens google maps with intent for local
// gas stations
object FlameService extends app.Service {
	val lowGasoline : Double = 10
	val binder = new os.Binder

	override def onBind(intent: content.Intent) : os.IBinder = {
		binder
	}
}
