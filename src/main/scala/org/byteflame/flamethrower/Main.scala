package org.byteflame.flamethrower

import android.{content, os, app, widget, graphics, view, util}

// Background Service
// Checks if default device is set, if not sends notification
// Checks if bluetooth is on, else waits until turned on
// Waits until the default device is seen then attempts to connect
// Lazily polls for current gasoline level
// On gasoline level < some percentage, sends notification
// On notification click, opens google maps with intent for local
// gas stations
class Background extends app.Service {
	val lowGasoline : Double = 10
	val binder = new os.Binder

	def handleCommand(intent : content.Intent) {
		// Do work
	}

	override def onStartCommand(intent : content.Intent, flags : Int,
			startId : Int) : Int = {
		handleCommand(intent)

		app.Service.START_STICKY
	}

	override def onBind(intent: content.Intent) : os.IBinder = {
		binder
	}
}

// On Boot Reciever
// Recieves boot broadcast at device startup
// Used to start a background service
class OnBootReceiver extends content.BroadcastReceiver {
	override def onReceive(context : content.Context,
			intent : content.Intent) {

		if (intent.getAction() == "android.intent.action.BOOT_COMPLETED") {

			// Start background service
			context.startService(new content.Intent(context, classOf[Background]))
		}
	}
}

// Main Activity
// User's view of the application
// Should provide access to app settings and perhaps
// control of the background service
class Main extends app.Activity {
	override def onCreate(instance : os.Bundle) = {
		super.onCreate(instance)

		// Instantiate UI
		// UI should include a way to set the default device
		// by both entering the MAC address or by selecting
		// a paired device or seen device.

		var layout = new widget.LinearLayout(this)
		layout.addView((() => {
			var b = new widget.Button(this)
			b.setText("Test")
			b.setBackgroundColor(graphics.Color.RED)
			b.setOnClickListener(
				new view.View.OnClickListener {
					def onClick(v : view.View) {
						widget.Toast.makeText(
							getApplicationContext(),
							"Test!",
							widget.Toast.LENGTH_SHORT
						).show() }})
			b
		})())

		setContentView(layout)
	}
}
