package org.byteflame.flamethrower

import android.{content, os, app, widget, graphics, view, util, bluetooth}
import scala.concurrent._

// FlameThrower App
// The goal of this application is to provide proactive
// recommendations for nearby gas stations on low gasoline
// detected via OBD-II bluetooth elm327 interface.

// Wrapper class for notifications
class Notification(context : content.Context, id : Int, title : String, text : String, intent : app.PendingIntent = null) {

	def display = context.getSystemService(content.Context.NOTIFICATION_SERVICE) match {
		case n : app.NotificationManager => n.notify(id,
			(() => {
				var note = new app.Notification.Builder(context)
					.setSmallIcon(android.R.drawable.alert_dark_frame)
					.setContentTitle(title)
					.setContentText(text)

				if (intent != null) note.setContentIntent(intent)

				note.build
			})())
		case _ => throw new ClassCastException
	}
}

// Abstracts the getting and setting of saved data
object SavedData {
	val SETTINGS_FILE_KEY = "org.byteflame.flamethrower.USER_SETTINGS"
	val MAC_ADDRESS = "MAC_ADDRESS"

	def getMacAddress(context : content.Context) : String = context.getSharedPreferences(SETTINGS_FILE_KEY, content.Context.MODE_PRIVATE).getString(MAC_ADDRESS, null)

	def setMacAddress(context : content.Context, addr : String) = ((e : content.SharedPreferences.Editor) => {
		e.putString(MAC_ADDRESS, addr)
		e.commit
	})(context.getSharedPreferences(SETTINGS_FILE_KEY, content.Context.MODE_PRIVATE).edit)
}

// Background Service
// Checks if default device is set, if not sends notification
// Checks if bluetooth is on, else waits until turned on
// Waits until the default device is seen then attempts to connect
// Lazily polls for current gasoline level
// On gasoline level < set percentage, sends notification
// On notification click, opens google maps with intent for local
// gas stations
object Background {
	val enableBluetoothNotificationId = 1
	val unsupportedDeviceNotificationId = 2
	val selectBluetoothDeviceNotificationId = 3
}

class Background extends app.Service {

	// Use Channels to communicate blocking tasks.
	// Notify listener of default address
	val addrChan = new Channel[String]

	// Notify listener of bluetooth enabling
	val btEnableChan = new Channel[Boolean]

	// Binder class
	// Provides access to the Background service via
	// getService.
	class Binder extends os.Binder {
        def getService : Background = Background.this
	}
	val binder : os.IBinder = new Binder

	override def onStartCommand(intent : content.Intent, flags : Int,
			startId : Int) : Int = {

		// Local Context for notification building from runnable
		this match {
			case lc : content.Context => new Thread(new Runnable {
				// Block until bluetooth adapter is enabled
				override def run = setup(lc)
			}).start
			case _ => throw new ClassCastException
		}

		app.Service.START_STICKY
	}

	override def onBind(intent: content.Intent) : os.IBinder = {
		// Calling startService will ensure that the service
		// is running before a bind occurs
		util.Log.d("FlameThrower", "Starting service from Bind")
		startService(new content.Intent(this, classOf[Background]))
		binder
	}

	private def setup(lc : content.Context) = getBluetoothAdapter(bluetooth.BluetoothAdapter.getDefaultAdapter()) match {
		 	// Bluetooth unavaliable
			case null => {
				// Notify user of unsupported system with
				// notification.
				new Notification(lc,
					Background.unsupportedDeviceNotificationId,
					"Unsupported Device",
					"FlameThrower requires bluetooth to search for devices."
				).display
			}
			// Bluetooth Adapter avaliable and enabled
			case ba : bluetooth.BluetoothAdapter => {
				SavedData.getMacAddress(lc) match {
					// No default address set
					case null => {
						// Notify user that no default address is set
						// Link to activity that allows user to choose
						new Notification(lc,
							Background.selectBluetoothDeviceNotificationId,
							"Choose Device",
							"Tell FlameThrower which device to connect to.",
							app.PendingIntent.getActivity(this, 0,
								new content.Intent(this, classOf[ChooseDevice]),
								app.PendingIntent.FLAG_UPDATE_CURRENT)
						).display

							// Wait for address to be sent
							addrChan.read match {
								case null => null
								case addr : String => {
									// Save new addr
									SavedData.setMacAddress(lc, addr)
									addr
								}
							}
					}
					case addr : String => addr
				}
			} match {
				case null => ()
				case addr : String => {
					// Here we have the address either
					// by waiting for the user to select or by
					// getting it from saved data

					os.Looper.prepare

					// Test toast to show workingness
					widget.Toast.makeText(
						lc,
						addr,
						widget.Toast.LENGTH_SHORT
					).show

					os.Looper.loop
				}
			}
		}

	// Blocking function, sets up bluetooth
	private def getBluetoothAdapter(ba : bluetooth.BluetoothAdapter) : bluetooth.BluetoothAdapter = {
		// This closure checks if bluetooth is enabled or can be
		// If bluetooth cannot be enabled, it returns false
		// Otherwise it waits until enabled and returns true
		// on success
	 	ba match {
			// Device does not support bluetooth
			// Send notification to user detailing failure.
			case null => null
			case ba if ba.isEnabled => ba
			case ba if !ba.isEnabled => {
				// Bluetooth is not enabled
				// Send notification to user allowing for user to enable
				// bluetooth via click.
				new Notification(this,
					Background.enableBluetoothNotificationId,
					"Enable Bluetooth?",
					"FlameThrower requires bluetooth to search for devices.",
					app.PendingIntent.getActivity(this, 0,
						new content.Intent(this, classOf[EnableBluetooth]),
						app.PendingIntent.FLAG_UPDATE_CURRENT)
				).display

				// Wait here until bluetooth is enabled
				// False could be returned which should cause failure
				if (btEnableChan.read) ba else null
			}
		}
	}

	def alertBluetoothEnabled = {
		util.Log.d("FlameThrower", "I HAVE BEEN SUMMONED, THE MIGHTY alertBluetoothEnabled")
		btEnableChan.write(true)
	}

	def alertAddressSelected(addr : String) = addrChan.write(addr)
}

// BindActivity
// Binds to Background service on startup
// and unbinds on stop
class BindActivity extends app.Activity {
	val TAG = "FlameThrower"

	// Couldn't get channels to work, so I hacked this together
	val serviceChan = new Channel[Background]

	val mConnection = new content.ServiceConnection {
		override def onServiceConnected(c : content.ComponentName, b : os.IBinder) = b match {
				case bi : Background#Binder => {
					serviceChan.write(bi.getService)
				}
				case _ => throw new ClassCastException
			}

		override def onServiceDisconnected(c : content.ComponentName) = ()
	}

	override def onStart = {
		super.onStart

		bindService(new content.Intent(this, classOf[Background]), mConnection, content.Context.BIND_AUTO_CREATE)
	}

	override def onStop = {
		super.onStop

		unbindService(mConnection)
	}
}

// Enable Bluetooth Activity
// Opened via notification action, enables bluetooth
class EnableBluetooth extends BindActivity {
	// Thread signals when it has finished
	val doneChan = new Channel[Boolean]

	override def onCreate(instance : os.Bundle) = {
		super.onCreate(instance)

		// Enable Bluetooth
		bluetooth.BluetoothAdapter.getDefaultAdapter().enable()

		val textview = new widget.TextView(this)
		setContentView((() => {
			var layout = new widget.LinearLayout(this)
			layout.addView((() => {
				textview.setText("Turning on Bluetooth...")
				textview
			})())
			layout
		})())

		val doTasks = new os.Handler

		this match {
			case lc : content.Context => new Thread(new Runnable {
				def run = {
					// Read service and send bluetooth alert
					util.Log.d(TAG, "Attempting to getService")
					serviceChan.read.alertBluetoothEnabled
					util.Log.d(TAG, "Have gotten service")
					doneChan.write(true)

					doTasks.post(new Runnable {
						// Alert user of bluetoth enabledness
						def run = textview.setText("Bluetooth is Enabled!")
					})
				}
			}).start
			case _ => throw new ClassCastException
		}

		// Cancel notification
		getSystemService(content.Context.NOTIFICATION_SERVICE) match {
			case n : app.NotificationManager => n.cancel(Background.enableBluetoothNotificationId)
			case _ => throw new ClassCastException
		}
	}
}

// Choose Device Activity
// Opened via notification action, allows user to pick device
class ChooseDevice extends BindActivity {
	override def onCreate(instance : os.Bundle) = {
		super.onCreate(instance)

		setContentView((() => {
			var layout = new widget.LinearLayout(this)
			layout.addView((() => {
				var b = new widget.Button(this)
				b.setText("Test")
				b.setBackgroundColor(graphics.Color.RED)
				b.setOnClickListener(
					new view.View.OnClickListener {
						def onClick(v : view.View) {
							// Wait for service to bind before using service
							new Thread(new Runnable {
								def run = {
									// TODO: send bluetooth device mac address
								}
							}).start
							}})
				b
			})())
			layout
		})())

		// Cancel notification
		getSystemService(content.Context.NOTIFICATION_SERVICE) match {
			case n : app.NotificationManager => n.cancel(Background.selectBluetoothDeviceNotificationId)
			case _ => throw new ClassCastException
		}
	}
}

// On Boot Reciever
// Recieves boot broadcast at device startup
// Used to start a background service
class OnBootReceiver extends content.BroadcastReceiver {
	// Android boot completed intent action
	val BOOT_COMPLETED = "android.intent.action.BOOT_COMPLETED"

	override def onReceive(context : content.Context,
			intent : content.Intent) = {

		if (intent.getAction() == BOOT_COMPLETED) {

			// Start background service
			context.startService(new content.Intent(context, classOf[Background]))
		}
	}
}

// Main Activity
// User's view of the application
// Should provide access to app settings and perhaps
// control of the background service
class Main extends BindActivity {

	override def onCreate(instance : os.Bundle) = {
		super.onCreate(instance)

		// Instantiate UI
		// UI should include a way to set the default device
		// by both entering the MAC address or by selecting
		// a paired device or seen device.

		setContentView((() => {
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
			layout
		})())
	}
}
