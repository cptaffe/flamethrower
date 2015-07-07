package org.byteflame.flamethrower

import android.{content, os, app, widget, graphics, view, util, bluetooth}
import scala.concurrent._

// FlameThrower App
// The goal of this application is to provide proactive
// recommendations for nearby gas stations on low gasoline
// detected via OBD-II bluetooth elm327 interface.

// Wrapper class for notifications
object Notification {
	def cancel(context : content.Context, id : Int) {
		context.getSystemService(content.Context.NOTIFICATION_SERVICE) match {
			case n : app.NotificationManager => n.cancel(id)
			case _ => throw new ClassCastException
		}
	}
}

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

	// NOTE: SharedPreferences are thread safe, but not process safe.

	// Get default Mac Address
	def getMacAddress(context : content.Context) : Option[String] = context.getSharedPreferences(SETTINGS_FILE_KEY, content.Context.MODE_PRIVATE).getString(MAC_ADDRESS, null) match {
		case null => None
		case a => Some(a)
	}

	// Set default Mac Address
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
	val notifIds = new Enumeration {
		val EnableBt, UnsupportedDev, SelectBt = Value
	}
}

class Background extends app.Service {

	// Notifications that could be sent
	private def unsupportedDevNotice(lc : content.Context) : Notification = new Notification(lc,
		Background.notifIds.UnsupportedDev.id,
		"Unsupported Device",
		"FlameThrower requires bluetooth to search for devices."
	)

	private def unselectedDevNotice(lc : content.Context) : Notification = new Notification(lc,
		Background.notifIds.SelectBt.id,
		"Choose Device",
		"Tell FlameThrower which device to connect to.",
		app.PendingIntent.getActivity(this, 0,
			new content.Intent(this, classOf[ChooseDevice]),
			app.PendingIntent.FLAG_UPDATE_CURRENT)
	)


	// Use Channels to communicate blocking tasks.
	private val addrChan = new Channel[Option[String]]
	private val btEnableChan = new Channel[Boolean]

	// Callbacks notify blocking tasks
	def alertBluetoothEnabled = btEnableChan.write(true)
	def alertAddressSelected(addr : Option[String]) = addrChan.write(addr)

	var workHandler : os.Handler = null

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
				override def run = {
					os.Looper.prepare
					workHandler = new os.Handler
					workHandler.post(new Runnable {
						def run = setup(lc)
					})
					os.Looper.loop
				}
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
			// Notify user of unsupported device
			case None => unsupportedDevNotice(lc).display
			// Bluetooth Adapter avaliable and enabled
			case ba if ba.isDefined => {
				(SavedData.getMacAddress(lc) match {
					// No default address set
					case None => {
						// Notify user that no default address is set
						// Link to activity that allows user to choose
						unselectedDevNotice(lc).display

						// Wait for address to be sent
						addrChan.read match {
							case addr if addr.isDefined => {
								// Save new addr
								SavedData.setMacAddress(lc, addr.get)
								addr
							}
							case _ => None
						}
					}
					case addr => addr
				}) match {
					case addr if addr.isDefined => {
						// Test toast to show workingness
						widget.Toast.makeText(
							lc,
							addr.get,
							widget.Toast.LENGTH_SHORT
						).show
					}
				}
			}
		}

	// Blocking function, sets up bluetooth
	private def getBluetoothAdapter(ba : bluetooth.BluetoothAdapter) : Option[bluetooth.BluetoothAdapter] = {
		// This closure checks if bluetooth is enabled or can be
		// If bluetooth cannot be enabled, it returns false
		// Otherwise it waits until enabled and returns true
		// on success
	 	ba match {
			// Device does not support bluetooth
			// Send notification to user detailing failure.
			case null => None
			case ba if ba.isEnabled => Some(ba)
			case ba if !ba.isEnabled => {
				// Bluetooth is not enabled
				// Send notification to user allowing for user to enable
				// bluetooth via click.
				new Notification(this,
					Background.notifIds.EnableBt.id,
					"Enable Bluetooth?",
					"FlameThrower requires bluetooth to search for devices.",
					app.PendingIntent.getActivity(this, 0,
						new content.Intent(this, classOf[EnableBluetooth]),
						app.PendingIntent.FLAG_UPDATE_CURRENT)
				).display

				// Wait here until bluetooth is enabled
				// False could be returned which should cause failure
				if (btEnableChan.read) Some(ba) else None
			}
		}
	}
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
		Notification.cancel(this, Background.notifIds.EnableBt.id)
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
		Notification.cancel(this, Background.notifIds.SelectBt.id)
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
