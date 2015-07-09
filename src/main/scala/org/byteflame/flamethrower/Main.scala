package org.byteflame.flamethrower

import android.{content, os, app, widget, graphics, view, util, bluetooth}
import android.support.v7
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

	def cancel = Notification.cancel(context, id)
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

	def clearMacAddress(context : content.Context) = ((e : content.SharedPreferences.Editor) => {
		e.clear
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
	object notifIds extends Enumeration {
		type notifIds = Value
		val EnableBt, UnsupportedDev, SelectBt = Value
	}
}

class Background extends app.Service {

	// Notifications that could be sent
	private def unsupportedDevNotice = new Notification(this,
		Background.notifIds.UnsupportedDev.id,
		"Unsupported Device",
		"FlameThrower requires bluetooth to search for devices.")

	private def unselectedDevNotice = new Notification(this,
		Background.notifIds.SelectBt.id,
		"Choose Device",
		"Tell FlameThrower which device to connect to.",
		app.PendingIntent.getActivity(this, 0,
			new content.Intent(this, classOf[ChooseDevice]),
			app.PendingIntent.FLAG_UPDATE_CURRENT))

	private def disabledBluetoothNotice = new Notification(this,
		Background.notifIds.EnableBt.id,
		"Enable Bluetooth?",
		"FlameThrower requires bluetooth to search for devices.",
		app.PendingIntent.getActivity(this, 0,
			new content.Intent(this, classOf[EnableBluetooth]),
			app.PendingIntent.FLAG_UPDATE_CURRENT))

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

		// TESTING REMOVE LATER
		SavedData.clearMacAddress(this)

		// Local Context for notification building from runnable
		this match {
			// Run handler in new thread
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
		startService(new content.Intent(this, classOf[Background]))
		binder
	}

	private def setup(lc : content.Context) = getBluetoothAdapter(bluetooth.BluetoothAdapter.getDefaultAdapter()) match {
			// Notify user of unsupported device
			case None => unsupportedDevNotice.display
			// Bluetooth Adapter avaliable and enabled
			case Some(ba) => {
				(SavedData.getMacAddress(lc) match {
					// No default address set
					case None => {
						// Notify user that no default address is set
						// Link to activity that allows user to choose
						val notif = unselectedDevNotice
						notif.display

						// Wait for address to be sent
						def lambda = addrChan.read match {
							case Some(addr) => SavedData.setMacAddress(lc, addr)
							case None => ()
						}
						// block for it to be set at least once
						lambda
						notif.cancel

						// forever keep it updated
						new Thread(new Runnable {
							def run = while (true) lambda
						}).start
					}
					case Some(_) => ()
				})

				SavedData.getMacAddress(lc) match {
					case Some(addr) => widget.Toast.makeText(
							lc,
							addr,
							widget.Toast.LENGTH_SHORT
						).show
					case None => widget.Toast.makeText(
							lc,
							"NONE!",
							widget.Toast.LENGTH_SHORT
						).show
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
				var notif = disabledBluetoothNotice
				notif.display

				// Wait here until bluetooth is enabled
				// False could be returned which should cause failure
				var b = btEnableChan.read
				notif.cancel
				if (b) Some(ba) else None
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
	override def onCreate(instance : os.Bundle) = {
		super.onCreate(instance)

		// Enable Bluetooth
		bluetooth.BluetoothAdapter.getDefaultAdapter().enable()

		val textview = new widget.TextView(this)
		setContentView((() => {
			var layout = new widget.LinearLayout(this)
			layout.setLayoutParams(new widget.LinearLayout.LayoutParams(view.ViewGroup.LayoutParams.MATCH_PARENT, view.ViewGroup.LayoutParams.MATCH_PARENT, 1))
			layout.addView((() => {
				textview.setText("Turning on Bluetooth...")
				textview.setGravity(view.Gravity.CENTER)
				textview.setLayoutParams((() => {
					var lp = new widget.LinearLayout.LayoutParams(view.ViewGroup.LayoutParams.WRAP_CONTENT, view.ViewGroup.LayoutParams.WRAP_CONTENT, 1)
					lp.gravity = view.Gravity.CENTER
					lp
				})())
				textview
			})())
			layout
		})())

		val doTasks = new os.Handler

		this match {
			case lc : content.Context => new Thread(new Runnable {
				def run = {
					// Read service and send bluetooth alert
					serviceChan.read.alertBluetoothEnabled

					doTasks.post(new Runnable {
						// Alert user of bluetoth enabledness
						def run = textview.setText("Bluetooth is Enabled!")
					})
				}
			}).start
			case _ => throw new ClassCastException
		}
	}
}

object TestAdapter {
	class ViewHolder(v : android.view.View) extends v7.widget.RecyclerView.ViewHolder(v) {
		var view = v
	}
}

class TestAdapter(chan : Channel[String], devices : Array[bluetooth.BluetoothDevice]) extends v7.widget.RecyclerView.Adapter[TestAdapter.ViewHolder] {

	object SelectedView {
		var selectedView : view.View = null

		private def doAnimations(v : view.View) = v.animate.alpha(1.0f)
		private def undoAnimations(v : view.View) = v.animate.alpha(0.5f)

		def set(v : view.View) = {
			if (selectedView != null) undoAnimations(selectedView)
			selectedView = v
			doAnimations(selectedView)
		}
	}

	override def onCreateViewHolder(parent : view.ViewGroup, viewType : Int) : TestAdapter.ViewHolder = new TestAdapter.ViewHolder(new widget.TextView(parent.getContext()))

	override def onBindViewHolder(holder : TestAdapter.ViewHolder, position : Int) = {
		((t : widget.TextView, dev : bluetooth.BluetoothDevice) => {
			t.setPadding(20, 10, 20, 10)
			t.setBackgroundColor(graphics.Color.parseColor("#e8e8e8"))
			t.setLayoutParams((() => {
				var lp = new view.ViewGroup.MarginLayoutParams(view.ViewGroup.LayoutParams.MATCH_PARENT, view.ViewGroup.LayoutParams.WRAP_CONTENT)
				lp.setMargins(5, 5, 5, 5)
				lp
			})())
			t.setText(dev.getName + "\n" + dev.getAddress)
			t.setAlpha(0.5f)
			SavedData.getMacAddress(t.getContext) match {
				case Some(addr) => if (addr == dev.getAddress) SelectedView.set(t)
				case None => ()
			}
			t.setOnClickListener(new view.View.OnClickListener {
				def onClick(v : view.View) {
					SelectedView.set(v)
					chan.write(dev.getAddress)
				}
			})
		})(holder.view match {
			case v : widget.TextView => v
			case _ => throw new ClassCastException
		}, devices(position))
	}

	override def getItemCount : Int = devices.length
}

// Choose Device Activity
// Opened via notification action, allows user to pick device
class ChooseDevice extends BindActivity {
	private val ADAPTER_VIEW_ID = 0xbada55

	override def onCreate(instance : os.Bundle) = {
		super.onCreate(instance)

		val chan = new Channel[String]

		setContentView((() => {
			var rview = new v7.widget.RecyclerView(this)
			rview.setLayoutManager(new v7.widget.LinearLayoutManager(this))
			rview.setAdapter(new TestAdapter(chan, bluetooth.BluetoothAdapter.getDefaultAdapter.getBondedDevices.toArray(new Array[bluetooth.BluetoothDevice](0))))
			rview
		})())

		new Thread(new Runnable {
			// Blocks until both service is ready and an address is
			// selected
			def run = serviceChan.read.alertAddressSelected(Some(chan.read))
		}).start
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
			layout.setGravity(view.Gravity.CENTER)
			layout.addView((() => {
				var b = new widget.Button(this)
				b.setText("Test")
				b.setBackgroundColor(graphics.Color.RED)
				b.setLayoutParams((() => {
					var lp = new widget.LinearLayout.LayoutParams(100, view.ViewGroup.LayoutParams.WRAP_CONTENT)
					lp.gravity = view.Gravity.CENTER
					lp
				})())
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
