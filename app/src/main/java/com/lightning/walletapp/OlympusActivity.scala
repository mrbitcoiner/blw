package com.lightning.walletapp

import android.support.v7.widget._
import com.lightning.walletapp.ln._
import com.thesurix.gesturerecycler._
import scala.collection.JavaConverters._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.lnutils.olympus._
import com.lightning.walletapp.lnutils.ImplicitConversions._
import android.widget.{ArrayAdapter, CheckBox, EditText, TextView}
import android.view.{Menu, MenuItem, ViewGroup}

import android.support.v7.widget.helper.ItemTouchHelper
import com.lightning.walletapp.lnutils.OlympusLogTable
import com.lightning.walletapp.helper.RichCursor
import com.lightning.walletapp.Utils.app
import android.app.AlertDialog
import scodec.bits.ByteVector
import android.os.Bundle
import android.net.Uri
import java.util.Date


class OlympusActivity extends TimerActivity with HumanTimeDisplay { me =>
  lazy val serverList = findViewById(R.id.serverList).asInstanceOf[RecyclerView]
  lazy val tokensLeft = getResources getStringArray R.array.olympus_tokens_left
  lazy val host = me

  val adapter = new GestureAdapter[Cloud, GestureViewHolder] {
    override def onCreateViewHolder(parent: ViewGroup, viewType: Int) = {
      val view = getLayoutInflater.inflate(R.layout.frag_line_double, parent, false)
      new GestureViewHolder(view)
    }

    override def onBindViewHolder(holder: GestureViewHolder, pos: Int) = {
      val olympusAddress = holder.itemView.findViewById(R.id.leftSideLine).asInstanceOf[TextView]
      val olympusTokens = holder.itemView.findViewById(R.id.rightSideLine).asInstanceOf[TextView]

      val cloud = getItem(pos)
      val tokensLeftHuman = app.plur1OrZero(tokensLeft, cloud.data.tokens.size)
      val finalTokensLeft = if (cloud.isAuthEnabled) tokensLeftHuman else tokensLeft.last
      olympusAddress setText Uri.parse(cloud.connector.url).getHost
      olympusTokens setText finalTokensLeft.html
      holder.swipable = 1 == cloud.removable
    }
  }

  val onClick = new DefaultItemClickListener[Cloud] {
    override def onItemClick(item: Cloud, position: Int) = {
      new FormManager(updateCloud(item), olympus_edit) set item
      false
    }
  }

  def INIT(s: Bundle) = {
    me setContentView R.layout.activity_olympus
    me initToolbar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
    getSupportActionBar setTitle sets_manage_olympus
    getSupportActionBar setSubtitle olympus_actions

    adapter setData olympusWrap.clouds.asJava
    adapter setDataChangeListener new GestureAdapter.OnDataChangeListener[Cloud] {
      override def onItemReorder(item: Cloud, fromPos: Int, targetPos: Int) = onUpdate
      override def onItemRemoved(item: Cloud, position: Int) = onUpdate
    }

    serverList setAdapter adapter
    serverList setHasFixedSize true
    serverList setLayoutManager new LinearLayoutManager(me)
    serverList addOnItemTouchListener new RecyclerItemTouchListener(onClick)

    new GestureManager.Builder(serverList)
      .setSwipeEnabled(true).setLongPressDragEnabled(true)
      .setDragFlags(ItemTouchHelper.UP | ItemTouchHelper.DOWN)
      .setSwipeFlags(ItemTouchHelper.LEFT).build
  }

  def onUpdate = LNParams.db txWrap {
    val updated: Vector[Cloud] = adapter.getData.asScala.toVector
    for (removed <- olympusWrap.clouds diff updated) olympusWrap.remove(removed.identifier)
    for (cloud \ order <- updated.zipWithIndex) olympusWrap.addServer(cloud, order)
    for (cloud \ order <- updated.zipWithIndex) olympusWrap.updMeta(cloud, order)
    adapter.notifyDataSetChanged
    olympusWrap.clouds = updated
  }

  def addNewCloud(url: String, auth: Int) = {
    val randomIdentity = ByteVector(random getBytes 16).toHex
    val emptyData = CloudData(info = None, tokens = Vector.empty, acts = Vector.empty)
    val cd = new Cloud(randomIdentity, new Connector(url), auth, 1) { data = emptyData }
    if (adapter add cd) onUpdate
  }

  def updateCloud(cloud: Cloud)(url: String, auth: Int) = {
    // Just update mutable fields and insert them into database
    // won't be re-addded because of INSERT IGNORE sql
    cloud.connector = new Connector(url)
    cloud.auth = auth
    onUpdate
  }

  override def onOptionsItemSelected(m: MenuItem) = {
    if (m.getItemId == R.id.actionQuestionMark) browse("http://lightning-wallet.com/what-does-olympus-server-do")
    else if (m.getItemId == R.id.actionAddEntity) new FormManager(addNewCloud, olympus_add)
    else if (m.getItemId == R.id.actionTokenLog) viewTokenUsageLog
    true
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.olympus, menu)
    true
  }

  class FormManager(next: (String, Int) => Unit, title: Int) {
    val content = getLayoutInflater.inflate(R.layout.frag_olympus_details, null, false)
    val serverHostPort = content.findViewById(R.id.serverHostPort).asInstanceOf[EditText]
    val formatInputHint = content.findViewById(R.id.formatInputHint).asInstanceOf[TextView]
    val serverBackupWatchtower = content.findViewById(R.id.serverBackup).asInstanceOf[CheckBox]
    mkCheckForm(addAttempt, none, baseBuilder(getString(title), content), dialog_ok, dialog_cancel)
    formatInputHint setText olympus_hint

    def set(c: Cloud) = {
      serverHostPort setText c.connector.url
      serverBackupWatchtower setChecked c.isAuthEnabled
    }

    def addAttempt(alert: AlertDialog): Unit = {
      val uriChecker = Uri parse serverHostPort.getText.toString
      if (uriChecker.getHost == null || uriChecker.getPort < 80) return
      next(uriChecker.toString, if (serverBackupWatchtower.isChecked) 1 else 0)
      alert.dismiss
    }
  }

  def viewTokenUsageLog = {
    val events = RichCursor(LNParams.db select OlympusLogTable.selectAllSql) vec { rc =>
      val stamp = when(thenDate = new Date(rc long OlympusLogTable.stamp), now = System.currentTimeMillis)
      s"<font color=#999999><strong>$stamp</strong></font> ${rc string OlympusLogTable.explanation}".html
    }

    val adapter = new ArrayAdapter(me, android.R.layout.simple_list_item_1, events.toArray)
    val bld = new AlertDialog.Builder(me).setCustomTitle(me getString olympus_log)
    bld.setAdapter(adapter, null).create.show
  }
}