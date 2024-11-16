package com.lightning.walletapp

import android.widget._
import com.lightning.walletapp.Utils._
import com.lightning.walletapp.ln.Tools._
import com.lightning.walletapp.R.string._
import com.lightning.walletapp.ln.LNParams._
import com.lightning.walletapp.FragPayMarket._
import com.lightning.walletapp.lnutils.IconGetter._
import com.lightning.walletapp.lnutils.ImplicitConversions._

import android.os.{Bundle, Handler}
import android.database.{ContentObserver, Cursor}
import android.view.{LayoutInflater, View, ViewGroup}
import com.lightning.walletapp.lnutils.{PayMarketTable, PayMarketWrap}
import com.lightning.walletapp.helper.{ReactLoader, RichCursor, ThrottledWork}

import android.support.v4.app.LoaderManager.LoaderCallbacks
import android.transition.TransitionManager.beginDelayedTransition
import com.arlib.floatingsearchview.FloatingSearchView.OnQueryChangeListener
import com.arlib.floatingsearchview.FloatingSearchView
import com.lightning.walletapp.LNUrl.LNUrlAndData
import android.support.v4.content.Loader
import android.support.v4.app.Fragment


object FragPayMarket {
  var worker: FragPayMarketWorker = _
}

class FragPayMarket extends Fragment {
  override def onCreateView(inf: LayoutInflater, viewGroup: ViewGroup, bundle: Bundle) = inf.inflate(R.layout.frag_view_pager_paymarket, viewGroup, false)
  override def onViewCreated(view: View, state: Bundle) = if (app.isAlive) worker = new FragPayMarketWorker(getActivity.asInstanceOf[WalletActivity], view)
  override def setUserVisibleHint(isVisibleToUser: Boolean) = if (isAdded && isVisibleToUser && worker.allPayLinks.isEmpty) PayMarketWrap.uiNotify
}

class FragPayMarketWorker(val host: WalletActivity, frag: View) extends HumanTimeDisplay { me =>
  import host.{UITask, onButtonTap, getLayoutInflater, rm, mkCheckForm, str2View, onLongButtonTap}

  val paySearch = frag.findViewById(R.id.paySearch).asInstanceOf[FloatingSearchView]
  val gridView = frag.findViewById(R.id.gridView).asInstanceOf[GridView]
  val removeLink = app getString pay_market_remove_link
  val lastPaid = app getString pay_market_last_payment
  var allPayLinks = Vector.empty[PayLinkInfo]

  val adapter = new BaseAdapter {
    def getCount = allPayLinks.size
    def getItemId(linkPosition: Int) = linkPosition
    def getItem(position: Int) = allPayLinks(position)
    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val card = if (null == savedView) getLayoutInflater.inflate(R.layout.card_pay_link, null) else savedView
      val holder = if (null == card.getTag) ViewHolder(card) else card.getTag.asInstanceOf[ViewHolder]
      holder fillView getItem(position)
      card
    }
  }

  val work = new ThrottledWork[LNUrl, LNUrlAndData] {
    def process(lnUrl: LNUrl, data: LNUrlAndData) = runAnd(UITask(host resolveLNUrl data).run)(updPayLinksList.run)
    def error(fetchFail: Throwable) = runAnd(host onFail fetchFail)(updPayLinksList.run)
    def work(lnUrl: LNUrl) = lnUrl.lnUrlAndDataObs
  }

  val loaderCallbacks = new LoaderCallbacks[Cursor] {
    def onCreateLoader(id: Int, bn: Bundle) = new ReactLoader[PayLinkInfo](host) {
      val consume = (payLinks: PayLinkInfoVec) => runAnd(allPayLinks = payLinks)(updPayLinksList.run)
      def getCursor = if (paySearch.getQuery.isEmpty) PayMarketWrap.byRecent else PayMarketWrap.byQuery(paySearch.getQuery)
      def createItem(rc: RichCursor) = PayMarketWrap.toLinkInfo(rc)
    }

    type LoaderCursor = Loader[Cursor]
    type PayLinkInfoVec = Vector[PayLinkInfo]
    def onLoaderReset(loaderCursor: LoaderCursor) = none
    def onLoadFinished(loaderCursor: LoaderCursor, c: Cursor) = none
  }

  case class ViewHolder(view: View) {
    val image = view.findViewById(R.id.image).asInstanceOf[ImageView]
    val wrapper = view.findViewById(R.id.wrapper).asInstanceOf[RelativeLayout]
    val domainName = view.findViewById(R.id.domainName).asInstanceOf[TextView]
    val textMetadata = view.findViewById(R.id.textMetadata).asInstanceOf[TextView]
    val lastAttempt = view.findViewById(R.id.lastAttempt).asInstanceOf[TextView]
    view setTag this

    def fillView(info: PayLinkInfo): Unit = {
      val thisLinkSelectedCurrently = work.subscriptionAndData.exists(_.data == info.lnurl)
      val backgroundColor = if (thisLinkSelectedCurrently) Denomination.yellowHighlight else 0x00000000
      val lastInfo = lastPaid.format(me time new java.util.Date(info.lastDate), denom parsedWithSign info.lastMsat)

      view setOnClickListener onButtonTap {
        // Do not proceed if 1st call is being made or related payment is in-flight
        val inFlight = ChannelManager.activeInFlightHashes.contains(info.paymentHash)
        val canProceed = !(inFlight || thisLinkSelectedCurrently)

        if (canProceed) {
          work.replaceWork(info.lnurl)
          updPayLinksList.run
        }
      }

      view setOnLongClickListener onLongButtonTap {
        def removePayLink = wrap(PayMarketWrap.uiNotify)(PayMarketWrap rm info.lnurl)
        val bld = host.baseTextBuilder(removeLink.html).setCustomTitle(info.lnurl.uri.getHost)
        mkCheckForm(alert => rm(alert)(removePayLink), none, bld, dialog_ok, dialog_cancel)
      }

      info.bitmap map image.setImageBitmap
      image setVisibility viewMap(info.bitmap.isSuccess)
      wrapper setBackgroundColor backgroundColor
      domainName setText info.lnurl.uri.getHost
      lastAttempt setText lastInfo.html
      textMetadata setText info.text
    }
  }

  def updPayLinksList = UITask {
    beginDelayedTransition(gridView)
    adapter.notifyDataSetChanged
  }

  private def reload = android.support.v4.app.LoaderManager.getInstance(host).restartLoader(2, null, loaderCallbacks).forceLoad
  val observer = new ContentObserver(new Handler) { override def onChange(askedFromSelf: Boolean) = if (!askedFromSelf) reload }
  paySearch setOnQueryChangeListener new OnQueryChangeListener { def onSearchTextChanged(q0: String, q1: String) = reload }
  host.getContentResolver.registerContentObserver(db sqlPath PayMarketTable.table, true, observer)
  gridView setNumColumns math.round(scrWidth / 2.4).toInt
  gridView setAdapter adapter
}