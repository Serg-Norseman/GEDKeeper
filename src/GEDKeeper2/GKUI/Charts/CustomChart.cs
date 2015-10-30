using System;
using System.Windows.Forms;
using GKCommon;

namespace GKUI.Charts
{
	public abstract class CustomChart : UserControl
	{
		private static readonly object EventNavRefresh;


		private readonly NavigationStack fNavman;


		public event EventHandler NavRefresh
		{
			add { base.Events.AddHandler(CustomChart.EventNavRefresh, value); }
			remove { base.Events.RemoveHandler(CustomChart.EventNavRefresh, value); }
		}


		static CustomChart()
        {
            CustomChart.EventNavRefresh = new object();
        }

		public CustomChart()
		{
			this.fNavman = new NavigationStack();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
                if (this.fNavman != null) this.fNavman.Dispose();
			}
			base.Dispose(disposing);
		}

        private void DoNavRefresh()
        {
            EventHandler eventHandler = (EventHandler)base.Events[CustomChart.EventNavRefresh];
            if (eventHandler == null) return;

            eventHandler(this, null);
        }


        protected abstract void SetNavObject(object obj);


        public bool NavAdd(object obj)
		{
			if (obj != null && !this.fNavman.Busy) {
				this.fNavman.Current = obj;
				return true;
			}
			return false;
		}

		public bool NavCanBackward()
		{
			return this.fNavman.CanBackward();
		}

		public bool NavCanForward()
		{
			return this.fNavman.CanForward();
		}

		public void NavNext()
		{
			if (!this.fNavman.CanForward()) return;

			this.fNavman.BeginNav();
			try
			{
				this.SetNavObject(this.fNavman.Next());
				this.DoNavRefresh();
			}
			finally
			{
				this.fNavman.EndNav();
			}
		}

		public void NavPrev()
		{
			if (!this.fNavman.CanBackward()) return;

			this.fNavman.BeginNav();
			try
			{
				this.SetNavObject(this.fNavman.Back());
				this.DoNavRefresh();
			}
			finally
			{
				this.fNavman.EndNav();
			}
		}
	}
}
