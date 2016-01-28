using System;
using System.Collections;

namespace GKCommon
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class NavigationStack : BaseObject
	{
		private bool fNavBusy;
		private readonly Stack fStackBackward;
		private readonly Stack fStackForward;
		private object fCurrent;

		public bool Busy
		{
			get { return this.fNavBusy; }
		}

		public object Current
		{
			get { return this.fCurrent; }
			set { this.SetCurrent(value); }
		}

		private void SetCurrent(object value)
		{
			if (this.fCurrent == value) return;

			if (this.fCurrent != null)
			{
				this.fStackBackward.Push(this.fCurrent);
			}
			this.fCurrent = value;
			this.fStackForward.Clear();
		}

		public NavigationStack()
		{
			this.fStackBackward = new Stack();
			this.fStackForward = new Stack();
			this.fCurrent = null;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				//this.fStackBackward.Dispose();
				//this.fStackForward.Dispose();
			}
			base.Dispose(disposing);
		}

		public object Back()
		{
			if (this.fCurrent != null)
			{
				this.fStackForward.Push(this.fCurrent);
			}
			this.fCurrent = this.fStackBackward.Pop();
			return this.fCurrent;
		}

		public object Next()
		{
			if (this.fCurrent != null)
			{
				this.fStackBackward.Push(this.fCurrent);
			}
			this.fCurrent = this.fStackForward.Pop();
			return this.fCurrent;
		}

		public void Clear()
		{
			this.fStackBackward.Clear();
			this.fStackForward.Clear();
			this.fCurrent = null;
		}

		public void BeginNav()
		{
			this.fNavBusy = true;
		}

		public void EndNav()
		{
			this.fNavBusy = false;
		}

		public bool CanBackward()
		{
			return this.fStackBackward.Count > 0;
		}

		public bool CanForward()
		{
			return this.fStackForward.Count > 0;
		}
	}
}
