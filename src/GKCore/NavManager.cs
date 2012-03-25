using System;
using System.Collections;
using System.Runtime.InteropServices;

using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public class NavManager : IDisposable
	{
		private bool FNavBusy;
		private Stack FStackBackward;
		private Stack FStackForward;
		private object FCurrent;
		protected bool Disposed_;

		public bool Busy
		{
			get { return this.FNavBusy; }
		}

		public object Current
		{
			get { return this.FCurrent; }
			set { this.SetCurrent(value); }
		}

		private void SetCurrent([In] object Value)
		{
			if (this.FCurrent == Value) return;

			if (this.FCurrent != null)
			{
				this.FStackBackward.Push(this.FCurrent);
			}
			this.FCurrent = Value;
			this.FStackForward.Clear();
		}

		public NavManager()
		{
			this.FStackBackward = new Stack();
			this.FStackForward = new Stack();
			this.FCurrent = null;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				//this.FStackBackward.Dispose();
				//this.FStackForward.Dispose();
				this.Disposed_ = true;
			}
		}

		public object Back()
		{
			if (this.FCurrent != null)
			{
				this.FStackForward.Push(this.FCurrent);
			}
			this.FCurrent = this.FStackBackward.Pop();
			return this.FCurrent;
		}

		public object Next()
		{
			if (this.FCurrent != null)
			{
				this.FStackBackward.Push(this.FCurrent);
			}
			this.FCurrent = this.FStackForward.Pop();
			return this.FCurrent;
		}

		public void Clear()
		{
			this.FStackBackward.Clear();
			this.FStackForward.Clear();
			this.FCurrent = null;
		}

		public void BeginNav()
		{
			this.FNavBusy = true;
		}

		public void EndNav()
		{
			this.FNavBusy = false;
		}

		public bool CanBackward()
		{
			return this.FStackBackward.Count > 0;
		}

		public bool CanForward()
		{
			return this.FStackForward.Count > 0;
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
