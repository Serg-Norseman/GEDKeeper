using GKSys;
using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore
{
	public class TBackManager : IDisposable
	{
		internal bool FNavBusy;
		internal TStack FStackBackward;
		internal TStack FStackForward;
		internal object FCurrent;
		protected internal bool Disposed_;

		[Browsable(false)]
		public bool Busy
		{
			get
			{
				return this.FNavBusy;
			}
		}

		[Browsable(false)]
		public object Current
		{
			get
			{
				return this.FCurrent;
			}
			set
			{
				this.SetCurrent(value);
			}
		}

		internal void SetCurrent([In] object Value)
		{
			if (this.FCurrent != null)
			{
				this.FStackBackward.Push(this.FCurrent);
			}
			this.FCurrent = Value;
			this.FStackForward.Clear();
		}

		public TBackManager()
		{
			this.FStackBackward = new TStack();
			this.FStackForward = new TStack();
			this.FCurrent = null;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FStackBackward.Free();
				this.FStackForward.Free();
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
			return this.FStackBackward.Count() > 0;
		}

		public bool CanForward()
		{
			return this.FStackForward.Count() > 0;
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
