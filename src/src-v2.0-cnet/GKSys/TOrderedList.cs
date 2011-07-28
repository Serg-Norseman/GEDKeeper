using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKSys
{

	public abstract class TOrderedList : IDisposable
	{
		internal TList FList;
		protected internal bool Disposed_;
		protected internal TList List
		{
			get
			{
				return this.FList;
			}
		}
		protected internal abstract void PushItem(object AItem);
		protected internal virtual object PopItem()
		{
			object Result = this.PeekItem();
			this.List.Delete(this.List.Count - 1);
			return Result;
		}
		protected internal virtual object PeekItem()
		{
			return this.List[this.List.Count - 1];
		}
		public TOrderedList()
		{
			this.FList = new TList();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.List.Free();
				this.Disposed_ = true;
			}
		}

		public int Count()
		{
			return this.List.Count;
		}
		public bool AtLeast(int ACount)
		{
			return this.List.Count >= ACount;
		}
		public object Push(object AItem)
		{
			this.PushItem(AItem);
			return AItem;
		}
		public object Pop()
		{
			return this.PopItem();
		}
		public object Peek()
		{
			return this.PeekItem();
		}
		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
