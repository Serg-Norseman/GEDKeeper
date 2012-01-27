using System;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKSys
{
	public class TStack : IDisposable
	{
		private TList FList;
		private bool Disposed_;

		protected TList List
		{
			get { return this.FList; }
		}

		public virtual void PushItem(object AItem)
		{
			this.FList.Add(AItem);
		}

		public void Clear()
		{
			this.FList.Clear();
		}

		public virtual object PopItem()
		{
			object Result = this.PeekItem();
			this.List.Delete(this.List.Count - 1);
			return Result;
		}

		public virtual object PeekItem()
		{
			return this.List[this.List.Count - 1];
		}

		public TStack()
		{
			this.FList = new TList();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.List.Dispose();
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
			SysUtils.Free(this);
		}
	}
}
