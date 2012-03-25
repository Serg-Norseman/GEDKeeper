using System;
using System.Collections.Generic;
using System.IO;

using Ext.Utils;

namespace GedCom551
{
	public class TGEDCOMList : IDisposable
	{
		private TList FList;
		//private TGEDCOMObject FOwner;
		private bool Disposed_;

		public int Count
		{
			get { return this.FList.Count; }
		}

		public TGEDCOMObject this[int Index]
		{
			get { return (this.FList[Index] as TGEDCOMObject); }
		}

		public TGEDCOMList(TGEDCOMObject AOwner)
		{
			//this.FOwner = AOwner;
			this.FList = new TList();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Clear();
				this.FList.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMObject Add(TGEDCOMObject AObject)
		{
			this.FList.Add(AObject);
			return AObject;
		}

		public void Clear()
		{
			for (int I = this.FList.Count - 1; I >= 0; I--)
			{
				(this.FList[I] as TGEDCOMObject).Free();
			}
			this.FList.Clear();
		}

		public void Delete(int Index)
		{
			(this.FList[Index] as TGEDCOMObject).Free();
			this.FList.Delete(Index);
		}

		public void DeleteObject(TGEDCOMObject AObject)
		{
			int Index = this.FList.IndexOf(AObject);
			if (Index >= 0)
			{
				this.Delete(Index);
			}
		}

		public void Exchange(int Index1, int Index2)
		{
			if (Index1 >= 0 && Index1 < this.FList.Count && Index2 >= 0 && Index2 < this.FList.Count)
			{
				this.FList.Exchange(Index1, Index2);
			}
		}

		public TGEDCOMObject Extract(int Index)
		{
			TGEDCOMObject Result = this[Index];
			this.FList.Delete(Index);
			return Result;
		}

		public int IndexOfObject(TGEDCOMObject AObject)
		{
			return this.FList.IndexOf(AObject);
		}

		public virtual void SaveToStream(StreamWriter AStream)
		{
			for (int I = 0; I <= this.FList.Count - 1; I++)
			{
				if (this.FList[I] is TGEDCOMCustomTag)
				{
					(this.FList[I] as TGEDCOMCustomTag).SaveToStream(AStream);
				}
			}
		}

		public void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			for (int i = 0; i <= this.FList.Count - 1; i++)
			{
				if (this.FList[i] is TGEDCOMCustomTag)
				{
					(this.FList[i] as TGEDCOMCustomTag).ReplaceXRefs(aMap);
				}
			}
		}

		public void ResetOwner(TGEDCOMTree AOwner)
		{
			for (int i = 0; i <= this.FList.Count - 1; i++)
			{
				(this.FList[i] as TGEDCOMCustomTag).ResetOwner(AOwner);
			}
		}

		public void Pack()
		{
			for (int i = this.FList.Count - 1; i >= 0; i--)
			{
				if (this.FList[i] is TGEDCOMCustomTag)
				{
					TGEDCOMCustomTag tag = this.FList[i] as TGEDCOMCustomTag;
					tag.Pack();
					if (tag.IsEmpty() && tag.IsEmptySkip())
					{
						this.Delete(i);
					}
				}
			}
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
	
	// // // // // // //

	public sealed class TGEDCOMListEx<T> : IDisposable
	{
		private List<T> FList = null; // lazy implementation
		//private TGEDCOMObject _owner;
		private bool _disposed;

		public int Count
		{
			get {
				return ((this.FList == null) ? 0 : this.FList.Count);
			}
		}

		public T this[int Index]
		{
			get {
				return ((this.FList == null) ? default(T) : this.FList[Index]);
			}
		}

		public TGEDCOMListEx(TGEDCOMObject AOwner)
		{
			//this._owner = AOwner;
		}

		public void Dispose()
		{
			if (!this._disposed)
			{
				this.Clear();
				//this.FList.Free(); isnot IDisposable
				this._disposed = true;
			}
		}

		public T Add(T item)
		{
			if (item != null)
			{
				if (this.FList == null)
				{
					this.FList = new List<T>();
				}

				this.FList.Add(item);
			}

			return item;
		}

		public void Clear()
		{
			if (this.FList != null)
			{
				for (int I = this.FList.Count - 1; I >= 0; I--)
				{
					(this.FList[I] as TGEDCOMObject).Free();
				}
				this.FList.Clear();
			}
		}

		public void Delete(int index)
		{
			if (this.FList != null)
			{
				(this.FList[index] as TGEDCOMObject).Free();
				this.FList.RemoveAt(index);
			}
		}

		public void DeleteObject(T item)
		{
			if (this.FList != null)
			{
				int idx = this.FList.IndexOf(item);
				if (idx >= 0)
				{
					this.Delete(idx);
				}
			}
		}

		public void Exchange(int Index1, int Index2)
		{
			if (this.FList != null)
			{
				if (Index1 >= 0 && Index1 < this.FList.Count && Index2 >= 0 && Index2 < this.FList.Count)
				{
					T tmp = this.FList[Index1];
					this.FList[Index1] = this.FList[Index2];
					this.FList[Index2] = tmp;
				}
			}
		}

		public T Extract(int index)
		{
			if (this.FList != null)
			{
				T result = this.FList[index];
				this.FList.RemoveAt(index);
				return result;
			}
			else
			{
				return default(T);
			}
		}

		public int IndexOfObject(T item)
		{
			if (this.FList != null)
			{
				return this.FList.IndexOf(item);
			}
			else
			{
				return -1;
			}
		}

		public void SaveToStream(StreamWriter AStream)
		{
			if (this.FList != null)
			{
				int num = this.FList.Count - 1;
				for (int I = 0; I <= num; I++)
				{
					if (this.FList[I] is TGEDCOMCustomTag)
					{
						(this.FList[I] as TGEDCOMCustomTag).SaveToStream(AStream);
					}
				}
			}
		}

		public void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			if (this.FList != null)
			{
				int num = this.FList.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this.FList[i] is TGEDCOMCustomTag)
					{
						(this.FList[i] as TGEDCOMCustomTag).ReplaceXRefs(aMap);
					}
				}
			}
		}

		public void ResetOwner(TGEDCOMTree AOwner)
		{
			if (this.FList != null)
			{
				int num = this.FList.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					(this.FList[i] as TGEDCOMCustomTag).ResetOwner(AOwner);
				}
			}
			//this._owner = AOwner;
		}

		public void Pack()
		{
			if (this.FList != null)
			{
				for (int i = this.FList.Count - 1; i >= 0; i--)
				{
					if (this.FList[i] is TGEDCOMCustomTag)
					{
						TGEDCOMCustomTag tag = this.FList[i] as TGEDCOMCustomTag;
						tag.Pack();
						if (tag.IsEmpty() && tag.IsEmptySkip())
						{
							this.Delete(i);
						}
					}
				}
			}
		}
	}
}
